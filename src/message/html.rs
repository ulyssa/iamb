//! # Rendering for formatted bodies
//!
//! This module contains the code for rendering messages that contained an
//! "org.matrix.custom.html"-formatted body.
//!
//! The Matrix specification recommends limiting rendered tags and attributes to a safe subset of
//! HTML. You can read more in section 11.2.1.1, "m.room.message msgtypes":
//!
//!   <https://spec.matrix.org/unstable/client-server-api/#mroommessage-msgtypes>
//!
//! This isn't as important for iamb, since it isn't a browser environment, but we do still map
//! input onto an enum of the safe list of tags to keep it easy to understand and process.
use std::borrow::Cow;
use std::ops::Deref;

use css_color_parser::Color as CssColor;
use markup5ever_rcdom::{Handle, NodeData, RcDom};
use matrix_sdk::ruma::{OwnedRoomAliasId, OwnedRoomId, OwnedUserId};
use unicode_segmentation::UnicodeSegmentation;
use url::Url;

use html5ever::{
    driver::{parse_fragment, ParseOpts},
    interface::{Attribute, QualName},
    local_name,
    namespace_url,
    ns,
    tendril::{StrTendril, TendrilSink},
};

use ratatui::{
    layout::Alignment,
    style::{Color, Modifier as StyleModifier, Style},
    symbols::line,
    text::{Line, Span, Text},
};

use crate::{
    config::ApplicationSettings,
    message::printer::TextPrinter,
    util::{join_cell_text, space_text},
};

const CODE_BACKGROUND: Color = Color::Indexed(236);

/// Generate bullet points from a [ListStyle].
pub struct BulletIterator {
    style: ListStyle,
    pos: usize,
    len: usize,
}

impl BulletIterator {
    fn width(&self) -> usize {
        match self.style {
            ListStyle::Unordered => 2,
            ListStyle::Ordered => self.len.to_string().len() + 2,
        }
    }
}

impl Iterator for BulletIterator {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos == self.len {
            return None;
        }

        self.pos += 1;

        let bullet = match self.style {
            ListStyle::Unordered => "- ".to_string(),
            ListStyle::Ordered => {
                let w = self.len.to_string().len();
                format!("{: >w$}. ", self.pos, w = w)
            },
        };

        return Some(bullet);
    }
}

/// Whether this list is ordered or unordered.
#[derive(Clone, Copy, Debug)]
pub enum ListStyle {
    Ordered,
    Unordered,
}

impl ListStyle {
    fn bullets(&self, len: usize) -> BulletIterator {
        BulletIterator { style: *self, pos: 0, len }
    }
}

pub type StyleTreeChildren = Vec<StyleTreeNode>;

/// Type of contents in a table cell.
#[derive(Debug)]
pub enum CellType {
    Data,
    Header,
}

/// A collection of cells for a single row in a table.
#[derive(Debug)]
pub struct TableRow {
    cells: Vec<(CellType, StyleTreeNode)>,
}

impl TableRow {
    pub fn gather_links(&self, urls: &mut Vec<(char, Url)>) {
        for (_, cell) in &self.cells {
            cell.gather_links(urls);
        }
    }

    fn columns(&self) -> usize {
        self.cells.len()
    }
}

/// A collection of rows in a table.
#[derive(Debug)]
pub struct TableSection {
    rows: Vec<TableRow>,
}

impl TableSection {
    pub fn gather_links(&self, urls: &mut Vec<(char, Url)>) {
        for row in &self.rows {
            row.gather_links(urls);
        }
    }

    fn columns(&self) -> usize {
        self.rows.iter().map(TableRow::columns).max().unwrap_or(0)
    }
}

/// A table.
#[derive(Debug)]
pub struct Table {
    caption: Option<Box<StyleTreeNode>>,
    sections: Vec<TableSection>,
}

impl Table {
    fn columns(&self) -> usize {
        self.sections.iter().map(TableSection::columns).max().unwrap_or(0)
    }

    pub fn gather_links(&self, urls: &mut Vec<(char, Url)>) {
        for section in &self.sections {
            section.gather_links(urls);
        }
    }

    fn to_text<'a>(
        &'a self,
        width: usize,
        style: Style,
        settings: &'a ApplicationSettings,
    ) -> Text<'a> {
        let mut text = Text::default();
        let columns = self.columns();
        let cell_total = width.saturating_sub(columns).saturating_sub(1);
        let cell_min = cell_total / columns;
        let mut cell_slop = cell_total - cell_min * columns;
        let cell_widths = (0..columns)
            .map(|_| {
                let slopped = cell_slop.min(1);
                cell_slop -= slopped;
                cell_min + slopped
            })
            .collect::<Vec<_>>();

        let mut nrows = 0;

        if let Some(caption) = &self.caption {
            let subw = width.saturating_sub(6);
            let mut printer =
                TextPrinter::new(subw, style, true, settings).align(Alignment::Center);
            caption.print(&mut printer, style);

            for mut line in printer.finish().lines {
                line.spans.insert(0, Span::styled("   ", style));
                line.spans.push(Span::styled("   ", style));
                text.lines.push(line);
            }
        }

        for section in self.sections.iter() {
            for row in section.rows.iter() {
                let mut ruler = String::new();

                for (i, w) in cell_widths.iter().enumerate() {
                    let cross = match (nrows, i) {
                        (0, 0) => line::TOP_LEFT,
                        (0, _) => line::HORIZONTAL_DOWN,
                        (_, 0) => line::VERTICAL_RIGHT,
                        (_, _) => line::CROSS,
                    };

                    ruler.push_str(cross);

                    for _ in 0..*w {
                        ruler.push_str(line::HORIZONTAL);
                    }
                }

                if nrows == 0 {
                    ruler.push_str(line::TOP_RIGHT);
                } else {
                    ruler.push_str(line::VERTICAL_LEFT);
                }

                text.lines.push(Line::from(vec![Span::styled(ruler, style)]));

                let cells = cell_widths
                    .iter()
                    .enumerate()
                    .map(|(i, w)| {
                        let text = if let Some((kind, cell)) = row.cells.get(i) {
                            let style = match kind {
                                CellType::Header => style.add_modifier(StyleModifier::BOLD),
                                CellType::Data => style,
                            };

                            cell.to_text(*w, style, settings)
                        } else {
                            space_text(*w, style)
                        };

                        (text, *w)
                    })
                    .collect();

                let joined = join_cell_text(cells, Span::styled(line::VERTICAL, style), style);
                text.lines.extend(joined.lines);

                nrows += 1;
            }
        }

        if nrows > 0 {
            let mut ruler = String::new();

            for (i, w) in cell_widths.iter().enumerate() {
                let cross = if i == 0 {
                    line::BOTTOM_LEFT
                } else {
                    line::HORIZONTAL_UP
                };

                ruler.push_str(cross);

                for _ in 0..*w {
                    ruler.push_str(line::HORIZONTAL);
                }
            }

            ruler.push_str(line::BOTTOM_RIGHT);
            text.lines.push(Line::from(vec![Span::styled(ruler, style)]));
        }

        text
    }
}

/// A processed HTML element that we can render to the terminal.
#[derive(Debug)]
pub enum StyleTreeNode {
    Anchor(Box<StyleTreeNode>, char, Url),
    Blockquote(Box<StyleTreeNode>),
    Break,
    #[allow(dead_code)]
    Code(Box<StyleTreeNode>, Option<String>),
    Header(Box<StyleTreeNode>, usize),
    Image(Option<String>),
    List(StyleTreeChildren, ListStyle),
    Paragraph(Box<StyleTreeNode>),
    Pre(Box<StyleTreeNode>),
    Reply(Box<StyleTreeNode>),
    Ruler,
    Style(Box<StyleTreeNode>, Style),
    Table(Table),
    Text(Cow<'static, str>),
    Sequence(StyleTreeChildren),
    RoomAlias(OwnedRoomAliasId),
    RoomId(OwnedRoomId),
    UserId(OwnedUserId),
    DisplayName(String, OwnedUserId),
}

impl StyleTreeNode {
    pub fn to_text<'a>(
        &'a self,
        width: usize,
        style: Style,
        settings: &'a ApplicationSettings,
    ) -> Text<'a> {
        let mut printer = TextPrinter::new(width, style, true, settings);
        self.print(&mut printer, style);
        printer.finish()
    }

    pub fn gather_links(&self, urls: &mut Vec<(char, Url)>) {
        match self {
            StyleTreeNode::Anchor(_, c, url) => {
                urls.push((*c, url.clone()));
            },

            StyleTreeNode::Blockquote(child) |
            StyleTreeNode::Code(child, _) |
            StyleTreeNode::Header(child, _) |
            StyleTreeNode::Paragraph(child) |
            StyleTreeNode::Pre(child) |
            StyleTreeNode::Reply(child) |
            StyleTreeNode::Style(child, _) => {
                child.gather_links(urls);
            },

            StyleTreeNode::List(children, _) | StyleTreeNode::Sequence(children) => {
                for child in children {
                    child.gather_links(urls);
                }
            },

            StyleTreeNode::Table(table) => {
                table.gather_links(urls);
            },

            StyleTreeNode::Image(_) => {},
            StyleTreeNode::Ruler => {},
            StyleTreeNode::Text(_) => {},
            StyleTreeNode::Break => {},

            // TODO: eventually these should turn into internal links:
            StyleTreeNode::UserId(_) => {},
            StyleTreeNode::RoomId(_) => {},
            StyleTreeNode::RoomAlias(_) => {},
            StyleTreeNode::DisplayName(_, _) => {},
        }
    }

    pub fn print<'a>(&'a self, printer: &mut TextPrinter<'a>, style: Style) {
        let width = printer.width();

        match self {
            StyleTreeNode::Anchor(child, c, _) => {
                let bold = style.add_modifier(StyleModifier::BOLD);
                child.print(printer, bold);

                let link = format!("[{c}]");
                let span = Span::styled(link, style);
                printer.push_span_nobreak(span);
            },
            StyleTreeNode::Blockquote(child) => {
                let mut subp = printer.sub(4);
                child.print(&mut subp, style);

                for mut line in subp.finish() {
                    line.spans.insert(0, Span::styled("    ", style));
                    printer.push_line(line);
                }
            },
            StyleTreeNode::Code(child, _) => {
                let style = style.bg(CODE_BACKGROUND);
                let old = printer.set_base_style(style);
                child.print(printer, style);
                printer.set_base_style(old);
            },
            StyleTreeNode::Header(child, level) => {
                let style = style.add_modifier(StyleModifier::BOLD);

                for _ in 0..*level {
                    printer.push_str("#", style);
                }

                printer.push_str(" ", style);
                child.print(printer, style);
            },
            StyleTreeNode::Image(None) => {},
            StyleTreeNode::Image(Some(alt)) => {
                printer.commit();
                printer.push_str("Image Alt: ", Style::default());
                printer.push_str(alt, Style::default());
                printer.commit();
            },
            StyleTreeNode::List(children, lt) => {
                let mut bullets = lt.bullets(children.len());
                let liw = bullets.width();

                for child in children {
                    let mut subp = printer.sub(liw);
                    let mut bullet = bullets.next();
                    child.print(&mut subp, style);

                    for mut line in subp.finish() {
                        let leading = if let Some(bullet) = bullet.take() {
                            Span::styled(bullet, style)
                        } else {
                            Span::styled(" ".repeat(liw), style)
                        };

                        line.spans.insert(0, leading);
                        printer.push_line(line);
                    }
                }
            },
            StyleTreeNode::Paragraph(child) => {
                printer.push_break();
                child.print(printer, style);
                printer.commit();
            },
            StyleTreeNode::Pre(child) => {
                let mut subp = printer.sub(2).literal(true);
                let subw = subp.width();

                child.print(&mut subp, style);

                printer.commit();
                printer.push_line(
                    vec![
                        Span::styled(line::TOP_LEFT, style),
                        Span::styled(line::HORIZONTAL.repeat(subw), style),
                        Span::styled(line::TOP_RIGHT, style),
                    ]
                    .into(),
                );

                for mut line in subp.finish() {
                    line.spans.insert(0, Span::styled(line::VERTICAL, style));
                    line.spans.push(Span::styled(line::VERTICAL, style));
                    printer.push_line(line);
                }

                printer.push_line(
                    vec![
                        Span::styled(line::BOTTOM_LEFT, style),
                        Span::styled(line::HORIZONTAL.repeat(subw), style),
                        Span::styled(line::BOTTOM_RIGHT, style),
                    ]
                    .into(),
                );

                printer.commit();
            },
            StyleTreeNode::Reply(child) => {
                if printer.hide_reply() {
                    return;
                }

                printer.push_break();
                child.print(printer, style);
                printer.commit();
            },
            StyleTreeNode::Ruler => {
                for _ in 0..width {
                    printer.push_str(line::HORIZONTAL, style);
                }
            },
            StyleTreeNode::Table(table) => {
                let text = table.to_text(width, style, printer.settings);
                printer.push_text(text);
            },
            StyleTreeNode::Break => {
                printer.push_break();
            },
            StyleTreeNode::Text(s) => {
                printer.push_str(s.as_ref(), style);
            },

            StyleTreeNode::Style(child, patch) => child.print(printer, style.patch(*patch)),
            StyleTreeNode::Sequence(children) => {
                for child in children {
                    child.print(printer, style);
                }
            },

            StyleTreeNode::UserId(user_id) => {
                let style = printer.settings().get_user_style(user_id);
                printer.push_str(user_id.as_str(), style);
            },
            StyleTreeNode::DisplayName(display_name, user_id) => {
                let style = printer.settings().get_user_style(user_id);
                printer.push_str(display_name.as_str(), style);
            },
            StyleTreeNode::RoomId(room_id) => {
                let bold = style.add_modifier(StyleModifier::BOLD);
                printer.push_str(room_id.as_str(), bold);
            },
            StyleTreeNode::RoomAlias(alias) => {
                let bold = style.add_modifier(StyleModifier::BOLD);
                printer.push_str(alias.as_str(), bold);
            },
        }
    }
}

/// A processed HTML document.
pub struct StyleTree {
    pub(super) children: StyleTreeChildren,
}

impl StyleTree {
    pub fn get_links(&self) -> Vec<(char, Url)> {
        let mut links = Vec::new();

        for child in &self.children {
            child.gather_links(&mut links);
        }

        return links;
    }

    pub fn to_text<'a>(
        &'a self,
        width: usize,
        style: Style,
        hide_reply: bool,
        settings: &'a ApplicationSettings,
    ) -> Text<'a> {
        let mut printer = TextPrinter::new(width, style, hide_reply, settings);

        for child in self.children.iter() {
            child.print(&mut printer, style);
        }

        printer.finish()
    }
}

pub struct TreeGenState {
    link_num: u8,
}

impl TreeGenState {
    fn next_link_char(&mut self) -> Option<char> {
        let num = self.link_num;

        if num < 62 {
            self.link_num = num + 1;
        }

        if num < 10 {
            Some((num + b'0') as char)
        } else if num < 36 {
            Some((num - 10 + b'a') as char)
        } else if num < 62 {
            Some((num - 36 + b'A') as char)
        } else {
            None
        }
    }
}

fn c2c(handles: &[Handle], state: &mut TreeGenState) -> Vec<StyleTreeNode> {
    handles.iter().flat_map(|h| h2t(h, state)).collect()
}

fn c2t(handles: &[Handle], state: &mut TreeGenState) -> Box<StyleTreeNode> {
    let node = StyleTreeNode::Sequence(c2c(handles, state));

    Box::new(node)
}

fn get_node(hdl: &Handle, want: &str, state: &mut TreeGenState) -> Option<StyleTreeNode> {
    let node = hdl.deref();

    if let NodeData::Element { name, .. } = &node.data {
        if name.local.as_ref() != want {
            return None;
        }

        let c = c2c(&node.children.borrow(), state);
        return Some(StyleTreeNode::Sequence(c));
    } else {
        return None;
    }
}

fn li2t(hdl: &Handle, state: &mut TreeGenState) -> Option<StyleTreeNode> {
    get_node(hdl, "li", state)
}

fn table_cell(hdl: &Handle, state: &mut TreeGenState) -> Option<(CellType, StyleTreeNode)> {
    if let Some(node) = get_node(hdl, "th", state) {
        return Some((CellType::Header, node));
    }

    Some((CellType::Data, get_node(hdl, "td", state)?))
}

fn table_row(hdl: &Handle, state: &mut TreeGenState) -> Option<TableRow> {
    let node = hdl.deref();

    if let NodeData::Element { name, .. } = &node.data {
        if name.local.as_ref() != "tr" {
            return None;
        }

        let cells = table_cells(&node.children.borrow(), state);
        return Some(TableRow { cells });
    } else {
        return None;
    }
}

fn table_section(hdl: &Handle, state: &mut TreeGenState) -> Option<TableSection> {
    let node = hdl.deref();

    if let NodeData::Element { name, .. } = &node.data {
        match name.local.as_ref() {
            "thead" | "tbody" => {
                let rows = table_rows(&node.children.borrow(), state);

                Some(TableSection { rows })
            },
            _ => None,
        }
    } else {
        return None;
    }
}

fn table_cells(handles: &[Handle], state: &mut TreeGenState) -> Vec<(CellType, StyleTreeNode)> {
    handles.iter().filter_map(|h| table_cell(h, state)).collect()
}

fn table_rows(handles: &[Handle], state: &mut TreeGenState) -> Vec<TableRow> {
    handles.iter().filter_map(|h| table_row(h, state)).collect()
}

fn table_sections(handles: &[Handle], state: &mut TreeGenState) -> Vec<TableSection> {
    handles.iter().filter_map(|h| table_section(h, state)).collect()
}

fn lic2t(handles: &[Handle], state: &mut TreeGenState) -> StyleTreeChildren {
    handles.iter().filter_map(|h| li2t(h, state)).collect()
}

fn attrs_to_alt(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.name.local.as_ref() != "alt" {
            continue;
        }

        return Some(attr.value.to_string());
    }

    return None;
}

fn attrs_to_href(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.name.local.as_ref() != "href" {
            continue;
        }

        return Some(attr.value.to_string());
    }

    return None;
}

fn attrs_to_language(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.name.local.as_ref() != "class" {
            continue;
        }

        for class in attr.value.as_ref().unicode_words() {
            if class.len() > 9 && class.starts_with("language-") {
                return Some(class[9..].to_string());
            }
        }
    }

    return None;
}

fn attrs_to_style(attrs: &[Attribute]) -> Style {
    let mut style = Style::default();

    for attr in attrs {
        match attr.name.local.as_ref() {
            "data-mx-bg-color" => {
                if let Ok(rgb) = attr.value.as_ref().parse::<CssColor>() {
                    let color = Color::Rgb(rgb.r, rgb.g, rgb.b);
                    style = style.bg(color);
                }
            },
            "data-mx-color" | "color" => {
                if let Ok(rgb) = attr.value.as_ref().parse::<CssColor>() {
                    let color = Color::Rgb(rgb.r, rgb.g, rgb.b);
                    style = style.fg(color);
                }
            },
            _ => continue,
        }
    }

    return style;
}

fn h2t(hdl: &Handle, state: &mut TreeGenState) -> StyleTreeChildren {
    let node = hdl.deref();

    let tree = match &node.data {
        NodeData::Document => *c2t(node.children.borrow().as_slice(), state),
        NodeData::Text { contents } => StyleTreeNode::Text(contents.borrow().to_string().into()),
        NodeData::Element { name, attrs, .. } => {
            match name.local.as_ref() {
                // Message that this one replies to.
                "mx-reply" => StyleTreeNode::Reply(c2t(&node.children.borrow(), state)),

                // Links
                "a" => {
                    let c = c2t(&node.children.borrow(), state);
                    let h = attrs_to_href(&attrs.borrow()).and_then(|u| Url::parse(&u).ok());

                    if let Some(h) = h {
                        if let Some(n) = state.next_link_char() {
                            StyleTreeNode::Anchor(c, n, h)
                        } else {
                            *c
                        }
                    } else {
                        *c
                    }
                },

                // Style change
                "b" | "strong" => {
                    let c = c2t(&node.children.borrow(), state);
                    let s = Style::default().add_modifier(StyleModifier::BOLD);

                    StyleTreeNode::Style(c, s)
                },
                "font" => {
                    let c = c2t(&node.children.borrow(), state);
                    let s = attrs_to_style(&attrs.borrow());

                    StyleTreeNode::Style(c, s)
                },
                "em" | "i" => {
                    let c = c2t(&node.children.borrow(), state);
                    let s = Style::default().add_modifier(StyleModifier::ITALIC);

                    StyleTreeNode::Style(c, s)
                },
                "span" => {
                    let c = c2t(&node.children.borrow(), state);
                    let s = attrs_to_style(&attrs.borrow());

                    StyleTreeNode::Style(c, s)
                },
                "del" | "strike" => {
                    let c = c2t(&node.children.borrow(), state);
                    let s = Style::default().add_modifier(StyleModifier::CROSSED_OUT);

                    StyleTreeNode::Style(c, s)
                },
                "u" => {
                    let c = c2t(&node.children.borrow(), state);
                    let s = Style::default().add_modifier(StyleModifier::UNDERLINED);

                    StyleTreeNode::Style(c, s)
                },

                // Lists
                "ol" => {
                    StyleTreeNode::List(lic2t(&node.children.borrow(), state), ListStyle::Ordered)
                },
                "ul" => {
                    StyleTreeNode::List(lic2t(&node.children.borrow(), state), ListStyle::Unordered)
                },

                // Headers
                "h1" => StyleTreeNode::Header(c2t(&node.children.borrow(), state), 1),
                "h2" => StyleTreeNode::Header(c2t(&node.children.borrow(), state), 2),
                "h3" => StyleTreeNode::Header(c2t(&node.children.borrow(), state), 3),
                "h4" => StyleTreeNode::Header(c2t(&node.children.borrow(), state), 4),
                "h5" => StyleTreeNode::Header(c2t(&node.children.borrow(), state), 5),
                "h6" => StyleTreeNode::Header(c2t(&node.children.borrow(), state), 6),

                // Table
                "table" => {
                    let sections = table_sections(&node.children.borrow(), state);
                    let caption = node
                        .children
                        .borrow()
                        .iter()
                        .find_map(|hdl| get_node(hdl, "caption", state))
                        .map(Box::new);
                    let table = Table { caption, sections };

                    StyleTreeNode::Table(table)
                },

                // Code blocks.
                "code" => {
                    let c = c2t(&node.children.borrow(), state);
                    let l = attrs_to_language(&attrs.borrow());

                    StyleTreeNode::Code(c, l)
                },

                // Other text blocks.
                "blockquote" => StyleTreeNode::Blockquote(c2t(&node.children.borrow(), state)),
                "div" | "p" => StyleTreeNode::Paragraph(c2t(&node.children.borrow(), state)),
                "pre" => StyleTreeNode::Pre(c2t(&node.children.borrow(), state)),

                // No children.
                "hr" => StyleTreeNode::Ruler,
                "br" => StyleTreeNode::Break,

                "img" => StyleTreeNode::Image(attrs_to_alt(&attrs.borrow())),

                // These don't render in any special way.
                "details" | "html" | "summary" | "sub" | "sup" => {
                    *c2t(&node.children.borrow(), state)
                },

                _ => return vec![],
            }
        },

        // These don't render as anything.
        NodeData::Doctype { .. } => return vec![],
        NodeData::Comment { .. } => return vec![],
        NodeData::ProcessingInstruction { .. } => return vec![],
    };

    vec![tree]
}

fn dom_to_style_tree(dom: RcDom) -> StyleTree {
    let mut state = TreeGenState { link_num: 0 };
    let children = h2t(&dom.document, &mut state);

    StyleTree { children }
}

/// Parse an HTML document from a string.
pub fn parse_matrix_html(s: &str) -> StyleTree {
    let dom = parse_fragment(
        RcDom::default(),
        ParseOpts::default(),
        QualName::new(None, ns!(html), local_name!("body")),
        vec![],
    )
    .one(StrTendril::from(s));

    dom_to_style_tree(dom)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tests::mock_settings;
    use crate::util::space_span;
    use pretty_assertions::assert_eq;
    use unicode_width::UnicodeWidthStr;

    #[test]
    fn test_header() {
        let settings = mock_settings();
        let bold = Style::default().add_modifier(StyleModifier::BOLD);

        let s = "<h1>Header 1</h1>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("#", bold),
            Span::styled(" ", bold),
            Span::styled("Header", bold),
            Span::styled(" ", bold),
            Span::styled("1", bold),
            space_span(10, Style::default())
        ])]);

        let s = "<h2>Header 2</h2>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled(" ", bold),
            Span::styled("Header", bold),
            Span::styled(" ", bold),
            Span::styled("2", bold),
            space_span(9, Style::default())
        ])]);

        let s = "<h3>Header 3</h3>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled(" ", bold),
            Span::styled("Header", bold),
            Span::styled(" ", bold),
            Span::styled("3", bold),
            space_span(8, Style::default())
        ])]);

        let s = "<h4>Header 4</h4>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled(" ", bold),
            Span::styled("Header", bold),
            Span::styled(" ", bold),
            Span::styled("4", bold),
            space_span(7, Style::default())
        ])]);

        let s = "<h5>Header 5</h5>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled(" ", bold),
            Span::styled("Header", bold),
            Span::styled(" ", bold),
            Span::styled("5", bold),
            space_span(6, Style::default())
        ])]);

        let s = "<h6>Header 6</h6>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled("#", bold),
            Span::styled(" ", bold),
            Span::styled("Header", bold),
            Span::styled(" ", bold),
            Span::styled("6", bold),
            space_span(5, Style::default())
        ])]);
    }

    #[test]
    fn test_style() {
        let settings = mock_settings();
        let def = Style::default();
        let bold = def.add_modifier(StyleModifier::BOLD);
        let italic = def.add_modifier(StyleModifier::ITALIC);
        let strike = def.add_modifier(StyleModifier::CROSSED_OUT);
        let underl = def.add_modifier(StyleModifier::UNDERLINED);
        let red = def.fg(Color::Rgb(0xff, 0x00, 0x00));

        let s = "<b>Bold!</b>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Bold", bold),
            Span::styled("!", bold),
            space_span(15, def)
        ])]);

        let s = "<strong>Bold!</strong>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Bold", bold),
            Span::styled("!", bold),
            space_span(15, def)
        ])]);

        let s = "<i>Italic!</i>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Italic", italic),
            Span::styled("!", italic),
            space_span(13, def)
        ])]);

        let s = "<em>Italic!</em>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Italic", italic),
            Span::styled("!", italic),
            space_span(13, def)
        ])]);

        let s = "<del>Strikethrough!</del>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Strikethrough", strike),
            Span::styled("!", strike),
            space_span(6, def)
        ])]);

        let s = "<strike>Strikethrough!</strike>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Strikethrough", strike),
            Span::styled("!", strike),
            space_span(6, def)
        ])]);

        let s = "<u>Underline!</u>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Underline", underl),
            Span::styled("!", underl),
            space_span(10, def)
        ])]);

        let s = "<font color=\"#ff0000\">Red!</u>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Red", red),
            Span::styled("!", red),
            space_span(16, def)
        ])]);

        let s = "<font color=\"red\">Red!</u>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false, &settings);
        assert_eq!(text.lines, vec![Line::from(vec![
            Span::styled("Red", red),
            Span::styled("!", red),
            space_span(16, def)
        ])]);
    }

    #[test]
    fn test_paragraph() {
        let settings = mock_settings();
        let s = "<p>Hello world!</p><p>Content</p><p>Goodbye world!</p>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), false, &settings);
        assert_eq!(text.lines.len(), 7);
        assert_eq!(
            text.lines[0],
            Line::from(vec![Span::raw("Hello"), Span::raw(" "), Span::raw("    ")])
        );
        assert_eq!(
            text.lines[1],
            Line::from(vec![Span::raw("world"), Span::raw("!"), Span::raw("    ")])
        );
        assert_eq!(text.lines[2], Line::from(vec![Span::raw("          ")]));
        assert_eq!(text.lines[3], Line::from(vec![Span::raw("Content"), Span::raw("   ")]));
        assert_eq!(text.lines[4], Line::from(vec![Span::raw("          ")]));
        assert_eq!(
            text.lines[5],
            Line::from(vec![Span::raw("Goodbye"), Span::raw(" "), Span::raw("  ")])
        );
        assert_eq!(
            text.lines[6],
            Line::from(vec![Span::raw("world"), Span::raw("!"), Span::raw("    ")])
        );
    }

    #[test]
    fn test_blockquote() {
        let settings = mock_settings();
        let s = "<blockquote>Hello world!</blockquote>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), false, &settings);
        assert_eq!(text.lines.len(), 2);
        assert_eq!(
            text.lines[0],
            Line::from(vec![Span::raw("    "), Span::raw("Hello"), Span::raw(" ")])
        );
        assert_eq!(
            text.lines[1],
            Line::from(vec![Span::raw("    "), Span::raw("world"), Span::raw("!")])
        );
    }

    #[test]
    fn test_list_unordered() {
        let settings = mock_settings();
        let s = "<ul><li>List Item 1</li><li>List Item 2</li><li>List Item 3</li></ul>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(8, Style::default(), false, &settings);
        assert_eq!(text.lines.len(), 6);
        assert_eq!(
            text.lines[0],
            Line::from(vec![
                Span::raw("- "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Line::from(vec![
                Span::raw("  "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("1")
            ])
        );
        assert_eq!(
            text.lines[2],
            Line::from(vec![
                Span::raw("- "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[3],
            Line::from(vec![
                Span::raw("  "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("2")
            ])
        );
        assert_eq!(
            text.lines[4],
            Line::from(vec![
                Span::raw("- "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[5],
            Line::from(vec![
                Span::raw("  "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("3")
            ])
        );
    }

    #[test]
    fn test_list_ordered() {
        let settings = mock_settings();
        let s = "<ol><li>List Item 1</li><li>List Item 2</li><li>List Item 3</li></ol>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(9, Style::default(), false, &settings);
        assert_eq!(text.lines.len(), 6);
        assert_eq!(
            text.lines[0],
            Line::from(vec![
                Span::raw("1. "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Line::from(vec![
                Span::raw("   "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("1")
            ])
        );
        assert_eq!(
            text.lines[2],
            Line::from(vec![
                Span::raw("2. "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[3],
            Line::from(vec![
                Span::raw("   "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("2")
            ])
        );
        assert_eq!(
            text.lines[4],
            Line::from(vec![
                Span::raw("3. "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[5],
            Line::from(vec![
                Span::raw("   "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("3")
            ])
        );
    }

    #[test]
    fn test_table() {
        let settings = mock_settings();
        let s = "<table>\
                 <thead>\
                 <tr><th>Column 1</th><th>Column 2</th><th>Column 3</th></tr>
                 </thead>\
                 <tbody>\
                 <tr><td>a</td><td>b</td><td>c</td></tr>\
                 <tr><td>a</td><td>b</td><td>c</td></tr>\
                 <tr><td>a</td><td>b</td><td>c</td></tr>\
                 </tbody></table>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(15, Style::default(), false, &settings);
        let bold = Style::default().add_modifier(StyleModifier::BOLD);
        assert_eq!(text.lines.len(), 11);

        // Table header
        assert_eq!(text.lines[0].spans, vec![Span::raw("┌────┬────┬───┐")]);
        assert_eq!(text.lines[1].spans, vec![
            Span::raw("│"),
            Span::styled("Colu", bold),
            Span::raw("│"),
            Span::styled("Colu", bold),
            Span::raw("│"),
            Span::styled("Col", bold),
            Span::raw("│")
        ]);
        assert_eq!(text.lines[2].spans, vec![
            Span::raw("│"),
            Span::styled("mn", bold),
            Span::styled(" ", bold),
            Span::styled("1", bold),
            Span::raw("│"),
            Span::styled("mn", bold),
            Span::styled(" ", bold),
            Span::styled("2", bold),
            Span::raw("│"),
            Span::styled("umn", bold),
            Span::raw("│")
        ]);
        assert_eq!(text.lines[3].spans, vec![
            Span::raw("│"),
            Span::raw("    "),
            Span::raw("│"),
            Span::raw("    "),
            Span::raw("│"),
            Span::styled("3", bold),
            Span::styled("  ", bold),
            Span::raw("│")
        ]);

        // First row
        assert_eq!(text.lines[4].spans, vec![Span::raw("├────┼────┼───┤")]);
        assert_eq!(text.lines[5].spans, vec![
            Span::raw("│"),
            Span::raw("a"),
            Span::raw("   "),
            Span::raw("│"),
            Span::raw("b"),
            Span::raw("   "),
            Span::raw("│"),
            Span::raw("c"),
            Span::raw("  "),
            Span::raw("│")
        ]);

        // Second row
        assert_eq!(text.lines[6].spans, vec![Span::raw("├────┼────┼───┤")]);
        assert_eq!(text.lines[7].spans, vec![
            Span::raw("│"),
            Span::raw("a"),
            Span::raw("   "),
            Span::raw("│"),
            Span::raw("b"),
            Span::raw("   "),
            Span::raw("│"),
            Span::raw("c"),
            Span::raw("  "),
            Span::raw("│")
        ]);

        // Third row
        assert_eq!(text.lines[8].spans, vec![Span::raw("├────┼────┼───┤")]);
        assert_eq!(text.lines[9].spans, vec![
            Span::raw("│"),
            Span::raw("a"),
            Span::raw("   "),
            Span::raw("│"),
            Span::raw("b"),
            Span::raw("   "),
            Span::raw("│"),
            Span::raw("c"),
            Span::raw("  "),
            Span::raw("│")
        ]);

        // Bottom ruler
        assert_eq!(text.lines[10].spans, vec![Span::raw("└────┴────┴───┘")]);
    }

    #[test]
    fn test_matrix_reply() {
        let settings = mock_settings();
        let s = "<mx-reply>This was replied to</mx-reply>This is the reply";

        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), false, &settings);
        assert_eq!(text.lines.len(), 4);
        assert_eq!(
            text.lines[0],
            Line::from(vec![
                Span::raw("This"),
                Span::raw(" "),
                Span::raw("was"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Line::from(vec![Span::raw("replied"), Span::raw(" "), Span::raw("to")])
        );
        assert_eq!(
            text.lines[2],
            Line::from(vec![
                Span::raw("This"),
                Span::raw(" "),
                Span::raw("is"),
                Span::raw(" "),
                Span::raw("  ")
            ])
        );
        assert_eq!(
            text.lines[3],
            Line::from(vec![
                Span::raw("the"),
                Span::raw(" "),
                Span::raw("reply"),
                Span::raw(" ")
            ])
        );

        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), true, &settings);
        assert_eq!(text.lines.len(), 2);
        assert_eq!(
            text.lines[0],
            Line::from(vec![
                Span::raw("This"),
                Span::raw(" "),
                Span::raw("is"),
                Span::raw(" "),
                Span::raw("  ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Line::from(vec![
                Span::raw("the"),
                Span::raw(" "),
                Span::raw("reply"),
                Span::raw(" ")
            ])
        );
    }

    #[test]
    fn test_self_closing() {
        let settings = mock_settings();
        let s = "Hello<br>World<br>Goodbye";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(7, Style::default(), true, &settings);
        assert_eq!(text.lines.len(), 3);
        assert_eq!(text.lines[0], Line::from(vec![Span::raw("Hello"), Span::raw("  "),]));
        assert_eq!(text.lines[1], Line::from(vec![Span::raw("World"), Span::raw("  "),]));
        assert_eq!(text.lines[2], Line::from(vec![Span::raw("Goodbye")]),);
    }

    #[test]
    fn test_embedded_newline() {
        let settings = mock_settings();
        let s = "<p>Hello\nWorld</p>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(15, Style::default(), true, &settings);
        assert_eq!(text.lines.len(), 1);
        assert_eq!(
            text.lines[0],
            Line::from(vec![
                Span::raw("Hello"),
                Span::raw(" "),
                Span::raw("World"),
                Span::raw("    ")
            ])
        );
    }

    #[test]
    fn test_pre_tag() {
        let settings = mock_settings();
        let s = concat!(
            "<pre><code class=\"language-rust\">",
            "fn hello() -&gt; usize {\n",
            "    return 5;\n",
            "}\n",
            "</code></pre>\n"
        );
        let tree = parse_matrix_html(s);
        let text = tree.to_text(25, Style::default(), true, &settings);
        let code_style = Style::new().bg(CODE_BACKGROUND);

        assert_eq!(text.lines.len(), 5);
        assert_eq!(
            text.lines[0],
            Line::from(vec![
                Span::raw(line::TOP_LEFT),
                Span::raw(line::HORIZONTAL.repeat(23)),
                Span::raw(line::TOP_RIGHT)
            ])
        );
        assert_eq!(
            text.lines[1],
            Line::from(vec![
                Span::raw(line::VERTICAL),
                Span::styled("fn", code_style),
                Span::styled(" ", code_style),
                Span::styled("hello", code_style),
                Span::styled("(", code_style),
                Span::styled(")", code_style),
                Span::styled(" ", code_style),
                Span::styled("-", code_style),
                Span::styled(">", code_style),
                Span::styled(" ", code_style),
                Span::styled("usize", code_style),
                Span::styled(" ", code_style),
                Span::styled("{", code_style),
                Span::styled("  ", code_style),
                Span::raw(line::VERTICAL)
            ])
        );
        assert_eq!(
            text.lines[2],
            Line::from(vec![
                Span::raw(line::VERTICAL),
                Span::styled("    ", code_style),
                Span::styled("return", code_style),
                Span::styled(" ", code_style),
                Span::styled("5", code_style),
                Span::styled(";", code_style),
                Span::styled("          ", code_style),
                Span::raw(line::VERTICAL)
            ])
        );
        assert_eq!(
            text.lines[3],
            Line::from(vec![
                Span::raw(line::VERTICAL),
                Span::styled("}", code_style),
                Span::styled(" ".repeat(22), code_style),
                Span::raw(line::VERTICAL)
            ])
        );
        assert_eq!(
            text.lines[4],
            Line::from(vec![
                Span::raw(line::BOTTOM_LEFT),
                Span::raw(line::HORIZONTAL.repeat(23)),
                Span::raw(line::BOTTOM_RIGHT)
            ])
        );
    }

    #[test]
    fn test_emoji_shortcodes() {
        let mut enabled = mock_settings();
        enabled.tunables.message_shortcode_display = true;
        let mut disabled = mock_settings();
        disabled.tunables.message_shortcode_display = false;

        for shortcode in ["exploding_head", "polar_bear", "canada"] {
            let emoji = emojis::get_by_shortcode(shortcode).unwrap().as_str();
            let emoji_width = UnicodeWidthStr::width(emoji);
            let replacement = format!(":{shortcode}:");
            let replacement_width = UnicodeWidthStr::width(replacement.as_str());
            let s = format!("<p>{emoji}</p>");
            let tree = parse_matrix_html(s.as_str());
            // Test with emojis_shortcodes set to false
            let text = tree.to_text(20, Style::default(), false, &disabled);
            assert_eq!(text.lines, vec![Line::from(vec![
                Span::raw(emoji),
                space_span(20 - emoji_width, Style::default()),
            ]),]);
            // Test with emojis_shortcodes set to true
            let text = tree.to_text(20, Style::default(), false, &enabled);
            assert_eq!(text.lines, vec![Line::from(vec![
                Span::raw(replacement.as_str()),
                space_span(20 - replacement_width, Style::default()),
            ])]);
        }
    }
}
