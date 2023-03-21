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
use std::ops::Deref;

use css_color_parser::Color as CssColor;
use markup5ever_rcdom::{Handle, NodeData, RcDom};
use unicode_segmentation::UnicodeSegmentation;

use html5ever::{
    driver::{parse_fragment, ParseOpts},
    interface::{Attribute, QualName},
    local_name,
    namespace_url,
    ns,
    tendril::{StrTendril, TendrilSink},
};

use modalkit::tui::{
    layout::Alignment,
    style::{Color, Modifier as StyleModifier, Style},
    symbols::line,
    text::{Span, Spans, Text},
};

use crate::{
    message::printer::TextPrinter,
    util::{join_cell_text, space_text},
};

struct BulletIterator {
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

pub enum CellType {
    Data,
    Header,
}

pub struct TableRow {
    cells: Vec<(CellType, StyleTreeNode)>,
}

impl TableRow {
    fn columns(&self) -> usize {
        self.cells.len()
    }
}

pub struct TableSection {
    rows: Vec<TableRow>,
}

impl TableSection {
    fn columns(&self) -> usize {
        self.rows.iter().map(TableRow::columns).max().unwrap_or(0)
    }
}

pub struct Table {
    caption: Option<Box<StyleTreeNode>>,
    sections: Vec<TableSection>,
}

impl Table {
    fn columns(&self) -> usize {
        self.sections.iter().map(TableSection::columns).max().unwrap_or(0)
    }

    fn to_text(&self, width: usize, style: Style) -> Text {
        let mut text = Text::default();
        let columns = self.columns();
        let cell_total = width.saturating_sub(columns).saturating_sub(1);
        let cell_min = cell_total / columns;
        let mut cell_slop = cell_total - cell_min * columns;
        let cell_widths = (0..columns)
            .into_iter()
            .map(|_| {
                let slopped = cell_slop.min(1);
                cell_slop -= slopped;
                cell_min + slopped
            })
            .collect::<Vec<_>>();

        let mut nrows = 0;

        if let Some(caption) = &self.caption {
            let subw = width.saturating_sub(6);
            let mut printer = TextPrinter::new(subw, style, true).align(Alignment::Center);
            caption.print(&mut printer, style);

            for mut line in printer.finish().lines {
                line.0.insert(0, Span::styled("   ", style));
                line.0.push(Span::styled("   ", style));
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

                text.lines.push(Spans(vec![Span::styled(ruler, style)]));

                let cells = cell_widths
                    .iter()
                    .enumerate()
                    .map(|(i, w)| {
                        let text = if let Some((kind, cell)) = row.cells.get(i) {
                            let style = match kind {
                                CellType::Header => style.add_modifier(StyleModifier::BOLD),
                                CellType::Data => style,
                            };

                            cell.to_text(*w, style)
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
            text.lines.push(Spans(vec![Span::styled(ruler, style)]));
        }

        text
    }
}

pub enum StyleTreeNode {
    Blockquote(Box<StyleTreeNode>),
    Break,
    Code(Box<StyleTreeNode>, Option<String>),
    Header(Box<StyleTreeNode>, usize),
    Image(Option<String>),
    List(StyleTreeChildren, ListStyle),
    Paragraph(Box<StyleTreeNode>),
    Reply(Box<StyleTreeNode>),
    Ruler,
    Style(Box<StyleTreeNode>, Style),
    Table(Table),
    Text(String),
    Sequence(StyleTreeChildren),
}

impl StyleTreeNode {
    pub fn to_text(&self, width: usize, style: Style) -> Text {
        let mut printer = TextPrinter::new(width, style, true);
        self.print(&mut printer, style);
        printer.finish()
    }

    pub fn print<'a>(&'a self, printer: &mut TextPrinter<'a>, style: Style) {
        let width = printer.width();

        match self {
            StyleTreeNode::Blockquote(child) => {
                let mut subp = printer.sub(4);
                child.print(&mut subp, style);

                for mut line in subp.finish() {
                    line.0.insert(0, Span::styled("    ", style));
                    printer.push_line(line);
                }
            },
            StyleTreeNode::Code(child, _) => {
                child.print(printer, style);
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

                        line.0.insert(0, leading);
                        printer.push_line(line);
                    }
                }
            },
            StyleTreeNode::Paragraph(child) => {
                printer.push_break();
                child.print(printer, style);
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
                let text = table.to_text(width, style);
                printer.push_text(text);
            },
            StyleTreeNode::Break => {
                printer.push_break();
            },
            StyleTreeNode::Text(s) => {
                printer.push_str(s.as_str(), style);
            },

            StyleTreeNode::Style(child, patch) => child.print(printer, style.patch(*patch)),
            StyleTreeNode::Sequence(children) => {
                for child in children {
                    child.print(printer, style);
                }
            },
        }
    }
}

pub struct StyleTree {
    children: StyleTreeChildren,
}

impl StyleTree {
    pub fn to_text(&self, width: usize, style: Style, hide_reply: bool) -> Text<'_> {
        let mut printer = TextPrinter::new(width, style, hide_reply);

        for child in self.children.iter() {
            child.print(&mut printer, style);
        }

        printer.finish()
    }
}

fn c2c(handles: &[Handle]) -> Vec<StyleTreeNode> {
    handles.iter().flat_map(h2t).collect()
}

fn c2t(handles: &[Handle]) -> Box<StyleTreeNode> {
    let node = StyleTreeNode::Sequence(c2c(handles));

    Box::new(node)
}

fn get_node(hdl: &Handle, want: &str) -> Option<StyleTreeNode> {
    let node = hdl.deref();

    if let NodeData::Element { name, .. } = &node.data {
        if name.local.as_ref() != want {
            return None;
        }

        let c = c2c(&node.children.borrow());
        return Some(StyleTreeNode::Sequence(c));
    } else {
        return None;
    }
}

fn li2t(hdl: &Handle) -> Option<StyleTreeNode> {
    get_node(hdl, "li")
}

fn table_cell(hdl: &Handle) -> Option<(CellType, StyleTreeNode)> {
    if let Some(node) = get_node(hdl, "th") {
        return Some((CellType::Header, node));
    }

    Some((CellType::Data, get_node(hdl, "td")?))
}

fn table_row(hdl: &Handle) -> Option<TableRow> {
    let node = hdl.deref();

    if let NodeData::Element { name, .. } = &node.data {
        if name.local.as_ref() != "tr" {
            return None;
        }

        let cells = table_cells(&node.children.borrow());
        return Some(TableRow { cells });
    } else {
        return None;
    }
}

fn table_section(hdl: &Handle) -> Option<TableSection> {
    let node = hdl.deref();

    if let NodeData::Element { name, .. } = &node.data {
        match name.local.as_ref() {
            "thead" | "tbody" => {
                let rows = table_rows(&node.children.borrow());

                Some(TableSection { rows })
            },
            _ => None,
        }
    } else {
        return None;
    }
}

fn table_cells(handles: &[Handle]) -> Vec<(CellType, StyleTreeNode)> {
    handles.iter().filter_map(table_cell).collect()
}

fn table_rows(handles: &[Handle]) -> Vec<TableRow> {
    handles.iter().filter_map(table_row).collect()
}

fn table_sections(handles: &[Handle]) -> Vec<TableSection> {
    handles.iter().filter_map(table_section).collect()
}

fn lic2t(handles: &[Handle]) -> StyleTreeChildren {
    handles.iter().filter_map(li2t).collect()
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

fn h2t(hdl: &Handle) -> StyleTreeChildren {
    let node = hdl.deref();

    let tree = match &node.data {
        NodeData::Document => *c2t(node.children.borrow().as_slice()),
        NodeData::Text { contents } => StyleTreeNode::Text(contents.borrow().to_string()),
        NodeData::Element { name, attrs, .. } => {
            match name.local.as_ref() {
                // Message that this one replies to.
                "mx-reply" => StyleTreeNode::Reply(c2t(&node.children.borrow())),

                // Style change
                "b" | "strong" => {
                    let c = c2t(&node.children.borrow());
                    let s = Style::default().add_modifier(StyleModifier::BOLD);

                    StyleTreeNode::Style(c, s)
                },
                "font" => {
                    let c = c2t(&node.children.borrow());
                    let s = attrs_to_style(&attrs.borrow());

                    StyleTreeNode::Style(c, s)
                },
                "em" | "i" => {
                    let c = c2t(&node.children.borrow());
                    let s = Style::default().add_modifier(StyleModifier::ITALIC);

                    StyleTreeNode::Style(c, s)
                },
                "span" => {
                    let c = c2t(&node.children.borrow());
                    let s = attrs_to_style(&attrs.borrow());

                    StyleTreeNode::Style(c, s)
                },
                "del" | "strike" => {
                    let c = c2t(&node.children.borrow());
                    let s = Style::default().add_modifier(StyleModifier::CROSSED_OUT);

                    StyleTreeNode::Style(c, s)
                },
                "u" => {
                    let c = c2t(&node.children.borrow());
                    let s = Style::default().add_modifier(StyleModifier::UNDERLINED);

                    StyleTreeNode::Style(c, s)
                },

                // Lists
                "ol" => StyleTreeNode::List(lic2t(&node.children.borrow()), ListStyle::Ordered),
                "ul" => StyleTreeNode::List(lic2t(&node.children.borrow()), ListStyle::Unordered),

                // Headers
                "h1" => StyleTreeNode::Header(c2t(&node.children.borrow()), 1),
                "h2" => StyleTreeNode::Header(c2t(&node.children.borrow()), 2),
                "h3" => StyleTreeNode::Header(c2t(&node.children.borrow()), 3),
                "h4" => StyleTreeNode::Header(c2t(&node.children.borrow()), 4),
                "h5" => StyleTreeNode::Header(c2t(&node.children.borrow()), 5),
                "h6" => StyleTreeNode::Header(c2t(&node.children.borrow()), 6),

                // Table
                "table" => {
                    let sections = table_sections(&node.children.borrow());
                    let caption = node
                        .children
                        .borrow()
                        .iter()
                        .find_map(|hdl| get_node(hdl, "caption"))
                        .map(Box::new);
                    let table = Table { caption, sections };

                    StyleTreeNode::Table(table)
                },

                // Code blocks.
                "code" => {
                    let c = c2t(&node.children.borrow());
                    let l = attrs_to_language(&attrs.borrow());

                    StyleTreeNode::Code(c, l)
                },

                // Other text blocks.
                "blockquote" => StyleTreeNode::Blockquote(c2t(&node.children.borrow())),
                "div" | "p" => StyleTreeNode::Paragraph(c2t(&node.children.borrow())),

                // No children.
                "hr" => StyleTreeNode::Ruler,
                "br" => StyleTreeNode::Break,

                "img" => StyleTreeNode::Image(attrs_to_alt(&attrs.borrow())),

                // These don't render in any special way.
                "a" | "details" | "html" | "pre" | "summary" | "sub" | "sup" => {
                    *c2t(&node.children.borrow())
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
    StyleTree { children: h2t(&dom.document) }
}

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
    use crate::util::space_span;

    #[test]
    fn test_header() {
        let bold = Style::default().add_modifier(StyleModifier::BOLD);

        let s = "<h1>Header 1</h1>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("#", bold),
            Span::styled(" ", bold),
            Span::styled("Header", bold),
            Span::styled(" ", bold),
            Span::styled("1", bold),
            space_span(10, Style::default())
        ])]);

        let s = "<h2>Header 2</h2>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
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
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
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
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
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
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
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
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
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
        let def = Style::default();
        let bold = def.add_modifier(StyleModifier::BOLD);
        let italic = def.add_modifier(StyleModifier::ITALIC);
        let strike = def.add_modifier(StyleModifier::CROSSED_OUT);
        let underl = def.add_modifier(StyleModifier::UNDERLINED);
        let red = def.fg(Color::Rgb(0xff, 0x00, 0x00));

        let s = "<b>Bold!</b>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Bold", bold),
            Span::styled("!", bold),
            space_span(15, def)
        ])]);

        let s = "<strong>Bold!</strong>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Bold", bold),
            Span::styled("!", bold),
            space_span(15, def)
        ])]);

        let s = "<i>Italic!</i>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Italic", italic),
            Span::styled("!", italic),
            space_span(13, def)
        ])]);

        let s = "<em>Italic!</em>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Italic", italic),
            Span::styled("!", italic),
            space_span(13, def)
        ])]);

        let s = "<del>Strikethrough!</del>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Strikethrough", strike),
            Span::styled("!", strike),
            space_span(6, def)
        ])]);

        let s = "<strike>Strikethrough!</strike>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Strikethrough", strike),
            Span::styled("!", strike),
            space_span(6, def)
        ])]);

        let s = "<u>Underline!</u>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Underline", underl),
            Span::styled("!", underl),
            space_span(10, def)
        ])]);

        let s = "<font color=\"#ff0000\">Red!</u>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Red", red),
            Span::styled("!", red),
            space_span(16, def)
        ])]);

        let s = "<font color=\"red\">Red!</u>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(20, Style::default(), false);
        assert_eq!(text.lines, vec![Spans(vec![
            Span::styled("Red", red),
            Span::styled("!", red),
            space_span(16, def)
        ])]);
    }

    #[test]
    fn test_paragraph() {
        let s = "<p>Hello world!</p><p>Content</p><p>Goodbye world!</p>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), false);
        assert_eq!(text.lines.len(), 7);
        assert_eq!(
            text.lines[0],
            Spans(vec![Span::raw("Hello"), Span::raw(" "), Span::raw("    ")])
        );
        assert_eq!(
            text.lines[1],
            Spans(vec![Span::raw("world"), Span::raw("!"), Span::raw("    ")])
        );
        assert_eq!(text.lines[2], Spans(vec![Span::raw("          ")]));
        assert_eq!(text.lines[3], Spans(vec![Span::raw("Content"), Span::raw("   ")]));
        assert_eq!(text.lines[4], Spans(vec![Span::raw("          ")]));
        assert_eq!(
            text.lines[5],
            Spans(vec![Span::raw("Goodbye"), Span::raw(" "), Span::raw("  ")])
        );
        assert_eq!(
            text.lines[6],
            Spans(vec![Span::raw("world"), Span::raw("!"), Span::raw("    ")])
        );
    }

    #[test]
    fn test_blockquote() {
        let s = "<blockquote>Hello world!</blockquote>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), false);
        assert_eq!(text.lines.len(), 2);
        assert_eq!(
            text.lines[0],
            Spans(vec![Span::raw("    "), Span::raw("Hello"), Span::raw(" ")])
        );
        assert_eq!(
            text.lines[1],
            Spans(vec![Span::raw("    "), Span::raw("world"), Span::raw("!")])
        );
    }

    #[test]
    fn test_list_unordered() {
        let s = "<ul><li>List Item 1</li><li>List Item 2</li><li>List Item 3</li></ul>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(8, Style::default(), false);
        assert_eq!(text.lines.len(), 6);
        assert_eq!(
            text.lines[0],
            Spans(vec![
                Span::raw("- "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Spans(vec![
                Span::raw("  "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("1")
            ])
        );
        assert_eq!(
            text.lines[2],
            Spans(vec![
                Span::raw("- "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[3],
            Spans(vec![
                Span::raw("  "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("2")
            ])
        );
        assert_eq!(
            text.lines[4],
            Spans(vec![
                Span::raw("- "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[5],
            Spans(vec![
                Span::raw("  "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("3")
            ])
        );
    }

    #[test]
    fn test_list_ordered() {
        let s = "<ol><li>List Item 1</li><li>List Item 2</li><li>List Item 3</li></ol>";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(9, Style::default(), false);
        assert_eq!(text.lines.len(), 6);
        assert_eq!(
            text.lines[0],
            Spans(vec![
                Span::raw("1. "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Spans(vec![
                Span::raw("   "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("1")
            ])
        );
        assert_eq!(
            text.lines[2],
            Spans(vec![
                Span::raw("2. "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[3],
            Spans(vec![
                Span::raw("   "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("2")
            ])
        );
        assert_eq!(
            text.lines[4],
            Spans(vec![
                Span::raw("3. "),
                Span::raw("List"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[5],
            Spans(vec![
                Span::raw("   "),
                Span::raw("Item"),
                Span::raw(" "),
                Span::raw("3")
            ])
        );
    }

    #[test]
    fn test_table() {
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
        let text = tree.to_text(15, Style::default(), false);
        let bold = Style::default().add_modifier(StyleModifier::BOLD);
        assert_eq!(text.lines.len(), 11);

        // Table header
        assert_eq!(text.lines[0].0, vec![Span::raw("┌────┬────┬───┐")]);
        assert_eq!(text.lines[1].0, vec![
            Span::raw("│"),
            Span::styled("Colu", bold),
            Span::raw("│"),
            Span::styled("Colu", bold),
            Span::raw("│"),
            Span::styled("Col", bold),
            Span::raw("│")
        ]);
        assert_eq!(text.lines[2].0, vec![
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
        assert_eq!(text.lines[3].0, vec![
            Span::raw("│"),
            Span::raw("    "),
            Span::raw("│"),
            Span::raw("    "),
            Span::raw("│"),
            Span::styled(" ", bold),
            Span::styled("3", bold),
            Span::styled(" ", bold),
            Span::raw("│")
        ]);

        // First row
        assert_eq!(text.lines[4].0, vec![Span::raw("├────┼────┼───┤")]);
        assert_eq!(text.lines[5].0, vec![
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
        assert_eq!(text.lines[6].0, vec![Span::raw("├────┼────┼───┤")]);
        assert_eq!(text.lines[7].0, vec![
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
        assert_eq!(text.lines[8].0, vec![Span::raw("├────┼────┼───┤")]);
        assert_eq!(text.lines[9].0, vec![
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
        assert_eq!(text.lines[10].0, vec![Span::raw("└────┴────┴───┘")]);
    }

    #[test]
    fn test_matrix_reply() {
        let s = "<mx-reply>This was replied to</mx-reply>This is the reply";

        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), false);
        assert_eq!(text.lines.len(), 4);
        assert_eq!(
            text.lines[0],
            Spans(vec![
                Span::raw("This"),
                Span::raw(" "),
                Span::raw("was"),
                Span::raw(" "),
                Span::raw(" ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Spans(vec![Span::raw("replied"), Span::raw(" "), Span::raw("to")])
        );
        assert_eq!(
            text.lines[2],
            Spans(vec![
                Span::raw("This"),
                Span::raw(" "),
                Span::raw("is"),
                Span::raw(" "),
                Span::raw("  ")
            ])
        );
        assert_eq!(
            text.lines[3],
            Spans(vec![
                Span::raw("the"),
                Span::raw(" "),
                Span::raw("reply"),
                Span::raw(" ")
            ])
        );

        let tree = parse_matrix_html(s);
        let text = tree.to_text(10, Style::default(), true);
        assert_eq!(text.lines.len(), 2);
        assert_eq!(
            text.lines[0],
            Spans(vec![
                Span::raw("This"),
                Span::raw(" "),
                Span::raw("is"),
                Span::raw(" "),
                Span::raw("  ")
            ])
        );
        assert_eq!(
            text.lines[1],
            Spans(vec![
                Span::raw("the"),
                Span::raw(" "),
                Span::raw("reply"),
                Span::raw(" ")
            ])
        );
    }

    #[test]
    fn test_self_closing() {
        let s = "Hello<br>World<br>Goodbye";
        let tree = parse_matrix_html(s);
        let text = tree.to_text(7, Style::default(), true);
        assert_eq!(text.lines.len(), 3);
        assert_eq!(text.lines[0], Spans(vec![Span::raw("Hello"), Span::raw("  "),]));
        assert_eq!(text.lines[1], Spans(vec![Span::raw("World"), Span::raw("  "),]));
        assert_eq!(text.lines[2], Spans(vec![Span::raw("Goodbye")]),);
    }
}
