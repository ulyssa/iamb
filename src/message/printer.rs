use std::borrow::Cow;

use modalkit::tui::layout::Alignment;
use modalkit::tui::style::Style;
use modalkit::tui::text::{Span, Spans, Text};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::util::{space_span, take_width};

pub struct TextPrinter<'a> {
    text: Text<'a>,
    width: usize,
    base_style: Style,
    hide_reply: bool,

    alignment: Alignment,
    curr_spans: Vec<Span<'a>>,
    curr_width: usize,
    literal: bool,
}

impl<'a> TextPrinter<'a> {
    pub fn new(width: usize, base_style: Style, hide_reply: bool) -> Self {
        TextPrinter {
            text: Text::default(),
            width,
            base_style,
            hide_reply,

            alignment: Alignment::Left,
            curr_spans: vec![],
            curr_width: 0,
            literal: false,
        }
    }

    pub fn align(mut self, alignment: Alignment) -> Self {
        self.alignment = alignment;
        self
    }

    pub fn literal(mut self, literal: bool) -> Self {
        self.literal = literal;
        self
    }

    pub fn hide_reply(&self) -> bool {
        self.hide_reply
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn sub(&self, indent: usize) -> Self {
        TextPrinter {
            text: Text::default(),
            width: self.width.saturating_sub(indent),
            base_style: self.base_style,
            hide_reply: self.hide_reply,

            alignment: self.alignment,
            curr_spans: vec![],
            curr_width: 0,
            literal: self.literal,
        }
    }

    fn remaining(&self) -> usize {
        self.width - self.curr_width
    }

    pub fn commit(&mut self) {
        if self.curr_width > 0 {
            self.push_break();
        }
    }

    fn push(&mut self) {
        self.curr_width = 0;
        self.text.lines.push(Spans(std::mem::take(&mut self.curr_spans)));
    }

    pub fn push_break(&mut self) {
        if self.curr_width == 0 && self.text.lines.is_empty() {
            // Disallow leading breaks.
            return;
        }

        let remaining = self.remaining();

        if remaining > 0 {
            match self.alignment {
                Alignment::Left => {
                    let tspan = space_span(remaining, self.base_style);
                    self.curr_spans.push(tspan);
                },
                Alignment::Center => {
                    let trailing = remaining / 2;
                    let leading = remaining - trailing;

                    let tspan = space_span(trailing, self.base_style);
                    let lspan = space_span(leading, self.base_style);

                    self.curr_spans.push(tspan);
                    self.curr_spans.insert(0, lspan);
                },
                Alignment::Right => {
                    let lspan = space_span(remaining, self.base_style);
                    self.curr_spans.insert(0, lspan);
                },
            }
        }

        self.push();
    }

    fn push_str_wrapped<T>(&mut self, s: T, style: Style)
    where
        T: Into<Cow<'a, str>>,
    {
        let style = self.base_style.patch(style);
        let mut cow = s.into();

        loop {
            let sw = UnicodeWidthStr::width(cow.as_ref());

            if self.curr_width + sw <= self.width {
                // The text fits within the current line.
                self.curr_spans.push(Span::styled(cow, style));
                self.curr_width += sw;
                break;
            }

            // Take a leading portion of the text that fits in the line.
            let ((s0, w), s1) = take_width(cow, self.remaining());
            cow = s1;

            self.curr_spans.push(Span::styled(s0, style));
            self.curr_width += w;

            self.commit();
        }

        if self.curr_width == self.width {
            // If the last bit fills the full line, start a new one.
            self.push();
        }
    }

    pub fn push_span_nobreak(&mut self, span: Span<'a>) {
        let sw = UnicodeWidthStr::width(span.content.as_ref());

        if self.curr_width + sw > self.width {
            // Span doesn't fit on this line, so start a new one.
            self.commit();
        }

        self.curr_spans.push(span);
        self.curr_width += sw;
    }

    pub fn push_str(&mut self, s: &'a str, style: Style) {
        let style = self.base_style.patch(style);

        if self.width == 0 {
            return;
        }

        for mut word in UnicodeSegmentation::split_word_bounds(s) {
            if let "\n" | "\r\n" = word {
                if self.literal {
                    self.commit();
                    continue;
                }

                // Render embedded newlines as spaces.
                word = " ";
            }

            if !self.literal && self.curr_width == 0 && word.chars().all(char::is_whitespace) {
                // Drop leading whitespace.
                continue;
            }

            let sw = UnicodeWidthStr::width(word);

            if sw > self.width {
                self.push_str_wrapped(word, style);
                continue;
            }

            if self.curr_width + sw > self.width {
                // Word doesn't fit on this line, so start a new one.
                self.commit();

                if !self.literal && word.chars().all(char::is_whitespace) {
                    // Drop leading whitespace.
                    continue;
                }
            }

            let span = Span::styled(word, style);
            self.curr_spans.push(span);
            self.curr_width += sw;
        }

        if self.curr_width == self.width {
            // If the last bit fills the full line, start a new one.
            self.push();
        }
    }

    pub fn push_line(&mut self, spans: Spans<'a>) {
        self.commit();
        self.text.lines.push(spans);
    }

    pub fn push_text(&mut self, text: Text<'a>) {
        self.commit();
        self.text.lines.extend(text.lines);
    }

    pub fn finish(mut self) -> Text<'a> {
        self.commit();
        self.text
    }
}
