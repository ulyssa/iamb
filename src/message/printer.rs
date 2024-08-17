//! # Line Wrapping Logic
//!
//! The [TextPrinter] handles wrapping stylized text and inserting spaces for padding at the end of
//! lines to make concatenation work right (e.g., combining table cells after wrapping their
//! contents).
use std::borrow::Cow;

use ratatui::layout::Alignment;
use ratatui::style::Style;
use ratatui::text::{Line, Span, Text};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::util::{
    replace_emojis_in_line,
    replace_emojis_in_span,
    replace_emojis_in_str,
    space_span,
    take_width,
};

/// Wrap styled text for the current terminal width.
pub struct TextPrinter<'a> {
    text: Text<'a>,
    width: usize,
    base_style: Style,
    hide_reply: bool,
    emoji_shortcodes: bool,

    alignment: Alignment,
    curr_spans: Vec<Span<'a>>,
    curr_width: usize,
    literal: bool,
}

impl<'a> TextPrinter<'a> {
    /// Create a new printer.
    pub fn new(width: usize, base_style: Style, hide_reply: bool, emoji_shortcodes: bool) -> Self {
        TextPrinter {
            text: Text::default(),
            width,
            base_style,
            hide_reply,
            emoji_shortcodes,

            alignment: Alignment::Left,
            curr_spans: vec![],
            curr_width: 0,
            literal: false,
        }
    }

    /// Configure the alignment for each line.
    pub fn align(mut self, alignment: Alignment) -> Self {
        self.alignment = alignment;
        self
    }

    /// Set whether newlines should be treated literally, or turned into spaces.
    pub fn literal(mut self, literal: bool) -> Self {
        self.literal = literal;
        self
    }

    /// Indicates whether replies should be pushed to the printer.
    pub fn hide_reply(&self) -> bool {
        self.hide_reply
    }

    /// Indicates whether emojis should be replaced by shortcodes
    pub fn emoji_shortcodes(&self) -> bool {
        self.emoji_shortcodes
    }

    /// Indicates the current printer's width.
    pub fn width(&self) -> usize {
        self.width
    }

    /// Create a new printer with a smaller width.
    pub fn sub(&self, indent: usize) -> Self {
        TextPrinter {
            text: Text::default(),
            width: self.width.saturating_sub(indent),
            base_style: self.base_style,
            hide_reply: self.hide_reply,
            emoji_shortcodes: self.emoji_shortcodes,

            alignment: self.alignment,
            curr_spans: vec![],
            curr_width: 0,
            literal: self.literal,
        }
    }

    fn remaining(&self) -> usize {
        self.width.saturating_sub(self.curr_width)
    }

    /// If there is any text on the current line, start a new one.
    pub fn commit(&mut self) {
        if self.curr_width > 0 {
            self.push_break();
        }
    }

    fn push(&mut self) {
        self.curr_width = 0;
        self.text.lines.push(Line::from(std::mem::take(&mut self.curr_spans)));
    }

    /// Start a new line.
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

    /// Push a [Span] that isn't allowed to break across lines.
    pub fn push_span_nobreak(&mut self, mut span: Span<'a>) {
        if self.emoji_shortcodes {
            replace_emojis_in_span(&mut span);
        }
        let sw = UnicodeWidthStr::width(span.content.as_ref());

        if self.curr_width + sw > self.width {
            // Span doesn't fit on this line, so start a new one.
            self.commit();
        }

        self.curr_spans.push(span);
        self.curr_width += sw;
    }

    /// Push text with a [Style].
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

            let cow = if self.emoji_shortcodes {
                Cow::Owned(replace_emojis_in_str(word))
            } else {
                Cow::Borrowed(word)
            };
            let sw = UnicodeWidthStr::width(cow.as_ref());

            if sw > self.width {
                self.push_str_wrapped(cow, style);
                continue;
            }

            if self.curr_width + sw > self.width {
                // Word doesn't fit on this line, so start a new one.
                self.commit();

                if !self.literal && cow.chars().all(char::is_whitespace) {
                    // Drop leading whitespace.
                    continue;
                }
            }

            let span = Span::styled(cow, style);
            self.curr_spans.push(span);
            self.curr_width += sw;
        }

        if self.curr_width == self.width {
            // If the last bit fills the full line, start a new one.
            self.push();
        }
    }

    /// Push a [Line] into the printer.
    pub fn push_line(&mut self, mut line: Line<'a>) {
        self.commit();
        if self.emoji_shortcodes {
            replace_emojis_in_line(&mut line);
        }
        self.text.lines.push(line);
    }

    /// Push multiline [Text] into the printer.
    pub fn push_text(&mut self, mut text: Text<'a>) {
        self.commit();
        if self.emoji_shortcodes {
            for line in &mut text.lines {
                replace_emojis_in_line(line);
            }
        }
        self.text.lines.extend(text.lines);
    }

    /// Render the contents of this printer as [Text].
    pub fn finish(mut self) -> Text<'a> {
        self.commit();
        self.text
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_push_nobreak() {
        let mut printer = TextPrinter::new(5, Style::default(), false, false);
        printer.push_span_nobreak("hello world".into());
        let text = printer.finish();
        assert_eq!(text.lines.len(), 1);
        assert_eq!(text.lines[0].spans.len(), 1);
        assert_eq!(text.lines[0].spans[0].content, "hello world");
    }
}
