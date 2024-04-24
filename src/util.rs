//! # Utility functions
use std::borrow::Cow;

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use ratatui::style::Style;
use ratatui::text::{Line, Span, Text};

pub fn split_cow(cow: Cow<'_, str>, idx: usize) -> (Cow<'_, str>, Cow<'_, str>) {
    match cow {
        Cow::Borrowed(s) => {
            let s1 = Cow::Borrowed(&s[idx..]);
            let s0 = Cow::Borrowed(&s[..idx]);

            (s0, s1)
        },
        Cow::Owned(mut s) => {
            let s1 = Cow::Owned(s.split_off(idx));
            let s0 = Cow::Owned(s);

            (s0, s1)
        },
    }
}

pub fn take_width(s: Cow<'_, str>, width: usize) -> ((Cow<'_, str>, usize), Cow<'_, str>) {
    // Find where to split the line.
    let mut w = 0;

    let idx = UnicodeSegmentation::grapheme_indices(s.as_ref(), true)
        .find_map(|(i, g)| {
            let gw = UnicodeWidthStr::width(g);
            if w + gw > width {
                Some(i)
            } else {
                w += gw;
                None
            }
        })
        .unwrap_or(s.len());

    let (s0, s1) = split_cow(s, idx);

    ((s0, w), s1)
}

pub struct WrappedLinesIterator<'a> {
    iter: std::vec::IntoIter<Cow<'a, str>>,
    curr: Option<Cow<'a, str>>,
    width: usize,
}

impl<'a> WrappedLinesIterator<'a> {
    fn new<T>(input: T, width: usize) -> Self
    where
        T: Into<Cow<'a, str>>,
    {
        let width = width.max(2);

        let cows: Vec<Cow<'a, str>> = match input.into() {
            Cow::Borrowed(s) => s.lines().map(Cow::Borrowed).collect(),
            Cow::Owned(s) => s.lines().map(ToOwned::to_owned).map(Cow::Owned).collect(),
        };

        WrappedLinesIterator { iter: cows.into_iter(), curr: None, width }
    }
}

impl<'a> Iterator for WrappedLinesIterator<'a> {
    type Item = (Cow<'a, str>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr.is_none() {
            self.curr = self.iter.next();
        }

        if let Some(s) = self.curr.take() {
            let width = UnicodeWidthStr::width(s.as_ref());

            if width <= self.width {
                return Some((s, width));
            } else {
                let (prefix, s1) = take_width(s, self.width);
                self.curr = Some(s1);
                return Some(prefix);
            }
        } else {
            return None;
        }
    }
}

pub fn wrap<'a, T>(input: T, width: usize) -> WrappedLinesIterator<'a>
where
    T: Into<Cow<'a, str>>,
{
    WrappedLinesIterator::new(input, width)
}

pub fn wrapped_text<'a, T>(s: T, width: usize, style: Style) -> Text<'a>
where
    T: Into<Cow<'a, str>>,
{
    let mut text = Text::default();

    for (line, w) in wrap(s, width) {
        let space = space_span(width.saturating_sub(w), style);
        let spans = Line::from(vec![Span::styled(line, style), space]);

        text.lines.push(spans);
    }

    return text;
}

pub fn space(width: usize) -> String {
    " ".repeat(width)
}

pub fn space_span(width: usize, style: Style) -> Span<'static> {
    Span::styled(space(width), style)
}

pub fn space_text(width: usize, style: Style) -> Text<'static> {
    space_span(width, style).into()
}

pub fn join_cell_text<'a>(texts: Vec<(Text<'a>, usize)>, join: Span<'a>, style: Style) -> Text<'a> {
    let height = texts.iter().map(|t| t.0.height()).max().unwrap_or(0);
    let mut text = Text::from(vec![Line::from(vec![join.clone()]); height]);

    for (mut t, w) in texts.into_iter() {
        for i in 0..height {
            if let Some(line) = t.lines.get_mut(i) {
                text.lines[i].spans.append(&mut line.spans);
            } else {
                text.lines[i].spans.push(space_span(w, style));
            }

            text.lines[i].spans.push(join.clone());
        }
    }

    text
}

fn replace_emoji_in_grapheme(grapheme: &str) -> String {
    emojis::get(grapheme)
        .and_then(|emoji| emoji.shortcode())
        .map(|shortcode| format!(":{shortcode}:"))
        .unwrap_or_else(|| grapheme.to_owned())
}

pub fn replace_emojis_in_str(s: &str) -> String {
    let graphemes = s.graphemes(true);
    graphemes.map(replace_emoji_in_grapheme).collect()
}

pub fn replace_emojis_in_span(span: &mut Span) {
    span.content = Cow::Owned(replace_emojis_in_str(span.content.as_ref()))
}

pub fn replace_emojis_in_line(line: &mut Line) {
    for span in &mut line.spans {
        replace_emojis_in_span(span);
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_wrapped_lines_ascii() {
        let s = "hello world!\nabcdefghijklmnopqrstuvwxyz\ngoodbye";

        let mut iter = wrap(s, 100);
        assert_eq!(iter.next(), Some((Cow::Borrowed("hello world!"), 12)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("abcdefghijklmnopqrstuvwxyz"), 26)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("goodbye"), 7)));
        assert_eq!(iter.next(), None);

        let mut iter = wrap(s, 5);
        assert_eq!(iter.next(), Some((Cow::Borrowed("hello"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed(" worl"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("d!"), 2)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("abcde"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("fghij"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("klmno"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("pqrst"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("uvwxy"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("z"), 1)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("goodb"), 5)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("ye"), 2)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_wrapped_lines_unicode() {
        let s = "ＣＨＩＣＫＥＮ";

        let mut iter = wrap(s, 14);
        assert_eq!(iter.next(), Some((Cow::Borrowed(s), 14)));
        assert_eq!(iter.next(), None);

        let mut iter = wrap(s, 5);
        assert_eq!(iter.next(), Some((Cow::Borrowed("ＣＨ"), 4)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("ＩＣ"), 4)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("ＫＥ"), 4)));
        assert_eq!(iter.next(), Some((Cow::Borrowed("Ｎ"), 2)));
        assert_eq!(iter.next(), None);
    }
}
