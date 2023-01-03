use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};
use std::str::Lines;

use chrono::{DateTime, NaiveDateTime, Utc};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use matrix_sdk::ruma::{
    events::{
        room::message::{MessageType, RoomMessageEventContent},
        MessageLikeEvent,
    },
    MilliSecondsSinceUnixEpoch,
    OwnedEventId,
    OwnedUserId,
    UInt,
};

use modalkit::tui::{
    style::{Color, Modifier as StyleModifier, Style},
    text::{Span, Spans, Text},
};

use modalkit::editing::{base::ViewportContext, cursor::Cursor};

use crate::base::{IambResult, RoomInfo};

pub type MessageEvent = MessageLikeEvent<RoomMessageEventContent>;
pub type MessageFetchResult = IambResult<(Option<String>, Vec<MessageEvent>)>;
pub type MessageKey = (MessageTimeStamp, OwnedEventId);
pub type Messages = BTreeMap<MessageKey, Message>;

const COLORS: [Color; 13] = [
    Color::Blue,
    Color::Cyan,
    Color::Green,
    Color::LightBlue,
    Color::LightGreen,
    Color::LightCyan,
    Color::LightMagenta,
    Color::LightRed,
    Color::LightYellow,
    Color::Magenta,
    Color::Red,
    Color::Reset,
    Color::Yellow,
];

const USER_GUTTER: usize = 30;
const TIME_GUTTER: usize = 12;
const MIN_MSG_LEN: usize = 30;

const USER_GUTTER_EMPTY: &str = "                              ";
const USER_GUTTER_EMPTY_SPAN: Span<'static> = Span {
    content: Cow::Borrowed(USER_GUTTER_EMPTY),
    style: Style {
        fg: None,
        bg: None,
        add_modifier: StyleModifier::empty(),
        sub_modifier: StyleModifier::empty(),
    },
};

pub(crate) fn user_color(user: &str) -> Color {
    let mut hasher = DefaultHasher::new();
    user.hash(&mut hasher);
    let color = hasher.finish() as usize % COLORS.len();

    COLORS[color]
}

pub(crate) fn user_style(user: &str) -> Style {
    Style::default().fg(user_color(user)).add_modifier(StyleModifier::BOLD)
}

struct WrappedLinesIterator<'a> {
    iter: Lines<'a>,
    curr: Option<&'a str>,
    width: usize,
}

impl<'a> WrappedLinesIterator<'a> {
    fn new(input: &'a str, width: usize) -> Self {
        WrappedLinesIterator { iter: input.lines(), curr: None, width }
    }
}

impl<'a> Iterator for WrappedLinesIterator<'a> {
    type Item = (&'a str, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr.is_none() {
            self.curr = self.iter.next();
        }

        if let Some(s) = self.curr.take() {
            let width = UnicodeWidthStr::width(s);

            if width <= self.width {
                return Some((s, width));
            } else {
                // Find where to split the line.
                let mut width = 0;
                let mut idx = 0;

                for (i, g) in UnicodeSegmentation::grapheme_indices(s, true) {
                    let gw = UnicodeWidthStr::width(g);
                    idx = i;

                    if width + gw > self.width {
                        break;
                    }

                    width += gw;
                }

                self.curr = Some(&s[idx..]);

                return Some((&s[..idx], width));
            }
        } else {
            return None;
        }
    }
}

fn wrap(input: &str, width: usize) -> WrappedLinesIterator<'_> {
    WrappedLinesIterator::new(input, width)
}

fn space(width: usize) -> String {
    " ".repeat(width)
}

#[derive(thiserror::Error, Debug)]
pub enum TimeStampIntError {
    #[error("Integer conversion error: {0}")]
    IntError(#[from] std::num::TryFromIntError),

    #[error("UInt conversion error: {0}")]
    UIntError(<UInt as TryFrom<u64>>::Error),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MessageTimeStamp {
    OriginServer(UInt),
    LocalEcho,
}

impl MessageTimeStamp {
    fn show(&self) -> Option<Span> {
        match self {
            MessageTimeStamp::OriginServer(ts) => {
                let time = i64::from(*ts) / 1000;
                let time = NaiveDateTime::from_timestamp_opt(time, 0)?;
                let time = DateTime::<Utc>::from_utc(time, Utc);
                let time = time.format("%T");
                let time = format!("  [{}]", time);

                Span::raw(time).into()
            },
            MessageTimeStamp::LocalEcho => None,
        }
    }

    fn is_local_echo(&self) -> bool {
        matches!(self, MessageTimeStamp::LocalEcho)
    }
}

impl Ord for MessageTimeStamp {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (MessageTimeStamp::OriginServer(_), MessageTimeStamp::LocalEcho) => Ordering::Less,
            (MessageTimeStamp::OriginServer(a), MessageTimeStamp::OriginServer(b)) => a.cmp(b),
            (MessageTimeStamp::LocalEcho, MessageTimeStamp::OriginServer(_)) => Ordering::Greater,
            (MessageTimeStamp::LocalEcho, MessageTimeStamp::LocalEcho) => Ordering::Equal,
        }
    }
}

impl PartialOrd for MessageTimeStamp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp(other).into()
    }
}

impl From<MilliSecondsSinceUnixEpoch> for MessageTimeStamp {
    fn from(millis: MilliSecondsSinceUnixEpoch) -> Self {
        MessageTimeStamp::OriginServer(millis.0)
    }
}

impl TryFrom<&MessageTimeStamp> for usize {
    type Error = TimeStampIntError;

    fn try_from(ts: &MessageTimeStamp) -> Result<Self, Self::Error> {
        let n = match ts {
            MessageTimeStamp::LocalEcho => 0,
            MessageTimeStamp::OriginServer(u) => usize::try_from(u64::from(*u))?,
        };

        Ok(n)
    }
}

impl TryFrom<usize> for MessageTimeStamp {
    type Error = TimeStampIntError;

    fn try_from(u: usize) -> Result<Self, Self::Error> {
        if u == 0 {
            Ok(MessageTimeStamp::LocalEcho)
        } else {
            let n = u64::try_from(u)?;
            let n = UInt::try_from(n).map_err(TimeStampIntError::UIntError)?;

            Ok(MessageTimeStamp::OriginServer(n))
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct MessageCursor {
    /// When timestamp is None, the corner is determined by moving backwards from
    /// the most recently received message.
    pub timestamp: Option<MessageKey>,

    /// A row within the [Text] representation of a [Message].
    pub text_row: usize,
}

impl MessageCursor {
    pub fn new(timestamp: MessageKey, text_row: usize) -> Self {
        MessageCursor { timestamp: Some(timestamp), text_row }
    }

    /// Get a cursor that refers to the most recent message.
    pub fn latest() -> Self {
        MessageCursor::default()
    }

    pub fn to_key<'a>(&'a self, info: &'a RoomInfo) -> Option<&'a MessageKey> {
        if let Some(ref key) = self.timestamp {
            Some(key)
        } else {
            Some(info.messages.last_key_value()?.0)
        }
    }

    pub fn from_cursor(cursor: &Cursor, info: &RoomInfo) -> Option<Self> {
        let ev_hash = u64::try_from(cursor.get_x()).ok()?;
        let ev_term = OwnedEventId::try_from("$").ok()?;

        let ts_start = MessageTimeStamp::try_from(cursor.get_y()).ok()?;
        let start = (ts_start, ev_term);
        let mut mc = None;

        for ((ts, event_id), _) in info.messages.range(start..) {
            let mut hasher = DefaultHasher::new();
            event_id.hash(&mut hasher);

            if hasher.finish() == ev_hash {
                mc = Self::from((*ts, event_id.clone())).into();
                break;
            }

            if mc.is_none() {
                mc = Self::from((*ts, event_id.clone())).into();
            }

            if ts > &ts_start {
                break;
            }
        }

        return mc;
    }

    pub fn to_cursor(&self, info: &RoomInfo) -> Option<Cursor> {
        let (ts, event_id) = self.to_key(info)?;

        let y: usize = usize::try_from(ts).ok()?;

        let mut hasher = DefaultHasher::new();
        event_id.hash(&mut hasher);
        let x = usize::try_from(hasher.finish()).ok()?;

        Cursor::new(y, x).into()
    }
}

impl From<Option<MessageKey>> for MessageCursor {
    fn from(key: Option<MessageKey>) -> Self {
        MessageCursor { timestamp: key, text_row: 0 }
    }
}

impl From<MessageKey> for MessageCursor {
    fn from(key: MessageKey) -> Self {
        MessageCursor { timestamp: Some(key), text_row: 0 }
    }
}

impl Ord for MessageCursor {
    fn cmp(&self, other: &Self) -> Ordering {
        match (&self.timestamp, &other.timestamp) {
            (None, None) => self.text_row.cmp(&other.text_row),
            (None, Some(_)) => Ordering::Greater,
            (Some(_), None) => Ordering::Less,
            (Some(st), Some(ot)) => {
                let pcmp = st.cmp(ot);
                let tcmp = self.text_row.cmp(&other.text_row);

                pcmp.then(tcmp)
            },
        }
    }
}

impl PartialOrd for MessageCursor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp(other).into()
    }
}

#[derive(Clone)]
pub enum MessageContent {
    Original(Box<RoomMessageEventContent>),
    Redacted,
}

impl AsRef<str> for MessageContent {
    fn as_ref(&self) -> &str {
        match self {
            MessageContent::Original(ev) => {
                match &ev.msgtype {
                    MessageType::Text(content) => {
                        return content.body.as_ref();
                    },
                    MessageType::Emote(content) => {
                        return content.body.as_ref();
                    },
                    MessageType::Notice(content) => {
                        return content.body.as_str();
                    },
                    MessageType::ServerNotice(_) => {
                        // XXX: implement

                        return "[server notice]";
                    },
                    MessageType::VerificationRequest(_) => {
                        // XXX: implement

                        return "[verification request]";
                    },
                    MessageType::Audio(..) => {
                        return "[audio]";
                    },
                    MessageType::File(..) => {
                        return "[file]";
                    },
                    MessageType::Image(..) => {
                        return "[image]";
                    },
                    MessageType::Video(..) => {
                        return "[video]";
                    },
                    _ => return "[unknown message type]",
                }
            },
            MessageContent::Redacted => "[redacted]",
        }
    }
}

#[derive(Clone)]
pub struct Message {
    pub content: MessageContent,
    pub sender: OwnedUserId,
    pub timestamp: MessageTimeStamp,
}

impl Message {
    pub fn new(content: MessageContent, sender: OwnedUserId, timestamp: MessageTimeStamp) -> Self {
        Message { content, sender, timestamp }
    }

    pub fn show(&self, selected: bool, vwctx: &ViewportContext<MessageCursor>) -> Text {
        let width = vwctx.get_width();
        let msg = self.as_ref();

        let mut lines = vec![];

        let mut style = Style::default();

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED)
        }

        if self.timestamp.is_local_echo() {
            style = style.add_modifier(StyleModifier::ITALIC);
        }

        if USER_GUTTER + TIME_GUTTER + MIN_MSG_LEN <= width {
            let lw = width - USER_GUTTER - TIME_GUTTER;

            for (i, (line, w)) in wrap(msg, lw).enumerate() {
                let line = Span::styled(line, style);
                let trailing = Span::styled(space(lw.saturating_sub(w)), style);

                if i == 0 {
                    let user = self.show_sender(true);

                    if let Some(time) = self.timestamp.show() {
                        lines.push(Spans(vec![user, line, trailing, time]))
                    } else {
                        lines.push(Spans(vec![user, line, trailing]))
                    }
                } else {
                    let space = USER_GUTTER_EMPTY_SPAN;

                    lines.push(Spans(vec![space, line, trailing]))
                }
            }
        } else if USER_GUTTER + MIN_MSG_LEN <= width {
            let lw = width - USER_GUTTER;

            for (i, (line, w)) in wrap(msg, lw).enumerate() {
                let line = Span::styled(line, style);
                let trailing = Span::styled(space(lw.saturating_sub(w)), style);

                let prefix = if i == 0 {
                    self.show_sender(true)
                } else {
                    USER_GUTTER_EMPTY_SPAN
                };

                lines.push(Spans(vec![prefix, line, trailing]))
            }
        } else {
            lines.push(Spans::from(self.show_sender(false)));

            for (line, _) in wrap(msg, width.saturating_sub(2)) {
                let line = format!("  {}", line);
                let line = Span::styled(line, style);

                lines.push(Spans(vec![line]))
            }
        }

        return Text { lines };
    }

    fn show_sender(&self, align_right: bool) -> Span {
        let sender = self.sender.to_string();
        let style = user_style(sender.as_str());

        let sender = if align_right {
            format!("{: >width$}  ", sender, width = 28)
        } else {
            format!("{: <width$}  ", sender, width = 28)
        };

        Span::styled(sender, style)
    }
}

impl From<MessageEvent> for Message {
    fn from(event: MessageEvent) -> Self {
        match event {
            MessageLikeEvent::Original(ev) => {
                let content = MessageContent::Original(ev.content.into());

                Message::new(content, ev.sender, ev.origin_server_ts.into())
            },
            MessageLikeEvent::Redacted(ev) => {
                Message::new(MessageContent::Redacted, ev.sender, ev.origin_server_ts.into())
            },
        }
    }
}

impl AsRef<str> for Message {
    fn as_ref(&self) -> &str {
        self.content.as_ref()
    }
}

impl ToString for Message {
    fn to_string(&self) -> String {
        self.as_ref().to_string()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn test_wrapped_lines_ascii() {
        let s = "hello world!\nabcdefghijklmnopqrstuvwxyz\ngoodbye";

        let mut iter = wrap(s, 100);
        assert_eq!(iter.next(), Some(("hello world!", 12)));
        assert_eq!(iter.next(), Some(("abcdefghijklmnopqrstuvwxyz", 26)));
        assert_eq!(iter.next(), Some(("goodbye", 7)));
        assert_eq!(iter.next(), None);

        let mut iter = wrap(s, 5);
        assert_eq!(iter.next(), Some(("hello", 5)));
        assert_eq!(iter.next(), Some((" worl", 5)));
        assert_eq!(iter.next(), Some(("d!", 2)));
        assert_eq!(iter.next(), Some(("abcde", 5)));
        assert_eq!(iter.next(), Some(("fghij", 5)));
        assert_eq!(iter.next(), Some(("klmno", 5)));
        assert_eq!(iter.next(), Some(("pqrst", 5)));
        assert_eq!(iter.next(), Some(("uvwxy", 5)));
        assert_eq!(iter.next(), Some(("z", 1)));
        assert_eq!(iter.next(), Some(("goodb", 5)));
        assert_eq!(iter.next(), Some(("ye", 2)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_wrapped_lines_unicode() {
        let s = "ＣＨＩＣＫＥＮ";

        let mut iter = wrap(s, 14);
        assert_eq!(iter.next(), Some((s, 14)));
        assert_eq!(iter.next(), None);

        let mut iter = wrap(s, 5);
        assert_eq!(iter.next(), Some(("ＣＨ", 4)));
        assert_eq!(iter.next(), Some(("ＩＣ", 4)));
        assert_eq!(iter.next(), Some(("ＫＥ", 4)));
        assert_eq!(iter.next(), Some(("Ｎ", 2)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_mc_cmp() {
        let mc1 = MessageCursor::from(MSG1_KEY.clone());
        let mc2 = MessageCursor::from(MSG2_KEY.clone());
        let mc3 = MessageCursor::from(MSG3_KEY.clone());
        let mc4 = MessageCursor::from(MSG4_KEY.clone());
        let mc5 = MessageCursor::from(MSG5_KEY.clone());

        // Everything is equal to itself.
        assert_eq!(mc1.cmp(&mc1), Ordering::Equal);
        assert_eq!(mc2.cmp(&mc2), Ordering::Equal);
        assert_eq!(mc3.cmp(&mc3), Ordering::Equal);
        assert_eq!(mc4.cmp(&mc4), Ordering::Equal);
        assert_eq!(mc5.cmp(&mc5), Ordering::Equal);

        // Local echo is always greater than an origin server timestamp.
        assert_eq!(mc1.cmp(&mc2), Ordering::Greater);
        assert_eq!(mc1.cmp(&mc3), Ordering::Greater);
        assert_eq!(mc1.cmp(&mc4), Ordering::Greater);
        assert_eq!(mc1.cmp(&mc5), Ordering::Greater);

        // mc2 is the smallest timestamp.
        assert_eq!(mc2.cmp(&mc1), Ordering::Less);
        assert_eq!(mc2.cmp(&mc3), Ordering::Less);
        assert_eq!(mc2.cmp(&mc4), Ordering::Less);
        assert_eq!(mc2.cmp(&mc5), Ordering::Less);

        // mc3 should be less than mc4 because of its event ID.
        assert_eq!(mc3.cmp(&mc1), Ordering::Less);
        assert_eq!(mc3.cmp(&mc2), Ordering::Greater);
        assert_eq!(mc3.cmp(&mc4), Ordering::Less);
        assert_eq!(mc3.cmp(&mc5), Ordering::Less);

        // mc4 should be greater than mc3 because of its event ID.
        assert_eq!(mc4.cmp(&mc1), Ordering::Less);
        assert_eq!(mc4.cmp(&mc2), Ordering::Greater);
        assert_eq!(mc4.cmp(&mc3), Ordering::Greater);
        assert_eq!(mc4.cmp(&mc5), Ordering::Less);

        // mc5 is the greatest OriginServer timestamp.
        assert_eq!(mc5.cmp(&mc1), Ordering::Less);
        assert_eq!(mc5.cmp(&mc2), Ordering::Greater);
        assert_eq!(mc5.cmp(&mc3), Ordering::Greater);
        assert_eq!(mc5.cmp(&mc4), Ordering::Greater);
    }

    #[test]
    fn test_mc_to_key() {
        let info = mock_room();
        let mc1 = MessageCursor::from(MSG1_KEY.clone());
        let mc2 = MessageCursor::from(MSG2_KEY.clone());
        let mc3 = MessageCursor::from(MSG3_KEY.clone());
        let mc4 = MessageCursor::from(MSG4_KEY.clone());
        let mc5 = MessageCursor::from(MSG5_KEY.clone());
        let mc6 = MessageCursor::latest();

        let k1 = mc1.to_key(&info).unwrap();
        let k2 = mc2.to_key(&info).unwrap();
        let k3 = mc3.to_key(&info).unwrap();
        let k4 = mc4.to_key(&info).unwrap();
        let k5 = mc5.to_key(&info).unwrap();
        let k6 = mc6.to_key(&info).unwrap();

        // These should all be equal to their MSGN_KEYs.
        assert_eq!(k1, &MSG1_KEY.clone());
        assert_eq!(k2, &MSG2_KEY.clone());
        assert_eq!(k3, &MSG3_KEY.clone());
        assert_eq!(k4, &MSG4_KEY.clone());
        assert_eq!(k5, &MSG5_KEY.clone());

        // MessageCursor::latest() turns into the largest key (our local echo message).
        assert_eq!(k6, &MSG1_KEY.clone());

        // MessageCursor::latest() fails to convert for a room w/o messages.
        let info_empty = RoomInfo::default();
        assert_eq!(mc6.to_key(&info_empty), None);
    }

    #[test]
    fn test_mc_to_from_cursor() {
        let info = mock_room();
        let mc1 = MessageCursor::from(MSG1_KEY.clone());
        let mc2 = MessageCursor::from(MSG2_KEY.clone());
        let mc3 = MessageCursor::from(MSG3_KEY.clone());
        let mc4 = MessageCursor::from(MSG4_KEY.clone());
        let mc5 = MessageCursor::from(MSG5_KEY.clone());
        let mc6 = MessageCursor::latest();

        let identity = |mc: &MessageCursor| {
            let c = mc.to_cursor(&info).unwrap();

            MessageCursor::from_cursor(&c, &info).unwrap()
        };

        // These should all convert to a Cursor and back to the original value.
        assert_eq!(identity(&mc1), mc1);
        assert_eq!(identity(&mc2), mc2);
        assert_eq!(identity(&mc3), mc3);
        assert_eq!(identity(&mc4), mc4);
        assert_eq!(identity(&mc5), mc5);

        // MessageCursor::latest() should point at the most recent message after conversion.
        assert_eq!(identity(&mc6), mc1);
    }
}
