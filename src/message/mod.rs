//! # Room Messages
use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::hash_map::DefaultHasher;
use std::collections::hash_set;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

use chrono::{DateTime, Local as LocalTz, NaiveDateTime, TimeZone};
use comrak::{markdown_to_html, ComrakOptions};
use unicode_width::UnicodeWidthStr;

use matrix_sdk::ruma::{
    events::{
        room::{
            encrypted::{
                OriginalRoomEncryptedEvent,
                RedactedRoomEncryptedEvent,
                RoomEncryptedEvent,
            },
            message::{
                FormattedBody,
                MessageFormat,
                MessageType,
                OriginalRoomMessageEvent,
                RedactedRoomMessageEvent,
                Relation,
                RoomMessageEvent,
                RoomMessageEventContent,
                TextMessageEventContent,
            },
            redaction::SyncRoomRedactionEvent,
        },
        AnyMessageLikeEvent,
        Redact,
        RedactedUnsigned,
    },
    EventId,
    MilliSecondsSinceUnixEpoch,
    OwnedEventId,
    OwnedUserId,
    RoomVersionId,
    UInt,
};

use modalkit::tui::{
    style::{Modifier as StyleModifier, Style},
    symbols::line::THICK_VERTICAL,
    text::{Line, Span, Text},
};

use modalkit::editing::{base::ViewportContext, cursor::Cursor};
use ratatui_image::protocol::Protocol;

use crate::config::ImagePreviewSize;
use crate::{
    base::{IambResult, RoomInfo},
    config::ApplicationSettings,
    message::html::{parse_matrix_html, StyleTree},
    util::{space_span, wrapped_text},
};

mod html;
mod printer;

pub type MessageFetchResult = IambResult<(Option<String>, Vec<AnyMessageLikeEvent>)>;
pub type MessageKey = (MessageTimeStamp, OwnedEventId);
pub type Messages = BTreeMap<MessageKey, Message>;

const fn span_static(s: &'static str) -> Span<'static> {
    Span {
        content: Cow::Borrowed(s),
        style: Style {
            fg: None,
            bg: None,
            add_modifier: StyleModifier::empty(),
            sub_modifier: StyleModifier::empty(),
            underline_color: None,
        },
    }
}

const BOLD_STYLE: Style = Style {
    fg: None,
    bg: None,
    add_modifier: StyleModifier::BOLD,
    sub_modifier: StyleModifier::empty(),
    underline_color: None,
};

const USER_GUTTER: usize = 30;
const TIME_GUTTER: usize = 12;
const READ_GUTTER: usize = 5;
const MIN_MSG_LEN: usize = 30;

const USER_GUTTER_EMPTY: &str = "                              ";
const USER_GUTTER_EMPTY_SPAN: Span<'static> = span_static(USER_GUTTER_EMPTY);

const TIME_GUTTER_EMPTY: &str = "            ";
const TIME_GUTTER_EMPTY_SPAN: Span<'static> = span_static(TIME_GUTTER_EMPTY);

fn text_to_message_content(input: String) -> TextMessageEventContent {
    let mut options = ComrakOptions::default();
    options.extension.shortcodes = true;
    options.extension.strikethrough = true;
    options.render.hardbreaks = true;
    let html = markdown_to_html(input.as_str(), &options);

    TextMessageEventContent::html(input, html)
}

pub fn text_to_message(input: String) -> RoomMessageEventContent {
    let msg = MessageType::Text(text_to_message_content(input));
    RoomMessageEventContent::new(msg)
}

#[inline]
fn millis_to_datetime(ms: UInt) -> DateTime<LocalTz> {
    let time = i64::from(ms) / 1000;
    let time = NaiveDateTime::from_timestamp_opt(time, 0).unwrap_or_default();

    LocalTz.from_utc_datetime(&time)
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
    fn as_datetime(&self) -> DateTime<LocalTz> {
        match self {
            MessageTimeStamp::OriginServer(ms) => millis_to_datetime(*ms),
            MessageTimeStamp::LocalEcho => LocalTz::now(),
        }
    }

    fn same_day(&self, other: &Self) -> bool {
        let dt1 = self.as_datetime();
        let dt2 = other.as_datetime();

        dt1.date_naive() == dt2.date_naive()
    }

    fn show_date(&self) -> Option<Span> {
        let time = self.as_datetime().format("%A, %B %d %Y").to_string();

        Span::styled(time, BOLD_STYLE).into()
    }

    fn show_time(&self) -> Option<Span> {
        match self {
            MessageTimeStamp::OriginServer(ms) => {
                let time = millis_to_datetime(*ms).format("%T");
                let time = format!("  [{time}]");

                Span::raw(time).into()
            },
            MessageTimeStamp::LocalEcho => None,
        }
    }

    fn is_local_echo(&self) -> bool {
        matches!(self, MessageTimeStamp::LocalEcho)
    }

    pub fn as_millis(&self) -> Option<MilliSecondsSinceUnixEpoch> {
        match self {
            MessageTimeStamp::OriginServer(ms) => MilliSecondsSinceUnixEpoch(*ms).into(),
            MessageTimeStamp::LocalEcho => None,
        }
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

impl From<UInt> for MessageTimeStamp {
    fn from(millis: UInt) -> Self {
        MessageTimeStamp::OriginServer(millis)
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

            Ok(MessageTimeStamp::from(n))
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
pub enum MessageEvent {
    EncryptedOriginal(Box<OriginalRoomEncryptedEvent>),
    EncryptedRedacted(Box<RedactedRoomEncryptedEvent>),
    Original(Box<OriginalRoomMessageEvent>),
    Redacted(Box<RedactedRoomMessageEvent>),
    Local(OwnedEventId, Box<RoomMessageEventContent>),
}

impl MessageEvent {
    pub fn event_id(&self) -> &EventId {
        match self {
            MessageEvent::EncryptedOriginal(ev) => ev.event_id.as_ref(),
            MessageEvent::EncryptedRedacted(ev) => ev.event_id.as_ref(),
            MessageEvent::Original(ev) => ev.event_id.as_ref(),
            MessageEvent::Redacted(ev) => ev.event_id.as_ref(),
            MessageEvent::Local(event_id, _) => event_id.as_ref(),
        }
    }

    pub fn content(&self) -> Option<&RoomMessageEventContent> {
        match self {
            MessageEvent::EncryptedOriginal(_) => None,
            MessageEvent::Original(ev) => Some(&ev.content),
            MessageEvent::EncryptedRedacted(_) => None,
            MessageEvent::Redacted(_) => None,
            MessageEvent::Local(_, content) => Some(content),
        }
    }

    pub fn is_emote(&self) -> bool {
        matches!(
            self.content(),
            Some(RoomMessageEventContent { msgtype: MessageType::Emote(_), .. })
        )
    }

    pub fn body(&self) -> Cow<'_, str> {
        match self {
            MessageEvent::EncryptedOriginal(_) => "[Unable to decrypt message]".into(),
            MessageEvent::Original(ev) => body_cow_content(&ev.content),
            MessageEvent::EncryptedRedacted(ev) => body_cow_reason(&ev.unsigned),
            MessageEvent::Redacted(ev) => body_cow_reason(&ev.unsigned),
            MessageEvent::Local(_, content) => body_cow_content(content),
        }
    }

    pub fn html(&self) -> Option<StyleTree> {
        let content = match self {
            MessageEvent::EncryptedOriginal(_) => return None,
            MessageEvent::EncryptedRedacted(_) => return None,
            MessageEvent::Original(ev) => &ev.content,
            MessageEvent::Redacted(_) => return None,
            MessageEvent::Local(_, content) => content,
        };

        if let MessageType::Text(content) = &content.msgtype {
            if let Some(FormattedBody { format: MessageFormat::Html, body }) = &content.formatted {
                Some(parse_matrix_html(body.as_str()))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn redact(&mut self, redaction: SyncRoomRedactionEvent, version: &RoomVersionId) {
        match self {
            MessageEvent::EncryptedOriginal(_) => return,
            MessageEvent::EncryptedRedacted(_) => return,
            MessageEvent::Redacted(_) => return,
            MessageEvent::Local(_, _) => return,
            MessageEvent::Original(ev) => {
                let redacted = ev.clone().redact(redaction, version);
                *self = MessageEvent::Redacted(Box::new(redacted));
            },
        }
    }
}

fn body_cow_content(content: &RoomMessageEventContent) -> Cow<'_, str> {
    let s = match &content.msgtype {
        MessageType::Text(content) => content.body.as_str(),
        MessageType::VerificationRequest(_) => "[Verification Request]",
        MessageType::Emote(content) => content.body.as_ref(),
        MessageType::Notice(content) => content.body.as_str(),
        MessageType::ServerNotice(content) => content.body.as_str(),

        MessageType::Audio(content) => {
            return Cow::Owned(format!("[Attached Audio: {}]", content.body));
        },
        MessageType::File(content) => {
            return Cow::Owned(format!("[Attached File: {}]", content.body));
        },
        MessageType::Image(content) => {
            return Cow::Owned(format!("[Attached Image: {}]", content.body));
        },
        MessageType::Video(content) => {
            return Cow::Owned(format!("[Attached Video: {}]", content.body));
        },
        _ => {
            return Cow::Owned(format!("[Unknown message type: {:?}]", content.msgtype()));
        },
    };

    Cow::Borrowed(s)
}

fn body_cow_reason(unsigned: &RedactedUnsigned) -> Cow<'_, str> {
    let reason = unsigned
        .redacted_because
        .as_ref()
        .and_then(|e| e.as_original())
        .and_then(|r| r.content.reason.as_ref());

    if let Some(r) = reason {
        Cow::Owned(format!("[Redacted: {r:?}]"))
    } else {
        Cow::Borrowed("[Redacted]")
    }
}

enum MessageColumns {
    /// Four columns: sender, message, timestamp, read receipts.
    Four,

    /// Three columns: sender, message, timestamp.
    Three,

    /// Two columns: sender, message.
    Two,

    /// One column: message with sender on line before the message.
    One,
}

struct MessageFormatter<'a> {
    settings: &'a ApplicationSettings,

    /// How many columns to print.
    cols: MessageColumns,

    /// The full, original width.
    orig: usize,

    /// The width that the message contents need to fill.
    fill: usize,

    /// The formatted Span for the message sender.
    user: Option<Span<'a>>,

    /// The time the message was sent.
    time: Option<Span<'a>>,

    /// The date the message was sent.
    date: Option<Span<'a>>,

    /// Iterator over the users who have read up to this message.
    read: Option<hash_set::Iter<'a, OwnedUserId>>,
}

impl<'a> MessageFormatter<'a> {
    fn width(&self) -> usize {
        self.fill
    }

    #[inline]
    fn push_spans(&mut self, prev_line: Line<'a>, style: Style, text: &mut Text<'a>) {
        if let Some(date) = self.date.take() {
            let len = date.content.as_ref().len();
            let padding = self.orig.saturating_sub(len);
            let leading = space_span(padding / 2, Style::default());
            let trailing = space_span(padding.saturating_sub(padding / 2), Style::default());

            text.lines.push(Line::from(vec![leading, date, trailing]));
        }

        match self.cols {
            MessageColumns::Four => {
                let settings = self.settings;
                let user = self.user.take().unwrap_or(USER_GUTTER_EMPTY_SPAN);
                let time = self.time.take().unwrap_or(TIME_GUTTER_EMPTY_SPAN);

                let mut line = vec![user];
                line.extend(prev_line.spans);
                line.push(time);

                // Show read receipts.
                let user_char =
                    |user: &'a OwnedUserId| -> Span<'a> { settings.get_user_char_span(user) };
                let mut read = self.read.iter_mut().flatten();

                let a = read.next().map(user_char).unwrap_or_else(|| Span::raw(" "));
                let b = read.next().map(user_char).unwrap_or_else(|| Span::raw(" "));
                let c = read.next().map(user_char).unwrap_or_else(|| Span::raw(" "));

                line.push(Span::raw(" "));
                line.push(c);
                line.push(b);
                line.push(a);
                line.push(Span::raw(" "));

                text.lines.push(Line::from(line))
            },
            MessageColumns::Three => {
                let user = self.user.take().unwrap_or(USER_GUTTER_EMPTY_SPAN);
                let time = self.time.take().unwrap_or_else(|| Span::from(""));

                let mut line = vec![user];
                line.extend(prev_line.spans);
                line.push(time);

                text.lines.push(Line::from(line))
            },
            MessageColumns::Two => {
                let user = self.user.take().unwrap_or(USER_GUTTER_EMPTY_SPAN);
                let mut line = vec![user];
                line.extend(prev_line.spans);

                text.lines.push(Line::from(line));
            },
            MessageColumns::One => {
                if let Some(user) = self.user.take() {
                    text.lines.push(Line::from(vec![user]));
                }

                let leading = space_span(2, style);
                let mut line = vec![leading];
                line.extend(prev_line.spans);

                text.lines.push(Line::from(line));
            },
        }
    }

    fn push_text(&mut self, append: Text<'a>, style: Style, text: &mut Text<'a>) {
        for line in append.lines.into_iter() {
            self.push_spans(line, style, text);
        }
    }
}

pub enum ImageStatus {
    None,
    Downloading(ImagePreviewSize),
    Loaded(Box<dyn Protocol>),
    Error(String),
}

pub struct Message {
    pub event: MessageEvent,
    pub sender: OwnedUserId,
    pub timestamp: MessageTimeStamp,
    pub downloaded: bool,
    pub html: Option<StyleTree>,
    pub image_preview: ImageStatus,
}

impl Message {
    pub fn new(event: MessageEvent, sender: OwnedUserId, timestamp: MessageTimeStamp) -> Self {
        let html = event.html();
        let downloaded = false;

        Message {
            event,
            sender,
            timestamp,
            downloaded,
            html,
            image_preview: ImageStatus::None,
        }
    }

    pub fn reply_to(&self) -> Option<OwnedEventId> {
        let content = match &self.event {
            MessageEvent::EncryptedOriginal(_) => return None,
            MessageEvent::EncryptedRedacted(_) => return None,
            MessageEvent::Local(_, content) => content,
            MessageEvent::Original(ev) => &ev.content,
            MessageEvent::Redacted(_) => return None,
        };

        if let Some(Relation::Reply { in_reply_to }) = &content.relates_to {
            Some(in_reply_to.event_id.clone())
        } else {
            None
        }
    }

    fn get_render_style(&self, selected: bool) -> Style {
        let mut style = Style::default();

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED)
        }

        if self.timestamp.is_local_echo() {
            style = style.add_modifier(StyleModifier::ITALIC);
        }

        return style;
    }

    fn get_render_format<'a>(
        &'a self,
        prev: Option<&Message>,
        width: usize,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> MessageFormatter<'a> {
        let orig = width;
        let date = match &prev {
            Some(prev) if prev.timestamp.same_day(&self.timestamp) => None,
            _ => self.timestamp.show_date(),
        };

        if USER_GUTTER + TIME_GUTTER + READ_GUTTER + MIN_MSG_LEN <= width &&
            settings.tunables.read_receipt_display
        {
            let cols = MessageColumns::Four;
            let fill = width - USER_GUTTER - TIME_GUTTER - READ_GUTTER;
            let user = self.show_sender(prev, true, info, settings);
            let time = self.timestamp.show_time();
            let read = info.event_receipts.get(self.event.event_id()).map(|read| read.iter());

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if USER_GUTTER + TIME_GUTTER + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Three;
            let fill = width - USER_GUTTER - TIME_GUTTER;
            let user = self.show_sender(prev, true, info, settings);
            let time = self.timestamp.show_time();
            let read = None;

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if USER_GUTTER + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Two;
            let fill = width - USER_GUTTER;
            let user = self.show_sender(prev, true, info, settings);
            let time = None;
            let read = None;

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else {
            let cols = MessageColumns::One;
            let fill = width.saturating_sub(2);
            let user = self.show_sender(prev, false, info, settings);
            let time = None;
            let read = None;

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        }
    }

    /// Get the image preview Protocol and x,y offset, based on get_render_format.
    pub fn line_preview<'a>(
        &'a self,
        prev: Option<&Message>,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
    ) -> Option<(&dyn Protocol, u16, u16)> {
        let width = vwctx.get_width();
        // The x position where get_render_format would render the text.
        let x = (if USER_GUTTER + MIN_MSG_LEN <= width {
            USER_GUTTER
        } else {
            0
        }) as u16;
        // See get_render_format; account for possible "date" line.
        let date_y = match &prev {
            Some(prev) if !prev.timestamp.same_day(&self.timestamp) => 1,
            _ => 0,
        };
        if let ImageStatus::Loaded(backend) = &self.image_preview {
            return Some((backend.as_ref(), x, date_y));
        } else if let Some(reply) = self.reply_to().and_then(|e| info.get_event(&e)) {
            if let ImageStatus::Loaded(backend) = &reply.image_preview {
                // The reply should be offset a bit:
                return Some((backend.as_ref(), x + 2, date_y + 1));
            }
        }
        None
    }

    pub fn show<'a>(
        &'a self,
        prev: Option<&Message>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> Text<'a> {
        let width = vwctx.get_width();

        let style = self.get_render_style(selected);
        let mut fmt = self.get_render_format(prev, width, info, settings);
        let mut text = Text { lines: vec![] };
        let width = fmt.width();

        // Show the message that this one replied to, if any.
        let reply = self.reply_to().and_then(|e| info.get_event(&e));

        if let Some(r) = &reply {
            let w = width.saturating_sub(2);
            let mut replied = r.show_msg(w, style, true);
            let mut sender = r.sender_span(info, settings);
            let sender_width = UnicodeWidthStr::width(sender.content.as_ref());
            let trailing = w.saturating_sub(sender_width + 1);

            sender.style = sender.style.patch(style);

            fmt.push_spans(
                Line::from(vec![
                    Span::styled(" ", style),
                    Span::styled(THICK_VERTICAL, style),
                    sender,
                    Span::styled(":", style),
                    space_span(trailing, style),
                ]),
                style,
                &mut text,
            );

            for line in replied.lines.iter_mut() {
                line.spans.insert(0, Span::styled(THICK_VERTICAL, style));
                line.spans.insert(0, Span::styled(" ", style));
            }

            fmt.push_text(replied, style, &mut text);
        }

        // Now show the message contents, and the inlined reply if we couldn't find it above.
        let msg = self.show_msg(width, style, reply.is_some());
        fmt.push_text(msg, style, &mut text);

        if text.lines.is_empty() {
            // If there was nothing in the body, just show an empty message.
            fmt.push_spans(space_span(width, style).into(), style, &mut text);
        }

        if settings.tunables.reaction_display {
            let mut emojis = printer::TextPrinter::new(width, style, false);
            let mut reactions = 0;

            for (key, count) in info.get_reactions(self.event.event_id()).into_iter() {
                if reactions != 0 {
                    emojis.push_str(" ", style);
                }

                let name = if settings.tunables.reaction_shortcode_display {
                    if let Some(emoji) = emojis::get(key) {
                        if let Some(short) = emoji.shortcode() {
                            short
                        } else {
                            // No ASCII shortcode name to show.
                            continue;
                        }
                    } else if key.chars().all(|c| c.is_ascii_alphanumeric()) {
                        key
                    } else {
                        // Not an Emoji or a printable ASCII string.
                        continue;
                    }
                } else {
                    key
                };

                emojis.push_str("[", style);
                emojis.push_str(name, style);
                emojis.push_str(" ", style);
                emojis.push_span_nobreak(Span::styled(count.to_string(), style));
                emojis.push_str("]", style);

                reactions += 1;
            }

            if reactions > 0 {
                fmt.push_text(emojis.finish(), style, &mut text);
            }
        }

        return text;
    }

    pub fn show_msg(&self, width: usize, style: Style, hide_reply: bool) -> Text {
        if let Some(html) = &self.html {
            html.to_text(width, style, hide_reply)
        } else {
            let mut msg = self.event.body();

            if self.downloaded {
                msg.to_mut().push_str(" \u{2705}");
            }

            if let Some(placeholder) = match &self.image_preview {
                ImageStatus::None => None,
                ImageStatus::Downloading(image_preview_size) => {
                    Message::placeholder_frame(Some("Downloading..."), width, image_preview_size)
                },
                ImageStatus::Loaded(backend) => {
                    Message::placeholder_frame(None, width, &backend.rect().into())
                },
                ImageStatus::Error(err) => Some(format!("[Image error: {err}]\n")),
            } {
                msg.to_mut().insert_str(0, &placeholder);
            }

            wrapped_text(msg, width, style)
        }
    }

    fn sender_span<'a>(
        &'a self,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> Span<'a> {
        settings.get_user_span(self.sender.as_ref(), info)
    }

    /// Before the image is loaded, already display a placeholder frame of the image size.
    fn placeholder_frame(
        text: Option<&str>,
        outer_width: usize,
        image_preview_size: &ImagePreviewSize,
    ) -> Option<String> {
        let ImagePreviewSize { width, height } = image_preview_size;
        if outer_width < *width || (*width < 2 || *height < 2) {
            return None;
        }
        let mut placeholder = "\u{230c}".to_string();
        placeholder.push_str(&" ".repeat(width - 2));
        placeholder.push_str("\u{230d}\n");
        if *height > 2 {
            if let Some(text) = text {
                if text.width() <= width - 2 {
                    placeholder.push(' ');
                    placeholder.push_str(text);
                }
            }
        }

        placeholder.push_str(&"\n".repeat(height - 2));
        placeholder.push('\u{230e}');
        placeholder.push_str(&" ".repeat(width - 2));
        placeholder.push_str("\u{230f}\n");
        Some(placeholder)
    }

    fn show_sender<'a>(
        &'a self,
        prev: Option<&Message>,
        align_right: bool,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> Option<Span<'a>> {
        if let Some(prev) = prev {
            if self.sender == prev.sender &&
                self.timestamp.same_day(&prev.timestamp) &&
                !self.event.is_emote()
            {
                return None;
            }
        }

        let Span { content, style } = self.sender_span(info, settings);
        let stop = content.len().min(28);
        let s = &content[..stop];

        let sender = if align_right {
            format!("{: >width$}  ", s, width = 28)
        } else {
            format!("{: <width$}  ", s, width = 28)
        };

        Span::styled(sender, style).into()
    }

    pub fn redact(&mut self, redaction: SyncRoomRedactionEvent, version: &RoomVersionId) {
        self.event.redact(redaction, version);
        self.html = None;
    }
}

impl From<RoomEncryptedEvent> for Message {
    fn from(event: RoomEncryptedEvent) -> Self {
        let timestamp = event.origin_server_ts().into();
        let user_id = event.sender().to_owned();
        let content = match event {
            RoomEncryptedEvent::Original(ev) => MessageEvent::EncryptedOriginal(ev.into()),
            RoomEncryptedEvent::Redacted(ev) => MessageEvent::EncryptedRedacted(ev.into()),
        };

        Message::new(content, user_id, timestamp)
    }
}

impl From<OriginalRoomMessageEvent> for Message {
    fn from(event: OriginalRoomMessageEvent) -> Self {
        let timestamp = event.origin_server_ts.into();
        let user_id = event.sender.clone();
        let content = MessageEvent::Original(event.into());

        Message::new(content, user_id, timestamp)
    }
}

impl From<RedactedRoomMessageEvent> for Message {
    fn from(event: RedactedRoomMessageEvent) -> Self {
        let timestamp = event.origin_server_ts.into();
        let user_id = event.sender.clone();
        let content = MessageEvent::Redacted(event.into());

        Message::new(content, user_id, timestamp)
    }
}

impl From<RoomMessageEvent> for Message {
    fn from(event: RoomMessageEvent) -> Self {
        match event {
            RoomMessageEvent::Original(ev) => ev.into(),
            RoomMessageEvent::Redacted(ev) => ev.into(),
        }
    }
}

impl ToString for Message {
    fn to_string(&self) -> String {
        self.event.body().into_owned()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tests::*;

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

    #[test]
    fn test_markdown_message() {
        let input = "**bold**\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p><strong>bold</strong></p>\n");

        let input = "*emphasis*\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p><em>emphasis</em></p>\n");

        let input = "`code`\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p><code>code</code></p>\n");

        let input = "```rust\nconst A: usize = 1;\n```\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<pre><code class=\"language-rust\">const A: usize = 1;\n</code></pre>\n"
        );

        let input = ":heart:\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p>\u{2764}\u{FE0F}</p>\n");

        let input = "para 1\n\npara 2\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p>para 1</p>\n<p>para 2</p>\n");

        let input = "line 1\nline 2\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(content.formatted.unwrap().body, "<p>line 1<br />\nline 2</p>\n");

        let input = "# Heading\n## Subheading\n\ntext\n";
        let content = text_to_message_content(input.into());
        assert_eq!(content.body, input);
        assert_eq!(
            content.formatted.unwrap().body,
            "<h1>Heading</h1>\n<h2>Subheading</h2>\n<p>text</p>\n"
        );
    }

    #[test]
    fn test_placeholder_frame() {
        fn pretty_frame_test(str: &str) -> Option<String> {
            Some(str[1..].to_string())
        }

        assert_eq!(
            Message::placeholder_frame(None, 4, &ImagePreviewSize { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍


⌎  ⌏
"#
            )
        );

        assert_eq!(
            Message::placeholder_frame(None, 2, &ImagePreviewSize { width: 4, height: 4 }),
            None
        );
        assert_eq!(
            Message::placeholder_frame(None, 4, &ImagePreviewSize { width: 1, height: 4 }),
            None
        );

        assert_eq!(
            Message::placeholder_frame(None, 4, &ImagePreviewSize { width: 4, height: 1 }),
            None
        );

        assert_eq!(
            Message::placeholder_frame(Some("OK"), 4, &ImagePreviewSize { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍
 OK

⌎  ⌏
"#
            )
        );
        assert_eq!(
            Message::placeholder_frame(Some("idontfit"), 4, &ImagePreviewSize {
                width: 4,
                height: 4,
            }),
            pretty_frame_test(
                r#"
⌌  ⌍


⌎  ⌏
"#
            )
        );
        assert_eq!(
            Message::placeholder_frame(Some("OK"), 4, &ImagePreviewSize { width: 4, height: 2 }),
            pretty_frame_test(
                r#"
⌌  ⌍
⌎  ⌏
"#
            )
        );
        assert_eq!(
            Message::placeholder_frame(Some("OK"), 4, &ImagePreviewSize { width: 2, height: 3 }),
            pretty_frame_test(
                r#"
⌌⌍

⌎⌏
"#
            )
        );
    }
}
