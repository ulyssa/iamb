//! # Room Messages
use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::hash_map::DefaultHasher;
use std::collections::hash_set;
use std::collections::BTreeMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

use chrono::{DateTime, Local as LocalTz};
use humansize::{format_size, DECIMAL};
use serde_json::json;
use unicode_width::UnicodeWidthStr;

use matrix_sdk::ruma::{
    events::{
        relation::Thread,
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
            },
            redaction::SyncRoomRedactionEvent,
        },
        RedactContent,
        RedactedUnsigned,
    },
    EventId,
    MilliSecondsSinceUnixEpoch,
    OwnedEventId,
    OwnedUserId,
    RoomVersionId,
    UInt,
};

use ratatui::{
    style::{Modifier as StyleModifier, Style},
    symbols::line::THICK_VERTICAL,
    text::{Line, Span, Text},
};

use modalkit::editing::cursor::Cursor;
use modalkit::prelude::*;
use ratatui_image::protocol::Protocol;

use crate::config::ImagePreviewSize;
use crate::{
    base::RoomInfo,
    config::ApplicationSettings,
    message::html::{parse_matrix_html, StyleTree},
    util::{replace_emojis_in_str, space, space_span, take_width, wrapped_text},
};

mod compose;
mod html;
mod printer;

pub use self::compose::text_to_message;

pub type MessageKey = (MessageTimeStamp, OwnedEventId);

#[derive(Default)]
pub struct Messages(BTreeMap<MessageKey, Message>);

impl Deref for Messages {
    type Target = BTreeMap<MessageKey, Message>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Messages {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Messages {
    pub fn insert_message(&mut self, key: MessageKey, msg: impl Into<Message>) {
        let event_id = key.1.clone();
        let msg = msg.into();

        self.0.insert(key, msg);

        // Remove any echo.
        let key = (MessageTimeStamp::LocalEcho, event_id);
        let _ = self.0.remove(&key);
    }
}

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

const TIME_GUTTER: usize = 12;
const READ_GUTTER: usize = 5;
const MIN_MSG_LEN: usize = 30;

const TIME_GUTTER_EMPTY: &str = "            ";
const TIME_GUTTER_EMPTY_SPAN: Span<'static> = span_static(TIME_GUTTER_EMPTY);

const USIZE_TOO_SMALL: bool = usize::BITS < u64::BITS;

/// Convert the [u64] hash to [usize] as needed.
fn hash_finish_usize(hasher: DefaultHasher) -> Option<usize> {
    if USIZE_TOO_SMALL {
        (hasher.finish() % usize::MAX as u64).try_into().ok()
    } else {
        hasher.finish().try_into().ok()
    }
}

/// Hash an [EventId] into a [usize].
fn hash_event_id(event_id: &EventId) -> Option<usize> {
    let mut hasher = DefaultHasher::new();
    event_id.hash(&mut hasher);
    hash_finish_usize(hasher)
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

#[inline]
fn millis_to_datetime(ms: UInt) -> DateTime<LocalTz> {
    let time = i64::from(ms) / 1000;
    let time = DateTime::from_timestamp(time, 0).unwrap_or_default();
    time.into()
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
        Some(self.cmp(other))
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

    pub fn to_key<'a>(&'a self, thread: &'a Messages) -> Option<&'a MessageKey> {
        if let Some(ref key) = self.timestamp {
            Some(key)
        } else {
            Some(thread.last_key_value()?.0)
        }
    }

    pub fn from_cursor(cursor: &Cursor, thread: &Messages) -> Option<Self> {
        let ev_hash = cursor.get_x();
        let ev_term = OwnedEventId::try_from("$").ok()?;

        let ts_start = MessageTimeStamp::try_from(cursor.get_y()).ok()?;
        let start = (ts_start, ev_term);

        for ((ts, event_id), _) in thread.range(&start..) {
            if hash_event_id(event_id)? == ev_hash {
                return Self::from((*ts, event_id.clone())).into();
            }

            if ts > &ts_start {
                break;
            }
        }

        // If we can't find the cursor, then go to the nearest timestamp.
        thread
            .range(start..)
            .next()
            .map(|((ts, ev), _)| Self::from((*ts, ev.clone())))
    }

    pub fn to_cursor(&self, thread: &Messages) -> Option<Cursor> {
        let (ts, event_id) = self.to_key(thread)?;

        let y = usize::try_from(ts).ok()?;
        let x = hash_event_id(event_id)?;

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
        Some(self.cmp(other))
    }
}

fn redaction_reason(ev: &SyncRoomRedactionEvent) -> Option<&str> {
    let SyncRoomRedactionEvent::Original(ev) = ev else {
        return None;
    };

    return ev.content.reason.as_deref();
}

fn redaction_unsigned(ev: SyncRoomRedactionEvent) -> RedactedUnsigned {
    let reason = redaction_reason(&ev);
    let redacted_because = json!({
        "content": {
            "reason": reason
        },
        "event_id": ev.event_id(),
        "sender": ev.sender(),
        "origin_server_ts": ev.origin_server_ts(),
        "unsigned": {},
    });
    RedactedUnsigned::new(serde_json::from_value(redacted_because).unwrap())
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
                let redacted = RedactedRoomMessageEvent {
                    content: ev.content.clone().redact(version),
                    event_id: ev.event_id.clone(),
                    sender: ev.sender.clone(),
                    origin_server_ts: ev.origin_server_ts,
                    room_id: ev.room_id.clone(),
                    unsigned: redaction_unsigned(redaction),
                };
                *self = MessageEvent::Redacted(Box::new(redacted));
            },
        }
    }
}

/// Macro rule converting a File / Image / Audio / Video to its text content with the shape:
/// `[Attached <type>: <content>[ (<human readable file size>)]]`
macro_rules! display_file_to_text {
    ( $msgtype:ident, $content:expr ) => {
        return Cow::Owned(format!(
            "[Attached {}: {}{}]",
            stringify!($msgtype),
            $content.body,
            $content
                .info
                .as_ref()
                .map(|info| {
                    info.size
                        .map(|s| format!(" ({})", format_size(u64::from(s), DECIMAL)))
                        .unwrap_or_else(String::new)
                })
                .unwrap_or_else(String::new)
        ))
    };
}

fn body_cow_content(content: &RoomMessageEventContent) -> Cow<'_, str> {
    let s = match &content.msgtype {
        MessageType::Text(content) => content.body.as_str(),
        MessageType::VerificationRequest(_) => "[Verification Request]",
        MessageType::Emote(content) => content.body.as_ref(),
        MessageType::Notice(content) => content.body.as_str(),
        MessageType::ServerNotice(content) => content.body.as_str(),

        MessageType::Audio(content) => {
            display_file_to_text!(Audio, content);
        },
        MessageType::File(content) => {
            display_file_to_text!(File, content);
        },
        MessageType::Image(content) => {
            display_file_to_text!(Image, content);
        },
        MessageType::Video(content) => {
            display_file_to_text!(Video, content);
        },
        _ => {
            match content.msgtype() {
                // Just show the body text for the special Element messages.
                "nic.custom.confetti" |
                "nic.custom.fireworks" |
                "io.element.effect.hearts" |
                "io.element.effect.rainfall" |
                "io.element.effect.snowfall" |
                "io.element.effects.space_invaders" => content.body(),
                other => {
                    return Cow::Owned(format!("[Unknown message type: {other:?}]"));
                },
            }
        },
    };

    Cow::Borrowed(s)
}

fn body_cow_reason(unsigned: &RedactedUnsigned) -> Cow<'_, str> {
    let reason = unsigned.redacted_because.content.reason.as_ref();

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

impl MessageColumns {
    fn user_gutter_width(&self, settings: &ApplicationSettings) -> u16 {
        if let MessageColumns::One = self {
            0
        } else {
            settings.tunables.user_gutter_width as u16
        }
    }
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

        let user_gutter_empty_span =
            space_span(self.settings.tunables.user_gutter_width, Style::default());

        match self.cols {
            MessageColumns::Four => {
                let settings = self.settings;
                let user = self.user.take().unwrap_or(user_gutter_empty_span);
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
                let user = self.user.take().unwrap_or(user_gutter_empty_span);
                let time = self.time.take().unwrap_or_else(|| Span::from(""));

                let mut line = vec![user];
                line.extend(prev_line.spans);
                line.push(time);

                text.lines.push(Line::from(line))
            },
            MessageColumns::Two => {
                let user = self.user.take().unwrap_or(user_gutter_empty_span);
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

    fn push_in_reply(
        &mut self,
        msg: &'a Message,
        style: Style,
        text: &mut Text<'a>,
        info: &'a RoomInfo,
    ) {
        let width = self.width();
        let w = width.saturating_sub(2);
        let shortcodes = self.settings.tunables.message_shortcode_display;
        let (mut replied, _) = msg.show_msg(w, style, true, shortcodes);
        let mut sender = msg.sender_span(info, self.settings);
        let sender_width = UnicodeWidthStr::width(sender.content.as_ref());
        let trailing = w.saturating_sub(sender_width + 1);

        sender.style = sender.style.patch(style);

        self.push_spans(
            Line::from(vec![
                Span::styled(" ", style),
                Span::styled(THICK_VERTICAL, style),
                sender,
                Span::styled(":", style),
                space_span(trailing, style),
            ]),
            style,
            text,
        );

        for line in replied.lines.iter_mut() {
            line.spans.insert(0, Span::styled(THICK_VERTICAL, style));
            line.spans.insert(0, Span::styled(" ", style));
        }

        self.push_text(replied, style, text);
    }

    fn push_reactions(&mut self, counts: Vec<(&'a str, usize)>, style: Style, text: &mut Text<'a>) {
        let mut emojis = printer::TextPrinter::new(self.width(), style, false, false);
        let mut reactions = 0;

        for (key, count) in counts {
            if reactions != 0 {
                emojis.push_str(" ", style);
            }

            let name = if self.settings.tunables.reaction_shortcode_display {
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
            self.push_text(emojis.finish(), style, text);
        }
    }

    fn push_thread_reply_count(&mut self, len: usize, text: &mut Text<'a>) {
        if len == 0 {
            return;
        }

        // If we have threaded replies to this message, show how many.
        let plural = len != 1;
        let style = Style::default();
        let mut threaded =
            printer::TextPrinter::new(self.width(), style, false, false).literal(true);
        let len = Span::styled(len.to_string(), style.add_modifier(StyleModifier::BOLD));
        threaded.push_str(" \u{2937} ", style);
        threaded.push_span_nobreak(len);
        if plural {
            threaded.push_str(" replies in thread", style);
        } else {
            threaded.push_str(" reply in thread", style);
        }

        self.push_text(threaded.finish(), style, text);
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

        match &content.relates_to {
            Some(Relation::Reply { in_reply_to }) => Some(in_reply_to.event_id.clone()),
            Some(Relation::Thread(Thread {
                in_reply_to: Some(in_reply_to),
                is_falling_back: false,
                ..
            })) => Some(in_reply_to.event_id.clone()),
            Some(_) | None => None,
        }
    }

    fn thread_root(&self) -> Option<OwnedEventId> {
        let content = match &self.event {
            MessageEvent::EncryptedOriginal(_) => return None,
            MessageEvent::EncryptedRedacted(_) => return None,
            MessageEvent::Local(_, content) => content,
            MessageEvent::Original(ev) => &ev.content,
            MessageEvent::Redacted(_) => return None,
        };

        match &content.relates_to {
            Some(Relation::Thread(Thread {
                event_id,
                in_reply_to: Some(in_reply_to),
                is_falling_back: true,
                ..
            })) if event_id == &in_reply_to.event_id => Some(event_id.clone()),
            Some(_) | None => None,
        }
    }

    fn get_render_style(&self, selected: bool, settings: &ApplicationSettings) -> Style {
        let mut style = Style::default();

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED)
        }

        if self.timestamp.is_local_echo() {
            style = style.add_modifier(StyleModifier::ITALIC);
        }

        if settings.tunables.message_user_color {
            let color = settings.get_user_color(&self.sender);
            style = style.fg(color);
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
        let user_gutter = settings.tunables.user_gutter_width;

        if user_gutter + TIME_GUTTER + READ_GUTTER + MIN_MSG_LEN <= width &&
            settings.tunables.read_receipt_display
        {
            let cols = MessageColumns::Four;
            let fill = width - user_gutter - TIME_GUTTER - READ_GUTTER;
            let user = self.show_sender(prev, true, info, settings);
            let time = self.timestamp.show_time();
            let read = info.event_receipts.get(self.event.event_id()).map(|read| read.iter());

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if user_gutter + TIME_GUTTER + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Three;
            let fill = width - user_gutter - TIME_GUTTER;
            let user = self.show_sender(prev, true, info, settings);
            let time = self.timestamp.show_time();
            let read = None;

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if user_gutter + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Two;
            let fill = width - user_gutter;
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

    /// Render the message as a [Text] object for the terminal.
    ///
    /// This will also get the image preview Protocol with an x/y offset.
    pub fn show_with_preview<'a>(
        &'a self,
        prev: Option<&Message>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> (Text<'a>, Option<(&dyn Protocol, u16, u16)>) {
        let width = vwctx.get_width();

        let style = self.get_render_style(selected, settings);
        let mut fmt = self.get_render_format(prev, width, info, settings);
        let mut text = Text::default();
        let width = fmt.width();

        // Show the message that this one replied to, if any.
        let reply = self
            .reply_to()
            .or_else(|| self.thread_root())
            .and_then(|e| info.get_event(&e));

        if let Some(r) = &reply {
            fmt.push_in_reply(r, style, &mut text, info);
        }

        // Now show the message contents, and the inlined reply if we couldn't find it above.
        let (msg, proto) = self.show_msg(
            width,
            style,
            reply.is_some(),
            settings.tunables.message_shortcode_display,
        );

        // Given our text so far, determine the image offset.
        let proto = proto.map(|p| {
            let y_off = text.lines.len() as u16;
            let x_off = fmt.cols.user_gutter_width(settings);
            // Adjust y_off by 1 if a date was printed before the message to account for the extra line.
            let y_off = if fmt.date.is_some() { y_off + 1 } else { y_off };
            (p, x_off, y_off)
        });

        fmt.push_text(msg, style, &mut text);

        if text.lines.is_empty() {
            // If there was nothing in the body, just show an empty message.
            fmt.push_spans(space_span(width, style).into(), style, &mut text);
        }

        if settings.tunables.reaction_display {
            let reactions = info.get_reactions(self.event.event_id());
            fmt.push_reactions(reactions, style, &mut text);
        }

        if let Some(thread) = info.get_thread(Some(self.event.event_id())) {
            fmt.push_thread_reply_count(thread.len(), &mut text);
        }

        (text, proto)
    }

    pub fn show<'a>(
        &'a self,
        prev: Option<&Message>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> Text<'a> {
        self.show_with_preview(prev, selected, vwctx, info, settings).0
    }

    fn show_msg(
        &self,
        width: usize,
        style: Style,
        hide_reply: bool,
        emoji_shortcodes: bool,
    ) -> (Text, Option<&dyn Protocol>) {
        if let Some(html) = &self.html {
            (html.to_text(width, style, hide_reply, emoji_shortcodes), None)
        } else {
            let mut msg = self.event.body();
            if emoji_shortcodes {
                msg = Cow::Owned(replace_emojis_in_str(msg.as_ref()));
            }

            if self.downloaded {
                msg.to_mut().push_str(" \u{2705}");
            }

            let mut proto = None;
            let placeholder = match &self.image_preview {
                ImageStatus::None => None,
                ImageStatus::Downloading(image_preview_size) => {
                    placeholder_frame(Some("Downloading..."), width, image_preview_size)
                },
                ImageStatus::Loaded(backend) => {
                    proto = Some(backend.as_ref());
                    placeholder_frame(None, width, &backend.rect().into())
                },
                ImageStatus::Error(err) => Some(format!("[Image error: {err}]\n")),
            };

            if let Some(placeholder) = placeholder {
                msg.to_mut().insert_str(0, &placeholder);
            }

            (wrapped_text(msg, width, style), proto)
        }
    }

    fn sender_span<'a>(
        &'a self,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> Span<'a> {
        settings.get_user_span(self.sender.as_ref(), info)
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
        let user_gutter = settings.tunables.user_gutter_width;
        let ((truncated, width), _) = take_width(content, user_gutter - 2);
        let padding = user_gutter - 2 - width;

        let sender = if align_right {
            space(padding) + &truncated + "  "
        } else {
            truncated.into_owned() + &space(padding) + "  "
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

impl Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.event.body())
    }
}

#[cfg(test)]
pub mod tests {
    use matrix_sdk::ruma::events::room::{
        message::{
            AudioInfo,
            AudioMessageEventContent,
            FileInfo,
            FileMessageEventContent,
            ImageMessageEventContent,
            VideoInfo,
            VideoMessageEventContent,
        },
        ImageInfo,
    };

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
        let messages = mock_messages();
        let mc1 = MessageCursor::from(MSG1_KEY.clone());
        let mc2 = MessageCursor::from(MSG2_KEY.clone());
        let mc3 = MessageCursor::from(MSG3_KEY.clone());
        let mc4 = MessageCursor::from(MSG4_KEY.clone());
        let mc5 = MessageCursor::from(MSG5_KEY.clone());
        let mc6 = MessageCursor::latest();

        let k1 = mc1.to_key(&messages).unwrap();
        let k2 = mc2.to_key(&messages).unwrap();
        let k3 = mc3.to_key(&messages).unwrap();
        let k4 = mc4.to_key(&messages).unwrap();
        let k5 = mc5.to_key(&messages).unwrap();
        let k6 = mc6.to_key(&messages).unwrap();

        // These should all be equal to their MSGN_KEYs.
        assert_eq!(k1, &MSG1_KEY.clone());
        assert_eq!(k2, &MSG2_KEY.clone());
        assert_eq!(k3, &MSG3_KEY.clone());
        assert_eq!(k4, &MSG4_KEY.clone());
        assert_eq!(k5, &MSG5_KEY.clone());

        // MessageCursor::latest() turns into the largest key (our local echo message).
        assert_eq!(k6, &MSG1_KEY.clone());

        // MessageCursor::latest() fails to convert for a room w/o messages.
        let messages_empty = Messages::default();
        assert_eq!(mc6.to_key(&messages_empty), None);
    }

    #[test]
    fn test_mc_to_from_cursor() {
        let messages = mock_messages();
        let mc1 = MessageCursor::from(MSG1_KEY.clone());
        let mc2 = MessageCursor::from(MSG2_KEY.clone());
        let mc3 = MessageCursor::from(MSG3_KEY.clone());
        let mc4 = MessageCursor::from(MSG4_KEY.clone());
        let mc5 = MessageCursor::from(MSG5_KEY.clone());
        let mc6 = MessageCursor::latest();

        let identity = |mc: &MessageCursor| {
            let c = mc.to_cursor(&messages).unwrap();

            MessageCursor::from_cursor(&c, &messages).unwrap()
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
    fn test_placeholder_frame() {
        fn pretty_frame_test(str: &str) -> Option<String> {
            Some(str[1..].to_string())
        }

        assert_eq!(
            placeholder_frame(None, 4, &ImagePreviewSize { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍


⌎  ⌏
"#
            )
        );

        assert_eq!(placeholder_frame(None, 2, &ImagePreviewSize { width: 4, height: 4 }), None);
        assert_eq!(placeholder_frame(None, 4, &ImagePreviewSize { width: 1, height: 4 }), None);

        assert_eq!(placeholder_frame(None, 4, &ImagePreviewSize { width: 4, height: 1 }), None);

        assert_eq!(
            placeholder_frame(Some("OK"), 4, &ImagePreviewSize { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍
 OK

⌎  ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("idontfit"), 4, &ImagePreviewSize { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍


⌎  ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("OK"), 4, &ImagePreviewSize { width: 4, height: 2 }),
            pretty_frame_test(
                r#"
⌌  ⌍
⌎  ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("OK"), 4, &ImagePreviewSize { width: 2, height: 3 }),
            pretty_frame_test(
                r#"
⌌⌍

⌎⌏
"#
            )
        );
    }

    #[test]
    fn test_display_attachment_size() {
        assert_eq!(
            body_cow_content(&RoomMessageEventContent::new(MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::default()))
            ))),
            "[Attached Image: Alt text]".to_string()
        );

        let mut info = ImageInfo::default();
        info.size = Some(442630_u32.into());
        assert_eq!(
            body_cow_content(&RoomMessageEventContent::new(MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            ))),
            "[Attached Image: Alt text (442.63 kB)]".to_string()
        );

        let mut info = ImageInfo::default();
        info.size = Some(12_u32.into());
        assert_eq!(
            body_cow_content(&RoomMessageEventContent::new(MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            ))),
            "[Attached Image: Alt text (12 B)]".to_string()
        );

        let mut info = AudioInfo::default();
        info.size = Some(4294967295_u32.into());
        assert_eq!(
            body_cow_content(&RoomMessageEventContent::new(MessageType::Audio(
                AudioMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            ))),
            "[Attached Audio: Alt text (4.29 GB)]".to_string()
        );

        let mut info = FileInfo::default();
        info.size = Some(4426300_u32.into());
        assert_eq!(
            body_cow_content(&RoomMessageEventContent::new(MessageType::File(
                FileMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            ))),
            "[Attached File: Alt text (4.43 MB)]".to_string()
        );

        let mut info = VideoInfo::default();
        info.size = Some(44000_u32.into());
        assert_eq!(
            body_cow_content(&RoomMessageEventContent::new(MessageType::Video(
                VideoMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            ))),
            "[Attached Video: Alt text (44 kB)]".to_string()
        );
    }
}
