//! # Room Messages
use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::BTreeMap;
use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

use chrono::{DateTime, Local as LocalTz};
use humansize::{DECIMAL, format_size};
use matrix_sdk::ruma::OwnedTransactionId;
use matrix_sdk::ruma::events::receipt::ReceiptThread;
use matrix_sdk::ruma::events::room::MediaSource;
use matrix_sdk::ruma::events::room::message::RoomMessageEventContentWithoutRelation;
use matrix_sdk::ruma::events::sticker::{OriginalStickerEvent, RedactedStickerEvent, StickerEvent};
use matrix_sdk::ruma::events::{AnyRedactionEvent, MessageLikeEvent};
use matrix_sdk::send_queue::SendHandle;
use ratatui::layout::Size;
use ratatui::style::Color;
use ratatui_image::sliced::SlicedProtocol;
use unicode_width::UnicodeWidthStr;

use matrix_sdk::ruma::{
    EventId,
    MilliSecondsSinceUnixEpoch,
    OwnedEventId,
    OwnedUserId,
    UInt,
    events::{
        AnySyncStateEvent,
        RedactedUnsigned,
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
    },
};

use ratatui::{
    style::{Modifier as StyleModifier, Style},
    symbols::line::THICK_VERTICAL,
    text::{Line, Span, Text},
};

use modalkit::editing::cursor::Cursor;
use modalkit::prelude::*;

use crate::base::MessageEdits;
use crate::preview::{ImageStatus, PreviewKind, PreviewManager};
use crate::{
    base::RoomInfo,
    config::ApplicationSettings,
    message::html::{StyleTree, parse_matrix_html},
    util::{replace_emojis_in_str, space, space_span, take_width, wrapped_text},
};

mod compose;
mod html;
mod printer;
mod state;

pub use self::compose::{text_to_message, text_to_text_message_event_content};
use self::state::{body_cow_state, html_state};
pub use html::TreeGenState;

type ProtocolPreview<'a> = (&'a SlicedProtocol, u16, u16);

/// The key used for uniquely identifying messages within a room and its threads.
///
/// Note that the ordering of the fields is important here, so that the derived
/// `Ord` trait will sort by timestamp first, and then sort by the message ID.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct MessageKey {
    pub ts: MessageTimeStamp,
    pub id: MessageId,
}

pub struct Messages(BTreeMap<MessageKey, Message>, pub ReceiptThread);

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
    pub fn new(thread: ReceiptThread) -> Self {
        Self(Default::default(), thread)
    }

    pub fn main() -> Self {
        Self::new(ReceiptThread::Main)
    }

    pub fn thread(root: OwnedEventId) -> Self {
        Self::new(ReceiptThread::Thread(root))
    }

    pub fn insert_message(&mut self, key: MessageKey, msg: impl Into<Message>) {
        let mut msg = msg.into();
        if let MessageEvent::Original(ev, edits) = &mut msg.event {
            strip_reply_fallback(&mut ev.content.msgtype);
            for edit in edits.values_mut() {
                strip_reply_fallback(&mut edit.msgtype);
            }
        }

        self.0.entry(key).or_insert(msg);
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

/// Hash an [`MessageId`] into a [`usize`].
fn hash_message_id(id: &MessageId) -> Option<usize> {
    let mut hasher = DefaultHasher::new();
    id.hash(&mut hasher);
    hash_finish_usize(hasher)
}

/// Before the image is loaded, already display a placeholder frame of the image size.
fn placeholder_frame(
    text: Option<&str>,
    outer_width: usize,
    image_preview_size: &Size,
) -> Option<String> {
    let Size { width, height } = image_preview_size;
    let width = usize::min(*width as usize, outer_width);
    if width < 2 || *height < 2 {
        return None;
    }
    let mut placeholder = "\u{230c}".to_string();
    placeholder.push_str(&" ".repeat(width - 2));
    placeholder.push('\u{230d}');
    placeholder.push_str(&"\n".repeat((*height as usize - 1) / 2));

    if *height > 2 &&
        let Some(text) = text &&
        text.width() <= width - 2
    {
        placeholder.push(' ');
        placeholder.push_str(text);
    }

    placeholder.push_str(&"\n".repeat(*height as usize / 2));
    placeholder.push('\u{230e}');
    placeholder.push_str(&" ".repeat(width - 2));
    placeholder.push_str("\u{230f}\n");
    Some(placeholder)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum MessageId {
    Origin(OwnedEventId),
    Local(OwnedTransactionId),
}

impl MessageId {
    pub fn as_origin(&self) -> Option<&EventId> {
        match self {
            Self::Origin(id) => Some(id),
            _ => None,
        }
    }
}

impl From<OwnedEventId> for MessageId {
    fn from(value: OwnedEventId) -> Self {
        Self::Origin(value)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum TimeStampIntError {
    #[error("Integer conversion error: {0}")]
    IntError(#[from] std::num::TryFromIntError),

    #[error("UInt conversion error: {0}")]
    UIntError(<UInt as TryFrom<u64>>::Error),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct MessageTimeStamp(pub MilliSecondsSinceUnixEpoch);

impl MessageTimeStamp {
    fn as_datetime(self) -> DateTime<LocalTz> {
        let time = i64::from(self.0.0) / 1000;
        let time = DateTime::from_timestamp(time, 0).unwrap_or_default();
        time.into()
    }

    fn same_day(self, other: Self) -> bool {
        let dt1 = self.as_datetime();
        let dt2 = other.as_datetime();

        dt1.date_naive() == dt2.date_naive()
    }

    fn show_date(self) -> Span<'static> {
        let time = self.as_datetime().format("%A, %B %d %Y").to_string();

        Span::styled(time, BOLD_STYLE)
    }

    fn show_time(self) -> Span<'static> {
        let time = self.as_datetime().format("%T");
        let time = format!("  [{time}]");

        Span::raw(time)
    }
}

impl From<MilliSecondsSinceUnixEpoch> for MessageTimeStamp {
    fn from(millis: MilliSecondsSinceUnixEpoch) -> Self {
        Self(millis)
    }
}

impl TryFrom<&MessageTimeStamp> for usize {
    type Error = TimeStampIntError;

    fn try_from(ts: &MessageTimeStamp) -> Result<Self, Self::Error> {
        let n = usize::try_from(u64::from(ts.0.0))?;

        Ok(n)
    }
}

impl TryFrom<usize> for MessageTimeStamp {
    type Error = TimeStampIntError;

    fn try_from(u: usize) -> Result<Self, Self::Error> {
        let n = u64::try_from(u)?;
        let n = UInt::try_from(n).map_err(TimeStampIntError::UIntError)?;

        Ok(MessageTimeStamp::from(MilliSecondsSinceUnixEpoch(n)))
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
        let ev_term = OwnedEventId::try_from("$").ok()?.into();

        let ts_start = MessageTimeStamp::try_from(cursor.get_y()).ok()?;
        let start = MessageKey { ts: ts_start, id: ev_term };

        for (key, _) in thread.range(&start..) {
            if hash_message_id(&key.id)? == ev_hash {
                return Self::from(key.clone()).into();
            }

            if key.ts > ts_start {
                break;
            }
        }

        // If we can't find the cursor, then go to the nearest timestamp.
        thread.range(start..).next().map(|(key, _)| Self::from(key.clone()))
    }

    pub fn to_cursor(&self, thread: &Messages) -> Option<Cursor> {
        let key = self.to_key(thread)?;

        let y = usize::try_from(&key.ts).ok()?;
        let x = hash_message_id(&key.id)?;

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

fn redaction_reason_event(ev: SyncRoomRedactionEvent) -> Option<String> {
    let SyncRoomRedactionEvent::Original(ev) = ev else {
        return None;
    };

    ev.content.reason
}

pub fn strip_reply_fallback(msgtype: &mut MessageType) {
    let MessageType::Text(content) = msgtype else {
        return;
    };

    if !content.body.starts_with('>') {
        return;
    }

    let new_body = content.body.lines().skip_while(|line| line.starts_with('>')).collect();

    content.body = new_body;
}

fn content_html(msgtype: &MessageType) -> Option<StyleTree> {
    let formatted = match msgtype {
        MessageType::Text(content) => content.formatted.as_ref(),
        MessageType::Emote(content) => content.formatted.as_ref(),
        MessageType::Notice(content) => content.formatted.as_ref(),

        MessageType::Audio(content) => content.formatted.as_ref(),
        MessageType::File(content) => content.formatted.as_ref(),
        MessageType::Image(content) => content.formatted.as_ref(),
        MessageType::Video(content) => content.formatted.as_ref(),
        _ => None,
    };

    if let Some(FormattedBody { format: MessageFormat::Html, body }) = formatted {
        Some(parse_matrix_html(body.as_str()))
    } else {
        None
    }
}

#[derive(Clone, Debug)]
pub enum MessageEvent {
    EncryptedOriginal(Box<OriginalRoomEncryptedEvent>),
    EncryptedRedacted(Box<RedactedRoomEncryptedEvent>),
    Original(Box<OriginalRoomMessageEvent>, MessageEdits),
    Redacted(OwnedEventId, Option<String>),
    State(Box<AnySyncStateEvent>),
    Sticker(Box<OriginalStickerEvent>, MediaSource),
    Local(OwnedTransactionId, SendHandle, Box<RoomMessageEventContent>),
}

impl MessageEvent {
    pub fn event_id(&self) -> Option<&EventId> {
        let event_id = match self {
            MessageEvent::EncryptedOriginal(ev) => ev.event_id.as_ref(),
            MessageEvent::EncryptedRedacted(ev) => ev.event_id.as_ref(),
            MessageEvent::Original(ev, _) => ev.event_id.as_ref(),
            MessageEvent::Redacted(event_id, _) => event_id.as_ref(),
            MessageEvent::State(ev) => ev.event_id(),
            MessageEvent::Local(..) => return None,
            MessageEvent::Sticker(ev, ..) => ev.event_id.as_ref(),
        };

        Some(event_id)
    }

    pub fn msgtype(&self) -> Option<&MessageType> {
        match self {
            MessageEvent::EncryptedOriginal(_) => None,
            MessageEvent::Original(ev, edits) => {
                edits
                    .last_key_value()
                    .map(|(_, ev)| &ev.msgtype)
                    .or(Some(&ev.content.msgtype))
            },
            MessageEvent::EncryptedRedacted(_) => None,
            MessageEvent::Redacted(_, _) => None,
            MessageEvent::State(_) => None,
            MessageEvent::Sticker(..) => None,
            MessageEvent::Local(_, _, content) => Some(&content.msgtype),
        }
    }

    pub fn body(&self) -> Cow<'_, str> {
        match self {
            MessageEvent::EncryptedOriginal(_) => "[Unable to decrypt message]".into(),
            MessageEvent::Original(ev, edits) => {
                let msgtype = edits
                    .last_key_value()
                    .map(|(_, ev)| &ev.msgtype)
                    .unwrap_or(&ev.content.msgtype);
                body_cow_content(msgtype)
            },
            MessageEvent::EncryptedRedacted(ev) => {
                body_cow_reason(redaction_reason_unsigned(&ev.unsigned).as_deref())
            },
            MessageEvent::Redacted(_, reason) => body_cow_reason(reason.as_deref()),
            MessageEvent::Sticker(ev, ..) => body_cow_sticker(ev),
            MessageEvent::State(ev) => body_cow_state(ev),
            MessageEvent::Local(_, _, content) => body_cow_content(&content.msgtype),
        }
    }

    pub fn html(&self) -> Option<StyleTree> {
        if let MessageEvent::State(ev) = self {
            return Some(html_state(ev));
        }

        self.msgtype().and_then(content_html)
    }

    pub fn filename(&self) -> Option<String> {
        self.msgtype().and_then(content_filename)
    }

    fn redact(&mut self, redaction: SyncRoomRedactionEvent) {
        match self {
            MessageEvent::EncryptedOriginal(_) => return,
            MessageEvent::EncryptedRedacted(_) => return,
            MessageEvent::Redacted(_, _) => return,
            MessageEvent::State(_) => return,
            MessageEvent::Sticker(ev, ..) => {
                let event_id = ev.event_id.to_owned();
                let reason = redaction_reason_event(redaction);
                *self = MessageEvent::Redacted(event_id, reason);
            },
            MessageEvent::Local(..) => return,
            MessageEvent::Original(ev, _) => {
                let event_id = ev.event_id.to_owned();
                let reason = redaction_reason_event(redaction);
                *self = MessageEvent::Redacted(event_id, reason);
            },
        }
    }

    fn is_edited(&self) -> bool {
        if let MessageEvent::Original(_, edits) = self {
            !edits.is_empty()
        } else {
            false
        }
    }
}

/// Macro rule converting a File / Image / Audio / Video to its text content with the shape:
/// `[Attached <type>: <content>[ (<human readable file size>)]]`
macro_rules! display_file_name {
    ( $msgtype:ident, $content:expr ) => {{
        Some(format!(
            "[Attached {}: {}{}]",
            stringify!($msgtype),
            $content.filename(),
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
    }};
}

/// Macro rule extraction the text caption of a File / Image / Audio / Video
macro_rules! display_file_to_text {
    ( $msgtype:ident, $content:expr ) => {{
        if $content
            .filename
            .as_ref()
            .is_none_or(|filename| *filename == $content.body)
        {
            return Cow::Borrowed("");
        }
        $content.body.as_str()
    }};
}

fn content_filename(msgtype: &MessageType) -> Option<String> {
    match msgtype {
        MessageType::Audio(content) => {
            display_file_name!(Audio, content)
        },
        MessageType::File(content) => {
            display_file_name!(File, content)
        },
        MessageType::Image(content) => {
            display_file_name!(Image, content)
        },
        MessageType::Video(content) => {
            display_file_name!(Video, content)
        },
        _ => None,
    }
}

fn body_cow_content(msgtype: &MessageType) -> Cow<'_, str> {
    let s = match msgtype {
        MessageType::Text(content) => content.body.as_str(),
        MessageType::VerificationRequest(_) => "[Verification Request]",
        MessageType::Emote(content) => content.body.as_ref(),
        MessageType::Notice(content) => content.body.as_str(),
        MessageType::ServerNotice(content) => content.body.as_str(),

        MessageType::Audio(content) => {
            display_file_to_text!(Audio, content)
        },
        MessageType::File(content) => {
            display_file_to_text!(File, content)
        },
        MessageType::Image(content) => {
            display_file_to_text!(Image, content)
        },
        MessageType::Video(content) => {
            display_file_to_text!(Video, content)
        },
        _ => msgtype.body(),
    };

    Cow::Borrowed(s)
}

fn body_cow_sticker(sticker: &OriginalStickerEvent) -> Cow<'_, str> {
    Cow::Owned(format!("* sent a sticker: {}", sticker.content.body))
}

fn redaction_reason_unsigned(unsigned: &RedactedUnsigned) -> Option<String> {
    let ev = unsigned.redacted_because.deserialize().ok()?;

    let AnyRedactionEvent::RoomRedaction(ev) = ev else {
        return None;
    };

    ev.content.reason
}

fn body_cow_reason(reason: Option<&str>) -> Cow<'static, str> {
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

#[derive(Default, Debug)]
enum SenderSpan<'a> {
    /// Show the sender name in the user gutter.
    /// This is truncated and padded to fit [`user_gutter_width`](`crate::config::TunableValues::user_gutter_width`).
    Gutter(Span<'a>),

    /// Show the sender name in an extra line at the top of the message.
    Line(Span<'a>),

    /// The sender name has already been printed.
    #[default]
    None,
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
    user: SenderSpan<'a>,

    /// The time the message was sent.
    time: Option<Span<'a>>,

    /// The date the message was sent.
    date: Option<Span<'a>>,

    /// The users who have read up to this message.
    read: Vec<OwnedUserId>,
}

impl<'a> MessageFormatter<'a> {
    fn width(&self) -> usize {
        self.fill
    }

    fn message_start_line(&self) -> u16 {
        let mut line = 0;

        if self.date.is_some() {
            line += 1;
        }

        if let SenderSpan::Line(_) = self.user {
            line += 1;
        }

        line
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

        let user_gutter = match std::mem::take(&mut self.user) {
            SenderSpan::Line(user) => {
                text.lines.push(user.into());
                user_gutter_empty_span
            },
            SenderSpan::Gutter(user) => user,
            SenderSpan::None => user_gutter_empty_span,
        };

        match self.cols {
            MessageColumns::Four => {
                let settings = self.settings;
                let time = self.time.take().unwrap_or(TIME_GUTTER_EMPTY_SPAN);

                let mut line = vec![user_gutter];
                line.extend(prev_line.spans);
                line.push(time);

                // Show read receipts.
                let user_char = |user: OwnedUserId| -> Span { settings.get_user_char_span(&user) };

                let a = self.read.pop().map(user_char).unwrap_or_else(|| Span::raw(" "));
                let b = self.read.pop().map(user_char).unwrap_or_else(|| Span::raw(" "));
                let c = self.read.pop().map(user_char).unwrap_or_else(|| Span::raw(" "));

                line.push(Span::raw(" "));
                line.push(c);
                line.push(b);
                line.push(a);
                line.push(Span::raw(" "));

                text.lines.push(Line::from(line))
            },
            MessageColumns::Three => {
                let time = self.time.take().unwrap_or_else(|| Span::from(""));

                let mut line = vec![user_gutter];
                line.extend(prev_line.spans);
                line.push(time);

                text.lines.push(Line::from(line))
            },
            MessageColumns::Two => {
                let mut line = vec![user_gutter];
                line.extend(prev_line.spans);

                text.lines.push(Line::from(line));
            },
            MessageColumns::One => {
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
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
    ) -> Option<ProtocolPreview<'a>> {
        let reply_style = if settings.tunables.message_user_color {
            style.patch(settings.get_user_color(&msg.sender))
        } else {
            style
        };

        let width = self.width();
        let w = width.saturating_sub(2);
        let (mut replied, proto) = msg.show_msg(w, reply_style, settings, previews);
        let mut sender = msg.sender_span(info, self.settings);
        let sender_width = UnicodeWidthStr::width(sender.content.as_ref());
        let trailing = w.saturating_sub(sender_width + 1);

        sender.style = sender.style.patch(reply_style);

        self.push_spans(
            Line::from(vec![
                Span::styled(" ", style),
                Span::styled(THICK_VERTICAL, style),
                sender,
                Span::styled(":", reply_style),
                space_span(trailing, reply_style),
            ]),
            style,
            text,
        );

        // Determine the image offset of the reply header, taking into account the formatting
        let proto = proto.map(|p| {
            let y_off = text.lines.len() as u16;
            // Adjust x_off by 2 to account for the vertical line and indent
            let x_off = self.cols.user_gutter_width(settings) + 2;
            (p, x_off, y_off)
        });

        for line in replied.lines.iter_mut() {
            line.spans.insert(0, Span::styled(THICK_VERTICAL, style));
            line.spans.insert(0, Span::styled(" ", style));
        }

        self.push_text(replied, reply_style, text);

        proto
    }

    fn push_reactions(
        &mut self,
        counts: Vec<(&'a str, usize, &'a Option<MediaSource>)>,
        style: Style,
        text: &mut Text<'a>,
        settings: &ApplicationSettings,
        previews: &'a PreviewManager,
    ) -> Vec<ProtocolPreview<'a>> {
        let mut emojis = printer::TextPrinter::new(self.width(), style, self.settings);
        let mut reactions = 0;
        let mut protos = Vec::new();

        for (key, count, source) in counts {
            if reactions != 0 {
                emojis.push_str(" ", style);
            }

            let proto = match source
                .as_ref()
                .and_then(|source| previews.get(source, PreviewKind::Reaction))
            {
                Some(ImageStatus::Loaded(backend)) => Some(Some(backend)),
                // Use empty space as placeholder
                Some(ImageStatus::Queued(_)) | Some(ImageStatus::Downloading(_)) => Some(None),
                // Fall back to text
                None | Some(ImageStatus::Error(_)) => None,
            };

            let name = if proto.is_some() {
                "  "
            } else if self.settings.tunables.reaction_shortcode_display {
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
            if let Some(Some(proto)) = proto {
                let (x, y) = emojis.cursor_pos();
                let y = (y + text.lines.len()) as u16;
                let x = x as u16 + self.cols.user_gutter_width(settings);

                protos.push((proto, x, y));
            }
            emojis.push_str(name, style);
            emojis.push_str(" ", style);
            emojis.push_span_nobreak(Span::styled(count.to_string(), style));
            emojis.push_str("]", style);

            reactions += 1;
        }

        if reactions > 0 {
            self.push_text(emojis.finish(), style, text);
        }

        protos
    }

    fn push_thread_reply_count(&mut self, len: usize, text: &mut Text<'a>) {
        if len == 0 {
            return;
        }

        // If we have threaded replies to this message, show how many.
        let plural = len != 1;
        let style = Style::default();
        let mut threaded =
            printer::TextPrinter::new(self.width(), style, self.settings).literal(true);
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

pub struct Message {
    pub event: MessageEvent,
    pub sender: OwnedUserId,
    pub timestamp: MessageTimeStamp,
    pub downloaded: bool,
    pub html: Option<StyleTree>,
}

impl Message {
    pub fn new(event: MessageEvent, sender: OwnedUserId, timestamp: MessageTimeStamp) -> Self {
        let html = event.html();
        let downloaded = false;

        Message { event, sender, timestamp, downloaded, html }
    }

    pub fn reply_to(&self) -> Option<OwnedEventId> {
        let content = match &self.event {
            MessageEvent::EncryptedOriginal(_) => return None,
            MessageEvent::EncryptedRedacted(_) => return None,
            MessageEvent::Local(_, _, content) => content,
            MessageEvent::Original(ev, _) => &ev.content,
            MessageEvent::Redacted(_, _) => return None,
            MessageEvent::State(_) => return None,
            MessageEvent::Sticker(ev, ..) => {
                return match &ev.content.relates_to {
                    Some(Relation::Reply(reply)) => Some(reply.in_reply_to.event_id.clone()),
                    Some(Relation::Thread(Thread {
                        in_reply_to: Some(in_reply_to),
                        is_falling_back: false,
                        ..
                    })) => Some(in_reply_to.event_id.clone()),
                    Some(_) | None => None,
                };
            },
        };

        match &content.relates_to {
            Some(Relation::Reply(reply)) => Some(reply.in_reply_to.event_id.clone()),
            Some(Relation::Thread(Thread {
                in_reply_to: Some(in_reply_to),
                is_falling_back: false,
                ..
            })) => Some(in_reply_to.event_id.clone()),
            Some(_) | None => None,
        }
    }

    pub fn thread_root(&self) -> Option<OwnedEventId> {
        let content = match &self.event {
            MessageEvent::EncryptedOriginal(_) => return None,
            MessageEvent::EncryptedRedacted(_) => return None,
            MessageEvent::Local(_, _, content) => content,
            MessageEvent::Original(ev, _) => &ev.content,
            MessageEvent::Redacted(_, _) => return None,
            MessageEvent::State(_) => return None,
            MessageEvent::Sticker(..) => return None,
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

    pub fn image_preview(&self) -> Option<&MediaSource> {
        if let Some(MessageType::Image(c)) = self.event.msgtype() {
            return Some(&c.source);
        }

        match &self.event {
            MessageEvent::Sticker(_, source) => Some(source),

            _ => None,
        }
    }

    fn get_render_style(&self, selected: bool, settings: &ApplicationSettings) -> Style {
        let mut style = Style::default();

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED)
        }

        if matches!(self.event, MessageEvent::Local(..)) {
            style = style.add_modifier(StyleModifier::ITALIC);
        }

        if settings.tunables.message_user_color {
            let color = settings.get_user_color(&self.sender);
            style = style.fg(color);
        }

        return style;
    }

    pub fn show_date(&self, prev: Option<&Message>) -> bool {
        let Some(prev) = prev else { return true };

        !prev.timestamp.same_day(self.timestamp)
    }
    pub fn message_column_width(
        viewctx: &ViewportContext<MessageCursor>,
        settings: &ApplicationSettings,
    ) -> usize {
        let width = viewctx.get_width();
        let user_gutter = settings.tunables.user_gutter_width;

        if user_gutter + TIME_GUTTER + READ_GUTTER + MIN_MSG_LEN <= width &&
            settings.tunables.read_receipt_display
        {
            width - user_gutter - TIME_GUTTER - READ_GUTTER
        } else if user_gutter + TIME_GUTTER + MIN_MSG_LEN <= width {
            width - user_gutter - TIME_GUTTER
        } else if user_gutter + MIN_MSG_LEN <= width {
            width - user_gutter
        } else {
            width.saturating_sub(2)
        }
    }

    fn get_render_format<'a>(
        &'a self,
        prev: Option<&Message>,
        width: usize,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> MessageFormatter<'a> {
        let orig = width;
        let date = self.show_date(prev).then(|| self.timestamp.show_date());
        let user_gutter = settings.tunables.user_gutter_width;

        if user_gutter + TIME_GUTTER + READ_GUTTER + MIN_MSG_LEN <= width &&
            settings.tunables.read_receipt_display
        {
            let cols = MessageColumns::Four;
            let fill = width - user_gutter - TIME_GUTTER - READ_GUTTER;
            let user = self.show_sender(prev, true, info, settings, width);
            let time = Some(self.timestamp.show_time());
            let read = info
                .event_receipts
                .values()
                .filter_map(|receipts| self.event.event_id().and_then(|id| receipts.get(id)))
                .flat_map(|read| read.iter())
                .map(|user_id| user_id.to_owned())
                .collect();

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if user_gutter + TIME_GUTTER + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Three;
            let fill = width - user_gutter - TIME_GUTTER;
            let user = self.show_sender(prev, true, info, settings, width);
            let time = Some(self.timestamp.show_time());
            let read = Vec::new();

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if user_gutter + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Two;
            let fill = width - user_gutter;
            let user = self.show_sender(prev, true, info, settings, width);
            let time = None;
            let read = Vec::new();

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else {
            let cols = MessageColumns::One;
            let fill = width.saturating_sub(2);
            let user = self.show_sender(prev, false, info, settings, width);
            let time = None;
            let read = Vec::new();

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
        previews: &'a PreviewManager,
    ) -> (Text<'a>, Vec<ProtocolPreview<'a>>) {
        let width = vwctx.get_width();

        let style = self.get_render_style(selected, settings);
        let mut fmt = self.get_render_format(prev, width, info, settings);
        let mut text = Text::default();
        let width = fmt.width();

        let mut protos = Vec::new();

        // Show the message that this one replied to, if any.
        let reply = self.reply_to().or_else(|| self.thread_root()).map(|e| info.get_event(&e));
        if let Some(r) = reply {
            if let Some(r) = r {
                // Format the reply header, push it into the `Text` buffer, and get any image.
                let proto_reply = fmt.push_in_reply(r, style, &mut text, info, settings, previews);
                if let Some(proto) = proto_reply {
                    protos.push(proto)
                }
            } else {
                fmt.push_spans(
                    Line::from(vec![
                        Span::styled(" ", style),
                        Span::styled(THICK_VERTICAL, style),
                        Span::styled("Original message not loaded", style),
                        space_span(width.saturating_sub(29), style),
                    ]),
                    style,
                    &mut text,
                );
            }
        }

        // Now show the message contents, and the inlined reply if we couldn't find it above.
        let (msg, proto) = self.show_msg(width, style, settings, previews);

        // Given our text so far, determine the image offset.
        if let Some(p) = proto {
            let y_off = text.lines.len() as u16;
            let x_off = fmt.cols.user_gutter_width(settings);

            // Account for extra lines printed before the message;
            let y_off = y_off + fmt.message_start_line();
            protos.push((p, x_off, y_off));
        }

        fmt.push_text(msg, style, &mut text);

        if text.lines.is_empty() {
            // If there was nothing in the body, just show an empty message.
            fmt.push_spans(space_span(width, style).into(), style, &mut text);
        }

        if self.event.is_edited() {
            fmt.push_spans(
                Line::from(vec![
                    Span::styled("(edited)", style.fg(Color::Gray)),
                    space_span(fmt.width().saturating_sub(8), style),
                ]),
                style,
                &mut text,
            );
        }

        if settings.tunables.reaction_display {
            let reactions =
                self.event.event_id().map(|id| info.get_reactions(id)).unwrap_or_default();
            let react_protos = fmt.push_reactions(reactions, style, &mut text, settings, previews);
            protos.extend(react_protos);
        }

        if let Some(thread) = self.event.event_id().and_then(|id| info.get_thread(Some(id))) {
            fmt.push_thread_reply_count(thread.len(), &mut text);
        }

        (text, protos)
    }

    pub fn show<'a>(
        &'a self,
        prev: Option<&Message>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
    ) -> Text<'a> {
        self.show_with_preview(prev, selected, vwctx, info, settings, previews).0
    }

    fn show_msg<'a>(
        &'a self,
        width: usize,
        style: Style,
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
    ) -> (Text<'a>, Option<&'a SlicedProtocol>) {
        let mut proto = None;
        let placeholder = match self
            .image_preview()
            .and_then(|source| previews.get(source, PreviewKind::Message))
        {
            None => None,
            Some(ImageStatus::Queued(image_preview_size)) => {
                placeholder_frame(Some("Queued..."), width, image_preview_size)
            },
            Some(ImageStatus::Downloading(image_preview_size)) => {
                placeholder_frame(Some("Downloading..."), width, image_preview_size)
            },
            Some(ImageStatus::Loaded(backend)) => {
                proto = Some(backend);
                placeholder_frame(None, width, &backend.size())
            },
            Some(ImageStatus::Error(err)) => Some(format!("[Image error: {err}]\n")),
        };

        let mut text = if let Some(placeholder) = placeholder {
            wrapped_text(placeholder, width, style)
        } else {
            Default::default()
        };

        if let Some(mut filename) = self.event.filename() {
            if self.downloaded {
                filename.push_str(" \u{2705}");
            }

            text += wrapped_text(filename, width, style);
        }

        if let Some(html) = &self.html {
            text += html.to_text(width, style, settings);
        } else {
            let mut msg = self.event.body();
            if settings.tunables.message_shortcode_display {
                msg = Cow::Owned(replace_emojis_in_str(msg.as_ref()));
            }
            text += wrapped_text(msg, width, style);
        };

        (text, proto)
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
        gutter_enabled: bool,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
        width: usize,
    ) -> SenderSpan<'a> {
        if let Some(prev) = prev &&
            self.sender == prev.sender &&
            self.timestamp.same_day(prev.timestamp) &&
            !matches!(self.event.msgtype(), Some(MessageType::Emote(_)))
        {
            return SenderSpan::None;
        }

        let Span { content, style } = self.sender_span(info, settings);
        let user_gutter = settings.tunables.user_gutter_width;

        let show_in_gutter = gutter_enabled && user_gutter > 2;

        if show_in_gutter {
            let ((truncated, width), _) = take_width(content, user_gutter - 2);
            let padding = user_gutter - 2 - width;

            let sender = format!("{}{}  ", space(padding), truncated);

            SenderSpan::Gutter(Span::styled(sender, style))
        } else if UnicodeWidthStr::width(content.as_ref()) > width {
            let ((truncated, _), _) = take_width(content, width);

            SenderSpan::Line(Span::styled(truncated, style))
        } else {
            SenderSpan::Line(Span::styled(content, style))
        }
    }

    pub fn redact(&mut self, redaction: SyncRoomRedactionEvent) {
        self.event.redact(redaction);
        self.html = None;
        self.downloaded = false;
    }

    pub fn set_edits(&mut self, new_edits: MessageEdits) {
        if let MessageEvent::Original(orig, edits) = &mut self.event {
            *edits = new_edits;

            for edit in edits.values_mut() {
                strip_reply_fallback(&mut edit.msgtype);
            }

            if let Some(most_recent) = edits.last_key_value() {
                self.html = content_html(&most_recent.1.msgtype);
            } else {
                self.html = content_html(&orig.content.msgtype);
            }
        }
    }

    pub fn insert_edit(
        &mut self,
        key: MessageKey,
        mut edit: RoomMessageEventContentWithoutRelation,
    ) {
        if let MessageEvent::Original(_, edits) = &mut self.event {
            strip_reply_fallback(&mut edit.msgtype);

            let inserted = edits.entry(key).insert_entry(edit);
            self.html = content_html(&inserted.get().msgtype);
        }
    }

    pub fn remove_edit(&mut self, key: &MessageKey) {
        let MessageEvent::Original(orig_content, edits) = &mut self.event else {
            return;
        };

        edits.remove(key);

        let content = edits
            .last_key_value()
            .map(|(_, msg)| &msg.msgtype)
            .unwrap_or(&orig_content.content.msgtype);

        self.html = content_html(content);
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
        let content = MessageEvent::Original(event.into(), Default::default());

        Message::new(content, user_id, timestamp)
    }
}

impl From<RedactedRoomMessageEvent> for Message {
    fn from(event: RedactedRoomMessageEvent) -> Self {
        let timestamp = event.origin_server_ts.into();
        let user_id = event.sender.clone();

        let event_id = event.event_id;
        let reason = redaction_reason_unsigned(&event.unsigned);
        let content = MessageEvent::Redacted(event_id, reason);

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

impl From<AnySyncStateEvent> for Message {
    fn from(event: AnySyncStateEvent) -> Self {
        let timestamp = event.origin_server_ts().into();
        let user_id = event.sender().to_owned();
        let event = MessageEvent::State(event.into());

        Message::new(event, user_id, timestamp)
    }
}

impl From<OriginalStickerEvent> for Message {
    fn from(event: OriginalStickerEvent) -> Self {
        let timestamp = event.origin_server_ts.into();
        let user_id = event.sender.clone();
        let source = event.content.source.clone().into();
        let content = MessageEvent::Sticker(event.into(), source);

        Message::new(content, user_id, timestamp)
    }
}

impl From<RedactedStickerEvent> for Message {
    fn from(event: RedactedStickerEvent) -> Self {
        let timestamp = event.origin_server_ts.into();
        let user_id = event.sender.clone();

        let event_id = event.event_id;
        let reason = redaction_reason_unsigned(&event.unsigned);
        let content = MessageEvent::Redacted(event_id, reason);

        Message::new(content, user_id, timestamp)
    }
}

impl From<StickerEvent> for Message {
    fn from(event: StickerEvent) -> Self {
        match event {
            MessageLikeEvent::Original(ev) => ev.into(),
            MessageLikeEvent::Redacted(ev) => ev.into(),
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
        ImageInfo,
        message::{
            AudioInfo,
            AudioMessageEventContent,
            FileInfo,
            FileMessageEventContent,
            ImageMessageEventContent,
            VideoInfo,
            VideoMessageEventContent,
        },
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
        let messages_empty = Messages::new(ReceiptThread::Main);
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
            placeholder_frame(None, 4, &Size { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍


⌎  ⌏
"#
            )
        );

        assert_eq!(
            placeholder_frame(None, 2, &Size { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌⌍


⌎⌏
"#
            )
        );
        assert_eq!(placeholder_frame(None, 4, &Size { width: 1, height: 4 }), None);

        assert_eq!(placeholder_frame(None, 4, &Size { width: 4, height: 1 }), None);

        assert_eq!(
            placeholder_frame(Some("OK"), 4, &Size { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍
 OK

⌎  ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("OK"), 6, &Size { width: 6, height: 6 }),
            pretty_frame_test(
                r#"
⌌    ⌍

 OK


⌎    ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("OK"), 6, &Size { width: 6, height: 7 }),
            pretty_frame_test(
                r#"
⌌    ⌍


 OK


⌎    ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("idontfit"), 4, &Size { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍


⌎  ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("OK"), 4, &Size { width: 4, height: 2 }),
            pretty_frame_test(
                r#"
⌌  ⌍
⌎  ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("OK"), 4, &Size { width: 2, height: 3 }),
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
            content_filename(&MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::default()))
            )),
            "[Attached Image: Alt text]".to_string().into()
        );

        let mut info = ImageInfo::default();
        info.size = Some(442630_u32.into());
        assert_eq!(
            content_filename(&MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Image: Alt text (442.63 kB)]".to_string().into()
        );

        let mut info = ImageInfo::default();
        info.size = Some(12_u32.into());
        assert_eq!(
            content_filename(&MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Image: Alt text (12 B)]".to_string().into()
        );

        let mut info = AudioInfo::default();
        info.size = Some(4294967295_u32.into());
        assert_eq!(
            content_filename(&MessageType::Audio(
                AudioMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Audio: Alt text (4.29 GB)]".to_string().into()
        );

        let mut info = FileInfo::default();
        info.size = Some(4426300_u32.into());
        assert_eq!(
            content_filename(&MessageType::File(
                FileMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached File: Alt text (4.43 MB)]".to_string().into()
        );

        let mut info = VideoInfo::default();
        info.size = Some(44000_u32.into());
        assert_eq!(
            content_filename(&MessageType::Video(
                VideoMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Video: Alt text (44 kB)]".to_string().into()
        );
    }
}
