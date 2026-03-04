//! # Room Messages
use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::hash::{Hash, Hasher};
use std::ops::{Bound, RangeBounds};
use std::sync::Arc;

use alternating_iter::AlternatingExt;
use chrono::{DateTime, Local as LocalTz};
use humansize::{format_size, DECIMAL};
use matrix_sdk::ruma::events::receipt::ReceiptThread;
use matrix_sdk::ruma::events::room::message::TextMessageEventContent;
use matrix_sdk::ruma::events::room::MediaSource;
use matrix_sdk_ui::eyeball_im::Vector;
use matrix_sdk_ui::timeline::{
    EventTimelineItem,
    MsgLikeContent,
    MsgLikeKind,
    ReactionsByKeyBySender,
    TimelineEventItemId,
    TimelineItem,
    TimelineItemContent,
    TimelineItemKind,
    TimelineUniqueId,
};
use matrix_sdk_ui::Timeline;
use unicode_width::UnicodeWidthStr;

use matrix_sdk::ruma::{
    events::room::message::{FormattedBody, MessageFormat, MessageType},
    MilliSecondsSinceUnixEpoch,
    OwnedEventId,
    OwnedUserId,
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
use crate::message::state::{body_cow_membership, body_cow_profile, html_membership, html_profile};
use crate::preview::{ImageStatus, PreviewManager};
use crate::{
    base::RoomInfo,
    config::ApplicationSettings,
    message::html::{parse_matrix_html, StyleTree},
    util::{replace_emojis_in_str, space, space_span, take_width, wrapped_text},
};

mod compose;
mod html;
mod printer;
mod state;

pub use self::compose::text_to_message;
use self::state::{body_cow_state, html_state};
pub use html::TreeGenState;

type ProtocolPreview<'a> = (&'a Protocol, u16, u16);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageKey {
    offset: isize,
    id: TimelineUniqueId,
}

impl MessageKey {
    pub const fn new(offset: isize, id: TimelineUniqueId) -> Self {
        Self { offset, id }
    }
    pub fn id(&self) -> &TimelineUniqueId {
        &self.id
    }
    pub fn offset(&self) -> isize {
        self.offset
    }
}

impl Ord for MessageKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl PartialOrd for MessageKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct Messages {
    timeline: Timeline,
    messages: Vector<Arc<TimelineItem>>,
    htmls: HashMap<TimelineEventItemId, StyleTree>,

    /// This is the index of the first element in the vector when this object was created. This is
    /// used by [`MessageKey`] as a referenc because the vector can be extended in both directions.
    start_element: usize,
}

impl Messages {
    pub fn timeline(&self) -> &Timeline {
        &self.timeline
    }
    pub fn get_html(&self, id: &TimelineEventItemId) -> Option<&StyleTree> {
        self.htmls.get(id)
    }
    pub fn get_message(&self, key: &MessageKey) -> Option<&Message> {
        let index = self.start_element.checked_add_signed(key.offset())?;
        let item = self.messages.get(index)?;

        if item.unique_id() != key.id() {
            // TODO: search messages around?
            return None;
        }

        item.as_event()
    }
    pub fn first_key(&self) -> Option<MessageKey> {
        let id = self.messages.front()?.unique_id().to_owned();

        let offset = 0isize.checked_sub_unsigned(self.start_element)?;

        MessageKey::new(offset, id).into()
    }
    pub fn last_key(&self) -> Option<MessageKey> {
        let id = self.messages.back()?.unique_id().to_owned();

        let offset = self
            .messages
            .len()
            .cast_signed()
            .checked_sub_unsigned(self.start_element)?;

        MessageKey::new(offset, id).into()
    }
    pub fn last_message(&self) -> Option<&Message> {
        self.messages.iter().filter_map(|item| item.as_event()).next_back()
    }
    pub fn range(&self, range: impl RangeBounds<MessageKey>) -> MessagesRange {
        let mut next = match range.start_bound() {
            Bound::Included(start) => self.start_element.saturating_add_signed(start.offset()),
            Bound::Excluded(start) => self.start_element.saturating_add_signed(start.offset() + 1),
            Bound::Unbounded => 0,
        };
        let last = match range.end_bound() {
            Bound::Included(end) => self.start_element.saturating_add_signed(end.offset()).into(),
            Bound::Excluded(end) => {
                self.start_element.saturating_add_signed(end.offset()).checked_sub(1)
            },
            Bound::Unbounded => self.messages.len().checked_sub(1),
        };

        let last = match last {
            Some(last) => last,
            None => {
                next = 1;
                0
            },
        };

        MessagesRange { messages: self, next, last }
    }
    pub fn range_messages(
        &self,
        range: impl RangeBounds<MessageKey>,
    ) -> impl DoubleEndedIterator<Item = (MessageKey, &Message)> {
        self.range(range)
            .filter_map(|(key, item)| item.as_event().map(|event| (key, event)))
    }

    #[allow(unused)]
    fn new(_thread: ReceiptThread) -> Self {
        let timeline = todo!();

        let messages: Vector<Arc<TimelineItem>> = todo!();

        let htmls = messages
            .iter()
            .filter_map(|item| item.as_event())
            .filter_map(|item| generate_html(item).map(|html| (item.identifier(), html)))
            .collect();

        Self { messages, htmls, timeline, start_element: 0 }
    }

    pub fn main() -> Self {
        Self::new(ReceiptThread::Main)
    }

    pub fn thread(root: OwnedEventId) -> Self {
        Self::new(ReceiptThread::Thread(root))
    }
}

pub struct MessagesRange<'a> {
    messages: &'a Messages,
    next: usize,
    last: usize,
}

impl<'a> Iterator for MessagesRange<'a> {
    type Item = (MessageKey, &'a TimelineItem);

    fn next(&mut self) -> Option<Self::Item> {
        if self.next > self.last {
            return None;
        }

        let item = &self.messages.messages[self.next];

        let offset = self.next.cast_signed().checked_sub_unsigned(self.messages.start_element)?;
        let key = MessageKey::new(offset, item.unique_id().to_owned());

        self.next += 1;

        Some((key, &**item))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = (self.last + 1).saturating_sub(self.next);

        (size, Some(size))
    }
}

impl DoubleEndedIterator for MessagesRange<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.next > self.last {
            return None;
        }

        let item = &self.messages.messages[self.last];

        let offset = self.last.cast_signed().checked_sub_unsigned(self.messages.start_element)?;
        let key = MessageKey::new(offset, item.unique_id().to_owned());

        if self.last == 0 {
            self.next = 1;
        } else {
            self.last -= 1;
        }

        Some((key, &**item))
    }
}

impl ExactSizeIterator for MessagesRange<'_> {}

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

/// Hash an [`TimelineUniqueId`] into a [usize].
fn hash_event_id(event_id: &TimelineUniqueId) -> Option<usize> {
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
    let width = usize::min(*width, outer_width);
    if width < 2 || *height < 2 {
        return None;
    }
    let mut placeholder = "\u{230c}".to_string();
    placeholder.push_str(&" ".repeat(width - 2));
    placeholder.push('\u{230d}');
    placeholder.push_str(&"\n".repeat((height - 1) / 2));

    if *height > 2 {
        if let Some(text) = text {
            if text.width() <= width - 2 {
                placeholder.push(' ');
                placeholder.push_str(text);
            }
        }
    }

    placeholder.push_str(&"\n".repeat(height / 2));
    placeholder.push('\u{230e}');
    placeholder.push_str(&" ".repeat(width - 2));
    placeholder.push_str("\u{230f}\n");
    Some(placeholder)
}

#[derive(thiserror::Error, Debug)]
pub enum TimeStampIntError {
    #[error("Integer conversion error: {0}")]
    IntError(#[from] std::num::TryFromIntError),

    #[error("UInt conversion error: {0}")]
    UIntError(<UInt as TryFrom<u64>>::Error),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct MessageTimeStamp(UInt);

impl MessageTimeStamp {
    fn as_datetime(&self) -> DateTime<LocalTz> {
        let time = i64::from(self.0) / 1000;
        let time = DateTime::from_timestamp(time, 0).unwrap_or_default();
        time.into()
    }

    fn same_day(&self, other: &Self) -> bool {
        let dt1 = self.as_datetime();
        let dt2 = other.as_datetime();

        dt1.date_naive() == dt2.date_naive()
    }

    fn show_date(&self) -> Option<Span<'static>> {
        let time = self.as_datetime().format("%A, %B %d %Y").to_string();

        Span::styled(time, BOLD_STYLE).into()
    }

    fn show_time(&self) -> Span<'static> {
        let time = self.as_datetime().format("%T");
        let time = format!("  [{time}]");

        Span::raw(time)
    }
}

impl From<UInt> for MessageTimeStamp {
    fn from(millis: UInt) -> Self {
        Self(millis)
    }
}

impl From<MilliSecondsSinceUnixEpoch> for MessageTimeStamp {
    fn from(millis: MilliSecondsSinceUnixEpoch) -> Self {
        Self(millis.0)
    }
}

impl TryFrom<&MessageTimeStamp> for usize {
    type Error = TimeStampIntError;

    fn try_from(ts: &MessageTimeStamp) -> Result<Self, Self::Error> {
        let n = usize::try_from(u64::from(ts.0))?;

        Ok(n)
    }
}

impl TryFrom<usize> for MessageTimeStamp {
    type Error = TimeStampIntError;

    fn try_from(u: usize) -> Result<Self, Self::Error> {
        let n = u64::try_from(u)?;
        let n = UInt::try_from(n).map_err(TimeStampIntError::UIntError)?;

        Ok(Self::from(n))
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct MessageCursor {
    /// When `key` is None, the corner is determined by moving backwards from
    /// the most recently received message.
    pub key: Option<MessageKey>,

    /// A row within the [Text] representation of a [Message].
    pub text_row: usize,
}

impl MessageCursor {
    pub fn new(timestamp: MessageKey, text_row: usize) -> Self {
        MessageCursor { key: Some(timestamp), text_row }
    }

    /// Get a cursor that refers to the most recent message.
    pub fn latest() -> Self {
        MessageCursor::default()
    }

    pub fn to_key<'a>(&'a self, thread: &'a Messages) -> Option<MessageKey> {
        if let Some(key) = &self.key {
            Some(key.clone())
        } else {
            Some(thread.last_key()?)
        }
    }

    pub fn from_cursor(cursor: &Cursor, thread: &Messages) -> Option<Self> {
        let ev_hash = cursor.get_x();
        let ev_term = TimelineUniqueId(String::new());

        let offset_start = isize::MIN.wrapping_add_unsigned(cursor.get_y());
        let start = MessageKey::new(offset_start, ev_term);

        let iter = thread.range(&start..).alternate_with_all(thread.range(..&start).rev());
        for (key, _) in iter {
            if hash_event_id(key.id())? == ev_hash {
                return Self::from(key.to_owned()).into();
            }

            if key.offset() > offset_start {
                break;
            }
        }

        // If we can't find the cursor, then go to the nearest timestamp.
        thread.range(start..).next().map(|(key, _)| Self::from(key.clone()))
    }

    pub fn to_cursor(&self, thread: &Messages) -> Option<Cursor> {
        let key = self.to_key(thread)?;

        let y = key.offset().abs_diff(isize::MIN);
        let x = hash_event_id(key.id())?;

        Cursor::new(y, x).into()
    }
}

impl From<Option<MessageKey>> for MessageCursor {
    fn from(key: Option<MessageKey>) -> Self {
        MessageCursor { key, text_row: 0 }
    }
}

impl From<MessageKey> for MessageCursor {
    fn from(key: MessageKey) -> Self {
        MessageCursor { key: Some(key), text_row: 0 }
    }
}

impl Ord for MessageCursor {
    fn cmp(&self, other: &Self) -> Ordering {
        match (&self.key, &other.key) {
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

fn body_cow_msglike(content: &MsgLikeContent) -> Cow<'_, str> {
    match &content.kind {
        MsgLikeKind::Message(message) => body_cow_content(message.msgtype()),
        MsgLikeKind::Sticker(sticker) => Cow::Owned(format!("Alt: {}", sticker.content().body)),
        MsgLikeKind::Poll(_) => {
            // TODO: implement
            Cow::Borrowed("[Poll]")
        },
        MsgLikeKind::Redacted => Cow::Borrowed("[Redacted]"),
        MsgLikeKind::UnableToDecrypt(_) => Cow::Borrowed("[Unable to decrypt message]"),
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
        _ => msgtype.body(),
    };

    Cow::Borrowed(s)
}

#[allow(unused)]
fn generate_html(item: &EventTimelineItem) -> Option<StyleTree> {
    match item.content() {
        TimelineItemContent::MsgLike(content) => {
            if let MsgLikeKind::Message(message) = &content.kind {
                if let MessageType::Text(TextMessageEventContent {
                    formatted: Some(FormattedBody { format: MessageFormat::Html, body }),
                    ..
                }) = message.msgtype()
                {
                    Some(parse_matrix_html(body))
                } else {
                    None
                }
            } else {
                None
            }
        },
        TimelineItemContent::MembershipChange(change) => Some(html_membership(change)),
        TimelineItemContent::ProfileChange(change) => Some(html_profile(change)),
        TimelineItemContent::OtherState(change) => Some(html_state(change)),

        TimelineItemContent::FailedToParseMessageLike { event_type, error } => todo!(),
        TimelineItemContent::FailedToParseState { event_type, state_key, error } => todo!(),
        TimelineItemContent::CallInvite | TimelineItemContent::CallNotify => None,
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

pub struct MessageFormatter<'a> {
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

    /// The users who have read up to this message.
    read: Vec<OwnedUserId>,
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
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
        thread: &'a Messages,
    ) -> Option<ProtocolPreview<'a>> {
        let reply_style = if settings.tunables.message_user_color {
            style.patch(settings.get_user_color(msg.sender()))
        } else {
            style
        };

        let width = self.width();
        let w = width.saturating_sub(2);
        let (mut replied, proto) = msg.show_msg(w, reply_style, true, settings, previews, thread);
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
        reactions: &'a ReactionsByKeyBySender,
        style: Style,
        text: &mut Text<'a>,
    ) {
        let mut emojis = printer::TextPrinter::new(self.width(), style, false, self.settings);
        let mut n_pushed = 0;

        for (key, counts) in reactions.iter() {
            if n_pushed != 0 {
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
            emojis.push_span_nobreak(Span::styled(counts.len().to_string(), style));
            emojis.push_str("]", style);

            n_pushed += 1;
        }

        if n_pushed > 0 {
            self.push_text(emojis.finish(), style, text);
        }
    }

    fn push_thread_reply_count(&mut self, len: u32, text: &mut Text<'a>) {
        if len == 0 {
            return;
        }

        // If we have threaded replies to this message, show how many.
        let plural = len != 1;
        let style = Style::default();
        let mut threaded =
            printer::TextPrinter::new(self.width(), style, false, self.settings).literal(true);
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

pub type Message = EventTimelineItem;

impl MessageExt for Message {
    #[inline]
    fn item(&self) -> &EventTimelineItem {
        self
    }
}

pub trait MessageExt {
    fn item(&self) -> &EventTimelineItem;
    fn body(&self) -> Cow<'_, str> {
        #[allow(unused)]
        match self.item().content() {
            TimelineItemContent::MsgLike(content) => body_cow_msglike(content),
            TimelineItemContent::MembershipChange(change) => body_cow_membership(change),
            TimelineItemContent::ProfileChange(change) => body_cow_profile(change),
            TimelineItemContent::OtherState(change) => body_cow_state(change),
            TimelineItemContent::FailedToParseMessageLike { event_type, error } => todo!(),
            TimelineItemContent::FailedToParseState { event_type, state_key, error } => todo!(),
            TimelineItemContent::CallInvite | TimelineItemContent::CallNotify => {
                Cow::Borrowed("* started a call")
            },
        }
    }

    fn image_preview(&self) -> Option<&MediaSource> {
        if let MessageType::Image(c) = self.item().content().as_message()?.msgtype() {
            Some(&c.source)
        } else {
            None
        }
    }

    fn message_timestamp(&self) -> MessageTimeStamp {
        self.item().timestamp().into()
    }

    fn is_emote(&self) -> bool {
        self.item()
            .content()
            .as_message()
            .is_some_and(|msg| matches!(msg.msgtype(), MessageType::Emote(_)))
    }

    fn reply_to(&self) -> Option<OwnedEventId> {
        Some(self.item().content().as_msglike()?.in_reply_to.as_ref()?.event_id.clone())
    }

    fn thread_root(&self) -> Option<OwnedEventId> {
        self.item().content().as_msglike()?.thread_root.clone()
    }

    fn get_render_style(&self, selected: bool, settings: &ApplicationSettings) -> Style {
        let mut style = Style::default();

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED)
        }

        if self.item().is_local_echo() {
            style = style.add_modifier(StyleModifier::ITALIC);
        }

        if settings.tunables.message_user_color {
            let color = settings.get_user_color(self.item().sender());
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
            Some(prev) if prev.message_timestamp().same_day(&self.message_timestamp()) => None,
            _ => self.message_timestamp().show_date(),
        };
        let user_gutter = settings.tunables.user_gutter_width;

        if user_gutter + TIME_GUTTER + READ_GUTTER + MIN_MSG_LEN <= width &&
            settings.tunables.read_receipt_display
        {
            let cols = MessageColumns::Four;
            let fill = width - user_gutter - TIME_GUTTER - READ_GUTTER;
            let user = self.show_sender(prev, true, info, settings);
            let time = Some(self.message_timestamp().show_time());
            let read = self
                .item()
                .read_receipts()
                .iter()
                .filter(|(_, receipt)| !matches!(receipt.thread, ReceiptThread::Unthreaded))
                .map(|(user_id, _)| user_id.to_owned())
                .collect();

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if user_gutter + TIME_GUTTER + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Three;
            let fill = width - user_gutter - TIME_GUTTER;
            let user = self.show_sender(prev, true, info, settings);
            let time = Some(self.message_timestamp().show_time());
            let read = Vec::new();

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else if user_gutter + MIN_MSG_LEN <= width {
            let cols = MessageColumns::Two;
            let fill = width - user_gutter;
            let user = self.show_sender(prev, true, info, settings);
            let time = None;
            let read = Vec::new();

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        } else {
            let cols = MessageColumns::One;
            let fill = width.saturating_sub(2);
            let user = self.show_sender(prev, false, info, settings);
            let time = None;
            let read = Vec::new();

            MessageFormatter { settings, cols, orig, fill, user, date, time, read }
        }
    }

    /// Render the message as a [Text] object for the terminal.
    ///
    /// This will also get the image preview Protocol with an x/y offset.
    fn show_with_preview<'a>(
        &'a self,
        prev: Option<&Message>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
        thread: &'a Messages,
    ) -> (Text<'a>, [Option<ProtocolPreview<'a>>; 2]) {
        let width = vwctx.get_width();

        let style = self.get_render_style(selected, settings);
        let mut fmt = self.get_render_format(prev, width, info, settings);
        let mut text = Text::default();
        let width = fmt.width();

        // Show the message that this one replied to, if any.
        // TODO: use `InReplyToDetails::event`
        let reply = self
            .reply_to()
            .or_else(|| self.thread_root())
            .and_then(|e| info.get_event(&e));
        let proto_reply = reply.as_ref().and_then(|r| {
            // Format the reply header, push it into the `Text` buffer, and get any image.
            fmt.push_in_reply(r, style, &mut text, info, settings, previews, thread)
        });

        // Now show the message contents, and the inlined reply if we couldn't find it above.
        let (msg, proto) = self.show_msg(width, style, reply.is_some(), settings, previews, thread);

        // Given our text so far, determine the image offset.
        let proto_main = proto.map(|p| {
            let y_off = text.lines.len() as u16;
            let x_off = fmt.cols.user_gutter_width(settings);
            // Adjust y_off by 1 if a date was printed before the message to account for
            // the extra line we're going to print.
            let y_off = if fmt.date.is_some() { y_off + 1 } else { y_off };
            (p, x_off, y_off)
        });

        fmt.push_text(msg, style, &mut text);

        if text.lines.is_empty() {
            // If there was nothing in the body, just show an empty message.
            fmt.push_spans(space_span(width, style).into(), style, &mut text);
        }

        if settings.tunables.reaction_display {
            if let Some(msg) = self.item().content().as_msglike() {
                fmt.push_reactions(&msg.reactions, style, &mut text);
            }
        }

        if let Some(thread) = self
            .item()
            .content()
            .as_msglike()
            .and_then(|msg| msg.thread_summary.as_ref())
        {
            fmt.push_thread_reply_count(thread.num_replies, &mut text);
        }

        (text, [proto_main, proto_reply])
    }

    fn show_msg<'a>(
        &'a self,
        width: usize,
        style: Style,
        hide_reply: bool,
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
        thread: &'a Messages,
    ) -> (Text<'a>, Option<&'a Protocol>) {
        if let Some(html) = thread.get_html(&self.item().identifier()) {
            (html.to_text(width, style, hide_reply, settings), None)
        } else {
            let mut msg = self.body();
            if settings.tunables.message_shortcode_display {
                msg = Cow::Owned(replace_emojis_in_str(msg.as_ref()));
            }

            let mut proto = None;
            let placeholder = match self.image_preview().and_then(|source| previews.get(source)) {
                None => None,
                Some(ImageStatus::Queued(image_preview_size)) => {
                    placeholder_frame(Some("Queued..."), width, image_preview_size)
                },
                Some(ImageStatus::Downloading(image_preview_size)) => {
                    placeholder_frame(Some("Downloading..."), width, image_preview_size)
                },
                Some(ImageStatus::Loaded(backend)) => {
                    proto = Some(backend);
                    placeholder_frame(Some("No Space..."), width, &backend.area().into())
                },
                Some(ImageStatus::Error(err)) => Some(format!("[Image error: {err}]\n")),
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
        settings.get_user_span(self.item().sender(), info)
    }

    fn show_sender<'a>(
        &'a self,
        prev: Option<&Message>,
        align_right: bool,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
    ) -> Option<Span<'a>> {
        if let Some(prev) = prev {
            if self.item().sender() == prev.sender() &&
                self.message_timestamp().same_day(&prev.message_timestamp()) &&
                !self.is_emote()
            {
                return None;
            }
        }

        let Span { content, style } = self.sender_span(info, settings);
        let user_gutter = settings.tunables.user_gutter_width;
        let ((truncated, width), _) = take_width(content, user_gutter - 2);
        let padding = user_gutter - 2 - width;

        let sender = if align_right {
            format!("{}{}  ", space(padding), truncated)
        } else {
            format!("{}{}  ", truncated, space(padding))
        };

        Span::styled(sender, style).into()
    }
}

impl TimelineItemExt for TimelineItem {
    fn item(&self) -> &TimelineItem {
        self
    }
}

pub trait TimelineItemExt {
    fn item(&self) -> &TimelineItem;

    /// Render the message as a [Text] object for the terminal.
    ///
    /// This will also get the image preview Protocol with an x/y offset.
    fn show_with_preview<'a>(
        &'a self,
        prev: Option<&Message>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
        thread: &'a Messages,
    ) -> (Text<'a>, [Option<ProtocolPreview<'a>>; 2]) {
        match self.item().kind() {
            TimelineItemKind::Event(item) => {
                item.show_with_preview(prev, selected, vwctx, info, settings, previews, thread)
            },
            TimelineItemKind::Virtual(_item) => todo!(),
        }
    }
    fn show<'a>(
        &'a self,
        prev: Option<&Message>,
        selected: bool,
        vwctx: &ViewportContext<MessageCursor>,
        info: &'a RoomInfo,
        settings: &'a ApplicationSettings,
        previews: &'a PreviewManager,
        thread: &'a Messages,
    ) -> Text<'a> {
        self.show_with_preview(prev, selected, vwctx, info, settings, previews, thread)
            .0
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
        assert_eq!(k1, MSG1_KEY.clone());
        assert_eq!(k2, MSG2_KEY.clone());
        assert_eq!(k3, MSG3_KEY.clone());
        assert_eq!(k4, MSG4_KEY.clone());
        assert_eq!(k5, MSG5_KEY.clone());

        // MessageCursor::latest() turns into the largest key (our local echo message).
        assert_eq!(k6, MSG1_KEY.clone());

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
            placeholder_frame(None, 4, &ImagePreviewSize { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌  ⌍


⌎  ⌏
"#
            )
        );

        assert_eq!(
            placeholder_frame(None, 2, &ImagePreviewSize { width: 4, height: 4 }),
            pretty_frame_test(
                r#"
⌌⌍


⌎⌏
"#
            )
        );
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
            placeholder_frame(Some("OK"), 6, &ImagePreviewSize { width: 6, height: 6 }),
            pretty_frame_test(
                r#"
⌌    ⌍

 OK


⌎    ⌏
"#
            )
        );
        assert_eq!(
            placeholder_frame(Some("OK"), 6, &ImagePreviewSize { width: 6, height: 7 }),
            pretty_frame_test(
                r#"
⌌    ⌍


 OK


⌎    ⌏
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
            body_cow_content(&MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::default()))
            )),
            "[Attached Image: Alt text]".to_string()
        );

        let mut info = ImageInfo::default();
        info.size = Some(442630_u32.into());
        assert_eq!(
            body_cow_content(&MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Image: Alt text (442.63 kB)]".to_string()
        );

        let mut info = ImageInfo::default();
        info.size = Some(12_u32.into());
        assert_eq!(
            body_cow_content(&MessageType::Image(
                ImageMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Image: Alt text (12 B)]".to_string()
        );

        let mut info = AudioInfo::default();
        info.size = Some(4294967295_u32.into());
        assert_eq!(
            body_cow_content(&MessageType::Audio(
                AudioMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Audio: Alt text (4.29 GB)]".to_string()
        );

        let mut info = FileInfo::default();
        info.size = Some(4426300_u32.into());
        assert_eq!(
            body_cow_content(&MessageType::File(
                FileMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached File: Alt text (4.43 MB)]".to_string()
        );

        let mut info = VideoInfo::default();
        info.size = Some(44000_u32.into());
        assert_eq!(
            body_cow_content(&MessageType::Video(
                VideoMessageEventContent::plain(
                    "Alt text".to_string(),
                    "mxc://matrix.org/jDErsDugkNlfavzLTjJNUKAH".into()
                )
                .info(Some(Box::new(info)))
            )),
            "[Attached Video: Alt text (44 kB)]".to_string()
        );
    }
}
