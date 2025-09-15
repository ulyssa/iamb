use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use matrix_sdk::room::Room as MatrixRoom;
use matrix_sdk::ruma::{EventId, OwnedEventId, OwnedRoomId, RoomId};
use modalkit::actions::{Action, WindowAction};
use modalkit::editing::completion::CompletionList;
use modalkit::editing::context::EditContext;
use modalkit::editing::rope::EditRope;
use modalkit::errors::UIError;
use modalkit::prelude::{CloseFlags, EditInfo, OpenTarget, WordStyle, WriteFlags};
use modalkit_ratatui::textbox::TextBoxState;
use modalkit_ratatui::{TermOffset, TerminalCursor, WindowOps};
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{StatefulWidget, Widget};
use ratatui_image::{protocol::Protocol, Image};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;
use url::Url;

use crate::base::{
    IambBufferId,
    IambError,
    IambId,
    IambInfo,
    IambResult,
    MessageAction,
    ProgramContext,
    ProgramStore,
    RoomFocus,
    RoomInfo,
    RoomView,
};
use crate::config::{TunableValues, UserDisplayStyle};
use crate::message::{millis_to_datetime, Message, MessageTimeStamp};
use crate::util::space;
use crate::windows::room::chat;

type ImagePreview = (Arc<Protocol>, u16);

fn user_date_line(
    msg: &Message,
    width: usize,
    info: &RoomInfo,
    tunables: &TunableValues,
) -> Line<'static> {
    let user_id = msg.sender.as_ref();
    let Span { content: user, style: user_style } = tunables.get_user_span(user_id, info);
    let mut user = user.to_string();
    if let UserDisplayStyle::Username = tunables.username_display {
    } else {
        user.push_str(&format!(" ({})", user_id.as_str()));
    }
    user.push(' ');

    let mut date = if let MessageTimeStamp::OriginServer(ms) = msg.timestamp {
        millis_to_datetime(ms).format("%T %A, %B %d %Y").to_string()
    } else {
        String::new()
    };

    // truncate if needed
    if user.len() > width {
        date.clear();
        std::mem::drop(user.drain(width.saturating_sub(2)..));
        if width >= 2 {
            user.push_str("..");
        }
    } else if user.len() + date.len() >= width {
        let date_width = width - user.len();
        std::mem::drop(date.drain(..=date.len().saturating_sub(date_width) + 2));
        if date_width >= 2 {
            date.insert_str(0, "..");
        }
    }

    let padding = width - user.len() - date.len();

    Span::styled(user, user_style) +
        Span::raw(space(padding)) +
        Span::styled(date, Style::new().add_modifier(Modifier::BOLD))
}

/// State needed for rendering [`MessageWidget`].
pub struct MessageState {
    room_id: OwnedRoomId,
    room: MatrixRoom,

    message_id: OwnedEventId,

    tbox: TextBoxState<IambInfo>,
}

impl MessageState {
    pub fn new(store: &mut ProgramStore, room: MatrixRoom, message_id: OwnedEventId) -> Self {
        let room_id = room.room_id().to_owned();

        let buf = store.buffers.load(IambBufferId::Room(
            room_id.clone(),
            RoomView::Message(message_id.to_owned()),
            RoomFocus::Scrollback,
        ));
        let mut tbox = TextBoxState::new(buf);
        tbox.set_readonly(true);
        tbox.set_wrap(false);

        Self { room_id, room, message_id, tbox }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        if let Some(room) = store.application.worker.client.get_room(self.room_id()) {
            self.room = room;
        }
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, EditContext)>> {
        let worker = &store.application.worker;

        let settings = &store.application.settings;
        let info = store.application.rooms.get_or_default(self.room_id.clone());

        let msg = info.get_event(self.id()).ok_or(IambError::NoSelectedMessage)?;

        match act {
            MessageAction::Download(filename, flags) => {
                let msg = info.get_event_mut(self.id()).unwrap();
                chat::msg_download(
                    ctx,
                    &store.application.worker.client,
                    msg,
                    settings,
                    filename,
                    flags,
                )
                .await
            },
            MessageAction::React(reaction, literal) => {
                chat::msg_react(msg, settings, info, worker, &self.room_id, reaction, literal).await
            },
            MessageAction::Redact(reason, skip_confirm) => {
                chat::msg_redact(msg, worker, &self.room_id, reason, skip_confirm).await
            },
            MessageAction::Unreact(reaction, literal) => {
                chat::msg_unreact(msg, settings, info, worker, &self.room_id, reaction, literal)
                    .await
            },
            MessageAction::Replied => {
                let Some(reply) = msg.reply_to() else {
                    let msg = "Selected message is not a reply";
                    return Err(UIError::Failure(msg.into()));
                };
                let act = Action::Window(WindowAction::Switch(OpenTarget::Application(
                    IambId::Room(self.room_id.clone(), RoomView::Message(reply)),
                )));

                Ok(vec![(act, ctx)])
            },
            MessageAction::Edit | MessageAction::Reply => {
                let msg = "Cannot write message in this view.";
                let err = UIError::Failure(msg.into());

                Err(err)
            },
            _ => Ok(vec![]),
        }
    }

    pub fn room(&self) -> &MatrixRoom {
        &self.room
    }

    pub fn id(&self) -> &EventId {
        &self.message_id
    }

    pub fn room_id(&self) -> &RoomId {
        &self.room_id
    }

    fn render<'b>(
        &self,
        widget: &'b mut MessageWidget<'_>,
        width: usize,
    ) -> Vec<(Line<'b>, Option<ImagePreview>)> {
        let info = widget.store.application.rooms.get_or_default(self.room_id.clone());
        let settings = &widget.store.application.settings;

        let bold = Style::new().add_modifier(Modifier::BOLD);

        let Some(msg) = info.get_event(&self.message_id) else {
            widget
                .store
                .application
                .need_load
                .need_event(self.room_id.clone(), self.message_id.clone());

            return vec![
                (Line::raw(""), None),
                (Line::styled("  Loading...", bold), None),
            ];
        };

        let mut lines = vec![];

        // header
        lines.push((user_date_line(msg, width, info, &settings.tunables), None));

        // message
        let (txt, [mut msg_preview, mut reply_preview]) =
            msg.show_with_preview(Some(msg), false, width, info, &widget.message_tunables);

        for (row, mut line) in txt.lines.into_iter().enumerate() {
            // Only take the previews into the matching row number.
            // `reply` and `msg` previews are on rows,
            // so an `or` works to pick the one that matches (if any)
            let line_preview = match msg_preview {
                Some((_, _, y)) if y as usize == row => msg_preview.take(),
                _ => None,
            }
            .or(match reply_preview {
                Some((_, _, y)) if y as usize == row => reply_preview.take(),
                _ => None,
            })
            .map(|(backend, x, _)| (backend, x));

            // remove trailing whitespace from printer
            if let Some(last) = line.spans.last() {
                if last.content.trim().is_empty() {
                    line.spans.remove(line.spans.len() - 1);
                }
            }

            lines.push((line, line_preview));
        }

        // mentions
        if let Some(mentions) = msg.event.mentions() {
            if mentions.room || !mentions.user_ids.is_empty() {
                lines.push((Line::raw(""), None));
                lines.push((Line::styled("Mentions:", bold), None));
                if mentions.room {
                    lines.push((Span::raw("- ") + Span::styled("@room", bold), None));
                }
                for user_id in &mentions.user_ids {
                    let user = settings.tunables.get_user_span(user_id, info);
                    lines.push((Span::raw("- ") + user, None));
                }
            }
        }

        // links
        let links = if let Some(html) = &msg.html {
            html.get_links()
        } else if let Ok(url) = Url::parse(&msg.event.body()) {
            vec![('0', url)]
        } else {
            vec![]
        };

        if !links.is_empty() {
            lines.push((Line::raw(""), None));
            lines.push((Line::styled("Links:", bold), None));

            for (c, url) in links {
                lines.push((Line::raw(format!("[{c}] {url}")), None));
            }
        }

        // reactions
        if settings.tunables.reaction_display {
            for (key, users) in info.get_reactions(&self.message_id) {
                let short = emojis::get(key).and_then(|emoji| emoji.shortcode()).or(
                    if key.chars().all(|c| c.is_ascii_alphanumeric()) {
                        Some(key)
                    } else {
                        None
                    },
                );

                let (text, desc) = if settings.tunables.reaction_shortcode_display {
                    if let Some(short) = short {
                        (short, None)
                    } else {
                        (key, None)
                    }
                } else {
                    (key, short)
                };

                let content = if let Some(desc) = desc {
                    format!("[{text} {}] ({desc})", users.len())
                } else {
                    format!("[{text} {}]", users.len())
                };

                lines.push((Line::raw(""), None));
                lines.push((Line::raw(content), None));

                for id in users {
                    let user = settings.tunables.get_user_span(id, info);
                    lines.push((Span::raw("- ") + user, None));
                }
            }
        }

        // read receipts
        if settings.tunables.read_receipt_display {
            lines.push((Line::raw(""), None));
            lines.push((Line::styled("Last message seen by:", bold), None));
            for user in info
                .event_receipts
                .values()
                .filter_map(|receipts| receipts.get(msg.event.event_id()))
                .flat_map(|read| read.iter())
            {
                let user = settings.tunables.get_user_span(user, info);
                lines.push((Span::raw("- ") + user, None))
            }
        }

        lines
    }
}

impl Deref for MessageState {
    type Target = TextBoxState<IambInfo>;

    fn deref(&self) -> &Self::Target {
        &self.tbox
    }
}
impl DerefMut for MessageState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tbox
    }
}

impl TerminalCursor for MessageState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.tbox.get_term_cursor()
    }
}

impl WindowOps<IambInfo> for MessageState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        self.tbox.draw(area, buf, focused, store)
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        Self {
            room_id: self.room_id.clone(),
            room: self.room.clone(),
            message_id: self.message_id.clone(),
            tbox: self.tbox.dup(store),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        self.tbox.close(flags, store)
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        self.tbox.write(path, flags, store)
    }

    fn get_completions(&self) -> Option<CompletionList> {
        self.tbox.get_completions()
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        self.tbox.get_cursor_word(style)
    }

    fn get_selected_word(&self) -> Option<String> {
        self.tbox.get_selected_word()
    }
}

pub struct MessageWidget<'a> {
    store: &'a mut ProgramStore,
    message_tunables: TunableValues,
    focused: bool,
}

impl<'a> MessageWidget<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Self {
        let mut message_tunables = store.application.settings.tunables.clone();
        message_tunables.user_gutter_width = 2;
        message_tunables.read_receipt_display = false;
        message_tunables.message_time_display = false;
        message_tunables.message_user_color = false;
        message_tunables.reaction_display = false;

        Self { store, message_tunables, focused: false }
    }

    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl<'a> StatefulWidget for MessageWidget<'a> {
    type State = MessageState;

    fn render(mut self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let lines = state.render(&mut self, area.width as usize);

        // update text
        let buffer = state.tbox.buffer();
        let mut locked = buffer.write().unwrap();

        let mut text = EditRope::default();
        for (line, _) in lines.iter() {
            for span in line {
                text += span.content.as_ref().into();
            }
            text += '\n'.into();
        }
        locked.text = text;

        std::mem::drop(locked);

        // only store style info
        let lines: Vec<_> = lines
            .into_iter()
            .map(|(line, preview)| {
                let styles: Vec<(_, u16)> = line
                    .iter()
                    .map(|span| {
                        let width = UnicodeSegmentation::graphemes(span.content.as_ref(), true)
                            .filter(|symbol| !symbol.contains(|char: char| char.is_control()))
                            .map(|symbol| symbol.width() as u16)
                            .sum();
                        (span.style, width)
                    })
                    .collect();
                (line.style, styles, preview)
            })
            .collect();

        // draw text
        state.draw(area, buf, self.focused, self.store);

        // set highlighting
        let mut image_previews = vec![];

        let mut draw_lines = lines.into_iter().fuse().skip(state.tbox.viewctx.corner.y);
        let draw_area = area.intersection(buf.area);
        for y in draw_area.top()..draw_area.top() + draw_area.height {
            let mut x = draw_area.left();
            if let Some((line_style, styles, line_preview)) = draw_lines.next() {
                if let Some((backend, msg_x)) = line_preview {
                    image_previews.push((x + msg_x, y, backend));
                }
                for (style, width) in styles {
                    let remaining_width = draw_area.right().saturating_sub(x);

                    for i in 0..remaining_width.min(width) {
                        let old_style = buf[(x + i, y)].style();
                        let new_style = old_style.patch(line_style).patch(style);
                        buf[(x + i, y)].set_style(new_style);
                    }
                    x += width;
                }
            }
        }

        // Render image previews after all text lines have been drawn, as the render might draw below the current
        // line.
        for (x, y, backend) in image_previews {
            let image_widget = Image::new(&backend);
            let mut rect = backend.area();
            rect.x = x;
            rect.y = y;
            // Don't render outside of scrollback area
            if rect.bottom() <= area.bottom() && rect.right() <= area.right() {
                image_widget.render(rect, buf);
            }
        }
    }
}
