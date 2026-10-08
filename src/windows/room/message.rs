use modalkit::editing::context::EditContext;
use modalkit_ratatui::list::{List, ListCursor, ListItem, ListState};
use ratatui_image::sliced::{SignedPosition, SlicedImage};

use crate::prelude::*;
use crate::preview::ImageStatus;
use crate::util::space;
use crate::windows::room::chat;
use crate::windows::selected_style;

/// State needed for rendering [MessageWidget].
pub(crate) struct MessageState {
    room: MatrixRoom,
    message_id: MessageId,
    list: ListState<MessageInfoItem, IambInfo>,
}

impl MessageState {
    pub fn new(room: MatrixRoom, message_id: MessageId) -> Self {
        let content = IambBufferId::Room(
            room.room_id().to_owned(),
            RoomView::Message(message_id.clone()),
            RoomFocus::Scrollback,
        );
        let list = ListState::new(content, vec![]);

        MessageState { room, list, message_id }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        if let Some(room) = store.application.worker.client.get_room(self.room_id()) {
            self.room = room;
        }
    }

    pub fn room(&self) -> &MatrixRoom {
        &self.room
    }

    pub fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    pub fn id(&self) -> MessageId {
        self.message_id.clone()
    }

    pub fn dup(&self, store: &mut ProgramStore) -> Self {
        MessageState {
            room: self.room.clone(),
            list: self.list.dup(store),
            message_id: self.message_id.clone(),
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
        let info = store.application.rooms.get_or_default(self.room_id().to_owned());

        let msg = info.get_message(&self.id()).ok_or(IambError::NoSelectedMessage)?;

        match act {
            MessageAction::Download(filename, flags) => {
                let msg = info.get_message_mut(&self.id()).unwrap();
                chat::msg_download(
                    ctx,
                    &store.application.worker.client,
                    msg,
                    settings,
                    filename,
                    flags,
                    &store.application.settings.tunables,
                )
                .await
            },
            MessageAction::React(reaction, literal) => {
                chat::msg_react(msg, settings, info, worker, self.room_id(), reaction, literal)
                    .await
            },
            MessageAction::Redact(reason, skip_confirm) => {
                chat::msg_redact(msg, worker, self.room_id(), reason, skip_confirm).await
            },
            MessageAction::Unreact(reaction, literal) => {
                chat::msg_unreact(msg, settings, info, worker, self.room_id(), reaction, literal)
                    .await
            },
            MessageAction::Pin => chat::msg_pin(true, msg, self.room_id(), worker, settings).await,
            MessageAction::Unpin => {
                chat::msg_pin(false, msg, self.room_id(), worker, settings).await
            },
            MessageAction::Replied => {
                let Some(reply) = msg.reply_to() else {
                    let msg = "Selected message is not a reply";
                    return Err(UIError::Failure(msg.into()));
                };
                let act = Action::Window(WindowAction::Switch(OpenTarget::Application(
                    IambId::Room(self.room_id().to_owned().into(), RoomView::Message(reply.into())),
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
}

impl TerminalCursor for MessageState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.list.get_term_cursor()
    }

    fn hide_term_cursor(&self) -> bool {
        self.list.hide_term_cursor()
    }
}

impl Deref for MessageState {
    type Target = ListState<MessageInfoItem, IambInfo>;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

impl DerefMut for MessageState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.list
    }
}

pub(crate) struct MessageWidget<'a> {
    focused: bool,
    store: &'a mut ProgramStore,
}

/// Used to temprarily store some settings that are changed for message rendering.
struct SavedTunables {
    user_gutter_width: usize,
    read_receipt_display: bool,
    message_user_color: bool,
    reaction_display: bool,
    time_format: String,
    time_gutter_width: usize,
}

impl SavedTunables {
    fn setup(tunables: &mut TunableValues) -> Self {
        use std::mem::replace;

        let user_gutter_width = replace(&mut tunables.user_gutter_width, 2);
        let read_receipt_display = replace(&mut tunables.read_receipt_display, false);
        let message_user_color = replace(&mut tunables.message_user_color, false);
        let reaction_display = replace(&mut tunables.reaction_display, false);
        let time_format = replace(&mut tunables.time_format, "".into());
        let time_gutter_width = replace(&mut tunables.time_gutter_width, 0);

        Self {
            user_gutter_width,
            read_receipt_display,
            message_user_color,
            reaction_display,
            time_format,
            time_gutter_width,
        }
    }

    fn restore(self, tunables: &mut TunableValues) {
        let Self {
            user_gutter_width,
            read_receipt_display,
            message_user_color,
            reaction_display,
            time_format,
            time_gutter_width,
        } = self;

        tunables.user_gutter_width = user_gutter_width;
        tunables.read_receipt_display = read_receipt_display;
        tunables.message_user_color = message_user_color;
        tunables.reaction_display = reaction_display;
        tunables.time_format = time_format;
        tunables.time_gutter_width = time_gutter_width;
    }
}

impl<'a> MessageWidget<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Self {
        MessageWidget { focused: false, store }
    }

    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl StatefulWidget for MessageWidget<'_> {
    type State = MessageState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        let ChatStore { rooms, worker, settings, previews, .. } = &mut self.store.application;
        let style = settings.theme.rooms.default;

        let saved_tunables = SavedTunables::setup(&mut settings.tunables);

        let info = rooms.get_or_default(state.room_id().to_owned());
        let Some(msg) = info.get_message(&state.message_id) else {
            match &state.message_id {
                MessageId::Origin(event_id) => {
                    self.store
                        .application
                        .need_load
                        .need_message(state.room_id().to_owned(), event_id.to_owned());

                    Paragraph::new(Span::styled("Loading...", style))
                        .alignment(Alignment::Center)
                        .render(area, buffer);
                    return;
                },
                MessageId::Local(_) => {
                    Paragraph::new(Span::styled(
                        "Local echo not found.",
                        settings.theme.cmdbar.error,
                    ))
                    .alignment(Alignment::Center)
                    .render(area, buffer);
                    return;
                },
            }
        };

        // load image previews
        {
            if let Some(source) = msg.image_preview() {
                previews.load(source, PreviewKind::Message, worker);
            }
            let reply = msg
                .reply_to()
                .or_else(|| msg.thread_root())
                .and_then(|e| info.get_event(&e))
                .and_then(|msg| msg.image_preview());
            if let Some(source) = reply {
                previews.load(source, PreviewKind::Message, worker);
            }
            let reactions =
                msg.event.event_id().and_then(|event_id| info.reactions.get(event_id)).map(
                    |reactions| reactions.iter().filter_map(|(_, (_, _, source))| source.as_ref()),
                );
            if let Some(reactions) = reactions {
                for source in reactions {
                    previews.load(source, PreviewKind::Reaction, worker);
                }
            }
        }

        let (_, msg_previews) = msg.show_with_preview(
            // use self as prev to suppress user and date preview
            Some(msg),
            false,
            area.width as usize,
            info,
            settings,
            previews,
        );

        // message
        let mut items = vec![
            MessageInfoItem::Header(state.room_id().to_owned(), msg.sender.clone(), msg.timestamp),
            MessageInfoItem::Message(state.room_id().to_owned(), state.message_id.clone()),
        ];

        // mentions
        if let Some(mentions) = msg.event.mentions() {
            items.extend(mentions.user_ids.iter().map(|user_id| {
                MessageInfoItem::User(
                    "Mentions:".into(),
                    state.room_id().to_owned(),
                    user_id.to_owned(),
                )
            }));
        }

        // links
        items.extend(msg.find_links().into_iter().map(|(c, url)| MessageInfoItem::Link(c, url)));

        // reactions
        // this uses the item index
        let mut reaction_previews = HashMap::new();
        if saved_tunables.reaction_display &&
            let Some(event_id) = state.message_id.as_origin()
        {
            for (key, users, source) in info.get_reactions(event_id) {
                if users.is_empty() {
                    continue;
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

                let short = emojis::get(key).and_then(|emoji| emoji.shortcode());
                let (text, desc) = if proto.is_some() {
                    ("  ", Some(key))
                } else if settings.tunables.reaction_shortcode_display {
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

                if let Some(proto) = proto.flatten() {
                    let proto = proto.clone();
                    reaction_previews.insert(items.len(), proto);
                }

                let section = Cow::from(content);
                items.extend(users.into_iter().map(|user_id| {
                    MessageInfoItem::User(
                        section.clone(),
                        state.room_id().to_owned(),
                        user_id.to_owned(),
                    )
                }));
            }
        }

        // receipts
        if saved_tunables.read_receipt_display &&
            let Some(event_id) = msg.event.event_id()
        {
            let receipts = info
                .event_receipts
                .values()
                .filter_map(|receipts| receipts.get(event_id))
                .flat_map(|receipts| receipts.iter())
                .map(|user_id| {
                    MessageInfoItem::User(
                        "Last seen by:".into(),
                        state.room_id().to_owned(),
                        user_id.to_owned(),
                    )
                });
            items.extend(receipts);
        }

        state.list.set(items);
        state.set_ignorecase(settings.tunables.ignorecase);

        List::new(self.store)
            .empty_message("This space is empty")
            .empty_alignment(Alignment::Center)
            .focus(self.focused)
            .style(style)
            .render(area, buffer, &mut state.list);

        // render image previews

        let viewctx = state.last_viewctx();

        let mut y = 0;
        let mut prev_section = None;
        let mut scrolled_lines = 0;
        let mut previews = msg_previews;

        for (i, item) in state.get_items().iter().enumerate() {
            if let Some(proto) = reaction_previews.remove(&i) {
                previews.push((proto, 1, y));
            }

            let curr_section = item.get_section();
            if curr_section != prev_section {
                y += 1;
                prev_section = curr_section;
            }

            if i == viewctx.corner.position {
                scrolled_lines = y as usize + viewctx.corner.text_row;
            }

            y += item.show(false, viewctx, self.store).lines.len() as u16;
        }

        for (backend, msg_x, msg_y) in previews {
            let x = msg_x + area.left();
            let y = msg_y as i16 - scrolled_lines as i16 + area.top() as i16;
            if backend.size().height as i16 + y >= area.top() as i16 {
                let hidden_lines = (area.top() as i16 - y).max(0);

                let position = SignedPosition { x: 0, y: -hidden_lines };
                let image_widget = SlicedImage::new(&backend, position);
                let mut rect: Rect = backend.size().into();
                rect.x = x;
                rect.y = (y + hidden_lines) as u16;

                rect.height -= hidden_lines as u16;

                let rect = rect.intersection(area);
                if !rect.is_empty() {
                    image_widget.render(rect, buffer);
                }
            }
        }

        saved_tunables.restore(&mut self.store.application.settings.tunables);
    }
}

/// The different parts of the `:message` window
#[derive(Debug, Clone)]
pub(crate) enum MessageInfoItem {
    /// The message header
    Header(OwnedRoomId, OwnedUserId, MessageTimeStamp),

    /// The message itself
    Message(OwnedRoomId, MessageId),

    User(Cow<'static, str>, OwnedRoomId, OwnedUserId),
    Link(char, Url),
}

impl Display for MessageInfoItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MessageInfoItem::Message(_, message_id) => {
                // XXX: This should return the message text for yanking (or modify modalkit to not
                // use `ToString` here)
                write!(f, "{message_id}")
            },
            MessageInfoItem::Header(_, user_id, _) | MessageInfoItem::User(_, _, user_id) => {
                write!(f, "{user_id}")
            },
            MessageInfoItem::Link(_, url) => write!(f, "{url}"),
        }
    }
}

impl ListItem<IambInfo> for MessageInfoItem {
    type Section = Cow<'static, str>;

    fn show<'a>(
        &'a self,
        selected: bool,
        viewctx: &ViewportContext<ListCursor>,
        store: &'a ProgramStore,
    ) -> Text<'a> {
        let style = store.application.settings.theme.timeline.default;
        let style = selected_style(selected, style);
        match self {
            MessageInfoItem::Header(room_id, user_id, timestamp) => {
                let info = store.application.rooms.get(room_id);
                let settings = &store.application.settings;
                let width = viewctx.get_width();

                let Span { content: mut user, style: user_style } =
                    settings.get_user_span_maybe(user_id, info);

                let mut date = timestamp.as_datetime().format("%T %A, %B %d %Y").to_string();

                // truncate if needed
                if user.len() > width {
                    date.clear();
                    let user = user.to_mut();
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
                let date_style = store.application.settings.theme.timeline.date;

                let line = Span::styled(user, style.patch(user_style)) +
                    Span::raw(space(padding)) +
                    Span::styled(date, date_style);
                line.into()
            },
            MessageInfoItem::Message(room_id, message_id) => {
                let Some(info) = store.application.rooms.get(room_id) else {
                    return Span::styled("Loading room...", style.fg(Color::Gray)).into();
                };
                let Some(msg) = info.get_message(message_id) else {
                    return Span::styled("Loading message...", style.fg(Color::Gray)).into();
                };

                msg.show(
                    // use self as prev to suppress user and date preview
                    Some(msg),
                    selected,
                    viewctx.get_width(),
                    info,
                    &store.application.settings,
                    &store.application.previews,
                )
            },
            // XXX: show reaction time
            MessageInfoItem::User(_, room_id, user_id) => {
                let info = store.application.rooms.get(room_id);
                let user = store.application.settings.get_user_span_maybe(user_id, info);
                let user_span = Span::styled(user.content.into_owned(), style.patch(user.style));

                Text::from(Span::raw("- ") + user_span)
            },
            MessageInfoItem::Link(c, url) => {
                Text::from(Span::styled(format!("[{c}] {url}"), style.bold()))
            },
        }
    }

    fn get_word(&self) -> Option<String> {
        match self {
            MessageInfoItem::Message(..) => {
                // XXX: This could return a link to the replied-to item
                None
            },
            MessageInfoItem::Header(_, user_id, _) | MessageInfoItem::User(.., user_id) => {
                Some(user_id.to_string())
            },
            MessageInfoItem::Link(.., url) => Some(url.to_string()),
        }
    }

    fn get_section(&self) -> Option<&Self::Section> {
        match self {
            MessageInfoItem::Header(..) | MessageInfoItem::Message(..) => None,
            MessageInfoItem::User(section, ..) => Some(section),
            MessageInfoItem::Link(..) => Some(&Cow::Borrowed("Links:")),
        }
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for MessageInfoItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        match act {
            PromptAction::Submit => {
                match self {
                    // XXX: Add action for [un-]react
                    MessageInfoItem::Message(room_id, MessageId::Origin(event_id)) => {
                        let id = IambId::Room(
                            room_id.to_owned().into(),
                            RoomView::Thread(event_id.to_owned()),
                        );
                        let open = WindowAction::Switch(OpenTarget::Application(id));
                        Ok(vec![(open.into(), ctx.clone())])
                    },
                    MessageInfoItem::Message(_, MessageId::Local(_)) => {
                        let msg = "Cannot create thread for local echo.";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },
                    MessageInfoItem::Header(..) | MessageInfoItem::User(..) => {
                        // XXX: This should link to the user profile at some point
                        Ok(vec![])
                    },
                    MessageInfoItem::Link(_, url) => {
                        let act = IambAction::OpenLink(url.to_string());
                        Ok(vec![(act.into(), ctx.clone())])
                    },
                }
            },
            PromptAction::Abort(..) => {
                let msg = "Cannot abort a message.";
                let err = EditError::Failure(msg.into());
                Err(err)
            },
            PromptAction::Recall(..) => {
                let msg = "Cannot recall previous messages.";
                let err = EditError::Failure(msg.into());
                Err(err)
            },
        }
    }
}
