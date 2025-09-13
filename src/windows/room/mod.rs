//! # Windows for Matrix rooms and spaces

use modalkit::editing::completion::CompletionList;
use modalkit::editing::context::EditContext;

use crate::config::EncryptionIndicatorLocation;
use crate::prelude::*;
use crate::windows::room::chat::ChatState;
use crate::windows::room::message::{MessageState, MessageWidget};
use crate::windows::room::not_joined::{NotJoined, NotJoinedState};
use crate::windows::room::space::{Space, SpaceState};

mod chat;
mod message;
mod not_joined;
mod scrollback;
mod space;

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s {
            RoomState::NotJoined($id) => $e,
            RoomState::Chat($id) => $e,
            RoomState::Space($id) => $e,
            RoomState::Message($id) => $e,
        }
    };
}

/// State for a Matrix room or space.
///
/// Since spaces function as special rooms within Matrix, we wrap their window state together, so
/// that operations like sending and accepting invites, opening the members window, etc., all work
/// similarly.
pub enum RoomState {
    NotJoined(Box<NotJoinedState>),
    Chat(Box<ChatState>),
    Space(Box<SpaceState>),
    Message(Box<MessageState>),
}

impl From<NotJoinedState> for RoomState {
    fn from(chat: NotJoinedState) -> Self {
        RoomState::NotJoined(Box::new(chat))
    }
}

impl From<ChatState> for RoomState {
    fn from(chat: ChatState) -> Self {
        RoomState::Chat(Box::new(chat))
    }
}

impl From<SpaceState> for RoomState {
    fn from(space: SpaceState) -> Self {
        RoomState::Space(Box::new(space))
    }
}

impl From<MessageState> for RoomState {
    fn from(msg: MessageState) -> Self {
        RoomState::Message(Box::new(msg))
    }
}

impl RoomState {
    pub fn new(room: MatrixRoom, view: RoomView, store: &mut ProgramStore) -> Self {
        if room.is_space() {
            SpaceState::new(room).into()
        } else {
            match view {
                RoomView::Main => ChatState::new(room, None, store).into(),
                RoomView::Thread(thread) => ChatState::new(room, Some(thread), store).into(),
                RoomView::Message(message) => MessageState::new(store, room, message).into(),
            }
        }
    }

    pub fn not_joined(alias: OwnedRoomOrAliasId, store: &mut ProgramStore) -> Self {
        Self::from(NotJoinedState::new(alias, store))
    }

    pub fn window_id(&self) -> IambId {
        match self {
            RoomState::NotJoined(nj) => IambId::Room(nj.alias().to_owned(), RoomView::Main),
            RoomState::Chat(chat) => {
                IambId::Room(chat.id().to_owned().into(), chat.thread().cloned().into())
            },
            RoomState::Space(space) => IambId::Room(space.id().to_owned().into(), RoomView::Main),
            RoomState::Message(msg) => {
                IambId::Room(
                    msg.room_id().to_owned().into(),
                    RoomView::Message(msg.id().to_owned()),
                )
            },
        }
    }

    pub fn room_state(&self) -> Option<MatrixRoomState> {
        match self {
            RoomState::NotJoined(_) => None,
            RoomState::Chat(chat) => Some(chat.room().state()),
            RoomState::Space(space) => Some(space.room().state()),
            RoomState::Message(msg) => Some(msg.room().state()),
        }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        if let Some(room) = self.room() &&
            room.state() == MatrixRoomState::Left
        {
            // room previews have more information than left rooms
            let alias_id = if let Some(alias) = room.canonical_alias() {
                alias.into()
            } else {
                room.room_id().to_owned().into()
            };
            *self = Self::not_joined(alias_id, store);
        }

        match self {
            RoomState::NotJoined(not_joined) => {
                if let Some(state) = not_joined.refresh_room(store) {
                    *self = state;
                }
            },
            RoomState::Chat(chat) => chat.refresh_room(store),
            RoomState::Space(space) => space.refresh_room(store),
            RoomState::Message(msg) => msg.refresh_room(store),
        }
    }

    fn draw_invite(
        &self,
        invited: &MatrixRoom,
        area: Rect,
        buf: &mut Buffer,
        store: &mut ProgramStore,
    ) {
        let inviter = store.application.worker.get_inviter(invited.clone());
        let room_id = invited.room_id();

        let name = match invited.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(room_id)),
        };

        let mut invited = vec![Span::from(format!("You have been invited to join {name}"))];

        if let Ok(Some(inviter)) = &inviter {
            let info = store.application.rooms.get_or_default(room_id.to_owned());
            invited.push(Span::from(" by "));
            invited.push(store.application.settings.get_user_span(inviter.user_id(), info));
        }

        let l1 = Line::from(invited);
        let l2 = Line::from(
            "You can run `:invite accept` or `:invite reject` to accept or reject this invitation.",
        );
        let text = Text::from(vec![l1, l2]);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    fn draw_knock(
        &self,
        knocked: &MatrixRoom,
        area: Rect,
        buf: &mut Buffer,
        store: &mut ProgramStore,
    ) {
        let name = match knocked.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(knocked.room_id())),
        };

        let l1 = Line::from(format!(
            "Your request to join {name} is pending review by room moderators."
        ));
        let l2 = Line::from("You can run `:leave` to withdraw your knock request.");
        let text = Text::from(vec![l1, l2]);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    pub async fn timeline_command(
        &mut self,
        act: TimelineAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let RoomState::Chat(chat) = self {
            chat.timeline_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedRoom.into())
        }
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, EditContext)>> {
        match self {
            RoomState::Chat(chat) => chat.message_command(act, ctx, store).await,
            RoomState::Message(msg) => msg.message_command(act, ctx, store).await,
            _ => Err(IambError::NoSelectedMessage.into()),
        }
    }

    pub async fn space_command(
        &mut self,
        act: SpaceAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let RoomState::Space(space) = self {
            space.space_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedSpace.into())
        }
    }

    pub async fn join_command(
        &mut self,
        act: JoinAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        if let RoomState::NotJoined(nj) = self {
            nj.join_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedSpace.into())
        }
    }

    pub async fn send_command(
        &mut self,
        act: SendAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let RoomState::Chat(chat) = self {
            chat.send_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedRoom.into())
        }
    }

    pub fn get_title(&self, store: &mut ProgramStore) -> Line<'_> {
        let theme = &store.application.settings.theme;
        let default_style = theme.windows.default;
        let title_style = theme.windows.title;

        let Some(room) = self.room() else {
            if let RoomState::NotJoined(nj) = self {
                return nj.get_title(store);
            } else {
                return Line::styled("Unknown Room", title_style);
            }
        };

        let room_id = room.room_id();
        let title = store.application.get_room_title(room_id);
        let mut spans = vec![];

        let encryption_settings = &store.application.settings.tunables.encryption;
        let encryption_indicator = encryption_settings.get_indicator(
            EncryptionIndicatorLocation::TITLE,
            room.encryption_state(),
            theme,
        );
        spans.extend(encryption_indicator);
        spans.push(Span::styled(" ", default_style));

        if let RoomState::Chat(chat) = self &&
            chat.thread().is_some()
        {
            spans.push(Span::styled("Thread in ", default_style));
        }
        if let RoomState::Message(_) = self {
            spans.push("Message in ".into());
        }

        spans.push(Span::styled(title, title_style));

        match room.topic() {
            Some(desc) if !desc.is_empty() => {
                spans.push(Span::styled(" (", default_style));
                spans.push(Span::styled(desc, default_style));
                spans.push(Span::styled(")", default_style));
            },
            _ => {
                spans.push(Span::styled(" ", default_style));
            },
        }

        Line::from(spans)
    }

    pub fn get_tab_title(&self, store: &mut ProgramStore) -> Line<'_> {
        match self {
            RoomState::Space(w) => store.application.get_room_title(w.id()).into(),
            RoomState::Chat(w) => store.application.get_room_title(w.id()).into(),
            RoomState::NotJoined(w) => w.get_tab_title(store),
            RoomState::Message(w) => {
                let name = store.application.get_room_title(w.room_id());

                Line::from(vec![Span::raw("Message in "), Span::raw(name)])
            },
        }
    }

    pub fn focus_toggle(&mut self) {
        if let RoomState::Chat(chat) = self {
            chat.focus_toggle();
        }
    }

    pub fn room(&self) -> Option<&MatrixRoom> {
        match self {
            RoomState::Chat(chat) => Some(chat.room()),
            RoomState::Space(space) => Some(space.room()),
            RoomState::Message(msg) => Some(msg.room()),
            RoomState::NotJoined(_) => None,
        }
    }

    pub fn id(&mut self, store: &ProgramStore) -> Option<&RoomId> {
        match self {
            RoomState::Chat(chat) => Some(chat.id()),
            RoomState::Space(space) => Some(space.id()),
            RoomState::NotJoined(nj) => nj.room_id(store),
            RoomState::Message(msg) => Some(msg.room_id()),
        }
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for RoomState {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.editor_command(act, ctx, store))
    }
}

impl Jumpable<ProgramContext, IambInfo> for RoomState {
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &ProgramContext,
    ) -> IambResult<usize> {
        delegate!(self, w => w.jump(list, dir, count, ctx))
    }
}

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for RoomState {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.scroll(style, ctx, store))
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for RoomState {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        delegate!(self, w => w.prompt(act, ctx, store))
    }
}

impl TerminalCursor for RoomState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        delegate!(self, w => w.get_term_cursor())
    }

    fn hide_term_cursor(&self) -> bool {
        delegate!(self, w => w.hide_term_cursor())
    }
}

impl WindowOps<IambInfo> for RoomState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        if self.room_state() != Some(MatrixRoomState::Joined) {
            self.refresh_room(store);
        }

        if let Some(room) = self.room() {
            match room.state() {
                MatrixRoomState::Invited => return self.draw_invite(room, area, buf, store),
                MatrixRoomState::Knocked => return self.draw_knock(room, area, buf, store),
                _ => (),
            }
        }

        match self {
            RoomState::Chat(chat) => {
                chat.draw(area, buf, focused, store);
            },
            RoomState::NotJoined(state) => {
                NotJoined::new(store).render(area, buf, state);
            },
            RoomState::Message(msg) => {
                MessageWidget::new(store).focus(focused).render(area, buf, msg)
            },
            RoomState::Space(space) => {
                Space::new(store).focus(focused).render(area, buf, space);
            },
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            RoomState::NotJoined(w) => RoomState::NotJoined(Box::new(w.dup())),
            RoomState::Chat(chat) => RoomState::Chat(Box::new(chat.dup(store))),
            RoomState::Space(space) => RoomState::Space(Box::new(space.dup(store))),
            RoomState::Message(msg) => RoomState::Message(Box::new(msg.dup(store))),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        match self {
            RoomState::NotJoined(_) => true,
            RoomState::Chat(chat) => chat.close(flags, store),
            RoomState::Space(space) => space.close(flags, store),
            RoomState::Message(msg) => msg.close(flags, store),
        }
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::NotJoined(_) => Err(EditError::ReadOnly.into()),
            RoomState::Chat(chat) => chat.write(path, flags, store),
            RoomState::Space(space) => space.write(path, flags, store),
            RoomState::Message(msg) => msg.write(path, flags, store),
        }
    }

    fn get_completions(&self) -> Option<CompletionList> {
        match self {
            RoomState::NotJoined(_) => None,
            RoomState::Chat(chat) => chat.get_completions(),
            RoomState::Space(space) => space.get_completions(),
            RoomState::Message(msg) => msg.get_completions(),
        }
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        match self {
            RoomState::NotJoined(_) => None,
            RoomState::Chat(chat) => chat.get_cursor_word(style),
            RoomState::Space(space) => space.get_cursor_word(style),
            RoomState::Message(msg) => msg.get_cursor_word(style),
        }
    }

    fn get_selected_word(&self) -> Option<String> {
        match self {
            RoomState::NotJoined(_) => None,
            RoomState::Chat(chat) => chat.get_selected_word(),
            RoomState::Space(space) => space.get_selected_word(),
            RoomState::Message(msg) => msg.get_selected_word(),
        }
    }
}
