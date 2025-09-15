//! # Windows for Matrix rooms and spaces
use matrix_sdk::{
    RoomDisplayName,
    RoomState as MatrixRoomState,
    room::Room as MatrixRoom,
    ruma::{RoomId, events::tag::Tags},
};

use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Modifier as StyleModifier, Style},
    text::{Line, Span, Text},
    widgets::{Paragraph, StatefulWidget, Widget},
};

use modalkit::editing::completion::CompletionList;
use modalkit::errors::EditResult;
use modalkit::prelude::*;
use modalkit::{
    actions::{Action, Editable, EditorAction, Jumpable, PromptAction, Promptable, Scrollable},
    editing::context::EditContext,
};
use modalkit_ratatui::{TermOffset, TerminalCursor, WindowOps};

use crate::{
    base::{
        IambError,
        IambInfo,
        IambResult,
        MessageAction,
        ProgramAction,
        ProgramContext,
        ProgramStore,
        RoomView,
        SendAction,
        SpaceAction,
    },
    windows::room::message::MessageWidget,
};

pub use message::MessageState;

use self::chat::ChatState;
use self::space::{Space, SpaceState};
use crate::config::EncryptionIndicatorLocation;

mod chat;
mod message;
mod scrollback;
mod space;

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s {
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
    Chat(Box<ChatState>),
    Space(Box<SpaceState>),
    Message(Box<MessageState>),
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
    pub fn new(
        room: MatrixRoom,
        view: RoomView,
        name: RoomDisplayName,
        tags: Option<Tags>,
        store: &mut ProgramStore,
    ) -> Self {
        let room_id = room.room_id().to_owned();
        let info = store.application.get_room_info(room_id);
        info.name = name.to_string().into();
        info.tags = tags;

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

    pub fn view(&self) -> RoomView {
        match self {
            RoomState::Chat(chat) => chat.thread().into(),
            RoomState::Space(_) => RoomView::Main,
            RoomState::Message(msg) => RoomView::Message(msg.id()),
        }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        delegate!(self, w => w.refresh_room(store))
    }

    fn draw_invite(
        &self,
        invited: &MatrixRoom,
        area: Rect,
        buf: &mut Buffer,
        store: &mut ProgramStore,
    ) {
        let inviter = store.application.worker.get_inviter(invited.clone());

        let name = match invited.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(self.id())),
        };

        let mut invited = vec![Span::from(format!("You have been invited to join {name}"))];

        if let Ok(Some(inviter)) = &inviter {
            let info = store.application.rooms.get_or_default(self.id().to_owned());
            invited.push(Span::from(" by "));
            invited
                .push(store.application.settings.tunables.get_user_span(inviter.user_id(), info));
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
            None => format!("{:?}", store.application.get_room_title(self.id())),
        };

        let l1 = Line::from(format!(
            "Your request to join {name} is pending review by room moderators."
        ));
        let l2 = Line::from("You can run `:leave` to withdraw your knock request.");
        let text = Text::from(vec![l1, l2]);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    fn draw_left(&self, room: &MatrixRoom, area: Rect, buf: &mut Buffer, store: &mut ProgramStore) {
        let name = match room.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(self.id())),
        };

        let mut lines = vec![Line::from(format!("You have left {name}!"))];

        if room.is_public().is_some_and(|b| b) {
            lines.push(Line::from(format!("You can run `:join {name}` to rejoin.")));
        }

        let text = Text::from(lines);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, EditContext)>> {
        match self {
            RoomState::Chat(chat) => chat.message_command(act, ctx, store).await,
            RoomState::Space(_) => Err(IambError::NoSelectedMessage.into()),
            RoomState::Message(msg) => msg.message_command(act, ctx, store).await,
        }
    }

    pub async fn space_command(
        &mut self,
        act: SpaceAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::Space(space) => space.space_command(act, ctx, store).await,
            RoomState::Chat(_) | RoomState::Message(_) => Err(IambError::NoSelectedSpace.into()),
        }
    }

    pub async fn send_command(
        &mut self,
        act: SendAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::Chat(chat) => chat.send_command(act, ctx, store).await,
            RoomState::Space(_) | RoomState::Message(_) => Err(IambError::NoSelectedRoom.into()),
        }
    }

    pub fn get_title(&self, store: &mut ProgramStore) -> Line<'_> {
        let room = store.application.worker.client.get_room(self.id());

        let title = store.application.get_room_title(self.id());
        let style = Style::default().add_modifier(StyleModifier::BOLD);
        let mut spans = vec![];

        if let RoomState::Chat(chat) = self &&
            chat.thread().is_some()
        {
            spans.push("Thread in ".into());
        }
        if let RoomState::Message(_) = self {
            spans.push("Message in ".into());
        }

        spans.push(Span::styled(title, style));

        if let Some(room) = room {
            let encryption_settings = &store.application.settings.tunables.encryption;
            let encryption_indicator = encryption_settings
                .get_indicator(EncryptionIndicatorLocation::TITLE, room.encryption_state());
            spans.extend(encryption_indicator);
        }

        match self.room().topic() {
            Some(desc) if !desc.is_empty() => {
                spans.push(" (".into());
                spans.push(desc.into());
                spans.push(")".into());
            },
            _ => {
                spans.push(" ".into());
            },
        }

        Line::from(spans)
    }

    pub fn focus_toggle(&mut self) {
        match self {
            RoomState::Chat(chat) => chat.focus_toggle(),
            RoomState::Space(_) | RoomState::Message(_) => return,
        }
    }

    pub fn room(&self) -> &MatrixRoom {
        delegate!(self, w => w.room())
    }

    pub fn id(&self) -> &RoomId {
        match self {
            RoomState::Chat(chat) => chat.id(),
            RoomState::Space(space) => space.id(),
            RoomState::Message(msg) => msg.room_id(),
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
        if self.room().state() != MatrixRoomState::Joined {
            self.refresh_room(store);
        }

        match self.room().state() {
            MatrixRoomState::Invited => return self.draw_invite(self.room(), area, buf, store),
            MatrixRoomState::Knocked => return self.draw_knock(self.room(), area, buf, store),
            MatrixRoomState::Left => return self.draw_left(self.room(), area, buf, store),
            _ => (),
        }

        match self {
            RoomState::Chat(chat) => chat.draw(area, buf, focused, store),
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
            RoomState::Chat(chat) => RoomState::Chat(Box::new(chat.dup(store))),
            RoomState::Space(space) => RoomState::Space(Box::new(space.dup(store))),
            RoomState::Message(msg) => RoomState::Message(Box::new(msg.dup(store))),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        delegate!(self, w => w.close(flags, store))
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        delegate!(self, w => w.write(path, flags, store))
    }

    fn get_completions(&self) -> Option<CompletionList> {
        delegate!(self, w => w.get_completions())
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        delegate!(self, w => w.get_cursor_word(style))
    }

    fn get_selected_word(&self) -> Option<String> {
        delegate!(self, w => w.get_selected_word())
    }
}
