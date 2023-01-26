use matrix_sdk::{
    room::{Invited, Room as MatrixRoom},
    ruma::{
        events::{
            room::{name::RoomNameEventContent, topic::RoomTopicEventContent},
            tag::{TagInfo, Tags},
        },
        RoomId,
    },
    DisplayName,
};

use modalkit::tui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Modifier as StyleModifier, Style},
    text::{Span, Spans, Text},
    widgets::{Paragraph, StatefulWidget, Widget},
};

use modalkit::{
    editing::action::{
        Action,
        EditInfo,
        EditResult,
        Editable,
        EditorAction,
        Jumpable,
        PromptAction,
        Promptable,
        Scrollable,
        UIError,
    },
    editing::base::{
        Axis,
        CloseFlags,
        Count,
        MoveDir1D,
        OpenTarget,
        PositionList,
        ScrollStyle,
        WordStyle,
    },
    input::InputContext,
    widgets::{TermOffset, TerminalCursor, WindowOps},
};

use crate::base::{
    IambError,
    IambId,
    IambInfo,
    IambResult,
    MessageAction,
    ProgramAction,
    ProgramContext,
    ProgramStore,
    RoomAction,
    RoomField,
    SendAction,
};

use self::chat::ChatState;
use self::space::{Space, SpaceState};

mod chat;
mod scrollback;
mod space;

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s {
            RoomState::Chat($id) => $e,
            RoomState::Space($id) => $e,
        }
    };
}

pub enum RoomState {
    Chat(ChatState),
    Space(SpaceState),
}

impl From<ChatState> for RoomState {
    fn from(chat: ChatState) -> Self {
        RoomState::Chat(chat)
    }
}

impl From<SpaceState> for RoomState {
    fn from(space: SpaceState) -> Self {
        RoomState::Space(space)
    }
}

impl RoomState {
    pub fn new(
        room: MatrixRoom,
        name: DisplayName,
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
            ChatState::new(room, store).into()
        }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        match self {
            RoomState::Chat(chat) => chat.refresh_room(store),
            RoomState::Space(space) => space.refresh_room(store),
        }
    }

    fn draw_invite(
        &self,
        invited: Invited,
        area: Rect,
        buf: &mut Buffer,
        store: &mut ProgramStore,
    ) {
        let inviter = store.application.worker.get_inviter(invited.clone());

        let name = match invited.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(self.id())),
        };

        let mut invited = vec![Span::from(format!(
            "You have been invited to join {}",
            name
        ))];

        if let Ok(Some(inviter)) = &inviter {
            invited.push(Span::from(" by "));
            invited.push(store.application.settings.get_user_span(inviter.user_id()));
        }

        let l1 = Spans(invited);
        let l2 = Spans::from(
            "You can run `:invite accept` or `:invite reject` to accept or reject this invitation.",
        );
        let text = Text { lines: vec![l1, l2] };

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::Chat(chat) => chat.message_command(act, ctx, store).await,
            RoomState::Space(_) => Err(IambError::NoSelectedMessage.into()),
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
            RoomState::Space(_) => Err(IambError::NoSelectedRoom.into()),
        }
    }

    pub async fn room_command(
        &mut self,
        act: RoomAction,
        _: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        match act {
            RoomAction::InviteAccept => {
                if let Some(room) = store.application.worker.client.get_invited_room(self.id()) {
                    room.accept_invitation().await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    Err(IambError::NotInvited.into())
                }
            },
            RoomAction::InviteReject => {
                if let Some(room) = store.application.worker.client.get_invited_room(self.id()) {
                    room.reject_invitation().await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    Err(IambError::NotInvited.into())
                }
            },
            RoomAction::InviteSend(user) => {
                if let Some(room) = store.application.worker.client.get_joined_room(self.id()) {
                    room.invite_user_by_id(user.as_ref()).await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    Err(IambError::NotJoined.into())
                }
            },
            RoomAction::Members(mut cmd) => {
                let width = Count::Exact(30);
                let act =
                    cmd.default_axis(Axis::Vertical).default_relation(MoveDir1D::Next).window(
                        OpenTarget::Application(IambId::MemberList(self.id().to_owned())),
                        width.into(),
                    );

                Ok(vec![(act, cmd.context.take())])
            },
            RoomAction::Set(field, value) => {
                let room = store
                    .application
                    .get_joined_room(self.id())
                    .ok_or(UIError::Application(IambError::NotJoined))?;

                match field {
                    RoomField::Name => {
                        let ev = RoomNameEventContent::new(value.into());
                        let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                    },
                    RoomField::Tag(tag) => {
                        let mut info = TagInfo::new();
                        info.order = Some(1.0);

                        let _ = room.set_tag(tag, info).await.map_err(IambError::from)?;
                    },
                    RoomField::Topic => {
                        let ev = RoomTopicEventContent::new(value);
                        let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                    },
                }

                Ok(vec![])
            },
            RoomAction::Unset(field) => {
                let room = store
                    .application
                    .get_joined_room(self.id())
                    .ok_or(UIError::Application(IambError::NotJoined))?;

                match field {
                    RoomField::Name => {
                        let ev = RoomNameEventContent::new(None);
                        let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                    },
                    RoomField::Tag(tag) => {
                        let _ = room.remove_tag(tag).await.map_err(IambError::from)?;
                    },
                    RoomField::Topic => {
                        let ev = RoomTopicEventContent::new("".into());
                        let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                    },
                }

                Ok(vec![])
            },
        }
    }

    pub fn get_title(&self, store: &mut ProgramStore) -> Spans {
        let title = store.application.get_room_title(self.id());
        let style = Style::default().add_modifier(StyleModifier::BOLD);
        let mut spans = vec![Span::styled(title, style)];

        match self.room().topic() {
            Some(desc) if !desc.is_empty() => {
                spans.push(" (".into());
                spans.push(desc.into());
                spans.push(")".into());
            },
            _ => {},
        }

        Spans(spans)
    }

    pub fn focus_toggle(&mut self) {
        match self {
            RoomState::Chat(chat) => chat.focus_toggle(),
            RoomState::Space(_) => return,
        }
    }

    pub fn room(&self) -> &MatrixRoom {
        match self {
            RoomState::Chat(chat) => chat.room(),
            RoomState::Space(space) => space.room(),
        }
    }

    pub fn id(&self) -> &RoomId {
        match self {
            RoomState::Chat(chat) => chat.id(),
            RoomState::Space(space) => space.id(),
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
}

impl WindowOps<IambInfo> for RoomState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        if let MatrixRoom::Invited(_) = self.room() {
            self.refresh_room(store);
        }

        if let MatrixRoom::Invited(invited) = self.room() {
            self.draw_invite(invited.clone(), area, buf, store);
        }

        match self {
            RoomState::Chat(chat) => chat.draw(area, buf, focused, store),
            RoomState::Space(space) => {
                Space::new(store).focus(focused).render(area, buf, space);
            },
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            RoomState::Chat(chat) => RoomState::Chat(chat.dup(store)),
            RoomState::Space(space) => RoomState::Space(space.dup(store)),
        }
    }

    fn close(&mut self, _: CloseFlags, _: &mut ProgramStore) -> bool {
        // XXX: what's the right closing behaviour for a room?
        // Should write send a message?
        true
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        match self {
            RoomState::Chat(chat) => chat.get_cursor_word(style),
            RoomState::Space(space) => space.get_cursor_word(style),
        }
    }

    fn get_selected_word(&self) -> Option<String> {
        match self {
            RoomState::Chat(chat) => chat.get_selected_word(),
            RoomState::Space(space) => space.get_selected_word(),
        }
    }
}
