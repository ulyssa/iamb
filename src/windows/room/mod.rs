use matrix_sdk::room::Room as MatrixRoom;
use matrix_sdk::ruma::RoomId;
use matrix_sdk::DisplayName;

use modalkit::tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Modifier as StyleModifier, Style},
    text::{Span, Spans},
    widgets::StatefulWidget,
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
    IambId,
    IambInfo,
    IambResult,
    ProgramAction,
    ProgramContext,
    ProgramStore,
    RoomAction,
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
    pub fn new(room: MatrixRoom, name: DisplayName, store: &mut ProgramStore) -> Self {
        let room_id = room.room_id().to_owned();
        let info = store.application.get_room_info(room_id);
        info.name = name.to_string().into();

        if room.is_space() {
            SpaceState::new(room).into()
        } else {
            ChatState::new(room, store).into()
        }
    }

    pub fn room_command(
        &mut self,
        act: RoomAction,
        _: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        match act {
            RoomAction::Members(mut cmd) => {
                let width = Count::Exact(30);
                let act =
                    cmd.default_axis(Axis::Vertical).default_relation(MoveDir1D::Next).window(
                        OpenTarget::Application(IambId::MemberList(self.id().to_owned())),
                        width.into(),
                    );

                Ok(vec![(act, cmd.context.take())])
            },
            RoomAction::Set(field) => {
                store.application.worker.set_room(self.id().to_owned(), field)?;

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
