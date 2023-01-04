use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::hash_map::Entry;

use matrix_sdk::{
    encryption::verification::{format_emojis, SasVerification},
    room::{Room as MatrixRoom, RoomMember},
    ruma::{events::room::member::MembershipState, OwnedRoomId, RoomId},
    DisplayName,
};

use modalkit::tui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Modifier as StyleModifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, StatefulWidget, Widget},
};

use modalkit::{
    editing::{
        action::{
            Action,
            EditError,
            EditInfo,
            EditResult,
            Editable,
            EditorAction,
            Jumpable,
            PromptAction,
            Promptable,
            Scrollable,
            UIError,
            WindowAction,
        },
        base::{
            CloseFlags,
            MoveDir1D,
            OpenTarget,
            PositionList,
            ScrollStyle,
            ViewportContext,
            WordStyle,
        },
    },
    widgets::{
        list::{List, ListCursor, ListItem, ListState},
        TermOffset,
        TerminalCursor,
        Window,
        WindowOps,
    },
};

use crate::{
    base::{
        ChatStore,
        IambBufferId,
        IambId,
        IambInfo,
        IambResult,
        ProgramAction,
        ProgramContext,
        ProgramStore,
        RoomAction,
    },
    message::user_style,
};

use self::{room::RoomState, welcome::WelcomeState};

pub mod room;
pub mod welcome;

#[inline]
fn selected_style(selected: bool) -> Style {
    if selected {
        Style::default().add_modifier(StyleModifier::REVERSED)
    } else {
        Style::default()
    }
}

#[inline]
fn selected_span(s: &str, selected: bool) -> Span {
    Span::styled(s, selected_style(selected))
}

#[inline]
fn selected_text(s: &str, selected: bool) -> Text {
    Text::from(selected_span(s, selected))
}

#[inline]
fn room_prompt(
    room_id: &RoomId,
    act: &PromptAction,
    ctx: &ProgramContext,
) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
    match act {
        PromptAction::Submit => {
            let room = IambId::Room(room_id.to_owned());
            let open = WindowAction::Switch(OpenTarget::Application(room));
            let acts = vec![(open.into(), ctx.clone())];

            Ok(acts)
        },
        PromptAction::Abort(_) => {
            let msg = "Cannot abort entry inside a list";
            let err = EditError::Failure(msg.into());

            Err(err)
        },
        PromptAction::Recall(_, _) => {
            let msg = "Cannot recall history inside a list";
            let err = EditError::Failure(msg.into());

            Err(err)
        },
        _ => Err(EditError::Unimplemented("unknown prompt action".to_string())),
    }
}

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s {
            IambWindow::Room($id) => $e,
            IambWindow::DirectList($id) => $e,
            IambWindow::MemberList($id, _) => $e,
            IambWindow::RoomList($id) => $e,
            IambWindow::SpaceList($id) => $e,
            IambWindow::VerifyList($id) => $e,
            IambWindow::Welcome($id) => $e,
        }
    };
}

pub enum IambWindow {
    DirectList(DirectListState),
    MemberList(MemberListState, OwnedRoomId),
    Room(RoomState),
    VerifyList(VerifyListState),
    RoomList(RoomListState),
    SpaceList(SpaceListState),
    Welcome(WelcomeState),
}

impl IambWindow {
    pub fn focus_toggle(&mut self) {
        if let IambWindow::Room(w) = self {
            w.focus_toggle()
        } else {
            return;
        }
    }

    pub fn room_command(
        &mut self,
        act: RoomAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        if let IambWindow::Room(w) = self {
            w.room_command(act, ctx, store)
        } else {
            let msg = "No room currently focused!";
            let err = UIError::Failure(msg.into());

            return Err(err);
        }
    }

    pub fn get_title(&self, store: &mut ProgramStore) -> String {
        match self {
            IambWindow::DirectList(_) => "Direct Messages".to_string(),
            IambWindow::RoomList(_) => "Rooms".to_string(),
            IambWindow::SpaceList(_) => "Spaces".to_string(),
            IambWindow::VerifyList(_) => "Verifications".to_string(),
            IambWindow::Welcome(_) => "Welcome to iamb".to_string(),

            IambWindow::Room(w) => w.get_title(store),
            IambWindow::MemberList(_, room_id) => {
                let title = store.application.get_room_title(room_id.as_ref());
                format!("Room Members: {}", title)
            },
        }
    }
}

pub type DirectListState = ListState<DirectItem, IambInfo>;
pub type MemberListState = ListState<MemberItem, IambInfo>;
pub type RoomListState = ListState<RoomItem, IambInfo>;
pub type SpaceListState = ListState<SpaceItem, IambInfo>;
pub type VerifyListState = ListState<VerifyItem, IambInfo>;

impl From<RoomState> for IambWindow {
    fn from(room: RoomState) -> Self {
        IambWindow::Room(room)
    }
}

impl From<VerifyListState> for IambWindow {
    fn from(list: VerifyListState) -> Self {
        IambWindow::VerifyList(list)
    }
}

impl From<DirectListState> for IambWindow {
    fn from(list: DirectListState) -> Self {
        IambWindow::DirectList(list)
    }
}

impl From<RoomListState> for IambWindow {
    fn from(list: RoomListState) -> Self {
        IambWindow::RoomList(list)
    }
}

impl From<SpaceListState> for IambWindow {
    fn from(list: SpaceListState) -> Self {
        IambWindow::SpaceList(list)
    }
}

impl From<WelcomeState> for IambWindow {
    fn from(win: WelcomeState) -> Self {
        IambWindow::Welcome(win)
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for IambWindow {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.editor_command(act, ctx, store))
    }
}

impl Jumpable<ProgramContext, IambInfo> for IambWindow {
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

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for IambWindow {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.scroll(style, ctx, store))
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for IambWindow {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        delegate!(self, w => w.prompt(act, ctx, store))
    }
}

impl TerminalCursor for IambWindow {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        delegate!(self, w => w.get_term_cursor())
    }
}

impl WindowOps<IambInfo> for IambWindow {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        let title = self.get_title(store);
        let block = Block::default().title(title.as_str()).borders(Borders::ALL);
        let inner = block.inner(area);
        block.render(area, buf);

        match self {
            IambWindow::Room(state) => state.draw(inner, buf, focused, store),
            IambWindow::DirectList(state) => {
                let dms = store.application.worker.direct_messages();
                let items = dms.into_iter().map(|(id, name)| DirectItem::new(id, name, store));
                state.set(items.collect());

                List::new(store)
                    .empty_message("No direct messages yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(inner, buf, state);
            },
            IambWindow::MemberList(state, room_id) => {
                if let Ok(mems) = store.application.worker.members(room_id.clone()) {
                    let items = mems.into_iter().map(MemberItem::new);
                    state.set(items.collect());
                }

                List::new(store)
                    .empty_message("No users here yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(inner, buf, state);
            },
            IambWindow::RoomList(state) => {
                let joined = store.application.worker.joined_rooms();
                let items = joined.into_iter().map(|(id, name)| RoomItem::new(id, name, store));
                state.set(items.collect());

                List::new(store)
                    .empty_message("You haven't joined any rooms yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(inner, buf, state);
            },
            IambWindow::SpaceList(state) => {
                let spaces = store.application.worker.spaces();
                let items =
                    spaces.into_iter().map(|(room, name)| SpaceItem::new(room, name, store));
                state.set(items.collect());
                state.draw(inner, buf, focused, store);

                List::new(store)
                    .empty_message("You haven't joined any spaces yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(inner, buf, state);
            },
            IambWindow::VerifyList(state) => {
                let verifications = &store.application.verifications;
                let mut items = verifications.iter().map(VerifyItem::from).collect::<Vec<_>>();

                // Sort the active verifications towards the top.
                items.sort();

                state.set(items);

                List::new(store)
                    .empty_message("No in-progress verifications")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(inner, buf, state);
            },
            IambWindow::Welcome(state) => state.draw(inner, buf, focused, store),
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            IambWindow::Room(w) => w.dup(store).into(),
            IambWindow::DirectList(w) => w.dup(store).into(),
            IambWindow::MemberList(w, room_id) => {
                IambWindow::MemberList(w.dup(store), room_id.clone())
            },
            IambWindow::RoomList(w) => w.dup(store).into(),
            IambWindow::SpaceList(w) => w.dup(store).into(),
            IambWindow::VerifyList(w) => w.dup(store).into(),
            IambWindow::Welcome(w) => w.dup(store).into(),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        delegate!(self, w => w.close(flags, store))
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        delegate!(self, w => w.get_cursor_word(style))
    }

    fn get_selected_word(&self) -> Option<String> {
        delegate!(self, w => w.get_selected_word())
    }
}

impl Window<IambInfo> for IambWindow {
    fn id(&self) -> IambId {
        match self {
            IambWindow::Room(room) => IambId::Room(room.id().to_owned()),
            IambWindow::DirectList(_) => IambId::DirectList,
            IambWindow::MemberList(_, room_id) => IambId::MemberList(room_id.clone()),
            IambWindow::RoomList(_) => IambId::RoomList,
            IambWindow::SpaceList(_) => IambId::SpaceList,
            IambWindow::VerifyList(_) => IambId::VerifyList,
            IambWindow::Welcome(_) => IambId::Welcome,
        }
    }

    fn open(id: IambId, store: &mut ProgramStore) -> IambResult<Self> {
        match id {
            IambId::Room(room_id) => {
                let (room, name) = store.application.worker.get_room(room_id)?;
                let room = RoomState::new(room, name, store);

                return Ok(room.into());
            },
            IambId::DirectList => {
                let list = DirectListState::new(IambBufferId::DirectList, vec![]);

                return Ok(list.into());
            },
            IambId::MemberList(room_id) => {
                let id = IambBufferId::MemberList(room_id.clone());
                let list = MemberListState::new(id, vec![]);
                let win = IambWindow::MemberList(list, room_id);

                return Ok(win);
            },
            IambId::RoomList => {
                let list = RoomListState::new(IambBufferId::RoomList, vec![]);

                return Ok(list.into());
            },
            IambId::SpaceList => {
                let list = SpaceListState::new(IambBufferId::SpaceList, vec![]);

                return Ok(list.into());
            },
            IambId::VerifyList => {
                let list = VerifyListState::new(IambBufferId::VerifyList, vec![]);

                return Ok(list.into());
            },
            IambId::Welcome => {
                let win = WelcomeState::new(store);

                return Ok(win.into());
            },
        }
    }

    fn find(name: String, store: &mut ProgramStore) -> IambResult<Self> {
        let ChatStore { names, worker, .. } = &mut store.application;

        match names.entry(name) {
            Entry::Vacant(v) => {
                let room_id = worker.join_room(v.key().to_string())?;
                v.insert(room_id.clone());

                let (room, name) = store.application.worker.get_room(room_id)?;
                let room = RoomState::new(room, name, store);

                Ok(room.into())
            },
            Entry::Occupied(o) => {
                let id = IambId::Room(o.get().clone());

                IambWindow::open(id, store)
            },
        }
    }

    fn posn(index: usize, _: &mut ProgramStore) -> IambResult<Self> {
        let msg = format!("Cannot find indexed buffer (index = {})", index);
        let err = UIError::Unimplemented(msg);

        Err(err)
    }
}

#[derive(Clone)]
pub struct RoomItem {
    room: MatrixRoom,
    name: String,
}

impl RoomItem {
    fn new(room: MatrixRoom, name: DisplayName, store: &mut ProgramStore) -> Self {
        let name = name.to_string();

        store.application.set_room_name(room.room_id(), name.as_str());

        RoomItem { room, name }
    }
}

impl ToString for RoomItem {
    fn to_string(&self) -> String {
        return self.name.clone();
    }
}

impl ListItem<IambInfo> for RoomItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        selected_text(self.name.as_str(), selected)
    }

    fn get_word(&self) -> Option<String> {
        self.room.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for RoomItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room.room_id(), act, ctx)
    }
}

#[derive(Clone)]
pub struct DirectItem {
    room: MatrixRoom,
    name: String,
}

impl DirectItem {
    fn new(room: MatrixRoom, name: DisplayName, store: &mut ProgramStore) -> Self {
        let name = name.to_string();

        store.application.set_room_name(room.room_id(), name.as_str());

        DirectItem { room, name }
    }
}

impl ToString for DirectItem {
    fn to_string(&self) -> String {
        return self.name.clone();
    }
}

impl ListItem<IambInfo> for DirectItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        selected_text(self.name.as_str(), selected)
    }

    fn get_word(&self) -> Option<String> {
        self.room.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for DirectItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room.room_id(), act, ctx)
    }
}

#[derive(Clone)]
pub struct SpaceItem {
    room: MatrixRoom,
    name: String,
}

impl SpaceItem {
    fn new(room: MatrixRoom, name: DisplayName, store: &mut ProgramStore) -> Self {
        let name = name.to_string();

        store.application.set_room_name(room.room_id(), name.as_str());

        SpaceItem { room, name }
    }
}

impl ToString for SpaceItem {
    fn to_string(&self) -> String {
        return self.room.room_id().to_string();
    }
}

impl ListItem<IambInfo> for SpaceItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        selected_text(self.name.as_str(), selected)
    }

    fn get_word(&self) -> Option<String> {
        self.room.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for SpaceItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room.room_id(), act, ctx)
    }
}

#[derive(Clone)]
pub struct VerifyItem {
    user_dev: String,
    sasv1: SasVerification,
}

impl VerifyItem {
    fn new(user_dev: String, sasv1: SasVerification) -> Self {
        VerifyItem { user_dev, sasv1 }
    }

    fn show_item(&self) -> String {
        let state = if self.sasv1.is_done() {
            "done"
        } else if self.sasv1.is_cancelled() {
            "cancelled"
        } else if self.sasv1.emoji().is_some() {
            "accepted"
        } else {
            "not accepted"
        };

        if self.sasv1.is_self_verification() {
            let device = self.sasv1.other_device();

            if let Some(display_name) = device.display_name() {
                format!("Device verification with {} ({})", display_name, state)
            } else {
                format!("Device verification with device {} ({})", device.device_id(), state)
            }
        } else {
            format!("User Verification with {} ({})", self.sasv1.other_user_id(), state)
        }
    }
}

impl PartialEq for VerifyItem {
    fn eq(&self, other: &Self) -> bool {
        self.user_dev == other.user_dev
    }
}

impl Eq for VerifyItem {}

impl Ord for VerifyItem {
    fn cmp(&self, other: &Self) -> Ordering {
        fn state_val(sas: &SasVerification) -> usize {
            if sas.is_done() {
                return 3;
            } else if sas.is_cancelled() {
                return 2;
            } else {
                return 1;
            }
        }

        fn device_val(sas: &SasVerification) -> usize {
            if sas.is_self_verification() {
                return 1;
            } else {
                return 2;
            }
        }

        let state1 = state_val(&self.sasv1);
        let state2 = state_val(&other.sasv1);

        let dev1 = device_val(&self.sasv1);
        let dev2 = device_val(&other.sasv1);

        let scmp = state1.cmp(&state2);
        let dcmp = dev1.cmp(&dev2);

        scmp.then(dcmp).then_with(|| {
            let did1 = self.sasv1.other_device().device_id();
            let did2 = other.sasv1.other_device().device_id();

            did1.cmp(did2)
        })
    }
}

impl PartialOrd for VerifyItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp(other).into()
    }
}

impl From<(&String, &SasVerification)> for VerifyItem {
    fn from((user_dev, sasv1): (&String, &SasVerification)) -> Self {
        VerifyItem::new(user_dev.clone(), sasv1.clone())
    }
}

impl ToString for VerifyItem {
    fn to_string(&self) -> String {
        if self.sasv1.is_done() {
            String::new()
        } else if self.sasv1.is_cancelled() {
            format!(":verify request {}", self.sasv1.other_user_id())
        } else if self.sasv1.emoji().is_some() {
            format!(":verify confirm {}", self.user_dev)
        } else {
            format!(":verify accept {}", self.user_dev)
        }
    }
}

impl ListItem<IambInfo> for VerifyItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        let mut lines = vec![];

        let bold = Style::default().add_modifier(StyleModifier::BOLD);
        let item = Span::styled(self.show_item(), selected_style(selected));
        lines.push(Spans::from(item));

        if self.sasv1.is_done() {
            // Print nothing.
        } else if self.sasv1.is_cancelled() {
            if let Some(info) = self.sasv1.cancel_info() {
                lines.push(Spans::from(format!("    Cancelled: {}", info.reason())));
                lines.push(Spans::from(""));
            }

            lines.push(Spans::from("    You can start a new verification request with:"));
        } else if let Some(emoji) = self.sasv1.emoji() {
            lines.push(Spans::from(
                "    Both devices should see the following Emoji sequence:".to_string(),
            ));
            lines.push(Spans::from(""));

            for line in format_emojis(emoji).lines() {
                lines.push(Spans::from(format!("    {}", line)));
            }

            lines.push(Spans::from(""));
            lines.push(Spans::from("    If they don't match, run:"));
            lines.push(Spans::from(""));
            lines.push(Spans::from(Span::styled(
                format!(":verify mismatch {}", self.user_dev),
                bold,
            )));
            lines.push(Spans::from(""));
            lines.push(Spans::from("    If everything looks right, you can confirm with:"));
        } else {
            lines.push(Spans::from("    To accept this request, run:"));
        }

        let cmd = self.to_string();

        if !cmd.is_empty() {
            lines.push(Spans::from(""));
            lines.push(Spans(vec![Span::from("        "), Span::styled(cmd, bold)]));
            lines.push(Spans::from(""));
            lines.push(Spans(vec![
                Span::from("You can copy the above command with "),
                Span::styled("yy", bold),
                Span::from(" and then execute it with "),
                Span::styled("@\"", bold),
            ]));
        }

        Text { lines }
    }

    fn get_word(&self) -> Option<String> {
        None
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for VerifyItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        match act {
            PromptAction::Submit => Ok(vec![]),
            PromptAction::Abort(_) => {
                let msg = "Cannot abort entry inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            PromptAction::Recall(_, _) => {
                let msg = "Cannot recall history inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            _ => Err(EditError::Unimplemented("unknown prompt action".to_string())),
        }
    }
}

#[derive(Clone)]
pub struct MemberItem {
    member: RoomMember,
}

impl MemberItem {
    fn new(member: RoomMember) -> Self {
        Self { member }
    }
}

impl ToString for MemberItem {
    fn to_string(&self) -> String {
        self.member.user_id().to_string()
    }
}

impl ListItem<IambInfo> for MemberItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        let mut style = user_style(self.member.user_id().as_str());

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED);
        }

        let user = Span::styled(self.to_string(), style);

        let state = match self.member.membership() {
            MembershipState::Ban => Span::raw(" (banned)").into(),
            MembershipState::Invite => Span::raw(" (invited)").into(),
            MembershipState::Knock => Span::raw(" (wants to join)").into(),
            MembershipState::Leave => Span::raw(" (left)").into(),
            MembershipState::Join => None,
            _ => None,
        };

        if let Some(state) = state {
            Spans(vec![user, state]).into()
        } else {
            user.into()
        }
    }

    fn get_word(&self) -> Option<String> {
        self.member.user_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for MemberItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        match act {
            PromptAction::Submit => Ok(vec![]),
            PromptAction::Abort(_) => {
                let msg = "Cannot abort entry inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            PromptAction::Recall(_, _) => {
                let msg = "Cannot recall history inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            _ => Err(EditError::Unimplemented("unknown prompt action".to_string())),
        }
    }
}
