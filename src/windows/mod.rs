//! # Windows for the User Interface
//!
//! This module contains the logic for rendering windows, and handling UI actions that get
//! delegated to individual windows/UI elements (e.g., typing text or selecting a list item).
//!
//! Additionally, some of the iamb commands delegate behaviour to the current UI element. For
//! example, [sending messages][crate::base::SendAction] delegate to the [room window][RoomState],
//! where we have the message bar and room ID easily accesible and resetable.
use std::cmp::{Ord, Ordering, PartialOrd};
use std::ops::Deref;
use std::sync::Arc;
use std::time::{Duration, Instant};

use matrix_sdk::{
    encryption::verification::{format_emojis, SasVerification},
    room::{Room as MatrixRoom, RoomMember},
    ruma::{
        events::room::member::MembershipState,
        events::tag::{TagName, Tags},
        OwnedRoomAliasId,
        OwnedRoomId,
        RoomAliasId,
        RoomId,
    },
};

use modalkit::tui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Modifier as StyleModifier, Style},
    text::{Line, Span, Text},
    widgets::StatefulWidget,
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
            WriteFlags,
        },
        completion::CompletionList,
    },
    widgets::{
        list::{List, ListCursor, ListItem, ListState},
        TermOffset,
        TerminalCursor,
        Window,
        WindowOps,
    },
};

use crate::base::{
    ChatStore,
    IambBufferId,
    IambError,
    IambId,
    IambInfo,
    IambResult,
    MessageAction,
    Need,
    ProgramAction,
    ProgramContext,
    ProgramStore,
    RoomAction,
    SendAction,
    SortColumn,
    SortFieldRoom,
    SortFieldUser,
    SortOrder,
};

use self::{room::RoomState, welcome::WelcomeState};

pub mod room;
pub mod welcome;

type MatrixRoomInfo = Arc<(MatrixRoom, Option<Tags>)>;

const MEMBER_FETCH_DEBOUNCE: Duration = Duration::from_secs(5);

#[inline]
fn bold_style() -> Style {
    Style::default().add_modifier(StyleModifier::BOLD)
}

#[inline]
fn bold_span(s: &str) -> Span {
    Span::styled(s, bold_style())
}

#[inline]
fn bold_spans(s: &str) -> Line {
    bold_span(s).into()
}

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

/// Sort `Some` to be less than `None` so that list items with values come before those without.
#[inline]
fn some_cmp<T: Ord>(a: Option<T>, b: Option<T>) -> Ordering {
    match (a, b) {
        (Some(a), Some(b)) => a.cmp(&b),
        (None, None) => Ordering::Equal,
        (None, Some(_)) => Ordering::Greater,
        (Some(_), None) => Ordering::Less,
    }
}

fn user_cmp(a: &MemberItem, b: &MemberItem, field: &SortFieldUser) -> Ordering {
    let a_id = a.member.user_id();
    let b_id = b.member.user_id();

    match field {
        SortFieldUser::UserId => a_id.cmp(b_id),
        SortFieldUser::LocalPart => a_id.localpart().cmp(b_id.localpart()),
        SortFieldUser::Server => a_id.server_name().cmp(b_id.server_name()),
        SortFieldUser::PowerLevel => {
            // Sort higher power levels towards the top of the list.
            b.member.power_level().cmp(&a.member.power_level())
        },
    }
}

fn room_cmp<T: RoomLikeItem>(a: &T, b: &T, field: &SortFieldRoom) -> Ordering {
    match field {
        SortFieldRoom::Favorite => {
            let fava = a.has_tag(TagName::Favorite);
            let favb = b.has_tag(TagName::Favorite);

            // If a has Favorite and b doesn't, it should sort earlier in room list.
            favb.cmp(&fava)
        },
        SortFieldRoom::LowPriority => {
            let lowa = a.has_tag(TagName::LowPriority);
            let lowb = b.has_tag(TagName::LowPriority);

            // If a has LowPriority and b doesn't, it should sort later in room list.
            lowa.cmp(&lowb)
        },
        SortFieldRoom::Name => a.name().cmp(b.name()),
        SortFieldRoom::Alias => some_cmp(a.alias(), b.alias()),
        SortFieldRoom::RoomId => a.room_id().cmp(b.room_id()),
    }
}

/// Compare two rooms according the configured sort criteria.
fn room_fields_cmp<T: RoomLikeItem>(
    a: &T,
    b: &T,
    fields: &[SortColumn<SortFieldRoom>],
) -> Ordering {
    for SortColumn(field, order) in fields {
        match (room_cmp(a, b, field), order) {
            (Ordering::Equal, _) => continue,
            (o, SortOrder::Ascending) => return o,
            (o, SortOrder::Descending) => return o.reverse(),
        }
    }

    // Break ties on ascending room id.
    room_cmp(a, b, &SortFieldRoom::RoomId)
}

fn user_fields_cmp(
    a: &MemberItem,
    b: &MemberItem,
    fields: &[SortColumn<SortFieldUser>],
) -> Ordering {
    for SortColumn(field, order) in fields {
        match (user_cmp(a, b, field), order) {
            (Ordering::Equal, _) => continue,
            (o, SortOrder::Ascending) => return o,
            (o, SortOrder::Descending) => return o.reverse(),
        }
    }

    // Break ties on ascending user id.
    user_cmp(a, b, &SortFieldUser::UserId)
}

fn tag_to_span(tag: &TagName, style: Style) -> Vec<Span<'_>> {
    match tag {
        TagName::Favorite => vec![Span::styled("Favorite", style)],
        TagName::LowPriority => vec![Span::styled("Low Priority", style)],
        TagName::ServerNotice => vec![Span::styled("Server Notice", style)],
        TagName::User(tag) => {
            vec![
                Span::styled("User Tag: ", style),
                Span::styled(tag.as_ref(), style),
            ]
        },
        tag => vec![Span::styled(format!("{tag:?}"), style)],
    }
}

fn append_tags<'a>(tags: Vec<Vec<Span<'a>>>, spans: &mut Vec<Span<'a>>, style: Style) {
    if tags.is_empty() {
        return;
    }

    spans.push(Span::styled(" (", style));

    for (i, tag) in tags.into_iter().enumerate() {
        if i > 0 {
            spans.push(Span::styled(", ", style));
        }

        spans.extend(tag);
    }

    spans.push(Span::styled(")", style));
}

trait RoomLikeItem {
    fn room_id(&self) -> &RoomId;
    fn has_tag(&self, tag: TagName) -> bool;
    fn alias(&self) -> Option<&RoomAliasId>;
    fn name(&self) -> &str;
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
        PromptAction::Recall(..) => {
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
            IambWindow::MemberList($id, _, _) => $e,
            IambWindow::RoomList($id) => $e,
            IambWindow::SpaceList($id) => $e,
            IambWindow::VerifyList($id) => $e,
            IambWindow::Welcome($id) => $e,
            IambWindow::ChatList($id) => $e,
        }
    };
}

pub enum IambWindow {
    DirectList(DirectListState),
    MemberList(MemberListState, OwnedRoomId, Option<Instant>),
    Room(RoomState),
    VerifyList(VerifyListState),
    RoomList(RoomListState),
    SpaceList(SpaceListState),
    Welcome(WelcomeState),
    ChatList(ChatListState),
}

impl IambWindow {
    pub fn focus_toggle(&mut self) {
        if let IambWindow::Room(w) = self {
            w.focus_toggle()
        } else {
            return;
        }
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let IambWindow::Room(w) = self {
            w.message_command(act, ctx, store).await
        } else {
            return Err(IambError::NoSelectedRoom.into());
        }
    }

    pub async fn room_command(
        &mut self,
        act: RoomAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        if let IambWindow::Room(w) = self {
            w.room_command(act, ctx, store).await
        } else {
            return Err(IambError::NoSelectedRoomOrSpace.into());
        }
    }

    pub async fn send_command(
        &mut self,
        act: SendAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let IambWindow::Room(w) = self {
            w.send_command(act, ctx, store).await
        } else {
            return Err(IambError::NoSelectedRoom.into());
        }
    }
}

pub type DirectListState = ListState<DirectItem, IambInfo>;
pub type MemberListState = ListState<MemberItem, IambInfo>;
pub type RoomListState = ListState<RoomItem, IambInfo>;
pub type ChatListState = ListState<GenericChatItem, IambInfo>;
pub type SpaceListState = ListState<SpaceItem, IambInfo>;
pub type VerifyListState = ListState<VerifyItem, IambInfo>;

impl From<ChatListState> for IambWindow {
    fn from(list: ChatListState) -> Self {
        IambWindow::ChatList(list)
    }
}

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
        match self {
            IambWindow::Room(state) => state.draw(area, buf, focused, store),
            IambWindow::DirectList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .dms
                    .clone()
                    .into_iter()
                    .map(|room_info| DirectItem::new(room_info, store))
                    .collect::<Vec<_>>();
                let fields = &store.application.settings.tunables.sort.dms;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields));

                state.set(items);

                List::new(store)
                    .empty_message("No direct messages yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::MemberList(state, room_id, last_fetch) => {
                let need_fetch = match last_fetch {
                    Some(i) => i.elapsed() >= MEMBER_FETCH_DEBOUNCE,
                    None => true,
                };

                if need_fetch {
                    if let Ok(mems) = store.application.worker.members(room_id.clone()) {
                        let mut items = mems
                            .into_iter()
                            .map(|m| MemberItem::new(m, room_id.clone()))
                            .collect::<Vec<_>>();
                        let fields = &store.application.settings.tunables.sort.members;
                        items.sort_by(|a, b| user_fields_cmp(a, b, fields));
                        state.set(items);
                        *last_fetch = Some(Instant::now());
                    }
                }

                List::new(store)
                    .empty_message("No users here yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::RoomList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .rooms
                    .clone()
                    .into_iter()
                    .map(|room_info| RoomItem::new(room_info, store))
                    .collect::<Vec<_>>();
                let fields = &store.application.settings.tunables.sort.rooms;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields));

                state.set(items);

                List::new(store)
                    .empty_message("You haven't joined any rooms yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::ChatList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .rooms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, false))
                    .collect::<Vec<_>>();

                let dms = store
                    .application
                    .sync_info
                    .dms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, true));

                items.extend(dms);

                let fields = &store.application.settings.tunables.sort.chats;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields));

                state.set(items);

                List::new(store)
                    .empty_message("You do not have rooms or dms yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::SpaceList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .spaces
                    .clone()
                    .into_iter()
                    .map(|room| SpaceItem::new(room, store))
                    .collect::<Vec<_>>();
                let fields = &store.application.settings.tunables.sort.spaces;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields));

                state.set(items);

                List::new(store)
                    .empty_message("You haven't joined any spaces yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
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
                    .render(area, buf, state);
            },
            IambWindow::Welcome(state) => state.draw(area, buf, focused, store),
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            IambWindow::Room(w) => w.dup(store).into(),
            IambWindow::DirectList(w) => w.dup(store).into(),
            IambWindow::MemberList(w, room_id, last_fetch) => {
                IambWindow::MemberList(w.dup(store), room_id.clone(), *last_fetch)
            },
            IambWindow::RoomList(w) => w.dup(store).into(),
            IambWindow::SpaceList(w) => w.dup(store).into(),
            IambWindow::VerifyList(w) => w.dup(store).into(),
            IambWindow::Welcome(w) => w.dup(store).into(),
            IambWindow::ChatList(w) => w.dup(store).into(),
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

impl Window<IambInfo> for IambWindow {
    fn id(&self) -> IambId {
        match self {
            IambWindow::Room(room) => IambId::Room(room.id().to_owned()),
            IambWindow::DirectList(_) => IambId::DirectList,
            IambWindow::MemberList(_, room_id, _) => IambId::MemberList(room_id.clone()),
            IambWindow::RoomList(_) => IambId::RoomList,
            IambWindow::SpaceList(_) => IambId::SpaceList,
            IambWindow::VerifyList(_) => IambId::VerifyList,
            IambWindow::Welcome(_) => IambId::Welcome,
            IambWindow::ChatList(_) => IambId::ChatList,
        }
    }

    fn get_tab_title(&self, store: &mut ProgramStore) -> Line {
        match self {
            IambWindow::DirectList(_) => bold_spans("Direct Messages"),
            IambWindow::RoomList(_) => bold_spans("Rooms"),
            IambWindow::SpaceList(_) => bold_spans("Spaces"),
            IambWindow::VerifyList(_) => bold_spans("Verifications"),
            IambWindow::Welcome(_) => bold_spans("Welcome to iamb"),
            IambWindow::ChatList(_) => bold_spans("DMs & Rooms"),

            IambWindow::Room(w) => {
                let title = store.application.get_room_title(w.id());

                Line::from(title)
            },
            IambWindow::MemberList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    bold_span("Room Members "),
                    Span::styled(format!("({n}): "), bold_style()),
                    title.into(),
                ];
                Line::from(v)
            },
        }
    }

    fn get_win_title(&self, store: &mut ProgramStore) -> Line {
        match self {
            IambWindow::DirectList(_) => bold_spans("Direct Messages"),
            IambWindow::RoomList(_) => bold_spans("Rooms"),
            IambWindow::SpaceList(_) => bold_spans("Spaces"),
            IambWindow::VerifyList(_) => bold_spans("Verifications"),
            IambWindow::Welcome(_) => bold_spans("Welcome to iamb"),
            IambWindow::ChatList(_) => bold_spans("DMs & Rooms"),

            IambWindow::Room(w) => w.get_title(store),
            IambWindow::MemberList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    bold_span("Room Members "),
                    Span::styled(format!("({n}): "), bold_style()),
                    title.into(),
                ];
                Line::from(v)
            },
        }
    }

    fn open(id: IambId, store: &mut ProgramStore) -> IambResult<Self> {
        match id {
            IambId::Room(room_id) => {
                let (room, name, tags) = store.application.worker.get_room(room_id)?;
                let room = RoomState::new(room, name, tags, store);

                store.application.need_load.insert(room.id().to_owned(), Need::MEMBERS);
                return Ok(room.into());
            },
            IambId::DirectList => {
                let list = DirectListState::new(IambBufferId::DirectList, vec![]);

                return Ok(list.into());
            },
            IambId::MemberList(room_id) => {
                let id = IambBufferId::MemberList(room_id.clone());
                let list = MemberListState::new(id, vec![]);
                let win = IambWindow::MemberList(list, room_id, None);

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
            IambId::ChatList => {
                let list = ChatListState::new(IambBufferId::ChatList, vec![]);

                Ok(list.into())
            },
        }
    }

    fn find(name: String, store: &mut ProgramStore) -> IambResult<Self> {
        let ChatStore { names, worker, .. } = &mut store.application;

        if let Some(room) = names.get_mut(&name) {
            let id = IambId::Room(room.clone());

            IambWindow::open(id, store)
        } else {
            let room_id = worker.join_room(name.clone())?;
            names.insert(name, room_id.clone());

            let (room, name, tags) = store.application.worker.get_room(room_id)?;
            let room = RoomState::new(room, name, tags, store);

            store.application.need_load.insert(room.id().to_owned(), Need::MEMBERS);
            Ok(room.into())
        }
    }

    fn posn(index: usize, _: &mut ProgramStore) -> IambResult<Self> {
        let msg = format!("Cannot find indexed buffer (index = {index})");
        let err = UIError::Unimplemented(msg);

        Err(err)
    }

    fn unnamed(store: &mut ProgramStore) -> IambResult<Self> {
        Self::open(IambId::RoomList, store)
    }
}

#[derive(Clone)]
pub struct GenericChatItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
    is_dm: bool,
}

impl GenericChatItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore, is_dm: bool) -> Self {
        let room = &room_info.deref().0;
        let room_id = room.room_id();

        let info = store.application.get_room_info(room_id.to_owned());

        let name = info.name.clone().unwrap_or_default();
        let alias = room.canonical_alias();
        info.tags = room_info.deref().1.clone();

        if let Some(alias) = &alias {
            store.application.names.insert(alias.to_string(), room_id.to_owned());
        }

        GenericChatItem { room_info, name, alias, is_dm }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }

    #[inline]
    fn tags(&self) -> &Option<Tags> {
        &self.room_info.deref().1
    }
}

impl RoomLikeItem for GenericChatItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    fn has_tag(&self, tag: TagName) -> bool {
        if let Some(tags) = &self.room_info.deref().1 {
            tags.contains_key(&tag)
        } else {
            false
        }
    }
}

impl ToString for GenericChatItem {
    fn to_string(&self) -> String {
        return self.name.clone();
    }
}

impl ListItem<IambInfo> for GenericChatItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        let style = selected_style(selected);
        let mut spans = vec![Span::styled(self.name.as_str(), style)];
        let mut labels = if self.is_dm {
            vec![vec![Span::styled("DM", style)]]
        } else {
            vec![vec![Span::styled("Room", style)]]
        };

        if let Some(tags) = &self.tags() {
            labels.extend(tags.keys().map(|t| tag_to_span(t, style)));
        }

        append_tags(labels, &mut spans, style);
        Text::from(Line::from(spans))
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for GenericChatItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room_id(), act, ctx)
    }
}

#[derive(Clone)]
pub struct RoomItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
}

impl RoomItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore) -> Self {
        let room = &room_info.deref().0;
        let room_id = room.room_id();

        let info = store.application.get_room_info(room_id.to_owned());
        let name = info.name.clone().unwrap_or_default();
        let alias = room.canonical_alias();
        info.tags = room_info.deref().1.clone();

        if let Some(alias) = &alias {
            store.application.names.insert(alias.to_string(), room_id.to_owned());
        }

        RoomItem { room_info, name, alias }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }

    #[inline]
    fn tags(&self) -> &Option<Tags> {
        &self.room_info.deref().1
    }
}

impl RoomLikeItem for RoomItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    fn has_tag(&self, tag: TagName) -> bool {
        if let Some(tags) = &self.room_info.deref().1 {
            tags.contains_key(&tag)
        } else {
            false
        }
    }
}

impl ToString for RoomItem {
    fn to_string(&self) -> String {
        return self.name.clone();
    }
}

impl ListItem<IambInfo> for RoomItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        if let Some(tags) = &self.tags() {
            let style = selected_style(selected);
            let mut spans = vec![Span::styled(self.name.as_str(), style)];
            let tags = tags.keys().map(|t| tag_to_span(t, style)).collect();

            append_tags(tags, &mut spans, style);

            Text::from(Line::from(spans))
        } else {
            selected_text(self.name.as_str(), selected)
        }
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for RoomItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room_id(), act, ctx)
    }
}

#[derive(Clone)]
pub struct DirectItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
}

impl DirectItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore) -> Self {
        let room_id = room_info.0.room_id().to_owned();
        let name = store.application.get_room_info(room_id).name.clone().unwrap_or_default();
        let alias = room_info.0.canonical_alias();

        DirectItem { room_info, name, alias }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }

    #[inline]
    fn tags(&self) -> &Option<Tags> {
        &self.room_info.deref().1
    }
}

impl RoomLikeItem for DirectItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn has_tag(&self, tag: TagName) -> bool {
        if let Some(tags) = &self.room_info.deref().1 {
            tags.contains_key(&tag)
        } else {
            false
        }
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }
}

impl ToString for DirectItem {
    fn to_string(&self) -> String {
        return self.name.clone();
    }
}

impl ListItem<IambInfo> for DirectItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        if let Some(tags) = &self.tags() {
            let style = selected_style(selected);
            let mut spans = vec![Span::styled(self.name.as_str(), style)];
            let tags = tags.keys().map(|t| tag_to_span(t, style)).collect();

            append_tags(tags, &mut spans, style);

            Text::from(Line::from(spans))
        } else {
            selected_text(self.name.as_str(), selected)
        }
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for DirectItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room_id(), act, ctx)
    }
}

#[derive(Clone)]
pub struct SpaceItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
}

impl SpaceItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore) -> Self {
        let room_id = room_info.0.room_id();
        let name = store
            .application
            .get_room_info(room_id.to_owned())
            .name
            .clone()
            .unwrap_or_default();
        let alias = room_info.0.canonical_alias();

        if let Some(alias) = &alias {
            store.application.names.insert(alias.to_string(), room_id.to_owned());
        }

        SpaceItem { room_info, name, alias }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }
}

impl RoomLikeItem for SpaceItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn has_tag(&self, _: TagName) -> bool {
        // I think that spaces can technically have tags, but afaik no client
        // exposes them, so we'll just always return false here for now.
        false
    }
}

impl ToString for SpaceItem {
    fn to_string(&self) -> String {
        return self.room_id().to_string();
    }
}

impl ListItem<IambInfo> for SpaceItem {
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut ProgramStore) -> Text {
        selected_text(self.name.as_str(), selected)
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for SpaceItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room_id(), act, ctx)
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
                format!("Device verification with {display_name} ({state})")
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
        Some(self.cmp(other))
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
        lines.push(Line::from(item));

        if self.sasv1.is_done() {
            // Print nothing.
        } else if self.sasv1.is_cancelled() {
            if let Some(info) = self.sasv1.cancel_info() {
                lines.push(Line::from(format!("    Cancelled: {}", info.reason())));
                lines.push(Line::from(""));
            }

            lines.push(Line::from("    You can start a new verification request with:"));
        } else if let Some(emoji) = self.sasv1.emoji() {
            lines.push(Line::from(
                "    Both devices should see the following Emoji sequence:".to_string(),
            ));
            lines.push(Line::from(""));

            for line in format_emojis(emoji).lines() {
                lines.push(Line::from(format!("    {line}")));
            }

            lines.push(Line::from(""));
            lines.push(Line::from("    If they don't match, run:"));
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                format!(":verify mismatch {}", self.user_dev),
                bold,
            )));
            lines.push(Line::from(""));
            lines.push(Line::from("    If everything looks right, you can confirm with:"));
        } else {
            lines.push(Line::from("    To accept this request, run:"));
        }

        let cmd = self.to_string();

        if !cmd.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::from(vec![Span::from("        "), Span::styled(cmd, bold)]));
            lines.push(Line::from(""));
            lines.push(Line::from(vec![
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
            PromptAction::Recall(..) => {
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
    room_id: OwnedRoomId,
}

impl MemberItem {
    fn new(member: RoomMember, room_id: OwnedRoomId) -> Self {
        Self { member, room_id }
    }
}

impl ToString for MemberItem {
    fn to_string(&self) -> String {
        self.member.user_id().to_string()
    }
}

impl ListItem<IambInfo> for MemberItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        store: &mut ProgramStore,
    ) -> Text {
        let info = store.application.rooms.get_or_default(self.room_id.clone());
        let user_id = self.member.user_id();

        let (color, name) = store.application.settings.get_user_overrides(self.member.user_id());
        let color = color.unwrap_or_else(|| super::config::user_color(user_id.as_str()));
        let mut style = super::config::user_style_from_color(color);

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED);
        }

        let mut spans = vec![];
        let mut parens = false;

        if let Some(name) = name {
            spans.push(Span::styled(name, style));
            parens = true;
        } else if let Some(display) = info.display_names.get(user_id) {
            spans.push(Span::styled(display.clone(), style));
            parens = true;
        }

        spans.extend(parens.then_some(Span::styled(" (", style)));
        spans.push(Span::styled(user_id.as_str(), style));
        spans.extend(parens.then_some(Span::styled(")", style)));

        let state = match self.member.membership() {
            MembershipState::Ban => Span::raw(" (banned)").into(),
            MembershipState::Invite => Span::raw(" (invited)").into(),
            MembershipState::Knock => Span::raw(" (wants to join)").into(),
            MembershipState::Leave => Span::raw(" (left)").into(),
            MembershipState::Join => None,
            _ => None,
        };

        spans.extend(state);

        return Line::from(spans).into();
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
            PromptAction::Recall(..) => {
                let msg = "Cannot recall history inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            _ => Err(EditError::Unimplemented("unknown prompt action".to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use matrix_sdk::ruma::{room_alias_id, server_name};

    #[derive(Debug, Eq, PartialEq)]
    struct TestRoomItem {
        room_id: OwnedRoomId,
        tags: Vec<TagName>,
        alias: Option<OwnedRoomAliasId>,
        name: &'static str,
    }

    impl RoomLikeItem for &TestRoomItem {
        fn room_id(&self) -> &RoomId {
            self.room_id.as_ref()
        }

        fn has_tag(&self, tag: TagName) -> bool {
            self.tags.contains(&tag)
        }

        fn alias(&self) -> Option<&RoomAliasId> {
            self.alias.as_deref()
        }

        fn name(&self) -> &str {
            self.name
        }
    }

    #[test]
    fn test_sort_rooms() {
        let server = server_name!("example.com");

        let room1 = TestRoomItem {
            room_id: RoomId::new(server).to_owned(),
            tags: vec![TagName::Favorite],
            alias: Some(room_alias_id!("#room1:example.com").to_owned()),
            name: "Z",
        };

        let room2 = TestRoomItem {
            room_id: RoomId::new(server).to_owned(),
            tags: vec![],
            alias: Some(room_alias_id!("#a:example.com").to_owned()),
            name: "Unnamed Room",
        };

        let room3 = TestRoomItem {
            room_id: RoomId::new(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "Cool Room",
        };

        // Sort by Name ascending.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[SortColumn(SortFieldRoom::Name, SortOrder::Ascending)];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields));
        assert_eq!(rooms, vec![&room3, &room2, &room1]);

        // Sort by Name descending.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[SortColumn(SortFieldRoom::Name, SortOrder::Descending)];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields));
        assert_eq!(rooms, vec![&room1, &room2, &room3]);

        // Sort by Favorite and Alias before Name to show order matters.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[
            SortColumn(SortFieldRoom::Favorite, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Alias, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields));
        assert_eq!(rooms, vec![&room1, &room2, &room3]);

        // Now flip order of Favorite with Descending
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[
            SortColumn(SortFieldRoom::Favorite, SortOrder::Descending),
            SortColumn(SortFieldRoom::Alias, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields));
        assert_eq!(rooms, vec![&room2, &room3, &room1]);
    }
}
