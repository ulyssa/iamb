use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::str::FromStr;
use std::sync::Arc;
use std::time::{Duration, Instant};

use emojis::Emoji;
use tokio::sync::Mutex as AsyncMutex;
use tracing::warn;

use matrix_sdk::{
    encryption::verification::SasVerification,
    room::Joined,
    ruma::{
        events::{
            reaction::ReactionEvent,
            room::message::{
                OriginalRoomMessageEvent,
                Relation,
                Replacement,
                RoomMessageEvent,
                RoomMessageEventContent,
            },
            tag::{TagName, Tags},
            AnyMessageLikeEvent,
            MessageLikeEvent,
        },
        presence::PresenceState,
        EventId,
        OwnedEventId,
        OwnedRoomId,
        OwnedUserId,
        RoomId,
    },
};

use modalkit::{
    editing::{
        action::{Action, UIError, UIResult},
        application::{
            ApplicationAction,
            ApplicationContentId,
            ApplicationError,
            ApplicationInfo,
            ApplicationStore,
            ApplicationWindowId,
        },
        base::{CommandType, WordStyle},
        completion::{complete_path, CompletionMap},
        context::EditContext,
        cursor::Cursor,
        rope::EditRope,
        store::Store,
    },
    env::vim::{
        command::{CommandContext, CommandDescription, VimCommand, VimCommandMachine},
        keybindings::VimMachine,
        VimContext,
    },
    input::bindings::SequenceStatus,
    input::key::TerminalKey,
    tui::{
        buffer::Buffer,
        layout::{Alignment, Rect},
        text::{Span, Spans},
        widgets::{Paragraph, Widget},
    },
};

use crate::{
    message::{Message, MessageEvent, MessageKey, MessageTimeStamp, Messages},
    worker::Requester,
    ApplicationSettings,
};

pub const MATRIX_ID_WORD: WordStyle = WordStyle::CharSet(is_mxid_char);

/// Find the boundaries for a Matrix username, room alias, or room ID.
///
/// Technically "[" and "]" should be here since IPv6 addresses are allowed
/// in the server name, but in practice that should be uncommon, and people
/// can just use `gf` and friends in Visual mode instead.
fn is_mxid_char(c: char) -> bool {
    return c >= 'a' && c <= 'z' ||
        c >= 'A' && c <= 'Z' ||
        c >= '0' && c <= '9' ||
        ":-./@_#!".contains(c);
}

const ROOM_FETCH_DEBOUNCE: Duration = Duration::from_secs(2);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambInfo {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VerifyAction {
    Accept,
    Cancel,
    Confirm,
    Mismatch,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MessageAction {
    /// Cance the current reply or edit.
    Cancel,

    /// Download an attachment to the given path.
    ///
    /// The second argument controls whether to overwrite any already existing file at the
    /// destination path, or to open the attachment after downloading.
    Download(Option<String>, DownloadFlags),

    /// Edit a sent message.
    Edit,

    /// React to a message with an Emoji.
    React(String),

    /// Redact a message, with an optional reason.
    Redact(Option<String>),

    /// Reply to a message.
    Reply,

    /// Unreact to a message.
    ///
    /// If no specific Emoji to remove to is specified, then all reactions from the user on the
    /// message are removed.
    Unreact(Option<String>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CreateRoomType {
    /// A direct message room.
    Direct(OwnedUserId),

    /// A standard chat room.
    Room,

    /// A Matrix space.
    Space,
}

bitflags::bitflags! {
    pub struct CreateRoomFlags: u32 {
        const NONE = 0b00000000;

        /// Make the room public.
        const PUBLIC = 0b00000001;

        /// Encrypt this room.
        const ENCRYPTED = 0b00000010;
    }
}

bitflags::bitflags! {
    pub struct DownloadFlags: u32 {
        const NONE = 0b00000000;

        /// Overwrite file if it already exists.
        const FORCE = 0b00000001;

        /// Open file after downloading.
        const OPEN = 0b00000010;
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RoomField {
    Name,
    Tag(TagName),
    Topic,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RoomAction {
    InviteAccept,
    InviteReject,
    InviteSend(OwnedUserId),
    Members(Box<CommandContext<ProgramContext>>),
    Set(RoomField, String),
    Unset(RoomField),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SendAction {
    Submit,
    Upload(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HomeserverAction {
    CreateRoom(Option<String>, CreateRoomType, CreateRoomFlags),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambAction {
    Homeserver(HomeserverAction),
    Message(MessageAction),
    Room(RoomAction),
    Send(SendAction),
    Verify(VerifyAction, String),
    VerifyRequest(String),
    ToggleScrollbackFocus,
}

impl From<HomeserverAction> for IambAction {
    fn from(act: HomeserverAction) -> Self {
        IambAction::Homeserver(act)
    }
}

impl From<MessageAction> for IambAction {
    fn from(act: MessageAction) -> Self {
        IambAction::Message(act)
    }
}

impl From<RoomAction> for IambAction {
    fn from(act: RoomAction) -> Self {
        IambAction::Room(act)
    }
}

impl From<SendAction> for IambAction {
    fn from(act: SendAction) -> Self {
        IambAction::Send(act)
    }
}

impl ApplicationAction for IambAction {
    fn is_edit_sequence<C: EditContext>(&self, _: &C) -> SequenceStatus {
        match self {
            IambAction::Homeserver(..) => SequenceStatus::Break,
            IambAction::Message(..) => SequenceStatus::Break,
            IambAction::Room(..) => SequenceStatus::Break,
            IambAction::Send(..) => SequenceStatus::Break,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Break,
            IambAction::Verify(..) => SequenceStatus::Break,
            IambAction::VerifyRequest(..) => SequenceStatus::Break,
        }
    }

    fn is_last_action<C: EditContext>(&self, _: &C) -> SequenceStatus {
        match self {
            IambAction::Homeserver(..) => SequenceStatus::Atom,
            IambAction::Message(..) => SequenceStatus::Atom,
            IambAction::Room(..) => SequenceStatus::Atom,
            IambAction::Send(..) => SequenceStatus::Atom,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Atom,
            IambAction::Verify(..) => SequenceStatus::Atom,
            IambAction::VerifyRequest(..) => SequenceStatus::Atom,
        }
    }

    fn is_last_selection<C: EditContext>(&self, _: &C) -> SequenceStatus {
        match self {
            IambAction::Homeserver(..) => SequenceStatus::Ignore,
            IambAction::Message(..) => SequenceStatus::Ignore,
            IambAction::Room(..) => SequenceStatus::Ignore,
            IambAction::Send(..) => SequenceStatus::Ignore,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Ignore,
            IambAction::Verify(..) => SequenceStatus::Ignore,
            IambAction::VerifyRequest(..) => SequenceStatus::Ignore,
        }
    }

    fn is_switchable<C: EditContext>(&self, _: &C) -> bool {
        match self {
            IambAction::Homeserver(..) => false,
            IambAction::Message(..) => false,
            IambAction::Room(..) => false,
            IambAction::Send(..) => false,
            IambAction::ToggleScrollbackFocus => false,
            IambAction::Verify(..) => false,
            IambAction::VerifyRequest(..) => false,
        }
    }
}

impl From<RoomAction> for ProgramAction {
    fn from(act: RoomAction) -> Self {
        IambAction::from(act).into()
    }
}

impl From<IambAction> for ProgramAction {
    fn from(act: IambAction) -> Self {
        Action::Application(act)
    }
}

pub type ProgramAction = Action<IambInfo>;
pub type ProgramContext = VimContext<IambInfo>;
pub type Keybindings = VimMachine<TerminalKey, IambInfo>;
pub type ProgramCommand = VimCommand<ProgramContext, IambInfo>;
pub type ProgramCommands = VimCommandMachine<ProgramContext, IambInfo>;
pub type ProgramStore = Store<IambInfo>;
pub type AsyncProgramStore = Arc<AsyncMutex<ProgramStore>>;

pub type IambResult<T> = UIResult<T, IambInfo>;

/// Reaction events for some message.
///
/// The event identifier used as a key here is the ID for the reaction, and not for the message
/// it's reacting to.
pub type MessageReactions = HashMap<OwnedEventId, (String, OwnedUserId)>;

pub type Receipts = HashMap<OwnedEventId, Vec<OwnedUserId>>;

#[derive(thiserror::Error, Debug)]
pub enum IambError {
    #[error("Invalid user identifier: {0}")]
    InvalidUserId(String),

    #[error("Invalid verification user/device pair: {0}")]
    InvalidVerificationId(String),

    #[error("Cryptographic storage error: {0}")]
    CryptoStore(#[from] matrix_sdk::encryption::CryptoStoreError),

    #[error("HTTP client error: {0}")]
    Http(#[from] matrix_sdk::HttpError),

    #[error("Matrix client error: {0}")]
    Matrix(#[from] matrix_sdk::Error),

    #[error("Matrix client storage error: {0}")]
    Store(#[from] matrix_sdk::StoreError),

    #[error("Serialization/deserialization error: {0}")]
    Serde(#[from] serde_json::Error),

    #[error("Selected message does not have any attachments")]
    NoAttachment,

    #[error("No message currently selected")]
    NoSelectedMessage,

    #[error("Current window is not a room or space")]
    NoSelectedRoomOrSpace,

    #[error("Current window is not a room")]
    NoSelectedRoom,

    #[error("You do not have a current invitation to this room")]
    NotInvited,

    #[error("You need to join the room before you can do that")]
    NotJoined,

    #[error("Unknown room identifier: {0}")]
    UnknownRoom(OwnedRoomId),

    #[error("Verification request error: {0}")]
    VerificationRequestError(#[from] matrix_sdk::encryption::identities::RequestVerificationError),
}

impl From<IambError> for UIError<IambInfo> {
    fn from(err: IambError) -> Self {
        UIError::Application(err)
    }
}

impl ApplicationError for IambError {}

#[derive(Default)]
pub enum RoomFetchStatus {
    Done,
    HaveMore(String),
    #[default]
    NotStarted,
}

pub enum EventLocation {
    Message(MessageKey),
    Reaction(OwnedEventId),
}

impl EventLocation {
    fn to_message_key(&self) -> Option<&MessageKey> {
        if let EventLocation::Message(key) = self {
            Some(key)
        } else {
            None
        }
    }
}

#[derive(Default)]
pub struct RoomInfo {
    /// The display name for this room.
    pub name: Option<String>,

    /// The tags placed on this room.
    pub tags: Option<Tags>,

    /// A map of event IDs to where they are stored in this struct.
    pub keys: HashMap<OwnedEventId, EventLocation>,

    /// The messages loaded for this room.
    pub messages: Messages,

    /// A map of read markers to display on different events.
    pub receipts: HashMap<OwnedEventId, Vec<OwnedUserId>>,

    /// An event ID for where we should indicate we've read up to.
    pub read_till: Option<OwnedEventId>,

    /// A map of message identifiers to a map of reaction events.
    pub reactions: HashMap<OwnedEventId, MessageReactions>,

    /// Where to continue fetching from when we continue loading scrollback history.
    pub fetch_id: RoomFetchStatus,

    /// The time that we last fetched scrollback for this room.
    pub fetch_last: Option<Instant>,

    /// Users currently typing in this room, and when we received notification of them doing so.
    pub users_typing: Option<(Instant, Vec<OwnedUserId>)>,
}

impl RoomInfo {
    pub fn get_reactions(&self, event_id: &EventId) -> Vec<(&str, usize)> {
        if let Some(reacts) = self.reactions.get(event_id) {
            let mut counts = HashMap::new();

            for (key, _) in reacts.values() {
                let count = counts.entry(key.as_str()).or_default();
                *count += 1;
            }

            let mut reactions = counts.into_iter().collect::<Vec<_>>();
            reactions.sort();

            reactions
        } else {
            vec![]
        }
    }

    pub fn get_event(&self, event_id: &EventId) -> Option<&Message> {
        self.messages.get(self.keys.get(event_id)?.to_message_key()?)
    }

    pub fn insert_reaction(&mut self, react: ReactionEvent) {
        match react {
            MessageLikeEvent::Original(react) => {
                let rel_id = react.content.relates_to.event_id;
                let key = react.content.relates_to.key;

                let message = self.reactions.entry(rel_id.clone()).or_default();
                let event_id = react.event_id;
                let user_id = react.sender;

                message.insert(event_id.clone(), (key, user_id));

                let loc = EventLocation::Reaction(rel_id);
                self.keys.insert(event_id, loc);
            },
            MessageLikeEvent::Redacted(_) => {
                return;
            },
        }
    }

    pub fn insert_edit(&mut self, msg: Replacement) {
        let event_id = msg.event_id;
        let new_content = msg.new_content;

        let key = if let Some(EventLocation::Message(k)) = self.keys.get(&event_id) {
            k
        } else {
            return;
        };

        let msg = if let Some(msg) = self.messages.get_mut(key) {
            msg
        } else {
            return;
        };

        match &mut msg.event {
            MessageEvent::Original(orig) => {
                orig.content = *new_content;
            },
            MessageEvent::Local(_, content) => {
                *content = new_content;
            },
            MessageEvent::Redacted(_) => {
                return;
            },
        }
    }

    pub fn insert_message(&mut self, msg: RoomMessageEvent) {
        let event_id = msg.event_id().to_owned();
        let key = (msg.origin_server_ts().into(), event_id.clone());

        self.keys.insert(event_id.clone(), EventLocation::Message(key.clone()));
        self.messages.insert(key, msg.into());

        // Remove any echo.
        let key = (MessageTimeStamp::LocalEcho, event_id);
        let _ = self.messages.remove(&key);
    }

    pub fn insert(&mut self, msg: RoomMessageEvent) {
        match msg {
            RoomMessageEvent::Original(OriginalRoomMessageEvent {
                content:
                    RoomMessageEventContent { relates_to: Some(Relation::Replacement(repl)), .. },
                ..
            }) => self.insert_edit(repl),
            _ => self.insert_message(msg),
        }
    }

    fn recently_fetched(&self) -> bool {
        self.fetch_last.map_or(false, |i| i.elapsed() < ROOM_FETCH_DEBOUNCE)
    }

    fn get_typers(&self) -> &[OwnedUserId] {
        if let Some((t, users)) = &self.users_typing {
            if t.elapsed() < Duration::from_secs(4) {
                return users.as_ref();
            } else {
                return &[];
            }
        } else {
            return &[];
        }
    }

    fn get_typing_spans<'a>(&'a self, settings: &'a ApplicationSettings) -> Spans<'a> {
        let typers = self.get_typers();
        let n = typers.len();

        match n {
            0 => Spans(vec![]),
            1 => {
                let user = settings.get_user_span(typers[0].as_ref());

                Spans(vec![user, Span::from(" is typing...")])
            },
            2 => {
                let user1 = settings.get_user_span(typers[0].as_ref());
                let user2 = settings.get_user_span(typers[1].as_ref());

                Spans(vec![
                    user1,
                    Span::raw(" and "),
                    user2,
                    Span::from(" are typing..."),
                ])
            },
            n if n < 5 => Spans::from("Several people are typing..."),
            _ => Spans::from("Many people are typing..."),
        }
    }

    pub fn set_typing(&mut self, user_ids: Vec<OwnedUserId>) {
        self.users_typing = (Instant::now(), user_ids).into();
    }

    pub fn render_typing(
        &mut self,
        area: Rect,
        buf: &mut Buffer,
        settings: &ApplicationSettings,
    ) -> Rect {
        if area.height <= 2 || area.width <= 20 {
            return area;
        }

        if !settings.tunables.typing_notice_display {
            return area;
        }

        let top = Rect::new(area.x, area.y, area.width, area.height - 1);
        let bar = Rect::new(area.x, area.y + top.height, area.width, 1);

        Paragraph::new(self.get_typing_spans(settings))
            .alignment(Alignment::Center)
            .render(bar, buf);

        return top;
    }
}

fn emoji_map() -> CompletionMap<String, &'static Emoji> {
    let mut emojis = CompletionMap::default();

    for emoji in emojis::iter() {
        for shortcode in emoji.shortcodes() {
            emojis.insert(shortcode.to_string(), emoji);
        }
    }

    return emojis;
}

pub struct ChatStore {
    pub cmds: ProgramCommands,
    pub worker: Requester,
    pub rooms: CompletionMap<OwnedRoomId, RoomInfo>,
    pub names: CompletionMap<String, OwnedRoomId>,
    pub presences: CompletionMap<OwnedUserId, PresenceState>,
    pub verifications: HashMap<String, SasVerification>,
    pub settings: ApplicationSettings,
    pub need_load: HashSet<OwnedRoomId>,
    pub emojis: CompletionMap<String, &'static Emoji>,
}

impl ChatStore {
    pub fn new(worker: Requester, settings: ApplicationSettings) -> Self {
        ChatStore {
            worker,
            settings,

            cmds: crate::commands::setup_commands(),
            names: Default::default(),
            rooms: Default::default(),
            presences: Default::default(),
            verifications: Default::default(),
            need_load: Default::default(),
            emojis: emoji_map(),
        }
    }

    pub fn get_joined_room(&self, room_id: &RoomId) -> Option<Joined> {
        self.worker.client.get_joined_room(room_id)
    }

    pub fn get_room_title(&self, room_id: &RoomId) -> String {
        self.rooms
            .get(room_id)
            .and_then(|i| i.name.as_ref())
            .map(String::from)
            .unwrap_or_else(|| "Untitled Matrix Room".to_string())
    }

    pub async fn set_receipts(&mut self, receipts: Vec<(OwnedRoomId, Receipts)>) {
        let mut updates = vec![];

        for (room_id, receipts) in receipts.into_iter() {
            if let Some(info) = self.rooms.get_mut(&room_id) {
                info.receipts = receipts;

                if let Some(read_till) = info.read_till.take() {
                    updates.push((room_id, read_till));
                }
            }
        }

        for (room_id, read_till) in updates.into_iter() {
            if let Some(room) = self.worker.client.get_joined_room(&room_id) {
                let _ = room.read_receipt(read_till.as_ref()).await;
            }
        }
    }

    pub fn mark_for_load(&mut self, room_id: OwnedRoomId) {
        self.need_load.insert(room_id);
    }

    pub fn load_older(&mut self, limit: u32) {
        let ChatStore { need_load, presences, rooms, worker, .. } = self;

        for room_id in std::mem::take(need_load).into_iter() {
            let info = rooms.get_or_default(room_id.clone());

            if info.recently_fetched() {
                need_load.insert(room_id);
                continue;
            } else {
                info.fetch_last = Instant::now().into();
            }

            let fetch_id = match &info.fetch_id {
                RoomFetchStatus::Done => continue,
                RoomFetchStatus::HaveMore(fetch_id) => Some(fetch_id.clone()),
                RoomFetchStatus::NotStarted => None,
            };

            let res = worker.load_older(room_id.clone(), fetch_id, limit);

            match res {
                Ok((fetch_id, msgs)) => {
                    for msg in msgs.into_iter() {
                        let sender = msg.sender().to_owned();
                        let _ = presences.get_or_default(sender);

                        match msg {
                            AnyMessageLikeEvent::RoomMessage(msg) => {
                                info.insert(msg);
                            },
                            AnyMessageLikeEvent::Reaction(ev) => {
                                info.insert_reaction(ev);
                            },
                            _ => continue,
                        }
                    }

                    info.fetch_id =
                        fetch_id.map_or(RoomFetchStatus::Done, RoomFetchStatus::HaveMore);
                },
                Err(e) => {
                    warn!(
                        room_id = room_id.as_str(),
                        err = e.to_string(),
                        "Failed to load older messages"
                    );

                    // Wait and try again.
                    need_load.insert(room_id);
                },
            }
        }
    }

    pub fn get_room_info(&mut self, room_id: OwnedRoomId) -> &mut RoomInfo {
        self.rooms.get_or_default(room_id)
    }

    pub fn set_room_name(&mut self, room_id: &RoomId, name: &str) {
        self.rooms.get_or_default(room_id.to_owned()).name = name.to_string().into();
    }

    pub fn insert_sas(&mut self, sas: SasVerification) {
        let key = format!("{}/{}", sas.other_user_id(), sas.other_device().device_id());

        self.verifications.insert(key, sas);
    }
}

impl ApplicationStore for ChatStore {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IambId {
    Room(OwnedRoomId),
    DirectList,
    MemberList(OwnedRoomId),
    RoomList,
    SpaceList,
    VerifyList,
    Welcome,
}

impl ApplicationWindowId for IambId {}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RoomFocus {
    Scrollback,
    MessageBar,
}

impl RoomFocus {
    pub fn is_scrollback(&self) -> bool {
        matches!(self, RoomFocus::Scrollback)
    }

    pub fn is_msgbar(&self) -> bool {
        matches!(self, RoomFocus::MessageBar)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IambBufferId {
    Command(CommandType),
    Room(OwnedRoomId, RoomFocus),
    DirectList,
    MemberList(OwnedRoomId),
    RoomList,
    SpaceList,
    VerifyList,
    Welcome,
}

impl IambBufferId {
    pub fn to_window(&self) -> Option<IambId> {
        match self {
            IambBufferId::Command(_) => None,
            IambBufferId::Room(room, _) => Some(IambId::Room(room.clone())),
            IambBufferId::DirectList => Some(IambId::DirectList),
            IambBufferId::MemberList(room) => Some(IambId::MemberList(room.clone())),
            IambBufferId::RoomList => Some(IambId::RoomList),
            IambBufferId::SpaceList => Some(IambId::SpaceList),
            IambBufferId::VerifyList => Some(IambId::VerifyList),
            IambBufferId::Welcome => Some(IambId::Welcome),
        }
    }
}

impl ApplicationContentId for IambBufferId {}

impl ApplicationInfo for IambInfo {
    type Error = IambError;
    type Store = ChatStore;
    type Action = IambAction;
    type WindowId = IambId;
    type ContentId = IambBufferId;

    fn complete(
        text: &EditRope,
        cursor: &mut Cursor,
        content: &IambBufferId,
        store: &mut ProgramStore,
    ) -> Vec<String> {
        match content {
            IambBufferId::Command(CommandType::Command) => complete_cmdbar(text, cursor, store),
            IambBufferId::Command(CommandType::Search) => vec![],

            IambBufferId::Room(_, RoomFocus::MessageBar) => {
                complete_matrix_names(text, cursor, store)
            },
            IambBufferId::Room(_, RoomFocus::Scrollback) => vec![],

            IambBufferId::DirectList => vec![],
            IambBufferId::MemberList(_) => vec![],
            IambBufferId::RoomList => vec![],
            IambBufferId::SpaceList => vec![],
            IambBufferId::VerifyList => vec![],
            IambBufferId::Welcome => vec![],
        }
    }

    fn content_of_command(ct: CommandType) -> IambBufferId {
        IambBufferId::Command(ct)
    }
}

fn complete_users(text: &EditRope, cursor: &mut Cursor, store: &ProgramStore) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    store
        .application
        .presences
        .complete(id.as_ref())
        .into_iter()
        .map(|i| i.to_string())
        .collect()
}

fn complete_matrix_names(
    text: &EditRope,
    cursor: &mut Cursor,
    store: &ProgramStore,
) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    let list = store.application.names.complete(id.as_ref());
    if !list.is_empty() {
        return list;
    }

    let list = store.application.presences.complete(id.as_ref());
    if !list.is_empty() {
        return list.into_iter().map(|i| i.to_string()).collect();
    }

    store
        .application
        .rooms
        .complete(id.as_ref())
        .into_iter()
        .map(|i| i.to_string())
        .collect()
}

fn complete_emoji(text: &EditRope, cursor: &mut Cursor, store: &ProgramStore) -> Vec<String> {
    let sc = text.get_prefix_word_mut(cursor, &WordStyle::Little);
    let sc = sc.unwrap_or_else(EditRope::empty);
    let sc = Cow::from(&sc);

    store.application.emojis.complete(sc.as_ref())
}

fn complete_cmdarg(
    desc: CommandDescription,
    text: &EditRope,
    cursor: &mut Cursor,
    store: &ProgramStore,
) -> Vec<String> {
    let cmd = match store.application.cmds.get(desc.command.as_str()) {
        Ok(cmd) => cmd,
        Err(_) => return vec![],
    };

    match cmd.name.as_str() {
        "cancel" | "dms" | "edit" | "redact" | "reply" => vec![],
        "members" | "rooms" | "spaces" | "welcome" => vec![],
        "download" | "open" | "upload" => complete_path(text, cursor),
        "react" | "unreact" => complete_emoji(text, cursor, store),

        "invite" => complete_users(text, cursor, store),
        "join" => complete_matrix_names(text, cursor, store),
        "room" => vec![],
        "verify" => vec![],
        _ => panic!("unknown command {}", cmd.name.as_str()),
    }
}

fn complete_cmdbar(text: &EditRope, cursor: &mut Cursor, store: &ProgramStore) -> Vec<String> {
    let eo = text.cursor_to_offset(cursor);
    let slice = text.slice(0.into(), eo, false);
    let cow = Cow::from(&slice);

    match CommandDescription::from_str(cow.as_ref()) {
        Ok(desc) => {
            if desc.arg.untrimmed.is_empty() {
                // Complete command name and set cursor position.
                let _ = text.get_prefix_word_mut(cursor, &WordStyle::Little);
                store.application.cmds.complete_name(desc.command.as_str())
            } else {
                // Complete command argument.
                complete_cmdarg(desc, text, cursor, store)
            }
        },

        // Can't parse command text, so return zero completions.
        Err(_) => vec![],
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::config::user_style_from_color;
    use crate::tests::*;
    use modalkit::tui::style::Color;

    #[test]
    fn test_typing_spans() {
        let mut info = RoomInfo::default();
        let settings = mock_settings();

        let users0 = vec![];
        let users1 = vec![TEST_USER1.clone()];
        let users2 = vec![TEST_USER1.clone(), TEST_USER2.clone()];
        let users4 = vec![
            TEST_USER1.clone(),
            TEST_USER2.clone(),
            TEST_USER3.clone(),
            TEST_USER4.clone(),
        ];
        let users5 = vec![
            TEST_USER1.clone(),
            TEST_USER2.clone(),
            TEST_USER3.clone(),
            TEST_USER4.clone(),
            TEST_USER5.clone(),
        ];

        // Nothing set.
        assert_eq!(info.users_typing, None);
        assert_eq!(info.get_typing_spans(&settings), Spans(vec![]));

        // Empty typing list.
        info.set_typing(users0);
        assert!(info.users_typing.is_some());
        assert_eq!(info.get_typing_spans(&settings), Spans(vec![]));

        // Single user typing.
        info.set_typing(users1);
        assert!(info.users_typing.is_some());
        assert_eq!(
            info.get_typing_spans(&settings),
            Spans(vec![
                Span::styled("@user1:example.com", user_style("@user1:example.com")),
                Span::from(" is typing...")
            ])
        );

        // Two users typing.
        info.set_typing(users2);
        assert!(info.users_typing.is_some());
        assert_eq!(
            info.get_typing_spans(&settings),
            Spans(vec![
                Span::styled("@user1:example.com", user_style("@user1:example.com")),
                Span::raw(" and "),
                Span::styled("@user2:example.com", user_style("@user2:example.com")),
                Span::raw(" are typing...")
            ])
        );

        // Four users typing.
        info.set_typing(users4);
        assert!(info.users_typing.is_some());
        assert_eq!(info.get_typing_spans(&settings), Spans::from("Several people are typing..."));

        // Five users typing.
        info.set_typing(users5);
        assert!(info.users_typing.is_some());
        assert_eq!(info.get_typing_spans(&settings), Spans::from("Many people are typing..."));

        // Test that USER5 gets rendered using the configured color and name.
        info.set_typing(vec![TEST_USER5.clone()]);
        assert!(info.users_typing.is_some());
        assert_eq!(
            info.get_typing_spans(&settings),
            Spans(vec![
                Span::styled("USER 5", user_style_from_color(Color::Black)),
                Span::from(" is typing...")
            ])
        );
    }

    #[tokio::test]
    async fn test_complete_cmdbar() {
        let store = mock_store().await;

        let text = EditRope::from("invite    ");
        let mut cursor = Cursor::new(0, 7);
        let id = text
            .get_prefix_word_mut(&mut cursor, &MATRIX_ID_WORD)
            .unwrap_or_else(EditRope::empty);
        assert_eq!(id.to_string(), "");
        assert_eq!(cursor, Cursor::new(0, 7));

        let text = EditRope::from("invite    ");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec![
            "@user1:example.com",
            "@user2:example.com",
            "@user3:example.com",
            "@user4:example.com",
            "@user5:example.com"
        ]);

        let text = EditRope::from("invite ignored");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec![
            "@user1:example.com",
            "@user2:example.com",
            "@user3:example.com",
            "@user4:example.com",
            "@user5:example.com"
        ]);

        let text = EditRope::from("invite @user1ignored");
        let mut cursor = Cursor::new(0, 13);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["@user1:example.com"]);
    }
}
