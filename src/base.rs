//! # Common types and utilities
//!
//! The types defined here get used throughout iamb.
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::str::FromStr;
use std::sync::Arc;
use std::time::{Duration, Instant};

use emojis::Emoji;
use serde::{
    de::Error as SerdeError,
    de::Visitor,
    Deserialize,
    Deserializer,
    Serialize,
    Serializer,
};
use tokio::sync::Mutex as AsyncMutex;
use url::Url;

use matrix_sdk::{
    encryption::verification::SasVerification,
    room::{Joined, Room as MatrixRoom},
    ruma::{
        events::{
            reaction::ReactionEvent,
            room::encrypted::RoomEncryptedEvent,
            room::message::{
                OriginalRoomMessageEvent,
                Relation,
                Replacement,
                RoomMessageEvent,
                RoomMessageEventContent,
            },
            tag::{TagName, Tags},
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
        text::{Line, Span},
        widgets::{Paragraph, Widget},
    },
};

use crate::{
    message::{Message, MessageEvent, MessageKey, MessageTimeStamp, Messages},
    worker::Requester,
    ApplicationSettings,
};

/// The set of characters used in different Matrix IDs.
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

/// Empty type used solely to implement [ApplicationInfo].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambInfo {}

/// An action taken against an ongoing verification request.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VerifyAction {
    /// Accept a verification request.
    Accept,

    /// Cancel an in-progress verification.
    Cancel,

    /// Confirm an in-progress verification.
    Confirm,

    /// Reject an in-progress verification due to mismatched Emoji.
    Mismatch,
}

/// An action taken against the currently selected message.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MessageAction {
    /// Cance the current reply or edit.
    ///
    /// The [bool] argument indicates whether to skip confirmation for clearing the message bar.
    Cancel(bool),

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
    ///
    /// The [bool] argument indicates whether to skip confirmation.
    Redact(Option<String>, bool),

    /// Reply to a message.
    Reply,

    /// Unreact to a message.
    ///
    /// If no specific Emoji to remove to is specified, then all reactions from the user on the
    /// message are removed.
    Unreact(Option<String>),
}

/// The type of room being created.
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
    /// Available options for newly created rooms.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct CreateRoomFlags: u32 {
        /// No flags specified.
        const NONE = 0b00000000;

        /// Make the room public.
        const PUBLIC = 0b00000001;

        /// Encrypt this room.
        const ENCRYPTED = 0b00000010;
    }
}

bitflags::bitflags! {
    /// Available options when downloading files.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct DownloadFlags: u32 {
        /// No flags specified.
        const NONE = 0b00000000;

        /// Overwrite file if it already exists.
        const FORCE = 0b00000001;

        /// Open file after downloading.
        const OPEN = 0b00000010;
    }
}

/// A room property.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RoomField {
    /// The room name.
    Name,

    /// A room tag.
    Tag(TagName),

    /// The room topic.
    Topic,
}

/// An action that operates on a focused room.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RoomAction {
    /// Accept an invitation to join this room.
    InviteAccept,

    /// Reject an invitation to join this room.
    InviteReject,

    /// Invite a user to this room.
    InviteSend(OwnedUserId),

    /// Leave this room.
    Leave(bool),

    /// Open the members window.
    Members(Box<CommandContext<ProgramContext>>),

    /// Set a room property.
    Set(RoomField, String),

    /// Unset a room property.
    Unset(RoomField),
}

/// An action that sends a message to a room.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SendAction {
    /// Send the text in the message bar.
    Submit,

    /// Send text provided from an external editor.
    SubmitFromEditor,

    /// Upload a file.
    Upload(String),

    /// Upload the image data.
    UploadImage(usize, usize, Cow<'static, [u8]>),
}

/// An action performed against the user's homeserver.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HomeserverAction {
    /// Create a new room with an optional localpart.
    CreateRoom(Option<String>, CreateRoomType, CreateRoomFlags),
}

/// An action that the main program loop should.
///
/// See [the commands module][super::commands] for where these are usually created.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambAction {
    /// Perform an action against the homeserver.
    Homeserver(HomeserverAction),

    /// Perform an action on the currently selected message.
    Message(MessageAction),

    /// Open a URL.
    OpenLink(String),

    /// Perform an action on the currently focused room.
    Room(RoomAction),

    /// Send a message to the currently focused room.
    Send(SendAction),

    /// Perform an action for an in-progress verification.
    Verify(VerifyAction, String),

    /// Request a new verification with the specified user.
    VerifyRequest(String),

    /// Toggle the focus within the focused room.
    ToggleScrollbackFocus,
}

impl IambAction {
    /// Indicates whether this action will draw over the screen.
    pub fn scribbles(&self) -> bool {
        matches!(self, IambAction::Send(SendAction::SubmitFromEditor))
    }
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
            IambAction::OpenLink(..) => SequenceStatus::Break,
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
            IambAction::OpenLink(..) => SequenceStatus::Atom,
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
            IambAction::OpenLink(..) => SequenceStatus::Ignore,
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
            IambAction::OpenLink(..) => false,
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

/// Alias for program actions.
pub type ProgramAction = Action<IambInfo>;
/// Alias for program context.
pub type ProgramContext = VimContext<IambInfo>;
/// Alias for program keybindings.
pub type Keybindings = VimMachine<TerminalKey, IambInfo>;
/// Alias for a program command.
pub type ProgramCommand = VimCommand<ProgramContext, IambInfo>;
/// Alias for mapped program commands.
pub type ProgramCommands = VimCommandMachine<ProgramContext, IambInfo>;
/// Alias for program store.
pub type ProgramStore = Store<IambInfo>;
/// Alias for shared program store.
pub type AsyncProgramStore = Arc<AsyncMutex<ProgramStore>>;
/// Alias for an action result.
pub type IambResult<T> = UIResult<T, IambInfo>;

/// Reaction events for some message.
///
/// The event identifier used as a key here is the ID for the reaction, and not for the message
/// it's reacting to.
pub type MessageReactions = HashMap<OwnedEventId, (String, OwnedUserId)>;

/// Errors encountered during application use.
#[derive(thiserror::Error, Debug)]
pub enum IambError {
    /// An invalid user identifier was specified.
    #[error("Invalid user identifier: {0}")]
    InvalidUserId(String),

    /// An invalid verification identifier was specified.
    #[error("Invalid verification user/device pair: {0}")]
    InvalidVerificationId(String),

    /// A failure related to the cryptographic store.
    #[error("Cryptographic storage error: {0}")]
    CryptoStore(#[from] matrix_sdk::encryption::CryptoStoreError),

    /// An HTTP error.
    #[error("HTTP client error: {0}")]
    Http(#[from] matrix_sdk::HttpError),

    /// A failure from the Matrix client.
    #[error("Matrix client error: {0}")]
    Matrix(#[from] matrix_sdk::Error),

    /// A failure in the sled storage.
    #[error("Matrix client storage error: {0}")]
    Store(#[from] matrix_sdk::StoreError),

    /// A failure during serialization or deserialization.
    #[error("Serialization/deserialization error: {0}")]
    Serde(#[from] serde_json::Error),

    /// A failure due to not having a configured download directory.
    #[error("No download directory configured")]
    NoDownloadDir,

    /// A failure due to not having a message with an attachment selected.
    #[error("Selected message does not have any attachments")]
    NoAttachment,

    /// A failure due to not having a message selected.
    #[error("No message currently selected")]
    NoSelectedMessage,

    /// A failure due to not having a room or space selected.
    #[error("Current window is not a room or space")]
    NoSelectedRoomOrSpace,

    /// A failure due to not having a room selected.
    #[error("Current window is not a room")]
    NoSelectedRoom,

    /// A failure due to not having an outstanding room invitation.
    #[error("You do not have a current invitation to this room")]
    NotInvited,

    /// A failure due to not being a joined room member.
    #[error("You need to join the room before you can do that")]
    NotJoined,

    /// An unknown room was specified.
    #[error("Unknown room identifier: {0}")]
    UnknownRoom(OwnedRoomId),

    /// A failure occurred during verification.
    #[error("Verification request error: {0}")]
    VerificationRequestError(#[from] matrix_sdk::encryption::identities::RequestVerificationError),

    /// A failure related to images.
    #[error("Image error: {0}")]
    Image(#[from] image::ImageError),

    /// A failure to access the system's clipboard.
    #[error("Could not use system clipboard data")]
    Clipboard,
}

impl From<IambError> for UIError<IambInfo> {
    fn from(err: IambError) -> Self {
        UIError::Application(err)
    }
}

impl ApplicationError for IambError {}

/// Status for tracking how much room scrollback we've fetched.
#[derive(Default)]
pub enum RoomFetchStatus {
    /// Room history has been completely fetched.
    Done,

    /// More room history can be fetched.
    HaveMore(String),

    /// We have not yet started fetching history for this room.
    #[default]
    NotStarted,
}

/// Indicates where an [EventId] lives in the [ChatStore].
pub enum EventLocation {
    /// The [EventId] belongs to a message.
    Message(MessageKey),

    /// The [EventId] belongs to a reaction to the given event.
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

/// Information about room's the user's joined.
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

    /// Whether the scrollback for this room is currently being fetched.
    pub fetching: bool,

    /// Where to continue fetching from when we continue loading scrollback history.
    pub fetch_id: RoomFetchStatus,

    /// The time that we last fetched scrollback for this room.
    pub fetch_last: Option<Instant>,

    /// Users currently typing in this room, and when we received notification of them doing so.
    pub users_typing: Option<(Instant, Vec<OwnedUserId>)>,

    /// The display names for users in this room.
    pub display_names: HashMap<OwnedUserId, String>,
}

impl RoomInfo {
    /// Get the reactions and their counts for a message.
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

    /// Map an event identifier to its [MessageKey].
    pub fn get_message_key(&self, event_id: &EventId) -> Option<&MessageKey> {
        self.keys.get(event_id)?.to_message_key()
    }

    /// Get an event for an identifier.
    pub fn get_event(&self, event_id: &EventId) -> Option<&Message> {
        self.messages.get(self.get_message_key(event_id)?)
    }

    /// Insert a reaction to a message.
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

    /// Insert an edit.
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
                orig.content.msgtype = new_content.msgtype;
            },
            MessageEvent::Local(_, content) => {
                content.msgtype = new_content.msgtype;
            },
            MessageEvent::Redacted(_) |
            MessageEvent::EncryptedOriginal(_) |
            MessageEvent::EncryptedRedacted(_) => {
                return;
            },
        }

        msg.html = msg.event.html();
    }

    /// Inserts events that couldn't be decrypted into the scrollback.
    pub fn insert_encrypted(&mut self, msg: RoomEncryptedEvent) {
        let event_id = msg.event_id().to_owned();
        let key = (msg.origin_server_ts().into(), event_id.clone());

        self.keys.insert(event_id, EventLocation::Message(key.clone()));
        self.messages.insert(key, msg.into());
    }

    /// Insert a new message.
    pub fn insert_message(&mut self, msg: RoomMessageEvent) {
        let event_id = msg.event_id().to_owned();
        let key = (msg.origin_server_ts().into(), event_id.clone());

        self.keys.insert(event_id.clone(), EventLocation::Message(key.clone()));
        self.messages.insert(key, msg.into());

        // Remove any echo.
        let key = (MessageTimeStamp::LocalEcho, event_id);
        let _ = self.messages.remove(&key);
    }

    /// Insert a new message event.
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

    /// Indicates whether we've recently fetched scrollback for this room.
    pub fn recently_fetched(&self) -> bool {
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

    fn get_typing_spans<'a>(&'a self, settings: &'a ApplicationSettings) -> Line<'a> {
        let typers = self.get_typers();
        let n = typers.len();

        match n {
            0 => Line::from(vec![]),
            1 => {
                let user = settings.get_user_span(typers[0].as_ref(), self);

                Line::from(vec![user, Span::from(" is typing...")])
            },
            2 => {
                let user1 = settings.get_user_span(typers[0].as_ref(), self);
                let user2 = settings.get_user_span(typers[1].as_ref(), self);

                Line::from(vec![
                    user1,
                    Span::raw(" and "),
                    user2,
                    Span::from(" are typing..."),
                ])
            },
            n if n < 5 => Line::from("Several people are typing..."),
            _ => Line::from("Many people are typing..."),
        }
    }

    /// Update typing information for this room.
    pub fn set_typing(&mut self, user_ids: Vec<OwnedUserId>) {
        self.users_typing = (Instant::now(), user_ids).into();
    }

    /// Create a [Rect] that displays what users are typing.
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

/// Generate a [CompletionMap] for Emoji shortcodes.
fn emoji_map() -> CompletionMap<String, &'static Emoji> {
    let mut emojis = CompletionMap::default();

    for emoji in emojis::iter() {
        for shortcode in emoji.shortcodes() {
            emojis.insert(shortcode.to_string(), emoji);
        }
    }

    return emojis;
}

/// Information gathered during server syncs about joined rooms.
#[derive(Default)]
pub struct SyncInfo {
    /// Spaces that the user is a member of.
    pub spaces: Vec<MatrixRoom>,

    /// Rooms that the user is a member of.
    pub rooms: Vec<Arc<(MatrixRoom, Option<Tags>)>>,

    /// DMs that the user is a member of.
    pub dms: Vec<Arc<(MatrixRoom, Option<Tags>)>>,
}

/// The main application state.
pub struct ChatStore {
    /// `:`-commands
    pub cmds: ProgramCommands,

    /// Handle for communicating w/ the worker thread.
    pub worker: Requester,

    /// Map of joined rooms.
    pub rooms: CompletionMap<OwnedRoomId, RoomInfo>,

    /// Map of room names.
    pub names: CompletionMap<String, OwnedRoomId>,

    /// Presence information for other users.
    pub presences: CompletionMap<OwnedUserId, PresenceState>,

    /// In-progress and completed verifications.
    pub verifications: HashMap<String, SasVerification>,

    /// Settings for the current profile loaded from config file.
    pub settings: ApplicationSettings,

    /// Set of rooms that need more messages loaded in their scrollback.
    pub need_load: HashSet<OwnedRoomId>,

    /// [CompletionMap] of Emoji shortcodes.
    pub emojis: CompletionMap<String, &'static Emoji>,

    /// Information gathered by the background thread.
    pub sync_info: SyncInfo,
}

impl ChatStore {
    /// Create a new [ChatStore].
    pub fn new(worker: Requester, settings: ApplicationSettings) -> Self {
        ChatStore {
            worker,
            settings,

            cmds: crate::commands::setup_commands(),
            emojis: emoji_map(),

            names: Default::default(),
            rooms: Default::default(),
            presences: Default::default(),
            verifications: Default::default(),
            need_load: Default::default(),
            sync_info: Default::default(),
        }
    }

    /// Get a joined room.
    pub fn get_joined_room(&self, room_id: &RoomId) -> Option<Joined> {
        self.worker.client.get_joined_room(room_id)
    }

    /// Get the title for a room.
    pub fn get_room_title(&self, room_id: &RoomId) -> String {
        self.rooms
            .get(room_id)
            .and_then(|i| i.name.as_ref())
            .map(String::from)
            .unwrap_or_else(|| "Untitled Matrix Room".to_string())
    }

    /// Mark a room for loading more scrollback.
    pub fn mark_for_load(&mut self, room_id: OwnedRoomId) {
        self.need_load.insert(room_id);
    }

    /// Get the [RoomInfo] for a given room identifier.
    pub fn get_room_info(&mut self, room_id: OwnedRoomId) -> &mut RoomInfo {
        self.rooms.get_or_default(room_id)
    }

    /// Set the name for a room.
    pub fn set_room_name(&mut self, room_id: &RoomId, name: &str) {
        self.rooms.get_or_default(room_id.to_owned()).name = name.to_string().into();
    }

    /// Insert a new E2EE verification.
    pub fn insert_sas(&mut self, sas: SasVerification) {
        let key = format!("{}/{}", sas.other_user_id(), sas.other_device().device_id());

        self.verifications.insert(key, sas);
    }
}

impl ApplicationStore for ChatStore {}

/// Identified used to track window content.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IambId {
    /// A Matrix room.
    Room(OwnedRoomId),

    /// The `:rooms` window.
    DirectList,

    /// The `:members` window for a given Matrix room.
    MemberList(OwnedRoomId),

    /// The `:rooms` window.
    RoomList,

    /// The `:spaces` window.
    SpaceList,

    /// The `:verify` window.
    VerifyList,

    /// The `:welcome` window.
    Welcome,
}

impl Display for IambId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IambId::Room(room_id) => {
                write!(f, "iamb://room/{room_id}")
            },
            IambId::MemberList(room_id) => {
                write!(f, "iamb://members/{room_id}")
            },
            IambId::DirectList => f.write_str("iamb://dms"),
            IambId::RoomList => f.write_str("iamb://rooms"),
            IambId::SpaceList => f.write_str("iamb://spaces"),
            IambId::VerifyList => f.write_str("iamb://verify"),
            IambId::Welcome => f.write_str("iamb://welcome"),
        }
    }
}

impl ApplicationWindowId for IambId {}

impl Serialize for IambId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for IambId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(IambIdVisitor)
    }
}

/// [serde] visitor for deserializing [IambId].
struct IambIdVisitor;

impl<'de> Visitor<'de> for IambIdVisitor {
    type Value = IambId;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid window URL")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        let Ok(url) = Url::parse(value) else {
            return Err(E::custom("Invalid iamb window URL"));
        };

        if url.scheme() != "iamb" {
            return Err(E::custom("Invalid iamb window URL"));
        }

        match url.domain() {
            Some("room") => {
                let Some(path) = url.path_segments() else {
                    return Err(E::custom("Invalid members window URL"));
                };

                let &[room_id] = path.collect::<Vec<_>>().as_slice() else {
                    return Err(E::custom("Invalid members window URL"));
                };

                let Ok(room_id) = OwnedRoomId::try_from(room_id) else {
                    return Err(E::custom("Invalid room identifier"));
                };

                Ok(IambId::Room(room_id))
            },
            Some("members") => {
                let Some(path) = url.path_segments() else {
                    return Err(E::custom("Invalid members window URL"));
                };

                let &[room_id] = path.collect::<Vec<_>>().as_slice() else {
                    return Err(E::custom("Invalid members window URL"));
                };

                let Ok(room_id) = OwnedRoomId::try_from(room_id) else {
                    return Err(E::custom("Invalid room identifier"));
                };

                Ok(IambId::MemberList(room_id))
            },
            Some("dms") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://dms takes no path"));
                }

                Ok(IambId::DirectList)
            },
            Some("rooms") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://rooms takes no path"));
                }

                Ok(IambId::RoomList)
            },
            Some("spaces") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://spaces takes no path"));
                }

                Ok(IambId::SpaceList)
            },
            Some("verify") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://verify takes no path"));
                }

                Ok(IambId::VerifyList)
            },
            Some("welcome") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://welcome takes no path"));
                }

                Ok(IambId::Welcome)
            },
            Some(s) => Err(E::custom(format!("{s:?} is not a valid window"))),
            None => Err(E::custom("Invalid iamb window URL")),
        }
    }
}

/// Which part of the room window's UI is focused.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RoomFocus {
    /// The scrollback for a room window is focused.
    Scrollback,

    /// The message bar for a room window is focused.
    MessageBar,
}

impl RoomFocus {
    /// Whether this is [RoomFocus::Scrollback].
    pub fn is_scrollback(&self) -> bool {
        matches!(self, RoomFocus::Scrollback)
    }

    /// Whether this is [RoomFocus::MessageBar].
    pub fn is_msgbar(&self) -> bool {
        matches!(self, RoomFocus::MessageBar)
    }
}

/// Identifiers used to track where a mark was placed.
///
/// While this is the "buffer identifier" for the mark,
/// not all of these are necessarily actual buffers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IambBufferId {
    /// The command bar buffer.
    Command(CommandType),

    /// The message buffer or a specific message in a room.
    Room(OwnedRoomId, RoomFocus),

    /// The `:dms` window.
    DirectList,

    /// The `:members` window for a room.
    MemberList(OwnedRoomId),

    /// The `:rooms` window.
    RoomList,

    /// The `:spaces` window.
    SpaceList,

    /// The `:verify` window.
    VerifyList,

    /// The buffer for the `:rooms` window.
    Welcome,
}

impl IambBufferId {
    /// Get the identifier for the window that contains this buffer.
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

            IambBufferId::Room(_, RoomFocus::MessageBar) => complete_msgbar(text, cursor, store),
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

/// Tab completion for user IDs.
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

/// Tab completion within the message bar.
fn complete_msgbar(text: &EditRope, cursor: &mut Cursor, store: &ProgramStore) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    match id.chars().next() {
        // Complete room aliases.
        Some('#') => {
            return store.application.names.complete(id.as_ref());
        },

        // Complete room identifiers.
        Some('!') => {
            return store
                .application
                .rooms
                .complete(id.as_ref())
                .into_iter()
                .map(|i| i.to_string())
                .collect();
        },

        // Complete Emoji shortcodes.
        Some(':') => {
            let list = store.application.emojis.complete(&id[1..]);
            let iter = list.into_iter().take(200).map(|s| format!(":{}:", s));

            return iter.collect();
        },

        // Complete usernames for @ and empty strings.
        Some('@') | None => {
            return store
                .application
                .presences
                .complete(id.as_ref())
                .into_iter()
                .map(|i| i.to_string())
                .collect();
        },

        // Unknown sigil.
        Some(_) => return vec![],
    }
}

/// Tab completion for Matrix identifiers (usernames, room aliases, etc.)
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

/// Tab completion for Emoji shortcode names.
fn complete_emoji(text: &EditRope, cursor: &mut Cursor, store: &ProgramStore) -> Vec<String> {
    let sc = text.get_prefix_word_mut(cursor, &WordStyle::Little);
    let sc = sc.unwrap_or_else(EditRope::empty);
    let sc = Cow::from(&sc);

    store.application.emojis.complete(sc.as_ref())
}

/// Tab completion for command names.
fn complete_cmdname(
    desc: CommandDescription,
    text: &EditRope,
    cursor: &mut Cursor,
    store: &ProgramStore,
) -> Vec<String> {
    // Complete command name and set cursor position.
    let _ = text.get_prefix_word_mut(cursor, &WordStyle::Little);
    store.application.cmds.complete_name(desc.command.as_str())
}

/// Tab completion for command arguments.
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
        "join" | "split" | "vsplit" | "tabedit" => complete_matrix_names(text, cursor, store),
        "room" => vec![],
        "verify" => vec![],
        "vertical" | "horizontal" | "aboveleft" | "belowright" | "tab" => {
            complete_cmd(desc.arg.text.as_str(), text, cursor, store)
        },
        _ => vec![],
    }
}

/// Tab completion for commands.
fn complete_cmd(
    cmd: &str,
    text: &EditRope,
    cursor: &mut Cursor,
    store: &ProgramStore,
) -> Vec<String> {
    match CommandDescription::from_str(cmd) {
        Ok(desc) => {
            if desc.arg.untrimmed.is_empty() {
                complete_cmdname(desc, text, cursor, store)
            } else {
                // Complete command argument.
                complete_cmdarg(desc, text, cursor, store)
            }
        },

        // Can't parse command text, so return zero completions.
        Err(_) => vec![],
    }
}

/// Tab completion for the command bar.
fn complete_cmdbar(text: &EditRope, cursor: &mut Cursor, store: &ProgramStore) -> Vec<String> {
    let eo = text.cursor_to_offset(cursor);
    let slice = text.slice(0.into(), eo, false);
    let cow = Cow::from(&slice);

    complete_cmd(cow.as_ref(), text, cursor, store)
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
        assert_eq!(info.get_typing_spans(&settings), Line::from(vec![]));

        // Empty typing list.
        info.set_typing(users0);
        assert!(info.users_typing.is_some());
        assert_eq!(info.get_typing_spans(&settings), Line::from(vec![]));

        // Single user typing.
        info.set_typing(users1);
        assert!(info.users_typing.is_some());
        assert_eq!(
            info.get_typing_spans(&settings),
            Line::from(vec![
                Span::styled("@user1:example.com", user_style("@user1:example.com")),
                Span::from(" is typing...")
            ])
        );

        // Two users typing.
        info.set_typing(users2);
        assert!(info.users_typing.is_some());
        assert_eq!(
            info.get_typing_spans(&settings),
            Line::from(vec![
                Span::styled("@user1:example.com", user_style("@user1:example.com")),
                Span::raw(" and "),
                Span::styled("@user2:example.com", user_style("@user2:example.com")),
                Span::raw(" are typing...")
            ])
        );

        // Four users typing.
        info.set_typing(users4);
        assert!(info.users_typing.is_some());
        assert_eq!(info.get_typing_spans(&settings), Line::from("Several people are typing..."));

        // Five users typing.
        info.set_typing(users5);
        assert!(info.users_typing.is_some());
        assert_eq!(info.get_typing_spans(&settings), Line::from("Many people are typing..."));

        // Test that USER5 gets rendered using the configured color and name.
        info.set_typing(vec![TEST_USER5.clone()]);
        assert!(info.users_typing.is_some());
        assert_eq!(
            info.get_typing_spans(&settings),
            Line::from(vec![
                Span::styled("USER 5", user_style_from_color(Color::Black)),
                Span::from(" is typing...")
            ])
        );
    }

    #[tokio::test]
    async fn test_complete_msgbar() {
        let store = mock_store().await;

        let text = EditRope::from("going for a walk :walk ");
        let mut cursor = Cursor::new(0, 22);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec![":walking:", ":walking_man:", ":walking_woman:"]);
        assert_eq!(cursor, Cursor::new(0, 17));

        let text = EditRope::from("hello @user1 ");
        let mut cursor = Cursor::new(0, 12);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["@user1:example.com"]);
        assert_eq!(cursor, Cursor::new(0, 6));

        let text = EditRope::from("see #room ");
        let mut cursor = Cursor::new(0, 9);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["#room1:example.com"]);
        assert_eq!(cursor, Cursor::new(0, 4));
    }

    #[tokio::test]
    async fn test_complete_cmdbar() {
        let store = mock_store().await;
        let users = vec![
            "@user1:example.com",
            "@user2:example.com",
            "@user3:example.com",
            "@user4:example.com",
            "@user5:example.com",
        ];

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
        assert_eq!(res, users);

        let text = EditRope::from("invite ignored");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, users);

        let text = EditRope::from("invite @user1ignored");
        let mut cursor = Cursor::new(0, 13);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["@user1:example.com"]);

        let text = EditRope::from("abo hor");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["horizontal"]);

        let text = EditRope::from("abo hor inv");
        let mut cursor = Cursor::new(0, 11);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["invite"]);

        let text = EditRope::from("abo hor invite \n");
        let mut cursor = Cursor::new(0, 15);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, users);
    }
}
