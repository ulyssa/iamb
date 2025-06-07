//! # Common types and utilities
//!
//! The types defined here get used throughout iamb.
use std::borrow::Cow;
use std::collections::hash_map::IntoIter;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::str::FromStr;
use std::sync::Arc;
use std::time::{Duration, Instant};

use emojis::Emoji;
use matrix_sdk::ruma::events::receipt::ReceiptThread;
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    text::{Line, Span},
    widgets::{Paragraph, Widget},
};
use ratatui_image::picker::{Picker, ProtocolType};
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
    room::Room as MatrixRoom,
    ruma::{
        events::{
            reaction::ReactionEvent,
            relation::{Replacement, Thread},
            room::encrypted::RoomEncryptedEvent,
            room::message::{
                OriginalRoomMessageEvent,
                Relation,
                RoomMessageEvent,
                RoomMessageEventContent,
                RoomMessageEventContentWithoutRelation,
            },
            room::redaction::{OriginalSyncRoomRedactionEvent, SyncRoomRedactionEvent},
            tag::{TagName, Tags},
            AnySyncStateEvent,
            MessageLikeEvent,
        },
        presence::PresenceState,
        EventId,
        OwnedEventId,
        OwnedRoomId,
        OwnedUserId,
        RoomId,
        RoomVersionId,
        UserId,
    },
    RoomState as MatrixRoomState,
};

use modalkit::{
    actions::Action,
    editing::{
        application::{
            ApplicationAction,
            ApplicationContentId,
            ApplicationError,
            ApplicationInfo,
            ApplicationStore,
            ApplicationWindowId,
        },
        completion::{complete_path, CompletionMap},
        context::EditContext,
        cursor::Cursor,
        rope::EditRope,
        store::Store,
    },
    env::vim::{
        command::{CommandContext, CommandDescription, VimCommand, VimCommandMachine},
        keybindings::VimMachine,
    },
    errors::{UIError, UIResult},
    key::TerminalKey,
    keybindings::SequenceStatus,
    prelude::{CommandType, WordStyle},
};

use crate::config::ImagePreviewProtocolValues;
use crate::message::ImageStatus;
use crate::preview::{source_from_event, spawn_insert_preview};
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
    ///
    /// `:react` will by default try to convert the [String] argument to an Emoji, and error when
    /// it doesn't recognize it. The second [bool] argument forces it to be interpreted literally
    /// when it is `true`.
    React(String, bool),

    /// Redact a message, with an optional reason.
    ///
    /// The [bool] argument indicates whether to skip confirmation.
    Redact(Option<String>, bool),

    /// Reply to a message.
    Reply,

    /// Go to the message the hovered message replied to.
    Replied,

    /// Unreact to a message.
    ///
    /// If no specific Emoji to remove to is specified, then all reactions from the user on the
    /// message are removed.
    ///
    /// Like `:react`, `:unreact` will by default try to convert the [String] argument to an Emoji,
    /// and error when it doesn't recognize it. The second [bool] argument forces it to be
    /// interpreted literally when it is `true`.
    Unreact(Option<String>, bool),
}

/// An action taken in the currently selected space.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SpaceAction {
    /// Add a room or update metadata.
    ///
    /// The [`Option<String>`] argument is the order parameter.
    /// The [`bool`] argument indicates whether the room is suggested.
    SetChild(OwnedRoomId, Option<String>, bool),

    /// Remove the selected room.
    RemoveChild,
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

/// Fields that rooms and spaces can be sorted by.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SortFieldRoom {
    /// Sort rooms by whether they have the Favorite tag.
    Favorite,

    /// Sort rooms by whether they have the Low Priority tag.
    LowPriority,

    /// Sort rooms by their room name.
    Name,

    /// Sort rooms by their canonical room alias.
    Alias,

    /// Sort rooms by their Matrix room identifier.
    RoomId,

    /// Sort rooms by whether they have unread messages.
    Unread,

    /// Sort rooms by the timestamps of their most recent messages.
    Recent,

    /// Sort rooms by whether they are invites.
    Invite,
}

/// Fields that users can be sorted by.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SortFieldUser {
    PowerLevel,
    UserId,
    LocalPart,
    Server,
}

/// Whether to use the default sort direction for a field, or to reverse it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SortOrder {
    Ascending,
    Descending,
}

/// One of the columns to sort on.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SortColumn<T>(pub T, pub SortOrder);

impl<'de> Deserialize<'de> for SortColumn<SortFieldRoom> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(SortRoomVisitor)
    }
}

/// [serde] visitor for deserializing [SortColumn] for rooms and spaces.
struct SortRoomVisitor;

impl Visitor<'_> for SortRoomVisitor {
    type Value = SortColumn<SortFieldRoom>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid field for sorting rooms")
    }

    fn visit_str<E>(self, mut value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        if value.is_empty() {
            return Err(E::custom("Invalid sort field"));
        }

        let order = if value.starts_with('~') {
            value = &value[1..];
            SortOrder::Descending
        } else {
            SortOrder::Ascending
        };

        let field = match value {
            "favorite" => SortFieldRoom::Favorite,
            "lowpriority" => SortFieldRoom::LowPriority,
            "recent" => SortFieldRoom::Recent,
            "unread" => SortFieldRoom::Unread,
            "name" => SortFieldRoom::Name,
            "alias" => SortFieldRoom::Alias,
            "id" => SortFieldRoom::RoomId,
            "invite" => SortFieldRoom::Invite,
            _ => {
                let msg = format!("Unknown sort field: {value:?}");
                return Err(E::custom(msg));
            },
        };

        Ok(SortColumn(field, order))
    }
}

impl<'de> Deserialize<'de> for SortColumn<SortFieldUser> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(SortUserVisitor)
    }
}

/// [serde] visitor for deserializing [SortColumn] for users.
struct SortUserVisitor;

impl Visitor<'_> for SortUserVisitor {
    type Value = SortColumn<SortFieldUser>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid field for sorting rooms")
    }

    fn visit_str<E>(self, mut value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        if value.is_empty() {
            return Err(E::custom("Invalid field for sorting users"));
        }

        let order = if value.starts_with('~') {
            value = &value[1..];
            SortOrder::Descending
        } else {
            SortOrder::Ascending
        };

        let field = match value {
            "id" => SortFieldUser::UserId,
            "localpart" => SortFieldUser::LocalPart,
            "server" => SortFieldUser::Server,
            "power" => SortFieldUser::PowerLevel,
            _ => {
                let msg = format!("Unknown sort field: {value:?}");
                return Err(E::custom(msg));
            },
        };

        Ok(SortColumn(field, order))
    }
}

/// A room property.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RoomField {
    /// The room's history visibility.
    History,

    /// The room name.
    Name,

    /// The room id.
    Id,

    /// A room tag.
    Tag(TagName),

    /// The room topic.
    Topic,

    /// Notification level.
    NotificationMode,

    /// The room's entire list of alternative aliases.
    Aliases,

    /// A specific alternative alias to the room.
    Alias(String),

    /// The room's canonical alias.
    CanonicalAlias,
}

/// An action that operates on a room member.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MemberUpdateAction {
    Ban,
    Kick,
    Unban,
}

impl Display for MemberUpdateAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberUpdateAction::Ban => write!(f, "ban"),
            MemberUpdateAction::Kick => write!(f, "kick"),
            MemberUpdateAction::Unban => write!(f, "unban"),
        }
    }
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

    /// Update a user's membership in this room.
    MemberUpdate(MemberUpdateAction, String, Option<String>, bool),

    /// Open the members window.
    Members(Box<CommandContext>),

    /// Set whether a room is a direct message.
    SetDirect(bool),

    /// Set a room property.
    Set(RoomField, String),

    /// Unset a room property.
    Unset(RoomField),

    /// List the values in a list room property.
    Show(RoomField),
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
    Logout(String, bool),
}

/// An action performed against the user's room keys.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum KeysAction {
    /// Export room keys to a file, encrypted with a passphrase.
    Export(String, String),
    /// Import room keys from a file, encrypted with a passphrase.
    Import(String, String),
}

/// An action that the main program loop should.
///
/// See [the commands module][super::commands] for where these are usually created.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambAction {
    /// Perform an action against the homeserver.
    Homeserver(HomeserverAction),

    /// Perform an action over room keys.
    Keys(KeysAction),

    /// Perform an action on the currently selected message.
    Message(MessageAction),

    /// Perform an action on the current space.
    Space(SpaceAction),

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

    /// Clear all unread messages.
    ClearUnreads,
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

impl From<SpaceAction> for IambAction {
    fn from(act: SpaceAction) -> Self {
        IambAction::Space(act)
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
    fn is_edit_sequence(&self, _: &EditContext) -> SequenceStatus {
        match self {
            IambAction::ClearUnreads => SequenceStatus::Break,
            IambAction::Homeserver(..) => SequenceStatus::Break,
            IambAction::Keys(..) => SequenceStatus::Break,
            IambAction::Message(..) => SequenceStatus::Break,
            IambAction::Space(..) => SequenceStatus::Break,
            IambAction::Room(..) => SequenceStatus::Break,
            IambAction::OpenLink(..) => SequenceStatus::Break,
            IambAction::Send(..) => SequenceStatus::Break,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Break,
            IambAction::Verify(..) => SequenceStatus::Break,
            IambAction::VerifyRequest(..) => SequenceStatus::Break,
        }
    }

    fn is_last_action(&self, _: &EditContext) -> SequenceStatus {
        match self {
            IambAction::ClearUnreads => SequenceStatus::Atom,
            IambAction::Homeserver(..) => SequenceStatus::Atom,
            IambAction::Keys(..) => SequenceStatus::Atom,
            IambAction::Message(..) => SequenceStatus::Atom,
            IambAction::Space(..) => SequenceStatus::Atom,
            IambAction::OpenLink(..) => SequenceStatus::Atom,
            IambAction::Room(..) => SequenceStatus::Atom,
            IambAction::Send(..) => SequenceStatus::Atom,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Atom,
            IambAction::Verify(..) => SequenceStatus::Atom,
            IambAction::VerifyRequest(..) => SequenceStatus::Atom,
        }
    }

    fn is_last_selection(&self, _: &EditContext) -> SequenceStatus {
        match self {
            IambAction::ClearUnreads => SequenceStatus::Ignore,
            IambAction::Homeserver(..) => SequenceStatus::Ignore,
            IambAction::Keys(..) => SequenceStatus::Ignore,
            IambAction::Message(..) => SequenceStatus::Ignore,
            IambAction::Space(..) => SequenceStatus::Ignore,
            IambAction::Room(..) => SequenceStatus::Ignore,
            IambAction::OpenLink(..) => SequenceStatus::Ignore,
            IambAction::Send(..) => SequenceStatus::Ignore,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Ignore,
            IambAction::Verify(..) => SequenceStatus::Ignore,
            IambAction::VerifyRequest(..) => SequenceStatus::Ignore,
        }
    }

    fn is_switchable(&self, _: &EditContext) -> bool {
        match self {
            IambAction::ClearUnreads => false,
            IambAction::Homeserver(..) => false,
            IambAction::Message(..) => false,
            IambAction::Space(..) => false,
            IambAction::Room(..) => false,
            IambAction::Keys(..) => false,
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

impl From<SpaceAction> for ProgramAction {
    fn from(act: SpaceAction) -> Self {
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
pub type ProgramContext = EditContext;
/// Alias for program keybindings.
pub type Keybindings = VimMachine<TerminalKey, IambInfo>;
/// Alias for a program command.
pub type ProgramCommand = VimCommand<IambInfo>;
/// Alias for mapped program commands.
pub type ProgramCommands = VimCommandMachine<IambInfo>;
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
    /// An invalid history visibility was specified.
    #[error("Invalid history visibility setting: {0}")]
    InvalidHistoryVisibility(String),

    /// An invalid notification level was specified.
    #[error("Invalid notification level: {0}")]
    InvalidNotificationLevel(String),

    /// An invalid user identifier was specified.
    #[error("Invalid user identifier: {0}")]
    InvalidUserId(String),

    /// An invalid user identifier was specified.
    #[error("Invalid room alias: {0}")]
    InvalidRoomAlias(String),

    /// An invalid verification identifier was specified.
    #[error("Invalid verification user/device pair: {0}")]
    InvalidVerificationId(String),

    /// A failure related to the cryptographic store.
    #[error("Cryptographic storage error: {0}")]
    CryptoStore(#[from] matrix_sdk::encryption::CryptoStoreError),

    #[error("Failed to import room keys: {0}")]
    FailedKeyImport(#[from] matrix_sdk::encryption::RoomKeyImportError),

    /// A failure related to the cryptographic store.
    #[error("Cannot export keys from sled: {0}")]
    UpgradeSled(#[from] crate::sled_export::SledMigrationError),

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

    /// A failure due to not having a room or space item selected in a list.
    #[error("No room or space currently selected in list")]
    NoSelectedRoomOrSpaceItem,

    /// A failure due to not having a room selected.
    #[error("Current window is not a room")]
    NoSelectedRoom,

    /// A failure due to not having a space selected.
    #[error("Current window is not a space")]
    NoSelectedSpace,

    /// A failure due to not having sufficient permission to perform an action in a room.
    #[error("You do not have the permission to do that")]
    InsufficientPermission,

    /// A failure due to not having an outstanding room invitation.
    #[error("You do not have a current invitation to this room")]
    NotInvited,

    /// A failure due to not being a joined room member.
    #[error("You need to join the room before you can do that")]
    NotJoined,

    /// An unknown room was specified.
    #[error("Unknown room identifier: {0}")]
    UnknownRoom(OwnedRoomId),

    /// An invalid room alias id was specified.
    #[error("Invalid room alias id: {0}")]
    InvalidRoomAliasId(#[from] matrix_sdk::ruma::IdParseError),

    /// A failure occurred during verification.
    #[error("Verification request error: {0}")]
    VerificationRequestError(#[from] matrix_sdk::encryption::identities::RequestVerificationError),

    #[error("Notification setting error: {0}")]
    NotificationSettingError(#[from] matrix_sdk::NotificationSettingsError),

    /// A failure related to images.
    #[error("Image error: {0}")]
    Image(#[from] image::ImageError),

    /// A failure to access the system's clipboard.
    #[error("Could not use system clipboard data")]
    Clipboard,

    /// An failure during disk/network/ipc/etc. I/O.
    #[error("Input/Output error: {0}")]
    IOError(#[from] std::io::Error),

    /// A failure while trying to show an image preview.
    #[error("Preview error: {0}")]
    Preview(String),
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
    ///
    /// If the first argument is [None], then it's part of the main scrollback. When [Some],
    /// it specifies which thread it's in reply to.
    Message(Option<OwnedEventId>, MessageKey),

    /// The [EventId] belongs to a reaction to the given event.
    Reaction(OwnedEventId),

    /// The [EventId] belongs to a state event in the main timeline of the room.
    State(MessageKey),
}

impl EventLocation {
    fn to_message_key(&self) -> Option<&MessageKey> {
        if let EventLocation::Message(_, key) = self {
            Some(key)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct UnreadInfo {
    pub(crate) unread: bool,
    pub(crate) latest: Option<MessageTimeStamp>,
}

impl UnreadInfo {
    pub fn is_unread(&self) -> bool {
        self.unread
    }

    pub fn latest(&self) -> Option<&MessageTimeStamp> {
        self.latest.as_ref()
    }
}

/// Information about room's the user's joined.
pub struct RoomInfo {
    /// The display name for this room.
    pub name: Option<String>,

    /// The tags placed on this room.
    pub tags: Option<Tags>,

    /// A map of event IDs to where they are stored in this struct.
    pub keys: HashMap<OwnedEventId, EventLocation>,

    /// The messages loaded for this room.
    messages: Messages,

    /// A map of read markers to display on different events.
    pub event_receipts: HashMap<ReceiptThread, HashMap<OwnedEventId, HashSet<OwnedUserId>>>,
    /// A map of the most recent read marker for each user.
    ///
    /// Every receipt in this map should also have an entry in [`event_receipts`](`Self::event_receipts`),
    /// however not every user has an entry. If a user's most recent receipt is
    /// older than the oldest loaded event, that user will not be included.
    pub user_receipts: HashMap<ReceiptThread, HashMap<OwnedUserId, OwnedEventId>>,
    /// A map of message identifiers to a map of reaction events.
    pub reactions: HashMap<OwnedEventId, MessageReactions>,

    /// A map of message identifiers to thread replies.
    threads: HashMap<OwnedEventId, Messages>,

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

    /// The last time the room was rendered, used to detect if it is currently open.
    pub draw_last: Option<Instant>,
}

impl Default for RoomInfo {
    fn default() -> Self {
        Self {
            messages: Messages::new(ReceiptThread::Main),

            name: Default::default(),
            tags: Default::default(),
            keys: Default::default(),
            event_receipts: Default::default(),
            user_receipts: Default::default(),
            reactions: Default::default(),
            threads: Default::default(),
            fetching: Default::default(),
            fetch_id: Default::default(),
            fetch_last: Default::default(),
            users_typing: Default::default(),
            display_names: Default::default(),
            draw_last: Default::default(),
        }
    }
}

impl RoomInfo {
    pub fn get_thread(&self, root: Option<&EventId>) -> Option<&Messages> {
        if let Some(thread_root) = root {
            self.threads.get(thread_root)
        } else {
            Some(&self.messages)
        }
    }

    pub fn get_thread_mut(&mut self, root: Option<OwnedEventId>) -> &mut Messages {
        if let Some(thread_root) = root {
            self.threads
                .entry(thread_root.clone())
                .or_insert_with(|| Messages::thread(thread_root))
        } else {
            &mut self.messages
        }
    }

    /// Get the event for the last message in a thread (or the thread root if there are no
    /// in-thread replies yet).
    ///
    /// This returns `None` if the event identifier isn't in the room.
    pub fn get_thread_last<'a>(
        &'a self,
        thread_root: &OwnedEventId,
    ) -> Option<&'a OriginalRoomMessageEvent> {
        let last = self.threads.get(thread_root).and_then(|t| Some(t.last_key_value()?.1));

        let msg = if let Some(last) = last {
            &last.event
        } else if let EventLocation::Message(_, key) = self.keys.get(thread_root)? {
            let msg = self.messages.get(key)?;
            &msg.event
        } else {
            return None;
        };

        if let MessageEvent::Original(ev) = &msg {
            Some(ev)
        } else {
            None
        }
    }

    /// Get the reactions and their counts for a message.
    pub fn get_reactions(&self, event_id: &EventId) -> Vec<(&str, usize)> {
        if let Some(reacts) = self.reactions.get(event_id) {
            let mut counts = HashMap::new();

            let mut seen_user_reactions = BTreeSet::new();

            for (key, user) in reacts.values() {
                if !seen_user_reactions.contains(&(key, user)) {
                    seen_user_reactions.insert((key, user));
                    let count = counts.entry(key.as_str()).or_default();
                    *count += 1;
                }
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

    /// Get an event for an identifier as mutable.
    pub fn get_event_mut(&mut self, event_id: &EventId) -> Option<&mut Message> {
        self.messages.get_mut(self.keys.get(event_id)?.to_message_key()?)
    }

    pub fn redact(&mut self, ev: OriginalSyncRoomRedactionEvent, room_version: &RoomVersionId) {
        let Some(redacts) = &ev.redacts else {
            return;
        };

        match self.keys.get(redacts) {
            None => return,
            Some(EventLocation::State(key)) => {
                if let Some(msg) = self.messages.get_mut(key) {
                    let ev = SyncRoomRedactionEvent::Original(ev);
                    msg.redact(ev, room_version);
                }
            },
            Some(EventLocation::Message(None, key)) => {
                if let Some(msg) = self.messages.get_mut(key) {
                    let ev = SyncRoomRedactionEvent::Original(ev);
                    msg.redact(ev, room_version);
                }
            },
            Some(EventLocation::Message(Some(root), key)) => {
                if let Some(thread) = self.threads.get_mut(root) {
                    if let Some(msg) = thread.get_mut(key) {
                        let ev = SyncRoomRedactionEvent::Original(ev);
                        msg.redact(ev, room_version);
                    }
                }
            },
            Some(EventLocation::Reaction(event_id)) => {
                if let Some(reactions) = self.reactions.get_mut(event_id) {
                    reactions.remove(redacts);
                }

                self.keys.remove(redacts);
            },
        }
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
    pub fn insert_edit(&mut self, msg: Replacement<RoomMessageEventContentWithoutRelation>) {
        let event_id = msg.event_id;
        let new_msgtype = msg.new_content;

        let Some(EventLocation::Message(thread, key)) = self.keys.get(&event_id) else {
            return;
        };

        let source = if let Some(thread) = thread {
            self.threads
                .entry(thread.clone())
                .or_insert_with(|| Messages::thread(thread.clone()))
        } else {
            &mut self.messages
        };

        let Some(msg) = source.get_mut(key) else {
            return;
        };

        match &mut msg.event {
            MessageEvent::Original(orig) => {
                orig.content.apply_replacement(new_msgtype);
            },
            MessageEvent::Local(_, content) => {
                content.apply_replacement(new_msgtype);
            },
            MessageEvent::Redacted(_) |
            MessageEvent::State(_) |
            MessageEvent::EncryptedOriginal(_) |
            MessageEvent::EncryptedRedacted(_) => {
                return;
            },
        }

        msg.html = msg.event.html();
    }

    pub fn insert_any_state(&mut self, msg: AnySyncStateEvent) {
        let event_id = msg.event_id().to_owned();
        let key = (msg.origin_server_ts().into(), event_id.clone());

        let loc = EventLocation::State(key.clone());
        self.keys.insert(event_id, loc);
        self.messages.insert_message(key, msg);
    }

    /// Indicates whether this room has unread messages.
    pub fn unreads(&self, settings: &ApplicationSettings) -> UnreadInfo {
        let last_message = self.messages.last_key_value();
        let last_receipt = self
            .user_receipts
            .get(&ReceiptThread::Main)
            .and_then(|receipts| receipts.get(&settings.profile.user_id));

        match (last_message, last_receipt) {
            (Some(((ts, recent), _)), Some(last_read)) => {
                UnreadInfo { unread: last_read != recent, latest: Some(*ts) }
            },
            (Some(((ts, _), _)), None) => {
                // If we've never loaded/generated a room's receipt (example,
                // a newly joined but never viewed room), show it as unread.
                UnreadInfo { unread: true, latest: Some(*ts) }
            },
            (None, _) => UnreadInfo::default(),
        }
    }

    /// Inserts events that couldn't be decrypted into the scrollback.
    pub fn insert_encrypted(&mut self, msg: RoomEncryptedEvent) {
        let event_id = msg.event_id().to_owned();
        let key = (msg.origin_server_ts().into(), event_id.clone());

        self.keys.insert(event_id, EventLocation::Message(None, key.clone()));
        self.messages.insert(key, msg.into());
    }

    /// Insert a new message.
    pub fn insert_message(&mut self, msg: RoomMessageEvent) {
        let event_id = msg.event_id().to_owned();
        let key = (msg.origin_server_ts().into(), event_id.clone());

        let loc = EventLocation::Message(None, key.clone());
        self.keys.insert(event_id, loc);
        self.messages.insert_message(key, msg);
    }

    fn insert_thread(&mut self, msg: RoomMessageEvent, thread_root: OwnedEventId) {
        let event_id = msg.event_id().to_owned();
        let key = (msg.origin_server_ts().into(), event_id.clone());

        let replies = self
            .threads
            .entry(thread_root.clone())
            .or_insert_with(|| Messages::thread(thread_root.clone()));
        let loc = EventLocation::Message(Some(thread_root), key.clone());
        self.keys.insert(event_id, loc);
        replies.insert_message(key, msg);
    }

    /// Insert a new message event.
    pub fn insert(&mut self, msg: RoomMessageEvent) {
        match msg {
            RoomMessageEvent::Original(OriginalRoomMessageEvent {
                content: RoomMessageEventContent { relates_to: Some(ref relates_to), .. },
                ..
            }) => {
                match relates_to {
                    Relation::Replacement(repl) => self.insert_edit(repl.clone()),
                    Relation::Thread(Thread { event_id, .. }) => {
                        let event_id = event_id.clone();
                        self.insert_thread(msg, event_id);
                    },
                    Relation::Reply { .. } => self.insert_message(msg),
                    _ => self.insert_message(msg),
                }
            },
            _ => self.insert_message(msg),
        }
    }

    /// Insert a new message event, and spawn a task for image-preview if it has an image
    /// attachment.
    pub fn insert_with_preview(
        &mut self,
        room_id: OwnedRoomId,
        store: AsyncProgramStore,
        picker: Option<Picker>,
        ev: RoomMessageEvent,
        settings: &mut ApplicationSettings,
        media: matrix_sdk::Media,
    ) {
        let source = picker.and_then(|_| source_from_event(&ev));
        self.insert(ev);

        if let Some((event_id, source)) = source {
            if let (Some(msg), Some(image_preview)) =
                (self.get_event_mut(&event_id), &settings.tunables.image_preview)
            {
                msg.image_preview = ImageStatus::Downloading(image_preview.size.clone());
                spawn_insert_preview(
                    store,
                    room_id,
                    event_id,
                    source,
                    media,
                    settings.dirs.image_previews.clone(),
                )
            }
        }
    }

    /// Indicates whether we've recently fetched scrollback for this room.
    pub fn recently_fetched(&self) -> bool {
        self.fetch_last.is_some_and(|i| i.elapsed() < ROOM_FETCH_DEBOUNCE)
    }

    fn clear_receipt(&mut self, thread: &ReceiptThread, user_id: &OwnedUserId) -> Option<()> {
        let old_event_id =
            self.user_receipts.get(thread).and_then(|receipts| receipts.get(user_id))?;
        let old_thread = self.event_receipts.get_mut(thread)?;
        let old_receipts = old_thread.get_mut(old_event_id)?;
        old_receipts.remove(user_id);

        if old_receipts.is_empty() {
            old_thread.remove(old_event_id);
        }
        if old_thread.is_empty() {
            self.event_receipts.remove(thread);
        }

        None
    }

    pub fn set_receipt(
        &mut self,
        thread: ReceiptThread,
        user_id: OwnedUserId,
        event_id: OwnedEventId,
    ) {
        self.clear_receipt(&thread, &user_id);
        self.event_receipts
            .entry(thread.clone())
            .or_default()
            .entry(event_id.clone())
            .or_default()
            .insert(user_id.clone());
        self.user_receipts.entry(thread).or_default().insert(user_id, event_id);
    }

    pub fn fully_read(&mut self, user_id: &UserId) {
        let Some(((_, event_id), _)) = self.messages.last_key_value() else {
            return;
        };

        self.set_receipt(ReceiptThread::Main, user_id.to_owned(), event_id.clone());

        let newest = self
            .threads
            .iter()
            .filter_map(|(thread_id, messages)| {
                let thread = ReceiptThread::Thread(thread_id.to_owned());

                messages
                    .last_key_value()
                    .map(|((_, event_id), _)| (thread, event_id.to_owned()))
            })
            .collect::<Vec<_>>();

        for (thread, event_id) in newest.into_iter() {
            self.set_receipt(thread, user_id.to_owned(), event_id.clone());
        }
    }

    pub fn receipts<'a>(
        &'a self,
        user_id: &'a UserId,
    ) -> impl Iterator<Item = (&'a ReceiptThread, &'a OwnedEventId)> + 'a {
        self.user_receipts
            .iter()
            .filter_map(move |(t, rs)| rs.get(user_id).map(|r| (t, r)))
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

    /// Checks if a given user has reacted with the given emoji on the given event
    pub fn user_reactions_contains(
        &mut self,
        user_id: &UserId,
        event_id: &EventId,
        emoji: &str,
    ) -> bool {
        if let Some(reactions) = self.reactions.get(event_id) {
            reactions
                .values()
                .any(|(annotation, user)| annotation == emoji && user == user_id)
        } else {
            false
        }
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

#[cfg(unix)]
fn picker_from_termios(protocol_type: Option<ProtocolType>) -> Option<Picker> {
    let mut picker = match Picker::from_query_stdio() {
        Ok(picker) => picker,
        Err(e) => {
            tracing::error!("Failed to setup image previews: {e}");
            return None;
        },
    };

    if let Some(protocol_type) = protocol_type {
        picker.set_protocol_type(protocol_type);
    }

    Some(picker)
}

/// Windows cannot guess the right protocol, and always needs type and font_size.
#[cfg(windows)]
fn picker_from_termios(_: Option<ProtocolType>) -> Option<Picker> {
    tracing::error!("\"image_preview\" requires \"protocol\" with \"type\" and \"font_size\" options on Windows.");
    None
}

fn picker_from_settings(settings: &ApplicationSettings) -> Option<Picker> {
    let image_preview = settings.tunables.image_preview.as_ref()?;
    let image_preview_protocol = image_preview.protocol.as_ref();

    if let Some(&ImagePreviewProtocolValues {
        r#type: Some(protocol_type),
        font_size: Some(font_size),
    }) = image_preview_protocol
    {
        // User forced type and font_size: use that.
        let mut picker = Picker::from_fontsize(font_size);
        picker.set_protocol_type(protocol_type);
        Some(picker)
    } else {
        // Guess, but use type if forced.
        picker_from_termios(image_preview_protocol.and_then(|p| p.r#type))
    }
}

/// Information gathered during server syncs about joined rooms.
#[derive(Default)]
pub struct SyncInfo {
    /// Spaces that the user is a member of.
    pub spaces: Vec<Arc<(MatrixRoom, Option<Tags>)>>,

    /// Rooms that the user is a member of.
    pub rooms: Vec<Arc<(MatrixRoom, Option<Tags>)>>,

    /// DMs that the user is a member of.
    pub dms: Vec<Arc<(MatrixRoom, Option<Tags>)>>,
}

impl SyncInfo {
    pub fn rooms(&self) -> impl Iterator<Item = &RoomId> {
        self.rooms.iter().map(|r| r.0.room_id())
    }

    pub fn dms(&self) -> impl Iterator<Item = &RoomId> {
        self.dms.iter().map(|r| r.0.room_id())
    }

    pub fn chats(&self) -> impl Iterator<Item = &RoomId> {
        self.rooms().chain(self.dms())
    }
}

static MESSAGE_NEED_TTL: u8 = 10;
#[derive(Debug, PartialEq)]
/// Load messages until the event is loaded or `ttl` loads are exceeded
pub struct MessageNeed {
    pub event_id: OwnedEventId,
    pub ttl: u8,
}

#[derive(Default, Debug, PartialEq)]
pub struct Need {
    pub members: bool,
    pub messages: Option<Vec<MessageNeed>>,
}

/// Things that need loading for different rooms.
#[derive(Default)]
pub struct RoomNeeds {
    needs: HashMap<OwnedRoomId, Need>,
}

impl RoomNeeds {
    /// Mark a room for needing to load members.
    pub fn need_members(&mut self, room_id: OwnedRoomId) {
        self.needs.entry(room_id).or_default().members = true;
    }

    /// Mark a room for needing to load messages.
    pub fn need_messages(&mut self, room_id: OwnedRoomId) {
        self.needs.entry(room_id).or_default().messages.get_or_insert_default();
    }

    /// Mark a room for needing to load messages until the given message is loaded or a retry limit
    /// is exceeded.
    pub fn need_message(&mut self, room_id: OwnedRoomId, event_id: OwnedEventId) {
        let messages = &mut self.needs.entry(room_id).or_default().messages.get_or_insert_default();

        messages.push(MessageNeed { event_id, ttl: MESSAGE_NEED_TTL });
    }

    pub fn need_messages_all(&mut self, room_id: OwnedRoomId, message_needs: Vec<MessageNeed>) {
        self.needs
            .entry(room_id)
            .or_default()
            .messages
            .get_or_insert_default()
            .extend(message_needs);
    }

    pub fn rooms(&self) -> usize {
        self.needs.len()
    }
}

impl IntoIterator for RoomNeeds {
    type Item = (OwnedRoomId, Need);
    type IntoIter = IntoIter<OwnedRoomId, Need>;

    fn into_iter(self) -> Self::IntoIter {
        self.needs.into_iter()
    }
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
    pub need_load: RoomNeeds,

    /// [CompletionMap] of Emoji shortcodes.
    pub emojis: CompletionMap<String, &'static Emoji>,

    /// Information gathered by the background thread.
    pub sync_info: SyncInfo,

    /// Image preview "protocol" picker.
    pub picker: Option<Picker>,

    /// Last draw time, used to match with RoomInfo's draw_last.
    pub draw_curr: Option<Instant>,

    /// Whether to ring the terminal bell on the next redraw.
    pub ring_bell: bool,

    /// Whether the application is currently focused
    pub focused: bool,

    /// Collator for locale-aware text sorting.
    pub collator: feruca::Collator,
}

impl ChatStore {
    /// Create a new [ChatStore].
    pub fn new(worker: Requester, settings: ApplicationSettings) -> Self {
        let picker = picker_from_settings(&settings);

        ChatStore {
            worker,
            settings,
            picker,
            cmds: crate::commands::setup_commands(),
            emojis: emoji_map(),

            collator: Default::default(),
            names: Default::default(),
            rooms: Default::default(),
            presences: Default::default(),
            verifications: Default::default(),
            need_load: Default::default(),
            sync_info: Default::default(),
            draw_curr: None,
            ring_bell: false,
            focused: true,
        }
    }

    /// Get a joined room.
    pub fn get_joined_room(&self, room_id: &RoomId) -> Option<MatrixRoom> {
        let room = self.worker.client.get_room(room_id)?;

        if room.state() == MatrixRoomState::Joined {
            Some(room)
        } else {
            None
        }
    }

    /// Get the title for a room.
    pub fn get_room_title(&self, room_id: &RoomId) -> String {
        self.rooms
            .get(room_id)
            .and_then(|i| i.name.as_ref())
            .map(String::from)
            .unwrap_or_else(|| "Untitled Matrix Room".to_string())
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
    /// A Matrix room, with an optional thread to show.
    Room(OwnedRoomId, Option<OwnedEventId>),

    /// The `:dms` window.
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

    /// The `:chats` window.
    ChatList,

    /// The `:unreads` window.
    UnreadList,
}

impl Display for IambId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IambId::Room(room_id, None) => {
                write!(f, "iamb://room/{room_id}")
            },
            IambId::Room(room_id, Some(thread)) => {
                write!(f, "iamb://room/{room_id}/threads/{thread}")
            },
            IambId::MemberList(room_id) => {
                write!(f, "iamb://members/{room_id}")
            },
            IambId::DirectList => f.write_str("iamb://dms"),
            IambId::RoomList => f.write_str("iamb://rooms"),
            IambId::SpaceList => f.write_str("iamb://spaces"),
            IambId::VerifyList => f.write_str("iamb://verify"),
            IambId::Welcome => f.write_str("iamb://welcome"),
            IambId::ChatList => f.write_str("iamb://chats"),
            IambId::UnreadList => f.write_str("iamb://unreads"),
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

impl Visitor<'_> for IambIdVisitor {
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

                match *path.collect::<Vec<_>>().as_slice() {
                    [room_id] => {
                        let Ok(room_id) = OwnedRoomId::try_from(room_id) else {
                            return Err(E::custom("Invalid room identifier"));
                        };

                        Ok(IambId::Room(room_id, None))
                    },
                    [room_id, "threads", thread_root] => {
                        let Ok(room_id) = OwnedRoomId::try_from(room_id) else {
                            return Err(E::custom("Invalid room identifier"));
                        };

                        let Ok(thread_root) = OwnedEventId::try_from(thread_root) else {
                            return Err(E::custom("Invalid thread root identifier"));
                        };

                        Ok(IambId::Room(room_id, Some(thread_root)))
                    },
                    _ => return Err(E::custom("Invalid members window URL")),
                }
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
            Some("chats") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://chats takes no path"));
                }

                Ok(IambId::ChatList)
            },
            Some("unreads") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://unreads takes no path"));
                }

                Ok(IambId::UnreadList)
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

    pub fn toggle(&mut self) {
        *self = match self {
            RoomFocus::MessageBar => RoomFocus::Scrollback,
            RoomFocus::Scrollback => RoomFocus::MessageBar,
        };
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
    Room(OwnedRoomId, Option<OwnedEventId>, RoomFocus),

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

    /// The `:chats` window.
    ChatList,

    /// The `:unreads` window.
    UnreadList,
}

impl IambBufferId {
    /// Get the identifier for the window that contains this buffer.
    pub fn to_window(&self) -> Option<IambId> {
        let id = match self {
            IambBufferId::Command(_) => return None,
            IambBufferId::Room(room, thread, _) => IambId::Room(room.clone(), thread.clone()),
            IambBufferId::DirectList => IambId::DirectList,
            IambBufferId::MemberList(room) => IambId::MemberList(room.clone()),
            IambBufferId::RoomList => IambId::RoomList,
            IambBufferId::SpaceList => IambId::SpaceList,
            IambBufferId::VerifyList => IambId::VerifyList,
            IambBufferId::Welcome => IambId::Welcome,
            IambBufferId::ChatList => IambId::ChatList,
            IambBufferId::UnreadList => IambId::UnreadList,
        };

        Some(id)
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
            IambBufferId::Room(_, _, RoomFocus::MessageBar) => complete_msgbar(text, cursor, store),
            IambBufferId::Room(_, _, RoomFocus::Scrollback) => vec![],

            IambBufferId::DirectList => vec![],
            IambBufferId::MemberList(_) => vec![],
            IambBufferId::RoomList => vec![],
            IambBufferId::SpaceList => vec![],
            IambBufferId::VerifyList => vec![],
            IambBufferId::Welcome => vec![],
            IambBufferId::ChatList => vec![],
            IambBufferId::UnreadList => vec![],
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
        "download" | "keys" | "open" | "upload" => complete_path(text, cursor),
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
    let slice = text.slice(..eo);
    let cow = Cow::from(&slice);

    complete_cmd(cow.as_ref(), text, cursor, store)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::config::user_style_from_color;
    use crate::tests::*;
    use matrix_sdk::ruma::{
        events::{reaction::ReactionEventContent, relation::Annotation, MessageLikeUnsigned},
        owned_event_id,
        owned_room_id,
        owned_user_id,
        MilliSecondsSinceUnixEpoch,
    };
    use pretty_assertions::assert_eq;
    use ratatui::style::Color;

    #[test]
    fn multiple_identical_reactions() {
        let mut info = RoomInfo::default();

        let content = ReactionEventContent::new(Annotation::new(
            owned_event_id!("$my_reaction"),
            "🏠".to_owned(),
        ));

        for i in 0..3 {
            let event_id = format!("$house_{}", i);
            info.insert_reaction(MessageLikeEvent::Original(
                matrix_sdk::ruma::events::OriginalMessageLikeEvent {
                    content: content.clone(),
                    event_id: OwnedEventId::from_str(&event_id).unwrap(),
                    sender: owned_user_id!("@foo:example.org"),
                    origin_server_ts: MilliSecondsSinceUnixEpoch::now(),
                    room_id: owned_room_id!("!foo:example.org"),
                    unsigned: MessageLikeUnsigned::new(),
                },
            ));
        }

        let content = ReactionEventContent::new(Annotation::new(
            owned_event_id!("$my_reaction"),
            "🙂".to_owned(),
        ));

        for i in 0..2 {
            let event_id = format!("$smile_{}", i);
            info.insert_reaction(MessageLikeEvent::Original(
                matrix_sdk::ruma::events::OriginalMessageLikeEvent {
                    content: content.clone(),
                    event_id: OwnedEventId::from_str(&event_id).unwrap(),
                    sender: owned_user_id!("@foo:example.org"),
                    origin_server_ts: MilliSecondsSinceUnixEpoch::now(),
                    room_id: owned_room_id!("!foo:example.org"),
                    unsigned: MessageLikeUnsigned::new(),
                },
            ));
        }

        for i in 2..4 {
            let event_id = format!("$smile_{}", i);
            info.insert_reaction(MessageLikeEvent::Original(
                matrix_sdk::ruma::events::OriginalMessageLikeEvent {
                    content: content.clone(),
                    event_id: OwnedEventId::from_str(&event_id).unwrap(),
                    sender: owned_user_id!("@bar:example.org"),
                    origin_server_ts: MilliSecondsSinceUnixEpoch::now(),
                    room_id: owned_room_id!("!foo:example.org"),
                    unsigned: MessageLikeUnsigned::new(),
                },
            ));
        }

        assert_eq!(info.get_reactions(&owned_event_id!("$my_reaction")), vec![
            ("🏠", 1),
            ("🙂", 2)
        ]);
    }

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

    #[test]
    fn test_need_load() {
        let room_id = TEST_ROOM1_ID.clone();

        let mut need_load = RoomNeeds::default();

        need_load.need_messages(room_id.clone());
        need_load.need_members(room_id.clone());

        assert_eq!(need_load.into_iter().collect::<Vec<(OwnedRoomId, Need)>>(), vec![(
            room_id,
            Need { members: true, messages: Some(Vec::new()) }
        )],);
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
