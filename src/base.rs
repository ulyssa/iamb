//! # Common types and utilities
//!
//! The types defined here get used throughout iamb.

use std::collections::hash_map::IntoIter;
use std::collections::{BTreeSet, HashSet};
use std::path::PathBuf;

use emojis::Emoji;
use matrix_sdk::Client;
use matrix_sdk::ruma::events::poll::start::PollStartEvent;
use matrix_sdk::ruma::events::poll::unstable_start::{
    UnstablePollStartEvent,
    UnstablePollStartEventContent,
};
use matrix_sdk::ruma::events::reaction::ReactionEvent;
use matrix_sdk::ruma::events::relation::Replacement;
use matrix_sdk::ruma::events::room::encrypted::RoomEncryptedEvent;
use matrix_sdk::ruma::events::room::message::RelationWithoutReplacement;
use matrix_sdk::ruma::events::room::message::{
    RoomMessageEvent,
    RoomMessageEventContentWithoutRelation,
};
use matrix_sdk::ruma::events::room::redaction::{
    OriginalSyncRoomRedactionEvent,
    SyncRoomRedactionEvent,
};
use matrix_sdk::ruma::events::sticker::{StickerEvent, StickerEventContent};
use matrix_sdk::ruma::events::{MessageLikeEvent, OriginalMessageLikeEvent};
use matrix_sdk::ruma::presence::PresenceState;
use matrix_sdk::ruma::room::{AllowRule, Restricted};
use matrix_sdk::ruma::{OwnedMxcUri, OwnedTransactionId, RoomVersionId};
use modalkit::editing::application::{
    ApplicationAction,
    ApplicationContentId,
    ApplicationError,
    ApplicationInfo,
    ApplicationStore,
    ApplicationWindowId,
};
use modalkit::editing::completion::CompletionMap;
use modalkit::editing::context::EditContext;
use modalkit::editing::store::Store;
use modalkit::env::vim::command::{CommandContext, VimCommand, VimCommandMachine};
use modalkit::env::vim::keybindings::VimMachine;
use modalkit::errors::UIResult;
use modalkit::keybindings::SequenceStatus;
use percent_encoding::{NON_ALPHANUMERIC, percent_decode, percent_encode};
use serde::de::Error as SerdeError;
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use strum::VariantNames;
use tokio::sync::Mutex as AsyncMutex;

use crate::config::reload::{ReloadError, TunablesUpdate};
use crate::message::poll::{
    Poll,
    PollEventLocation,
    PollRelation,
    UnloadedPoll,
    UnloadedUnstablePoll,
    UnstablePoll,
    UnstablePollRelation,
};
use crate::notifications::NotificationHandle;
use crate::prelude::*;

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

    /// Start an interactive (SAS) emoji verification
    Emoji,
}

/// An action taken against a room's timeline.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TimelineAction {
    /// Jump to a loaded message in the scrollback.
    GotoEvent(OwnedEventId),
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
    Download(Option<PathBuf>, DownloadFlags),

    /// Edit a sent message.
    Edit,

    /// React to a message with an Emoji.
    ///
    /// `:react` will by default try to convert the [String] argument to an Emoji, and error when
    /// it doesn't recognize it. The second [bool] argument forces it to be interpreted literally
    /// when it is `true`.
    React(String, bool),

    /// Pin a message to the room.
    Pin,

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

    /// Unpin a message from the room.
    Unpin,
}

/// An action taken in the currently selected space.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SpaceAction {
    /// Add a room or update metadata.
    SetChild {
        /// The room that should be added to the space.
        child: OwnedRoomOrAliasId,
        /// The order parameter to use when sorting children in the space.
        order: Option<String>,
        /// Whether the room is suggested.
        suggested: bool,
    },

    /// Remove the selected room.
    RemoveChild,
}

/// The type of room being created.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CreateRoomType {
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
#[derive(Clone, Debug, Eq, PartialEq, VariantNames)]
#[strum(serialize_all = "lowercase")]
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
    #[strum(serialize = "id")]
    RoomId,

    /// Sort rooms by the server portion of their canonical room alias.
    ///
    /// If the room has no canonical alias, and the room identifier uses the version 1 syntax
    /// for formatting the MXID, then this will fall back to using the server portion of the
    /// identifier (aka, the "namespace").
    Server,

    /// Sort rooms by whether they have unread messages.
    Unread,

    /// Sort rooms by the timestamps of their most recent messages.
    Recent,

    /// Sort rooms by whether they are invites.
    Invite,
}

/// Fields that users can be sorted by.
#[derive(Clone, Debug, Eq, PartialEq, VariantNames)]
#[strum(serialize_all = "lowercase")]
pub enum SortFieldUser {
    #[strum(serialize = "power")]
    PowerLevel,
    #[strum(serialize = "id")]
    UserId,
    LocalPart,
    Server,
    Knock,
    Invite,
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
pub(crate) struct SortRoomVisitor;

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
            "server" => SortFieldRoom::Server,
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
pub(crate) struct SortUserVisitor;

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
            "knock" => SortFieldUser::Knock,
            "invite" => SortFieldUser::Invite,
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
    /// The room's join rules, aka who can access this room.
    Access,

    /// The room's history visibility.
    History,

    /// The room name.
    Name,

    /// The room version.
    Version,

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

    /// The users own display name.
    UserName,
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

/// An internal version of [`JoinRule`]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambJoinRule {
    Public,
    Restricted(Vec<OwnedRoomOrAliasId>),
    Knock,
    KnockRestricted(Vec<OwnedRoomOrAliasId>),
    Invite,
}

impl IambJoinRule {
    pub async fn into_join_rule(self, client: &Client) -> Result<JoinRule, IambError> {
        async fn resolve_aliases(
            rooms: Vec<OwnedRoomOrAliasId>,
            client: &Client,
        ) -> Result<Restricted, IambError> {
            let mut allow = vec![];
            for room in rooms {
                let alias = match OwnedRoomId::try_from(room) {
                    Ok(room_id) => {
                        allow.push(AllowRule::room_membership(room_id));
                        continue;
                    },
                    Err(alias) => alias,
                };

                let resp = client.resolve_room_alias(&alias).await?;

                allow.push(AllowRule::room_membership(resp.room_id));
            }

            Ok(Restricted::new(allow))
        }

        Ok(match self {
            Self::Public => JoinRule::Public,
            Self::Invite => JoinRule::Invite,
            Self::Knock => JoinRule::Knock,
            Self::Restricted(rooms) => JoinRule::Restricted(resolve_aliases(rooms, client).await?),
            Self::KnockRestricted(rooms) => {
                JoinRule::KnockRestricted(resolve_aliases(rooms, client).await?)
            },
        })
    }
}

/// An action that operates on a focused room.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RoomAction {
    /// Follow the room upgrade information.
    Follow(Box<CommandContext>, MoveDir1D),

    /// Accept an invitation to join this room.
    InviteAccept,

    /// Reject an invitation to join this room.
    InviteReject,

    /// Invite a user to this room.
    InviteSend(OwnedUserId),

    /// Accept a knock from someone who wants to join this room.
    KnockAccept(OwnedUserId),

    /// Reject a knock from someone who wants to join this room.
    KnockReject(OwnedUserId, Option<String>),

    /// Reject a knock from someone who wants to join this room and ban them
    /// to prevent them from being able to try knocking again.
    KnockBan(OwnedUserId, Option<String>),

    /// Leave this room.
    Leave(bool),

    /// Update a user's membership in this room.
    MemberUpdate(MemberUpdateAction, String, Option<String>, bool),

    /// Open the members window.
    Members(Box<CommandContext>),

    /// Open the pinned messages window.
    Pinned(Box<CommandContext>),

    /// Set whether a room is a direct message.
    SetDirect(bool),

    /// Set the join rules for a room to control who can access it and how.
    SetAccess(IambJoinRule),

    /// Set a room property.
    Set(RoomField, String),

    /// Unset a room property.
    Unset(RoomField),

    /// Upgrade the version of a room.
    Upgrade(RoomVersionId, Vec<OwnedUserId>, bool),

    /// List the values in a list room property.
    Show(RoomField),

    /// Mark the room as read/unread.
    SetUnread(bool),
}

/// An action that joins  a room.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum JoinAction {
    /// Join the focused room.
    Join,

    /// Knock on the focused room.
    Knock,
}

/// An action that sends a message to a room.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SendAction {
    /// Send the text in the message bar.
    Submit,

    /// Send text provided from an external editor.
    SubmitFromEditor,

    /// Upload a file.
    ///
    /// The second argument indicates whether to use the messagebar as a caption, don't use it or
    /// ask the user.
    Upload(PathBuf, Option<bool>),

    /// Upload the image data.
    ///
    /// The [`bool`] arguments indicates whether to use the messagebar as a caption.
    UploadImage(usize, usize, Cow<'static, [u8]>, bool),
}

/// An action performed against the user's homeserver.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HomeserverAction {
    /// Create a new room with an optional localpart.
    CreateRoom(Option<String>, CreateRoomType, CreateRoomFlags),

    /// "Knock" on a room, aka "request to join".
    KnockSend(OwnedRoomOrAliasId, Option<String>),

    /// Logout the current iamb session on the homeserver.
    Logout(String, bool),

    /// Forget all left rooms
    Forget,

    /// Set a profile field.
    ProfileFieldSet(ProfileFieldValue),

    /// Set a profile field.
    ProfileFieldUnset(ProfileFieldName),

    /// Set a profile field.
    ProfileFieldShow(ProfileFieldName),
}

/// An action performed against the user's room keys.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum KeysAction {
    /// Export room keys to a file, encrypted with a passphrase.
    Export(String, String),
    /// Import room keys from a file, encrypted with a passphrase.
    Import(String, String),
}

/// An action performed on the application settings.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SettingsAction {
    /// Change some settings.
    Set(Vec<TunablesUpdate>),

    /// Reload the (specified) config file.
    Reload(Option<PathBuf>),
}

/// An action that the main program loop should execute.
///
/// See [the commands module][super::commands] for where these are usually created.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambAction {
    /// Perform an action against the homeserver.
    Homeserver(HomeserverAction),

    /// Perform an action against a room's timeline.
    Timeline(TimelineAction),

    /// Perform an action over room keys.
    Keys(KeysAction),

    /// Perform an action on the currently selected message.
    Message(MessageAction),

    /// Perform an action on the current space.
    Space(SpaceAction),

    /// Perform an action on the application settings.
    Settings(SettingsAction),

    /// Open a URL.
    OpenLink(String),

    /// Perform an action on the currently focused room.
    Room(RoomAction),

    /// Join the focused room preview.
    Join(JoinAction),

    /// Send a message to the currently focused room.
    Send(SendAction),

    /// Perform an action for an in-progress verification.
    Verify(VerifyAction, String),

    /// Request a new verification with the specified user.
    VerifyRequest(String),

    /// Recover the encryption secrets for this session with the given recovery key.
    Recover(String),

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

impl From<SettingsAction> for IambAction {
    fn from(act: SettingsAction) -> Self {
        IambAction::Settings(act)
    }
}

impl From<RoomAction> for IambAction {
    fn from(act: RoomAction) -> Self {
        IambAction::Room(act)
    }
}

impl From<JoinAction> for IambAction {
    fn from(act: JoinAction) -> Self {
        IambAction::Join(act)
    }
}

impl From<SendAction> for IambAction {
    fn from(act: SendAction) -> Self {
        IambAction::Send(act)
    }
}

impl From<TimelineAction> for IambAction {
    fn from(act: TimelineAction) -> Self {
        IambAction::Timeline(act)
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
            IambAction::Settings(..) => SequenceStatus::Break,
            IambAction::Timeline(..) => SequenceStatus::Break,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Break,
            IambAction::Verify(..) => SequenceStatus::Break,
            IambAction::VerifyRequest(..) => SequenceStatus::Break,
            IambAction::Recover(..) => SequenceStatus::Break,
            IambAction::Join(..) => SequenceStatus::Break,
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
            IambAction::Settings(..) => SequenceStatus::Atom,
            IambAction::Timeline(..) => SequenceStatus::Atom,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Atom,
            IambAction::Verify(..) => SequenceStatus::Atom,
            IambAction::VerifyRequest(..) => SequenceStatus::Atom,
            IambAction::Recover(..) => SequenceStatus::Atom,
            IambAction::Join(..) => SequenceStatus::Atom,
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
            IambAction::Settings(..) => SequenceStatus::Ignore,
            IambAction::Timeline(..) => SequenceStatus::Ignore,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Ignore,
            IambAction::Verify(..) => SequenceStatus::Ignore,
            IambAction::VerifyRequest(..) => SequenceStatus::Ignore,
            IambAction::Recover(..) => SequenceStatus::Ignore,
            IambAction::Join(..) => SequenceStatus::Ignore,
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
            IambAction::Settings(..) => false,
            IambAction::OpenLink(..) => false,
            IambAction::Timeline(..) => false,
            IambAction::ToggleScrollbackFocus => false,
            IambAction::Verify(..) => false,
            IambAction::VerifyRequest(..) => false,
            IambAction::Recover(..) => false,
            IambAction::Join(..) => false,
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

impl From<TimelineAction> for ProgramAction {
    fn from(act: TimelineAction) -> Self {
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
pub type MessageReactions = HashMap<OwnedEventId, (String, OwnedUserId, Option<MediaSource>)>;

pub type MessageEdits = BTreeMap<MessageKey, RoomMessageEventContentWithoutRelation>;

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

    /// An HTTP error.
    #[error("HTTP client error: {0}")]
    Http(#[from] matrix_sdk::HttpError),

    /// A failure from the Matrix client.
    #[error("Matrix client error: {0}")]
    Matrix(#[from] matrix_sdk::Error),

    /// A failure when sending a message.
    #[error("Send queue error: {0}")]
    SendQueue(#[from] matrix_sdk::send_queue::RoomSendQueueError),

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

    /// An invalid space child order was specified.
    #[error("Invalid space child order: {0}")]
    InvalidSpaceChildOrder(matrix_sdk::ruma::IdParseError),

    /// A failure occurred during verification.
    #[error("Verification request error: {0}")]
    VerificationRequestError(#[from] matrix_sdk::encryption::identities::RequestVerificationError),

    /// A failure occurred while recovering the encryption secrets.
    #[error("Recovery error: {0}")]
    RecoveryError(#[from] matrix_sdk::encryption::recovery::RecoveryError),

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

    /// A generic error that doesn't need a specific error type.
    #[error("{0}")]
    Custom(String),

    /// Config couldn't be reloaded
    #[error("Reload error: {0}")]
    ConfigReload(#[from] ReloadError),
}

impl From<IambError> for UIError<IambInfo> {
    fn from(err: IambError) -> Self {
        UIError::Application(err)
    }
}

impl From<matrix_sdk::event_cache::EventCacheError> for IambError {
    fn from(value: matrix_sdk::event_cache::EventCacheError) -> Self {
        Self::from(matrix_sdk::Error::from(value))
    }
}

impl ApplicationError for IambError {}

/// Indicates where an [EventId] lives in the [ChatStore].
#[derive(Clone)]
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

    /// The [EventId] belongs to an edit for the given event and has key [MessageKey].
    Edit(OwnedEventId, MessageKey),

    /// The [EventId] belongs to an aggregation for the given poll.
    Poll(OwnedEventId, PollEventLocation),
}

impl EventLocation {
    fn to_message_key(&self) -> Option<&MessageKey> {
        match self {
            EventLocation::Message(_, key) => Some(key),
            EventLocation::State(key) => Some(key),
            _ => None,
        }
    }
    fn to_thread_root(&self) -> Option<&EventId> {
        match self {
            EventLocation::Message(root, _) => root.as_deref(),
            _ => None,
        }
    }
}

/// Indicates where a local echo lives in the [`ChatStore`].
#[derive(Debug, Clone)]
pub enum EchoLocation {
    /// The [`OwnedTransactionId`] belongs to a message.
    ///
    /// If the first argument is [`None`], then it's part of the main scrollback. When [`Some`], it
    /// specifies which thread it's in reply to.
    Message(Option<OwnedEventId>, MessageKey),

    /// The local echo has been replaced by an event with this event id.
    Replaced(OwnedEventId),
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct UnreadInfo {
    pub(crate) unread_mark: bool,
    pub(crate) unread_messages: u64,
    pub(crate) unread_notifications: u64,
    pub(crate) unread_mentions: u64,
    pub(crate) latest: Option<MessageTimeStamp>,
}

impl UnreadInfo {
    pub fn is_unread(&self) -> bool {
        self.unread_mark || self.unread_notifications > 0 || self.unread_mentions > 0
    }

    pub fn has_mention(&self) -> bool {
        self.unread_mentions > 0
    }

    pub fn latest(&self) -> Option<&MessageTimeStamp> {
        self.latest.as_ref()
    }
}

/// The [`OwnedUserId`]s for users with the given displayname in [`DisplayNameStore`].
#[derive(Default)]
struct DisplayNameUsers {
    /// Joined or invited users. Count towards username disambiguation.
    active: HashSet<OwnedUserId>,

    /// Left and knocking users. Are always disambiguated.
    inactive: HashSet<OwnedUserId>,
}

/// Track the display names for users and render any needed disambiguation for
/// those with overlapping names.
#[derive(Default)]
pub struct DisplayNameStore {
    /// The boolean is the same `is_active` field as the argument to [`Self::set`].
    by_ids: CompletionMap<OwnedUserId, (Option<String>, bool)>,

    by_names: CompletionMap<String, DisplayNameUsers>,
}

impl DisplayNameStore {
    /// Update the `HashSet` associated with a given displayname.
    ///
    /// Note that this *could* be done more elegantly using the Entry API, but
    /// is intentionally written in a way to avoid cloning conflicting display
    /// names.
    fn set_by_name(&mut self, user_id: OwnedUserId, name: &str, is_active: bool) {
        if let Some(existing) = self.by_names.get_mut(name) {
            if is_active {
                existing.active.insert(user_id);
            } else {
                existing.inactive.insert(user_id);
            }
        } else {
            let mut value = DisplayNameUsers::default();
            if is_active {
                value.active.insert(user_id);
            } else {
                value.inactive.insert(user_id);
            }
            self.by_names.insert(name.to_owned(), value);
        }
    }

    /// Track a new user ID to displayname mapping, or unset any existing ones. `is_active` tracks,
    /// whether the user is an active member (invited or joined) or not.
    pub fn set(&mut self, user_id: OwnedUserId, name: Option<String>, is_active: bool) {
        if let Some(name) = name.as_deref() {
            self.set_by_name(user_id.clone(), name, is_active);
        }

        if self
            .by_ids
            .get(&user_id)
            .is_some_and(|(n, a)| *n == name && *a == is_active)
        {
            // nothing to do
            return;
        }

        let previous = self.by_ids.insert(user_id.to_owned(), (name, is_active));

        let Some((Some(name), was_active)) = previous else {
            // no previous entry in `self.by_names` to remove
            return;
        };

        let Some(users) = self.by_names.get_mut(&name) else {
            return;
        };

        if was_active {
            users.active.remove(&user_id);
        } else {
            users.inactive.remove(&user_id);
        }

        if users.active.is_empty() && users.inactive.is_empty() {
            self.by_names.remove(&name);
        }
    }

    pub fn get<'a>(&'a self, user_id: &UserId) -> Option<Cow<'a, str>> {
        let (displayname, is_active) = self.by_ids.get(user_id)?;
        let displayname = displayname.as_ref()?;
        let users = self.by_names.get(displayname)?;

        if !users.active.contains(user_id) && !users.inactive.contains(user_id) {
            // Internal consistency error? Assume no display name:
            return None;
        }

        // Inactive members are always assumed to be ambiguous.
        if *is_active && users.active.len() == 1 {
            // Unambiguous!
            return Some(Cow::Borrowed(displayname.as_str()));
        }

        // Ambiguous username, so include unique user ID:
        Some(Cow::Owned(format!("{displayname} ({user_id})")))
    }

    pub fn complete_mention(&self, prefix: &str) -> Vec<String> {
        // spec says to mention with display name in anchor text
        let mut users: BTreeSet<_> = self
            .by_names
            .complete(prefix.strip_prefix('@').unwrap_or(prefix))
            .into_iter()
            .flat_map(|name| {
                let users = self.by_names.get(&name).unwrap();
                users.active.iter().map(move |id| format!("[{name}][{}]", id))
            })
            .collect();

        users.extend(self.by_ids.complete(prefix).into_iter().map(|id| {
            let name = self
                .by_ids
                .get(&id)
                .and_then(|(name, _)| name.as_deref())
                .unwrap_or(id.as_str());
            format!("[{}][{}]", name, id)
        }));

        users.into_iter().collect()
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
    pub echo_keys: HashMap<OwnedTransactionId, EchoLocation>,

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
    /// A map of message identifiers to a list of edit events for message that are not yet cached.
    pub unloaded_edits: HashMap<OwnedEventId, MessageEdits>,
    /// A map of message identifiers to the aggregated data for a poll that hasn't been loaded.
    pub unloaded_polls: HashMap<OwnedEventId, UnloadedPoll>,
    pub unloaded_unstable_polls: HashMap<OwnedEventId, UnloadedUnstablePoll>,

    /// A map of message identifiers to thread replies.
    threads: HashMap<OwnedEventId, Messages>,

    /// Whether the scrollback for this room is currently being fetched.
    pub fetching: bool,

    /// Whether all messages are loaded.
    pub reached_timeline_start: bool,

    /// The time that we last fetched scrollback for this room.
    pub fetch_last: Option<Instant>,

    /// Users currently typing in this room, and when we received notification of them doing so.
    pub users_typing: Option<(Instant, Vec<OwnedUserId>)>,

    /// The display names for users in this room.
    pub display_names: DisplayNameStore,

    /// The last time the room was rendered, used to detect if it is currently open.
    pub draw_last: Option<Instant>,

    /// The room's pinned events, mirrored from the SDK's room state for rendering.
    pub pinned_events: Vec<OwnedEventId>,

    /// Pinned events fetched for the `:pinned` window that aren't in the loaded scrollback.
    pub pinned_previews: HashMap<OwnedEventId, Message>,

    /// How many times fetching each pinned event for the `:pinned` window has failed.
    pub pinned_failures: HashMap<OwnedEventId, u8>,
}

impl Default for RoomInfo {
    fn default() -> Self {
        Self {
            messages: Messages::new(ReceiptThread::Main),

            name: Default::default(),
            tags: Default::default(),
            keys: Default::default(),
            echo_keys: Default::default(),
            event_receipts: Default::default(),
            user_receipts: Default::default(),
            reactions: Default::default(),
            threads: Default::default(),
            fetching: Default::default(),
            reached_timeline_start: false,
            fetch_last: Default::default(),
            users_typing: Default::default(),
            display_names: Default::default(),
            draw_last: Default::default(),
            pinned_events: Default::default(),
            pinned_previews: Default::default(),
            pinned_failures: Default::default(),
            unloaded_edits: Default::default(),
            unloaded_polls: Default::default(),
            unloaded_unstable_polls: Default::default(),
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
    /// This does not apply edits to the returned event.
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

        if let MessageEvent::Original(ev, _) = &msg {
            Some(ev)
        } else {
            None
        }
    }

    /// Whether a message is pinned to the room.
    pub fn is_pinned(&self, event_id: &EventId) -> bool {
        self.pinned_events.iter().any(|id| id == event_id)
    }

    /// Get a pinned message, from the scrollback if it's loaded or else from the fetched previews.
    pub fn get_pinned(&self, event_id: &EventId) -> Option<&Message> {
        self.get_event(event_id).or_else(|| self.pinned_previews.get(event_id))
    }

    /// Whether fetching a pinned event has failed too many times to keep retrying.
    pub fn pinned_unavailable(&self, event_id: &EventId) -> bool {
        self.pinned_failures
            .get(event_id)
            .is_some_and(|failures| *failures >= PINNED_FETCH_ATTEMPTS)
    }

    /// Record the result of fetching a pinned event for the `:pinned` window.
    pub fn insert_pinned(&mut self, event_id: OwnedEventId, msg: Option<Message>) {
        match msg {
            Some(msg) => {
                self.pinned_failures.remove(&event_id);
                self.pinned_previews.insert(event_id, msg);
            },
            None => {
                let failures = self.pinned_failures.entry(event_id).or_default();
                *failures = failures.saturating_add(1);
            },
        }
    }

    /// Pinned events that still need to be fetched for the `:pinned` window.
    pub fn missing_pinned(&self) -> Vec<OwnedEventId> {
        self.pinned_events
            .iter()
            .filter(|id| self.get_pinned(id).is_none() && !self.pinned_unavailable(id))
            .cloned()
            .collect()
    }

    /// Get where a loaded message lives, as its thread root and key.
    pub fn get_message_location(
        &self,
        event_id: &EventId,
    ) -> Option<(Option<&EventId>, &MessageKey)> {
        let loc = self.keys.get(event_id)?;

        Some((loc.to_thread_root(), loc.to_message_key()?))
    }

    pub fn get_receipt_thread(&self, event_id: &EventId) -> Option<ReceiptThread> {
        match self.keys.get(event_id)? {
            EventLocation::Message(None, _) | EventLocation::State(_) => Some(ReceiptThread::Main),
            EventLocation::Message(Some(root), _) => Some(ReceiptThread::Thread(root.clone())),
            _ => None,
        }
    }

    /// Get the reactions and their counts for a message.
    pub fn get_reactions(&self, event_id: &EventId) -> Vec<(&str, usize, &Option<MediaSource>)> {
        if let Some(reacts) = self.reactions.get(event_id) {
            let mut counts = HashMap::new();

            let mut seen_user_reactions = BTreeSet::new();

            for (key, user, source) in reacts.values() {
                if !seen_user_reactions.contains(&(key, user)) {
                    seen_user_reactions.insert((key, user));
                    let count = counts.entry(key.as_str()).or_insert((0, source));
                    count.0 += 1;
                }
            }

            let mut reactions = counts
                .into_iter()
                .map(|(key, (count, source))| (key, count, source))
                .collect::<Vec<_>>();
            reactions.sort_by_key(|item| (item.0, item.1));

            reactions
        } else {
            vec![]
        }
    }

    pub fn get_reaction_images(&self, event_id: &EventId) -> impl Iterator<Item = &MediaSource> {
        self.reactions
            .get(event_id)
            .map(HashMap::iter)
            .unwrap_or_default()
            .filter_map(|(_, (_, _, source))| source.as_ref())
    }

    /// Map an event identifier to its [MessageKey].
    pub fn get_message_key(&self, event_id: &EventId) -> Option<&MessageKey> {
        self.keys.get(event_id)?.to_message_key()
    }

    /// Get an event for an identifier.
    pub fn get_event(&self, event_id: &EventId) -> Option<&Message> {
        let loc = self.keys.get(event_id)?;

        let key = loc.to_message_key()?;
        let root = loc.to_thread_root();

        self.get_thread(root)?.get(key)
    }

    /// Get an event for an identifier as mutable.
    pub fn get_event_mut(&mut self, event_id: &EventId) -> Option<&mut Message> {
        let loc = self.keys.get(event_id)?.clone();

        let key = loc.to_message_key()?;
        let root = loc.to_thread_root();

        self.get_thread_mut(root.map(ToOwned::to_owned)).get_mut(key)
    }

    pub fn redact(&mut self, ev: OriginalSyncRoomRedactionEvent) {
        let Some(redacts) = &ev.redacts else {
            return;
        };

        match self.keys.get(redacts) {
            None => return,
            Some(EventLocation::Edit(msg_event_id, edit_key)) => {
                let edit_key = edit_key.clone();
                let msg_loc = self.keys.get(msg_event_id).cloned();
                if let Some(EventLocation::Message(thread, msg_key)) = msg_loc {
                    if let Some(msg) = self.get_thread_mut(thread).get_mut(&msg_key) {
                        msg.remove_edit(&edit_key);
                    }
                } else {
                    self.unloaded_edits
                        .get_mut(msg_event_id)
                        .and_then(|edits| edits.remove(&edit_key));
                }

                if let Some(msg) = self.messages.get_mut(&edit_key) {
                    let ev = SyncRoomRedactionEvent::Original(ev);
                    msg.redact(ev);
                }
            },
            Some(EventLocation::Poll(poll_event_id, loc)) => {
                let poll_event_id = poll_event_id.to_owned();
                let loc = loc.to_owned();
                if let Some(Message { event, .. }) = self.get_event_mut(&poll_event_id) {
                    match event {
                        MessageEvent::Poll(poll) => poll.redact(&loc),
                        MessageEvent::UnstablePoll(poll) => poll.redact(&loc),
                        _ => (),
                    }
                } else if let Some(poll) = self.unloaded_unstable_polls.get_mut(&poll_event_id) {
                    poll.redact(&loc);
                }
            },
            Some(EventLocation::State(key)) => {
                if let Some(msg) = self.messages.get_mut(key) {
                    let ev = SyncRoomRedactionEvent::Original(ev);
                    msg.redact(ev);
                }
            },
            Some(EventLocation::Message(None, key)) => {
                if let Some(msg) = self.messages.get_mut(key) {
                    let ev = SyncRoomRedactionEvent::Original(ev);
                    msg.redact(ev);
                }
            },
            Some(EventLocation::Message(Some(root), key)) => {
                if let Some(thread) = self.threads.get_mut(root) &&
                    let Some(msg) = thread.get_mut(key)
                {
                    let ev = SyncRoomRedactionEvent::Original(ev);
                    msg.redact(ev);
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
    fn insert_reaction(&mut self, react: ReactionEvent, source: Option<MediaSource>) {
        let MessageLikeEvent::Original(react) = react else {
            return;
        };
        let rel_id = react.content.relates_to.event_id;
        let key = react.content.relates_to.key;

        let message = self.reactions.entry(rel_id.clone()).or_default();
        let event_id = react.event_id;
        let user_id = react.sender;

        message.insert(event_id.clone(), (key, user_id, source));

        let loc = EventLocation::Reaction(rel_id);
        self.keys.insert(event_id, loc);
    }

    /// Insert a sticker
    pub fn insert_sticker_with_preview(
        &mut self,
        sticker: StickerEvent,
        settings: &ApplicationSettings,
        previews: &mut PreviewManager,
    ) {
        let event_id = sticker.event_id().to_owned();
        let sender = sticker.sender().to_owned();
        let key = MessageKey {
            ts: sticker.origin_server_ts().into(),
            id: event_id.clone().into(),
        };

        let thread_root = match &sticker {
            MessageLikeEvent::Original(OriginalMessageLikeEvent {
                content:
                    StickerEventContent {
                        relates_to: Some(Relation::Thread(Thread { event_id, .. })),
                        ..
                    },
                ..
            }) => Some(event_id.to_owned()),
            _ => None,
        };

        if let MessageLikeEvent::Original(OriginalMessageLikeEvent {
            content: StickerEventContent { source, .. },
            ..
        }) = &sticker &&
            settings.tunables.image_preview.enabled
        {
            let source = source.clone().into();
            previews.register_preview(settings, &source, PreviewKind::Message);
        }

        let loc = EventLocation::Message(thread_root.clone(), key.clone());
        self.keys.insert(event_id.clone(), loc);

        let thread = self.get_thread_mut(thread_root);
        thread.insert_message(key, sticker);
        self.set_implicit_receipt(sender, event_id);
    }

    /// Insert a reaction to a message.
    pub fn insert_reaction_with_preview(
        &mut self,
        react: ReactionEvent,
        settings: &ApplicationSettings,
        previews: &mut PreviewManager,
    ) {
        let MessageLikeEvent::Original(ref orig_react) = react else {
            return;
        };
        let image_uri = OwnedMxcUri::from(orig_react.content.relates_to.key.as_str());
        let source = if image_uri.is_valid() && settings.tunables.image_preview.enabled {
            Some(MediaSource::Plain(image_uri))
        } else {
            None
        };

        if settings.tunables.image_preview.enabled &&
            let Some(source) = source.as_ref()
        {
            previews.register_preview(settings, source, PreviewKind::Reaction);
        }

        self.insert_reaction(react, source);
    }

    /// Insert the start of a poll.
    pub fn insert_poll_start(&mut self, poll: PollStartEvent) {
        let sender = poll.sender().to_owned();
        let event_id = poll.event_id().to_owned();
        let key = MessageKey {
            ts: poll.origin_server_ts().into(),
            id: event_id.clone().into(),
        };

        match poll {
            MessageLikeEvent::Original(ev) => {
                if let Some(Relation::Replacement(Replacement {
                    event_id: poll_event_id,
                    new_content,
                    ..
                })) = ev.content.relates_to
                {
                    let loc = EventLocation::Poll(
                        poll_event_id.to_owned(),
                        PollEventLocation::Replacement(key.clone()),
                    );
                    self.keys.insert(event_id.clone(), loc);

                    if let Some(msg) = self.get_event_mut(&poll_event_id) {
                        let MessageEvent::Poll(poll) = &mut msg.event else {
                            tracing::warn!("encountered poll replacement for non-poll message");
                            return;
                        };

                        poll.replacements.insert(key, new_content);
                    } else {
                        self.unloaded_polls
                            .entry(poll_event_id)
                            .or_default()
                            .replacements
                            .insert(key, new_content);
                    }
                } else {
                    let thread_root = match &ev.content.relates_to {
                        Some(Relation::Thread(Thread { event_id, .. })) => {
                            Some(event_id.to_owned())
                        },
                        _ => None,
                    };

                    let loc = EventLocation::Message(thread_root.clone(), key.clone());
                    self.keys.insert(event_id.clone(), loc);

                    let unloaded = self.unloaded_polls.remove(&ev.event_id).unwrap_or_default();
                    let msg = MessageEvent::Poll(
                        Poll::new(ev.event_id, ev.sender.to_owned(), ev.content, unloaded).into(),
                    );
                    let msg = Message::new(msg, ev.sender, ev.origin_server_ts.into());

                    self.get_thread_mut(thread_root).insert_message(key, msg);
                }
            },
            MessageLikeEvent::Redacted(ev) => {
                let loc = EventLocation::Message(None, key.clone());
                let message: Message = ev.into();
                self.keys.insert(event_id.clone(), loc);
                self.messages.insert_message(key, message);
            },
        }

        self.set_implicit_receipt(sender, event_id);
    }

    /// Insert the start of a poll.
    pub fn insert_unstable_poll_start(&mut self, poll: UnstablePollStartEvent) {
        let sender = poll.sender().to_owned();
        let event_id = poll.event_id().to_owned();
        let key = MessageKey {
            ts: poll.origin_server_ts().into(),
            id: event_id.clone().into(),
        };

        match poll {
            MessageLikeEvent::Original(ev) => {
                match ev.content {
                    UnstablePollStartEventContent::New(content) => {
                        let thread_root = match &content.relates_to {
                            Some(RelationWithoutReplacement::Thread(Thread {
                                event_id, ..
                            })) => Some(event_id.to_owned()),
                            _ => None,
                        };

                        let loc = EventLocation::Message(thread_root.clone(), key.clone());
                        self.keys.insert(event_id.clone(), loc);

                        let unloaded =
                            self.unloaded_unstable_polls.remove(&ev.event_id).unwrap_or_default();
                        let msg = MessageEvent::UnstablePoll(
                            UnstablePoll::new(ev.event_id, ev.sender.to_owned(), content, unloaded)
                                .into(),
                        );
                        let msg = Message::new(msg, ev.sender, ev.origin_server_ts.into());

                        self.get_thread_mut(thread_root).insert_message(key, msg);
                    },
                    UnstablePollStartEventContent::Replacement(content) => {
                        let loc = EventLocation::Poll(
                            content.relates_to.event_id.to_owned(),
                            PollEventLocation::Replacement(key.clone()),
                        );
                        self.keys.insert(event_id.clone(), loc);

                        if let Some(msg) = self.get_event_mut(&content.relates_to.event_id) {
                            let MessageEvent::UnstablePoll(poll) = &mut msg.event else {
                                tracing::warn!("encountered poll replacement for non-poll message");
                                return;
                            };

                            poll.replacements.insert(key, content);
                        } else {
                            self.unloaded_unstable_polls
                                .entry(content.relates_to.event_id.to_owned())
                                .or_default()
                                .replacements
                                .insert(key, content);
                        }
                    },
                    _ => {
                        tracing::warn!("ignoring unknown unstable poll variant");
                    },
                }
            },
            MessageLikeEvent::Redacted(ev) => {
                let loc = EventLocation::Message(None, key.clone());
                let message: Message = ev.into();
                self.keys.insert(event_id.clone(), loc);
                self.messages.insert_message(key, message);
            },
        }

        self.set_implicit_receipt(sender, event_id);
    }

    /// Insert an event that relates to a poll
    pub fn insert_poll_relation(&mut self, relation: PollRelation) {
        let event_id = relation.event_id().to_owned();
        let poll_event_id = relation.poll_event_id().to_owned();

        let loc = if let Some(msg) = self.get_event_mut(&poll_event_id) {
            let MessageEvent::Poll(poll) = &mut msg.event else {
                tracing::warn!("encountered poll replacement for non-poll message");
                return;
            };

            poll.insert_relation(relation)
        } else {
            self.unloaded_polls
                .entry(poll_event_id.to_owned())
                .or_default()
                .insert_relation(relation)
        };

        self.keys.insert(event_id, EventLocation::Poll(poll_event_id, loc));
    }

    /// Insert an event that relates to a poll
    pub fn insert_unstable_poll_relation(&mut self, relation: UnstablePollRelation) {
        let event_id = relation.event_id().to_owned();
        let poll_event_id = relation.poll_event_id().to_owned();

        let loc = if let Some(msg) = self.get_event_mut(&poll_event_id) {
            let MessageEvent::UnstablePoll(poll) = &mut msg.event else {
                tracing::warn!("encountered poll replacement for non-poll message");
                return;
            };

            poll.insert_relation(relation)
        } else {
            self.unloaded_unstable_polls
                .entry(poll_event_id.to_owned())
                .or_default()
                .insert_relation(relation)
        };

        self.keys.insert(event_id, EventLocation::Poll(poll_event_id, loc));
    }

    /// Insert an edit.
    fn insert_edit(
        &mut self,
        edit_msg: RoomMessageEvent,
        replacement: Replacement<RoomMessageEventContentWithoutRelation>,
    ) {
        let RoomMessageEvent::Original(edit_msg) = edit_msg else {
            return;
        };
        let edit_key = MessageKey {
            ts: edit_msg.origin_server_ts.into(),
            id: edit_msg.event_id.clone().into(),
        };
        let msg_loc = self.keys.get(&replacement.event_id).cloned();

        if let Some(EventLocation::Message(thread, key)) = msg_loc {
            // The edited message is already loaded in cache
            let Some(msg) = self.get_thread_mut(thread).get_mut(&key) else {
                return;
            };
            msg.insert_edit(edit_key.clone(), replacement.new_content);
        } else {
            // The edited message is not yet loaded
            let entry = self.unloaded_edits.entry(replacement.event_id.clone());
            entry.or_default().insert(edit_key.clone(), replacement.new_content);
        }

        let loc = EventLocation::Edit(replacement.event_id.clone(), edit_key.clone());
        self.keys.insert(edit_msg.event_id.clone(), loc);
    }

    pub fn insert_any_state(&mut self, msg: AnySyncStateEvent) {
        let event_id = msg.event_id().to_owned();
        let sender = msg.sender().to_owned();
        let key = MessageKey {
            ts: msg.origin_server_ts().into(),
            id: event_id.clone().into(),
        };

        let loc = EventLocation::State(key.clone());
        self.keys.insert(event_id.clone(), loc);
        self.messages.insert_message(key, msg);
        self.set_implicit_receipt(sender, event_id);
    }

    /// Indicates whether this room has unread messages.
    pub fn unreads(&self, room: &matrix_sdk::Room) -> UnreadInfo {
        let last_message = self
            .messages
            .iter()
            .rev()
            .find(|(_, msg)| !matches!(&msg.event, MessageEvent::State(..)));

        UnreadInfo {
            unread_mark: room.is_marked_unread(),
            unread_messages: room.num_unread_messages(),
            unread_notifications: room.num_unread_notifications(),
            unread_mentions: room.num_unread_mentions(),
            latest: last_message.map(|(key, _)| key.ts.to_owned()),
        }
    }

    /// Inserts events that couldn't be decrypted into the scrollback.
    pub fn insert_encrypted(&mut self, msg: RoomEncryptedEvent) {
        let event_id = msg.event_id().to_owned();
        let sender = msg.sender().to_owned();
        let key = MessageKey {
            ts: msg.origin_server_ts().into(),
            id: event_id.clone().into(),
        };

        self.keys
            .insert(event_id.clone(), EventLocation::Message(None, key.clone()));
        self.messages.insert(key, msg.into());
        self.set_implicit_receipt(sender, event_id);
    }

    /// Insert a new message.
    pub fn insert_message(&mut self, msg: RoomMessageEvent) {
        let event_id = msg.event_id().to_owned();
        let sender = msg.sender().to_owned();
        let key = MessageKey {
            ts: msg.origin_server_ts().into(),
            id: event_id.clone().into(),
        };

        let loc = EventLocation::Message(None, key.clone());
        let mut message: Message = msg.into();
        if let Some(edits) = self.unloaded_edits.remove(&event_id) {
            message.set_edits(edits);
        }
        self.keys.insert(event_id.clone(), loc);
        self.messages.insert_message(key, message);
        self.set_implicit_receipt(sender, event_id);
    }

    fn insert_thread(&mut self, msg: RoomMessageEvent, thread_root: OwnedEventId) {
        let event_id = msg.event_id().to_owned();
        let sender = msg.sender().to_owned();
        let key = MessageKey {
            ts: msg.origin_server_ts().into(),
            id: event_id.clone().into(),
        };

        let replies = self
            .threads
            .entry(thread_root.clone())
            .or_insert_with(|| Messages::thread(thread_root.clone()));
        let loc = EventLocation::Message(Some(thread_root), key.clone());
        let mut message: Message = msg.into();
        if let Some(edits) = self.unloaded_edits.remove(&event_id) {
            message.set_edits(edits);
        }
        self.keys.insert(event_id.clone(), loc);
        replies.insert_message(key, message);
        self.set_implicit_receipt(sender, event_id);
    }

    /// Insert a new message event.
    pub fn insert(&mut self, msg: RoomMessageEvent) {
        match msg {
            RoomMessageEvent::Original(OriginalRoomMessageEvent {
                content: RoomMessageEventContent { relates_to: Some(ref relates_to), .. },
                ..
            }) => {
                match relates_to {
                    Relation::Replacement(repl) => {
                        let repl = repl.clone();
                        self.insert_edit(msg, repl)
                    },
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

    /// Insert a new message event, and prepare for image-preview if it has an image attachment.
    pub fn insert_with_preview(
        &mut self,
        ev: RoomMessageEvent,
        settings: &ApplicationSettings,
        previews: &mut PreviewManager,
    ) {
        if let MessageLikeEvent::Original(OriginalMessageLikeEvent {
            content: RoomMessageEventContent { msgtype: MessageType::Image(c), .. },
            ..
        }) = &ev &&
            settings.tunables.image_preview.enabled
        {
            previews.register_preview(settings, &c.source, PreviewKind::Message)
        }

        self.insert(ev);
    }

    /// Indicates whether we've recently fetched scrollback for this room.
    pub fn recently_fetched(&self) -> bool {
        self.fetch_last.is_some_and(|i| i.elapsed() < ROOM_FETCH_DEBOUNCE)
    }

    fn clear_receipt(&mut self, thread: &ReceiptThread, user_id: &OwnedUserId) -> Option<()> {
        let old_user_receipts = self.user_receipts.get_mut(thread);
        let old_event_id = old_user_receipts.and_then(|rs| rs.remove(user_id))?;
        let old_thread = self.event_receipts.get_mut(thread)?;
        let old_receipts = old_thread.get_mut(&old_event_id)?;
        old_receipts.remove(user_id);

        if old_receipts.is_empty() {
            old_thread.remove(&old_event_id);
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
        if let Some(old_event_id) =
            self.user_receipts.get(&thread).and_then(|receipts| receipts.get(&user_id)) &&
            let (Some(old_key), Some(new_key)) =
                (self.receipt_key(old_event_id), self.receipt_key(&event_id)) &&
            new_key <= old_key
        {
            return;
        }

        self.clear_receipt(&thread, &user_id);
        self.event_receipts(&thread, &event_id).insert(user_id.clone());
        self.user_receipts.entry(thread).or_default().insert(user_id, event_id);
    }

    /// Get the users whose receipts currently point at the given event in the specified `thread`.
    fn event_receipts(
        &mut self,
        thread: &ReceiptThread,
        event_id: &EventId,
    ) -> &mut HashSet<OwnedUserId> {
        self.event_receipts
            .entry(thread.clone())
            .or_default()
            .entry(event_id.to_owned())
            .or_default()
    }

    fn receipt_key(&self, event_id: &EventId) -> Option<&MessageKey> {
        match self.keys.get(event_id)? {
            EventLocation::Message(_, key) | EventLocation::State(key) => Some(key),
            _ => None,
        }
    }

    /// Whenever a user sends a message, we can consider that an effective update to their
    /// read receipt, even when their client isn't sending them to the room.
    ///
    /// While the Matrix specification doesn't explicitly mention "implicit receipts", this
    /// is effectively what is described by "Marking notifications as read",
    ///
    /// > When the user updates their read receipt (either by using the API or by sending an
    /// > event), notifications prior to and including that event MUST be marked as read.
    ///
    /// This method is called every time we insert a new message into the [RoomInfo], so that
    /// we show in the timeline that the user has now read up to the point where they sent
    /// a message.
    fn set_implicit_receipt(&mut self, user_id: OwnedUserId, event_id: OwnedEventId) {
        let Some(thread) = self.get_receipt_thread(&event_id) else {
            return;
        };

        let current = self.user_receipts.get(&thread).and_then(|receipts| receipts.get(&user_id));
        let current_key = current.and_then(|event_id| self.receipt_key(event_id));

        if current.is_some() && current_key.is_none() {
            // Do not set an implicit receipt if we have an explicit receipt that points
            // at an event whose information we don't have, since we need it to make sure
            // that we don't move the receipt backwards in the timeline.
            //
            // This event could technically be newer than the one the receipt currently
            // points at, but we can't be sure, so just trust what the user's receipt in
            // the room says to point at, and we'll update it once they send us a new one.
            //
            // This technically makes it possible for a user who previously sent room
            // receipts and then switched to just private receipts to not have their marker
            // moved with recent messages, but solving that would require us to force a
            // load of the event that every user's read marker points at.
            return;
        }

        // If the user's client isn't thread-aware then make sure the implicit receipt
        // doesn't result in displaying a second read receipt after where their unthreaded
        // receipt is currently pointing at:
        self.clear_receipt(&ReceiptThread::Unthreaded, &user_id);

        self.set_receipt(thread, user_id, event_id);
    }

    /// This is called whenever the user has viewed the most recent message sent to a room or
    /// thread, so that we can update the read receipt on the homeserver.
    ///
    /// Note that the Matrix specification says:
    ///
    /// > Clients should send read receipts when there is some certainty that the event in
    /// > question has been displayed to the user. Simply receiving an event does not provide
    /// > enough certainty that the user has seen the event. The user SHOULD need to take some
    /// > action such as viewing the room that the event was sent to or dismissing a notification
    /// > in order for the event to count as “read”. Clients SHOULD NOT send read receipts for
    /// > events sent by their own user.
    pub fn fully_read(
        &mut self,
        room_id: OwnedRoomId,
        thread: ReceiptThread,
        worker: &Requester,
        settings: &ApplicationSettings,
        open_notifications: &mut HashMap<OwnedRoomId, Vec<NotificationHandle>>,
    ) {
        let user_id = &settings.profile.user_id;

        let messages = match &thread {
            ReceiptThread::Main => self.get_thread(None),
            ReceiptThread::Thread(root) => self.get_thread(Some(root)),
            _ => None,
        };

        let Some(messages) = messages else {
            return;
        };

        let event_id = messages
            .iter()
            .filter(|(_, msg)| {
                // Handle the "do not send read receipts for our own events" part of the
                // specification quoted above.
                msg.sender != *user_id
            })
            .filter(|(_, msg)| {
                matches!(
                    msg.event,
                    MessageEvent::EncryptedOriginal(..) |
                        MessageEvent::EncryptedRedacted(..) |
                        MessageEvent::Original(..) |
                        MessageEvent::Redacted(..)
                )
            })
            .flat_map(|(_, msg)| msg.event.event_id())
            .next_back()
            .map(ToOwned::to_owned);

        if let Some((_, msg)) = messages.last_key_value() &&
            let Some(event_id) = msg.event.event_id()
        {
            // Always update the local receipt info for this room so that we aren't reliant
            // on the homeserver echoing it back to us to update this and clear the trackbar:
            self.set_receipt(thread.clone(), user_id.clone(), event_id.to_owned());
        }

        if let Some(event_id) = event_id {
            // Clear any desktop notifications for the room:
            open_notifications.remove(&room_id);

            // Update the homeserver with the latest receipt:
            worker.send_receipt(room_id, thread, event_id, settings);
        }
    }

    pub fn fully_read_all(
        &mut self,
        room_id: OwnedRoomId,
        worker: &Requester,
        settings: &ApplicationSettings,
        open_notifications: &mut HashMap<OwnedRoomId, Vec<NotificationHandle>>,
    ) {
        let threads: Vec<_> = self.threads.keys().map(|root| root.to_owned()).collect();

        for thread in threads {
            self.fully_read(
                room_id.to_owned(),
                ReceiptThread::Thread(thread),
                worker,
                settings,
                open_notifications,
            );
        }

        self.fully_read(room_id, ReceiptThread::Main, worker, settings, open_notifications);
    }

    pub fn read_event_users<'a>(
        &'a self,
        thread: ReceiptThread,
        event_id: &'a EventId,
    ) -> impl Iterator<Item = &'a OwnedUserId> + 'a {
        self.event_receipts
            .get(&thread)
            .and_then(|rs| rs.get(event_id))
            .map(|read| read.iter())
            .unwrap_or_default()
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
            // still keep one line blank, so `render_jump_to_recent` doesn't immediately hide the
            // last line in scrollback
            return Rect::new(area.x, area.y, area.width, area.height - 1);
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
                .any(|(annotation, user, _)| annotation == emoji && user == user_id)
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

/// Information gathered during server syncs about joined rooms.
#[derive(Default)]
pub struct SyncInfo {
    /// Spaces that the user is a member of.
    pub spaces: Vec<MatrixRoom>,

    /// Rooms that the user is a member of.
    pub rooms: Vec<MatrixRoom>,

    /// DMs that the user is a member of.
    pub dms: Vec<MatrixRoom>,
}

impl SyncInfo {
    pub fn rooms(&self) -> impl Iterator<Item = &RoomId> {
        self.rooms.iter().map(|r| r.room_id())
    }

    pub fn dms(&self) -> impl Iterator<Item = &RoomId> {
        self.dms.iter().map(|r| r.room_id())
    }

    pub fn chats(&self) -> impl Iterator<Item = &RoomId> {
        self.rooms().chain(self.dms())
    }
}

static MESSAGE_NEED_TTL: u8 = 30;

/// How many failed fetches of a pinned event before the `:pinned` window stops retrying.
const PINNED_FETCH_ATTEMPTS: u8 = 10;

#[derive(Debug, PartialEq)]
/// Load messages until the event is loaded or `ttl` loads are exceeded
pub struct MessageNeed {
    pub event_id: OwnedEventId,
    pub ttl: u8,
}

#[derive(Default, Debug, PartialEq)]
pub struct Need {
    pub members: bool,
    pub pinned: bool,
    pub messages: Option<Vec<MessageNeed>>,
}

/// Things that need loading for different rooms.
#[derive(Default, Debug)]
pub struct RoomNeeds {
    needs: HashMap<OwnedRoomId, Need>,
    previews: HashSet<OwnedRoomOrAliasId>,
}

impl RoomNeeds {
    /// Mark a room for needing to load members.
    pub fn need_members(&mut self, room_id: OwnedRoomId) {
        self.needs.entry(room_id).or_default().members = true;
    }

    /// Mark a room for needing to fetch its pinned events.
    pub fn need_pinned(&mut self, room_id: OwnedRoomId) {
        self.needs.entry(room_id).or_default().pinned = true;
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

    /// Request the load of a room preview.
    pub fn need_preview(&mut self, room: OwnedRoomOrAliasId) {
        self.previews.insert(room);
    }

    /// Return all requested room previews
    pub fn preview_needs(&mut self) -> impl Iterator<Item = OwnedRoomOrAliasId> {
        std::mem::take(&mut self.previews).into_iter()
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

    /// Map of loaded room previews with their fetch time
    pub room_previews:
        HashMap<OwnedRoomOrAliasId, (Result<RoomPreview, matrix_sdk::Error>, Instant)>,

    /// Cache of encountered `via` parameters in room links.
    ///
    /// This is stored here because this data is lost in the conversion to [IambId].
    pub room_via: HashMap<OwnedRoomOrAliasId, Vec<OwnedServerName>>,

    /// Map of room aliases.
    pub aliases: CompletionMap<OwnedRoomAliasId, OwnedRoomId>,

    /// Presence information for other users.
    pub presences: CompletionMap<OwnedUserId, PresenceState>,

    /// In-progress and completed verifications.
    /// The map key is the `flow_id`.
    pub verifications: CompletionMap<String, VerificationRequest>,

    /// Settings for the current profile loaded from config file.
    pub settings: ApplicationSettings,

    /// Set of rooms that need more messages loaded in their scrollback.
    pub need_load: RoomNeeds,

    /// [CompletionMap] of Emoji shortcodes.
    pub emojis: CompletionMap<String, &'static Emoji>,

    /// Information gathered by the background thread.
    pub sync_info: SyncInfo,

    /// Rendered image previews.
    pub previews: PreviewManager,

    /// Last draw time, used to match with RoomInfo's draw_last.
    pub draw_curr: Option<Instant>,

    /// Whether to ring the terminal bell on the next redraw.
    pub ring_bell: bool,

    /// An error raised while drawing, shown in the message bar on the next redraw.
    pub draw_error: Option<String>,

    /// Whether the application is currently focused
    pub focused: bool,

    /// Collator for locale-aware text sorting.
    pub collator: feruca::Collator,

    /// Notifications that should be dismissed when the user opens the room.
    pub open_notifications: HashMap<OwnedRoomId, Vec<NotificationHandle>>,
}

impl ChatStore {
    /// Create a new [ChatStore].
    pub fn new(worker: Requester, settings: ApplicationSettings) -> IambResult<Self> {
        let previews = PreviewManager::new(&settings);
        let cmds = crate::commands::setup_commands(&settings.aliases)?;

        let store = ChatStore {
            worker,
            settings,
            previews,
            cmds,
            emojis: emoji_map(),

            collator: Default::default(),
            aliases: Default::default(),
            rooms: Default::default(),
            room_previews: Default::default(),
            room_via: Default::default(),
            presences: Default::default(),
            verifications: Default::default(),
            need_load: Default::default(),
            sync_info: Default::default(),
            draw_curr: None,
            ring_bell: false,
            draw_error: None,
            focused: true,
            open_notifications: Default::default(),
        };

        Ok(store)
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

    /// Get the alias for a room if it has one (and the client knows it).
    pub fn get_joined_room_alias(&self, room_id: &RoomId) -> Option<OwnedRoomAliasId> {
        self.worker.client.get_room(room_id).and_then(|r| r.canonical_alias())
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

    /// Set the name and tags for a room.
    pub fn set_room_info(
        &mut self,
        room_id: OwnedRoomId,
        name: String,
        tags: Option<Tags>,
        aliases: Vec<OwnedRoomAliasId>,
    ) {
        for alias in aliases {
            self.aliases.insert(alias, room_id.clone());
        }

        let info = self.rooms.get_or_default(room_id);
        info.name = name.into();
        info.tags = tags;
    }
}

impl ApplicationStore for ChatStore {}

/// Identified used to track window content.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IambId {
    /// A Matrix room, with an optional thread to show.
    Room(OwnedRoomOrAliasId, Option<OwnedEventId>),

    /// The `:dms` window.
    DirectList,

    /// The `:members` window for a given Matrix room.
    MemberList(OwnedRoomId),

    /// The `:pinned` window for a given Matrix room.
    PinnedList(OwnedRoomId),

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

    /// The `:mentions` window.
    MentionsList,

    /// The `:invites` window.
    InvitesList,
}

impl Display for IambId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IambId::Room(alias, None) => {
                let encoded = percent_encode(alias.as_bytes(), NON_ALPHANUMERIC);
                write!(f, "iamb://room/{}", encoded)
            },
            IambId::Room(alias, Some(thread)) => {
                let encoded = percent_encode(alias.as_bytes(), NON_ALPHANUMERIC);
                write!(f, "iamb://room/{}/threads/{thread}", encoded)
            },
            IambId::MemberList(room_id) => {
                write!(f, "iamb://members/{room_id}")
            },
            IambId::PinnedList(room_id) => {
                write!(f, "iamb://room/{room_id}/pinned")
            },
            IambId::DirectList => f.write_str("iamb://dms"),
            IambId::RoomList => f.write_str("iamb://rooms"),
            IambId::SpaceList => f.write_str("iamb://spaces"),
            IambId::VerifyList => f.write_str("iamb://verify"),
            IambId::Welcome => f.write_str("iamb://welcome"),
            IambId::ChatList => f.write_str("iamb://chats"),
            IambId::UnreadList => f.write_str("iamb://unreads"),
            IambId::MentionsList => f.write_str("iamb://mentions"),
            IambId::InvitesList => f.write_str("iamb://invites"),
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
                    [alias] => {
                        let decoded = percent_decode(alias.as_bytes()).decode_utf8_lossy();
                        let Ok(room_id) = OwnedRoomOrAliasId::try_from(decoded.as_ref()) else {
                            return Err(E::custom(format!("Invalid room identifier: {decoded:?}")));
                        };

                        Ok(IambId::Room(room_id, None))
                    },
                    [alias, "threads", thread_root] => {
                        let decoded = percent_decode(alias.as_bytes()).decode_utf8_lossy();
                        let Ok(room_id) = OwnedRoomOrAliasId::try_from(decoded.as_ref()) else {
                            return Err(E::custom("Invalid room identifier: {decoded:?}"));
                        };

                        let Ok(thread_root) = OwnedEventId::try_from(thread_root) else {
                            return Err(E::custom("Invalid thread root identifier"));
                        };

                        Ok(IambId::Room(room_id, Some(thread_root)))
                    },
                    [room_id, "pinned"] => {
                        let Ok(room_id) = OwnedRoomId::try_from(room_id) else {
                            return Err(E::custom("Invalid room identifier"));
                        };

                        Ok(IambId::PinnedList(room_id))
                    },
                    _ => return Err(E::custom("Invalid iamb window URL")),
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
            Some("mentions") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://mentions takes no path"));
                }

                Ok(IambId::MentionsList)
            },
            Some("invites") => {
                if url.path() != "" {
                    return Err(E::custom("iamb://invites takes no path"));
                }

                Ok(IambId::InvitesList)
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

    /// The `:pinned` window for a room.
    PinnedList(OwnedRoomId),

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

    /// The `:mentions` window.
    MentionsList,

    /// The `:invites` window.
    InvitesList,
}

impl IambBufferId {
    /// Get the identifier for the window that contains this buffer.
    pub fn to_window(&self) -> Option<IambId> {
        let id = match self {
            IambBufferId::Command(_) => return None,
            IambBufferId::Room(room, thread, _) => {
                IambId::Room(room.clone().into(), thread.clone())
            },
            IambBufferId::DirectList => IambId::DirectList,
            IambBufferId::MemberList(room) => IambId::MemberList(room.clone()),
            IambBufferId::PinnedList(room) => IambId::PinnedList(room.clone()),
            IambBufferId::RoomList => IambId::RoomList,
            IambBufferId::SpaceList => IambId::SpaceList,
            IambBufferId::VerifyList => IambId::VerifyList,
            IambBufferId::Welcome => IambId::Welcome,
            IambBufferId::ChatList => IambId::ChatList,
            IambBufferId::UnreadList => IambId::UnreadList,
            IambBufferId::MentionsList => IambId::MentionsList,
            IambBufferId::InvitesList => IambId::InvitesList,
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

    fn content_of_command(ct: CommandType) -> IambBufferId {
        IambBufferId::Command(ct)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    use std::iter::FromIterator as _;

    use matrix_sdk::ruma::events::reaction::ReactionEventContent;
    use matrix_sdk::ruma::events::relation::Annotation;
    use matrix_sdk::ruma::{MilliSecondsSinceUnixEpoch, owned_event_id};
    use pretty_assertions::assert_eq;
    use ratatui::style::Color;
    use serde_json::{Map, Value};

    use crate::tests::*;

    fn mock_room_message_event(
        content: RoomMessageEventContent,
        sender: OwnedUserId,
        key: MessageKey,
    ) -> RoomMessageEvent {
        let message = mock_room1_message(content, sender, key);
        let MessageEvent::Original(event, _) = message.event else {
            unreachable!("mock_room1_message always returns an original room message")
        };

        MessageLikeEvent::Original(*event)
    }

    fn create_reaction_event(
        content: &ReactionEventContent,
        event_id: &str,
        sender: &str,
    ) -> ReactionEvent {
        serde_json::from_value(Value::Object(Map::from_iter([
            ("type".to_owned(), Value::String("m.reaction".into())),
            ("content".to_owned(), serde_json::to_value(content).unwrap()),
            ("event_id".to_owned(), serde_json::to_value(event_id).unwrap()),
            ("sender".to_owned(), Value::String(sender.into())),
            (
                "origin_server_ts".to_owned(),
                serde_json::to_value(MilliSecondsSinceUnixEpoch::now()).unwrap(),
            ),
            ("room_id".to_owned(), Value::String("!foo:example.org".into())),
        ])))
        .unwrap()
    }

    #[test]
    fn test_multiple_identical_reactions() {
        let mut info = RoomInfo::default();

        let content = ReactionEventContent::new(Annotation::new(
            owned_event_id!("$my_reaction"),
            "🏠".to_owned(),
        ));

        for i in 0..3 {
            let event_id = format!("$house_{i}");
            let react = create_reaction_event(&content, &event_id, "@foo:example.com");
            info.insert_reaction(react, None);
        }

        let content = ReactionEventContent::new(Annotation::new(
            owned_event_id!("$my_reaction"),
            "🙂".to_owned(),
        ));

        for i in 0..2 {
            let event_id = format!("$smile_{i}");
            let react = create_reaction_event(&content, &event_id, "@foo:example.com");
            info.insert_reaction(react, None);
        }

        for i in 2..4 {
            let event_id = format!("$smile2_{i}");
            let react = create_reaction_event(&content, &event_id, "@bar:example.com");
            info.insert_reaction(react, None);
        }

        let reacts: Vec<_> = info
            .get_reactions(&owned_event_id!("$my_reaction"))
            .into_iter()
            .map(|(key, count, _)| (key, count))
            .collect();
        assert_eq!(reacts, vec![("🏠", 1), ("🙂", 2)]);
    }

    #[test]
    fn test_implicit_receipt_tracks_message_sender_for_display() {
        let mut info = RoomInfo::default();
        let settings = mock_settings();
        let mut previews = PreviewManager::new(&settings);
        let event = mock_room_message_event(
            RoomMessageEventContent::text_plain("sent by another user"),
            TEST_USER2.clone(),
            MSG5_KEY.clone(),
        );

        info.insert_with_preview(event, &settings, &mut previews);

        assert_eq!(
            info.user_receipts
                .get(&ReceiptThread::Main)
                .and_then(|receipts| receipts.get(&*TEST_USER2)),
            Some(&*MSG5_EVID),
        );
        assert!(
            info.event_receipts
                .get(&ReceiptThread::Main)
                .and_then(|receipts| receipts.get(&*MSG5_EVID))
                .is_some_and(|users| users.contains(&*TEST_USER2))
        );
    }

    #[test]
    fn test_implicit_receipts_do_not_move_backwards_during_backpagination() {
        let mut info = RoomInfo::default();
        let settings = mock_settings();
        let mut previews = PreviewManager::new(&settings);
        let newer = mock_room_message_event(
            RoomMessageEventContent::text_plain("newer"),
            TEST_USER2.clone(),
            MSG5_KEY.clone(),
        );
        let older = mock_room_message_event(
            RoomMessageEventContent::text_plain("older"),
            TEST_USER2.clone(),
            MSG2_KEY.clone(),
        );

        info.insert_with_preview(newer, &settings, &mut previews);
        info.insert_with_preview(older, &settings, &mut previews);

        assert_eq!(
            info.user_receipts
                .get(&ReceiptThread::Main)
                .and_then(|receipts| receipts.get(&*TEST_USER2)),
            Some(&*MSG5_EVID),
        );
    }

    #[test]
    fn test_implicit_receipt_preserves_thread_context() {
        let mut info = RoomInfo::default();
        let root = owned_event_id!("$thread_root");
        let reply = owned_event_id!("$thread_reply");
        let thread = ReceiptThread::Thread(root.clone());

        info.keys
            .insert(reply.clone(), EventLocation::Message(Some(root), MSG5_KEY.clone()));
        info.set_implicit_receipt(TEST_USER1.clone(), reply.clone());

        assert_eq!(
            info.user_receipts
                .get(&thread)
                .and_then(|receipts| receipts.get(&*TEST_USER1)),
            Some(&reply),
        );
    }

    #[test]
    fn test_implicit_receipt_supports_state_events() {
        let mut info = RoomInfo::default();
        info.keys.insert(MSG5_EVID.clone(), EventLocation::State(MSG5_KEY.clone()));

        info.set_implicit_receipt(TEST_USER2.clone(), MSG5_EVID.clone());

        assert_eq!(
            info.user_receipts
                .get(&ReceiptThread::Main)
                .and_then(|receipts| receipts.get(&*TEST_USER2)),
            Some(&*MSG5_EVID),
        );
    }

    #[test]
    fn test_explicit_receipt_does_not_move_implicit_receipt_backwards() {
        let mut info = RoomInfo::default();
        let older = mock_room_message_event(
            RoomMessageEventContent::text_plain("older"),
            TEST_USER2.clone(),
            MSG2_KEY.clone(),
        );
        let newer = mock_room_message_event(
            RoomMessageEventContent::text_plain("newer"),
            TEST_USER2.clone(),
            MSG5_KEY.clone(),
        );

        info.insert_message(older);
        info.insert_message(newer);
        info.set_receipt(ReceiptThread::Main, TEST_USER2.clone(), MSG2_EVID.clone());

        assert_eq!(
            info.user_receipts
                .get(&ReceiptThread::Main)
                .and_then(|receipts| receipts.get(&*TEST_USER2)),
            Some(&*MSG5_EVID),
        );
    }

    #[test]
    fn test_implicit_receipt_waits_for_unloaded_explicit_receipt() {
        let mut info = RoomInfo::default();
        info.set_receipt(ReceiptThread::Main, TEST_USER2.clone(), MSG5_EVID.clone());
        let older = mock_room_message_event(
            RoomMessageEventContent::text_plain("older"),
            TEST_USER2.clone(),
            MSG2_KEY.clone(),
        );

        info.insert_message(older);

        assert_eq!(
            info.user_receipts
                .get(&ReceiptThread::Main)
                .and_then(|receipts| receipts.get(&*TEST_USER2)),
            Some(&*MSG5_EVID),
        );
    }

    #[test]
    fn test_typing_spans() {
        let mut info = RoomInfo::default();
        let settings = mock_settings();
        let theme = &settings.theme;

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
                Span::styled("@user1:example.com", theme.users.style("@user1:example.com", None)),
                Span::from(" is typing...")
            ])
        );

        // Two users typing.
        info.set_typing(users2);
        assert!(info.users_typing.is_some());
        assert_eq!(
            info.get_typing_spans(&settings),
            Line::from(vec![
                Span::styled("@user1:example.com", theme.users.style("@user1:example.com", None)),
                Span::raw(" and "),
                Span::styled("@user2:example.com", theme.users.style("@user2:example.com", None)),
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
                Span::styled("USER 5", Style::default().fg(Color::Black).bold()),
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
            Need {
                members: true,
                messages: Some(Vec::new()),
                pinned: false
            }
        )],);
    }

    #[test]
    fn test_pinned_lookup() {
        let mut info = mock_room();
        let unloaded = owned_event_id!("$unloaded");

        info.pinned_events = vec![MSG3_EVID.clone(), unloaded.clone()];

        assert!(info.is_pinned(&MSG3_EVID));
        assert!(!info.is_pinned(&MSG4_EVID));

        // Loaded messages come from the scrollback, so only the unloaded one needs fetching.
        assert!(info.get_pinned(&MSG3_EVID).is_some());
        assert_eq!(info.missing_pinned(), vec![unloaded.clone()]);

        info.insert_pinned(unloaded.clone(), mock_message1().into());
        assert!(info.get_pinned(&unloaded).is_some());
        assert!(info.missing_pinned().is_empty());

        // Failed fetches are retried until they hit the limit.
        let broken = owned_event_id!("$broken");
        info.pinned_events.push(broken.clone());

        for _ in 1..PINNED_FETCH_ATTEMPTS {
            info.insert_pinned(broken.clone(), None);
        }
        assert!(!info.pinned_unavailable(&broken));
        assert_eq!(info.missing_pinned(), vec![broken.clone()]);

        info.insert_pinned(broken.clone(), None);
        assert!(info.pinned_unavailable(&broken));
        assert!(info.missing_pinned().is_empty());

        let (thread, key) = info.get_message_location(&MSG3_EVID).unwrap();
        assert_eq!(thread, None);
        assert_eq!(key, &*MSG3_KEY);
        assert!(info.get_message_location(&unloaded).is_none());
    }

    #[test]
    fn test_alias_window_id() {
        let room_id = TEST_ROOM1_ALIAS.clone();
        let id = IambId::Room(room_id.into(), None);

        // Hash gets replaced during encoding:
        let exp = "iamb://room/%23room1%3Aexample%2Ecom";
        assert_eq!(id.to_string(), exp);

        // Percent encoding turns back into hash during decoding:
        let parsed: IambId = serde_json::from_str(&format!("{exp:?}")).unwrap();
        assert_eq!(parsed, id);
    }

    #[test]
    fn test_pinned_window_id() {
        let room_id = TEST_ROOM1_ID.clone();
        let id = IambId::PinnedList(room_id.clone());
        let url = format!("iamb://room/{room_id}/pinned");

        assert_eq!(id.to_string(), url);

        let parsed: IambId = serde_json::from_str(&format!("{url:?}")).unwrap();
        assert_eq!(parsed, id);
    }

    #[test]
    fn test_ambiguous_displaynames() {
        let mut store = DisplayNameStore::default();

        store.set(TEST_USER1.clone(), Some("John".into()), true);
        store.set(TEST_USER2.clone(), Some("John".into()), true);
        store.set(TEST_USER3.clone(), Some("Jane".into()), true);
        store.set(TEST_USER4.clone(), Some("Alice".into()), true);
        store.set(TEST_USER5.clone(), Some("Bob".into()), true);

        // TEST_USER1 and TEST_USER2 are both ambiguous, while the other are unambiguous:
        assert_eq!(store.get(&TEST_USER1).unwrap().as_ref(), "John (@user1:example.com)");
        assert_eq!(store.get(&TEST_USER2).unwrap().as_ref(), "John (@user2:example.com)");
        assert_eq!(store.get(&TEST_USER3).unwrap().as_ref(), "Jane");
        assert_eq!(store.get(&TEST_USER4).unwrap().as_ref(), "Alice");
        assert_eq!(store.get(&TEST_USER5).unwrap().as_ref(), "Bob");

        // TEST_USER1 becomes unambiguous when TEST_USER2 changes:
        store.set(TEST_USER2.clone(), Some("Eve".into()), true);
        assert_eq!(store.get(&TEST_USER1).unwrap().as_ref(), "John");
        assert_eq!(store.get(&TEST_USER2).unwrap().as_ref(), "Eve");
        assert_eq!(store.get(&TEST_USER3).unwrap().as_ref(), "Jane");
        assert_eq!(store.get(&TEST_USER4).unwrap().as_ref(), "Alice");
        assert_eq!(store.get(&TEST_USER5).unwrap().as_ref(), "Bob");

        // TEST_USER5 becomes ambiguous when TEST_USER2 once again changes their name to match:
        store.set(TEST_USER2.clone(), Some("Bob".into()), true);
        assert_eq!(store.get(&TEST_USER1).unwrap().as_ref(), "John");
        assert_eq!(store.get(&TEST_USER2).unwrap().as_ref(), "Bob (@user2:example.com)");
        assert_eq!(store.get(&TEST_USER3).unwrap().as_ref(), "Jane");
        assert_eq!(store.get(&TEST_USER4).unwrap().as_ref(), "Alice");
        assert_eq!(store.get(&TEST_USER5).unwrap().as_ref(), "Bob (@user5:example.com)");

        // Now "Everyone is John":
        store.set(TEST_USER2.clone(), Some("John".into()), true);
        store.set(TEST_USER3.clone(), Some("John".into()), true);
        store.set(TEST_USER4.clone(), Some("John".into()), true);
        store.set(TEST_USER5.clone(), Some("John".into()), true);
        assert_eq!(store.get(&TEST_USER1).unwrap().as_ref(), "John (@user1:example.com)");
        assert_eq!(store.get(&TEST_USER2).unwrap().as_ref(), "John (@user2:example.com)");
        assert_eq!(store.get(&TEST_USER3).unwrap().as_ref(), "John (@user3:example.com)");
        assert_eq!(store.get(&TEST_USER4).unwrap().as_ref(), "John (@user4:example.com)");
        assert_eq!(store.get(&TEST_USER5).unwrap().as_ref(), "John (@user5:example.com)");

        // 2-4 unset their displayname:
        store.set(TEST_USER2.clone(), None, true);
        store.set(TEST_USER3.clone(), None, true);
        store.set(TEST_USER4.clone(), None, true);
        // and 5 leaves
        store.set(TEST_USER5.clone(), Some("John".into()), false);
        assert_eq!(store.get(&TEST_USER1).unwrap().as_ref(), "John");
        assert_eq!(store.get(&TEST_USER2), None);
        assert_eq!(store.get(&TEST_USER3), None);
        assert_eq!(store.get(&TEST_USER4), None);
        assert_eq!(store.get(&TEST_USER5).unwrap().as_ref(), "John (@user5:example.com)");
    }
}
