use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::sync::Arc;
use std::time::{Duration, Instant};

use tokio::sync::Mutex as AsyncMutex;
use tracing::warn;

use matrix_sdk::{
    encryption::verification::SasVerification,
    ruma::{OwnedRoomId, RoomId},
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
        context::EditContext,
        store::Store,
    },
    env::vim::{
        command::{VimCommand, VimCommandMachine},
        keybindings::VimMachine,
        VimContext,
    },
    input::bindings::SequenceStatus,
    input::key::TerminalKey,
};

use crate::{
    message::{Message, Messages},
    worker::Requester,
    ApplicationSettings,
};

const ROOM_FETCH_DEBOUNCE: Duration = Duration::from_secs(3);

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
pub enum IambAction {
    Verify(VerifyAction, String),
    VerifyRequest(String),
    SendMessage(OwnedRoomId, String),
    ToggleScrollbackFocus,
}

impl ApplicationAction for IambAction {
    fn is_edit_sequence<C: EditContext>(&self, _: &C) -> SequenceStatus {
        match self {
            IambAction::SendMessage(..) => SequenceStatus::Break,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Break,
            IambAction::Verify(..) => SequenceStatus::Break,
            IambAction::VerifyRequest(..) => SequenceStatus::Break,
        }
    }

    fn is_last_action<C: EditContext>(&self, _: &C) -> SequenceStatus {
        match self {
            IambAction::SendMessage(..) => SequenceStatus::Atom,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Atom,
            IambAction::Verify(..) => SequenceStatus::Atom,
            IambAction::VerifyRequest(..) => SequenceStatus::Atom,
        }
    }

    fn is_last_selection<C: EditContext>(&self, _: &C) -> SequenceStatus {
        match self {
            IambAction::SendMessage(..) => SequenceStatus::Ignore,
            IambAction::ToggleScrollbackFocus => SequenceStatus::Ignore,
            IambAction::Verify(..) => SequenceStatus::Ignore,
            IambAction::VerifyRequest(..) => SequenceStatus::Ignore,
        }
    }

    fn is_switchable<C: EditContext>(&self, _: &C) -> bool {
        match self {
            IambAction::SendMessage(..) => false,
            IambAction::ToggleScrollbackFocus => false,
            IambAction::Verify(..) => false,
            IambAction::VerifyRequest(..) => false,
        }
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

#[derive(thiserror::Error, Debug)]
pub enum IambError {
    #[error("Unknown room identifier: {0}")]
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

#[derive(Default)]
pub struct RoomInfo {
    pub name: Option<String>,
    pub messages: Messages,
    pub fetch_id: RoomFetchStatus,
    pub fetch_last: Option<Instant>,
}

impl RoomInfo {
    fn recently_fetched(&self) -> bool {
        self.fetch_last.map_or(false, |i| i.elapsed() < ROOM_FETCH_DEBOUNCE)
    }
}

pub struct ChatStore {
    pub worker: Requester,
    pub rooms: HashMap<OwnedRoomId, RoomInfo>,
    pub names: HashMap<String, OwnedRoomId>,
    pub verifications: HashMap<String, SasVerification>,
    pub settings: ApplicationSettings,
    pub need_load: HashSet<OwnedRoomId>,
}

impl ChatStore {
    pub fn new(worker: Requester, settings: ApplicationSettings) -> Self {
        ChatStore {
            worker,
            settings,

            names: Default::default(),
            rooms: Default::default(),
            verifications: Default::default(),
            need_load: Default::default(),
        }
    }

    pub fn mark_for_load(&mut self, room_id: OwnedRoomId) {
        self.need_load.insert(room_id);
    }

    pub fn load_older(&mut self, limit: u32) {
        let ChatStore { need_load, rooms, worker, .. } = self;

        for room_id in std::mem::take(need_load).into_iter() {
            let info = rooms.entry(room_id.clone()).or_default();

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
                        let key = (msg.origin_server_ts().into(), msg.event_id().to_owned());

                        info.messages.insert(key, Message::from(msg));
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
        self.rooms.entry(room_id).or_default()
    }

    pub fn set_room_name(&mut self, room_id: &RoomId, name: &str) {
        self.rooms.entry(room_id.to_owned()).or_default().name = name.to_string().into();
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
    Command,
    Room(OwnedRoomId, RoomFocus),
    DirectList,
    RoomList,
    SpaceList,
    VerifyList,
    Welcome,
}

impl IambBufferId {
    pub fn to_window(&self) -> Option<IambId> {
        match self {
            IambBufferId::Command => None,
            IambBufferId::Room(room, _) => Some(IambId::Room(room.clone())),
            IambBufferId::DirectList => Some(IambId::DirectList),
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
}
