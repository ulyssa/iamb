use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::sync::Arc;
use std::time::{Duration, Instant};

use tokio::sync::Mutex as AsyncMutex;
use tracing::warn;

use matrix_sdk::{
    encryption::verification::SasVerification,
    ruma::{OwnedRoomId, OwnedUserId, RoomId},
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
        command::{CommandContext, VimCommand, VimCommandMachine},
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
pub enum MessageAction {
    Cancel,
    Download(Option<String>, bool),
    Redact(Option<String>),
    Reply,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SetRoomField {
    Name(String),
    Topic(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RoomAction {
    InviteAccept,
    InviteReject,
    InviteSend(OwnedUserId),
    Members(Box<CommandContext<ProgramContext>>),
    Set(SetRoomField),
}

impl From<SetRoomField> for RoomAction {
    fn from(act: SetRoomField) -> Self {
        RoomAction::Set(act)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SendAction {
    Submit,
    Upload(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IambAction {
    Message(MessageAction),
    Room(RoomAction),
    Send(SendAction),
    Verify(VerifyAction, String),
    VerifyRequest(String),
    ToggleScrollbackFocus,
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
            IambAction::Message(..) => false,
            IambAction::Room(..) => false,
            IambAction::Send(..) => false,
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

#[derive(Default)]
pub struct RoomInfo {
    pub name: Option<String>,
    pub messages: Messages,
    pub fetch_id: RoomFetchStatus,
    pub fetch_last: Option<Instant>,
    pub users_typing: Option<(Instant, Vec<OwnedUserId>)>,
}

impl RoomInfo {
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

    fn get_typing_spans(&self, settings: &ApplicationSettings) -> Spans {
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

    pub fn get_room_title(&self, room_id: &RoomId) -> String {
        self.rooms
            .get(room_id)
            .and_then(|i| i.name.as_ref())
            .map(String::from)
            .unwrap_or_else(|| "Untitled Matrix Room".to_string())
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
    Command,
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
            IambBufferId::Command => None,
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
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::config::{user_style, user_style_from_color};
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
}
