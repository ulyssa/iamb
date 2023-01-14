use std::convert::TryFrom;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io::BufWriter;
use std::str::FromStr;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::Arc;
use std::time::Duration;

use gethostname::gethostname;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tokio::task::JoinHandle;
use tracing::error;

use matrix_sdk::{
    config::{RequestConfig, StoreConfig, SyncSettings},
    encryption::verification::{SasVerification, Verification},
    event_handler::Ctx,
    reqwest,
    room::{Invited, Messages, MessagesOptions, Room as MatrixRoom, RoomMember},
    ruma::{
        api::client::{
            room::create_room::v3::{Request as CreateRoomRequest, RoomPreset},
            room::Visibility,
            space::get_hierarchy::v1::Request as SpaceHierarchyRequest,
        },
        events::{
            key::verification::{
                done::{OriginalSyncKeyVerificationDoneEvent, ToDeviceKeyVerificationDoneEvent},
                key::{OriginalSyncKeyVerificationKeyEvent, ToDeviceKeyVerificationKeyEvent},
                request::ToDeviceKeyVerificationRequestEvent,
                start::{OriginalSyncKeyVerificationStartEvent, ToDeviceKeyVerificationStartEvent},
                VerificationMethod,
            },
            room::{
                message::{MessageType, RoomMessageEventContent},
                name::RoomNameEventContent,
                redaction::{OriginalSyncRoomRedactionEvent, SyncRoomRedactionEvent},
                topic::RoomTopicEventContent,
            },
            typing::SyncTypingEvent,
            AnyMessageLikeEvent,
            AnyTimelineEvent,
            SyncMessageLikeEvent,
            SyncStateEvent,
        },
        OwnedRoomId,
        OwnedRoomOrAliasId,
        OwnedUserId,
        RoomVersionId,
    },
    Client,
    DisplayName,
    Session,
};

use modalkit::editing::action::{EditInfo, InfoMessage, UIError};

use crate::{
    base::{AsyncProgramStore, IambError, IambResult, SetRoomField, VerifyAction},
    message::{Message, MessageFetchResult, MessageTimeStamp},
    ApplicationSettings,
};

const IAMB_DEVICE_NAME: &str = "iamb";
const IAMB_USER_AGENT: &str = "iamb";
const REQ_TIMEOUT: Duration = Duration::from_secs(60);

fn initial_devname() -> String {
    format!("{} on {}", IAMB_DEVICE_NAME, gethostname().to_string_lossy())
}

#[derive(Debug)]
pub enum LoginStyle {
    SessionRestore(Session),
    Password(String),
}

pub struct ClientResponse<T>(Receiver<T>);
pub struct ClientReply<T>(SyncSender<T>);

impl<T> ClientResponse<T> {
    fn recv(self) -> T {
        self.0.recv().expect("failed to receive response from client thread")
    }
}

impl<T> ClientReply<T> {
    fn send(self, t: T) {
        self.0.send(t).unwrap();
    }
}

fn oneshot<T>() -> (ClientReply<T>, ClientResponse<T>) {
    let (tx, rx) = sync_channel(1);
    let reply = ClientReply(tx);
    let response = ClientResponse(rx);

    return (reply, response);
}

pub enum WorkerTask {
    ActiveRooms(ClientReply<Vec<(MatrixRoom, DisplayName)>>),
    DirectMessages(ClientReply<Vec<(MatrixRoom, DisplayName)>>),
    Init(AsyncProgramStore, ClientReply<()>),
    LoadOlder(OwnedRoomId, Option<String>, u32, ClientReply<MessageFetchResult>),
    Login(LoginStyle, ClientReply<IambResult<EditInfo>>),
    GetInviter(Invited, ClientReply<IambResult<Option<RoomMember>>>),
    GetRoom(OwnedRoomId, ClientReply<IambResult<(MatrixRoom, DisplayName)>>),
    JoinRoom(String, ClientReply<IambResult<OwnedRoomId>>),
    Members(OwnedRoomId, ClientReply<IambResult<Vec<RoomMember>>>),
    SpaceMembers(OwnedRoomId, ClientReply<IambResult<Vec<OwnedRoomId>>>),
    Spaces(ClientReply<Vec<(MatrixRoom, DisplayName)>>),
    SetRoom(OwnedRoomId, SetRoomField, ClientReply<IambResult<()>>),
    TypingNotice(OwnedRoomId),
    Verify(VerifyAction, SasVerification, ClientReply<IambResult<EditInfo>>),
    VerifyRequest(OwnedUserId, ClientReply<IambResult<EditInfo>>),
}

impl Debug for WorkerTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            WorkerTask::ActiveRooms(_) => {
                f.debug_tuple("WorkerTask::ActiveRooms").field(&format_args!("_")).finish()
            },
            WorkerTask::DirectMessages(_) => {
                f.debug_tuple("WorkerTask::DirectMessages")
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::Init(_, _) => {
                f.debug_tuple("WorkerTask::Init")
                    .field(&format_args!("_"))
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::LoadOlder(room_id, from, n, _) => {
                f.debug_tuple("WorkerTask::LoadOlder")
                    .field(room_id)
                    .field(from)
                    .field(n)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::Login(style, _) => {
                f.debug_tuple("WorkerTask::Login")
                    .field(style)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::GetInviter(invite, _) => {
                f.debug_tuple("WorkerTask::GetInviter").field(invite).finish()
            },
            WorkerTask::GetRoom(room_id, _) => {
                f.debug_tuple("WorkerTask::GetRoom")
                    .field(room_id)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::JoinRoom(s, _) => {
                f.debug_tuple("WorkerTask::JoinRoom")
                    .field(s)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::Members(room_id, _) => {
                f.debug_tuple("WorkerTask::Members")
                    .field(room_id)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::SpaceMembers(room_id, _) => {
                f.debug_tuple("WorkerTask::SpaceMembers")
                    .field(room_id)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::Spaces(_) => {
                f.debug_tuple("WorkerTask::Spaces").field(&format_args!("_")).finish()
            },
            WorkerTask::SetRoom(room_id, field, _) => {
                f.debug_tuple("WorkerTask::SetRoom")
                    .field(room_id)
                    .field(field)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::TypingNotice(room_id) => {
                f.debug_tuple("WorkerTask::TypingNotice").field(room_id).finish()
            },
            WorkerTask::Verify(act, sasv1, _) => {
                f.debug_tuple("WorkerTask::Verify")
                    .field(act)
                    .field(sasv1)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::VerifyRequest(user_id, _) => {
                f.debug_tuple("WorkerTask::VerifyRequest")
                    .field(user_id)
                    .field(&format_args!("_"))
                    .finish()
            },
        }
    }
}

#[derive(Clone)]
pub struct Requester {
    pub client: Client,
    pub tx: UnboundedSender<WorkerTask>,
}

impl Requester {
    pub fn init(&self, store: AsyncProgramStore) {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Init(store, reply)).unwrap();

        return response.recv();
    }

    pub fn load_older(
        &self,
        room_id: OwnedRoomId,
        fetch_id: Option<String>,
        limit: u32,
    ) -> MessageFetchResult {
        let (reply, response) = oneshot();

        self.tx
            .send(WorkerTask::LoadOlder(room_id, fetch_id, limit, reply))
            .unwrap();

        return response.recv();
    }

    pub fn login(&self, style: LoginStyle) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Login(style, reply)).unwrap();

        return response.recv();
    }

    pub fn direct_messages(&self) -> Vec<(MatrixRoom, DisplayName)> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::DirectMessages(reply)).unwrap();

        return response.recv();
    }

    pub fn get_inviter(&self, invite: Invited) -> IambResult<Option<RoomMember>> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::GetInviter(invite, reply)).unwrap();

        return response.recv();
    }

    pub fn get_room(&self, room_id: OwnedRoomId) -> IambResult<(MatrixRoom, DisplayName)> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::GetRoom(room_id, reply)).unwrap();

        return response.recv();
    }

    pub fn join_room(&self, name: String) -> IambResult<OwnedRoomId> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::JoinRoom(name, reply)).unwrap();

        return response.recv();
    }

    pub fn active_rooms(&self) -> Vec<(MatrixRoom, DisplayName)> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::ActiveRooms(reply)).unwrap();

        return response.recv();
    }

    pub fn members(&self, room_id: OwnedRoomId) -> IambResult<Vec<RoomMember>> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Members(room_id, reply)).unwrap();

        return response.recv();
    }

    pub fn space_members(&self, space: OwnedRoomId) -> IambResult<Vec<OwnedRoomId>> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::SpaceMembers(space, reply)).unwrap();

        return response.recv();
    }

    pub fn set_room(&self, room_id: OwnedRoomId, ev: SetRoomField) -> IambResult<()> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::SetRoom(room_id, ev, reply)).unwrap();

        return response.recv();
    }

    pub fn spaces(&self) -> Vec<(MatrixRoom, DisplayName)> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Spaces(reply)).unwrap();

        return response.recv();
    }

    pub fn typing_notice(&self, room_id: OwnedRoomId) {
        self.tx.send(WorkerTask::TypingNotice(room_id)).unwrap();
    }

    pub fn verify(&self, act: VerifyAction, sas: SasVerification) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Verify(act, sas, reply)).unwrap();

        return response.recv();
    }

    pub fn verify_request(&self, user_id: OwnedUserId) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::VerifyRequest(user_id, reply)).unwrap();

        return response.recv();
    }
}

pub struct ClientWorker {
    initialized: bool,
    settings: ApplicationSettings,
    client: Client,
    sync_handle: Option<JoinHandle<()>>,
}

impl ClientWorker {
    pub async fn spawn(settings: ApplicationSettings) -> Requester {
        let (tx, rx) = unbounded_channel();
        let account = &settings.profile;

        // Set up a custom client that only uses HTTP/1.
        //
        // During my testing, I kept stumbling across something weird with sync and HTTP/2 that
        // will need to be revisited in the future.
        let http = reqwest::Client::builder()
            .user_agent(IAMB_USER_AGENT)
            .timeout(Duration::from_secs(30))
            .pool_idle_timeout(Duration::from_secs(60))
            .pool_max_idle_per_host(10)
            .tcp_keepalive(Duration::from_secs(10))
            .http1_only()
            .build()
            .unwrap();

        // Set up the Matrix client for the selected profile.
        let client = Client::builder()
            .http_client(Arc::new(http))
            .homeserver_url(account.url.clone())
            .store_config(StoreConfig::default())
            .sled_store(settings.matrix_dir.as_path(), None)
            .expect("Failed to setup up sled store for Matrix SDK")
            .request_config(RequestConfig::new().timeout(REQ_TIMEOUT).retry_timeout(REQ_TIMEOUT))
            .build()
            .await
            .expect("Failed to instantiate Matrix client");

        let mut worker = ClientWorker {
            initialized: false,
            settings,
            client: client.clone(),
            sync_handle: None,
        };

        let _ = tokio::spawn(async move {
            worker.work(rx).await;
        });

        return Requester { client, tx };
    }

    async fn work(&mut self, mut rx: UnboundedReceiver<WorkerTask>) {
        loop {
            let t = rx.recv().await;

            match t {
                Some(task) => self.run(task).await,
                None => {
                    break;
                },
            }
        }

        if let Some(handle) = self.sync_handle.take() {
            handle.abort();
        }
    }

    async fn run(&mut self, task: WorkerTask) {
        match task {
            WorkerTask::DirectMessages(reply) => {
                assert!(self.initialized);
                reply.send(self.direct_messages().await);
            },
            WorkerTask::Init(store, reply) => {
                assert_eq!(self.initialized, false);
                self.init(store).await;
                reply.send(());
            },
            WorkerTask::JoinRoom(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.join_room(room_id).await);
            },
            WorkerTask::GetInviter(invited, reply) => {
                assert!(self.initialized);
                reply.send(self.get_inviter(invited).await);
            },
            WorkerTask::GetRoom(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.get_room(room_id).await);
            },
            WorkerTask::ActiveRooms(reply) => {
                assert!(self.initialized);
                reply.send(self.active_rooms().await);
            },
            WorkerTask::LoadOlder(room_id, fetch_id, limit, reply) => {
                assert!(self.initialized);
                reply.send(self.load_older(room_id, fetch_id, limit).await);
            },
            WorkerTask::Login(style, reply) => {
                assert!(self.initialized);
                reply.send(self.login_and_sync(style).await);
            },
            WorkerTask::Members(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.members(room_id).await);
            },
            WorkerTask::SetRoom(room_id, field, reply) => {
                assert!(self.initialized);
                reply.send(self.set_room(room_id, field).await);
            },
            WorkerTask::SpaceMembers(space, reply) => {
                assert!(self.initialized);
                reply.send(self.space_members(space).await);
            },
            WorkerTask::Spaces(reply) => {
                assert!(self.initialized);
                reply.send(self.spaces().await);
            },
            WorkerTask::TypingNotice(room_id) => {
                assert!(self.initialized);
                self.typing_notice(room_id).await;
            },
            WorkerTask::Verify(act, sas, reply) => {
                assert!(self.initialized);
                reply.send(self.verify(act, sas).await);
            },
            WorkerTask::VerifyRequest(user_id, reply) => {
                assert!(self.initialized);
                reply.send(self.verify_request(user_id).await);
            },
        }
    }

    async fn init(&mut self, store: AsyncProgramStore) {
        self.client.add_event_handler_context(store);

        let _ = self.client.add_event_handler(
            |ev: SyncTypingEvent, room: MatrixRoom, store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id().to_owned();
                    let mut locked = store.lock().await;

                    let users = ev
                        .content
                        .user_ids
                        .into_iter()
                        .filter(|u| u != &locked.application.settings.profile.user_id)
                        .collect();

                    locked.application.get_room_info(room_id).set_typing(users);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncStateEvent<RoomNameEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    if let SyncStateEvent::Original(ev) = ev {
                        if let Some(room_name) = ev.content.name {
                            let room_id = room.room_id().to_owned();
                            let room_name = Some(room_name.to_string());
                            let mut locked = store.lock().await;
                            let mut info =
                                locked.application.rooms.entry(room_id.to_owned()).or_default();
                            info.name = room_name;
                        }
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<RoomMessageEventContent>,
             room: MatrixRoom,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();
                    let room_name = room.display_name().await.ok();
                    let room_name = room_name.as_ref().map(ToString::to_string);

                    if let Some(msg) = ev.as_original() {
                        if let MessageType::VerificationRequest(_) = msg.content.msgtype {
                            if let Some(request) = client
                                .encryption()
                                .get_verification_request(ev.sender(), ev.event_id())
                                .await
                            {
                                request.accept().await.expect("Failed to accept request");
                            }
                        }
                    }

                    let mut locked = store.lock().await;
                    let mut info = locked.application.get_room_info(room_id.to_owned());
                    info.name = room_name;

                    let event_id = ev.event_id().to_owned();
                    let key = (ev.origin_server_ts().into(), event_id.clone());
                    let msg = Message::from(ev.into_full_event(room_id.to_owned()));
                    info.messages.insert(key, msg);

                    // Remove the echo.
                    let key = (MessageTimeStamp::LocalEcho, event_id);
                    let _ = info.messages.remove(&key);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncRoomRedactionEvent,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();
                    let room_info = room.clone_info();
                    let room_version = room_info.room_version().unwrap_or(&RoomVersionId::V1);

                    let mut locked = store.lock().await;
                    let info = locked.application.get_room_info(room_id.to_owned());

                    // XXX: need to store a mapping of EventId -> MessageKey somewhere
                    // to avoid having to iterate over the messages here.
                    for ((_, id), msg) in info.messages.iter_mut().rev() {
                        if id != &ev.redacts {
                            continue;
                        }

                        let ev = SyncRoomRedactionEvent::Original(ev);
                        msg.event.redact(ev, room_version);

                        break;
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncKeyVerificationStartEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let tx_id = ev.content.relates_to.event_id.as_ref();

                    if let Some(Verification::SasV1(sas)) =
                        client.encryption().get_verification(&ev.sender, tx_id).await
                    {
                        sas.accept().await.unwrap();

                        store.lock().await.application.insert_sas(sas)
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncKeyVerificationKeyEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let tx_id = ev.content.relates_to.event_id.as_ref();

                    if let Some(Verification::SasV1(sas)) =
                        client.encryption().get_verification(&ev.sender, tx_id).await
                    {
                        store.lock().await.application.insert_sas(sas);
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncKeyVerificationDoneEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let tx_id = ev.content.relates_to.event_id.as_ref();

                    if let Some(Verification::SasV1(sas)) =
                        client.encryption().get_verification(&ev.sender, tx_id).await
                    {
                        store.lock().await.application.insert_sas(sas);
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: ToDeviceKeyVerificationRequestEvent, client: Client| {
                async move {
                    let request = client
                        .encryption()
                        .get_verification_request(&ev.sender, &ev.content.transaction_id)
                        .await
                        .unwrap();

                    request.accept().await.unwrap();
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: ToDeviceKeyVerificationStartEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let tx_id = ev.content.transaction_id;

                    if let Some(Verification::SasV1(sas)) =
                        client.encryption().get_verification(&ev.sender, tx_id.as_ref()).await
                    {
                        sas.accept().await.unwrap();

                        store.lock().await.application.insert_sas(sas);
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: ToDeviceKeyVerificationKeyEvent, client: Client, store: Ctx<AsyncProgramStore>| {
                async move {
                    let tx_id = ev.content.transaction_id;

                    if let Some(Verification::SasV1(sas)) =
                        client.encryption().get_verification(&ev.sender, tx_id.as_ref()).await
                    {
                        store.lock().await.application.insert_sas(sas);
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: ToDeviceKeyVerificationDoneEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let tx_id = ev.content.transaction_id;

                    if let Some(Verification::SasV1(sas)) =
                        client.encryption().get_verification(&ev.sender, tx_id.as_ref()).await
                    {
                        store.lock().await.application.insert_sas(sas);
                    }
                }
            },
        );

        self.initialized = true;
    }

    async fn login_and_sync(&mut self, style: LoginStyle) -> IambResult<EditInfo> {
        let client = self.client.clone();

        match style {
            LoginStyle::SessionRestore(session) => {
                client.restore_login(session).await.map_err(IambError::from)?;
            },
            LoginStyle::Password(password) => {
                let resp = client
                    .login_username(&self.settings.profile.user_id, &password)
                    .initial_device_display_name(initial_devname().as_str())
                    .send()
                    .await
                    .map_err(IambError::from)?;
                let file = File::create(self.settings.session_json.as_path())?;
                let writer = BufWriter::new(file);
                let session = Session::from(resp);
                serde_json::to_writer(writer, &session).map_err(IambError::from)?;
            },
        }

        let handle = tokio::spawn(async move {
            loop {
                let settings = SyncSettings::default();

                let _ = client.sync(settings).await;
            }
        });

        self.sync_handle = Some(handle);

        self.client
            .sync_once(SyncSettings::default())
            .await
            .map_err(IambError::from)?;

        Ok(Some(InfoMessage::from("Successfully logged in!")))
    }

    async fn direct_message(&mut self, user: OwnedUserId) -> IambResult<(MatrixRoom, DisplayName)> {
        for (room, name) in self.direct_messages().await {
            if room.get_member(user.as_ref()).await.map_err(IambError::from)?.is_some() {
                return Ok((room, name));
            }
        }

        let mut request = CreateRoomRequest::new();
        let invite = [user.clone()];
        request.is_direct = true;
        request.invite = &invite;
        request.visibility = Visibility::Private;
        request.preset = Some(RoomPreset::PrivateChat);

        match self.client.create_room(request).await {
            Ok(resp) => self.get_room(resp.room_id).await,
            Err(e) => {
                error!(
                    user_id = user.as_str(),
                    err = e.to_string(),
                    "Failed to create direct message room"
                );

                let msg = format!("Could not open a room with {}", user);
                let err = UIError::Failure(msg);

                Err(err)
            },
        }
    }

    async fn get_inviter(&mut self, invited: Invited) -> IambResult<Option<RoomMember>> {
        let details = invited.invite_details().await.map_err(IambError::from)?;

        Ok(details.inviter)
    }

    async fn get_room(&mut self, room_id: OwnedRoomId) -> IambResult<(MatrixRoom, DisplayName)> {
        if let Some(room) = self.client.get_room(&room_id) {
            let name = room.display_name().await.map_err(IambError::from)?;

            Ok((room, name))
        } else {
            Err(IambError::UnknownRoom(room_id).into())
        }
    }

    async fn join_room(&mut self, name: String) -> IambResult<OwnedRoomId> {
        if let Ok(alias_id) = OwnedRoomOrAliasId::from_str(name.as_str()) {
            match self.client.join_room_by_id_or_alias(&alias_id, &[]).await {
                Ok(resp) => Ok(resp.room_id),
                Err(e) => {
                    let msg = e.to_string();
                    let err = UIError::Failure(msg);

                    return Err(err);
                },
            }
        } else if let Ok(user) = OwnedUserId::try_from(name.as_str()) {
            let room = self.direct_message(user).await?.0;

            return Ok(room.room_id().to_owned());
        } else {
            let msg = format!("{:?} is not a valid room or user name", name.as_str());
            let err = UIError::Failure(msg);

            return Err(err);
        }
    }

    async fn direct_messages(&self) -> Vec<(MatrixRoom, DisplayName)> {
        let mut rooms = vec![];

        for room in self.client.invited_rooms().into_iter() {
            if !room.is_direct() {
                continue;
            }

            let name = room.display_name().await.unwrap_or(DisplayName::Empty);

            rooms.push((room.into(), name));
        }

        for room in self.client.joined_rooms().into_iter() {
            if !room.is_direct() {
                continue;
            }

            let name = room.display_name().await.unwrap_or(DisplayName::Empty);

            rooms.push((room.into(), name));
        }

        return rooms;
    }

    async fn active_rooms(&self) -> Vec<(MatrixRoom, DisplayName)> {
        let mut rooms = vec![];

        for room in self.client.invited_rooms().into_iter() {
            if room.is_space() || room.is_direct() {
                continue;
            }

            let name = room.display_name().await.unwrap_or(DisplayName::Empty);

            rooms.push((room.into(), name));
        }

        for room in self.client.joined_rooms().into_iter() {
            if room.is_space() || room.is_direct() {
                continue;
            }

            let name = room.display_name().await.unwrap_or(DisplayName::Empty);

            rooms.push((room.into(), name));
        }

        return rooms;
    }

    async fn load_older(
        &mut self,
        room_id: OwnedRoomId,
        fetch_id: Option<String>,
        limit: u32,
    ) -> MessageFetchResult {
        if let Some(room) = self.client.get_room(room_id.as_ref()) {
            let mut opts = match &fetch_id {
                Some(id) => MessagesOptions::backward().from(id.as_str()),
                None => MessagesOptions::backward(),
            };
            opts.limit = limit.into();

            let Messages { end, chunk, .. } = room.messages(opts).await.map_err(IambError::from)?;

            let msgs = chunk.into_iter().filter_map(|ev| {
                match ev.event.deserialize() {
                    Ok(AnyTimelineEvent::MessageLike(msg)) => {
                        if let AnyMessageLikeEvent::RoomMessage(msg) = msg {
                            Some(msg)
                        } else {
                            None
                        }
                    },
                    Ok(AnyTimelineEvent::State(_)) => None,
                    Err(_) => None,
                }
            });

            Ok((end, msgs.collect()))
        } else {
            Err(IambError::UnknownRoom(room_id).into())
        }
    }

    async fn members(&mut self, room_id: OwnedRoomId) -> IambResult<Vec<RoomMember>> {
        if let Some(room) = self.client.get_room(room_id.as_ref()) {
            Ok(room.active_members().await.map_err(IambError::from)?)
        } else {
            Err(IambError::UnknownRoom(room_id).into())
        }
    }

    async fn set_room(&mut self, room_id: OwnedRoomId, field: SetRoomField) -> IambResult<()> {
        let room = if let Some(r) = self.client.get_joined_room(&room_id) {
            r
        } else {
            return Err(IambError::UnknownRoom(room_id).into());
        };

        match field {
            SetRoomField::Name(name) => {
                let ev = RoomNameEventContent::new(name.into());
                let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
            },
            SetRoomField::Topic(topic) => {
                let ev = RoomTopicEventContent::new(topic);
                let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
            },
        }

        Ok(())
    }

    async fn space_members(&mut self, space: OwnedRoomId) -> IambResult<Vec<OwnedRoomId>> {
        let mut req = SpaceHierarchyRequest::new(&space);
        req.limit = Some(1000u32.into());
        req.max_depth = Some(1u32.into());

        let resp = self.client.send(req, None).await.map_err(IambError::from)?;

        let rooms = resp.rooms.into_iter().map(|chunk| chunk.room_id).collect();

        Ok(rooms)
    }

    async fn spaces(&self) -> Vec<(MatrixRoom, DisplayName)> {
        let mut spaces = vec![];

        for room in self.client.invited_rooms().into_iter() {
            if !room.is_space() {
                continue;
            }

            let name = room.display_name().await.unwrap_or(DisplayName::Empty);

            spaces.push((room.into(), name));
        }

        for room in self.client.joined_rooms().into_iter() {
            if !room.is_space() {
                continue;
            }

            let name = room.display_name().await.unwrap_or(DisplayName::Empty);

            spaces.push((room.into(), name));
        }

        return spaces;
    }

    async fn typing_notice(&mut self, room_id: OwnedRoomId) {
        if let Some(room) = self.client.get_joined_room(room_id.as_ref()) {
            let _ = room.typing_notice(true).await;
        }
    }

    async fn verify(&self, action: VerifyAction, sas: SasVerification) -> IambResult<EditInfo> {
        match action {
            VerifyAction::Accept => {
                sas.accept().await.map_err(IambError::from)?;

                Ok(Some(InfoMessage::from("Accepted verification request")))
            },
            VerifyAction::Confirm => {
                if sas.is_done() || sas.is_cancelled() {
                    let msg = "Can only confirm in-progress verifications!";
                    let err = UIError::Failure(msg.into());

                    return Err(err);
                }

                sas.confirm().await.map_err(IambError::from)?;

                Ok(Some(InfoMessage::from("Confirmed verification")))
            },
            VerifyAction::Cancel => {
                if sas.is_done() || sas.is_cancelled() {
                    let msg = "Can only cancel in-progress verifications!";
                    let err = UIError::Failure(msg.into());

                    return Err(err);
                }

                sas.cancel().await.map_err(IambError::from)?;

                Ok(Some(InfoMessage::from("Cancelled verification")))
            },
            VerifyAction::Mismatch => {
                if sas.is_done() || sas.is_cancelled() {
                    let msg = "Can only cancel in-progress verifications!";
                    let err = UIError::Failure(msg.into());

                    return Err(err);
                }

                sas.mismatch().await.map_err(IambError::from)?;

                Ok(Some(InfoMessage::from("Cancelled verification")))
            },
        }
    }

    async fn verify_request(&self, user_id: OwnedUserId) -> IambResult<EditInfo> {
        let enc = self.client.encryption();

        match enc.get_user_identity(user_id.as_ref()).await.map_err(IambError::from)? {
            Some(identity) => {
                let methods = vec![VerificationMethod::SasV1];
                let request = identity.request_verification_with_methods(methods);
                let _req = request.await.map_err(IambError::from)?;
                let info = format!("Sent verification request to {}", user_id);

                Ok(InfoMessage::from(info).into())
            },
            None => {
                let msg = format!("Could not find identity information for {}", user_id);
                let err = UIError::Failure(msg);

                Err(err)
            },
        }
    }
}
