//! # Async Matrix Client Worker
//!
//! The worker thread handles asynchronous work, and can receive messages from the main thread that
//! block on a reply from the async worker.
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::str::FromStr;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::{Arc, Weak};
use std::time::Duration;

use gethostname::gethostname;
use matrix_sdk::room::Invite;
use matrix_sdk::ruma::api::client::receipt::create_receipt::v3::ReceiptType;
use matrix_sdk::ruma::events::fully_read::FullyReadEventContent;
use matrix_sdk::ruma::events::receipt::ReceiptThread;
use matrix_sdk::ruma::events::room::message::{MessageType, OriginalSyncRoomMessageEvent};
use matrix_sdk::ruma::events::room::MediaSource;
use matrix_sdk::ruma::OwnedEventId;
use matrix_sdk_ui::timeline::TimelineEventItemId;
use ratatui_image::picker::Picker;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tokio::sync::{Mutex, Semaphore};
use tokio::task::JoinHandle;
use tracing::error;
use url::Url;

use matrix_sdk::{
    authentication::matrix::MatrixSession,
    config::{RequestConfig, SyncSettings},
    encryption::verification::{SasVerification, Verification},
    encryption::{BackupDownloadStrategy, EncryptionSettings},
    event_handler::Ctx,
    reqwest,
    room::{Room as MatrixRoom, RoomMember},
    ruma::{
        api::client::{
            filter::{FilterDefinition, LazyLoadOptions, RoomEventFilter, RoomFilter},
            room::create_room::v3::{CreationContent, Request as CreateRoomRequest, RoomPreset},
            room::Visibility,
            space::get_hierarchy::v1::Request as SpaceHierarchyRequest,
        },
        assign,
        events::{
            key::verification::{
                done::{OriginalSyncKeyVerificationDoneEvent, ToDeviceKeyVerificationDoneEvent},
                key::{OriginalSyncKeyVerificationKeyEvent, ToDeviceKeyVerificationKeyEvent},
                request::ToDeviceKeyVerificationRequestEvent,
                start::{OriginalSyncKeyVerificationStartEvent, ToDeviceKeyVerificationStartEvent},
                VerificationMethod,
            },
            presence::PresenceEvent,
            room::encryption::RoomEncryptionEventContent,
            typing::SyncTypingEvent,
            AnyInitialStateEvent,
            EmptyStateKey,
            InitialStateEvent,
        },
        room::RoomType,
        serde::Raw,
        EventEncryptionAlgorithm,
        OwnedRoomId,
        OwnedRoomOrAliasId,
        OwnedUserId,
    },
    Client,
    ClientBuildError,
    Error as MatrixError,
    RoomMemberships,
};

use modalkit::errors::UIError;
use modalkit::prelude::{EditInfo, InfoMessage};

use crate::base::RoomInfo;
use crate::config::ImagePreviewSize;
use crate::message::html::StyleTree;
use crate::message::Messages;
use crate::notifications::register_notifications;
use crate::preview::load_image;
use crate::{
    base::{
        AsyncProgramStore,
        CreateRoomFlags,
        CreateRoomType,
        IambError,
        IambResult,
        ProgramStore,
        VerifyAction,
    },
    ApplicationSettings,
};

const DEFAULT_ENCRYPTION_SETTINGS: EncryptionSettings = EncryptionSettings {
    auto_enable_cross_signing: true,
    auto_enable_backups: true,
    backup_download_strategy: BackupDownloadStrategy::AfterDecryptionFailure,
};

const IAMB_DEVICE_NAME: &str = "iamb";
const IAMB_USER_AGENT: &str = "iamb";

fn initial_devname() -> String {
    format!("{} on {}", IAMB_DEVICE_NAME, gethostname().to_string_lossy())
}

async fn is_direct(room: &MatrixRoom) -> bool {
    room.deref().is_direct().await.unwrap_or_default()
}

pub async fn create_room(
    client: &Client,
    room_alias_name: Option<String>,
    rt: CreateRoomType,
    flags: CreateRoomFlags,
) -> IambResult<MatrixRoom> {
    let mut creation_content = None;
    let mut initial_state = vec![];
    let mut is_direct = false;
    let mut preset = None;
    let mut invite = vec![];

    let visibility = if flags.contains(CreateRoomFlags::PUBLIC) {
        Visibility::Public
    } else {
        Visibility::Private
    };

    match rt {
        CreateRoomType::Direct(user) => {
            invite.push(user);
            is_direct = true;
            preset = Some(RoomPreset::TrustedPrivateChat);
        },
        CreateRoomType::Space => {
            let mut cc = CreationContent::new();
            cc.room_type = Some(RoomType::Space);

            let raw_cc = Raw::new(&cc).map_err(IambError::from)?;
            creation_content = Some(raw_cc);
        },
        CreateRoomType::Room => {},
    }

    // Set up encryption.
    if flags.contains(CreateRoomFlags::ENCRYPTED) {
        // XXX: Once matrix-sdk uses ruma 0.8, then this can skip the cast.
        let algo = EventEncryptionAlgorithm::MegolmV1AesSha2;
        let content = RoomEncryptionEventContent::new(algo);
        let encr = InitialStateEvent::new(EmptyStateKey, content);
        let encr_raw = Raw::new(&encr).map_err(IambError::from)?;
        let encr_raw = encr_raw.cast::<AnyInitialStateEvent>();
        initial_state.push(encr_raw);
    }

    let request = assign!(CreateRoomRequest::new(), {
        room_alias_name,
        creation_content,
        initial_state,
        invite,
        is_direct,
        visibility,
        preset,
    });

    let resp = client.create_room(request).await.map_err(IambError::from)?;

    if is_direct {
        if let Some(room) = client.get_room(resp.room_id()) {
            room.set_is_direct(true).await.map_err(IambError::from)?;
        } else {
            error!(
                room_id = resp.room_id().as_str(),
                "Couldn't set is_direct for new direct message room"
            );
        }
    }

    return Ok(resp);
}

async fn refresh_matrix_room(room: &MatrixRoom, store: &mut ProgramStore) {
    if let Some(alias) = room.canonical_alias() {
        store
            .application
            .names
            .insert(alias.to_string(), room.room_id().to_owned());
    }

    match room.members(RoomMemberships::ACTIVE).await {
        Ok(members) => {
            for member in members {
                store.application.presences.get_or_default(member.user_id().to_owned());
            }
        },
        Err(err) => {
            tracing::warn!(room_id = ?room.room_id(), "cannot load room members: {err}");
        },
    }

    // pre-compute name
    if let Err(err) = room.display_name().await {
        tracing::warn!(room_id = ?room.room_id(), "cannot load room name: {err}");
    };
}

async fn refresh_rooms(client: &Client, store: &AsyncProgramStore) {
    let mut spaces = vec![];
    let mut rooms = vec![];
    let mut dms = vec![];
    let mut invites = vec![];

    let mut locked = store.lock().await;

    for room in client.invited_rooms() {
        refresh_matrix_room(&room, &mut locked).await;
        invites.push(room.room_id().to_owned());
    }

    for room in client.joined_rooms() {
        refresh_matrix_room(&room, &mut locked).await;

        if room.is_space() {
            spaces.push(room.room_id().to_owned());
            continue;
        }

        if is_direct(&room).await {
            dms.push(room.room_id().to_owned());
        } else {
            rooms.push(room.room_id().to_owned());
        }

        // only create `RoomInfo` for chats

        if locked.application.rooms.get(room.room_id()).is_none() {
            let info = match RoomInfo::new(&room, Arc::clone(store)).await {
                Ok(info) => info,
                Err(err) => {
                    tracing::warn!(room_id = ?room.room_id(), "cannot create room info: {err}");
                    continue;
                },
            };

            match room.account_data_static::<FullyReadEventContent>().await {
                Ok(Some(_)) => (),
                Ok(None) => {
                    // initialize with normal read marker
                    if let Some((event_id, _)) = info
                        .get_thread(None)
                        .unwrap()
                        .timeline()
                        .latest_user_read_receipt(client.user_id().unwrap())
                        .await
                    {
                        if let Err(err) = room
                            .send_single_receipt(
                                ReceiptType::FullyRead,
                                ReceiptThread::Unthreaded,
                                event_id,
                            )
                            .await
                        {
                            tracing::warn!(room_id = ?room.room_id(), "cannot set default fully_read marker: {err}");
                        }
                    }
                },
                Err(err) => {
                    tracing::warn!(room_id = ?room.room_id(), "failed to load fully_read marker: {err}");
                },
            };

            locked.application.rooms.insert(room.room_id().to_owned(), info);
        }
        let info = locked
            .application
            .rooms
            .get_mut(room.room_id())
            .expect("value should have been inserted");

        match room.tags().await {
            Ok(tags) => info.tags = tags,
            Err(err) => {
                tracing::warn!(room_id = ?room.room_id(), "cannot to load room tags: {err}");
            },
        }
    }

    locked.application.sync_info.spaces = spaces;
    locked.application.sync_info.rooms = rooms;
    locked.application.sync_info.dms = dms;
    locked.application.sync_info.invites = invites;
}

async fn refresh_rooms_forever(client: &Client, store: &AsyncProgramStore) {
    let mut interval = tokio::time::interval(Duration::from_secs(5));

    loop {
        refresh_rooms(client, store).await;
        interval.tick().await;
    }
}

pub async fn do_first_sync(client: &Client, store: &AsyncProgramStore) -> Result<(), MatrixError> {
    // Perform an initial, lazily-loaded sync.
    let mut room = RoomEventFilter::default();
    room.lazy_load_options = LazyLoadOptions::Enabled { include_redundant_members: false };

    let mut room_ev = RoomFilter::default();
    room_ev.state = room;

    let mut filter = FilterDefinition::default();
    filter.room = room_ev;

    let settings = SyncSettings::new().filter(filter.into()).timeout(Duration::from_secs(0));

    client.sync_once(settings).await?;

    // Populate sync_info with our initial set of rooms/dms/spaces.
    refresh_rooms(client, store).await;

    Ok(())
}

#[derive(Debug)]
pub enum LoginStyle {
    SessionRestore(MatrixSession),
    Password(String),
    SingleSignOn,
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

pub type MessagesResult =
    Result<(Messages, HashMap<TimelineEventItemId, StyleTree>), matrix_sdk_ui::timeline::Error>;

pub enum WorkerTask {
    Init(AsyncProgramStore, ClientReply<()>),
    Login(LoginStyle, ClientReply<IambResult<EditInfo>>),
    Logout(String, ClientReply<IambResult<EditInfo>>),
    GetInviter(MatrixRoom, ClientReply<Result<Invite, matrix_sdk::Error>>),
    GetMessages(MatrixRoom, Option<OwnedEventId>, ClientReply<MessagesResult>),
    JoinRoom(String, ClientReply<IambResult<MatrixRoom>>),
    CreateRoomInfo(MatrixRoom, ClientReply<Result<RoomInfo, matrix_sdk_ui::timeline::Error>>),
    Members(OwnedRoomId, ClientReply<IambResult<Vec<RoomMember>>>),
    SpaceMembers(OwnedRoomId, ClientReply<IambResult<Vec<OwnedRoomId>>>),
    TypingNotice(OwnedRoomId),
    Verify(VerifyAction, SasVerification, ClientReply<IambResult<EditInfo>>),
    VerifyRequest(OwnedUserId, ClientReply<IambResult<EditInfo>>),
    LoadImage(MediaSource, ImagePreviewSize, Arc<Picker>, Arc<Semaphore>),
}

impl Debug for WorkerTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            WorkerTask::Init(_, _) => {
                f.debug_tuple("WorkerTask::Init")
                    .field(&format_args!("_"))
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::Login(style, _) => {
                f.debug_tuple("WorkerTask::Login")
                    .field(style)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::Logout(user_id, _) => {
                f.debug_tuple("WorkerTask::Logout").field(user_id).finish()
            },
            WorkerTask::GetInviter(invite, _) => {
                f.debug_tuple("WorkerTask::GetInviter").field(invite).finish()
            },
            WorkerTask::GetMessages(room, thread, _) => {
                f.debug_tuple("WorkerTask::GetMessages")
                    .field(&room.room_id())
                    .field(thread)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::JoinRoom(s, _) => {
                f.debug_tuple("WorkerTask::JoinRoom")
                    .field(s)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::CreateRoomInfo(room, _) => {
                f.debug_tuple("WorkerTask::CreateRoomInfo")
                    .field(room)
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
            WorkerTask::LoadImage(source, size, _, _) => {
                f.debug_tuple("WorkerTask::RenderImage")
                    .field(source)
                    .field(size)
                    .field(&format_args!("_"))
                    .field(&format_args!("_"))
                    .finish()
            },
        }
    }
}

async fn create_client_inner(
    homeserver: &Option<Url>,
    settings: &ApplicationSettings,
) -> Result<Client, ClientBuildError> {
    let req_timeout = Duration::from_secs(settings.tunables.request_timeout);

    // Set up the HTTP client.
    let http = reqwest::Client::builder()
        .user_agent(IAMB_USER_AGENT)
        .timeout(req_timeout)
        .pool_idle_timeout(Duration::from_secs(60))
        .pool_max_idle_per_host(10)
        .tcp_keepalive(Duration::from_secs(10))
        .build()
        .unwrap();

    let req_config = RequestConfig::new().timeout(req_timeout).max_retry_time(req_timeout);

    // Set up the Matrix client for the selected profile.
    let builder = Client::builder()
        .http_client(http)
        .sqlite_store(settings.sqlite_dir.as_path(), None)
        .request_config(req_config)
        .with_encryption_settings(DEFAULT_ENCRYPTION_SETTINGS);

    let builder = if let Some(url) = homeserver {
        // Use the explicitly specified homeserver.
        builder.homeserver_url(url.as_str())
    } else {
        // Try to discover the homeserver from the user ID.
        let account = &settings.profile;
        builder.server_name(account.user_id.server_name())
    };

    builder.build().await
}

pub async fn create_client(settings: &ApplicationSettings) -> Client {
    let account = &settings.profile;
    let res = match create_client_inner(&account.url, settings).await {
        Err(ClientBuildError::AutoDiscovery(_)) => {
            let url = format!("https://{}/", account.user_id.server_name().as_str());
            let url = Url::parse(&url).unwrap();
            create_client_inner(&Some(url), settings).await
        },
        res => res,
    };

    res.expect("Failed to instantiate client")
}

#[derive(Clone)]
pub struct Requester {
    pub client: Client,
    pub tx: UnboundedSender<WorkerTask>,
    pub store: Weak<Mutex<ProgramStore>>,
}

impl Requester {
    pub fn init(&self, store: AsyncProgramStore) {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Init(store, reply)).unwrap();

        return response.recv();
    }

    pub fn login(&self, style: LoginStyle) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Login(style, reply)).unwrap();

        return response.recv();
    }

    pub fn logout(&self, user_id: String) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Logout(user_id, reply)).unwrap();

        return response.recv();
    }

    pub fn get_inviter(&self, invite: MatrixRoom) -> Result<Invite, matrix_sdk::Error> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::GetInviter(invite, reply)).unwrap();

        return response.recv();
    }

    pub fn get_messages(&self, room: MatrixRoom, thread: Option<OwnedEventId>) -> MessagesResult {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::GetMessages(room, thread, reply)).unwrap();

        return response.recv();
    }

    pub fn join_room(&self, name: String) -> IambResult<MatrixRoom> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::JoinRoom(name, reply)).unwrap();

        return response.recv();
    }

    pub fn create_room_info(
        &self,
        room: MatrixRoom,
    ) -> Result<RoomInfo, matrix_sdk_ui::timeline::Error> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::CreateRoomInfo(room, reply)).unwrap();

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

    pub fn load_image(
        &self,
        source: MediaSource,
        size: ImagePreviewSize,
        picker: Arc<Picker>,
        permits: Arc<Semaphore>,
    ) {
        self.tx.send(WorkerTask::LoadImage(source, size, picker, permits)).unwrap();
    }
}

pub struct ClientWorker {
    initialized: bool,
    settings: ApplicationSettings,
    client: Client,
    load_handle: Option<JoinHandle<()>>,
    sync_handle: Option<JoinHandle<()>>,

    /// Take care when locking since worker commands are sent with the lock already hold
    store: Option<AsyncProgramStore>,
}

impl ClientWorker {
    pub async fn spawn(client: Client, settings: ApplicationSettings) -> Requester {
        let (tx, rx) = unbounded_channel();

        let mut worker = ClientWorker {
            initialized: false,
            settings,
            client: client.clone(),
            load_handle: None,
            sync_handle: None,
            store: None,
        };

        tokio::spawn(async move {
            worker.work(rx).await;
        });

        return Requester { client, tx, store: Weak::new() };
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
            WorkerTask::Init(store, reply) => {
                assert_eq!(self.initialized, false);
                self.init(store).await;
                reply.send(());
            },
            WorkerTask::JoinRoom(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.join_room(room_id).await);
            },
            WorkerTask::CreateRoomInfo(room, reply) => {
                assert!(self.initialized);
                reply.send(RoomInfo::new(&room, Arc::clone(self.store.as_ref().unwrap())).await);
            },
            WorkerTask::GetInviter(invited, reply) => {
                assert!(self.initialized);
                reply.send(invited.invite_details().await);
            },
            WorkerTask::GetMessages(room, thread, reply) => {
                assert!(self.initialized);
                reply.send(self.get_messages(room, thread).await);
            },
            WorkerTask::Login(style, reply) => {
                assert!(self.initialized);
                reply.send(self.login_and_sync(style).await);
            },
            WorkerTask::Logout(user_id, reply) => {
                assert!(self.initialized);
                reply.send(self.logout(user_id).await);
            },
            WorkerTask::Members(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.members(room_id).await);
            },
            WorkerTask::SpaceMembers(space, reply) => {
                assert!(self.initialized);
                reply.send(self.space_members(space).await);
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
            WorkerTask::LoadImage(source, size, picker, permits) => {
                assert!(self.initialized);
                tokio::spawn(load_image(
                    self.store.clone().unwrap(),
                    self.client.media(),
                    source,
                    picker,
                    permits,
                    size,
                ));
            },
        }
    }

    async fn init(&mut self, store: AsyncProgramStore) {
        store.lock().await.application.worker.store = Arc::downgrade(&store);
        self.client.add_event_handler_context(store.clone());

        let _ = self.client.add_event_handler(
            |ev: SyncTypingEvent, room: MatrixRoom, store: Ctx<AsyncProgramStore>| {
                async move {
                    let mut locked = store.lock().await;

                    let mut users = vec![];

                    for user_id in ev.content.user_ids {
                        if user_id == locked.application.settings.profile.user_id {
                            continue;
                        }

                        let display_name =
                            room.get_member_no_sync(&user_id).await.ok().flatten().and_then(
                                |member| member.display_name().map(|name| name.to_owned()),
                            );

                        users.push((user_id, display_name));
                    }

                    if let Some(info) = locked.application.rooms.get_mut(room.room_id()) {
                        info.set_typing(users);
                    }
                }
            },
        );

        let _ =
            self.client
                .add_event_handler(|ev: PresenceEvent, store: Ctx<AsyncProgramStore>| {
                    async move {
                        let mut locked = store.lock().await;
                        locked.application.presences.insert(ev.sender, ev.content.presence);
                    }
                });

        let _ =
            self.client
                .add_event_handler(|ev: OriginalSyncRoomMessageEvent, client: Client| {
                    async move {
                        if let MessageType::VerificationRequest(_) = ev.content.msgtype {
                            if let Some(request) = client
                                .encryption()
                                .get_verification_request(&ev.sender, &ev.event_id)
                                .await
                            {
                                request.accept().await.expect("Failed to accept request");
                            }
                        }
                    }
                });

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
                        .await;

                    if let Some(request) = request {
                        request.accept().await.unwrap();
                    }
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

        self.store = Some(store.clone());

        self.load_handle = tokio::spawn({
            let client = self.client.clone();
            let settings = self.settings.clone();

            async move {
                while !client.is_active() {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                }

                let room = refresh_rooms_forever(&client, &store);
                let notifications = register_notifications(&client, &settings, &store);
                let ((), ()) = tokio::join!(room, notifications);
            }
        })
        .into();

        self.initialized = true;
    }

    async fn login_and_sync(&mut self, style: LoginStyle) -> IambResult<EditInfo> {
        let client = self.client.clone();

        match style {
            LoginStyle::SessionRestore(session) => {
                client.restore_session(session).await.map_err(IambError::from)?;
            },
            LoginStyle::Password(password) => {
                let resp = client
                    .matrix_auth()
                    .login_username(&self.settings.profile.user_id, &password)
                    .initial_device_display_name(initial_devname().as_str())
                    .send()
                    .await
                    .map_err(IambError::from)?;
                let session = MatrixSession::from(&resp);
                self.settings.write_session(session)?;
            },
            LoginStyle::SingleSignOn => {
                let resp = client
                    .matrix_auth()
                    .login_sso(|url| {
                        let opened = format!(
                            "The following URL should have been opened in your browser:\n    {url}"
                        );

                        async move {
                            tokio::task::spawn_blocking(move || open::that(url));
                            println!("\n{opened}\n");
                            Ok(())
                        }
                    })
                    .initial_device_display_name(initial_devname().as_str())
                    .send()
                    .await
                    .map_err(IambError::from)?;

                let session = MatrixSession::from(&resp);
                self.settings.write_session(session)?;
            },
        }

        self.sync_handle = tokio::spawn(async move {
            loop {
                let mut room = RoomEventFilter::default();
                room.lazy_load_options =
                    LazyLoadOptions::Enabled { include_redundant_members: false };

                let mut room_ev = RoomFilter::default();
                room_ev.state = room;

                let mut filter = FilterDefinition::default();
                filter.room = room_ev;

                let settings = SyncSettings::new().filter(filter.into());

                if let Err(err) = client.sync(settings).await {
                    tracing::warn!("sync error: {err}");
                };
            }
        })
        .into();

        Ok(Some(InfoMessage::from("* Successfully logged in!")))
    }

    async fn logout(&mut self, user_id: String) -> IambResult<EditInfo> {
        // Verify that the user is logging out of the correct profile.
        let curr = self.settings.profile.user_id.as_str();

        if user_id != curr {
            let msg = format!("Incorrect user ID (currently logged in as {curr})");
            let err = UIError::Failure(msg);

            return Err(err);
        }

        // Send the logout request.
        if let Err(e) = self.client.matrix_auth().logout().await {
            let msg = format!("Failed to logout: {e}");
            let err = UIError::Failure(msg);

            return Err(err);
        }

        // Remove the session.json file.
        std::fs::remove_file(&self.settings.session_json)?;

        Ok(Some(InfoMessage::from("Successfully logged out")))
    }

    async fn direct_message(&mut self, user: OwnedUserId) -> IambResult<MatrixRoom> {
        for room in self.client.rooms() {
            if !is_direct(&room).await {
                continue;
            }

            if room.get_member(user.as_ref()).await.map_err(IambError::from)?.is_some() {
                return Ok(room);
            }
        }

        let rt = CreateRoomType::Direct(user.clone());
        let flags = CreateRoomFlags::ENCRYPTED;

        create_room(&self.client, None, rt, flags).await.map_err(|e| {
            error!(
                user_id = user.as_str(),
                err = e.to_string(),
                "Failed to create direct message room"
            );

            let msg = format!("Could not open a room with {user}");
            UIError::Failure(msg)
        })
    }

    async fn get_messages(
        &mut self,
        room: MatrixRoom,
        thread: Option<OwnedEventId>,
    ) -> MessagesResult {
        let store = self.store.clone().unwrap();
        Messages::new(&room, thread, store).await
    }

    async fn join_room(&mut self, name: String) -> IambResult<MatrixRoom> {
        let room = if let Ok(alias_id) = OwnedRoomOrAliasId::from_str(name.as_str()) {
            match self.client.join_room_by_id_or_alias(&alias_id, &[]).await {
                Ok(resp) => resp,
                Err(e) => {
                    let msg = e.to_string();
                    let err = UIError::Failure(msg);

                    return Err(err);
                },
            }
        } else if let Ok(user) = OwnedUserId::try_from(name.as_str()) {
            self.direct_message(user).await?
        } else {
            let msg = format!("{:?} is not a valid room or user name", name.as_str());
            let err = UIError::Failure(msg);

            return Err(err);
        };

        // pre-compute name
        if let Err(err) = room.display_name().await {
            tracing::warn!(room_id = ?room.room_id(), "cannot load room name: {err}");
        };

        Ok(room)
    }

    async fn members(&mut self, room_id: OwnedRoomId) -> IambResult<Vec<RoomMember>> {
        if let Some(room) = self.client.get_room(room_id.as_ref()) {
            Ok(room.members(RoomMemberships::ACTIVE).await.map_err(IambError::from)?)
        } else {
            Err(IambError::UnknownRoom(room_id).into())
        }
    }

    async fn space_members(&mut self, space: OwnedRoomId) -> IambResult<Vec<OwnedRoomId>> {
        let mut req = SpaceHierarchyRequest::new(space);
        req.limit = Some(1000u32.into());
        req.max_depth = Some(1u32.into());

        let resp = self.client.send(req).await.map_err(IambError::from)?;

        let rooms = resp.rooms.into_iter().map(|chunk| chunk.summary.room_id).collect();

        Ok(rooms)
    }

    async fn typing_notice(&mut self, room_id: OwnedRoomId) {
        if let Some(room) = self.client.get_room(room_id.as_ref()) {
            if let Err(err) = room.typing_notice(true).await {
                tracing::warn!("cannot send typing notice: {err}");
            };
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
                let info = format!("Sent verification request to {user_id}");

                Ok(Some(InfoMessage::from(info)))
            },
            None => {
                let msg = format!("Could not find identity information for {user_id}");
                let err = UIError::Failure(msg);

                Err(err)
            },
        }
    }
}
