//! # Async Matrix Client Worker
//!
//! The worker thread handles asynchronous work, and can receive messages from the main thread that
//! block on a reply from the async worker.
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{Debug, Formatter};
use std::ops::DerefMut;
use std::str::FromStr;
use std::sync::Arc;
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};
use std::time::{Duration, Instant};
#[cfg(feature = "voip")]
use {
    matrix_sdk::deserialized_responses::EncryptionInfo,
    matrix_sdk::ruma::MilliSecondsSinceUnixEpoch,
    std::time::SystemTime,
};

use futures::{StreamExt, stream::FuturesUnordered};
use gethostname::gethostname;
use matrix_sdk_base::RoomStateFilter;
use ratatui_image::picker::Picker;
use tokio::sync::Semaphore;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel};
use tokio::task::JoinHandle;
use tracing::{error, warn};
use url::Url;

#[cfg(feature = "voip")]
use crate::voip::devices::{self, DeviceKind};
#[cfg(feature = "voip")]
use crate::voip::livekit_session::{FIRST_KEY_INDEX, SessionConfig};
#[cfg(feature = "voip")]
use crate::voip::{
    CallEncryptionKeysEvent,
    CallManager,
    CallNotice,
    CallSession,
    CallStatus,
    IncomingCall,
    KeyInbox,
    OriginalSyncCallEncryptionKeysRoomEvent,
    ReceivedCallKey,
    matrix_rtc,
};
#[cfg(feature = "voip")]
use livekit::PlatformAudio;
#[cfg(feature = "voip")]
use matrix_sdk::EncryptionState;
#[cfg(feature = "voip")]
use matrix_sdk::ruma::events::call::member::CallMemberEventContent;
#[cfg(feature = "voip")]
use matrix_sdk::ruma::events::rtc::decline::OriginalSyncRtcDeclineEvent;
#[cfg(feature = "voip")]
use matrix_sdk::ruma::events::rtc::notification::{
    NotificationType,
    OriginalSyncRtcNotificationEvent,
};

use matrix_sdk::{
    Client,
    ClientBuildError,
    Error as MatrixError,
    RoomDisplayName,
    RoomMemberships,
    authentication::matrix::MatrixSession,
    config::{RequestConfig, SyncSettings},
    encryption::{
        BackupDownloadStrategy,
        EncryptionSettings,
        verification::{SasVerification, Verification},
    },
    event_handler::Ctx,
    reqwest,
    room::{Messages, MessagesOptions, Room as MatrixRoom, RoomMember},
    ruma::{
        EventId,
        OwnedEventId,
        OwnedRoomId,
        OwnedRoomOrAliasId,
        OwnedUserId,
        RoomId,
        api::client::{
            filter::{FilterDefinition, LazyLoadOptions, RoomEventFilter, RoomFilter},
            room::{
                Visibility,
                create_room::v3::{CreationContent, Request as CreateRoomRequest},
            },
            space::get_hierarchy::v1::Request as SpaceHierarchyRequest,
        },
        assign,
        events::{
            AnyMessageLikeEvent,
            AnyMessageLikeEventContent,
            AnySyncStateEvent,
            AnyTimelineEvent,
            InitialStateEvent,
            SyncEphemeralRoomEvent,
            SyncMessageLikeEvent,
            SyncStateEvent,
            key::verification::{
                VerificationMethod,
                done::{OriginalSyncKeyVerificationDoneEvent, ToDeviceKeyVerificationDoneEvent},
                key::{OriginalSyncKeyVerificationKeyEvent, ToDeviceKeyVerificationKeyEvent},
                request::ToDeviceKeyVerificationRequestEvent,
                start::{OriginalSyncKeyVerificationStartEvent, ToDeviceKeyVerificationStartEvent},
            },
            presence::PresenceEvent,
            reaction::ReactionEventContent,
            receipt::{ReceiptEventContent, ReceiptThread, ReceiptType},
            relation::Thread,
            room::{
                MediaSource,
                encryption::RoomEncryptionEventContent,
                member::OriginalSyncRoomMemberEvent,
                message::{MessageType, Relation, RoomMessageEventContent},
                name::RoomNameEventContent,
                redaction::OriginalSyncRoomRedactionEvent,
            },
            sticker::StickerEventContent,
            tag::Tags,
            typing::SyncTypingEvent,
        },
        room::RoomType,
        serde::Raw,
    },
    send_queue::{LocalEcho, LocalEchoContent, RoomSendQueueUpdate, SendQueueUpdate},
};

use modalkit::errors::UIError;
use modalkit::prelude::{EditInfo, InfoMessage};

use crate::base::{EchoLocation, MessageNeed};
use crate::config::{ImagePreviewSize, ProxyUrl};
use crate::message::{Message, MessageEvent, MessageId, MessageKey};
use crate::notifications::register_notifications;
use crate::preview::PreviewKind;
use crate::{
    ApplicationSettings,
    base::{
        AsyncProgramStore,
        ChatStore,
        CreateRoomFlags,
        CreateRoomType,
        IambError,
        IambResult,
        ProgramStore,
        RoomFetchStatus,
        RoomInfo,
        VerifyAction,
    },
};

const DEFAULT_ENCRYPTION_SETTINGS: EncryptionSettings = EncryptionSettings {
    auto_enable_cross_signing: true,
    auto_enable_backups: true,
    backup_download_strategy: BackupDownloadStrategy::AfterDecryptionFailure,
};

const IAMB_DEVICE_NAME: &str = "iamb";
const IAMB_USER_AGENT: &str = "iamb";
const MIN_MSG_LOAD: u32 = 50;

type MessageFetchResult = IambResult<(Option<String>, Vec<(AnyTimelineEvent, Vec<OwnedUserId>)>)>;

fn initial_devname() -> String {
    format!("{} on {}", IAMB_DEVICE_NAME, gethostname().to_string_lossy())
}

pub async fn create_room(
    client: &Client,
    room_alias_name: Option<String>,
    rt: CreateRoomType,
    flags: CreateRoomFlags,
) -> IambResult<OwnedRoomId> {
    let mut creation_content = None;
    let mut initial_state = vec![];

    let visibility = if flags.contains(CreateRoomFlags::PUBLIC) {
        Visibility::Public
    } else {
        Visibility::Private
    };

    match rt {
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
        let ev = InitialStateEvent::with_empty_state_key(
            RoomEncryptionEventContent::with_recommended_defaults(),
        )
        .to_raw_any();
        initial_state.push(ev);
    }

    let request = assign!(CreateRoomRequest::new(), {
        room_alias_name,
        creation_content,
        initial_state,
        visibility,
    });

    let resp = client.create_room(request).await.map_err(IambError::from)?;

    return Ok(resp.room_id().to_owned());
}

async fn update_event_receipts(info: &mut RoomInfo, room: &MatrixRoom, event_id: &EventId) {
    let receipts = match room
        .load_event_receipts(ReceiptType::Read, ReceiptThread::Main, event_id)
        .await
    {
        Ok(receipts) => receipts,
        Err(e) => {
            tracing::warn!(?event_id, "failed to get event receipts: {e}");
            return;
        },
    };

    for (user_id, _) in receipts {
        info.set_receipt(ReceiptThread::Main, user_id, event_id.to_owned());
    }
}

#[derive(Debug)]
enum Plan {
    Messages(OwnedRoomId, Option<String>, Vec<MessageNeed>),
    Members(OwnedRoomId),
}

async fn load_plans(store: &AsyncProgramStore) -> Vec<Plan> {
    let mut locked = store.lock().await;
    let ChatStore { need_load, rooms, .. } = &mut locked.application;
    let mut plan = Vec::with_capacity(need_load.rooms() * 2);

    for (room_id, need) in std::mem::take(need_load).into_iter() {
        if let Some(message_need) = need.messages {
            let info = rooms.get_or_default(room_id.clone());

            if !info.recently_fetched() && !info.fetching {
                info.fetch_last = Instant::now().into();
                info.fetching = true;

                let fetch_id = match &info.fetch_id {
                    RoomFetchStatus::Done => continue,
                    RoomFetchStatus::HaveMore(fetch_id) => Some(fetch_id.clone()),
                    RoomFetchStatus::NotStarted => None,
                };

                plan.push(Plan::Messages(room_id.to_owned(), fetch_id, message_need));
            }
        }
        if need.members {
            plan.push(Plan::Members(room_id.to_owned()));
        }
    }

    return plan;
}

async fn run_plan(client: &Client, store: &AsyncProgramStore, plan: Plan, permits: &Semaphore) {
    let permit = permits.acquire().await;
    match plan {
        Plan::Messages(room_id, fetch_id, message_need) => {
            let limit = MIN_MSG_LOAD;
            let client = client.clone();

            let res = load_older_one(&client, &room_id, fetch_id, limit).await;
            let mut locked = store.lock().await;
            load_insert(room_id, res, locked.deref_mut(), message_need);
        },
        Plan::Members(room_id) => {
            let res = members_load(client, &room_id).await;
            let mut locked = store.lock().await;
            members_insert(room_id, res, locked.deref_mut());
        },
    }
    drop(permit);
}

async fn load_older_one(
    client: &Client,
    room_id: &RoomId,
    fetch_id: Option<String>,
    limit: u32,
) -> MessageFetchResult {
    if let Some(room) = client.get_room(room_id) {
        // Update cached encryption state. This is a noop if the state is already cached.
        let _ = room.request_encryption_state().await;

        let mut opts = match &fetch_id {
            Some(id) => MessagesOptions::backward().from(id.as_str()),
            None => MessagesOptions::backward(),
        };
        opts.limit = limit.into();

        let Messages { end, chunk, .. } = room.messages(opts).await.map_err(IambError::from)?;

        let mut msgs = vec![];

        for ev in chunk.into_iter() {
            let Ok(msg) = ev.into_raw().deserialize() else {
                continue;
            };

            let event_id = msg.event_id();
            let receipts = match room
                .load_event_receipts(ReceiptType::Read, ReceiptThread::Main, event_id)
                .await
            {
                Ok(receipts) => receipts.into_iter().map(|(u, _)| u).collect(),
                Err(e) => {
                    tracing::warn!(?event_id, "failed to get event receipts: {e}");
                    vec![]
                },
            };

            let msg = msg.into_full_event(room_id.to_owned());
            msgs.push((msg, receipts));
        }

        Ok((end, msgs))
    } else {
        Err(IambError::UnknownRoom(room_id.to_owned()).into())
    }
}

fn load_insert(
    room_id: OwnedRoomId,
    res: MessageFetchResult,
    locked: &mut ProgramStore,
    message_needs: Vec<MessageNeed>,
) {
    let ChatStore { presences, rooms, previews, settings, worker, .. } = &mut locked.application;
    let info = rooms.get_or_default(room_id.clone());
    info.fetching = false;

    match res {
        Ok((fetch_id, msgs)) => {
            for (msg, receipts) in msgs.into_iter() {
                let sender = msg.sender().to_owned();
                let _ = presences.get_or_default(sender);

                for user_id in receipts {
                    info.set_receipt(ReceiptThread::Main, user_id, msg.event_id().to_owned());
                }

                match msg {
                    AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomEncrypted(msg)) => {
                        info.insert_encrypted(msg);
                    },
                    AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomMessage(msg)) => {
                        info.insert_with_preview(msg, settings, previews, worker);
                    },
                    AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::Reaction(ev)) => {
                        info.insert_reaction_with_preview(ev, settings, previews, worker);
                    },
                    AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::Sticker(ev)) => {
                        info.insert_sticker_with_preview(ev, settings, previews, worker);
                    },
                    AnyTimelineEvent::MessageLike(_) => {
                        continue;
                    },
                    AnyTimelineEvent::State(msg) => {
                        if settings.tunables.state_event_display {
                            info.insert_any_state(msg.into());
                        }
                    },
                }
            }

            info.fetch_id = fetch_id.map_or(RoomFetchStatus::Done, RoomFetchStatus::HaveMore);

            // check if more are needed
            let needs: Vec<_> = message_needs
                .into_iter()
                .filter(|need| !info.keys.contains_key(&need.event_id) && need.ttl > 0)
                .map(|mut need| {
                    need.ttl -= 1;
                    need
                })
                .collect();
            if !needs.is_empty() {
                locked.application.need_load.need_messages_all(room_id, needs);
            }
        },
        Err(e) => {
            warn!(room_id = room_id.as_str(), err = e.to_string(), "Failed to load older messages");

            // Wait and try again.
            locked.application.need_load.need_messages_all(room_id, message_needs);
        },
    }
}

async fn load_older(client: &Client, store: &AsyncProgramStore) -> usize {
    // This is an arbitrary limit on how much work we do in parallel to avoid
    // spawning too many tasks at startup and overwhelming the client. We
    // should normally only surpass this limit at startup when doing an initial.
    // fetch for each room.
    const LIMIT: usize = 15;

    // Plans are run in parallel. Any room *may* have several plans.
    let plans = load_plans(store).await;
    let permits = Semaphore::new(LIMIT);

    plans
        .into_iter()
        .map(|plan| run_plan(client, store, plan, &permits))
        .collect::<FuturesUnordered<_>>()
        .count()
        .await
}

async fn members_load(client: &Client, room_id: &RoomId) -> IambResult<Vec<RoomMember>> {
    if let Some(room) = client.get_room(room_id) {
        Ok(room
            .members_no_sync(RoomMemberships::all())
            .await
            .map_err(IambError::from)?)
    } else {
        Err(IambError::UnknownRoom(room_id.to_owned()).into())
    }
}

fn members_insert(
    room_id: OwnedRoomId,
    res: IambResult<Vec<RoomMember>>,
    store: &mut ProgramStore,
) {
    if let Ok(members) = res {
        let ChatStore { rooms, .. } = &mut store.application;
        let info = rooms.get_or_default(room_id);

        for member in members {
            let user_id = member.user_id().to_owned();
            let name = member.display_name().map(|s| s.to_owned());
            info.display_names.set(user_id, name);
        }
    }
    // else ???
}

async fn load_older_forever(client: &Client, store: &AsyncProgramStore) {
    // Load any pending older messages or members every 2 seconds.
    let mut interval = tokio::time::interval(Duration::from_secs(2));

    loop {
        interval.tick().await;
        load_older(client, store).await;
    }
}

async fn refresh_rooms(client: &Client, store: &AsyncProgramStore, first_sync: bool) {
    let mut names = vec![];

    let mut spaces = vec![];
    let mut rooms = vec![];
    let mut dms = vec![];

    let iter = client.rooms_filtered(
        RoomStateFilter::JOINED | RoomStateFilter::INVITED | RoomStateFilter::KNOCKED,
    );

    for room in iter {
        let display = if let Some(name) = room.cached_display_name() {
            name
        } else if !first_sync && let Ok(name) = room.display_name().await {
            // If we are not trying to fill out the SyncInfo during startup,
            // then we can take our time here and force room information
            // to be loaded.
            name
        } else {
            RoomDisplayName::Empty
        };

        let name = display.to_string();
        let tags = room.tags().await.unwrap_or_default();

        names.push((room.room_id().to_owned(), name));

        if room.is_direct().await.unwrap_or_default() {
            dms.push(Arc::new((room, tags)));
        } else if room.is_space() {
            spaces.push(Arc::new((room, tags)));
        } else {
            rooms.push(Arc::new((room, tags)));
        }
    }

    let mut locked = store.lock().await;
    locked.application.sync_info.spaces = spaces;
    locked.application.sync_info.rooms = rooms;
    locked.application.sync_info.dms = dms;

    for (room_id, name) in names {
        locked.application.set_room_name(&room_id, &name);
    }
}

async fn refresh_rooms_forever(client: &Client, store: &AsyncProgramStore) {
    let mut interval = tokio::time::interval(Duration::from_secs(5));

    loop {
        refresh_rooms(client, store, false).await;
        interval.tick().await;
    }
}

async fn send_receipts_forever(client: &Client, store: &AsyncProgramStore) {
    use matrix_sdk::ruma::api::client::receipt::create_receipt::v3::ReceiptType;

    let mut interval = tokio::time::interval(Duration::from_secs(2));
    let mut sent: HashMap<OwnedRoomId, HashMap<ReceiptThread, OwnedEventId>> = Default::default();

    loop {
        interval.tick().await;

        let mut locked = store.lock().await;
        let ChatStore { settings, open_notifications, rooms, .. } = &mut locked.application;
        let user_id = &settings.profile.user_id;

        let mut updates = Vec::new();
        for room in client.joined_rooms() {
            let room_id = room.room_id();
            let Some(info) = rooms.get(room_id) else {
                continue;
            };

            let changed = info.receipts(user_id).filter_map(|(thread, new_receipt)| {
                let old_receipt = sent.get(room_id).and_then(|ts| ts.get(thread));
                let changed = Some(new_receipt) != old_receipt;
                if changed {
                    open_notifications.remove(room_id);
                }
                changed.then(|| (room_id.to_owned(), thread.to_owned(), new_receipt.to_owned()))
            });

            updates.extend(changed);
        }

        let receipt_type = if locked.application.settings.tunables.read_receipt_send {
            ReceiptType::Read
        } else {
            ReceiptType::ReadPrivate
        };
        drop(locked);

        for (room_id, thread, new_receipt) in updates {
            let Some(room) = client.get_room(&room_id) else {
                continue;
            };

            if ReceiptThread::Main == thread || ReceiptThread::Unthreaded == thread {
                let _ = room
                    .set_unread_flag(false)
                    .await
                    .inspect_err(|e| tracing::warn!(?room_id, "Failed to clear unread flag: {e}"));
            }

            match room
                .send_single_receipt(receipt_type.clone(), thread.to_owned(), new_receipt.clone())
                .await
            {
                Ok(()) => {
                    sent.entry(room_id).or_default().insert(thread, new_receipt);
                },
                Err(e) => tracing::warn!(?room_id, "Failed to set read receipt: {e}"),
            }
        }
    }
}

fn insert_local_echo(
    own_user_id: OwnedUserId,
    info: &mut RoomInfo,
    echo: LocalEcho,
) -> Result<(), serde_json::Error> {
    let LocalEcho { transaction_id, content } = echo;

    match content {
        LocalEchoContent::Event { serialized_event, send_handle, .. } => {
            let content = serialized_event.deserialize()?;
            let AnyMessageLikeEventContent::RoomMessage(msg) = content else {
                // XXX: Handle other event types
                return Ok(());
            };

            let thread = match msg.relates_to.as_ref() {
                Some(Relation::Replacement(..)) => {
                    // XXX: Show echo on edited message
                    return Ok(());
                },
                Some(Relation::Thread(Thread { event_id, .. })) => Some(event_id.to_owned()),
                _ => None,
            };

            let ts = send_handle.created_at.into();
            let key = MessageKey { ts, id: MessageId::Local(transaction_id.clone()) };
            let msg = MessageEvent::Local(transaction_id.clone(), send_handle, msg.into());
            let msg = Message::new(msg, own_user_id, ts);

            info.echo_keys
                .insert(transaction_id, EchoLocation::Message(thread.clone(), key.clone()));

            let thread = info.get_thread_mut(thread);
            thread.insert(key, msg);
        },
        LocalEchoContent::Redaction { .. } => {
            // Don't show anything locally for the redaction until the server actually does it.
        },
        LocalEchoContent::React { .. } => {
            // XXX: Handle reactions to local echos
        },
    }
    Ok(())
}

/// How long shutdown waits for the call to be left before giving up on it.
///
/// Retracting the membership means a round trip to the homeserver, and the user
/// has already asked to quit; past this point the membership expiry is a good
/// enough backstop.
#[cfg(feature = "voip")]
const HANGUP_ON_EXIT_TIMEOUT: Duration = Duration::from_secs(5);

/// Ask the worker to refresh the active call's membership, forever.
///
/// This only nudges: the worker owns the call session, so it decides whether
/// there is anything to refresh. Going through the task queue rather than
/// touching the session from here keeps the session single-threaded and avoids
/// taking any lock on a timer.
#[cfg(feature = "voip")]
async fn refresh_call_membership_forever(tx: UnboundedSender<WorkerTask>) {
    let mut interval = tokio::time::interval(matrix_rtc::MEMBERSHIP_REFRESH_INTERVAL);

    // The first tick completes immediately, and a refresh the instant we start
    // up would be pointless - nothing has had time to expire.
    interval.tick().await;

    loop {
        interval.tick().await;

        if tx.send(WorkerTask::CallRefresh).is_err() {
            break;
        }
    }
}

async fn subscribe_sendqueue_forever(client: &Client, store: &AsyncProgramStore) {
    let own_user_id = client.user_id().unwrap();
    let mut receiver = client.send_queue().subscribe();

    // load unsent requests
    if let Ok(room_echos) = client.send_queue().local_echoes().await {
        let mut locked = store.lock().await;
        for (room_id, echos) in room_echos {
            let info = locked.application.get_room_info(room_id);
            for echo in echos {
                let _ = insert_local_echo(own_user_id.to_owned(), info, echo);
            }
        }
    }

    while let Ok(SendQueueUpdate { room_id, update }) = receiver.recv().await {
        let mut locked = store.lock().await;
        let info = locked.application.get_room_info(room_id);
        match update {
            RoomSendQueueUpdate::NewLocalEvent(echo) => {
                let _ = insert_local_echo(own_user_id.to_owned(), info, echo);
            },
            RoomSendQueueUpdate::ReplacedLocalEvent { transaction_id, new_content } => {
                let Some(EchoLocation::Message(thread, key)) =
                    info.echo_keys.get(&transaction_id).cloned()
                else {
                    continue;
                };

                let Ok(content) = new_content.deserialize() else {
                    continue;
                };
                let AnyMessageLikeEventContent::RoomMessage(new_content) = content else {
                    // XXX: Handle other event types
                    continue;
                };

                let Some(msg) = info.get_thread_mut(thread).get_mut(&key) else {
                    continue;
                };

                let MessageEvent::Local(_, _, msg) = &mut msg.event else {
                    continue;
                };

                *msg = new_content.into();
            },

            RoomSendQueueUpdate::SendError { .. } => {
                // XXX: Show the error to the user
            },
            RoomSendQueueUpdate::CancelledLocalEvent { transaction_id } => {
                info.echo_keys.remove(&transaction_id);
            },
            RoomSendQueueUpdate::SentEvent { transaction_id, event_id } => {
                if let Some(location) = info.echo_keys.get_mut(&transaction_id) {
                    let location = std::mem::replace(location, EchoLocation::Replaced(event_id));

                    if let EchoLocation::Message(thread, key) = location {
                        info.get_thread_mut(thread).remove(&key);
                    }
                }
            },

            RoomSendQueueUpdate::RetryEvent { .. } | RoomSendQueueUpdate::MediaUpload { .. } => {
                // Ignore these events
            },
        }
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

    client.send_queue().respawn_tasks_for_rooms_with_unsent_requests().await;

    // Populate sync_info with our initial set of rooms/dms/spaces.
    refresh_rooms(client, store, true).await;

    // Insert Need::Messages to fetch accurate recent timestamps in the background.
    let mut locked = store.lock().await;
    let ChatStore { sync_info, need_load, .. } = &mut locked.application;

    for room in sync_info.rooms.iter() {
        let room_id = room.as_ref().0.room_id().to_owned();
        need_load.need_messages(room_id);
    }

    for room in sync_info.dms.iter() {
        let room_id = room.as_ref().0.room_id().to_owned();
        need_load.need_messages(room_id);
    }

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

    /// Wait up to `timeout` for a response, tolerating never getting one.
    ///
    /// For shutdown paths, where the worker having already gone away is a
    /// normal race rather than a bug, and where blocking indefinitely on an
    /// unreachable server would mean the application never exits.
    #[cfg(feature = "voip")]
    fn recv_before(self, timeout: Duration) -> Option<T> {
        self.0.recv_timeout(timeout).ok()
    }
}

impl<T> ClientReply<T> {
    fn send(self, t: T) {
        self.0.send(t).unwrap();
    }
}

/// Decode the base64 key material from an `io.element.call.encryption_keys` event.
///
/// Element Call sends the key unpadded, but padded input is accepted too so that
/// a stricter sender still interoperates.
#[cfg(feature = "voip")]
fn decode_call_key(encoded: &str) -> Option<Vec<u8>> {
    use base64::Engine;
    use base64::engine::general_purpose::{STANDARD, STANDARD_NO_PAD};

    STANDARD_NO_PAD.decode(encoded).or_else(|_| STANDARD.decode(encoded)).ok()
}

/// Render a call key event for the log with the key material taken out.
///
/// The whole point of logging the payload is to see the shape a sender used,
/// and the one field that never helps with that is the only one worth keeping
/// secret. Writing raw call keys into a file on disk would undo the encryption
/// they exist to provide.
///
/// Input that is not JSON at all is reported as such rather than passed
/// through, since that is exactly the case where it cannot be redacted.
#[cfg(feature = "voip")]
pub fn redact_call_key(json: &str) -> String {
    let Ok(mut value) = serde_json::from_str::<serde_json::Value>(json) else {
        return "<unparseable>".to_owned();
    };

    // Both transports, since either shape may be what failed: the to-device
    // event carries one key object, the room event an array of them.
    if let Some(key) = value.pointer_mut("/content/keys/key") {
        *key = serde_json::Value::from("<redacted>");
    }

    if let Some(serde_json::Value::Array(keys)) = value.pointer_mut("/content/keys") {
        for entry in keys {
            if let Some(key) = entry.pointer_mut("/key") {
                *key = serde_json::Value::from("<redacted>");
            }
        }
    }

    value.to_string()
}

/// Generate the E2EE key our media is encrypted with for one call.
///
/// 16 bytes, matching what Element Call generates, so that the key we hand out
/// slots straight into other clients' key rings.
#[cfg(feature = "voip")]
fn generate_call_key() -> Vec<u8> {
    use rand::Rng as _;

    let mut key = vec![0u8; 16];
    rand::rng().fill_bytes(&mut key);

    key
}

/// Whether a call in this room should encrypt its media.
///
/// The room decides, the same way Element Call decides it, because LiveKit only
/// installs a frame decryptor when encryption is on: a peer that disagrees with
/// us is inaudible in both directions, not merely unencrypted.
///
/// `latest_encryption_state` fetches `m.room.encryption` when the state never
/// reached us, which matters because `:call` works from the room list, on rooms
/// we may never have opened. If even that fails we assume encryption: guessing
/// wrong that way costs audio, while guessing wrong the other way puts our
/// media on the wire in the clear.
#[cfg(feature = "voip")]
async fn call_media_encrypted(room: &MatrixRoom) -> bool {
    match room.latest_encryption_state().await {
        Ok(state) => !matches!(state, EncryptionState::NotEncrypted),
        Err(e) => {
            warn!(
                room_id = %room.room_id(),
                "could not read the room's encryption state, assuming encrypted: {e}"
            );

            true
        },
    }
}

/// Wrap a call setup failure as something the UI can show.
#[cfg(feature = "voip")]
fn call_error(e: impl std::fmt::Display) -> UIError<crate::base::IambInfo> {
    IambError::Call(format!("{e:#}")).into()
}

fn oneshot<T>() -> (ClientReply<T>, ClientResponse<T>) {
    let (tx, rx) = sync_channel(1);
    let reply = ClientReply(tx);
    let response = ClientResponse(rx);

    return (reply, response);
}

pub type FetchedRoom = (MatrixRoom, RoomDisplayName, Option<Tags>);

pub enum WorkerTask {
    Init(AsyncProgramStore, ClientReply<()>),
    Login(LoginStyle, ClientReply<IambResult<EditInfo>>),
    Logout(String, ClientReply<IambResult<EditInfo>>),
    GetInviter(MatrixRoom, ClientReply<IambResult<Option<RoomMember>>>),
    GetRoom(OwnedRoomId, ClientReply<IambResult<FetchedRoom>>),
    JoinRoom(String, ClientReply<IambResult<OwnedRoomId>>),
    Members(OwnedRoomId, ClientReply<IambResult<Vec<RoomMember>>>),
    SpaceMembers(OwnedRoomId, ClientReply<IambResult<Vec<OwnedRoomId>>>),
    TypingNotice(OwnedRoomId),
    Verify(VerifyAction, SasVerification, ClientReply<IambResult<EditInfo>>),
    VerifyRequest(OwnedUserId, ClientReply<IambResult<EditInfo>>),
    LoadImage(MediaSource, PreviewKind, ImagePreviewSize, Arc<Picker>, Arc<Semaphore>),

    #[cfg(feature = "voip")]
    CallJoin(OwnedRoomId, ClientReply<IambResult<EditInfo>>),
    #[cfg(feature = "voip")]
    CallHangup(OwnedRoomId, ClientReply<IambResult<EditInfo>>),
    #[cfg(feature = "voip")]
    CallDecline(OwnedRoomId, OwnedEventId, ClientReply<IambResult<EditInfo>>),
    #[cfg(feature = "voip")]
    CallMute(bool),
    #[cfg(feature = "voip")]
    CallNotice(OwnedRoomId, CallNotice),
    #[cfg(feature = "voip")]
    CallRefresh,
    #[cfg(feature = "voip")]
    CallDevices(ClientReply<IambResult<EditInfo>>),
    #[cfg(feature = "voip")]
    CallSetDevice(DeviceKind, String, ClientReply<IambResult<EditInfo>>),
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
            WorkerTask::LoadImage(source, kind, size, _, _) => {
                f.debug_tuple("WorkerTask::RenderImage")
                    .field(source)
                    .field(kind)
                    .field(size)
                    .field(&format_args!("_"))
                    .field(&format_args!("_"))
                    .finish()
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallJoin(room_id, _) => {
                f.debug_tuple("WorkerTask::CallJoin").field(room_id).finish()
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallHangup(room_id, _) => {
                f.debug_tuple("WorkerTask::CallHangup").field(room_id).finish()
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallDecline(room_id, event_id, _) => {
                f.debug_tuple("WorkerTask::CallDecline")
                    .field(room_id)
                    .field(event_id)
                    .finish()
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallMute(muted) => {
                f.debug_tuple("WorkerTask::CallMute").field(muted).finish()
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallNotice(room_id, notice) => {
                f.debug_tuple("WorkerTask::CallNotice")
                    .field(room_id)
                    .field(notice)
                    .finish()
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallRefresh => f.debug_tuple("WorkerTask::CallRefresh").finish(),
            #[cfg(feature = "voip")]
            WorkerTask::CallDevices(_) => f.debug_tuple("WorkerTask::CallDevices").finish(),
            #[cfg(feature = "voip")]
            WorkerTask::CallSetDevice(kind, spec, _) => {
                f.debug_tuple("WorkerTask::CallSetDevice").field(kind).field(spec).finish()
            },
        }
    }
}

/// Build the HTTP client that all of iamb's outbound requests go through.
///
/// Shared rather than inlined because the Matrix client is not the only thing
/// that talks HTTP: the VoIP feature fetches `.well-known` and the LiveKit JWT
/// service directly. Those requests have to honour the same proxy, user agent
/// and timeout, or signalling works while call setup fails for anyone behind a
/// proxy.
/// Build an HTTP client for talking to the LiveKit JWT service.
///
/// This mirrors the proxy and TLS settings used for the homeserver connection,
/// so a user behind a proxy can still reach the SFU's token endpoint.
#[cfg(feature = "voip")]
fn build_http_client(settings: &ApplicationSettings) -> reqwest::Client {
    let req_timeout = Duration::from_secs(settings.tunables.request_timeout);

    let mut builder = reqwest::Client::builder()
        .user_agent(IAMB_USER_AGENT)
        .timeout(req_timeout)
        .danger_accept_invalid_certs(!settings.tunables.ssl_verify);

    let proxy_config = &settings.tunables.proxy;

    match &proxy_config.url {
        ProxyUrl::Disabled => builder = builder.no_proxy(),
        ProxyUrl::Endpoint(url) => {
            if let Ok(mut proxy) = reqwest::Proxy::all(url.clone()) {
                if !proxy_config.headers.is_empty() {
                    proxy = proxy.headers(proxy_config.headers.clone());
                }

                if let Some(auth) = proxy_config.auth.clone() {
                    proxy = proxy.custom_http_auth(auth);
                }

                builder = builder.proxy(proxy);
            }
        },
        ProxyUrl::System => {},
    }

    builder.build().unwrap_or_default()
}

async fn create_client_inner(
    homeserver: &Option<Url>,
    settings: &ApplicationSettings,
) -> Result<Client, ClientBuildError> {
    let req_timeout = Duration::from_secs(settings.tunables.request_timeout);

    // Set up the HTTP client.
    let mut builder = reqwest::Client::builder()
        .user_agent(IAMB_USER_AGENT)
        .timeout(req_timeout)
        .pool_idle_timeout(Duration::from_secs(60))
        .pool_max_idle_per_host(10)
        .tcp_keepalive(Duration::from_secs(10))
        .danger_accept_invalid_certs(!settings.tunables.ssl_verify);

    // Configure the HTTP client to use any provided proxy settings:
    let proxy_config = &settings.tunables.proxy;

    match &proxy_config.url {
        ProxyUrl::Disabled => builder = builder.no_proxy(),
        ProxyUrl::Endpoint(url) => {
            let mut proxy =
                reqwest::Proxy::all(url.clone()).map_err(matrix_sdk::HttpError::Reqwest)?;

            if !proxy_config.headers.is_empty() {
                proxy = proxy.headers(proxy_config.headers.clone());
            }

            if let Some(auth) = proxy_config.auth.clone() {
                proxy = proxy.custom_http_auth(auth);
            }

            builder = builder.proxy(proxy)
        },
        ProxyUrl::System => {
            // `reqwest` will use the *_PROXY environment variables from the
            // system by default (through `hyper_util::client::proxy`), so do
            // nothing and let it just figure things out for us.
        },
    }

    let http = builder.build().map_err(matrix_sdk::HttpError::Reqwest)?;

    let req_config = RequestConfig::new()
        .timeout(req_timeout)
        .max_retry_time(req_timeout)
        .retry_limit(8);

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

    let client = res.expect("Failed to instantiate client");

    client.event_cache().subscribe().expect("Failed to start event cache");

    client
        .media()
        .set_media_retention_policy(settings.tunables.cache_policy)
        .await
        .expect("Failed to set media cache policy");

    client
}

#[derive(Clone)]
pub struct Requester {
    pub client: Client,
    pub tx: UnboundedSender<WorkerTask>,

    /// The call state the worker publishes for the UI to render.
    #[cfg(feature = "voip")]
    pub call_status: CallStatus,
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

    pub fn get_inviter(&self, invite: MatrixRoom) -> IambResult<Option<RoomMember>> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::GetInviter(invite, reply)).unwrap();

        return response.recv();
    }

    pub fn get_room(&self, room_id: OwnedRoomId) -> IambResult<FetchedRoom> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::GetRoom(room_id, reply)).unwrap();

        return response.recv();
    }

    pub fn join_room(&self, name: String) -> IambResult<OwnedRoomId> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::JoinRoom(name, reply)).unwrap();

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
        kind: PreviewKind,
        size: ImagePreviewSize,
        picker: Arc<Picker>,
        permits: Arc<Semaphore>,
    ) {
        self.tx
            .send(WorkerTask::LoadImage(source, kind, size, picker, permits))
            .unwrap();
    }

    /// Join the call in the given room, connecting to the LiveKit focus.
    #[cfg(feature = "voip")]
    pub fn call_join(&self, room_id: OwnedRoomId) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::CallJoin(room_id, reply)).unwrap();

        return response.recv();
    }

    /// Leave the call in the given room and tear down the LiveKit session.
    #[cfg(feature = "voip")]
    pub fn call_hangup(&self, room_id: OwnedRoomId) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::CallHangup(room_id, reply)).unwrap();

        return response.recv();
    }

    /// Leave the active call as part of shutting the application down.
    ///
    /// Unlike [`Requester::call_hangup`], this neither panics nor blocks
    /// forever: at exit the worker may already be gone, and an unreachable
    /// homeserver must not keep the process from quitting. If the retraction
    /// does not get through, the membership expiry cleans up instead.
    #[cfg(feature = "voip")]
    pub fn call_hangup_on_exit(&self, room_id: OwnedRoomId) {
        let (reply, response) = oneshot();

        if self.tx.send(WorkerTask::CallHangup(room_id, reply)).is_err() {
            return;
        }

        if response.recv_before(HANGUP_ON_EXIT_TIMEOUT).is_none() {
            tracing::warn!("timed out leaving the call while shutting down");
        }
    }

    /// Reject an incoming call without joining it.
    ///
    /// `notification` is the `m.rtc.notification` event that rang us, which the
    /// decline is sent as a reply to - so declining is only possible for a call
    /// someone explicitly rang, not for any call happening in the room.
    ///
    /// This does not end the call or leave one (see [`Requester::call_hangup`]
    /// for that); the others carry on without us. Its real purpose is to stop
    /// *our own other devices* ringing, since they watch for our declines.
    #[cfg(feature = "voip")]
    pub fn call_decline(
        &self,
        room_id: OwnedRoomId,
        notification: OwnedEventId,
    ) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx
            .send(WorkerTask::CallDecline(room_id, notification, reply))
            .unwrap();

        return response.recv();
    }

    /// Mute or unmute the local microphone in the active call.
    #[cfg(feature = "voip")]
    pub fn call_mute(&self, muted: bool) {
        self.tx.send(WorkerTask::CallMute(muted)).unwrap();
    }

    /// List the audio devices available for calls.
    #[cfg(feature = "voip")]
    pub fn call_devices(&self) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::CallDevices(reply)).unwrap();

        return response.recv();
    }

    /// Choose the audio device to use for calls.
    #[cfg(feature = "voip")]
    pub fn call_set_device(&self, kind: DeviceKind, spec: String) -> IambResult<EditInfo> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::CallSetDevice(kind, spec, reply)).unwrap();

        return response.recv();
    }
}

pub struct ClientWorker {
    initialized: bool,
    settings: ApplicationSettings,
    client: Client,
    load_handle: Option<JoinHandle<()>>,
    sync_handle: Option<JoinHandle<()>>,

    /// Tracks the active call session, if any.
    #[cfg(feature = "voip")]
    call_manager: CallManager,

    /// Lets the call thread queue work that needs the Matrix client.
    #[cfg(feature = "voip")]
    tx: UnboundedSender<WorkerTask>,

    /// For the call setup requests that do not go through the Matrix client:
    /// `.well-known` discovery and the LiveKit JWT service. Built from the same
    /// settings as the Matrix client's, so both honour the configured proxy.
    #[cfg(feature = "voip")]
    http: reqwest::Client,

    /// The call state published to the UI. The worker is its only writer.
    #[cfg(feature = "voip")]
    call_status: CallStatus,

    /// Take care when locking since worker commands are sent with the lock already hold
    store: Option<AsyncProgramStore>,
}
impl ClientWorker {
    pub async fn spawn(client: Client, settings: ApplicationSettings) -> Requester {
        let (tx, rx) = unbounded_channel();

        #[cfg(feature = "voip")]
        let call_status = CallStatus::default();

        #[cfg(feature = "voip")]
        let http = build_http_client(&settings);

        let mut worker = ClientWorker {
            initialized: false,
            settings,
            client: client.clone(),
            load_handle: None,
            sync_handle: None,
            #[cfg(feature = "voip")]
            call_manager: CallManager::new(),
            #[cfg(feature = "voip")]
            tx: tx.clone(),
            #[cfg(feature = "voip")]
            http,
            #[cfg(feature = "voip")]
            call_status: call_status.clone(),
            store: None,
        };

        tokio::spawn(async move {
            worker.work(rx).await;
        });

        return Requester {
            client,
            tx,
            #[cfg(feature = "voip")]
            call_status,
        };
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
            WorkerTask::GetInviter(invited, reply) => {
                assert!(self.initialized);
                reply.send(self.get_inviter(invited).await);
            },
            WorkerTask::GetRoom(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.get_room(room_id).await);
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
            WorkerTask::LoadImage(source, kind, size, picker, permits) => {
                assert!(self.initialized);
                tokio::spawn(crate::preview::load_image(
                    self.store.clone().unwrap(),
                    self.client.media(),
                    source,
                    kind,
                    picker,
                    permits,
                    size,
                ));
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallJoin(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.call_join(room_id).await);
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallHangup(room_id, reply) => {
                assert!(self.initialized);
                reply.send(self.call_hangup(room_id).await);
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallDecline(room_id, notification, reply) => {
                assert!(self.initialized);
                reply.send(self.call_decline(room_id, notification).await);
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallMute(muted) => {
                assert!(self.initialized);
                self.call_mute(muted).await;
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallNotice(room_id, notice) => {
                assert!(self.initialized);
                self.call_notice(room_id, notice).await;
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallRefresh => {
                assert!(self.initialized);
                self.call_refresh().await;
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallDevices(reply) => {
                assert!(self.initialized);
                reply.send(self.call_devices());
            },
            #[cfg(feature = "voip")]
            WorkerTask::CallSetDevice(kind, spec, reply) => {
                assert!(self.initialized);
                reply.send(self.call_set_device(kind, spec));
            },
        }
    }

    async fn init(&mut self, store: AsyncProgramStore) {
        self.client.add_event_handler_context(store.clone());

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

        let _ =
            self.client
                .add_event_handler(|ev: PresenceEvent, store: Ctx<AsyncProgramStore>| {
                    async move {
                        let mut locked = store.lock().await;
                        locked.application.presences.insert(ev.sender, ev.content.presence);
                    }
                });

        let _ = self.client.add_event_handler(
            |ev: SyncStateEvent<RoomNameEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    if let SyncStateEvent::Original(ev) = ev {
                        let room_id = room.room_id().to_owned();
                        let room_name = Some(ev.content.name);
                        let mut locked = store.lock().await;
                        let info = locked.application.rooms.get_or_default(room_id.clone());
                        info.name = room_name;
                    }
                }
            },
        );

        // Calls already in progress arrive as room state during the first sync.
        // Announcing those would greet every startup with a burst of "call in
        // …" for calls that may have started hours ago, so memberships older
        // than this are tracked but not announced.
        #[cfg(feature = "voip")]
        let startup_ts = MilliSecondsSinceUnixEpoch::from_system_time(SystemTime::now());

        #[cfg(feature = "voip")]
        let _ = self.client.add_event_handler(
            move |ev: SyncStateEvent<CallMemberEventContent>,
                  room: MatrixRoom,
                  client: Client,
                  store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id().to_owned();
                    let sender = ev.sender().to_owned();

                    let ours = client.user_id() == Some(&sender);
                    let fresh = startup_ts.is_some_and(|start| ev.origin_server_ts() >= start);

                    // Handlers run after the sync response has been folded into
                    // the store, so the SDK's MatrixRTC state already accounts
                    // for this event - including empty content, redactions, and
                    // memberships whose expiry has already elapsed.
                    let active = room.has_active_room_call();

                    let mut locked = store.lock().await;
                    let info = locked.application.rooms.get_or_default(room_id.clone());

                    // A call "starts" the moment the room goes from nobody in it
                    // to somebody, which is the only transition worth announcing.
                    let started = active && !info.had_active_call;
                    info.had_active_call = active;

                    // The call this ring belonged to is over, so stop offering
                    // it as answerable and let the next call announce itself.
                    if !active {
                        info.incoming_call = None;
                        info.call_announced = false;
                    }

                    // An `m.rtc.notification` for the same call says everything
                    // Whichever arrives first announces; the other stays quiet.
                    let announce = started && fresh && !ours && !info.call_announced;

                    if announce {
                        info.call_announced = true;
                    }

                    let name = info.name.clone();

                    if announce {
                        let name = name.unwrap_or_else(|| room_id.to_string());

                        crate::notifications::notify_call_started(&name, room_id, &mut locked)
                            .await;
                    }
                }
            },
        );

        // Someone ringing the room (MSC4075). This is the explicit "pick up"
        // signal, as opposed to the `m.call.member` state above, which only says
        // a call exists.
        #[cfg(feature = "voip")]
        let _ = self.client.add_event_handler(
            move |ev: OriginalSyncRtcNotificationEvent,
                  room: MatrixRoom,
                  client: Client,
                  store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id().to_owned();
                    let sender = ev.sender.clone();

                    // Check if we are the sender
                    if client.user_id() == Some(&sender) {
                        return;
                    }

                    // Call experation timer
                    let expires_at = ev.content.expiration_ts(ev.origin_server_ts, None);

                    if expires_at <= MilliSecondsSinceUnixEpoch::now() {
                        return;
                    }

                    // Mentions say who the ring is for, so a call aimed at two
                    // people in a large room does not ring everybody.
                    let mentioned = client.user_id().is_some_and(|us| {
                        crate::voip::ring_is_for_us(ev.content.mentions.as_ref(), us)
                    });

                    if !mentioned {
                        return;
                    }

                    let ring = ev.content.notification_type == NotificationType::Ring;

                    let caller = room
                        .get_member_no_sync(&sender)
                        .await
                        .ok()
                        .flatten()
                        .and_then(|member| member.display_name().map(ToOwned::to_owned))
                        .unwrap_or_else(|| sender.localpart().to_owned());

                    let mut locked = store.lock().await;

                    // If the ring is aimed at us, and we have already joined the
                    // call. Skip a notification
                    let joined = locked
                        .application
                        .call_status
                        .get()
                        .is_some_and(|call| *call.room_id == *room_id);

                    if joined {
                        return;
                    }

                    let info = locked.application.rooms.get_or_default(room_id.clone());

                    info.incoming_call = Some(IncomingCall {
                        notification: ev.event_id.clone(),
                        from: sender,
                        expires_at,
                        ring,
                    });

                    // The `m.call.member` handler may have announced this call
                    // already; one call is worth one notification.
                    if info.call_announced {
                        return;
                    }

                    info.call_announced = true;

                    let name = info.name.clone().unwrap_or_else(|| room_id.to_string());

                    crate::notifications::notify_incoming_call(
                        &name,
                        &caller,
                        room_id,
                        ring,
                        &mut locked,
                    )
                    .await;
                }
            },
        );

        // Someone declining a call they were rung about, including our own other
        // devices - answering or rejecting on one device silences the rest.
        #[cfg(feature = "voip")]
        let _ = self.client.add_event_handler(
            move |ev: OriginalSyncRtcDeclineEvent,
                  room: MatrixRoom,
                  client: Client,
                  store: Ctx<AsyncProgramStore>| {
                async move {
                    //Check if the device user that declined is our own
                    if client.user_id() != Some(&ev.sender) {
                        return;
                    }

                    let room_id = room.room_id().to_owned();
                    let declined = &ev.content.relates_to.event_id;

                    let mut locked = store.lock().await;
                    let info = locked.application.rooms.get_or_default(room_id);

                    if info
                        .incoming_call
                        .as_ref()
                        .is_some_and(|call| call.notification == *declined)
                    {
                        info.incoming_call = None;
                    }
                }
            },
        );

        #[cfg(feature = "voip")]
        {
            self.client.add_event_handler_context(self.call_manager.inbox.clone());

            // Taken raw so that a shape we do not recognise is something we can
            // read rather than something the SDK silently drops. Every client
            // writes this event slightly differently, and a typed handler that
            // fails to parse takes the key down with it: the participant is
            // then inaudible for the whole call, with `MissingKey` in the log
            // and no way to tell which field was to blame.
            let _ = self.client.add_event_handler(
                |ev: Raw<CallEncryptionKeysEvent>,
                 encryption: Option<EncryptionInfo>,
                 inbox: Ctx<KeyInbox>| {
                    async move {
                        let ev = match ev.deserialize() {
                            Ok(ev) => ev,
                            Err(e) => {
                                tracing::warn!(
                                    payload = %redact_call_key(ev.json().get()),
                                    "could not parse an incoming call encryption key: {e}"
                                );

                                return;
                            },
                        };

                        let content = ev.content;

                        // `member.id` is not necessarily a user id. Element X
                        // puts a per-session MatrixRTC member identifier there
                        // - a fresh UUID for every call - so requiring it to
                        // equal the sender rejected every key it ever sent.
                        //
                        // Nothing is lost by not insisting: the field is never
                        // read again. The key is filed under `ev.sender`, which
                        // the homeserver vouches for, so a sender cannot claim
                        // another user's slot no matter what it writes here.
                        if content.member.id != ev.sender.as_str() {
                            tracing::debug!(
                                sender = %ev.sender,
                                member_id = %content.member.id,
                                "call encryption key names its member by something other than the sender"
                            );
                        }

                        // The device named in the content picks which LiveKit
                        // participant the key is filed under, so a wrong one
                        // silences that participant. Olm authenticates the
                        // sending device, so encrypted keys can be checked;
                        // ones sent in the clear cannot, but older Element Call
                        // still sends those, so they are accepted unverified.
                        let sender_device =
                            encryption.as_ref().and_then(|info| info.sender_device.as_ref());

                        // Olm's device is preferred over the claimed one rather
                        // than merely checked against it. Both name the same
                        // thing, but only Olm's is authenticated, and taking it
                        // means a sender that writes something unexpected in
                        // the content still gets its key filed correctly - the
                        // same class of mistake that `member.id` turned out to
                        // be. Unencrypted senders have no Olm device, so the
                        // claim is all there is.
                        let device_id = match sender_device {
                            Some(device) => {
                                if *device != content.member.claimed_device_id {
                                    tracing::warn!(
                                        sender = %ev.sender,
                                        claimed = %content.member.claimed_device_id,
                                        authenticated = %device,
                                        "call encryption key claims a device other than the one that sent it"
                                    );
                                }

                                device.to_owned()
                            },
                            None => content.member.claimed_device_id.clone(),
                        };

                        let Some(material) = decode_call_key(&content.keys.key) else {
                            tracing::warn!(
                                sender = %ev.sender,
                                "ignoring call encryption key with invalid base64"
                            );

                            return;
                        };

                        // Only now, once the key itself is known to be usable,
                        // is the room id worth insisting on. Parsing it earlier
                        // as part of the event is what cost us every key a peer
                        // ever sent: one field ruma dislikes and the whole
                        // event is dropped before any of this runs.
                        let room_id = match OwnedRoomId::try_from(content.room_id.clone()) {
                            Ok(room_id) => room_id,
                            Err(e) => {
                                tracing::warn!(
                                    sender = %ev.sender,
                                    room_id = %content.room_id,
                                    "ignoring call encryption key for an unparseable room: {e}"
                                );

                                return;
                            },
                        };

                        // The counterpart of the room transport's own line
                        // below. Both are needed to tell "their key never
                        // arrived" from "their key arrived and did not work":
                        // a key lost to a broken Olm session is dropped by the
                        // SDK before it ever reaches this handler, and from
                        // here looks exactly like a peer that sent nothing.
                        tracing::debug!(
                            sender = %ev.sender,
                            %device_id,
                            index = content.keys.index,
                            encrypted = encryption.is_some(),
                            "received a call encryption key over the to-device transport"
                        );

                        inbox.push(ReceivedCallKey {
                            room_id,
                            user_id: ev.sender,
                            device_id,
                            index: content.keys.index,
                            key: material,
                        });
                    }
                },
            );

            // The older key transport, still the only one some Element Call
            // builds speak. Nothing here authenticates the device beyond what
            // the room's own encryption provides, which is exactly the trust
            // model the senders using this transport already have.
            let _ = self.client.add_event_handler(
                |ev: OriginalSyncCallEncryptionKeysRoomEvent,
                 room: MatrixRoom,
                 inbox: Ctx<KeyInbox>| {
                    async move {
                        let room_id = room.room_id().to_owned();
                        let sender = ev.sender;
                        let device_id = ev.content.device_id;

                        for key in ev.content.keys {
                            let Some(material) = decode_call_key(&key.key) else {
                                tracing::warn!(
                                    %sender,
                                    "ignoring call encryption key with invalid base64"
                                );

                                continue;
                            };

                            tracing::debug!(
                                %sender,
                                %device_id,
                                index = key.index,
                                "received a call encryption key over the room transport"
                            );

                            inbox.push(ReceivedCallKey {
                                room_id: room_id.clone(),
                                user_id: sender.clone(),
                                device_id: device_id.clone(),
                                index: key.index,
                                key: material,
                            });
                        }
                    }
                },
            );
        }

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<RoomMessageEventContent>,
             room: MatrixRoom,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    if let Some(msg) = ev.as_original() &&
                        let MessageType::VerificationRequest(_) = msg.content.msgtype &&
                        let Some(request) = client
                            .encryption()
                            .get_verification_request(ev.sender(), ev.event_id())
                            .await
                    {
                        request.accept().await.expect("Failed to accept request");
                    }

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let ChatStore { rooms, previews, settings, worker, .. } =
                        &mut locked.application;
                    let info = rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    info.insert_with_preview(full_ev, settings, previews, worker);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<ReactionEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let ChatStore { rooms, previews, settings, worker, .. } =
                        &mut locked.application;
                    let info = rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    info.insert_reaction_with_preview(
                        ev.into_full_event(room_id.to_owned()),
                        settings,
                        previews,
                        worker,
                    );
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<StickerEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let ChatStore { rooms, settings, previews, worker, .. } =
                        &mut locked.application;

                    let info = rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    info.insert_sticker_with_preview(full_ev, settings, previews, worker);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncEphemeralRoomEvent<ReceiptEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let info = locked.application.get_room_info(room_id.to_owned());
                    for (event_id, receipts) in ev.content.0.into_iter() {
                        let Some(receipts) = receipts.get(&ReceiptType::Read) else {
                            continue;
                        };
                        for (user_id, rcpt) in receipts.iter() {
                            info.set_receipt(
                                rcpt.thread.clone(),
                                user_id.to_owned(),
                                event_id.clone(),
                            );
                        }
                    }
                }
            },
        );

        if self.settings.tunables.state_event_display {
            let _ = self.client.add_event_handler(
                |ev: AnySyncStateEvent, room: MatrixRoom, store: Ctx<AsyncProgramStore>| {
                    async move {
                        let room_id = room.room_id();
                        let mut locked = store.lock().await;

                        let info = locked.application.get_room_info(room_id.to_owned());
                        info.insert_any_state(ev);
                    }
                },
            );
        }

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncRoomRedactionEvent,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;
                    let info = locked.application.get_room_info(room_id.to_owned());
                    info.redact(ev);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncRoomMemberEvent, room: MatrixRoom, store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();
                    let user_id = ev.state_key;

                    let mut locked = store.lock().await;
                    let info = locked.application.get_room_info(room_id.to_owned());
                    info.display_names.set(user_id, ev.content.displayname);
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
            #[cfg(feature = "voip")]
            let tx = self.tx.clone();

            async move {
                while !client.is_active() {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                }

                let load = load_older_forever(&client, &store);
                let rcpt = send_receipts_forever(&client, &store);
                let room = refresh_rooms_forever(&client, &store);
                let notifications = register_notifications(&client, &settings, &store);
                let sendqueue = subscribe_sendqueue_forever(&client, &store);

                #[cfg(feature = "voip")]
                let call = refresh_call_membership_forever(tx);
                #[cfg(not(feature = "voip"))]
                let call = std::future::pending::<()>();

                let ((), (), (), (), (), ()) =
                    tokio::join!(load, rcpt, room, notifications, sendqueue, call);
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
                let settings = SyncSettings::default();

                let _ = client.sync(settings).await;
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

    async fn direct_message(&mut self, user: OwnedUserId) -> IambResult<OwnedRoomId> {
        if let Some(room) = self.client.get_dm_room(&user) {
            return Ok(room.room_id().to_owned());
        }

        self.client
            .create_dm(&user)
            .await
            .map(|room| room.room_id().to_owned())
            .map_err(|err| {
                error!(
                    user_id = user.as_str(),
                    err = err.to_string(),
                    "Failed to create direct message room"
                );

                let msg = format!("Could not open a room with {user}");
                UIError::Failure(msg)
            })
    }

    async fn get_inviter(&mut self, invited: MatrixRoom) -> IambResult<Option<RoomMember>> {
        let details = invited.invite_details().await.map_err(IambError::from)?;

        Ok(details.inviter)
    }

    async fn get_room(&mut self, room_id: OwnedRoomId) -> IambResult<FetchedRoom> {
        if let Some(room) = self.client.get_room(&room_id) {
            let name = room.cached_display_name().ok_or_else(|| IambError::UnknownRoom(room_id))?;
            let tags = room.tags().await.map_err(IambError::from)?;

            Ok((room, name, tags))
        } else {
            Err(IambError::UnknownRoom(room_id).into())
        }
    }

    async fn join_room(&mut self, name: String) -> IambResult<OwnedRoomId> {
        if let Ok(alias_id) = OwnedRoomOrAliasId::from_str(name.as_str()) {
            match self.client.join_room_by_id_or_alias(&alias_id, &[]).await {
                Ok(resp) => Ok(resp.room_id().to_owned()),
                Err(e) => {
                    let msg = e.to_string();
                    let err = UIError::Failure(msg);

                    return Err(err);
                },
            }
        } else if let Ok(user) = OwnedUserId::try_from(name.as_str()) {
            self.direct_message(user).await
        } else {
            let msg = format!("{:?} is not a valid room or user name", name.as_str());
            let err = UIError::Failure(msg);

            return Err(err);
        }
    }

    async fn members(&mut self, room_id: OwnedRoomId) -> IambResult<Vec<RoomMember>> {
        if let Some(room) = self.client.get_room(room_id.as_ref()) {
            Ok(room
                .members(RoomMemberships::ACTIVE | RoomMemberships::KNOCK)
                .await
                .map_err(IambError::from)?)
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

    /// Join the call in `room_id`: discover the LiveKit focus, obtain a token,
    /// connect, and publish our `m.call.member` state event.
    #[cfg(feature = "voip")]
    async fn call_join(&mut self, room_id: OwnedRoomId) -> IambResult<EditInfo> {
        if self.call_manager.is_active() {
            return Ok(Some(InfoMessage::from("Already in a call")));
        }

        let Some(room) = self.client.get_room(&room_id) else {
            return Err(IambError::UnknownRoom(room_id).into());
        };

        let encrypted = call_media_encrypted(&room).await;

        if !encrypted {
            tracing::warn!(%room_id, "the room is unencrypted, so our media is too");
        }

        let user_id = self
            .client
            .user_id()
            .ok_or_else(|| call_error("not logged in"))?
            .to_owned();
        let device_id = self
            .client
            .device_id()
            .ok_or_else(|| call_error("no device id"))?
            .to_owned();

        let focus = matrix_rtc::discover_focus(&self.client, &self.http, &room)
            .await
            .map_err(call_error)?;
        let credentials =
            matrix_rtc::request_sfu_credentials(&self.client, &self.http, &focus, &device_id)
                .await
                .map_err(call_error)?;

        let audio = PlatformAudio::new().map_err(call_error)?;

        // Playback is LiveKit's, inside libwebrtc, and reports nothing back:
        // there is no way to ask whether playout ever started. The device
        // counts are the one thing we can see, and an output count of zero
        // turns "the call is silent" from a mystery into a fact.
        tracing::info!(
            microphones = audio.recording_devices().count(),
            speakers = audio.playout_devices().count(),
            "opened the audio device module for a call"
        );

        devices::apply(&audio, &self.settings.read_voip_devices());

        let key = generate_call_key();
        let config = SessionConfig {
            audio,
            url: credentials.url,
            token: credentials.jwt,
            e2ee_key: key.clone(),
            identity: matrix_rtc::participant_identity(&user_id, &device_id),
            inbox: self.call_manager.inbox.clone(),
            room_id: room_id.clone(),
            encrypted,
        };

        // If we start the call we claim ownership
        let starting = !matrix_rtc::call_already_started(&room);

        // Announce ourselves before connecting so that anyone already in the
        // call sees us arrive and starts sending us their keys.
        //
        // `None` leaves `created_ts` for the server to stamp, which is what
        // MSC3401 asks of an initial join; refreshes read it back.
        let membership = matrix_rtc::publish_membership(&room, &user_id, &device_id, &focus, None)
            .await
            .map_err(call_error)?;

        let session = match CallSession::new(config, room_id.clone(), focus, self.tx.clone()) {
            Ok(session) => session,
            Err(e) => {
                // We already told the room we were joining, so take that back
                // rather than leaving a membership for a call we are not in.
                if let Err(e) = self.retract_our_membership(&room_id).await {
                    tracing::warn!("could not retract our call membership: {e:#}");
                }

                return Err(call_error(format!("could not start the call thread: {e}")));
            },
        };

        self.call_manager.session = Some(session);
        self.call_status.joined(room_id.clone());

        // Ring the room, if we are the one starting the call.
        if starting {
            let ring = matrix_rtc::should_ring(&room).await;

            if let Err(e) = matrix_rtc::send_call_notification(&room, membership, ring).await {
                tracing::warn!("could not announce the call to the room: {e:#}");
            }
        }

        // We are in the call from here on, so key distribution failing is worth
        // a warning but must not report the join as failed which would leave
        // the worker in a call the UI does not know about.
        match matrix_rtc::key_recipients(&room).await {
            Err(e) => tracing::warn!("could not list call key recipients: {e:#}"),
            Ok(recipients) => {
                if let Err(e) = self
                    .share_call_key(&room, &device_id, FIRST_KEY_INDEX, &key, recipients)
                    .await
                {
                    tracing::warn!("could not share our call encryption key: {e:#}");
                }
            },
        }

        Ok(Some(InfoMessage::from("Joined the call")))
    }

    /// Leave the call in `room_id`, tearing down the LiveKit session and
    /// retracting our `m.call.member` state event.
    #[cfg(feature = "voip")]
    async fn call_hangup(&mut self, room_id: OwnedRoomId) -> IambResult<EditInfo> {
        if !self.call_manager.is_active() {
            self.call_status.left();

            return Ok(Some(InfoMessage::from("Not in a call")));
        }

        // Dropping the session shuts the call thread down and leaves the SFU.
        //
        // Clearing `call_status` is also what drops us from the participant
        // list right away: the state event we are about to retract only stops
        // counting once the homeserver echoes it back, and until then
        // `call_participants` uses this to overrule it.
        self.call_manager.session = None;
        self.call_status.left();

        match self.retract_our_membership(&room_id).await {
            Ok(()) => Ok(Some(InfoMessage::from("Left the call"))),
            Err(e) => {
                tracing::warn!("could not retract our call membership: {e:#}");

                Ok(Some(InfoMessage::from("Left the call (membership not retracted)")))
            },
        }
    }

    /// Decline a call we were rung about, without joining it.
    /// Also tells other devices we declined
    #[cfg(feature = "voip")]
    async fn call_decline(
        &mut self,
        room_id: OwnedRoomId,
        notification: OwnedEventId,
    ) -> IambResult<EditInfo> {
        let Some(room) = self.client.get_room(&room_id) else {
            return Err(IambError::UnknownRoom(room_id).into());
        };

        matrix_rtc::send_decline(&room, &notification).await.map_err(call_error)?;

        Ok(Some(InfoMessage::from("Declined the call")))
    }

    /// Push the expiry of our `m.call.member` membership further out.
    ///
    /// A membership is only valid for [`matrix_rtc::MEMBERSHIP_LIFETIME`] past
    /// its creation, so a call that outlives that would silently vanish from
    /// every other participant's view while still running. Re-publishing on a
    /// timer keeps it alive; the same expiry is what eventually clears our
    /// membership if we die without retracting it.
    #[cfg(feature = "voip")]
    async fn call_refresh(&mut self) {
        let Some(session) = &self.call_manager.session else {
            return;
        };

        let (Some(room), Some(device_id), Some(user_id)) = (
            self.client.get_room(&session.room_id),
            self.client.device_id().map(|id| id.to_owned()),
            self.client.user_id().map(|id| id.to_owned()),
        ) else {
            return;
        };

        // The server stamped `created_ts` when we joined, so read it back rather
        // than trusting our own clock for when the session began. `None` here
        // means our membership has not synced back, in which case starting a
        // fresh chain is safer than guessing at a start time.
        let created_ts = matrix_rtc::our_membership_created_ts(&room, &user_id, &device_id).await;

        // If failed to refresh, the call doesn't drop. The call can be refreshed later
        if let Err(e) =
            matrix_rtc::publish_membership(&room, &user_id, &device_id, &session.focus, created_ts)
                .await
        {
            tracing::warn!("could not refresh our call membership: {e:#}");
        }
    }

    /// Announce that this device has left the call in `room_id`.
    #[cfg(feature = "voip")]
    async fn retract_our_membership(&self, room_id: &OwnedRoomId) -> anyhow::Result<()> {
        let room = self
            .client
            .get_room(room_id)
            .ok_or_else(|| anyhow::anyhow!("unknown room {room_id}"))?;
        let user_id = self
            .client
            .user_id()
            .ok_or_else(|| anyhow::anyhow!("not logged in"))?
            .to_owned();
        let device_id = self
            .client
            .device_id()
            .ok_or_else(|| anyhow::anyhow!("no device id"))?
            .to_owned();

        matrix_rtc::retract_membership(&room, &user_id, &device_id).await
    }

    /// Mute or unmute the local microphone in the active call.
    #[cfg(feature = "voip")]
    async fn call_mute(&mut self, muted: bool) {
        if let Some(session) = &mut self.call_manager.session {
            session.set_muted(muted);
            self.call_status.set_muted(muted);
        }
    }

    /// A handle on the system audio devices.
    ///
    /// During a call this is the session's own handle, so changes take effect on
    /// the live call. Outside a call it is a temporary one, which acquires the
    /// audio device module just long enough to answer and releases it on drop.
    #[cfg(feature = "voip")]
    fn audio_handle(&self) -> IambResult<PlatformAudio> {
        if let Some(session) = &self.call_manager.session {
            return Ok(session.audio.clone());
        }

        PlatformAudio::new().map_err(call_error)
    }

    /// Show the audio devices available for calls.
    #[cfg(feature = "voip")]
    fn call_devices(&self) -> IambResult<EditInfo> {
        let audio = self.audio_handle()?;
        let prefs = self.settings.read_voip_devices();

        Ok(Some(InfoMessage::Pager(devices::format_listing(&audio, &prefs))))
    }

    /// Choose an audio device, applying it now and remembering it for later.
    #[cfg(feature = "voip")]
    fn call_set_device(&self, kind: DeviceKind, spec: String) -> IambResult<EditInfo> {
        let audio = self.audio_handle()?;
        let name = devices::select(&audio, kind, &spec).map_err(call_error)?;

        let mut prefs = self.settings.read_voip_devices();
        prefs.set(kind, name.clone());
        self.settings.write_voip_devices(&prefs).map_err(call_error)?;

        Ok(Some(InfoMessage::from(format!("Using {name:?} as the {}", kind.keyword()))))
    }

    /// Act on something the call thread asked us to do.
    #[cfg(feature = "voip")]
    async fn call_notice(&mut self, room_id: OwnedRoomId, notice: CallNotice) {
        let Some(session) = &self.call_manager.session else {
            return;
        };

        if session.room_id != room_id {
            return;
        }

        match notice {
            CallNotice::Connected => {
                self.call_status.connected();
            },
            CallNotice::Speakers(speakers) => {
                self.call_status.set_speakers(speakers);
            },
            CallNotice::ShareKey(user_id) => {
                let (Some(room), Some(device_id)) =
                    (self.client.get_room(&room_id), self.client.device_id())
                else {
                    return;
                };

                // Whatever key we are on *now* a late joiner arriving after a
                // rotation must be told the current one, not the original.
                let key = session.key.clone();
                let index = session.key_index;
                let device_id = device_id.to_owned();

                if let Err(e) =
                    self.share_call_key(&room, &device_id, index, &key, vec![user_id]).await
                {
                    tracing::warn!("could not share our call encryption key: {e:#}");
                }
            },
            CallNotice::RotateKey => {
                self.rotate_call_key(&room_id).await;
            },
            CallNotice::Ended(reason) => {
                tracing::warn!("call ended: {reason}");
                self.call_manager.session = None;

                if let Err(e) = self.retract_our_membership(&room_id).await {
                    tracing::warn!("could not retract our call membership: {e:#}");
                }

                self.call_status.left();
            },
        }
    }

    /// Distribute our call E2EE key over both MatrixRTC key transports.
    ///
    /// To-device is the primary one and the only one that encrypts the key to
    /// the participants rather than to the room, so it is the one whose failure
    /// is reported back. The room event exists for Element Call builds from
    /// before the to-device transport landed, which listen for nothing else;
    /// against those, sending only to-device produces a call where both sides
    /// join, see each other, and hear nothing.
    #[cfg(feature = "voip")]
    async fn share_call_key(
        &self,
        room: &MatrixRoom,
        device_id: &matrix_sdk::ruma::DeviceId,
        index: u8,
        key: &[u8],
        recipients: Vec<OwnedUserId>,
    ) -> anyhow::Result<()> {
        let sent =
            matrix_rtc::send_encryption_key(&self.client, room, device_id, index, key, recipients)
                .await;

        if let Err(e) = matrix_rtc::send_encryption_key_to_room(room, device_id, index, key).await {
            tracing::warn!("could not send our call encryption key as a room event: {e:#}");
        }

        sent
    }

    /// Re-key the call in `room_id` after someone left it.
    /// Avoids people being able to read call data from outside the call
    #[cfg(feature = "voip")]
    async fn rotate_call_key(&mut self, room_id: &OwnedRoomId) {
        let (Some(room), Some(device_id)) =
            (self.client.get_room(room_id), self.client.device_id().map(|id| id.to_owned()))
        else {
            return;
        };

        let Some(session) = &self.call_manager.session else {
            return;
        };

        let key = generate_call_key();
        let index = session.next_key_index();

        let recipients = match matrix_rtc::key_recipients(&room).await {
            Ok(recipients) => recipients,
            Err(e) => {
                tracing::warn!("could not list call key recipients for rotation: {e:#}");
                return;
            },
        };

        if let Err(e) = self.share_call_key(&room, &device_id, index, &key, recipients).await {
            // Switching anyway would encrypt under a key nobody has, silently
            // cutting our audio for the rest of the call. Staying on the old key
            // only means the departed participant can still follow along.
            tracing::warn!(
                "could not distribute the rotated call key, staying on the old one: {e:#}"
            );
            return;
        }

        if let Some(session) = &mut self.call_manager.session {
            session.adopt_key(index, key);
            tracing::info!(index, "rotated the call encryption key");
        }
    }
}
