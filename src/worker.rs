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

use futures::future::join_all;
use futures::{StreamExt, stream::FuturesUnordered};
use gethostname::gethostname;
use matrix_sdk::deserialized_responses::{TimelineEvent, TimelineEventKind};
use matrix_sdk_base::RoomStateFilter;
use ratatui_image::picker::Picker;
use tokio::sync::Semaphore;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel};
use tokio::task::JoinHandle;
use tracing::{debug, error, warn};
use url::Url;

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
    Events(OwnedRoomId, Vec<OwnedEventId>),
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
        if !need.events.is_empty() {
            plan.push(Plan::Events(room_id, need.events));
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
        Plan::Events(room_id, events) => {
            let res = events_load(client, &room_id, events).await;
            let mut locked = store.lock().await;
            events_insert(room_id, res, locked.deref_mut());
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
    let ChatStore {
        presences,
        rooms,
        worker,
        previews,
        settings,
        need_load,
        ..
    } = &mut locked.application;
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
                        info.insert_with_preview(msg, settings, previews, worker, need_load);
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

async fn events_load(
    client: &Client,
    room_id: &RoomId,
    events: Vec<OwnedEventId>,
) -> IambResult<Vec<TimelineEvent>> {
    if let Some(room) = client.get_room(room_id) {
        let res = join_all(
            events
                .into_iter()
                .map(async |event_id| room.load_or_fetch_event(&event_id, None).await),
        )
        .await;

        Ok(res.into_iter().filter_map(Result::ok).collect())
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

fn events_insert(
    room_id: OwnedRoomId,
    res: IambResult<Vec<TimelineEvent>>,
    locked: &mut ProgramStore,
) {
    if let Ok(events) = res {
        let ChatStore { rooms, worker, settings, previews, need_load, .. } =
            &mut locked.application;
        let info = rooms.get_or_default(room_id.clone());

        for event in events {
            let event = match event.kind {
                TimelineEventKind::Decrypted(event) => {
                    match event.event.deserialize() {
                        Ok(event) => event,
                        Err(err) => {
                            warn!(
                                err = %err,
                                room_id = room_id.as_str(),
                                raw_event = ?event,
                                "Failed to deserialize event"
                            );
                            continue;
                        },
                    }
                },
                TimelineEventKind::UnableToDecrypt { event, utd_info: _ } |
                TimelineEventKind::PlainText { event } => {
                    let event = match event.deserialize() {
                        Ok(event) => event,
                        Err(err) => {
                            warn!(
                                err = %err,
                                room_id = room_id.as_str(),
                                raw_event = ?event,
                                "Failed to deserialize event"
                            );
                            continue;
                        },
                    };
                    event.into_full_event(room_id.clone())
                },
            };
            match event {
                AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomEncrypted(msg)) => {
                    info.insert_encrypted(msg);
                },
                AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomMessage(msg)) => {
                    info.insert_with_preview(msg, settings, previews, worker, need_load);
                },
                AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::Reaction(ev)) => {
                    info.insert_reaction_with_preview(ev, settings, previews, worker);
                },
                AnyTimelineEvent::MessageLike(ev) => {
                    debug!("Ignoring unimplemented event type {}", ev.event_type());
                    continue;
                },
                AnyTimelineEvent::State(msg) => {
                    if settings.tunables.state_event_display {
                        info.insert_any_state(msg.into());
                    }
                },
            }
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
        }
    }
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

                    let ChatStore { rooms, previews, settings, worker, need_load, .. } =
                        &mut locked.application;
                    let info = rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    info.insert_with_preview(full_ev, settings, previews, worker, need_load);
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

            async move {
                while !client.is_active() {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                }

                let load = load_older_forever(&client, &store);
                let rcpt = send_receipts_forever(&client, &store);
                let room = refresh_rooms_forever(&client, &store);
                let notifications = register_notifications(&client, &settings, &store);
                let sendqueue = subscribe_sendqueue_forever(&client, &store);
                let ((), (), (), (), ()) = tokio::join!(load, rcpt, room, notifications, sendqueue);
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
}
