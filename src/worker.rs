//! # Async Matrix Client Worker
//!
//! The worker thread handles asynchronous work, and can receive messages from the main thread that
//! block on a reply from the async worker.

use std::fmt::{Debug, Formatter};
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};

use futures::StreamExt;
use futures::stream::FuturesUnordered;
use gethostname::gethostname;
use matrix_sdk::OwnedServerName;
use matrix_sdk::authentication::matrix::MatrixSession;
use matrix_sdk::config::{RequestConfig, SyncSettings};
use matrix_sdk::deserialized_responses::{TimelineEvent, TimelineEventKind};
use matrix_sdk::encryption::{BackupDownloadStrategy, EncryptionSettings};
use matrix_sdk::event_handler::Ctx;
use matrix_sdk::room::RoomMember;
use matrix_sdk::ruma::OwnedRoomAliasId;
use matrix_sdk::ruma::api::client::filter::{
    FilterDefinition,
    LazyLoadOptions,
    RoomEventFilter,
    RoomFilter,
};
use matrix_sdk::ruma::api::client::receipt::create_receipt::v3::ReceiptType as CreateReceiptType;
use matrix_sdk::ruma::api::client::room::Visibility;
use matrix_sdk::ruma::api::client::room::create_room::v3::{
    CreationContent,
    Request as CreateRoomRequest,
};
use matrix_sdk::ruma::api::client::space::get_hierarchy::v1::Request as SpaceHierarchyRequest;
use matrix_sdk::ruma::assign;
use matrix_sdk::ruma::events::MessageLikeEvent;
use matrix_sdk::ruma::events::key::verification::ready::{
    OriginalSyncKeyVerificationReadyEvent,
    ToDeviceKeyVerificationReadyEvent,
};
use matrix_sdk::ruma::events::key::verification::request::ToDeviceKeyVerificationRequestEvent;
use matrix_sdk::ruma::events::key::verification::start::{
    OriginalSyncKeyVerificationStartEvent,
    ToDeviceKeyVerificationStartEvent,
};
use matrix_sdk::ruma::events::poll::end::PollEndEventContent;
use matrix_sdk::ruma::events::poll::response::PollResponseEventContent;
use matrix_sdk::ruma::events::poll::start::PollStartEventContent;
use matrix_sdk::ruma::events::poll::unstable_end::UnstablePollEndEventContent;
use matrix_sdk::ruma::events::poll::unstable_response::UnstablePollResponseEventContent;
use matrix_sdk::ruma::events::poll::unstable_start::UnstablePollStartEventContent;
use matrix_sdk::ruma::events::presence::PresenceEvent;
use matrix_sdk::ruma::events::reaction::ReactionEventContent;
use matrix_sdk::ruma::events::receipt::{ReceiptEventContent, ReceiptType};
use matrix_sdk::ruma::events::room::encryption::RoomEncryptionEventContent;
use matrix_sdk::ruma::events::room::member::{MembershipState, OriginalSyncRoomMemberEvent};
use matrix_sdk::ruma::events::room::name::RoomNameEventContent;
use matrix_sdk::ruma::events::room::pinned_events::SyncRoomPinnedEventsEvent;
use matrix_sdk::ruma::events::room::redaction::OriginalSyncRoomRedactionEvent;
use matrix_sdk::ruma::events::sticker::StickerEventContent;
use matrix_sdk::ruma::events::typing::SyncTypingEvent;
use matrix_sdk::ruma::events::{
    AnyMessageLikeEvent,
    AnyMessageLikeEventContent,
    AnyTimelineEvent,
    InitialStateEvent,
    SyncEphemeralRoomEvent,
    SyncMessageLikeEvent,
    SyncStateEvent,
};
use matrix_sdk::ruma::presence::PresenceState;
use matrix_sdk::ruma::room::RoomType;
use matrix_sdk::ruma::serde::Raw;
use matrix_sdk::send_queue::{LocalEcho, LocalEchoContent, RoomSendQueueUpdate, SendQueueUpdate};
use matrix_sdk::{
    ClientBuildError,
    Error as MatrixError,
    RoomDisplayName,
    RoomMemberships,
    reqwest,
};
use matrix_sdk_base::RoomStateFilter;
use modalkit::editing::completion::CompletionMap;
use ratatui_image::picker::Picker;
use tokio::sync::Semaphore;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel};
use tokio::task::JoinHandle;
use tokio_stream::wrappers::UnboundedReceiverStream;
use tracing::{Instrument as _, error, warn};

use crate::base::{CreateRoomFlags, CreateRoomType, EchoLocation, MessageNeed};
use crate::config::ProxyUrl;
use crate::message::MessageId;
use crate::notifications::register_notifications;
use crate::prelude::*;
use crate::preview::{PreviewKind, PreviewManager};
use crate::verifications;

const DEFAULT_ENCRYPTION_SETTINGS: EncryptionSettings = EncryptionSettings {
    auto_enable_cross_signing: true,
    auto_enable_backups: true,
    backup_download_strategy: BackupDownloadStrategy::AfterDecryptionFailure,
};

const IAMB_DEVICE_NAME: &str = "iamb";
const IAMB_USER_AGENT: &str = "iamb";
const MIN_MSG_LOAD: u16 = 50;

/// How long to wait before retrying receipts that we failed to send.
const RECEIPT_RETRY_INTERVAL: Duration = Duration::from_secs(2);

type ReceiptKey = (OwnedRoomId, ReceiptThread, ReceiptType);
type ReceiptUpdate = (OwnedRoomId, ReceiptThread, ReceiptType, OwnedEventId);

type MessageFetchResult = IambResult<(bool, Vec<(AnyTimelineEvent, Vec<OwnedUserId>)>)>;

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
        .load_event_receipts(ReceiptType::Read, &ReceiptThread::Main, event_id)
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
    Messages(OwnedRoomId, Vec<MessageNeed>),
    Members(OwnedRoomId),
    Pinned(OwnedRoomId, Vec<OwnedEventId>),
    RoomPreview(OwnedRoomOrAliasId),
}

async fn load_plans(store: &AsyncProgramStore) -> Vec<Plan> {
    let mut locked = store.lock().await;
    let ChatStore { need_load, rooms, .. } = &mut locked.application;
    let mut plan = Vec::with_capacity(need_load.rooms() * 2);

    for room in need_load.preview_needs() {
        plan.push(Plan::RoomPreview(room));
    }

    for (room_id, need) in std::mem::take(need_load).into_iter() {
        if need.pinned {
            let missing = rooms.get_or_default(room_id.clone()).missing_pinned();

            if !missing.is_empty() {
                plan.push(Plan::Pinned(room_id.to_owned(), missing));
            }
        }
        if let Some(message_need) = need.messages {
            let info = rooms.get_or_default(room_id.clone());

            if !info.recently_fetched() && !info.fetching {
                info.fetch_last = Instant::now().into();
                info.fetching = true;

                if info.reached_timeline_start {
                    continue;
                }

                plan.push(Plan::Messages(room_id.to_owned(), message_need));
            } else {
                need_load.need_messages_all(room_id.clone(), message_need);
            }
        }
        if need.members {
            plan.push(Plan::Members(room_id.to_owned()));
        }
    }

    return plan;
}

async fn run_plan(client: &Client, store: &AsyncProgramStore, plan: Plan) {
    match plan {
        Plan::Messages(room_id, message_need) => {
            let Some(room) = client.get_room(&room_id) else {
                warn!(room_id = room_id.as_str(), "Room not found in cache");
                store
                    .lock()
                    .await
                    .application
                    .need_load
                    .need_messages_all(room_id, message_need);
                return;
            };

            let res = load_older_one(&room).await;
            let mut locked = store.lock().await;
            load_insert(room_id, res, locked.deref_mut(), message_need);
        },
        Plan::Members(room_id) => {
            let res = members_load(client, &room_id).await;
            let mut locked = store.lock().await;
            members_insert(room_id, res, locked.deref_mut());
        },
        Plan::Pinned(room_id, event_ids) => {
            let msgs = pinned_load(client, &room_id, event_ids).await;
            let mut locked = store.lock().await;
            let info = locked.application.get_room_info(room_id);

            for (event_id, msg) in msgs {
                info.insert_pinned(event_id, msg);
            }
        },
        Plan::RoomPreview(alias_id) => {
            let via = {
                let locked = store.lock().await;
                locked
                    .application
                    .room_via
                    .get(&alias_id)
                    .unwrap_or(&locked.application.settings.tunables.default_via)
                    .to_vec()
            };

            let res = client.get_room_preview(&alias_id, via).await;

            let mut locked = store.lock().await;

            if let Ok(preview) = &res &&
                let Some(alias) = &preview.canonical_alias
            {
                locked
                    .application
                    .aliases
                    .insert(alias.to_owned(), preview.room_id.clone());
            }
            locked.application.room_previews.insert(alias_id, (res, Instant::now()));
        },
    }
}

async fn pinned_load(
    client: &Client,
    room_id: &RoomId,
    event_ids: Vec<OwnedEventId>,
) -> Vec<(OwnedEventId, Option<Message>)> {
    let Some(room) = client.get_room(room_id) else {
        return vec![];
    };

    let mut msgs = vec![];

    for event_id in event_ids {
        let msg = pinned_load_one(&room, room_id, &event_id).await;
        msgs.push((event_id, msg));
    }

    msgs
}

async fn pinned_load_one(
    room: &MatrixRoom,
    room_id: &RoomId,
    event_id: &EventId,
) -> Option<Message> {
    let ev = room
        .load_or_fetch_event(event_id, None)
        .await
        .inspect_err(|e| warn!(?event_id, "failed to fetch pinned event: {e}"))
        .ok()?;

    let msg = match ev.into_raw().deserialize().ok()?.into_full_event(room_id.to_owned()) {
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomMessage(ev)) => ev.into(),
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomEncrypted(ev)) => ev.into(),
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::Sticker(ev)) => ev.into(),
        AnyTimelineEvent::MessageLike(_) => return None,
        AnyTimelineEvent::State(ev) => Message::from(AnySyncStateEvent::from(ev)),
    };

    Some(msg)
}

async fn get_receipts_for_timeline_events(
    room: &MatrixRoom,
    events: Vec<TimelineEvent>,
) -> Vec<(AnyTimelineEvent, Vec<OwnedUserId>)> {
    let mut msgs = vec![];

    for ev in events.into_iter() {
        let event_id = ev.event_id().map(ToOwned::to_owned);
        let msg = match ev.kind {
            TimelineEventKind::Decrypted(event) => {
                match event.event.deserialize() {
                    Ok(event) => event,
                    Err(err) => {
                        warn!(
                            err = %err,
                            room_id = room.room_id().as_str(),
                            ?event_id,
                            "Failed to deserialize event"
                        );
                        continue;
                    },
                }
            },
            TimelineEventKind::UnableToDecrypt { event, utd_info, .. } => {
                let event = match event.deserialize() {
                    Ok(event) => {
                        tracing::debug!(
                            ?utd_info,
                            room_id = room.room_id().as_str(),
                            ?event_id,
                            "Failed to decrypt event"
                        );
                        event
                    },
                    Err(err) => {
                        warn!(
                            ?utd_info,
                            err = %err,
                            room_id = room.room_id().as_str(),
                            ?event_id,
                            "Failed to deserialize undecrypted event"
                        );
                        continue;
                    },
                };
                event.into_full_event(room.room_id().to_owned())
            },
            TimelineEventKind::PlainText { event } => {
                let event = match event.deserialize() {
                    Ok(event) => event,
                    Err(err) => {
                        warn!(
                            err = %err,
                            room_id = room.room_id().as_str(),
                            ?event_id,
                            "Failed to deserialize event"
                        );
                        continue;
                    },
                };
                event.into_full_event(room.room_id().to_owned())
            },
        };

        let event_id = msg.event_id();
        let receipts = match room
            .load_event_receipts(ReceiptType::Read, &ReceiptThread::Main, event_id)
            .await
        {
            Ok(receipts) => receipts.into_iter().map(|(u, _)| u).collect(),
            Err(e) => {
                tracing::warn!(?event_id, "failed to get event receipts: {e}");
                vec![]
            },
        };

        msgs.push((msg, receipts));
    }

    msgs
}

async fn load_older_one(room: &MatrixRoom) -> MessageFetchResult {
    // Update cached encryption state. This is a noop if the state is already cached.
    let _ = room.request_encryption_state().await;

    let (cache, _drop_handle) = room.event_cache().await.map_err(IambError::from)?;

    let outcome = cache
        .pagination()
        .run_backwards_until(MIN_MSG_LOAD)
        .await
        .map_err(IambError::from)?;

    let msgs = get_receipts_for_timeline_events(room, outcome.events).await;

    Ok((outcome.reached_start, msgs))
}

fn insert_msgs_and_receipts(
    msgs: Vec<(AnyTimelineEvent, Vec<OwnedUserId>)>,
    info: &mut RoomInfo,
    presences: &mut CompletionMap<OwnedUserId, PresenceState>,
    previews: &mut PreviewManager,
    settings: &ApplicationSettings,
) {
    for (msg, receipts) in msgs {
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
                info.insert_with_preview(msg, settings, previews);
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::Reaction(ev)) => {
                info.insert_reaction_with_preview(ev, settings, previews);
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::Sticker(ev)) => {
                info.insert_sticker_with_preview(ev, settings, previews);
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomRedaction(..)) => {
                // ignoring redaction events because we get them bundled with the redacted event
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::PollStart(ev)) => {
                info.insert_poll_start(ev);
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::UnstablePollStart(ev)) => {
                info.insert_unstable_poll_start(ev);
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::PollResponse(ev)) => {
                if let MessageLikeEvent::Original(ev) = ev {
                    info.insert_poll_relation(ev.into());
                }
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::UnstablePollResponse(ev)) => {
                if let MessageLikeEvent::Original(ev) = ev {
                    info.insert_unstable_poll_relation(ev.into());
                }
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::PollEnd(ev)) => {
                if let MessageLikeEvent::Original(ev) = ev {
                    info.insert_poll_relation(ev.into());
                }
            },
            AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::UnstablePollEnd(ev)) => {
                if let MessageLikeEvent::Original(ev) = ev {
                    info.insert_unstable_poll_relation(ev.into());
                }
            },
            AnyTimelineEvent::MessageLike(ev) => {
                tracing::trace!(
                    event_id = ev.event_id().as_str(),
                    "Ignoring unimplemented event type {}",
                    ev.event_type()
                );
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

fn load_insert(
    room_id: OwnedRoomId,
    res: MessageFetchResult,
    locked: &mut ProgramStore,
    message_needs: Vec<MessageNeed>,
) {
    let ChatStore { presences, rooms, previews, settings, .. } = &mut locked.application;
    let info = rooms.get_or_default(room_id.clone());
    info.fetching = false;

    match res {
        Ok((reached_start, msgs)) => {
            insert_msgs_and_receipts(msgs, info, presences, previews, settings);

            info.reached_timeline_start = reached_start;

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
    let plans = load_plans(store).await;

    plans
        .into_iter()
        .map(|plan| run_plan(client, store, plan))
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

fn member_active(state: &MembershipState) -> bool {
    matches!(state, MembershipState::Invite | MembershipState::Join)
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
            let is_active = member_active(member.membership());

            info.display_names.set(user_id, name, is_active);
        }
    }
    // else ???
}

async fn load_initial_messages(client: Client, store: AsyncProgramStore) {
    let rooms = client.joined_rooms();
    let mut need_load = vec![];

    // load initial cache
    for room in rooms.iter().filter(|room| !room.is_space()) {
        let cache = match room.event_cache().await {
            Ok((c, _)) => c,
            Err(e) => {
                // we got the room id from the client and have subscribed to the event cache,
                // so this should only error on IO errors. Skipping here just means that we
                // don't get any events from the cache and will fetch them from the homeserver
                // later on when viewing the room.
                tracing::error!(err = %e, "failed to load events from room cache");
                continue;
            },
        };

        let events = match cache.events().await {
            Ok(events) => events,
            Err(e) => {
                tracing::warn!(room_id = ?room.room_id(), "failed to load cached events: {e}");
                continue;
            },
        };

        if events.len() < MIN_MSG_LOAD as usize {
            need_load.push(room);
        }

        let msgs = get_receipts_for_timeline_events(room, events).await;

        let mut locked = store.lock().await;
        let ChatStore { presences, rooms, previews, settings, .. } = &mut locked.application;
        let info = rooms.get_or_default(room.room_id().to_owned());
        insert_msgs_and_receipts(msgs, info, presences, previews, settings);
    }

    // This is an arbitrary limit on how much work we do in parallel to avoid
    // spawning too many tasks at startup and overwhelming the client. We
    // should normally only surpass this limit at startup when doing an initial
    // fetch for each room.
    const LIMIT: usize = 15;
    let permits = Semaphore::new(LIMIT);

    // paginate backwards to get more events in the rooms
    need_load
        .into_iter()
        .map(|room| {
            async {
                let permit = permits.acquire().await;

                let (reached_start, msgs) = match load_older_one(room).await {
                    Ok(v) => v,
                    Err(e) => {
                        tracing::warn!(room_id = ?room.room_id(), "failed to paginate cached events: {e}");
                        return;
                    },
                };

                let mut locked = store.lock().await;
                let ChatStore { presences, rooms, previews, settings, .. } =
                    &mut locked.application;
                let info = rooms.get_or_default(room.room_id().to_owned());
                info.reached_timeline_start = reached_start;
                insert_msgs_and_receipts(msgs, info, presences, previews, settings);
                drop(permit);
            }
        })
        .collect::<FuturesUnordered<_>>()
        .count()
        .await;
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
    let mut pinned = vec![];
    let mut names_and_tags = vec![];

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
        let mut aliases = room.alt_aliases();
        aliases.extend(room.canonical_alias());

        pinned.push((room.room_id().to_owned(), room.pinned_event_ids().unwrap_or_default()));
        names_and_tags.push((room.room_id().to_owned(), name, tags, aliases));

        if room.is_direct().await.unwrap_or_default() {
            dms.push(room);
        } else if room.is_space() {
            spaces.push(room);
        } else {
            rooms.push(room);
        }
    }

    let mut locked = store.lock().await;
    locked.application.sync_info.spaces = spaces;
    locked.application.sync_info.rooms = rooms;
    locked.application.sync_info.dms = dms;

    for (room_id, name, tags, aliases) in names_and_tags {
        locked.application.set_room_info(room_id, name, tags, aliases);
    }

    for (room_id, pinned_events) in pinned {
        locked.application.get_room_info(room_id).pinned_events = pinned_events;
    }
}

async fn refresh_rooms_forever(client: &Client, store: &AsyncProgramStore) {
    let mut interval = tokio::time::interval(Duration::from_secs(5));

    loop {
        refresh_rooms(client, store, false).await;
        interval.tick().await;
    }
}

fn convert_receipt_type(value: ReceiptType) -> CreateReceiptType {
    match value {
        ReceiptType::Read => CreateReceiptType::Read,
        ReceiptType::ReadPrivate => CreateReceiptType::ReadPrivate,
        _ => CreateReceiptType::from(value.as_str()),
    }
}

/// Send a single read receipt, and return `true` if the server accepted it and we don't need
/// to retry sending it later on.
async fn send_single_receipt(
    client: &Client,
    room_id: &RoomId,
    thread: ReceiptThread,
    receipt_type: ReceiptType,
    event_id: OwnedEventId,
) -> bool {
    let Some(room) = client.get_room(room_id) else {
        tracing::warn!(?room_id, "trying to send receipt to unknown room");
        return false;
    };

    if ReceiptThread::Main == thread || ReceiptThread::Unthreaded == thread {
        let _ = room
            .set_unread_flag(false)
            .await
            .inspect_err(|e| tracing::warn!(?room_id, "Failed to clear unread flag: {e}"));
    }

    room.send_single_receipt(convert_receipt_type(receipt_type), thread, event_id)
        .await
        .inspect_err(|e| tracing::warn!(?room_id, "Failed to send read receipt: {e}"))
        .is_ok()
}

/// Listen for receipt updates from the main thread, and try sending them to the homeserver.
///
/// Any receipts we fail to send will be queued for resending later on.
async fn send_receipts_forever(client: &Client, stream: UnboundedReceiver<ReceiptUpdate>) {
    let mut stream = UnboundedReceiverStream::new(stream);

    let mut sent: HashMap<ReceiptKey, OwnedEventId> = Default::default();
    let mut outstanding: HashMap<ReceiptKey, OwnedEventId> = Default::default();
    let mut next_attempt = Instant::now();

    loop {
        tokio::select! {
            update = stream.next() => {
                let Some((room_id, thread, receipt_type, event_id)) = update else {
                    // The sender side has gone away, so just exit the loop.
                    return;
                };

                let key = (room_id, thread, receipt_type);

                if sent.get(&key).is_some_and(|sent| *sent == event_id) {
                    // Skip sending a duplicate receipt update.
                    continue;
                }

                outstanding.insert(key, event_id);
            },

            // If we have outstanding receipts, then ensure that we attempt a retry before
            // the next receipt update arrives over the stream:
            _ = tokio::time::sleep_until(next_attempt.into()), if !outstanding.is_empty() => {},
        }

        if Instant::now() < next_attempt {
            // Still backing off from an earlier failure.
            continue;
        }

        // Receipts are sent one at a time: two updates for the same room sent concurrently
        // can land out of order and leave the server pointing at the older event.
        let mut failed = false;

        for (key, event_id) in std::mem::take(&mut outstanding) {
            let (room_id, thread, receipt_type) = key.clone();
            let success =
                send_single_receipt(client, &room_id, thread, receipt_type, event_id.clone()).await;

            if success {
                sent.insert(key, event_id);
            } else {
                // Could not contact homeserver to send the receipt, so queue for a later
                // retry and mark that we need to backoff to avoid spamming send failures.
                outstanding.insert(key, event_id);
                failed = true;
            }
        }

        if failed {
            next_attempt = Instant::now() + RECEIPT_RETRY_INTERVAL;
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

            RoomSendQueueUpdate::SendError { error, .. } => {
                // XXX: Retry recoverable errors
                locked.application.draw_error = Some(format!("Error sending message: {error}"));
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

    tokio::spawn(load_initial_messages(client.clone(), store.clone()));

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

    pub fn try_recv(&self) -> Option<T> {
        self.0.try_recv().ok()
    }
}

impl<T> ClientReply<T> {
    fn send(self, t: T) {
        let _ = self.0.send(t);
    }
}

fn oneshot<T>() -> (ClientReply<T>, ClientResponse<T>) {
    let (tx, rx) = sync_channel(1);
    let reply = ClientReply(tx);
    let response = ClientResponse(rx);

    return (reply, response);
}

pub type FetchedRoom = (MatrixRoom, RoomDisplayName);

pub enum WorkerTask {
    Init(AsyncProgramStore, ClientReply<()>),
    Login(LoginStyle, ClientReply<IambResult<EditInfo>>),
    Logout(String, ClientReply<IambResult<EditInfo>>),
    GetInviter(MatrixRoom, ClientReply<IambResult<Option<RoomMember>>>),
    GetRoom(OwnedRoomId, ClientReply<IambResult<FetchedRoom>>),
    ResolveAlias(OwnedRoomAliasId, ClientReply<IambResult<OwnedRoomId>>),
    JoinRoom(OwnedRoomOrAliasId, Vec<OwnedServerName>, ClientReply<IambResult<OwnedRoomId>>),
    CreateDM(OwnedUserId, ClientReply<IambResult<OwnedRoomId>>),
    Members(OwnedRoomId, ClientReply<IambResult<Vec<RoomMember>>>),
    SpaceMembers(OwnedRoomId, ClientReply<IambResult<Vec<OwnedRoomId>>>),
    TypingNotice(OwnedRoomId),
    LoadImage(MediaSource, PreviewKind, Size, Arc<Picker>, Arc<Semaphore>),
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
            WorkerTask::ResolveAlias(s, _) => {
                f.debug_tuple("WorkerTask::ResolveAlias")
                    .field(s)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::JoinRoom(s, via, _) => {
                f.debug_tuple("WorkerTask::JoinRoom")
                    .field(s)
                    .field(via)
                    .field(&format_args!("_"))
                    .finish()
            },
            WorkerTask::CreateDM(user_id, _) => {
                f.debug_tuple("WorkerTask::CreateDM")
                    .field(user_id)
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
        .sqlite_store_with_cache_path(
            settings.sqlite_dir.as_path(),
            settings.sqlite_cache_dir.as_path(),
            None,
        )
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

async fn direct_message(user: OwnedUserId, client: &Client) -> IambResult<OwnedRoomId> {
    if let Some(room) = client.get_dm_room(&user) {
        return Ok(room.room_id().to_owned());
    }

    client
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

async fn join_room(
    alias_id: OwnedRoomOrAliasId,
    via: Vec<OwnedServerName>,
    client: Client,
) -> IambResult<OwnedRoomId> {
    match client.join_room_by_id_or_alias(&alias_id, &via).await {
        Ok(resp) => Ok(resp.room_id().to_owned()),
        Err(e) => {
            let msg = e.to_string();
            let err = UIError::Failure(msg);
            return Err(err);
        },
    }
}

#[derive(Clone)]
pub struct Requester {
    pub client: Client,
    pub tx: UnboundedSender<WorkerTask>,
    pub receipts: UnboundedSender<ReceiptUpdate>,
}

impl Requester {
    pub fn init(&self, store: AsyncProgramStore) {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::Init(store, reply)).unwrap();

        return response.recv();
    }

    pub fn send_receipt(
        &self,
        room: OwnedRoomId,
        thread: ReceiptThread,
        event: OwnedEventId,
        settings: &ApplicationSettings,
    ) {
        let receipt_type = if settings.tunables.read_receipt_send {
            ReceiptType::Read
        } else {
            ReceiptType::ReadPrivate
        };

        let _ = self
            .receipts
            .send((room, thread, receipt_type, event))
            .inspect_err(|_| tracing::warn!("Read receipt worker is no longer running"));
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

    pub fn resolve_alias(&self, alias_id: OwnedRoomAliasId) -> IambResult<OwnedRoomId> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::ResolveAlias(alias_id, reply)).unwrap();

        return response.recv();
    }

    pub fn join_room_chan(
        &self,
        alias_id: OwnedRoomOrAliasId,
        via: Vec<OwnedServerName>,
    ) -> ClientResponse<IambResult<OwnedRoomId>> {
        let (reply, response) = oneshot();
        self.tx.send(WorkerTask::JoinRoom(alias_id, via, reply)).unwrap();
        response
    }

    pub fn join_room(
        &self,
        alias_id: OwnedRoomOrAliasId,
        via: Vec<OwnedServerName>,
    ) -> IambResult<OwnedRoomId> {
        self.join_room_chan(alias_id, via).recv()
    }

    pub fn create_dm(&self, user_id: OwnedUserId) -> IambResult<OwnedRoomId> {
        let (reply, response) = oneshot();

        self.tx.send(WorkerTask::CreateDM(user_id, reply)).unwrap();

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

    pub fn load_image(
        &self,
        source: MediaSource,
        kind: PreviewKind,
        size: Size,
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

    /// this will be removed after login
    unspawned_receipt_stream: Option<UnboundedReceiver<ReceiptUpdate>>,

    /// Take care when locking since worker commands are sent with the lock already held
    store: Option<AsyncProgramStore>,
}

impl ClientWorker {
    pub async fn spawn(client: Client, settings: ApplicationSettings) -> Requester {
        let (tx, rx) = unbounded_channel();
        let (receipt_tx, receipt_rx) = unbounded_channel();

        let mut worker = ClientWorker {
            initialized: false,
            settings,
            client: client.clone(),
            load_handle: None,
            sync_handle: None,
            unspawned_receipt_stream: Some(receipt_rx),
            store: None,
        };

        tokio::spawn(async move {
            worker.work(rx).await;
        });

        return Requester { client, tx, receipts: receipt_tx };
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
            WorkerTask::ResolveAlias(alias_id, reply) => {
                assert!(self.initialized);
                reply.send(self.resolve_alias(alias_id).await);
            },
            WorkerTask::JoinRoom(alias_id, via, reply) => {
                assert!(self.initialized);
                let client = self.client.clone();
                tokio::spawn(async move { reply.send(join_room(alias_id, via, client).await) });
            },
            WorkerTask::CreateDM(user_id, reply) => {
                assert!(self.initialized);
                reply.send(direct_message(user_id, &self.client).await);
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
                        let MessageType::VerificationRequest(content) = &msg.content.msgtype
                    {
                        verifications::handle_request(
                            ev.event_id().into(),
                            ev.sender().into(),
                            content.from_device.clone(),
                            client.clone(),
                            Arc::clone(&store.0),
                        )
                        .await
                    }

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let ChatStore { rooms, previews, settings, .. } = &mut locked.application;
                    let info = rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    info.insert_with_preview(full_ev, settings, previews);
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

                    let ChatStore { rooms, previews, settings, .. } = &mut locked.application;
                    let info = rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    info.insert_reaction_with_preview(
                        ev.into_full_event(room_id.to_owned()),
                        settings,
                        previews,
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

                    let ChatStore { rooms, settings, previews, .. } = &mut locked.application;

                    let info = rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    info.insert_sticker_with_preview(full_ev, settings, previews);
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

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<PollStartEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let info = locked.application.rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    info.insert_poll_start(full_ev);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<UnstablePollStartEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let info = locked.application.rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    info.insert_unstable_poll_start(full_ev);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<PollResponseEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let info = locked.application.rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    if let MessageLikeEvent::Original(ev) = full_ev {
                        info.insert_poll_relation(ev.into());
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<UnstablePollResponseEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let info = locked.application.rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    if let MessageLikeEvent::Original(ev) = full_ev {
                        info.insert_unstable_poll_relation(ev.into());
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<PollEndEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let info = locked.application.rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    if let MessageLikeEvent::Original(ev) = full_ev {
                        info.insert_poll_relation(ev.into());
                    }
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: SyncMessageLikeEvent<UnstablePollEndEventContent>,
             room: MatrixRoom,
             store: Ctx<AsyncProgramStore>| {
                async move {
                    let room_id = room.room_id();

                    let mut locked = store.lock().await;

                    let sender = ev.sender().to_owned();
                    let _ = locked.application.presences.get_or_default(sender);

                    let info = locked.application.rooms.get_or_default(room_id.to_owned());

                    update_event_receipts(info, &room, ev.event_id()).await;

                    let full_ev = ev.into_full_event(room_id.to_owned());
                    if let MessageLikeEvent::Original(ev) = full_ev {
                        info.insert_unstable_poll_relation(ev.into());
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
            |_: SyncRoomPinnedEventsEvent, room: MatrixRoom, store: Ctx<AsyncProgramStore>| {
                async move {
                    // The SDK has already applied the event to its room state by the time
                    // handlers run, and it also copes with redacted pin lists.
                    let pinned = room.pinned_event_ids().unwrap_or_default();

                    let mut locked = store.lock().await;
                    let info = locked.application.get_room_info(room.room_id().to_owned());
                    info.pinned_events = pinned;
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
                    let is_active = member_active(&ev.content.membership);
                    info.display_names.set(user_id, ev.content.displayname, is_active);
                }
            },
        );

        let _ = self.client.add_event_handler(
            |ev: ToDeviceKeyVerificationRequestEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                let span = tracing::info_span!(
                    "to_device_verify_request",
                    other_user_id = ?ev.sender,
                    other_device_id = ?ev.content.from_device,
                    flow_id = ?ev.content.transaction_id,
                );
                verifications::handle_request(
                    ev.content.transaction_id.into(),
                    ev.sender,
                    ev.content.from_device,
                    client,
                    store.0,
                )
                .instrument(span)
            },
        );

        let _ = self.client.add_event_handler(
            |ev: ToDeviceKeyVerificationReadyEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                let span = tracing::info_span!(
                    "to_device_verify_ready",
                    other_user_id = ?ev.sender,
                    other_device_id = ?ev.content.from_device,
                    flow_id = ?ev.content.transaction_id,
                );
                verifications::handle_ready(
                    ev.content.transaction_id.into(),
                    ev.sender,
                    client,
                    store.0,
                )
                .instrument(span)
            },
        );

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncKeyVerificationReadyEvent,
             client: Client,
             store: Ctx<AsyncProgramStore>| {
                let span = tracing::info_span!(
                    "room_verify_ready",
                    other_user_id = ?ev.sender,
                    other_device_id = ?ev.content.from_device,
                    flow_id = ?ev.content.relates_to.event_id,
                );
                verifications::handle_ready(
                    ev.content.relates_to.event_id.into(),
                    ev.sender,
                    client,
                    store.0,
                )
                .instrument(span)
            },
        );

        let _ = self.client.add_event_handler(
            |ev: ToDeviceKeyVerificationStartEvent, client: Client| {
                let span = tracing::info_span!(
                    "to_device_verify_start",
                    other_user_id = ?ev.sender,
                    other_device_id = ?ev.content.from_device,
                    flow_id = ?ev.content.transaction_id,
                );
                verifications::handle_start(ev.content.transaction_id.into(), ev.sender, client)
                    .instrument(span)
            },
        );

        let _ = self.client.add_event_handler(
            |ev: OriginalSyncKeyVerificationStartEvent, client: Client| {
                let span = tracing::info_span!(
                    "room_verify_start",
                    other_user_id = ?ev.sender,
                    other_device_id = ?ev.content.from_device,
                    flow_id = ?ev.content.relates_to.event_id
                );
                verifications::handle_start(
                    ev.content.relates_to.event_id.into(),
                    ev.sender,
                    client,
                )
                .instrument(span)
            },
        );

        self.store = Some(store.clone());

        let unspawned_receipt_stream = self
            .unspawned_receipt_stream
            .take()
            .expect("client was started multiple times");
        self.load_handle = tokio::spawn({
            let client = self.client.clone();

            async move {
                while !client.is_active() {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                }

                let load = load_older_forever(&client, &store);
                let rcpt = send_receipts_forever(&client, unspawned_receipt_stream);
                let room = refresh_rooms_forever(&client, &store);
                let notifications = register_notifications(&client, &store);
                let sendqueue = subscribe_sendqueue_forever(&client, &store);
                let ((), (), (), (), ()) = tokio::join!(load, room, rcpt, notifications, sendqueue);
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

        let sync_delay = Duration::from_millis(self.settings.tunables.sync_delay_ms);
        self.sync_handle = tokio::spawn(async move {
            loop {
                let settings = SyncSettings::default();
                let _ = client.sync(settings).await;
                tokio::time::sleep(sync_delay).await;
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

    async fn get_inviter(&mut self, invited: MatrixRoom) -> IambResult<Option<RoomMember>> {
        let details = invited.invite_details().await.map_err(IambError::from)?;

        Ok(details.inviter)
    }

    async fn get_room(&mut self, room_id: OwnedRoomId) -> IambResult<FetchedRoom> {
        if let Some(room) = self.client.get_room(&room_id) {
            let name = if let Some(name) = room.cached_display_name() {
                name
            } else {
                room.display_name().await.map_err(IambError::from)?
            };

            Ok((room, name))
        } else {
            Err(IambError::UnknownRoom(room_id).into())
        }
    }

    async fn resolve_alias(&mut self, alias_id: OwnedRoomAliasId) -> IambResult<OwnedRoomId> {
        match self.client.resolve_room_alias(&alias_id).await {
            Ok(resp) => Ok(resp.room_id),
            Err(e) => {
                let msg = e.to_string();
                let err = UIError::Failure(msg);

                return Err(err);
            },
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
}
