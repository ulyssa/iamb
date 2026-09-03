//! # Voice/Video calls (VoIP)
//!
//! This module implements voice calls for iamb using [LiveKit] for the media
//! transport and the Matrix protocol (MatrixRTC) for signaling.
//!
//! It is only compiled when the `voip` feature is enabled.
//!
//! ## Layout
//!
//! - [`mod@self`] - call session state machine and the types exchanged over
//!   Matrix signaling ([`CallManager`], [`CallSession`], [`CallStatus`]).
//! - [`livekit_session`] - the LiveKit `Room` connection: audio capture,
//!   playback, and end-to-end encryption key handling.
//!
//! ## Signaling overview
//!
//! Joining a call means:
//! 1. Discover the LiveKit focus (SFU) URL from existing `m.call.member` state
//!    events in the room.
//! 2. Obtain an OpenID token and exchange it at the `lk-jwt-service` `/sfu/get`
//!    endpoint for a LiveKit JWT.
//! 3. Generate an E2EE key and connect to the LiveKit room.
//! 4. Publish our own `m.call.member` state event (state key
//!    `_{user_id}_{device_id}`) and share E2EE keys with other participants via
//!    `io.element.call.encryption_keys` to-device events.
//!

pub mod devices;
pub mod livekit_session;
pub mod matrix_rtc;

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use matrix_sdk::ruma::{
    MilliSecondsSinceUnixEpoch,
    OwnedDeviceId,
    OwnedEventId,
    OwnedRoomId,
    OwnedUserId,
    UserId,
    events::{Mentions, macros::EventContent},
};
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::UnboundedSender;

use matrix_sdk::ruma::events::call::member::LivekitFocus;

use self::livekit_session::{LiveKitSession, SessionConfig};
use crate::worker::WorkerTask;

/// A single E2EE key at a given index, as carried in an
/// `io.element.call.encryption_keys` to-device event.
///
/// The key itself is unpadded base64 of the raw key material.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CallEncryptionKey {
    /// The key index this key occupies in the sender's key ring.
    pub index: u8,

    /// The raw key material, unpadded base64.
    pub key: String,
}

/// Who a key belongs to, as the sender identifies themselves.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CallKeyMember {
    /// The user the key belongs to, as the sender wrote it.
    ///
    /// A plain [`String`] rather than an [`OwnedUserId`] on purpose. This field
    /// is remote input, and ruma rejects the whole event if it does not
    /// validate - taking the key down with it, which is silence for the rest of
    /// the call. Nothing is lost by being lenient: the field is only ever
    /// checked against the event's `sender`, which the homeserver vouches for,
    /// and a value that does not parse cannot match a parsed sender anyway.
    pub id: String,

    /// The device the key belongs to.
    ///
    /// "Claimed" because nothing authenticates it: the homeserver vouches for
    /// the event's `sender`, but the device named in the content is whatever the
    /// sender chose to put there.
    pub claimed_device_id: OwnedDeviceId,
}

/// The call session a key belongs to.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CallKeySession {
    /// The kind of session; `m.call` for a voice or video call.
    pub application: String,

    /// Identifier of the call session; empty for the room-wide call.
    pub call_id: String,

    /// How widely the session is scoped; `m.room` for the room-wide call.
    pub scope: String,
}

impl CallKeySession {
    /// The session block describing a room's one and only call, matching the
    /// `m.call.member` state published by `matrix_rtc::publish_membership`.
    pub fn room_call() -> Self {
        CallKeySession {
            application: "m.call".to_owned(),
            call_id: String::new(),
            scope: "m.room".to_owned(),
        }
    }
}

/// The content of an `io.element.call.encryption_keys` to-device event.
///
/// Element Call participants send this to every other device in the call to
/// distribute the key their media is encrypted with, and resend it whenever a
/// late joiner appears.
///
/// This is *not* the shape of the identically named room event, which carries a
/// `keys` array and names the device at the top level. The to-device event
/// carries a single key and identifies the sender through `member`. Modelling it
/// with the room event's shape is silently fatal: every incoming key fails to
/// deserialize before it reaches a handler, and every key we send is dropped by
/// the other end, leaving a call in which nobody can decrypt anybody.
#[derive(Clone, Debug, Deserialize, Serialize, EventContent)]
#[ruma_event(type = "io.element.call.encryption_keys", kind = ToDevice)]
pub struct CallEncryptionKeysEventContent {
    /// The key this event distributes.
    pub keys: CallEncryptionKey,

    /// The participant the key belongs to.
    pub member: CallKeyMember,

    /// The room the call is taking place in, as the sender wrote it.
    ///
    /// Lenient for the same reason as [`CallKeyMember::id`]: a room id that
    /// does not validate must not cost us the key it came with. It is only used
    /// to tell a key for this call from one for a call we are not in, which is
    /// a comparison that works just as well on the raw string.
    pub room_id: String,

    /// The call session the key is for.
    pub session: CallKeySession,

    /// When the sender sent this.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sent_ts: Option<MilliSecondsSinceUnixEpoch>,
}

/// The content of an `io.element.call.encryption_keys` **room** event.
///
/// The older of MatrixRTC's two key transports, and still the only one some
/// Element Call builds speak. It carries the same key material as
/// [`CallEncryptionKeysEventContent`] in a different shape: a `keys` *array*,
/// the device named at the top level, and no `room_id` or `session` block
/// the event is already in the room, and `call_id` names the session.
///
/// A client that only sends and receives the to-device form interoperates with
/// nothing but itself and current Element Call. Speaking both is what
/// matrix-js-sdk does for the duration of the migration, and it costs one extra
/// event per key.
#[derive(Clone, Debug, Deserialize, Serialize, EventContent)]
#[ruma_event(type = "io.element.call.encryption_keys", kind = MessageLike)]
pub struct CallEncryptionKeysRoomEventContent {
    /// The keys this event distributes.
    pub keys: Vec<CallEncryptionKey>,

    /// The device the keys belong to.
    ///
    /// The user is the event's `sender`, which the homeserver vouches for.
    pub device_id: OwnedDeviceId,

    /// Identifier of the call session; empty for the room-wide call.
    pub call_id: String,

    /// When the sender sent this.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sent_ts: Option<MilliSecondsSinceUnixEpoch>,
}

/// An E2EE key received from another participant, resolved to the participant
/// it belongs to.
#[derive(Clone, Debug)]
pub struct ReceivedCallKey {
    /// The room the call is taking place in.
    pub room_id: OwnedRoomId,

    /// The user that sent the key.
    pub user_id: OwnedUserId,

    /// The device that sent the key.
    pub device_id: OwnedDeviceId,

    /// The key index within that participant's key ring.
    pub index: u8,

    /// The decoded raw key material.
    pub key: Vec<u8>,
}

impl ReceivedCallKey {
    /// The LiveKit participant identity this key applies to.
    ///
    /// Element Call derives it as `{user_id}:{device_id}`, matching the identity
    /// each participant connects to the SFU with.
    pub fn participant_identity(&self) -> String {
        format!("{}:{}", self.user_id, self.device_id)
    }
}

/// How many inbound keys the inbox holds before it starts dropping the oldest.
///
/// The sync handler pushes every `io.element.call.encryption_keys` event it
/// sees, but only a *running call* drains the inbox so outside a call, and for
/// rooms whose calls we are not in, nothing ever takes these out again. Without
/// a cap the inbox is an unbounded buffer fed by remote peers, growing for as
/// long as the client stays open.
///
/// Dropping the oldest is the right end to lose: keys are cumulative per
/// participant and each new key for a given index supersedes the last, so the
/// newest entries are the ones that still describe the call.
const KEY_INBOX_LIMIT: usize = 256;

/// Keys received from other participants, waiting to be handed to LiveKit.
///
/// The Matrix sync handler and the call's LiveKit session live on different
/// threads, so inbound keys are buffered here rather than applied directly. Keys
/// can also arrive before we have finished joining, in which case the session
/// drains whatever accumulated once it connects.
///
/// Bounded at [`KEY_INBOX_LIMIT`]; see there for why that matters.
#[derive(Clone, Default)]
pub struct KeyInbox(Arc<Mutex<VecDeque<ReceivedCallKey>>>);

impl KeyInbox {
    /// Buffer a newly received key, evicting the oldest if the inbox is full.
    pub fn push(&self, key: ReceivedCallKey) {
        if let Ok(mut keys) = self.0.lock() {
            while keys.len() >= KEY_INBOX_LIMIT {
                keys.pop_front();
            }

            keys.push_back(key);
        }
    }

    /// Take everything buffered so far, leaving the inbox empty.
    pub fn drain(&self) -> Vec<ReceivedCallKey> {
        match self.0.lock() {
            Ok(mut keys) => std::mem::take(&mut *keys).into(),
            Err(_) => Vec::new(),
        }
    }

    /// How many keys are currently buffered.
    #[cfg(test)]
    fn len(&self) -> usize {
        self.0.lock().map(|keys| keys.len()).unwrap_or(0)
    }
}

/// A call someone has rung us about and that we have neither joined nor
/// declined ([MSC4075]).
///
/// Distinct from "a call is happening in this room", which is what
/// `m.call.member` state says: this is somebody actively asking *us* to pick up,
/// and it is the thing `:call decline` answers.
///
/// [MSC4075]: https://github.com/matrix-org/matrix-spec-proposals/pull/4075
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IncomingCall {
    /// The `m.rtc.notification` event that rang us.
    ///
    /// Kept because declining is a reference relation to it, so a decline is
    /// impossible without having held on to the event ID.
    pub notification: OwnedEventId,

    /// Who started the call.
    pub from: OwnedUserId,

    /// When this stops being worth ringing about.
    ///
    /// Computed by the SDK from the sender's timestamp and the server's, so a
    /// caller with a badly set clock cannot ring us indefinitely.
    pub expires_at: MilliSecondsSinceUnixEpoch,

    /// Whether the caller asked for an audible ring rather than a quiet
    /// notification.
    pub ring: bool,
}

/// Whether a call notification's mentions address us.
///
/// Mentions are how MSC4075 says who should be rung, so this is what keeps a
/// call aimed at two specific people in a large room from ringing everybody. A
/// notification carrying no mentions at all addresses nobody and is ignored:
/// treating it as room-wide would make the absence of a target mean the widest
/// possible target.
pub fn ring_is_for_us(mentions: Option<&Mentions>, user_id: &UserId) -> bool {
    match mentions {
        Some(mentions) => mentions.room || mentions.user_ids.contains(user_id),
        None => false,
    }
}

impl IncomingCall {
    /// Whether this notification is still within its lifetime.
    ///
    /// Checked at every read rather than cleared on a timer: an expired ring
    /// needs to stop being offered as answerable even if nothing has happened in
    /// the room since, and nothing else would wake us to clear it.
    pub fn is_live(&self) -> bool {
        self.expires_at > MilliSecondsSinceUnixEpoch::now()
    }
}

/// State for the call the local user is currently participating in.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ActiveCall {
    /// The room whose call we have joined.
    pub room_id: OwnedRoomId,

    /// Whether we have finished connecting to the SFU.
    ///
    /// Joining returns as soon as the signalling is done, but the SFU
    /// connection is still being established on the call thread, so there is a
    /// window where we are in the call without being able to hear anyone.
    pub connected: bool,

    /// Whether our microphone is currently muted.
    pub muted: bool,

    /// The participants LiveKit currently reports as speaking.
    ///
    /// Resolved back to Matrix users from the SFU participant identities, so the
    /// UI can highlight them in the participant list without knowing anything
    /// about LiveKit.
    pub speakers: Vec<OwnedUserId>,
}

/// The call state the UI renders.
///
/// There is exactly one writer, the worker, which owns the call, and the UI
/// only ever reads. That matters because the obvious alternative, letting the UI
/// keep its own copy and update it from command results, gives two sources of
/// truth for one fact and they drift apart the moment a call ends on its own.
///
/// This is deliberately *not* stored behind the [`AsyncProgramStore`] lock: the
/// UI holds that lock while blocking on a worker reply, so the worker cannot
/// take it without risking deadlock. A tiny `std` mutex, never held across an
/// `await`, sidesteps that entirely.
///
/// [`AsyncProgramStore`]: crate::base::AsyncProgramStore
#[derive(Clone, Default)]
pub struct CallStatus(Arc<Mutex<Option<ActiveCall>>>);

impl CallStatus {
    /// The call currently in progress, if any.
    pub fn get(&self) -> Option<ActiveCall> {
        self.0.lock().ok().and_then(|call| call.clone())
    }

    /// Record that we have joined the call in `room_id` and are connecting.
    pub fn joined(&self, room_id: OwnedRoomId) {
        if let Ok(mut call) = self.0.lock() {
            *call = Some(ActiveCall {
                room_id,
                connected: false,
                muted: false,
                speakers: Vec::new(),
            });
        }
    }

    /// Record that the SFU connection is up and media is flowing.
    pub fn connected(&self) {
        if let Ok(mut call) = self.0.lock() {
            if let Some(call) = call.as_mut() {
                call.connected = true;
            }
        }
    }

    /// Record that the call is over, however it ended.
    pub fn left(&self) {
        if let Ok(mut call) = self.0.lock() {
            *call = None;
        }
    }

    /// Record the microphone mute state of the call in progress.
    pub fn set_muted(&self, muted: bool) {
        if let Ok(mut call) = self.0.lock() {
            if let Some(call) = call.as_mut() {
                call.muted = muted;
            }
        }
    }

    /// Record who the SFU currently reports as speaking.
    pub fn set_speakers(&self, speakers: Vec<OwnedUserId>) {
        if let Ok(mut call) = self.0.lock() {
            if let Some(call) = call.as_mut() {
                call.speakers = speakers;
            }
        }
    }
}

/// A control message sent from the worker to the call's dedicated thread.
enum CallControl {
    /// Mute (`true`) or unmute (`false`) the local microphone.
    Mute(bool),

    /// Start encrypting our media with a new key at the given index.
    ///
    /// Sent after the worker has rotated the key and told the remaining
    /// participants about it.
    SetKey(u8, Vec<u8>),

    /// Tear the call down and let the thread exit.
    Shutdown,
}

/// Something the call thread needs the worker to do on its behalf.
///
/// The call thread owns no Matrix state, so anything that has to talk to the
/// homeserver sharing our E2EE key with a late joiner, reporting that the SFU
/// dropped us goes back to the worker as one of these.
#[derive(Debug)]
pub enum CallNotice {
    /// We are connected to the SFU and exchanging media.
    Connected,

    /// A participant joined the SFU and needs our E2EE key.
    ShareKey(OwnedUserId),

    /// Participants left the SFU, so our key should be rotated and reshared.
    ///
    /// Sent after a short debounce rather than per departure, so that everyone
    /// leaving at the end of a call causes one rotation instead of one each.
    RotateKey,

    /// The set of participants the SFU reports as currently speaking.
    Speakers(Vec<OwnedUserId>),

    /// The LiveKit session ended without us asking it to.
    Ended(String),
}

/// How often the call thread hands newly arrived E2EE keys to LiveKit.
const KEY_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(250);

/// How long to wait for the SFU to acknowledge us leaving before giving up.
///
/// Hanging up joins this thread from the worker, which the UI is blocked on, so
/// an unreachable SFU must not be able to freeze the application indefinitely.
const DISCONNECT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(3);

/// How long to wait after a participant leaves before rotating our E2EE key.
///
/// Departures cluster: a call winding down empties the room in a burst, and
/// rotating per departure would burn several key indices and send a to-device
/// message for each. Waiting a beat collapses that into one rotation.
const KEY_ROTATION_DEBOUNCE: std::time::Duration = std::time::Duration::from_secs(2);

/// A single active call the user is participating in.
///
/// Owned by the worker thread and tracked by [`CallManager`].
pub struct CallSession {
    /// The room whose call this is.
    pub room_id: OwnedRoomId,

    /// The system audio devices this call is using.
    ///
    /// A second handle on the same reference-counted device module the call
    /// thread holds, so that `:call device` can hot-swap devices mid-call
    /// without going through the control channel.
    pub audio: livekit::PlatformAudio,

    /// The E2EE key our own media is encrypted with, kept so that it can be
    /// resent to participants that join after us.
    pub key: Vec<u8>,

    /// The LiveKit focus this call is using.
    ///
    /// Kept so that a membership refresh republishes exactly what the original
    /// join advertised a refresh that dropped or changed the focus would tell
    /// late joiners to connect to a different SFU than the one we are on.
    pub focus: LivekitFocus,

    /// The key ring slot [`CallSession::key`] occupies.
    ///
    /// Advances on every rotation so that participants can tell a new key from
    /// the one it replaces, and so that media still in flight under the old key
    /// stays decryptable while the change propagates.
    pub key_index: u8,

    /// Whether the local microphone is currently muted.
    pub muted: bool,

    /// Sends control messages to the call's dedicated thread.
    control: std::sync::mpsc::Sender<CallControl>,

    thread: Option<std::thread::JoinHandle<()>>,
}

impl CallSession {
    /// Create a new session in the [`CallPhase::Connecting`] phase and spawn the
    /// dedicated OS thread that drives the call's media work.
    ///
    /// `notices` carries work the call thread cannot do itself back to the
    /// worker, which owns the Matrix client.
    pub fn new(
        config: SessionConfig,
        room_id: OwnedRoomId,
        focus: LivekitFocus,
        notices: UnboundedSender<WorkerTask>,
    ) -> std::io::Result<Self> {
        let (control, rx) = std::sync::mpsc::channel();
        let key = config.e2ee_key.clone();
        let audio = config.audio.clone();

        let thread = std::thread::Builder::new().name("iamb-voip".into()).spawn({
            let room_id = room_id.clone();

            move || call_thread(rx, config, room_id, notices)
        })?;

        Ok(CallSession {
            room_id,
            audio,
            key,
            focus,
            key_index: livekit_session::FIRST_KEY_INDEX,
            muted: false,
            control,
            thread: Some(thread),
        })
    }

    /// Update the mute state and signal the call thread.
    pub fn set_muted(&mut self, muted: bool) {
        self.muted = muted;
        let _ = self.control.send(CallControl::Mute(muted));
    }

    /// The slot the next rotation will move to.
    ///
    /// Distribution happens before the switch, so the caller needs the index
    /// ahead of [`CallSession::adopt_key`] rather than as a result of it.
    pub fn next_key_index(&self) -> u8 {
        livekit_session::next_key_index(self.key_index)
    }

    /// Start encrypting our media with `key` at `index`.
    ///
    /// Only call this once the remaining participants have been told, or our
    /// media becomes undecryptable to them.
    pub fn adopt_key(&mut self, index: u8, key: Vec<u8>) {
        self.key_index = index;
        self.key = key.clone();

        let _ = self.control.send(CallControl::SetKey(index, key));
    }
}

/// Reports the call as finished however the call thread stops.
///
/// Without this, a thread that dies unexpectedly. A panic, or a failure before
/// the control loop starts, leaves the worker holding a session for a call that
/// is not happening, and the UI showing a banner for it forever.
///
/// Disarmed on the paths where the worker already knows: it asked us to shut
/// down, or it dropped the session out from under us.
struct EndGuard {
    room_id: OwnedRoomId,
    notices: UnboundedSender<WorkerTask>,
    reason: Option<String>,
}

impl EndGuard {
    fn new(room_id: OwnedRoomId, notices: UnboundedSender<WorkerTask>) -> Self {
        let reason = Some("the call thread stopped unexpectedly".to_string());

        EndGuard { room_id, notices, reason }
    }

    /// Report `reason` if the thread stops from here on.
    fn failing(&mut self, reason: String) {
        self.reason = Some(reason);
    }

    /// The worker already knows the call is over; stay quiet.
    fn disarm(&mut self) {
        self.reason = None;
    }
}

impl Drop for EndGuard {
    fn drop(&mut self) {
        let Some(reason) = self.reason.take() else {
            return;
        };

        let notice = CallNotice::Ended(reason);
        let _ = self.notices.send(WorkerTask::CallNotice(self.room_id.clone(), notice));
    }
}

/// Body of the dedicated VoIP thread.
///
/// The thread owns a private tokio runtime that exists only for the duration of
/// the call: LiveKit's client is async, but running it here keeps every bit of
/// media work off the runtime that serves the UI. The thread itself does the one
/// thing it is best at, blocking on the control channel, while the runtime
/// drives the SFU connection and the room event pump.
fn call_thread(
    rx: std::sync::mpsc::Receiver<CallControl>,
    config: SessionConfig,
    room_id: OwnedRoomId,
    notices: UnboundedSender<WorkerTask>,
) {
    let mut guard = EndGuard::new(room_id.clone(), notices.clone());

    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .thread_name("iamb-voip-rt")
        .enable_all()
        .build();

    let runtime = match runtime {
        Ok(runtime) => runtime,
        Err(e) => return guard.failing(format!("could not start the call runtime: {e}")),
    };

    let (session, events) = match runtime.block_on(LiveKitSession::connect(config)) {
        Ok(connected) => connected,
        Err(e) => return guard.failing(format!("{e:#}")),
    };

    let _ = notices.send(WorkerTask::CallNotice(room_id.clone(), CallNotice::Connected));

    let session = Arc::new(session);
    let pump = runtime.spawn(pump_room(session.clone(), events, room_id.clone(), notices.clone()));

    while let Ok(msg) = rx.recv() {
        match msg {
            CallControl::Mute(muted) => session.set_muted(muted),
            CallControl::SetKey(index, key) => session.set_our_key(index, &key),
            CallControl::Shutdown => break,
        }
    }

    // Either the worker told us to stop or it dropped the session; both mean it
    // already knows, so leaving here is not something to report back.
    guard.disarm();

    pump.abort();

    let left = runtime
        .block_on(async { tokio::time::timeout(DISCONNECT_TIMEOUT, session.disconnect()).await });

    match left {
        Ok(Ok(())) => {},
        Ok(Err(e)) => tracing::warn!("failed to leave the LiveKit room cleanly: {e:#}"),
        Err(_) => tracing::warn!("timed out leaving the LiveKit room"),
    }
}

/// Pump the LiveKit room's event stream for the lifetime of the call.
///
/// Runs on the call thread's own runtime. Besides reacting to room events it
/// periodically hands LiveKit the keys the Matrix sync handler has buffered,
/// since those arrive on a completely different thread.
async fn pump_room(
    session: Arc<LiveKitSession>,
    mut events: tokio::sync::mpsc::UnboundedReceiver<livekit::RoomEvent>,
    room_id: OwnedRoomId,
    notices: UnboundedSender<WorkerTask>,
) {
    use livekit::RoomEvent;

    let mut keys = tokio::time::interval(KEY_POLL_INTERVAL);

    // When the pending rotation, if any, becomes due. Departures are collapsed
    // into one rotation by pushing this forward each time another one arrives.
    let mut rotate_at: Option<tokio::time::Instant> = None;

    loop {
        tokio::select! {
            _ = keys.tick() => {
                session.drain_inbox();

                // Piggy-backing the debounce on the key poll keeps the select
                // arms fixed, which an optional timer would not.
                if rotate_at.is_some_and(|at| at <= tokio::time::Instant::now()) {
                    rotate_at = None;

                    let notice = CallNotice::RotateKey;
                    let _ = notices.send(WorkerTask::CallNotice(room_id.clone(), notice));
                }
            },
            event = events.recv() => {
                let Some(event) = event else {
                    break;
                };

                match event {
                    RoomEvent::ParticipantConnected(participant) => {
                        // Logged verbatim because this is the string our keys
                        // have to be filed under: a peer whose identity is not
                        // the `{user_id}:{device_id}` we assume is one we can
                        // neither encrypt to nor decrypt from.
                        tracing::info!(
                            identity = %participant.identity().0,
                            "a participant joined the SFU"
                        );

                        // A late joiner cannot decrypt us until they have our
                        // key, and only the worker can send Matrix events.
                        let Some(user_id) = participant_user(&participant.identity().0) else {
                            continue;
                        };

                        let notice = CallNotice::ShareKey(user_id);
                        let _ = notices.send(WorkerTask::CallNotice(room_id.clone(), notice));
                    },
                    RoomEvent::TrackSubscribed { track, publication, participant } => {
                        // Playback itself is LiveKit's: its platform audio
                        // device module mixes every subscribed track into the
                        // system's output device. What that leaves us without
                        // is any way to tell a track we never received from one
                        // that arrived and was played to nobody, so the arrival
                        // is worth saying out loud.
                        let livekit::prelude::RemoteTrack::Audio(audio) = &track else {
                            continue;
                        };

                        // A disabled track is dropped from the mixer before it
                        // ever reaches the speaker. Nothing here disables one,
                        // but the cost of making sure is a single call and the
                        // symptom is indistinguishable from silence.
                        audio.enable();

                        // `encryption` is the field that decides whether this
                        // track is decrypted at all: LiveKit only installs a
                        // frame cryptor when the publication advertises one, so
                        // a remote that reports `None` while sending encrypted
                        // media is played straight into the decoder and comes
                        // out as nothing. That failure is invisible everywhere
                        // else - with no cryptor there is no `E2eeStateChanged`
                        // to warn from either.
                        tracing::info!(
                            identity = %participant.identity().0,
                            sid = %audio.sid(),
                            encryption = ?publication.encryption_type(),
                            muted = audio.is_muted(),
                            enabled = audio.is_enabled(),
                            "subscribed to a remote audio track"
                        );
                    },
                    RoomEvent::TrackSubscriptionFailed { participant, error, track_sid } => {
                        // A track we never subscribed to is silence that looks
                        // exactly like a participant who is not talking.
                        tracing::warn!(
                            identity = %participant.identity().0,
                            sid = %track_sid,
                            "could not subscribe to a remote track: {error}"
                        );
                    },
                    RoomEvent::ParticipantDisconnected(_) => {
                        // Whoever left keeps the key they were given, so our
                        // media stays readable to them until we rotate onto a
                        // new one they never receive. Their own key staying in
                        // our ring is harmless it only decrypts media they
                        // are no longer sending.
                        rotate_at = Some(tokio::time::Instant::now() + KEY_ROTATION_DEBOUNCE);
                    },
                    RoomEvent::ActiveSpeakersChanged { speakers } => {
                        let speakers = speakers
                            .iter()
                            .filter_map(|speaker| participant_user(&speaker.identity().0))
                            .collect();

                        let notice = CallNotice::Speakers(speakers);
                        let _ = notices.send(WorkerTask::CallNotice(room_id.clone(), notice));
                    },
                    RoomEvent::E2eeStateChanged { participant, state } => {
                        // The one place LiveKit says out loud whether the key
                        // exchange actually worked. A call that connects and
                        // stays silent looks identical from every other angle,
                        // so swallowing this leaves nothing to debug with.
                        use livekit::webrtc::native::frame_cryptor::EncryptionState;

                        let identity = participant.identity();

                        match state {
                            EncryptionState::New | EncryptionState::Ok => {
                                tracing::debug!(identity = %identity.0, ?state, "call media encryption is healthy");
                            },
                            _ => {
                                tracing::warn!(
                                    identity = %identity.0,
                                    ?state,
                                    "call media encryption failed; this participant will not be audible"
                                );
                            },
                        }
                    },
                    RoomEvent::Disconnected { reason } => {
                        let notice = CallNotice::Ended(format!("disconnected from the SFU: {reason:?}"));
                        let _ = notices.send(WorkerTask::CallNotice(room_id.clone(), notice));
                        break;
                    },
                    _ => {},
                }
            },
        }
    }
}

/// The Matrix user behind a LiveKit participant identity, if it parses.
///
/// Identities are made by whichever client the participant is running, so a
/// malformed one is a remote input rather than a bug on our side: log it and
/// carry on rather than treating the call as broken.
fn participant_user(identity: &str) -> Option<OwnedUserId> {
    let Some((user_id, _)) = livekit_session::split_identity(identity) else {
        tracing::warn!(%identity, "call participant has a malformed identity");
        return None;
    };

    match <OwnedUserId as std::str::FromStr>::from_str(user_id) {
        Ok(user_id) => Some(user_id),
        Err(_) => {
            tracing::warn!(%identity, "call participant has a malformed user id");
            None
        },
    }
}

impl Drop for CallSession {
    fn drop(&mut self) {
        // Signal the thread to exit, then wait for it to finish. The thread
        // stops as soon as it observes the message, so this does not block.
        let _ = self.control.send(CallControl::Shutdown);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

/// Tracks the currently active call, if any.
///
/// Stored on the worker so that call commands (`:call`, `:call hangup`,
/// `:call mute`) and inbound signaling events operate on shared state.
#[derive(Default)]
pub struct CallManager {
    /// The active call session, or `None` when not in a call.
    pub session: Option<CallSession>,

    /// E2EE keys received from other participants over to-device events.
    ///
    /// Shared with the sync event handler, which pushes keys in as they arrive.
    pub inbox: KeyInbox,
}

impl CallManager {
    /// Create an empty [`CallManager`] with no active call.
    pub fn new() -> Self {
        CallManager::default()
    }

    /// Whether the user is currently in a call.
    pub fn is_active(&self) -> bool {
        self.session.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use matrix_sdk::ruma::room_id;

    #[test]
    fn test_encryption_keys_to_device_shape_matches_element_call() {
        // Verbatim from matrix-js-sdk's `EncryptionKeysToDeviceEventContent`.
        // The room event of the same type carries a `keys` *array* and names the
        // device at the top level; modelling the to-device event that way parses
        // nothing and produces a call in which nobody can decrypt anybody.
        let json = serde_json::json!({
            "keys": { "index": 3, "key": "YWJjZGVmZ2hpamtsbW5vcA" },
            "member": { "id": "@alice:example.org", "claimed_device_id": "ABCDEFGH" },
            "room_id": "!room:example.org",
            "session": { "application": "m.call", "call_id": "", "scope": "m.room" },
            "sent_ts": 1_700_000_000_000_i64,
        });

        let content: CallEncryptionKeysEventContent = serde_json::from_value(json).unwrap();

        assert_eq!(content.keys.index, 3);
        assert_eq!(content.keys.key, "YWJjZGVmZ2hpamtsbW5vcA");
        assert_eq!(content.member.id, "@alice:example.org");
        assert_eq!(content.member.claimed_device_id, "ABCDEFGH");
        assert_eq!(content.session.application, "m.call");
        assert_eq!(content.session.scope, "m.room");
    }

    #[test]
    fn test_redacting_a_logged_key_keeps_the_shape_and_drops_the_secret() {
        // The payload is logged to diagnose a sender's field layout, so the
        // layout has to survive and the key material must not: a call key
        // written to a file on disk undoes the encryption it exists for.
        let json = r#"{"type":"io.element.call.encryption_keys","content":{
            "keys":{"index":3,"key":"c3VwZXJzZWNyZXQ"},
            "member":{"id":"@a:b.org","claimed_device_id":"DEV"},
            "room_id":"!r:b.org"}}"#;

        let redacted = crate::worker::redact_call_key(json);

        assert!(!redacted.contains("c3VwZXJzZWNyZXQ"), "the key material must not survive");
        assert!(redacted.contains("<redacted>"));
        assert!(redacted.contains("claimed_device_id"), "the shape must survive");
        assert!(redacted.contains(r#""index":3"#), "the index is not secret and locates the key");
    }

    #[test]
    fn test_redacting_a_key_array_covers_every_entry() {
        // The room transport carries an array, so a redactor that only knows
        // the to-device shape would leak every key it was meant to protect.
        let json = r#"{"content":{"keys":[
            {"index":0,"key":"Zmlyc3RzZWNyZXQ"},
            {"index":1,"key":"c2Vjb25kc2VjcmV0"}]}}"#;

        let redacted = crate::worker::redact_call_key(json);

        assert!(!redacted.contains("Zmlyc3RzZWNyZXQ"));
        assert!(!redacted.contains("c2Vjb25kc2VjcmV0"));
    }

    #[test]
    fn test_redacting_something_that_is_not_json() {
        // Non-JSON cannot be redacted, so it must not be passed through: that
        // is precisely the input whose contents are unknown.
        assert_eq!(crate::worker::redact_call_key("not json at all"), "<unparseable>");
    }

    #[test]
    fn test_encryption_keys_survive_an_unparseable_identifier() {
        // A peer that writes an identifier ruma will not accept must not cost
        // us the key that came with it. Typing these fields as `OwnedUserId`
        // and `OwnedRoomId` made the whole event fail to deserialize, so the
        // SDK dropped it before any handler ran and the participant stayed
        // silent for the entire call with `MissingKey` as the only clue.
        let json = serde_json::json!({
            "keys": { "index": 0, "key": "YWJjZGVmZ2hpamtsbW5vcA" },
            "member": { "id": "alice:example.org", "claimed_device_id": "ABCDEFGH" },
            "room_id": "room:example.org",
            "session": { "application": "m.call", "call_id": "", "scope": "m.room" },
        });

        let content: CallEncryptionKeysEventContent = serde_json::from_value(json).unwrap();

        assert_eq!(content.member.id, "alice:example.org");
        assert_eq!(content.room_id, "room:example.org");
        assert_eq!(content.keys.key, "YWJjZGVmZ2hpamtsbW5vcA");
    }

    #[test]
    fn test_encryption_keys_round_trip_to_the_same_shape() {
        // What we send has to be what the other end parses, so serializing must
        // land back on the same field layout we accept.
        let content = CallEncryptionKeysEventContent {
            keys: CallEncryptionKey { index: 0, key: "AAAA".to_owned() },
            member: CallKeyMember {
                id: "@bob:example.org".to_owned(),
                claimed_device_id: matrix_sdk::ruma::device_id!("DEVICE").to_owned(),
            },
            room_id: "!room:example.org".to_owned(),
            session: CallKeySession::room_call(),
            sent_ts: None,
        };

        let json = serde_json::to_value(&content).unwrap();

        assert!(json["keys"].is_object(), "keys must be one object, not an array");
        assert_eq!(json["member"]["claimed_device_id"], "DEVICE");
        assert_eq!(json["session"]["call_id"], "");
        assert!(json.get("sent_ts").is_none(), "an absent timestamp must be omitted");

        serde_json::from_value::<CallEncryptionKeysEventContent>(json).unwrap();
    }

    /// The room transport's shape is *not* the to-device one, and a client that
    /// confuses them exchanges keys that never parse.
    #[test]
    fn test_encryption_keys_room_event_shape_matches_element_call() {
        // Verbatim from matrix-js-sdk's `EncryptionKeysEventContent`: a `keys`
        // array, the device at the top level, no `room_id`, no `session`.
        let json = serde_json::json!({
            "keys": [{ "index": 1, "key": "YWJjZGVmZ2hpamtsbW5vcA" }],
            "device_id": "ABCDEFGH",
            "call_id": "",
            "sent_ts": 1_700_000_000_000_i64,
        });

        let content: CallEncryptionKeysRoomEventContent = serde_json::from_value(json).unwrap();

        assert_eq!(content.keys.len(), 1);
        assert_eq!(content.keys[0].index, 1);
        assert_eq!(content.keys[0].key, "YWJjZGVmZ2hpamtsbW5vcA");
        assert_eq!(content.device_id, "ABCDEFGH");
        assert_eq!(content.call_id, "");
    }

    #[test]
    fn test_encryption_keys_room_event_round_trips_to_the_same_shape() {
        let content = CallEncryptionKeysRoomEventContent {
            keys: vec![CallEncryptionKey { index: 0, key: "AAAA".to_owned() }],
            device_id: matrix_sdk::ruma::device_id!("DEVICE").to_owned(),
            call_id: String::new(),
            sent_ts: None,
        };

        let json = serde_json::to_value(&content).unwrap();

        assert!(json["keys"].is_array(), "keys must be an array, not one object");
        assert_eq!(json["device_id"], "DEVICE");
        assert!(json.get("sent_ts").is_none(), "an absent timestamp must be omitted");
        assert!(json.get("member").is_none(), "the room event names no member");

        serde_json::from_value::<CallEncryptionKeysRoomEventContent>(json).unwrap();
    }

    /// Both transports carry the same `io.element.call.encryption_keys` type and
    /// differ only in kind, so the two content types must stay distinguishable
    /// by shape alone.
    #[test]
    fn test_the_two_key_transports_do_not_parse_each_other() {
        let to_device = serde_json::json!({
            "keys": { "index": 0, "key": "AAAA" },
            "member": { "id": "@alice:example.org", "claimed_device_id": "DEVICE" },
            "room_id": "!room:example.org",
            "session": { "application": "m.call", "call_id": "", "scope": "m.room" },
        });

        let room = serde_json::json!({
            "keys": [{ "index": 0, "key": "AAAA" }],
            "device_id": "DEVICE",
            "call_id": "",
        });

        assert!(serde_json::from_value::<CallEncryptionKeysRoomEventContent>(to_device).is_err());
        assert!(serde_json::from_value::<CallEncryptionKeysEventContent>(room).is_err());
    }

    #[test]
    fn test_call_status_is_shared_between_clones() {
        // The UI's handle and the worker's handle must be the same state, or the
        // two drift apart the moment a call ends without the UI asking it to.
        let worker = CallStatus::default();
        let ui = worker.clone();

        assert_eq!(ui.get(), None);

        worker.joined(room_id!("!room:example.com").to_owned());
        assert_eq!(ui.get().unwrap().room_id, room_id!("!room:example.com"));
        assert!(!ui.get().unwrap().muted);

        // Joining is not the same as being connected: the SFU handshake is
        // still in flight on the call thread.
        assert!(!ui.get().unwrap().connected);
        worker.connected();
        assert!(ui.get().unwrap().connected);

        worker.set_muted(true);
        assert!(ui.get().unwrap().muted);

        worker.left();
        assert_eq!(ui.get(), None);
    }

    #[test]
    fn test_muting_without_a_call_does_nothing() {
        let status = CallStatus::default();

        status.set_muted(true);

        assert_eq!(status.get(), None);
    }

    #[test]
    fn test_call_status_tracks_speakers() {
        let status = CallStatus::default();
        let alice = matrix_sdk::ruma::user_id!("@alice:example.com").to_owned();

        status.joined(room_id!("!room:example.com").to_owned());
        assert!(status.get().unwrap().speakers.is_empty());

        status.set_speakers(vec![alice.clone()]);
        assert_eq!(status.get().unwrap().speakers, vec![alice]);

        // Speakers belong to the call, not the client, so they go with it.
        status.left();
        status.joined(room_id!("!room:example.com").to_owned());
        assert!(status.get().unwrap().speakers.is_empty());
    }

    /// The inbox is fed by remote peers and only drained by a running call, so
    /// without a cap it grows for as long as the client is open.
    #[test]
    fn test_key_inbox_is_bounded() {
        let inbox = KeyInbox::default();

        let key = |index: u8| {
            ReceivedCallKey {
                room_id: room_id!("!room:example.com").to_owned(),
                user_id: matrix_sdk::ruma::user_id!("@alice:example.com").to_owned(),
                device_id: matrix_sdk::ruma::device_id!("DEVICE").to_owned(),
                index,
                key: vec![index],
            }
        };

        for i in 0..(KEY_INBOX_LIMIT + 50) {
            inbox.push(key((i % 256) as u8));
        }

        assert_eq!(inbox.len(), KEY_INBOX_LIMIT);

        // The oldest are the ones dropped: a newer key for a given slot
        // supersedes the one it replaces, so the tail is what still matters.
        let drained = inbox.drain();
        assert_eq!(drained.len(), KEY_INBOX_LIMIT);
        assert_eq!(drained.last().unwrap().index, ((KEY_INBOX_LIMIT + 49) % 256) as u8);
        assert_eq!(inbox.len(), 0);
    }

    #[test]
    fn test_key_index_wraps_around_the_ring() {
        use livekit_session::{FIRST_KEY_INDEX, next_key_index};

        let mut index = FIRST_KEY_INDEX;
        let mut seen = vec![index];

        for _ in 0..15 {
            index = next_key_index(index);
            seen.push(index);
        }

        // Sixteen distinct slots, then back to the start.
        seen.sort_unstable();
        seen.dedup();
        assert_eq!(seen.len(), 16);
        assert_eq!(next_key_index(index), FIRST_KEY_INDEX);
    }

    /// A ring is a request with a deadline, not an invite that stands until
    /// answered: a client syncing a backlog must not ring for calls that are
    /// long over.
    #[test]
    fn test_incoming_call_expires() {
        use std::time::{Duration, SystemTime};

        let call = |at: SystemTime| {
            IncomingCall {
                notification: matrix_sdk::ruma::owned_event_id!("$ring:example.org"),
                from: matrix_sdk::ruma::user_id!("@alice:example.org").to_owned(),
                expires_at: MilliSecondsSinceUnixEpoch::from_system_time(at).unwrap(),
                ring: true,
            }
        };

        assert!(call(SystemTime::now() + Duration::from_secs(30)).is_live());
        assert!(!call(SystemTime::now() - Duration::from_secs(1)).is_live());
    }

    /// Mentions decide who a ring is aimed at. Getting this wrong either rings a
    /// whole room for a two-person call or silences a call meant for us.
    #[test]
    fn test_ring_targeting_follows_mentions() {
        let us = matrix_sdk::ruma::user_id!("@us:example.org");
        let them = matrix_sdk::ruma::user_id!("@them:example.org").to_owned();

        assert!(ring_is_for_us(Some(&Mentions::with_room_mention()), us));
        assert!(ring_is_for_us(Some(&Mentions::with_user_ids([us.to_owned()])), us));

        // Aimed at somebody else in a room we happen to be in.
        assert!(!ring_is_for_us(Some(&Mentions::with_user_ids([them])), us));

        // Addressed to nobody at all: the absence of a target must not read as
        // the widest possible target.
        assert!(!ring_is_for_us(Some(&Mentions::new()), us));
        assert!(!ring_is_for_us(None, us));
    }

    #[test]
    fn test_leaving_twice_is_harmless() {
        let status = CallStatus::default();

        status.joined(room_id!("!room:example.com").to_owned());
        status.left();
        status.left();

        assert_eq!(status.get(), None);
    }
}
