//! LiveKit media session for VoIP calls.
//!
//! This owns the connection to a LiveKit room (the SFU) for a single call: the
//! signalling connection, the end-to-end encryption key ring, and the microphone
//! track we publish.
//!
//! Audio I/O is LiveKit's own platform audio device module rather than something
//! we drive by hand. [`PlatformAudio`] captures the microphone and plays every
//! subscribed remote track back through the system's output device, with echo
//! cancellation, noise suppression, and automatic gain control applied by WebRTC.
//! There is no capture loop or playback ring buffer in iamb at all: we hand
//! LiveKit a `RtcAudioSource::Device` and it does the rest.
//!
//! The session runs on the dedicated `iamb-voip` thread (see
//! [`crate::voip::CallSession`]) inside that thread's own tokio runtime, so
//! nothing here ever runs on the runtime that serves the UI.
//!
//! Only compiled when the `voip` feature is enabled.

use anyhow::{Context, Result};
use livekit::PlatformAudio;
use livekit::e2ee::key_provider::{KeyDerivationAlgorithm, KeyProvider, KeyProviderOptions};
use livekit::e2ee::{E2eeOptions, EncryptionType};
use livekit::options::TrackPublishOptions;
use livekit::prelude::*;
use matrix_sdk::ruma::OwnedRoomId;
use tokio::sync::mpsc::UnboundedReceiver;

use super::{KeyInbox, ReceivedCallKey};

/// The key ring slot our own E2EE key starts in.
///
/// Rotation walks forward from here; other clients tell us which of *their*
/// indices each key they send belongs to.
pub const FIRST_KEY_INDEX: u8 = 0;

/// How many slots the key ring has.
///
/// LiveKit's key provider keeps a fixed-size ring per participant, and Element
/// Call's implementation uses 16 slots. Rotating past the end wraps around,
/// which is safe because a call would have to survive sixteen departures before
/// the oldest key is overwritten - long after any media encrypted under it has
/// been played out.
const KEY_RING_SIZE: u8 = 16;

/// The slot a rotation moves to from `index`.
pub fn next_key_index(index: u8) -> u8 {
    (index + 1) % KEY_RING_SIZE
}

/// The name our published microphone track carries in the LiveKit room.
const MIC_TRACK_NAME: &str = "microphone";

/// Parameters needed to establish a LiveKit media session.
///
/// Filled in by the worker after it has discovered the SFU URL and obtained a
/// LiveKit JWT via the `lk-jwt-service` `/sfu/get` exchange.
pub struct SessionConfig {
    /// The LiveKit SFU URL (`wss://…`) discovered from `m.call.member` state.
    pub url: String,

    /// The LiveKit access token (JWT) authorizing this participant.
    pub token: String,

    /// The raw E2EE key shared with other participants for this call.
    pub e2ee_key: Vec<u8>,

    /// Our LiveKit participant identity, `{user_id}:{device_id}`.
    pub identity: String,

    /// Keys received from other participants over Matrix to-device events.
    pub inbox: KeyInbox,

    /// The system audio devices, already set to the user's remembered choices.
    pub audio: PlatformAudio,

    /// The Matrix room this call belongs to.
    ///
    /// Senders address their keys to a room, so this is what tells apart a key
    /// for our call from one for a call we are not in.
    pub room_id: OwnedRoomId,

    /// Whether to encrypt our media and decrypt everyone else's.
    ///
    /// Normally true. When false the room is joined with no E2EE options at
    /// all, which is all or nothing: LiveKit installs no frame cryptor in
    /// *either* direction, so an encrypted peer is inaudible rather than
    /// merely unprotected.
    pub encrypted: bool,
}

/// A running LiveKit media session for one call.
///
/// Dropping the session leaves the room, but [`LiveKitSession::disconnect`] does
/// it deterministically and should be preferred.
pub struct LiveKitSession {
    room: Room,
    key_provider: KeyProvider,
    inbox: KeyInbox,
    room_id: OwnedRoomId,

    /// Our own participant identity, needed to register rotated keys under the
    /// same identity our media is published with.
    identity: ParticipantIdentity,

    /// The platform audio device module.
    ///
    /// Held for the lifetime of the call: LiveKit reference-counts the ADM and
    /// releases the microphone and speaker when the last handle drops.
    _audio: PlatformAudio,

    /// The microphone track we publish, kept so that it can be muted.
    mic: LocalAudioTrack,
}

impl LiveKitSession {
    /// Connect to the LiveKit room described by `config`.
    ///
    /// Returns the session along with the room's event stream, which the call
    /// thread pumps for participant and track events.
    pub async fn connect(config: SessionConfig) -> Result<(Self, UnboundedReceiver<RoomEvent>)> {
        let audio = config.audio;

        let options = KeyProviderOptions {
            key_derivation_algorithm: KeyDerivationAlgorithm::HKDF,
            ..Default::default()
        };

        let key_provider = KeyProvider::new(options);

        // Per-participant mode: every participant's media is encrypted with
        // their own key, so we register ours under our own identity and add
        // everyone else's as their to-device events arrive.
        //
        // Registered against the identity we *expect* first so that there is no
        // window in which we are connected without a key; the identity the SFU
        // actually assigned is only readable once the connection is up, and is
        // reconciled below.
        let expected = ParticipantIdentity(config.identity.clone());
        key_provider.set_key(&expected, FIRST_KEY_INDEX.into(), config.e2ee_key.clone());

        let e2ee = E2eeOptions {
            encryption_type: EncryptionType::Gcm,
            key_provider: key_provider.clone(),
        };

        let mut options = RoomOptions::default();

        // Leaving this `None` is what actually disables encryption: LiveKit
        // decides per track whether to build a frame cryptor, and with no
        // options it builds none, so our media goes out in the clear and
        // everyone else's arrives undecrypted.
        if config.encrypted {
            options.encryption = Some(e2ee);
        } else {
            tracing::warn!("connecting to the SFU with media encryption disabled");
        }

        let (room, events) = Room::connect(&config.url, &config.token, options)
            .await
            .context("could not connect to the LiveKit SFU")?;

        // The SFU, not us, decides what our participant identity is: it comes
        // out of the JWT the focus's token service minted. We derive the same
        // `{user_id}:{device_id}` that Element Call does, but a token service
        // that disagrees would leave our outgoing media encrypted under an
        // identity the frame cryptor never looks up - silence in both
        // directions, with every key exchange apparently succeeding.
        let identity = room.local_participant().identity();

        if identity != expected {
            tracing::warn!(
                expected = %expected.0,
                actual = %identity.0,
                "the SFU assigned a different participant identity than we derived"
            );

            key_provider.set_key(&identity, FIRST_KEY_INDEX.into(), config.e2ee_key.clone());
        }

        let mic = LocalAudioTrack::create_audio_track(MIC_TRACK_NAME, audio.rtc_source());
        let publish = TrackPublishOptions {
            source: TrackSource::Microphone,
            ..Default::default()
        };

        room.local_participant()
            .publish_track(LocalTrack::Audio(mic.clone()), publish)
            .await
            .context("could not publish the microphone track")?;

        let session = LiveKitSession {
            room,
            key_provider,
            inbox: config.inbox,
            room_id: config.room_id,
            identity,
            _audio: audio,
            mic,
        };

        // Keys can arrive before the connection finishes, so apply whatever the
        // sync handler buffered while we were connecting.
        session.drain_inbox();

        Ok((session, events))
    }

    /// Hand every key buffered by the Matrix sync handler to LiveKit.
    ///
    /// Called on a timer by the call thread; keys for participants that have not
    /// joined the SFU yet are harmless, the key ring simply holds them until
    /// their media shows up.
    pub fn drain_inbox(&self) {
        for key in self.inbox.drain() {
            if key.room_id == self.room_id {
                self.apply_key(&key);
            }
        }
    }

    /// Register one participant's key with the key provider.
    fn apply_key(&self, key: &ReceivedCallKey) {
        let identity = ParticipantIdentity(key.participant_identity());

        if !self.key_provider.set_key(&identity, key.index.into(), key.key.clone()) {
            tracing::warn!(identity = %identity.0, index = key.index, "LiveKit rejected a call key");
            return;
        }

        // A key filed under an identity nobody in the room is using decrypts
        // nothing, and is indistinguishable from having received no key at all
        // unless it is said out loud.
        let known = self
            .room
            .remote_participants()
            .values()
            .any(|participant| participant.identity() == identity);

        if known {
            tracing::debug!(identity = %identity.0, index = key.index, "applied a call key");
        } else {
            // Naming the identities actually on the SFU is what separates the
            // two ways this happens: a key that simply arrived before its
            // sender connected, which resolves itself, and a key filed under a
            // name that peer never uses, which never decrypts anything. The
            // second is invisible otherwise - the key exchange looks entirely
            // successful and the participant is silent for the whole call.
            let present = self
                .room
                .remote_participants()
                .values()
                .map(|participant| participant.identity().0)
                .collect::<Vec<_>>();

            tracing::info!(
                identity = %identity.0,
                index = key.index,
                on_sfu = ?present,
                "applied a call key for a participant not (yet) on the SFU"
            );
        }
    }

    /// Start encrypting our media with `key` at slot `index`.
    ///
    /// Registered under our own identity, which is what the E2EE frame
    /// cryptor consults for outgoing media. Remote participants have already
    /// been told about the new key by the time this arrives, so there is no
    /// window in which we publish under a key nobody holds.
    pub fn set_our_key(&self, index: u8, key: &[u8]) {
        if !self.key_provider.set_key(&self.identity, index.into(), key.to_vec()) {
            tracing::warn!(index, "LiveKit rejected our rotated call key");
            return;
        }

        // Registering the key is only half of a rotation. livekit-rust builds
        // the sending frame cryptor without ever calling `set_key_index`, so it
        // stays on the slot it was created with - index 0 - no matter what the
        // key provider is told. Without this the rotation is invisible to our
        // own encoder: we keep publishing under the superseded key while every
        // peer has moved on, and they stop hearing us.
        for ((identity, _), cryptor) in self.room.e2ee_manager().frame_cryptors() {
            if identity == self.identity {
                cryptor.set_key_index(index.into());
            }
        }
    }

    /// Mute or unmute the local microphone.
    ///
    /// Muting the track rather than stopping capture keeps the publication alive
    /// and tells the SFU, so other participants see us as muted instead of
    /// simply going silent.
    pub fn set_muted(&self, muted: bool) {
        if muted {
            self.mic.mute();
        } else {
            self.mic.unmute();
        }
    }

    /// Leave the LiveKit room.
    pub async fn disconnect(&self) -> Result<()> {
        self.room
            .close()
            .await
            .context("could not cleanly leave the LiveKit room")?;

        Ok(())
    }
}

/// Split a LiveKit participant identity back into its Matrix user and device.
///
/// Identities are `{user_id}:{device_id}` and user IDs themselves contain a
/// colon, so the split has to happen at the last one.
pub fn split_identity(identity: &str) -> Option<(&str, &str)> {
    identity.rsplit_once(':')
}
