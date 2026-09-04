//! MatrixRTC signaling for VoIP calls.
//!
//! Everything in this module talks to the homeserver rather than to LiveKit: it
//! discovers which SFU a room's call uses, trades a Matrix OpenID token for a
//! LiveKit JWT, publishes and retracts our `m.call.member` state event, and
//! distributes our end-to-end encryption key over to-device messages.
//!
//! Only compiled when the `voip` feature is enabled.

use std::time::{Duration, SystemTime};

use anyhow::{Context, Result, anyhow};
use matrix_sdk::encryption::identities::Device;
// The SDK's own reqwest, not iamb's direct dependency: the graph carries two
// majors, and this has to be the one the shared client was built with.
use matrix_sdk::Client;
use matrix_sdk::reqwest::{self, header::CONTENT_TYPE};
use matrix_sdk::room::Room as MatrixRoom;
use matrix_sdk::ruma::api::client::account::request_openid_token;
use matrix_sdk::ruma::events::call::member::{
    ActiveFocus,
    ActiveLivekitFocus,
    Application,
    CallApplicationContent,
    CallMemberEventContent,
    CallMemberStateKey,
    CallScope,
    Focus,
    LivekitFocus,
};
use matrix_sdk::ruma::events::relation::Reference;
use matrix_sdk::ruma::events::rtc::decline::RtcDeclineEventContent;
use matrix_sdk::ruma::events::rtc::notification::{
    CallIntent,
    NotificationType,
    RtcNotificationEventContent,
};
use matrix_sdk::ruma::events::{AnyToDeviceEventContent, Mentions, StaticEventContent};
use matrix_sdk::ruma::serde::Raw;
use matrix_sdk::ruma::{
    DeviceId,
    EventId,
    MilliSecondsSinceUnixEpoch,
    OwnedEventId,
    OwnedUserId,
    UserId,
};
use matrix_sdk_crypto::CollectStrategy;
use serde::{Deserialize, Serialize};
use serde_json::value::to_raw_value;

use super::{
    CallEncryptionKey,
    CallEncryptionKeysEventContent,
    CallEncryptionKeysRoomEventContent,
    CallKeyMember,
    CallKeySession,
};

/// The `.well-known/matrix/client` key listing a homeserver's advertised
/// MatrixRTC foci ([MSC4143]).
///
/// [MSC4143]: https://github.com/matrix-org/matrix-spec-proposals/pull/4143
const WELL_KNOWN_FOCI: &str = "org.matrix.msc4143.rtc_foci";

/// Everything needed to open the LiveKit connection for a call.
#[derive(Clone, Debug)]
pub struct SfuCredentials {
    /// The SFU's WebSocket URL (`wss://…`), as returned by the JWT service.
    pub url: String,

    /// The LiveKit access token authorizing this participant.
    pub jwt: String,
}

/// The response body of the `lk-jwt-service` `/sfu/get` endpoint.
#[derive(Deserialize)]
struct SfuGetResponse {
    url: String,
    jwt: String,
}

/// The OpenID token as `lk-jwt-service` expects to receive it.
#[derive(Serialize)]
struct OpenIdTokenBody {
    access_token: String,
    token_type: String,
    matrix_server_name: String,
}

/// The request body of the `lk-jwt-service` `/sfu/get` endpoint.
#[derive(Serialize)]
struct SfuGetRequest {
    room: String,
    openid_token: OpenIdTokenBody,
    device_id: String,
}

/// The state key our own `m.call.member` event is published under.
///
///
/// MatrixRTC gives every device its own membership event, keyed by user and
/// device, so several devices of the same user can be in a call at once.
///
/// The key takes the leading underscore form (`_{user_id}_{device_id}`,
/// [MSC3757]) rather than the bare `{user_id}_{device_id}`. A state key that
/// starts with `@` may only be set by the user it names, so the bare form
/// which starts with `@` but has a device suffix, is rejected by the
/// homeserver as an attempt to set another user's state, even in one's own
/// room. The leading underscore sidesteps that auth rule while still letting
/// each device own its own membership.
///
/// [MSC3757]: https://github.com/matrix-org/matrix-spec-proposals/pull/3757
pub fn membership_state_key(user_id: &UserId, device_id: &DeviceId) -> CallMemberStateKey {
    CallMemberStateKey::new(user_id.to_owned(), Some(device_id.to_string()), true)
}

/// The LiveKit participant identity we connect to the SFU with.
///
/// Must match what other clients derive for us, since E2EE keys are looked up
/// per participant identity.
pub fn participant_identity(user_id: &UserId, device_id: &DeviceId) -> String {
    format!("{user_id}:{device_id}")
}

/// Find the LiveKit focus to use for the call in `room`.
///
/// Prefers a focus already advertised by a participant, so that a late joiner
/// lands on the same SFU *and* the same LiveKit room as everyone else. The
/// focus carries the LiveKit room alias, so adopting it wholesale is what keeps
/// us in the same conversation. When nobody is in the call yet, falls back to
/// the homeserver's advertised foci, naming the LiveKit room after the Matrix
/// room as Element Call does.
pub async fn discover_focus(
    client: &Client,
    http: &reqwest::Client,
    room: &MatrixRoom,
) -> Result<LivekitFocus> {
    if let Some(focus) = focus_from_memberships(room).await {
        return Ok(focus);
    }

    let service_url = service_url_from_well_known(client, http)
        .await
        .context("no LiveKit focus advertised by the room or the homeserver")?;

    Ok(LivekitFocus::new(room.room_id().to_string(), service_url))
}

/// Pull the LiveKit focus of the *oldest* membership out of the room's existing
/// `m.call.member` state events.
///
/// Which membership wins is not arbitrary. Every participant, ours included,
/// advertises `focus_selection: "oldest_membership"` in its `ActiveFocus`, which
/// is a promise about how the active focus is chosen. Taking whichever focus the
/// state store happened to hand back first breaks that promise the moment two
/// participants prefer different focus: a federated call where each side's
/// homeserver advertises its own SFU is exactly that case, and lands us alone
/// in a LiveKit room while everyone else talks in another.
async fn focus_from_memberships(room: &MatrixRoom) -> Option<LivekitFocus> {
    let events = room.get_state_events_static::<CallMemberEventContent>().await.ok()?;

    let mut oldest: Option<(MilliSecondsSinceUnixEpoch, LivekitFocus)> = None;

    for event in events {
        let Ok(event) = event.deserialize() else {
            continue;
        };

        let Some(event) = event.as_sync().and_then(|ev| ev.as_original()) else {
            continue;
        };

        // A membership published by a spec-conforming client omits `created_ts`
        // on its initial join and lets the server stamp it, so the fallback is
        // load bearing: without it those memberships have no age to compare and
        // `active_memberships` cannot tell whether they have expired.
        let origin = Some(event.origin_server_ts);

        for membership in event.content.active_memberships(origin) {
            let Some(created_ts) = membership.created_ts().or(origin) else {
                continue;
            };

            for focus in membership.foci_preferred() {
                let Focus::Livekit(focus) = focus else {
                    continue;
                };

                if oldest.as_ref().is_none_or(|(seen, _)| created_ts < *seen) {
                    oldest = Some((created_ts, focus.clone()));
                }
            }
        }
    }

    oldest.map(|(_, focus)| focus)
}

/// Read the homeserver's `.well-known/matrix/client` and return the first
/// advertised LiveKit focus's JWT service URL.
///
/// The advertised entry carries only the service URL, not a `livekit_alias`:
/// the LiveKit room is per-call and named by the client, so the caller supplies
/// the alias itself. Deserializing into a full [`LivekitFocus`] would therefore
/// always fail here, its `alias` field is required, which is why this pulls
/// the URL out of the raw JSON instead.
async fn service_url_from_well_known(client: &Client, http: &reqwest::Client) -> Result<String> {
    // `.well-known/matrix/client` lives on the *server name* domain, not on the
    // homeserver base URL, the whole point of the file is to point at the
    // latter from the former. Asking the homeserver for it 404s on every
    // deployment that uses delegation, which is most of the large ones
    // (`matrix.org` delegates to `matrix-client.matrix.org`).
    //
    // `Client::server` is that domain when the client was built by discovery,
    // and `None` when it was pointed straight at a homeserver URL, in which
    // case the two are the same thing.
    let base = client.server().cloned().unwrap_or_else(|| client.homeserver());
    let url = base.join(".well-known/matrix/client")?;
    // Decoded by hand rather than with `Response::json`: the SDK's reqwest is
    // built without its `json` feature, and taking the body as text is all that
    // convenience wraps anyway.
    let body = http.get(url).send().await?.error_for_status()?.text().await?;
    let body: serde_json::Value = serde_json::from_str(&body)?;

    let foci = body
        .get(WELL_KNOWN_FOCI)
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| anyhow!("homeserver advertises no {WELL_KNOWN_FOCI}"))?;

    for focus in foci {
        let is_livekit = focus.get("type").and_then(serde_json::Value::as_str) == Some("livekit");
        let service_url = focus.get("livekit_service_url").and_then(serde_json::Value::as_str);

        if let (true, Some(service_url)) = (is_livekit, service_url) {
            return Ok(service_url.to_owned());
        }
    }

    Err(anyhow!("homeserver advertises no LiveKit focus"))
}

/// Trade a Matrix OpenID token for a LiveKit JWT at the focus's JWT service.
///
/// The service (`lk-jwt-service`) verifies the OpenID token against our
/// homeserver and answers with the SFU URL and an access token scoped to the
/// LiveKit room.
pub async fn request_sfu_credentials(
    client: &Client,
    http: &reqwest::Client,
    focus: &LivekitFocus,
    device_id: &DeviceId,
) -> Result<SfuCredentials> {
    let user_id = client.user_id().ok_or_else(|| anyhow!("not logged in"))?;
    let token = client
        .send(request_openid_token::v3::Request::new(user_id.to_owned()))
        .await?;

    let request = SfuGetRequest {
        // The focus's own alias names the LiveKit room. Deriving it ourselves
        // would put us in a different room from anyone who joined via a focus
        // that names it differently.
        room: focus.alias.clone(),
        openid_token: OpenIdTokenBody {
            access_token: token.access_token,
            token_type: token.token_type.to_string(),
            matrix_server_name: token.matrix_server_name.to_string(),
        },
        device_id: device_id.to_string(),
    };

    let endpoint = format!("{}/sfu/get", focus.service_url.trim_end_matches('/'));
    let response = http
        .post(&endpoint)
        .header(CONTENT_TYPE, "application/json")
        .body(serde_json::to_vec(&request)?)
        .send()
        .await
        .with_context(|| format!("could not reach the LiveKit JWT service at {endpoint}"))?;

    // Keep the response body: on a rejection it carries the reason (e.g. why the
    // OpenID token or room was refused), which `error_for_status` would discard.
    let status = response.status();
    let body = response.text().await.unwrap_or_default();

    if !status.is_success() {
        let reason = body.trim();
        let reason = if reason.is_empty() {
            "(no details)"
        } else {
            reason
        };

        return Err(anyhow!(
            "the LiveKit JWT service at {endpoint} rejected our request ({status}): {reason}"
        ));
    }

    let response: SfuGetResponse =
        serde_json::from_str(&body).context("malformed response from the LiveKit JWT service")?;

    Ok(SfuCredentials { url: response.url, jwt: response.jwt })
}

/// How far ahead of *now* a published membership's expiry is set.
///
/// A membership outlives the client that published it: if we die without
/// retracting, this is how long we haunt the room's participant list. Shorter is
/// tidier, but a call whose refreshes are all failing drops out of everyone
/// else's view once it elapses, so it also has to be comfortably longer than
/// [`MEMBERSHIP_REFRESH_INTERVAL`] - here, four missed refreshes.
pub const MEMBERSHIP_LIFETIME: Duration = Duration::from_secs(2 * 60 * 60);

/// How often a running call republishes its membership to push the expiry out.
pub const MEMBERSHIP_REFRESH_INTERVAL: Duration = Duration::from_secs(30 * 60);

/// The `expires` value a membership created at `created_ts` needs in order to
/// stay valid for [`MEMBERSHIP_LIFETIME`] from now.
///
/// MatrixRTC expiry is `created_ts + expires`, **not** `origin_server_ts +
/// expires`. Resending a membership that copies its original `created_ts`
/// forward therefore does not extend anything. The refresh has to grow
/// `expires` by however long the call has been running.
fn membership_expires(created_ts: MilliSecondsSinceUnixEpoch) -> Duration {
    let elapsed = created_ts
        .to_system_time()
        .and_then(|created| SystemTime::now().duration_since(created).ok())
        .unwrap_or_default();

    elapsed + MEMBERSHIP_LIFETIME
}

/// Announce that this device is in the room's call.
///
/// Used both to join and to refresh. `created_ts` names the instant the
/// membership chain began: `None` on the initial join, which is what MSC3401
/// requires - the server then stamps the event and `origin_server_ts` becomes
/// the creation time. A refresh passes the value read back by
/// [`our_membership_created_ts`] so the session keeps its original start while
/// its expiry moves forward.
///
/// Stamping `created_ts` ourselves on the initial join would put our own clock
/// in charge of when every other participant thinks we joined. A clock running
/// fast keeps us in the participant list past our welcome; one running slow
/// publishes a membership that everybody else considers already expired, so we
/// are connected to the SFU and invisible in the room.
///
/// Returns the event ID of the published membership, which the call notification
/// ([`send_call_notification`]) references so that receivers can tie the ring
/// back to the session it announces.
pub async fn publish_membership(
    room: &MatrixRoom,
    user_id: &UserId,
    device_id: &DeviceId,
    focus: &LivekitFocus,
    created_ts: Option<MilliSecondsSinceUnixEpoch>,
) -> Result<OwnedEventId> {
    let application =
        Application::Call(CallApplicationContent::new(String::new(), CallScope::Room));

    // Expiry runs from `created_ts`, so a refresh has to grow `expires` by
    // however long the call has already been running; merely resending the same
    // value renews nothing. A fresh chain starts the clock now.
    let expires = match created_ts {
        Some(created_ts) => membership_expires(created_ts),
        None => MEMBERSHIP_LIFETIME,
    };

    let content = CallMemberEventContent::new(
        application,
        device_id.to_owned(),
        ActiveFocus::Livekit(ActiveLivekitFocus::new()),
        vec![Focus::Livekit(focus.clone())],
        created_ts,
        Some(expires),
    );

    let response = room
        .send_state_event_for_key(&membership_state_key(user_id, device_id), content)
        .await?;

    Ok(response.event_id)
}

/// When our own membership chain for this room's call began.
///
/// Read back from room state rather than remembered from the join, because the
/// initial join deliberately leaves `created_ts` unset for the server to stamp
/// (see [`publish_membership`]). Falls back to `origin_server_ts`, which is
/// exactly what the server stamped it with.
///
/// `None` means our membership has not synced back yet, in which case the caller
/// should start a fresh chain rather than guess.
pub async fn our_membership_created_ts(
    room: &MatrixRoom,
    user_id: &UserId,
    device_id: &DeviceId,
) -> Option<MilliSecondsSinceUnixEpoch> {
    let event = room
        .get_state_event_static_for_key::<CallMemberEventContent, _>(&membership_state_key(
            user_id, device_id,
        ))
        .await
        .ok()??;

    let event = event.deserialize().ok()?;
    let event = event.as_sync()?.as_original()?;
    let origin = Some(event.origin_server_ts);

    event
        .content
        .active_memberships(origin)
        .first()
        .and_then(|membership| membership.created_ts())
        .or(origin)
}

/// Announce that this device has left the room's call.
///
/// MatrixRTC signals a departure with an empty membership rather than a
/// redaction, so the event stays in the room state as a tombstone.
pub async fn retract_membership(
    room: &MatrixRoom,
    user_id: &UserId,
    device_id: &DeviceId,
) -> Result<()> {
    let content = CallMemberEventContent::new_empty(None);

    room.send_state_event_for_key(&membership_state_key(user_id, device_id), content)
        .await?;

    Ok(())
}

/// Send our E2EE key to every device of every user in `recipients`.
///
/// Called once after joining to seed the participants already in the call, and
/// again whenever a late joiner appears, since LiveKit's per-participant key
/// provider needs our key before it can decrypt anything we publish.
///
/// The key is Olm-encrypted to each recipient device rather than sent in the
/// clear. Sending it unencrypted would hand the homeserver the key to every
/// call it relays, which leaves the media end to end encrypted only against
/// the SFU - the one party that was never going to have the key anyway.
///
/// Recipients that resolve to no devices at all, or that we could not establish
/// an Olm session with, are logged and skipped: excluding one participant is
/// better than aborting distribution for everybody. Failing to reach *every*
/// device is an error, since that is indistinguishable from not having sent the
/// key at all.
pub async fn send_encryption_key(
    client: &Client,
    room: &MatrixRoom,
    device_id: &DeviceId,
    index: u8,
    key: &[u8],
    recipients: impl IntoIterator<Item = OwnedUserId>,
) -> Result<()> {
    use base64::Engine;
    use base64::engine::general_purpose::STANDARD_NO_PAD;

    let user_id = client.user_id().context("not logged in")?;

    let content = CallEncryptionKeysEventContent {
        keys: CallEncryptionKey { index, key: STANDARD_NO_PAD.encode(key) },
        member: CallKeyMember {
            id: user_id.to_string(),
            claimed_device_id: device_id.to_owned(),
        },
        room_id: room.room_id().to_string(),
        session: CallKeySession::room_call(),
        sent_ts: Some(MilliSecondsSinceUnixEpoch::now()),
    };

    let raw = Raw::<AnyToDeviceEventContent>::from_json(to_raw_value(&content)?);

    let mut devices = Vec::new();

    for recipient in recipients {
        // Our own device is in the store too, and Olm has no session with
        // itself; every *other* device of ours is a legitimate recipient, since
        // we may well be in the call from two clients at once.
        let ours = recipient == user_id;
        let found = recipient_devices(client, &recipient).await?;

        let before = devices.len();
        devices.extend(
            found
                .into_iter()
                .filter(|device| !(ours && device.device_id() == device_id)),
        );

        if devices.len() == before {
            tracing::warn!(
                user_id = %recipient,
                "no devices to send our call encryption key to; they will not hear us"
            );
        }
    }

    if devices.is_empty() {
        return Ok(());
    }

    let total = devices.len();
    let failures = client
        .encryption()
        .encrypt_and_send_raw_to_device(
            devices.iter().collect(),
            CallEncryptionKeysEventContent::TYPE,
            raw,
            CollectStrategy::AllDevices,
        )
        .await
        .context("could not encrypt the call encryption key")?;

    if failures.len() == total {
        return Err(anyhow!(
            "could not deliver the call encryption key to any of the {total} devices in the call"
        ));
    }

    for (user_id, device_id) in &failures {
        tracing::warn!(%user_id, %device_id, "could not send our call encryption key to a device");
    }

    Ok(())
}

/// Send our E2EE key to the room as an `io.element.call.encryption_keys` room
/// event.
///
/// The other half of [`send_encryption_key`], and the transport MatrixRTC used
/// before to-device messages. Element Call builds from before the switch listen
/// for nothing else, so a client that only speaks to-device is inaudible to
/// them and cannot hear them either. A call in which the signalling all
/// succeeds and no media is ever decrypted. Sending both is what matrix-js-sdk
/// does while the migration is in flight.
///
/// This does not weaken the to-device path: in an encrypted room the event is
/// megolm-encrypted like any other, and in an unencrypted room the key is no
/// more exposed than the call itself already is. The to-device copy remains the
/// one every current client actually uses.
///
/// Failure is logged by the caller rather than fatal: the to-device copy is the
/// primary transport and has already gone out by the time this runs.
pub async fn send_encryption_key_to_room(
    room: &MatrixRoom,
    device_id: &DeviceId,
    index: u8,
    key: &[u8],
) -> Result<()> {
    use base64::Engine;
    use base64::engine::general_purpose::STANDARD_NO_PAD;

    let content = CallEncryptionKeysRoomEventContent {
        keys: vec![CallEncryptionKey { index, key: STANDARD_NO_PAD.encode(key) }],
        device_id: device_id.to_owned(),
        call_id: String::new(),
        sent_ts: Some(MilliSecondsSinceUnixEpoch::now()),
    };

    room.send(content).await?;

    Ok(())
}

/// The devices of `user_id` that we can encrypt to.
///
/// Device lists are only tracked for users we share an *encrypted* room with,
/// so in an unencrypted room the store can legitimately be empty on the first
/// call. A `/keys/query` populates it; without this the key would silently go
/// nowhere and the call would be audible to no one.
async fn recipient_devices(client: &Client, user_id: &UserId) -> Result<Vec<Device>> {
    let encryption = client.encryption();

    let devices = encryption.get_user_devices(user_id).await?;
    let devices = devices.devices().collect::<Vec<_>>();

    if !devices.is_empty() {
        return Ok(devices);
    }

    encryption
        .request_user_identity(user_id)
        .await
        .with_context(|| format!("could not fetch the device list for {user_id}"))?;

    Ok(encryption.get_user_devices(user_id).await?.devices().collect())
}

/// How long a call notification stays worth ringing for ([MSC4075]).
///
/// A notification is not a call invite that stands until answered: it is a ring
/// with a deadline, and a receiver that comes online after the deadline must not
/// start ringing for it. Thirty seconds matches Element Call, and doubles as the
/// reason a client syncing days of backlog stays quiet
///
/// [MSC4075]: https://github.com/matrix-org/matrix-spec-proposals/pull/4075
pub const NOTIFICATION_LIFETIME: Duration = Duration::from_secs(30);

/// Whether a call in `room` should ring rather than notify quietly.
///
/// Ringing is for calls that are unambiguously *for you*: a direct chat, where
/// there is one other person and they are calling you. In a group room the same
/// treatment would have every member's client ring whenever anybody started a
/// call, so those get a notification the user can ignore.
pub async fn should_ring(room: &MatrixRoom) -> bool {
    room.is_direct().await.unwrap_or(false)
}

/// Announce a newly started call to the room ([MSC4075]).
///
/// This is what makes other clients ring; it is separate from the `m.call.member`
/// state event, which only says who is in the call. A client that publishes
/// membership without this is joinable but silent - nobody is told to pick up.
///
/// Sent by whoever *starts* the call, not by everyone who joins it: a
/// notification per join would ring the room again for every arrival, so the
/// caller is responsible for checking that the call was empty first.
///
/// `membership` is the event ID returned by [`publish_membership`], referenced so
/// that a receiver can tie the ring to the session it belongs to.
///
/// [MSC4075]: https://github.com/matrix-org/matrix-spec-proposals/pull/4075
pub async fn send_call_notification(
    room: &MatrixRoom,
    membership: OwnedEventId,
    ring: bool,
) -> Result<()> {
    let notification_type = if ring {
        NotificationType::Ring
    } else {
        NotificationType::Notification
    };

    let mut content = RtcNotificationEventContent::new(
        MilliSecondsSinceUnixEpoch::now(),
        NOTIFICATION_LIFETIME,
        notification_type,
    );

    // Everyone in the room is a candidate to answer, so the notification is
    // addressed to the room rather than to named users. Without any mention the
    // receiving side has no one to apply it to.
    content.mentions = Some(Mentions::with_room_mention());
    content.relates_to = Some(Reference::new(membership));

    // We publish one microphone track and no camera, so anyone joining us can
    // start with video off.
    content.call_intent = Some(CallIntent::Audio);

    room.send(content).await?;

    Ok(())
}

/// Decline a call we were notified about ([MSC4310]).
///
/// Sent as a reference relation to the `m.rtc.notification` being declined, which
/// is what lets other clients stop ringing on our behalf - in particular our own
/// other devices, so answering or rejecting on one silences the rest.
///
/// [MSC4310]: https://github.com/matrix-org/matrix-spec-proposals/pull/4310
pub async fn send_decline(room: &MatrixRoom, notification: &EventId) -> Result<()> {
    room.send(RtcDeclineEventContent::new(notification.to_owned())).await?;

    Ok(())
}

/// Whether the call in `room` already has participants.
///
/// Decides whether joining is *starting* a call, and so whether to ring the
/// room, or merely joining one that is already ringing or under way.
pub fn call_already_started(room: &MatrixRoom) -> bool {
    room.has_active_room_call()
}

/// The users we distribute our E2EE key to: everyone joined to the room.
///
/// Our own user is included: another of our devices can be in the same call,
/// and it needs our key like anyone else. [`send_encryption_key`] drops only
/// *this* device from the resolved device list.
pub async fn key_recipients(room: &MatrixRoom) -> Result<Vec<OwnedUserId>> {
    Ok(room.joined_user_ids().await?)
}
