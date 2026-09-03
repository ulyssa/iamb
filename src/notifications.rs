use std::time::SystemTime;

use matrix_sdk::{
    Client,
    EncryptionState,
    deserialized_responses::RawAnySyncOrStrippedTimelineEvent,
    notification_settings::{IsEncrypted, IsOneToOne, NotificationSettings, RoomNotificationMode},
    room::Room as MatrixRoom,
    ruma::{
        MilliSecondsSinceUnixEpoch,
        OwnedRoomId,
        RoomId,
        events::{AnyMessageLikeEventContent, AnySyncTimelineEvent, room::message::MessageType},
        serde::Raw,
    },
};
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    base::{AsyncProgramStore, IambError, IambResult, ProgramStore},
    config::{ApplicationSettings, NotifyVia},
};

const IAMB_XDG_NAME: &str = match option_env!("IAMB_XDG_NAME") {
    None => "iamb",
    Some(iamb) => iamb,
};

/// Handle for an open notification that should be closed when the user views it.
pub struct NotificationHandle(
    #[cfg(all(feature = "desktop", unix, not(target_os = "macos")))]
    Option<notify_rust::NotificationHandle>,
);

impl Drop for NotificationHandle {
    fn drop(&mut self) {
        #[cfg(all(feature = "desktop", unix, not(target_os = "macos")))]
        if let Some(handle) = self.0.take() {
            handle.close();
        }
    }
}

pub async fn register_notifications(
    client: &Client,
    settings: &ApplicationSettings,
    store: &AsyncProgramStore,
) {
    if !settings.tunables.notifications.enabled {
        return;
    }
    let notify_via = settings.tunables.notifications.via;
    let show_message = settings.tunables.notifications.show_message;
    let sound_hint = settings.tunables.notifications.sound_hint.clone();
    let server_settings = client.notification_settings().await;
    let Some(startup_ts) = MilliSecondsSinceUnixEpoch::from_system_time(SystemTime::now()) else {
        return;
    };

    let store = store.clone();
    client
        .register_notification_handler(move |notification, room: MatrixRoom, client: Client| {
            let store = store.clone();
            let server_settings = server_settings.clone();
            let sound_hint = sound_hint.clone();
            async move {
                let mode = global_or_room_mode(&server_settings, &room).await;
                if mode == RoomNotificationMode::Mute {
                    return;
                }

                if is_visible_room(&store, room.room_id()).await {
                    return;
                }

                let room_id = room.room_id().to_owned();
                match notification.event {
                    RawAnySyncOrStrippedTimelineEvent::Sync(e) => {
                        match parse_full_notification(e, room, show_message).await {
                            Ok((summary, body, server_ts)) => {
                                if server_ts < startup_ts {
                                    return;
                                }

                                if is_missing_mention(&body, mode, &client) {
                                    return;
                                }

                                send_notification(
                                    &notify_via,
                                    &summary,
                                    body.as_deref(),
                                    room_id,
                                    &store,
                                    sound_hint.as_deref(),
                                )
                                .await;
                            },
                            Err(err) => {
                                tracing::error!("Failed to extract notification data: {err}")
                            },
                        }
                    },
                    // Stripped events may be dropped silently because they're
                    // only relevant if we're not in a room, and we presumably
                    // don't want notifications for rooms we're not in.
                    RawAnySyncOrStrippedTimelineEvent::Stripped(_) => (),
                }
            }
        })
        .await;
}

/// Announce that a call has started in a room the user is not currently looking
/// at.
///
/// Unlike message notifications this deliberately ignores the room's push
/// notification mode: a muted room mutes its *messages*, and someone calling you
/// is not a message. It is still gated on the global notification tunable.
///
/// Takes the already-locked store because the caller is a sync event handler
/// that holds it; locking it again here would deadlock.
#[cfg(feature = "voip")]
pub async fn notify_call_started(room_name: &str, room_id: OwnedRoomId, store: &mut ProgramStore) {
    if !store.application.settings.tunables.notifications.enabled {
        return;
    }

    if is_focused(store) && is_open(store, &room_id) {
        return;
    }

    let summary = format!("📞 Call in {room_name}");

    let _ = room_id;
    send_call_notification(&summary, None, false, store).await;
}

/// Announce that someone is calling and waiting for an answer (MSC4075).
///
/// Unlike [`notify_call_started`] this fires even when the room is open and
/// focused: a ring is a request for an answer, and silently dropping it because
/// the user happens to be looking at the room means the call goes unanswered
/// while they stare at it.
///
/// `ring` marks the notification urgent, which is what stops a compositor from
/// expiring it after a few seconds - a ring the user missed because they were
/// away from the keyboard is a ring that did not work.
#[cfg(feature = "voip")]
pub async fn notify_incoming_call(
    room_name: &str,
    caller: &str,
    room_id: OwnedRoomId,
    ring: bool,
    store: &mut ProgramStore,
) {
    if !store.application.settings.tunables.notifications.enabled {
        return;
    }

    let summary = format!("📞 {caller} is calling");
    let body = format!("in {room_name} — :call to answer, :call decline to reject");

    let _ = room_id;
    send_call_notification(&summary, Some(&body), ring, store).await;
}

/// Deliver a call notification from a caller that already holds the store lock.
///
/// [`send_notification`] takes the unlocked store and locks it itself, which
/// would deadlock the sync event handlers the call notices arrive on.
#[cfg(feature = "voip")]
async fn send_call_notification(
    summary: &str,
    body: Option<&str>,
    urgent: bool,
    store: &mut ProgramStore,
) {
    let via = store.application.settings.tunables.notifications.via;

    #[cfg(feature = "desktop")]
    if via.desktop {
        let mut notification = notify_rust::Notification::new();
        notification
            .summary(summary)
            .appname(IAMB_XDG_NAME)
            .icon(IAMB_XDG_NAME)
            .action("default", "default");

        if let Some(body) = body {
            notification.body(body);
        }

        // A ring stays up until it is dealt with; ordinary notifications expire
        // on the compositor's own schedule.
        #[cfg(all(unix, not(target_os = "macos")))]
        notification.urgency(if urgent {
            notify_rust::Urgency::Critical
        } else {
            notify_rust::Urgency::Normal
        });

        #[cfg(all(unix, not(target_os = "macos")))]
        let res = notification.show_async().await;
        #[cfg(any(not(unix), target_os = "macos"))]
        let res = notification.show();

        if let Err(err) = res {
            tracing::error!("Failed to send call notification: {err}");
        }
    }

    #[cfg(not(feature = "desktop"))]
    let _ = (summary, body, urgent);

    if via.bell {
        store.application.ring_bell = true;
    }
}

async fn send_notification(
    via: &NotifyVia,
    summary: &str,
    body: Option<&str>,
    room_id: OwnedRoomId,
    store: &AsyncProgramStore,
    sound_hint: Option<&str>,
) {
    #[cfg(feature = "desktop")]
    if via.desktop {
        send_notification_desktop(summary, body, room_id, store, sound_hint).await;
    }
    #[cfg(not(feature = "desktop"))]
    {
        let _ = (summary, body, IAMB_XDG_NAME);
    }

    if via.bell {
        send_notification_bell(store).await;
    }
}

async fn send_notification_bell(store: &AsyncProgramStore) {
    let mut locked = store.lock().await;
    locked.application.ring_bell = true;
}

#[cfg(feature = "desktop")]
#[cfg_attr(target_os = "macos", allow(unused_variables))]
async fn send_notification_desktop(
    summary: &str,
    body: Option<&str>,
    room_id: OwnedRoomId,
    _store: &AsyncProgramStore,
    sound_hint: Option<&str>,
) {
    let mut desktop_notification = notify_rust::Notification::new();
    desktop_notification
        .summary(summary)
        .appname(IAMB_XDG_NAME)
        .icon(IAMB_XDG_NAME)
        .action("default", "default");

    if let Some(sound_hint) = sound_hint {
        desktop_notification.sound_name(sound_hint);
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    desktop_notification.urgency(notify_rust::Urgency::Normal);

    if let Some(body) = body {
        desktop_notification.body(body);
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    let res = desktop_notification.show_async().await;
    #[cfg(any(not(unix), target_os = "macos"))]
    let res = desktop_notification.show();

    match res {
        Err(err) => tracing::error!("Failed to send notification: {err}"),
        Ok(handle) => {
            #[cfg(all(unix, not(target_os = "macos")))]
            _store
                .lock()
                .await
                .application
                .open_notifications
                .entry(room_id)
                .or_default()
                .push(NotificationHandle(Some(handle)));
        },
    }
}

async fn global_or_room_mode(
    settings: &NotificationSettings,
    room: &MatrixRoom,
) -> RoomNotificationMode {
    let room_mode = settings.get_user_defined_room_notification_mode(room.room_id()).await;
    if let Some(mode) = room_mode {
        return mode;
    }
    let is_one_to_one = match room.is_direct().await {
        Ok(true) => IsOneToOne::Yes,
        _ => IsOneToOne::No,
    };
    let is_encrypted = match room.latest_encryption_state().await {
        Ok(EncryptionState::Encrypted) => IsEncrypted::Yes,
        _ => IsEncrypted::No,
    };
    settings
        .get_default_room_notification_mode(is_encrypted, is_one_to_one)
        .await
}

fn is_missing_mention(body: &Option<String>, mode: RoomNotificationMode, client: &Client) -> bool {
    if let Some(body) = body &&
        mode == RoomNotificationMode::MentionsAndKeywordsOnly
    {
        let mentioned = match client.user_id() {
            Some(user_id) => body.contains(user_id.localpart()),
            _ => false,
        };
        return !mentioned;
    }
    false
}

fn is_open(locked: &mut ProgramStore, room_id: &RoomId) -> bool {
    if let Some(draw_curr) = locked.application.draw_curr {
        let info = locked.application.get_room_info(room_id.to_owned());
        if let Some(draw_last) = info.draw_last {
            return draw_last == draw_curr;
        }
    }
    false
}

fn is_focused(locked: &ProgramStore) -> bool {
    locked.application.focused
}

async fn is_visible_room(store: &AsyncProgramStore, room_id: &RoomId) -> bool {
    let mut locked = store.lock().await;

    is_focused(&locked) && is_open(&mut locked, room_id)
}

pub async fn parse_full_notification(
    event: Raw<AnySyncTimelineEvent>,
    room: MatrixRoom,
    show_body: bool,
) -> IambResult<(String, Option<String>, MilliSecondsSinceUnixEpoch)> {
    let event = event.deserialize().map_err(IambError::from)?;

    let server_ts = event.origin_server_ts();

    let sender_id = event.sender();
    let sender = room.get_member_no_sync(sender_id).await.map_err(IambError::from)?;

    let sender_name = sender
        .as_ref()
        .and_then(|m| m.display_name())
        .unwrap_or_else(|| sender_id.localpart());

    let summary = if let Some(room_name) = room.cached_display_name() {
        if room.is_direct().await.map_err(IambError::from)? && sender_name == room_name.to_string()
        {
            sender_name.to_string()
        } else {
            format!("{sender_name} in {room_name}")
        }
    } else {
        sender_name.to_string()
    };

    let body = if show_body {
        event_notification_body(&event, sender_name).map(truncate)
    } else {
        None
    };

    return Ok((summary, body, server_ts));
}

pub fn event_notification_body(event: &AnySyncTimelineEvent, sender_name: &str) -> Option<String> {
    let AnySyncTimelineEvent::MessageLike(event) = event else {
        return None;
    };

    match event.original_content()? {
        AnyMessageLikeEventContent::RoomMessage(message) => {
            let body = match message.msgtype {
                MessageType::Audio(_) => {
                    format!("{sender_name} sent an audio file.")
                },
                MessageType::Emote(content) => content.body,
                MessageType::File(_) => {
                    format!("{sender_name} sent a file.")
                },
                MessageType::Image(_) => {
                    format!("{sender_name} sent an image.")
                },
                MessageType::Location(_) => {
                    format!("{sender_name} sent their location.")
                },
                MessageType::Notice(content) => content.body,
                MessageType::ServerNotice(content) => content.body,
                MessageType::Text(content) => content.body,
                MessageType::Video(_) => {
                    format!("{sender_name} sent a video.")
                },
                MessageType::VerificationRequest(_) => {
                    format!("{sender_name} sent a verification request.")
                },
                _ => {
                    format!("[Unknown message type: {:?}]", &message.msgtype)
                },
            };
            Some(body)
        },
        AnyMessageLikeEventContent::Sticker(_) => Some(format!("{sender_name} sent a sticker.")),
        _ => None,
    }
}

fn truncate(s: String) -> String {
    static MAX_LENGTH: usize = 5000;
    if s.graphemes(true).count() > MAX_LENGTH {
        let truncated: String = s.graphemes(true).take(MAX_LENGTH).collect();
        truncated + "..."
    } else {
        s
    }
}
