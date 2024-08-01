use std::time::SystemTime;

use matrix_sdk::{
    notification_settings::{IsEncrypted, IsOneToOne, NotificationSettings, RoomNotificationMode},
    room::Room as MatrixRoom,
    ruma::{
        api::client::push::get_notifications::v3::Notification,
        events::{room::message::MessageType, AnyMessageLikeEventContent, AnySyncTimelineEvent},
        MilliSecondsSinceUnixEpoch,
        RoomId,
    },
    Client,
};
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    base::{AsyncProgramStore, IambError, IambResult, ProgramStore},
    config::{ApplicationSettings, NotifyVia},
};

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
    let server_settings = client.notification_settings().await;
    let Some(startup_ts) = MilliSecondsSinceUnixEpoch::from_system_time(SystemTime::now()) else {
        return;
    };

    let store = store.clone();
    client
        .register_notification_handler(move |notification, room: MatrixRoom, client: Client| {
            let store = store.clone();
            let server_settings = server_settings.clone();
            async move {
                let mode = global_or_room_mode(&server_settings, &room).await;
                if mode == RoomNotificationMode::Mute {
                    return;
                }

                if is_visible_room(&store, room.room_id()).await {
                    return;
                }

                match parse_notification(notification, room, show_message).await {
                    Ok((summary, body, server_ts)) => {
                        if server_ts < startup_ts {
                            return;
                        }

                        if is_missing_mention(&body, mode, &client) {
                            return;
                        }

                        match notify_via {
                            NotifyVia::Desktop => send_notification_desktop(summary, body),
                            NotifyVia::Bell => send_notification_bell(&store).await,
                        }
                    },
                    Err(err) => {
                        tracing::error!("Failed to extract notification data: {err}")
                    },
                }
            }
        })
        .await;
}

async fn send_notification_bell(store: &AsyncProgramStore) {
    let mut locked = store.lock().await;
    locked.application.ring_bell = true;
}

fn send_notification_desktop(summary: String, body: Option<String>) {
    let mut desktop_notification = notify_rust::Notification::new();
    desktop_notification
        .summary(&summary)
        .appname("iamb")
        .timeout(notify_rust::Timeout::Milliseconds(3000))
        .action("default", "default");

    if let Some(body) = body {
        desktop_notification.body(&body);
    }

    if let Err(err) = desktop_notification.show() {
        tracing::error!("Failed to send notification: {err}")
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
    let is_encrypted = match room.is_encrypted().await {
        Ok(true) => IsEncrypted::Yes,
        _ => IsEncrypted::No,
    };
    settings
        .get_default_room_notification_mode(is_encrypted, is_one_to_one)
        .await
}

fn is_missing_mention(body: &Option<String>, mode: RoomNotificationMode, client: &Client) -> bool {
    if let Some(body) = body {
        if mode == RoomNotificationMode::MentionsAndKeywordsOnly {
            let mentioned = match client.user_id() {
                Some(user_id) => body.contains(user_id.localpart()),
                _ => false,
            };
            return !mentioned;
        }
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

pub async fn parse_notification(
    notification: Notification,
    room: MatrixRoom,
    show_body: bool,
) -> IambResult<(String, Option<String>, MilliSecondsSinceUnixEpoch)> {
    let event = notification.event.deserialize().map_err(IambError::from)?;

    let server_ts = event.origin_server_ts();

    let sender_id = event.sender();
    let sender = room.get_member_no_sync(sender_id).await.map_err(IambError::from)?;

    let sender_name = sender
        .as_ref()
        .and_then(|m| m.display_name())
        .unwrap_or_else(|| sender_id.localpart());

    let body = if show_body {
        event_notification_body(
            &event,
            sender_name,
            room.is_direct().await.map_err(IambError::from)?,
        )
        .map(truncate)
    } else {
        None
    };

    return Ok((sender_name.to_string(), body, server_ts));
}

pub fn event_notification_body(
    event: &AnySyncTimelineEvent,
    sender_name: &str,
    is_direct: bool,
) -> Option<String> {
    let AnySyncTimelineEvent::MessageLike(event) = event else {
        return None;
    };

    match event.original_content()? {
        AnyMessageLikeEventContent::RoomMessage(message) => {
            let body = match message.msgtype {
                MessageType::Audio(_) => {
                    format!("{sender_name} sent an audio file.")
                },
                MessageType::Emote(content) => {
                    let message = &content.body;
                    format!("{sender_name}: {message}")
                },
                MessageType::File(_) => {
                    format!("{sender_name} sent a file.")
                },
                MessageType::Image(_) => {
                    format!("{sender_name} sent an image.")
                },
                MessageType::Location(_) => {
                    format!("{sender_name} sent their location.")
                },
                MessageType::Notice(content) => {
                    let message = &content.body;
                    format!("{sender_name}: {message}")
                },
                MessageType::ServerNotice(content) => {
                    let message = &content.body;
                    format!("{sender_name}: {message}")
                },
                MessageType::Text(content) => {
                    if is_direct {
                        content.body
                    } else {
                        let message = &content.body;
                        format!("{sender_name}: {message}")
                    }
                },
                MessageType::Video(_) => {
                    format!("{sender_name} sent a video.")
                },
                MessageType::VerificationRequest(_) => {
                    format!("{sender_name} sent a verification request.")
                },
                _ => unimplemented!(),
            };
            Some(body)
        },
        AnyMessageLikeEventContent::Sticker(_) => Some(format!("{sender_name} sent a sticker.")),
        _ => None,
    }
}

fn truncate(s: String) -> String {
    static MAX_LENGTH: usize = 100;
    if s.graphemes(true).count() > MAX_LENGTH {
        let truncated: String = s.graphemes(true).take(MAX_LENGTH).collect();
        truncated + "..."
    } else {
        s
    }
}
