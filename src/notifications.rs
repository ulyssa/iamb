use std::time::SystemTime;

use matrix_sdk::EncryptionState;
use matrix_sdk::deserialized_responses::RawAnySyncOrStrippedTimelineEvent;
use matrix_sdk::event_handler::Ctx;
use matrix_sdk::notification_settings::{
    IsEncrypted,
    IsOneToOne,
    NotificationSettings,
    RoomNotificationMode,
};
use matrix_sdk::ruma::events::reaction::ReactionEventContent;
use matrix_sdk::ruma::events::room::message::Relation;
use matrix_sdk::ruma::events::{
    AnyMessageLikeEvent,
    AnyMessageLikeEventContent,
    AnySyncMessageLikeEvent,
    AnySyncTimelineEvent,
    AnyTimelineEvent,
    Mentions,
    OriginalSyncMessageLikeEvent,
    SyncMessageLikeEvent,
};
use matrix_sdk::ruma::serde::Raw;

use crate::config::NotifyVia;
use crate::prelude::*;
use crate::worker::get_receipts_for_timeline_events;

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
    let server_settings = client.notification_settings().await;
    let Some(startup_ts) = MilliSecondsSinceUnixEpoch::from_system_time(SystemTime::now()) else {
        return;
    };
    let reactions = settings.tunables.notifications.reactions;
    let reactions_calmed = settings.tunables.notifications.reactions_calmed_rooms;

    let notify_settings = server_settings.clone();
    client.add_event_handler(
        move |event: OriginalSyncMessageLikeEvent<ReactionEventContent>,
              room: MatrixRoom,
              store: Ctx<AsyncProgramStore>| {
            let server_settings = notify_settings.clone();
            async move {
                if !reactions {
                    return;
                }

                let mode = global_or_room_mode(&server_settings, &room).await;
                if mode == RoomNotificationMode::Mute {
                    return;
                }
                if !reactions_calmed && mode == RoomNotificationMode::MentionsAndKeywordsOnly {
                    return;
                }

                if is_visible_room(&store, room.room_id()).await {
                    return;
                }

                let room_id = room.room_id().to_owned();

                match parse_reaction_notification(event, room, &store).await {
                    Ok(Some((summary, body))) => {
                        send_notification(&notify_via, &summary, &body, room_id, &store).await;
                    },
                    Ok(None) => {
                        // nothing to display
                    },
                    Err(err) => {
                        tracing::error!("Failed to extract notification data: {err}")
                    },
                }
            }
        },
    );

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

                let room_id = room.room_id().to_owned();
                match notification.event {
                    RawAnySyncOrStrippedTimelineEvent::Sync(e) => {
                        match parse_full_notification(e, room, &store).await {
                            Ok((summary, body, server_ts, mentions)) => {
                                if server_ts < startup_ts {
                                    return;
                                }

                                let Some(body) = body else {
                                    // Don't show notifications for unsupported event types.
                                    return;
                                };

                                if is_missing_mention(
                                    &body,
                                    mentions,
                                    mode,
                                    &client,
                                    &server_settings,
                                )
                                .await
                                {
                                    return;
                                }

                                send_notification(&notify_via, &summary, &body, room_id, &store)
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

async fn send_notification(
    via: &NotifyVia,
    summary: &str,
    body: &str,
    room_id: OwnedRoomId,
    store: &AsyncProgramStore,
) {
    #[cfg(feature = "desktop")]
    if via.desktop {
        send_notification_desktop(summary, body, room_id, store).await;
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
#[cfg_attr(target_os = "macos", allow(unused_variables, unused_mut))]
async fn send_notification_desktop(
    summary: &str,
    body: &str,
    room_id: OwnedRoomId,
    store: &AsyncProgramStore,
) {
    let mut locked = store.lock().await;
    let settings = &locked.application.settings;

    let mut desktop_notification = notify_rust::Notification::new();
    desktop_notification
        .summary(summary)
        .appname(IAMB_XDG_NAME)
        .icon(IAMB_XDG_NAME)
        .action("default", "default");

    if let Some(sound_hint) = &settings.tunables.notifications.sound_hint {
        desktop_notification.sound_name(sound_hint);
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    desktop_notification.urgency(notify_rust::Urgency::Normal);

    if settings.tunables.notifications.show_message {
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
            locked
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

async fn is_missing_mention(
    body: &str,
    mentions: Mentions,
    mode: RoomNotificationMode,
    client: &Client,
    settings: &NotificationSettings,
) -> bool {
    if mode != RoomNotificationMode::MentionsAndKeywordsOnly {
        return false;
    }

    if mentions.room {
        return false;
    }
    if client.user_id().is_some_and(|user_id| mentions.user_ids.contains(user_id)) {
        return false;
    }

    for keyword in settings.enabled_keywords().await {
        if body.contains(&keyword) {
            return false;
        }
    }

    true
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

async fn parse_reaction_notification(
    event: OriginalSyncMessageLikeEvent<ReactionEventContent>,
    room: MatrixRoom,
    store: &AsyncProgramStore,
) -> IambResult<Option<(String, String)>> {
    let mut locked = store.lock().await;
    let ChatStore { rooms, settings, .. } = &mut locked.application;
    let info = rooms.get_or_default(room.room_id().to_owned());

    let sender_name = notifiaction_sender_name(&event.sender, info, &room, settings).await?;
    let summary = event_notification_summary(&sender_name, &room).await?;

    let key = event.content.relates_to.key;
    let short = emojis::get(&key).and_then(|emoji| emoji.shortcode());
    let reaction = if settings.tunables.reaction_shortcode_display {
        short.unwrap_or(&key)
    } else {
        &key
    };

    let owned_msg;
    let event_id = &event.content.relates_to.event_id;
    let msg = if let Some(msg) = info.get_event(event_id) {
        msg
    } else if let Some(msg) = load_message(&room, event_id).await.map_err(IambError::from)? {
        owned_msg = msg;
        &owned_msg
    } else {
        return Ok(None);
    };

    if !(settings.tunables.notifications.reactions_other_users || msg.sender == settings.user_id) {
        return Ok(None);
    }

    let text = truncate(msg.event.body(), settings.tunables.notifications.reaction_context);
    let body = format!("Reacted {reaction} to: \"{text}\"");

    Ok(Some((summary, body)))
}

async fn load_message(
    room: &MatrixRoom,
    event_id: &EventId,
) -> Result<Option<Message>, matrix_sdk::Error> {
    let timeline_event = room.load_or_fetch_event(event_id, None).await?;
    let mut events = get_receipts_for_timeline_events(room, vec![timeline_event]).await;
    let (event, ..) = events.pop().ok_or(matrix_sdk::Error::InsufficientData)?;

    let msg = match event {
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomEncrypted(msg)) => msg.into(),
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::RoomMessage(msg)) => msg.into(),
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::Sticker(msg)) => msg.into(),
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::PollStart(_)) |
        AnyTimelineEvent::MessageLike(AnyMessageLikeEvent::UnstablePollStart(_)) => {
            // XXX: Implement more complex conversion since polls don't implement `From`
            return Ok(None);
        },
        AnyTimelineEvent::State(msg) => AnySyncStateEvent::from(msg).into(),
        _ => return Ok(None),
    };

    Ok(Some(msg))
}

async fn parse_full_notification(
    event: Raw<AnySyncTimelineEvent>,
    room: MatrixRoom,
    store: &AsyncProgramStore,
) -> IambResult<(String, Option<String>, MilliSecondsSinceUnixEpoch, Mentions)> {
    let event = event.deserialize().map_err(IambError::from)?;

    let server_ts = event.origin_server_ts();

    let mut locked = store.lock().await;
    let ChatStore { rooms, settings, .. } = &mut locked.application;
    let info = rooms.get_or_default(room.room_id().to_owned());

    let sender_name = notifiaction_sender_name(event.sender(), info, &room, settings).await?;
    let summary = event_notification_summary(&sender_name, &room).await?;
    let body = event_notification_body(&event, &sender_name).map(truncate_message);

    let mentions = if let AnySyncTimelineEvent::MessageLike(AnySyncMessageLikeEvent::RoomMessage(
        SyncMessageLikeEvent::Original(ev),
    )) = event
    {
        ev.content.mentions.unwrap_or_default()
    } else {
        Default::default()
    };

    return Ok((summary, body, server_ts, mentions));
}

async fn notifiaction_sender_name<'a>(
    sender_id: &'a UserId,
    info: &'a RoomInfo,
    room: &MatrixRoom,
    settings: &ApplicationSettings,
) -> IambResult<Cow<'a, str>> {
    // Get the sender name from room info or the room directly and respect the display_name setting
    let mut sender_name = info.display_names.get(sender_id);
    if sender_name.is_none() {
        let sender = room.get_member_no_sync(sender_id).await.map_err(IambError::from)?;
        sender_name = sender
            .as_ref()
            .and_then(|m| m.display_name())
            .map(|name| name.to_owned().into());
    }
    let sender_name = settings.get_user_span(sender_id, sender_name).content;

    Ok(sender_name)
}

async fn event_notification_summary(sender_name: &str, room: &MatrixRoom) -> IambResult<String> {
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

    Ok(summary)
}

fn event_notification_body(event: &AnySyncTimelineEvent, sender_name: &str) -> Option<String> {
    let AnySyncTimelineEvent::MessageLike(event) = event else {
        return None;
    };

    match event.original_content()? {
        AnyMessageLikeEventContent::RoomMessage(message) => {
            let is_thread = matches!(message.relates_to, Some(Relation::Thread(_)));
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
            Some(thread_notification_body(body, is_thread))
        },
        AnyMessageLikeEventContent::Sticker(_) => Some(format!("{sender_name} sent a sticker.")),
        AnyMessageLikeEventContent::PollStart(_) |
        AnyMessageLikeEventContent::UnstablePollStart(_) => {
            Some(format!("{sender_name} started a poll."))
        },
        AnyMessageLikeEventContent::PollEnd(_) | AnyMessageLikeEventContent::UnstablePollEnd(_) => {
            Some(format!("{sender_name} closed a poll."))
        },
        _ => None,
    }
}

fn thread_notification_body(body: String, is_thread: bool) -> String {
    if is_thread {
        format!("Thread: {body}")
    } else {
        body
    }
}

fn truncate(s: Cow<'_, str>, w: usize) -> Cow<'_, str> {
    if s.graphemes(true).count() > w {
        let truncated: String = s.graphemes(true).take(w).collect();
        Cow::Owned(truncated + "...")
    } else {
        s
    }
}

fn truncate_message(s: String) -> String {
    static MAX_LENGTH: usize = 5000;

    truncate(s.into(), MAX_LENGTH).into_owned()
}

#[cfg(test)]
mod tests {
    use super::thread_notification_body;

    #[test]
    fn thread_messages_are_identified_in_notifications() {
        assert_eq!(thread_notification_body("hello".into(), true), "Thread: hello");
        assert_eq!(thread_notification_body("hello".into(), false), "hello");
    }
}
