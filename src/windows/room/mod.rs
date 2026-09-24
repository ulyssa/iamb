//! # Windows for Matrix rooms and spaces
use std::collections::HashSet;

use matrix_sdk::RoomDisplayName;
use matrix_sdk::notification_settings::RoomNotificationMode;
use matrix_sdk::ruma::api::client::room::upgrade_room::v3::Request as UpgradeRoomRequest;
use matrix_sdk::ruma::api::error::ErrorKind as ClientApiErrorKind;
use matrix_sdk::ruma::events::room::canonical_alias::RoomCanonicalAliasEventContent;
use matrix_sdk::ruma::events::room::history_visibility::{
    HistoryVisibility,
    RoomHistoryVisibilityEventContent,
};
use matrix_sdk::ruma::events::room::name::RoomNameEventContent;
use matrix_sdk::ruma::events::room::topic::RoomTopicEventContent;
use matrix_sdk::ruma::events::tag::TagInfo;
use matrix_sdk::ruma::room::{AllowRule, Restricted as JoinRestrictions};

use crate::base::{MemberUpdateAction, RoomField};
use crate::config::EncryptionIndicatorLocation;
use crate::prelude::*;
use crate::windows::room::chat::ChatState;
use crate::windows::room::joining::{Joining, JoiningState};
use crate::windows::room::not_joined::{NotJoined, NotJoinedState};
use crate::windows::room::space::{Space, SpaceState};

mod chat;
mod joining;
mod not_joined;
mod scrollback;
mod space;

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s {
            RoomState::NotJoined($id) => $e,
            RoomState::Joining($id) => $e,
            RoomState::Chat($id) => $e,
            RoomState::Space($id) => $e,
        }
    };
}

fn notification_mode(name: impl Into<String>) -> IambResult<RoomNotificationMode> {
    let name = name.into();

    let mode = match name.to_lowercase().as_str() {
        "mute" => RoomNotificationMode::Mute,
        "mentions" | "keywords" => RoomNotificationMode::MentionsAndKeywordsOnly,
        "all" => RoomNotificationMode::AllMessages,
        _ => return Err(IambError::InvalidNotificationLevel(name).into()),
    };

    Ok(mode)
}

fn hist_visibility_mode(name: impl Into<String>) -> IambResult<HistoryVisibility> {
    let name = name.into();

    let mode = match name.to_lowercase().as_str() {
        "invited" => HistoryVisibility::Invited,
        "joined" => HistoryVisibility::Joined,
        "shared" => HistoryVisibility::Shared,
        "world" | "world_readable" => HistoryVisibility::WorldReadable,
        _ => return Err(IambError::InvalidHistoryVisibility(name).into()),
    };

    Ok(mode)
}

pub async fn room_command(
    id: &RoomId,
    act: RoomAction,
    ctx: ProgramContext,
    store: &mut ProgramStore,
) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
    let worker = &store.application.worker;

    match act {
        RoomAction::Follow(cmd, dir) => {
            let room = worker.client.get_room(id).ok_or(IambError::NotJoined)?;
            let room_id = match dir {
                MoveDir1D::Next => {
                    let successor = room
                        .successor_room()
                        .ok_or_else(|| UIError::Failure("No successor room found".into()))?;
                    successor.room_id
                },
                MoveDir1D::Previous => {
                    let predecessor = room
                        .predecessor_room()
                        .ok_or_else(|| UIError::Failure("No predecessor room found".into()))?;
                    predecessor.room_id
                },
            };

            let id = IambId::Room(room_id.to_owned(), None);
            let target = OpenTarget::Application(id);
            let act = cmd.switch(target);

            Ok(vec![(act, cmd.context.clone())])
        },
        RoomAction::InviteAccept => {
            if let Some(room) = store.application.worker.client.get_room(id) {
                room.join().await.map_err(IambError::from)?;
                Ok(vec![])
            } else {
                Err(IambError::NotInvited.into())
            }
        },
        RoomAction::InviteReject => {
            if let Some(room) = store.application.worker.client.get_room(id) {
                room.leave().await.map_err(IambError::from)?;

                Ok(vec![])
            } else {
                Err(IambError::NotInvited.into())
            }
        },
        RoomAction::InviteSend(user) => {
            if let Some(room) = store.application.worker.client.get_room(id) {
                room.invite_user_by_id(user.as_ref()).await.map_err(IambError::from)?;

                Ok(vec![])
            } else {
                Err(IambError::NotJoined.into())
            }
        },
        RoomAction::KnockAccept(user) => {
            let room = worker.client.get_room(id).ok_or(IambError::NotJoined)?;
            room.invite_user_by_id(&user).await.map_err(IambError::from)?;
            Ok(vec![])
        },
        RoomAction::KnockReject(user, reason) => {
            let room = worker.client.get_room(id).ok_or(IambError::NotJoined)?;
            room.kick_user(&user, reason.as_deref()).await.map_err(IambError::from)?;
            Ok(vec![])
        },
        RoomAction::KnockBan(user, reason) => {
            let room = worker.client.get_room(id).ok_or(IambError::NotJoined)?;
            room.ban_user(&user, reason.as_deref()).await.map_err(IambError::from)?;
            Ok(vec![])
        },
        RoomAction::Leave(skip_confirm) => {
            if let Some(room) = store.application.worker.client.get_room(id) {
                if skip_confirm {
                    room.leave().await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    let msg = "Do you really want to leave this room?";
                    let leave = IambAction::Room(RoomAction::Leave(true));
                    let prompt = PromptYesNo::new(msg, vec![Action::from(leave)]);
                    let prompt = Box::new(prompt);

                    Err(UIError::NeedConfirm(prompt))
                }
            } else {
                Err(IambError::NotJoined.into())
            }
        },
        RoomAction::MemberUpdate(mua, user, reason, skip_confirm) => {
            let Some(room) = store.application.worker.client.get_room(id) else {
                return Err(IambError::NotJoined.into());
            };

            let Ok(user_id) = OwnedUserId::try_from(user.as_str()) else {
                let err = IambError::InvalidUserId(user);

                return Err(err.into());
            };

            if !skip_confirm {
                let msg = format!("Do you really want to {mua} {user} from this room?");
                let act = RoomAction::MemberUpdate(mua, user, reason, true);
                let act = IambAction::from(act);
                let prompt = PromptYesNo::new(msg, vec![Action::from(act)]);
                let prompt = Box::new(prompt);

                return Err(UIError::NeedConfirm(prompt));
            }

            match mua {
                MemberUpdateAction::Ban => {
                    room.ban_user(&user_id, reason.as_deref()).await.map_err(IambError::from)?;
                },
                MemberUpdateAction::Unban => {
                    room.unban_user(&user_id, reason.as_deref())
                        .await
                        .map_err(IambError::from)?;
                },
                MemberUpdateAction::Kick => {
                    room.kick_user(&user_id, reason.as_deref())
                        .await
                        .map_err(IambError::from)?;
                },
            }

            Ok(vec![])
        },
        RoomAction::Members(mut cmd) => {
            let id = IambId::MemberList(id.to_owned());
            let target = OpenTarget::Application(id);
            let cmd = cmd.default_relation(MoveDir1D::Next);

            let act = match store.application.settings.tunables.members_split {
                Some(dir) => cmd.default_axis(dir.to_axis()).window(target, None),
                None => cmd.switch(target),
            };

            Ok(vec![(act, cmd.context.clone())])
        },
        RoomAction::Pinned(cmd) => {
            let id = IambId::PinnedList(id.to_owned());
            let target = OpenTarget::Application(id);
            let act = cmd.switch(target);

            Ok(vec![(act, cmd.context.clone())])
        },
        RoomAction::SetAccess(rule) => {
            let Some(room) = store.application.worker.client.get_room(id) else {
                return Err(IambError::NotJoined.into());
            };
            let rule = rule.into_join_rule(&store.application.worker.client).await?;
            room.privacy_settings()
                .update_join_rule(rule)
                .await
                .map_err(IambError::from)?;
            Ok(vec![])
        },
        RoomAction::SetDirect(is_direct) => {
            let room = store
                .application
                .get_joined_room(id)
                .ok_or(UIError::Application(IambError::NotJoined))?;

            room.set_is_direct(is_direct).await.map_err(IambError::from)?;

            Ok(vec![])
        },
        RoomAction::SetUnread(is_unread) => {
            let room = store
                .application
                .get_joined_room(id)
                .ok_or(UIError::Application(IambError::NotJoined))?;

            room.set_unread_flag(is_unread).await.map_err(IambError::from)?;

            if !is_unread {
                let info = store.application.rooms.get_or_default(id.to_owned());

                info.fully_read(
                    id.to_owned(),
                    ReceiptThread::Main,
                    worker,
                    &store.application.settings,
                    &mut store.application.open_notifications,
                );
            }

            Ok(vec![])
        },
        RoomAction::Set(field, value) => {
            let room = store
                .application
                .get_joined_room(id)
                .ok_or(UIError::Application(IambError::NotJoined))?;

            match field {
                RoomField::History => {
                    let visibility = hist_visibility_mode(value)?;
                    let ev = RoomHistoryVisibilityEventContent::new(visibility);
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::Name => {
                    let ev = RoomNameEventContent::new(value);
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::Tag(tag) => {
                    let mut info = TagInfo::new();
                    info.order = Some(1.0);

                    let _ = room.set_tag(tag, info).await.map_err(IambError::from)?;
                },
                RoomField::Topic => {
                    let ev = RoomTopicEventContent::new(value);
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::NotificationMode => {
                    let mode = notification_mode(value)?;
                    let client = &store.application.worker.client;
                    let notifications = client.notification_settings().await;

                    notifications
                        .set_room_notification_mode(id, mode)
                        .await
                        .map_err(IambError::from)?;
                },
                RoomField::CanonicalAlias => {
                    let client = &mut store.application.worker.client;

                    let Ok(orai) = OwnedRoomAliasId::try_from(value.as_str()) else {
                        let err = IambError::InvalidRoomAlias(value);

                        return Err(err.into());
                    };

                    let mut alt_aliases = room.alt_aliases().into_iter().collect::<HashSet<_>>();
                    let canonical_old = room.canonical_alias();

                    // If the room's alias is already that, ignore it
                    if canonical_old.as_ref() == Some(&orai) {
                        let msg = format!("The canonical room alias is already {orai}");

                        return Ok(vec![(Action::ShowInfoMessage(msg.into()), ctx)]);
                    }

                    // Try creating the room alias on the server.
                    if let Err(e) = client.create_room_alias(&orai, room.room_id()).await {
                        if let Some(ClientApiErrorKind::Unknown) = e.client_api_error_kind() {
                            // Ignore when it already exists.
                        } else {
                            return Err(IambError::from(e).into());
                        }
                    }

                    // Demote the previous one to an alt alias.
                    alt_aliases.extend(canonical_old);

                    // At this point the room alias definitely exists, and we can update the
                    // state event.
                    let mut ev = RoomCanonicalAliasEventContent::new();
                    ev.alias = Some(orai);
                    ev.alt_aliases = alt_aliases.into_iter().collect();
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::Alias(alias) => {
                    let client = &mut store.application.worker.client;

                    let Ok(orai) = OwnedRoomAliasId::try_from(alias.as_str()) else {
                        let err = IambError::InvalidRoomAlias(alias);

                        return Err(err.into());
                    };

                    let mut alt_aliases = room.alt_aliases().into_iter().collect::<HashSet<_>>();
                    let canonical = room.canonical_alias();

                    if alt_aliases.contains(&orai) || canonical.as_ref() == Some(&orai) {
                        let msg = format!("The alias {orai} already maps to this room");

                        return Ok(vec![(Action::ShowInfoMessage(msg.into()), ctx)]);
                    } else {
                        alt_aliases.insert(orai.clone());
                    }

                    // If the room alias does not exist on the server, create it
                    if let Err(e) = client.create_room_alias(&orai, room.room_id()).await {
                        if let Some(ClientApiErrorKind::Unknown) = e.client_api_error_kind() {
                            // Ignore when it already exists.
                        } else {
                            return Err(IambError::from(e).into());
                        }
                    }

                    // And add it to the aliases in the state event.
                    let mut ev = RoomCanonicalAliasEventContent::new();
                    ev.alias = canonical;
                    ev.alt_aliases = alt_aliases.into_iter().collect();
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::UserName => {
                    room.set_own_member_display_name(Some(value))
                        .await
                        .map_err(IambError::from)?;
                },
                RoomField::Access | RoomField::Aliases | RoomField::Id | RoomField::Version => {
                    // These variants exist for RoomAction::Show, so we never actually get here.
                },
            }

            Ok(vec![])
        },
        RoomAction::Unset(field) => {
            let room = store
                .application
                .get_joined_room(id)
                .ok_or(UIError::Application(IambError::NotJoined))?;

            match field {
                RoomField::History => {
                    let visibility = HistoryVisibility::Joined;
                    let ev = RoomHistoryVisibilityEventContent::new(visibility);
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::Name => {
                    let ev = RoomNameEventContent::new("".into());
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::Tag(tag) => {
                    let _ = room.remove_tag(tag).await.map_err(IambError::from)?;
                },
                RoomField::Topic => {
                    let ev = RoomTopicEventContent::new("".into());
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;
                },
                RoomField::NotificationMode => {
                    let client = &store.application.worker.client;
                    let notifications = client.notification_settings().await;

                    notifications
                        .delete_user_defined_room_rules(id)
                        .await
                        .map_err(IambError::from)?;
                },
                RoomField::CanonicalAlias => {
                    let Some(alias_to_destroy) = room.canonical_alias() else {
                        let msg = "This room has no canonical alias to unset";

                        return Ok(vec![(Action::ShowInfoMessage(msg.into()), ctx)]);
                    };

                    // Remove the canonical alias from the state event.
                    let mut ev = RoomCanonicalAliasEventContent::new();
                    ev.alias = None;
                    ev.alt_aliases = room.alt_aliases();
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;

                    // And then unmap it on the server.
                    store
                        .application
                        .worker
                        .client
                        .remove_room_alias(&alias_to_destroy)
                        .await
                        .map_err(IambError::from)?;
                },
                RoomField::Alias(alias) => {
                    let Ok(orai) = OwnedRoomAliasId::try_from(alias.as_str()) else {
                        let err = IambError::InvalidRoomAlias(alias);

                        return Err(err.into());
                    };

                    let alt_aliases = room.alt_aliases();
                    let canonical = room.canonical_alias();

                    if !alt_aliases.contains(&orai) && canonical.as_ref() != Some(&orai) {
                        let msg = format!("The alias {orai:?} isn't mapped to this room");

                        return Ok(vec![(Action::ShowInfoMessage(msg.into()), ctx)]);
                    }

                    // Remove the alias from the state event if it's in it.
                    let mut ev = RoomCanonicalAliasEventContent::new();
                    ev.alias = canonical.filter(|canon| canon != &orai);
                    ev.alt_aliases = alt_aliases;
                    ev.alt_aliases.retain(|in_orai| in_orai != &orai);
                    let _ = room.send_state_event(ev).await.map_err(IambError::from)?;

                    // And then unmap it on the server.
                    store
                        .application
                        .worker
                        .client
                        .remove_room_alias(&orai)
                        .await
                        .map_err(IambError::from)?;
                },
                RoomField::UserName => {
                    room.set_own_member_display_name(None).await.map_err(IambError::from)?;
                },
                RoomField::Access | RoomField::Aliases | RoomField::Id | RoomField::Version => {
                    // These variants exist for RoomAction::Show, so we never actually get here.
                },
            }

            Ok(vec![])
        },
        RoomAction::Show(field) => {
            let room = store
                .application
                .get_joined_room(id)
                .ok_or(UIError::Application(IambError::NotJoined))?;

            let msg = match field {
                RoomField::History => {
                    let visibility = room.history_visibility();
                    let visibility = visibility.as_ref().map(|v| v.as_str());
                    format!("Room history visibility: {}", visibility.unwrap_or("<unknown>"))
                },
                RoomField::Id => {
                    let id = room.room_id();
                    format!("Room identifier: {id}")
                },
                RoomField::Version => {
                    let v = room.version();
                    let v = v.as_ref().map(|v| v.as_str()).unwrap_or("<version unknown>");
                    format!("Room version: {v}")
                },
                RoomField::Name => {
                    match room.name() {
                        None => "Room has no name".into(),
                        Some(name) => format!("Room name: {name:?}"),
                    }
                },
                RoomField::Topic => {
                    match room.topic() {
                        None => "Room has no topic".into(),
                        Some(topic) => format!("Room topic: {topic:?}"),
                    }
                },
                RoomField::NotificationMode => {
                    let client = &store.application.worker.client;
                    let notifications = client.notification_settings().await;
                    let mode = notifications.get_user_defined_room_notification_mode(id).await;

                    let level = match mode {
                        Some(RoomNotificationMode::Mute) => "mute",
                        Some(RoomNotificationMode::MentionsAndKeywordsOnly) => "keywords",
                        Some(RoomNotificationMode::AllMessages) => "all",
                        None => "default",
                    };

                    format!("Room notification level: {level:?}")
                },
                RoomField::Access => {
                    let show_restrictions = |rs: JoinRestrictions| {
                        rs.allow
                            .into_iter()
                            .map(|a| {
                                match a {
                                    AllowRule::RoomMembership(m) => {
                                        if let Some(alias) =
                                            store.application.get_joined_room_alias(&m.room_id)
                                        {
                                            format!("members of {} ({alias})", m.room_id)
                                        } else {
                                            format!("members of {}", m.room_id)
                                        }
                                    },
                                    other => format!("{other:?}"),
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    };

                    let desc = match room.join_rule() {
                        None => "<unknown>".into(),
                        Some(JoinRule::Invite) => "invite".into(),
                        Some(JoinRule::Knock) => "knock".into(),
                        Some(JoinRule::Private) => "private".into(),
                        Some(JoinRule::Public) => "public".into(),
                        Some(JoinRule::Restricted(restrictions)) => {
                            let allowing = show_restrictions(restrictions);
                            if allowing.is_empty() {
                                "restricted".into()
                            } else {
                                format!("restricted, allowing {allowing}")
                            }
                        },
                        Some(JoinRule::KnockRestricted(restrictions)) => {
                            let allowing = show_restrictions(restrictions);
                            if allowing.is_empty() {
                                "knock-restricted".into()
                            } else {
                                format!("knock-restricted, allowing {allowing}")
                            }
                        },
                        Some(other) => format!("{other:?}"),
                    };

                    format!("Room join rules are set to: {desc}")
                },
                RoomField::Aliases => {
                    let aliases = room
                        .alt_aliases()
                        .iter()
                        .map(OwnedRoomAliasId::to_string)
                        .collect::<Vec<String>>();

                    if aliases.is_empty() {
                        "No alternative aliases in room".into()
                    } else {
                        format!("Alternative aliases: {}.", aliases.join(", "))
                    }
                },
                RoomField::CanonicalAlias => {
                    match room.canonical_alias() {
                        None => "No canonical alias for room".into(),
                        Some(can) => format!("Canonical alias: {can}"),
                    }
                },
                RoomField::Tag(_) => "Cannot currently show value for a tag".into(),
                RoomField::Alias(_) => {
                    "Cannot show a single alias; use `:room aliases show` instead.".into()
                },
                RoomField::UserName => {
                    let user_id = &store.application.settings.profile.user_id;
                    let Some(member) = room.get_member(user_id).await.map_err(IambError::from)?
                    else {
                        let msg = "Cannot find membership data".into();
                        return Err(IambError::Custom(msg))?;
                    };

                    match member.display_name() {
                        Some(name) => format!("User name: \"{name}\""),
                        None => "No user name set".into(),
                    }
                },
            };

            let msg = InfoMessage::Pager(msg);
            let act = Action::ShowInfoMessage(msg);

            Ok(vec![(act, ctx)])
        },
        RoomAction::Upgrade(new_version, additional_creators, false) => {
            let room = worker.client.get_room(id).ok_or(IambError::NotJoined)?;
            let alias = room.canonical_alias();
            let name = alias.as_ref().map(|c| c.as_str()).unwrap_or_else(|| id.as_str());
            let msg = format!(
                "Are you sure you want to upgrade {name} to version {new_version} with {} additional creators set?",
                additional_creators.len()
            );
            let upgrade =
                IambAction::Room(RoomAction::Upgrade(new_version, additional_creators, true));
            let prompt = PromptYesNo::new(msg, vec![Action::from(upgrade)]);
            let prompt = Box::new(prompt);

            Err(UIError::NeedConfirm(prompt))
        },
        RoomAction::Upgrade(new_version, additional_creators, true) => {
            let mut request = UpgradeRoomRequest::new(id.to_owned(), new_version);
            request.additional_creators = additional_creators;

            let response = worker.client.send(request).await.map_err(IambError::from)?;
            let id = IambId::Room(response.replacement_room, None);
            let target = OpenTarget::Application(id);
            let act = WindowAction::Switch(target);

            Ok(vec![(act.into(), ctx)])
        },
    }
}

/// State for a Matrix room or space.
///
/// Since spaces function as special rooms within Matrix, we wrap their window state together, so
/// that operations like sending and accepting invites, opening the members window, etc., all work
/// similarly.
pub enum RoomState {
    NotJoined(Box<NotJoinedState>),
    Joining(Box<JoiningState>),
    Chat(Box<ChatState>),
    Space(Box<SpaceState>),
}

impl From<NotJoinedState> for RoomState {
    fn from(chat: NotJoinedState) -> Self {
        RoomState::NotJoined(Box::new(chat))
    }
}

impl From<JoiningState> for RoomState {
    fn from(chat: JoiningState) -> Self {
        RoomState::Joining(Box::new(chat))
    }
}

impl From<ChatState> for RoomState {
    fn from(chat: ChatState) -> Self {
        RoomState::Chat(Box::new(chat))
    }
}

impl From<SpaceState> for RoomState {
    fn from(space: SpaceState) -> Self {
        RoomState::Space(Box::new(space))
    }
}

impl RoomState {
    pub fn new(
        room: MatrixRoom,
        thread: Option<OwnedEventId>,
        name: RoomDisplayName,
        tags: Option<Tags>,
        store: &mut ProgramStore,
    ) -> Self {
        let room_id = room.room_id().to_owned();
        let info = store.application.get_room_info(room_id);
        info.name = name.to_string().into();
        info.tags = tags;

        if room.is_space() {
            SpaceState::new(room).into()
        } else {
            ChatState::new(room, thread, store).into()
        }
    }

    pub fn join(name: String, store: &mut ProgramStore) -> Self {
        let joining = JoiningState::new(name, store);
        Self::from(joining)
    }

    pub fn not_joined(name: String) -> Self {
        Self::from(NotJoinedState::new(name))
    }

    pub fn window_id(&self) -> IambId {
        match self {
            RoomState::NotJoined(nj) => IambId::NotJoined(nj.room.clone()),
            RoomState::Joining(joining) => IambId::Joining(joining.room.clone()),
            RoomState::Chat(chat) => IambId::Room(chat.id().to_owned(), chat.thread().cloned()),
            RoomState::Space(space) => IambId::Room(space.id().to_owned(), None),
        }
    }

    pub fn room_state(&self) -> Option<MatrixRoomState> {
        match self {
            RoomState::NotJoined(_) | RoomState::Joining(_) => None,
            RoomState::Chat(chat) => Some(chat.room().state()),
            RoomState::Space(space) => Some(space.room().state()),
        }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        match self {
            RoomState::NotJoined(_) => {},
            RoomState::Joining(joining) => {
                let joined = joining.room.clone();

                let room_id = match joining.try_recv() {
                    None => return,
                    Some(Ok(room_id)) => room_id.clone(),
                    Some(Err(e)) => {
                        // We failed to join the room, so show the NotJoined window,
                        // and let the user try again if the error was transient:
                        *self = NotJoinedState::failed(joined, e).into();
                        return;
                    },
                };

                let Ok((room, name, tags)) = store.application.worker.get_room(room_id.clone())
                else {
                    return;
                };

                if let Ok(alias) = OwnedRoomAliasId::try_from(joined.as_str()) {
                    // If the `:join` was for a room alias, then track it so
                    // we can reuse it later for `:join`/`:sp`/etc:
                    store.application.names.insert(alias, room_id.clone());
                }

                store.application.need_load.need_members(room_id);

                *self = RoomState::new(room, None, name, tags, store);
            },
            RoomState::Chat(chat) => chat.refresh_room(store),
            RoomState::Space(space) => space.refresh_room(store),
        }
    }

    fn draw_invite(
        &self,
        invited: &MatrixRoom,
        area: Rect,
        buf: &mut Buffer,
        store: &mut ProgramStore,
    ) {
        let inviter = store.application.worker.get_inviter(invited.clone());
        let room_id = invited.room_id();

        let name = match invited.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(room_id)),
        };

        let mut invited = vec![Span::from(format!("You have been invited to join {name}"))];

        if let Ok(Some(inviter)) = &inviter {
            let info = store.application.rooms.get_or_default(room_id.to_owned());
            invited.push(Span::from(" by "));
            invited.push(store.application.settings.get_user_span(inviter.user_id(), info));
        }

        let l1 = Line::from(invited);
        let l2 = Line::from(
            "You can run `:invite accept` or `:invite reject` to accept or reject this invitation.",
        );
        let text = Text::from(vec![l1, l2]);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    fn draw_knock(
        &self,
        knocked: &MatrixRoom,
        area: Rect,
        buf: &mut Buffer,
        store: &mut ProgramStore,
    ) {
        let name = match knocked.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(knocked.room_id())),
        };

        let l1 = Line::from(format!(
            "Your request to join {name} is pending review by room moderators."
        ));
        let l2 = Line::from("You can run `:leave` to withdraw your knock request.");
        let text = Text::from(vec![l1, l2]);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    fn draw_left(&self, room: &MatrixRoom, area: Rect, buf: &mut Buffer, store: &mut ProgramStore) {
        let name = match room.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(room.room_id())),
        };

        let mut lines = vec![Line::from(format!("You have left {name}!"))];

        if room.is_public().is_some_and(|b| b) {
            lines.push(Line::from(format!("You can run `:join {name}` to rejoin.")));
        }

        let text = Text::from(lines);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);

        return;
    }

    pub async fn timeline_command(
        &mut self,
        act: TimelineAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let RoomState::Chat(chat) = self {
            chat.timeline_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedRoom.into())
        }
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let RoomState::Chat(chat) = self {
            chat.message_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedMessage.into())
        }
    }

    pub async fn space_command(
        &mut self,
        act: SpaceAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let RoomState::Space(space) = self {
            space.space_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedSpace.into())
        }
    }

    pub async fn send_command(
        &mut self,
        act: SendAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let RoomState::Chat(chat) = self {
            chat.send_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedRoom.into())
        }
    }

    pub fn get_title(&self, store: &mut ProgramStore, style: Style) -> Line<'_> {
        let Some(room) = self.room() else {
            return Line::from("Unjoined Room");
        };

        let room_id = room.room_id();
        let title = store.application.get_room_title(room_id);
        let bold_style = style.add_modifier(StyleModifier::BOLD);
        let mut spans = vec![];

        let encryption_settings = &store.application.settings.tunables.encryption;
        let encryption_indicator = encryption_settings
            .get_indicator(EncryptionIndicatorLocation::TITLE, room.encryption_state());
        spans.extend(encryption_indicator);
        spans.push(Span::raw(" "));

        if let RoomState::Chat(chat) = self &&
            chat.thread().is_some()
        {
            spans.push(Span::styled("Thread in ", style));
        }

        spans.push(Span::styled(title, bold_style));

        match room.topic() {
            Some(desc) if !desc.is_empty() => {
                spans.push(Span::styled(" (", style));
                spans.push(Span::styled(desc, style));
                spans.push(Span::styled(")", style));
            },
            _ => {
                spans.push(" ".into());
            },
        }

        Line::from(spans)
    }

    pub fn get_tab_title(&self, store: &mut ProgramStore) -> Line<'_> {
        match self {
            RoomState::Space(w) => store.application.get_room_title(w.id()).into(),
            RoomState::Chat(w) => store.application.get_room_title(w.id()).into(),
            RoomState::NotJoined(_) => Line::from("Unjoined Room"),
            RoomState::Joining(w) => {
                Line::from(vec![Span::raw("Joining "), Span::raw(w.room.as_str())])
            },
        }
    }

    pub fn focus_toggle(&mut self) {
        match self {
            RoomState::Chat(chat) => chat.focus_toggle(),
            RoomState::Joining(_) | RoomState::NotJoined(_) | RoomState::Space(_) => return,
        }
    }

    pub fn room(&self) -> Option<&MatrixRoom> {
        match self {
            RoomState::Chat(chat) => Some(chat.room()),
            RoomState::Space(space) => Some(space.room()),
            RoomState::Joining(_) | RoomState::NotJoined(_) => None,
        }
    }

    pub fn id(&self) -> Option<&RoomId> {
        match self {
            RoomState::Chat(chat) => Some(chat.id()),
            RoomState::Space(space) => Some(space.id()),
            RoomState::Joining(_) | RoomState::NotJoined(_) => None,
        }
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for RoomState {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.editor_command(act, ctx, store))
    }
}

impl Jumpable<ProgramContext, IambInfo> for RoomState {
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &ProgramContext,
    ) -> IambResult<usize> {
        delegate!(self, w => w.jump(list, dir, count, ctx))
    }
}

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for RoomState {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.scroll(style, ctx, store))
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for RoomState {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        delegate!(self, w => w.prompt(act, ctx, store))
    }
}

impl TerminalCursor for RoomState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        delegate!(self, w => w.get_term_cursor())
    }

    fn hide_term_cursor(&self) -> bool {
        delegate!(self, w => w.hide_term_cursor())
    }
}

impl WindowOps<IambInfo> for RoomState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        if self.room_state() != Some(MatrixRoomState::Joined) {
            self.refresh_room(store);
        }

        if let Some(room) = self.room() {
            match room.state() {
                MatrixRoomState::Invited => return self.draw_invite(room, area, buf, store),
                MatrixRoomState::Knocked => return self.draw_knock(room, area, buf, store),
                MatrixRoomState::Left => return self.draw_left(room, area, buf, store),
                _ => (),
            }
        }

        match self {
            RoomState::Chat(chat) => {
                chat.draw(area, buf, focused, store);
            },
            RoomState::Joining(state) => {
                Joining.render(area, buf, state);
            },
            RoomState::NotJoined(state) => {
                NotJoined.render(area, buf, state);
            },
            RoomState::Space(space) => {
                Space::new(store).focus(focused).render(area, buf, space);
            },
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            RoomState::Joining(w) => RoomState::join(w.room.clone(), store),
            RoomState::NotJoined(w) => RoomState::NotJoined((*w).clone()),
            RoomState::Chat(chat) => RoomState::Chat(Box::new(chat.dup(store))),
            RoomState::Space(space) => RoomState::Space(Box::new(space.dup(store))),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        match self {
            RoomState::NotJoined(_) | RoomState::Joining(_) => true,
            RoomState::Chat(chat) => chat.close(flags, store),
            RoomState::Space(space) => space.close(flags, store),
        }
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::NotJoined(_) | RoomState::Joining(_) => Err(EditError::ReadOnly.into()),
            RoomState::Chat(chat) => chat.write(path, flags, store),
            RoomState::Space(space) => space.write(path, flags, store),
        }
    }

    fn get_completions(&self) -> Option<CompletionList> {
        match self {
            RoomState::NotJoined(_) | RoomState::Joining(_) => None,
            RoomState::Chat(chat) => chat.get_completions(),
            RoomState::Space(space) => space.get_completions(),
        }
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        match self {
            RoomState::NotJoined(_) | RoomState::Joining(_) => None,
            RoomState::Chat(chat) => chat.get_cursor_word(style),
            RoomState::Space(space) => space.get_cursor_word(style),
        }
    }

    fn get_selected_word(&self) -> Option<String> {
        match self {
            RoomState::NotJoined(_) | RoomState::Joining(_) => None,
            RoomState::Chat(chat) => chat.get_selected_word(),
            RoomState::Space(space) => space.get_selected_word(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_room_notification_level() {
        let tests = vec![
            ("mute", RoomNotificationMode::Mute),
            ("mentions", RoomNotificationMode::MentionsAndKeywordsOnly),
            ("keywords", RoomNotificationMode::MentionsAndKeywordsOnly),
            ("all", RoomNotificationMode::AllMessages),
        ];

        for (input, expect) in tests {
            let res = notification_mode(input).unwrap();
            assert_eq!(expect, res);
        }

        assert!(notification_mode("invalid").is_err());
        assert!(notification_mode("not a level").is_err());
        assert!(notification_mode("@user:example.com").is_err());
    }
}
