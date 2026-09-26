//! # Windows for the User Interface
//!
//! This module contains the logic for rendering windows, and handling UI actions that get
//! delegated to individual windows/UI elements (e.g., typing text or selecting a list item).
//!
//! Additionally, some of the iamb commands delegate behaviour to the current UI element. For
//! example, [sending messages][crate::base::SendAction] delegate to the [room window][RoomState],
//! where we have the message bar and room ID easily accessible and resettable.

use std::cmp::Ord;
use std::collections::HashSet;
use std::fmt;

use feruca::Collator;
use matrix_sdk::notification_settings::RoomNotificationMode;
use matrix_sdk::room::RoomMember;
use matrix_sdk::ruma::api::client::room::upgrade_room::v3::Request as UpgradeRoomRequest;
use matrix_sdk::ruma::api::error::ErrorKind as ClientApiErrorKind;
use matrix_sdk::ruma::events::room::canonical_alias::RoomCanonicalAliasEventContent;
use matrix_sdk::ruma::events::room::history_visibility::{
    HistoryVisibility,
    RoomHistoryVisibilityEventContent,
};
use matrix_sdk::ruma::events::room::member::MembershipState;
use matrix_sdk::ruma::events::room::name::RoomNameEventContent;
use matrix_sdk::ruma::events::room::topic::RoomTopicEventContent;
use matrix_sdk::ruma::events::tag::TagInfo;
use matrix_sdk::ruma::room::{AllowRule, Restricted as JoinRestrictions};
use matrix_sdk::ruma::{RoomAliasId, RoomOrAliasId, assign};
use modalkit::editing::completion::CompletionMap;
use modalkit::editing::context::EditContext;
use modalkit_ratatui::Window;
use modalkit_ratatui::list::{List, ListCursor, ListItem, ListState};

use crate::base::{
    MemberUpdateAction,
    RoomField,
    RoomView,
    SortColumn,
    SortFieldRoom,
    SortFieldUser,
    SortOrder,
    UnreadInfo,
};
use crate::prelude::*;
use crate::windows::room::RoomState;
use crate::windows::verify::VerifyItem;
use crate::windows::welcome::WelcomeState;

pub mod room;
pub mod verify;
pub mod welcome;

const MEMBER_FETCH_DEBOUNCE: Duration = Duration::from_secs(5);

#[inline]
pub fn selected_style(selected: bool, style: Style) -> Style {
    if selected {
        style.add_modifier(StyleModifier::REVERSED)
    } else {
        style
    }
}

fn name_and_labels<'a>(
    name: &'a str,
    unread: &UnreadInfo,
    room_membership: MatrixRoomState,
    name_style: Style,
    tags_style: Style,
) -> (Span<'a>, Vec<Vec<Span<'static>>>) {
    let name = Span::styled(name, name_style);

    let mut labels = vec![];

    match room_membership {
        MatrixRoomState::Joined => {},
        MatrixRoomState::Left => labels.push(vec![Span::styled("Left", tags_style)]),
        MatrixRoomState::Banned => labels.push(vec![Span::styled("Banned", tags_style)]),
        MatrixRoomState::Knocked => labels.push(vec![Span::styled("Knocked", tags_style)]),
        MatrixRoomState::Invited => labels.push(vec![Span::styled("Invited", tags_style)]),
    }

    if unread.unread_mentions > 0 {
        labels.push(vec![Span::styled("Unread Mention", tags_style)]);
    } else if unread.is_unread() {
        labels.push(vec![Span::styled("Unread", tags_style)]);
    }

    (name, labels)
}

/// Sort `Some` to be less than `None` so that list items with values come before those without.
#[inline]
fn some_cmp<T, F>(a: Option<T>, b: Option<T>, f: F) -> Ordering
where
    F: Fn(&T, &T) -> Ordering,
{
    match (a, b) {
        (Some(a), Some(b)) => f(&a, &b),
        (None, None) => Ordering::Equal,
        (None, Some(_)) => Ordering::Greater,
        (Some(_), None) => Ordering::Less,
    }
}

fn user_cmp(a: &MemberItem, b: &MemberItem, field: &SortFieldUser) -> Ordering {
    let a_id = a.member.user_id();
    let b_id = b.member.user_id();

    match field {
        SortFieldUser::UserId => a_id.cmp(b_id),
        SortFieldUser::LocalPart => a_id.localpart().cmp(b_id.localpart()),
        SortFieldUser::Server => a_id.server_name().cmp(b_id.server_name()),
        SortFieldUser::Knock => {
            // Sort knocks before non-knocks:
            b.is_knock().cmp(&a.is_knock())
        },
        SortFieldUser::Invite => {
            // Sort invites before non-invites:
            b.is_invite().cmp(&a.is_invite())
        },
        SortFieldUser::PowerLevel => {
            // Sort higher power levels towards the top of the list.
            b.member.power_level().cmp(&a.member.power_level())
        },
    }
}

fn room_cmp<T: RoomLikeItem>(
    a: &T,
    b: &T,
    field: &SortFieldRoom,
    collator: &mut Collator,
) -> Ordering {
    match field {
        SortFieldRoom::Favorite => {
            let fava = a.has_tag(TagName::Favorite);
            let favb = b.has_tag(TagName::Favorite);

            // If a has Favorite and b doesn't, it should sort earlier in room list.
            favb.cmp(&fava)
        },
        SortFieldRoom::LowPriority => {
            let lowa = a.has_tag(TagName::LowPriority);
            let lowb = b.has_tag(TagName::LowPriority);

            // If a has LowPriority and b doesn't, it should sort later in room list.
            lowa.cmp(&lowb)
        },
        SortFieldRoom::Name => collator.collate(a.name(), b.name()),
        SortFieldRoom::Alias => some_cmp(a.alias(), b.alias(), Ord::cmp),
        SortFieldRoom::RoomId => a.room_id().cmp(b.room_id()),
        SortFieldRoom::Server => {
            let a = a
                .alias()
                .map(RoomAliasId::server_name)
                .or_else(|| a.room_id().server_name());
            let b = b
                .alias()
                .map(RoomAliasId::server_name)
                .or_else(|| b.room_id().server_name());
            some_cmp(a, b, Ord::cmp)
        },
        SortFieldRoom::Unread => {
            // Sort true (unread) before false (read)
            b.is_unread().cmp(&a.is_unread())
        },
        SortFieldRoom::Recent => {
            // sort larger timestamps towards the top.
            some_cmp(a.recent_ts(), b.recent_ts(), |a, b| b.cmp(a))
        },
        SortFieldRoom::Invite => {
            // sort invites before other rooms.
            b.is_invite().cmp(&a.is_invite())
        },
    }
}

/// Compare two rooms according the configured sort criteria.
fn room_fields_cmp<T: RoomLikeItem>(
    a: &T,
    b: &T,
    fields: &[SortColumn<SortFieldRoom>],
    collator: &mut Collator,
) -> Ordering {
    for SortColumn(field, order) in fields {
        match (room_cmp(a, b, field, collator), order) {
            (Ordering::Equal, _) => continue,
            (o, SortOrder::Ascending) => return o,
            (o, SortOrder::Descending) => return o.reverse(),
        }
    }

    // Break ties on ascending room id.
    room_cmp(a, b, &SortFieldRoom::RoomId, collator)
}

fn user_fields_cmp(
    a: &MemberItem,
    b: &MemberItem,
    fields: &[SortColumn<SortFieldUser>],
) -> Ordering {
    for SortColumn(field, order) in fields {
        match (user_cmp(a, b, field), order) {
            (Ordering::Equal, _) => continue,
            (o, SortOrder::Ascending) => return o,
            (o, SortOrder::Descending) => return o.reverse(),
        }
    }

    // Break ties on ascending user id.
    user_cmp(a, b, &SortFieldUser::UserId)
}

fn tag_to_span(tag: &TagName, style: Style) -> Vec<Span<'_>> {
    match tag {
        TagName::Favorite => vec![Span::styled("Favorite", style)],
        TagName::LowPriority => vec![Span::styled("Low Priority", style)],
        TagName::ServerNotice => vec![Span::styled("Server Notice", style)],
        TagName::User(tag) => {
            vec![
                Span::styled("User Tag: ", style),
                Span::styled(tag.as_ref(), style),
            ]
        },
        tag => vec![Span::styled(format!("{tag:?}"), style)],
    }
}

fn append_tags<'a>(tags: Vec<Vec<Span<'a>>>, spans: &mut Vec<Span<'a>>, style: Style) {
    if tags.is_empty() {
        return;
    }

    spans.push(Span::styled(" (", style));

    for (i, tag) in tags.into_iter().enumerate() {
        if i > 0 {
            spans.push(Span::styled(", ", style));
        }

        spans.extend(tag);
    }

    spans.push(Span::styled(")", style));
}

trait RoomLikeItem {
    fn room_id(&self) -> &RoomId;
    fn has_tag(&self, tag: TagName) -> bool;
    fn is_unread(&self) -> bool;
    fn recent_ts(&self) -> Option<&MessageTimeStamp>;
    fn alias(&self) -> Option<&RoomAliasId>;
    fn name(&self) -> &str;
    fn is_invite(&self) -> bool;
    fn has_mention(&self) -> bool;
}

#[inline]
fn room_prompt(
    room_id: &RoomId,
    act: &PromptAction,
    ctx: &ProgramContext,
) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
    match act {
        PromptAction::Submit => {
            let room = IambId::Room(room_id.to_owned().into(), RoomView::Main);
            let open = WindowAction::Switch(OpenTarget::Application(room));
            let acts = vec![(open.into(), ctx.clone())];

            Ok(acts)
        },
        PromptAction::Abort(_) => {
            let msg = "Cannot abort entry inside a list";
            let err = EditError::Failure(msg.into());

            Err(err)
        },
        PromptAction::Recall(..) => {
            let msg = "Cannot recall history inside a list";
            let err = EditError::Failure(msg.into());

            Err(err)
        },
    }
}

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s {
            IambWindow::Room($id) => $e,
            IambWindow::DirectList($id) => $e,
            IambWindow::MemberList($id, _, _) => $e,
            IambWindow::PinnedList($id, _, _) => $e,
            IambWindow::RoomList($id) => $e,
            IambWindow::SpaceList($id) => $e,
            IambWindow::VerifyList($id) => $e,
            IambWindow::Welcome($id) => $e,
            IambWindow::ChatList($id) => $e,
            IambWindow::UnreadList($id) => $e,
            IambWindow::MentionsList($id) => $e,
            IambWindow::InvitesList($id) => $e,
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

pub enum IambWindow {
    DirectList(RoomListState),
    MemberList(MemberListState, OwnedRoomId, Option<Instant>),
    PinnedList(PinnedListState, OwnedRoomId, Option<Instant>),
    Room(RoomState),
    VerifyList(VerifyListState),
    RoomList(RoomListState),
    SpaceList(RoomListState),
    Welcome(WelcomeState),
    ChatList(RoomListState),
    UnreadList(RoomListState),
    MentionsList(RoomListState),
    InvitesList(RoomListState),
}

impl IambWindow {
    pub fn focus_toggle(&mut self) {
        if let IambWindow::Room(w) = self {
            w.focus_toggle()
        } else {
            return;
        }
    }

    pub async fn timeline_command(
        &mut self,
        act: TimelineAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let IambWindow::Room(w) = self {
            w.timeline_command(act, ctx, store).await
        } else {
            Err(IambError::NoSelectedRoom.into())
        }
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, EditContext)>> {
        if let IambWindow::Room(w) = self {
            w.message_command(act, ctx, store).await
        } else {
            return Err(IambError::NoSelectedRoom.into());
        }
    }

    pub async fn space_command(
        &mut self,
        act: SpaceAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let IambWindow::Room(w) = self {
            w.space_command(act, ctx, store).await
        } else {
            return Err(IambError::NoSelectedRoom.into());
        }
    }

    pub async fn room_command(
        &mut self,
        act: RoomAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        let id = match self {
            IambWindow::Room(state) => state.id(store),
            IambWindow::MemberList(_, room_id, _) => Some(&**room_id),
            IambWindow::PinnedList(_, room_id, _) => Some(&**room_id),

            IambWindow::DirectList(state) => state.get().map(|state| state.room_id()),
            IambWindow::RoomList(state) => state.get().map(|state| state.room_id()),
            IambWindow::SpaceList(state) => state.get().map(|state| state.room_id()),
            IambWindow::ChatList(state) | IambWindow::UnreadList(state) => {
                state.get().map(|state| state.room_id())
            },

            _ => None,
        };

        let Some(id) = id else {
            return Err(IambError::NoSelectedRoomOrSpace.into());
        };
        let id = id.to_owned();

        let worker = &store.application.worker;
        match act {
            RoomAction::Follow(cmd, dir) => {
                let room = worker.client.get_room(&id).ok_or(IambError::NotJoined)?;
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

                let id = IambId::Room(room_id.into(), RoomView::Main);
                let target = OpenTarget::Application(id);
                let act = cmd.switch(target);

                Ok(vec![(act, cmd.context.clone())])
            },
            RoomAction::InviteAccept => {
                if let Some(room) = worker.client.get_room(&id) {
                    room.join().await.map_err(IambError::from)?;
                    Ok(vec![])
                } else {
                    Err(IambError::NotInvited.into())
                }
            },
            RoomAction::InviteReject => {
                if let Some(room) = worker.client.get_room(&id) {
                    room.leave().await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    Err(IambError::NotInvited.into())
                }
            },
            RoomAction::InviteSend(user) => {
                if let Some(room) = worker.client.get_room(&id) {
                    room.invite_user_by_id(user.as_ref()).await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    Err(IambError::NotJoined.into())
                }
            },
            RoomAction::KnockAccept(user) => {
                let room = worker.client.get_room(&id).ok_or(IambError::NotJoined)?;
                room.invite_user_by_id(&user).await.map_err(IambError::from)?;
                Ok(vec![])
            },
            RoomAction::KnockReject(user, reason) => {
                let room = worker.client.get_room(&id).ok_or(IambError::NotJoined)?;
                room.kick_user(&user, reason.as_deref()).await.map_err(IambError::from)?;
                Ok(vec![])
            },
            RoomAction::KnockBan(user, reason) => {
                let room = worker.client.get_room(&id).ok_or(IambError::NotJoined)?;
                room.ban_user(&user, reason.as_deref()).await.map_err(IambError::from)?;
                Ok(vec![])
            },
            RoomAction::Leave(skip_confirm) => {
                if let Some(room) = worker.client.get_room(&id) {
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
                let Some(room) = worker.client.get_room(&id) else {
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
                        room.ban_user(&user_id, reason.as_deref())
                            .await
                            .map_err(IambError::from)?;
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
                let id = IambId::MemberList(id);
                let target = OpenTarget::Application(id);
                let cmd = cmd.default_relation(MoveDir1D::Next);

                let act = match store.application.settings.tunables.members_split {
                    Some(dir) => cmd.default_axis(dir.to_axis()).window(target, None),
                    None => cmd.switch(target),
                };

                Ok(vec![(act, cmd.context.clone())])
            },
            RoomAction::Message(cmd) => {
                let msg_id = match self {
                    IambWindow::Room(RoomState::Chat(chat)) => chat.current_message(store),
                    IambWindow::Room(RoomState::Message(message)) => Some(message.id().to_owned()),
                    _ => None,
                };
                let Some(msg_id) = msg_id else {
                    return Err(UIError::Failure("No message selected".into()));
                };
                let act = Action::Window(WindowAction::Switch(OpenTarget::Application(
                    IambId::Room(id.into(), RoomView::Message(msg_id)),
                )));

                Ok(vec![(act, cmd.context.clone())])
            },
            RoomAction::Pinned(cmd) => {
                let id = IambId::PinnedList(id);
                let target = OpenTarget::Application(id);
                let act = cmd.switch(target);

                Ok(vec![(act, cmd.context.clone())])
            },
            RoomAction::SetAccess(rule) => {
                let room = store
                    .application
                    .get_joined_room(&id)
                    .ok_or(UIError::Application(IambError::NotJoined))?;

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
                    .get_joined_room(&id)
                    .ok_or(UIError::Application(IambError::NotJoined))?;

                room.set_is_direct(is_direct).await.map_err(IambError::from)?;

                Ok(vec![])
            },
            RoomAction::SetUnread(is_unread) => {
                let room = store
                    .application
                    .get_joined_room(&id)
                    .ok_or(UIError::Application(IambError::NotJoined))?;

                room.set_unread_flag(is_unread).await.map_err(IambError::from)?;

                if !is_unread {
                    let user_id = store.application.settings.profile.user_id.clone();
                    let info = store.application.get_room_info(id);
                    let messages = info.get_thread(None).expect("room main timeline doesn't exist");
                    if let Some((key, _)) = messages.last_key_value() &&
                        let Some(event_id) = key.id.as_origin()
                    {
                        info.set_receipt(
                            matrix_sdk::ruma::events::receipt::ReceiptThread::Main,
                            user_id,
                            event_id.to_owned(),
                        );
                    }
                }

                Ok(vec![])
            },
            RoomAction::Set(field, value) => {
                let room = store
                    .application
                    .get_joined_room(&id)
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
                        let notifications = worker.client.notification_settings().await;

                        notifications
                            .set_room_notification_mode(&id, mode)
                            .await
                            .map_err(IambError::from)?;
                    },
                    RoomField::CanonicalAlias => {
                        let client = &mut store.application.worker.client;

                        let Ok(orai) = OwnedRoomAliasId::try_from(value.as_str()) else {
                            let err = IambError::InvalidRoomAlias(value);

                            return Err(err.into());
                        };

                        let mut alt_aliases =
                            room.alt_aliases().into_iter().collect::<HashSet<_>>();
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

                        let mut alt_aliases =
                            room.alt_aliases().into_iter().collect::<HashSet<_>>();
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
                    .get_joined_room(&id)
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
                        let notifications = worker.client.notification_settings().await;

                        notifications
                            .delete_user_defined_room_rules(&id)
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
                        worker
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
                        worker.client.remove_room_alias(&orai).await.map_err(IambError::from)?;
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
                    .get_joined_room(&id)
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
                        let notifications = worker.client.notification_settings().await;
                        let mode = notifications.get_user_defined_room_notification_mode(&id).await;

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
                        let Some(member) =
                            room.get_member(user_id).await.map_err(IambError::from)?
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
                let room = worker.client.get_room(&id).ok_or(IambError::NotJoined)?;
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
                let mut request = UpgradeRoomRequest::new(id, new_version);
                request.additional_creators = additional_creators;

                let response = worker.client.send(request).await.map_err(IambError::from)?;
                let id = IambId::Room(response.replacement_room.into(), RoomView::Main);
                let target = OpenTarget::Application(id);
                let act = WindowAction::Switch(target);

                Ok(vec![(act.into(), ctx)])
            },
        }
    }

    pub async fn join_command(
        &mut self,
        act: JoinAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        if let IambWindow::Room(w) = self {
            w.join_command(act, ctx, store).await
        } else {
            return Err(IambError::NoSelectedRoom.into());
        }
    }

    pub async fn send_command(
        &mut self,
        act: SendAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if let IambWindow::Room(w) = self {
            w.send_command(act, ctx, store).await
        } else {
            return Err(IambError::NoSelectedRoom.into());
        }
    }
}

pub type MemberListState = ListState<MemberItem, IambInfo>;

pub type PinnedListState = ListState<PinnedItem, IambInfo>;

pub type RoomListState = ListState<GenericRoomItem, IambInfo>;

pub type VerifyListState = ListState<VerifyItem, IambInfo>;

impl From<RoomState> for IambWindow {
    fn from(room: RoomState) -> Self {
        IambWindow::Room(room)
    }
}

impl From<VerifyListState> for IambWindow {
    fn from(list: VerifyListState) -> Self {
        IambWindow::VerifyList(list)
    }
}

impl From<WelcomeState> for IambWindow {
    fn from(win: WelcomeState) -> Self {
        IambWindow::Welcome(win)
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for IambWindow {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.editor_command(act, ctx, store))
    }
}

impl Jumpable<ProgramContext, IambInfo> for IambWindow {
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

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for IambWindow {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        delegate!(self, w => w.scroll(style, ctx, store))
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for IambWindow {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        delegate!(self, w => w.prompt(act, ctx, store))
    }
}

impl TerminalCursor for IambWindow {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        delegate!(self, w => w.get_term_cursor())
    }

    fn hide_term_cursor(&self) -> bool {
        delegate!(self, w => w.hide_term_cursor())
    }
}

impl WindowOps<IambInfo> for IambWindow {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        let ChatStore {
            collator,
            aliases,
            rooms,
            settings,
            sync_info,
            verifications,
            worker,
            ..
        } = &mut store.application;

        let default_list_style = settings.theme.default;
        let default_rooms_style = settings.theme.rooms.default;

        match self {
            IambWindow::Room(state) => state.draw(area, buf, focused, store),
            IambWindow::DirectList(state) => {
                let mut items = sync_info
                    .dms
                    .iter()
                    .map(|room| GenericRoomItem::new_unspecified(room, rooms, aliases))
                    .collect::<Vec<_>>();
                let fields = &settings.tunables.sort.dms;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("No direct messages yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_rooms_style)
                    .render(area, buf, state);
            },
            IambWindow::MemberList(state, room_id, last_fetch) => {
                let need_fetch = match last_fetch {
                    Some(i) => i.elapsed() >= MEMBER_FETCH_DEBOUNCE,
                    None => true,
                };

                if need_fetch && let Ok(mems) = worker.members(room_id.clone()) {
                    let mut items = mems
                        .into_iter()
                        .map(|m| MemberItem::new(m, room_id.clone()))
                        .collect::<Vec<_>>();
                    let fields = &settings.tunables.sort.members;
                    items.sort_by(|a, b| user_fields_cmp(a, b, fields));
                    state.set(items);
                    *last_fetch = Some(Instant::now());
                }

                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("No users here yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_list_style)
                    .render(area, buf, state);
            },
            IambWindow::PinnedList(state, room_id, last_fetch) => {
                let info = store.application.rooms.get_or_default(room_id.clone());

                // Most recently pinned first.
                let items = info
                    .pinned_events
                    .iter()
                    .rev()
                    .map(|event_id| PinnedItem::new(room_id.clone(), event_id.clone()))
                    .collect::<Vec<_>>();

                let need_fetch = last_fetch.is_none_or(|i| i.elapsed() >= MEMBER_FETCH_DEBOUNCE);

                if need_fetch && !info.missing_pinned().is_empty() {
                    store.application.need_load.need_pinned(room_id.clone());
                    *last_fetch = Some(Instant::now());
                }

                state.set(items);
                state.set_ignorecase(store.application.settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("No pinned messages in this room")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_list_style)
                    .render(area, buf, state);
            },
            IambWindow::RoomList(state) => {
                let mut items = sync_info
                    .rooms
                    .iter()
                    .map(|room| GenericRoomItem::new_unspecified(room, rooms, aliases))
                    .collect::<Vec<_>>();
                let fields = &settings.tunables.sort.rooms;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("You haven't joined any rooms yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_rooms_style)
                    .render(area, buf, state);
            },
            IambWindow::ChatList(state) => {
                let mut items = sync_info
                    .rooms
                    .iter()
                    .chain(sync_info.dms.iter())
                    .map(|room| GenericRoomItem::new(room, rooms, aliases))
                    .collect::<Vec<_>>();

                let fields = &settings.tunables.sort.chats;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("You do not have rooms or dms yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_rooms_style)
                    .render(area, buf, state);
            },
            IambWindow::UnreadList(state) => {
                let mut items = sync_info
                    .rooms
                    .iter()
                    .chain(sync_info.dms.iter())
                    .map(|room| GenericRoomItem::new(room, rooms, aliases))
                    .filter(RoomLikeItem::is_unread)
                    .collect::<Vec<_>>();

                let fields = &settings.tunables.sort.chats;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("You do not have any unreads yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_rooms_style)
                    .render(area, buf, state);
            },
            IambWindow::MentionsList(state) => {
                let mut items = sync_info
                    .rooms
                    .iter()
                    .chain(sync_info.dms.iter())
                    .map(|room| GenericRoomItem::new(room, rooms, aliases))
                    .filter(RoomLikeItem::has_mention)
                    .collect::<Vec<_>>();

                let fields = &settings.tunables.sort.chats;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("You do not have any unread mentions yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_rooms_style)
                    .render(area, buf, state);
            },
            IambWindow::InvitesList(state) => {
                let mut items = sync_info
                    .rooms
                    .iter()
                    .chain(sync_info.dms.iter())
                    .map(|room| GenericRoomItem::new(room, rooms, aliases))
                    .filter(RoomLikeItem::is_invite)
                    .collect::<Vec<_>>();

                let fields = &settings.tunables.sort.chats;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("You do not have any open invites")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_rooms_style)
                    .render(area, buf, state);
            },
            IambWindow::SpaceList(state) => {
                let mut items = sync_info
                    .spaces
                    .iter()
                    .map(|room| GenericRoomItem::new_unspecified(room, rooms, aliases))
                    .collect::<Vec<_>>();

                let fields = &settings.tunables.sort.spaces;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("You haven't joined any spaces yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_rooms_style)
                    .render(area, buf, state);
            },
            IambWindow::VerifyList(state) => {
                let mut items = verifications
                    .iter()
                    .map(|(_, req)| VerifyItem::new(req.to_owned()))
                    .collect::<Vec<_>>();

                // Sort the active verifications towards the top.
                items.sort();

                if let Some(item) = items.first_mut() {
                    item.show_help();
                }

                state.set(items);
                state.set_ignorecase(settings.tunables.ignorecase);

                List::new(store)
                    .empty_message("No in-progress verifications")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .style(default_list_style)
                    .render(area, buf, state);
            },
            IambWindow::Welcome(state) => state.draw(area, buf, focused, store),
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            IambWindow::Room(w) => w.dup(store).into(),
            IambWindow::DirectList(w) => Self::DirectList(w.dup(store)),
            IambWindow::MemberList(w, room_id, last_fetch) => {
                IambWindow::MemberList(w.dup(store), room_id.clone(), *last_fetch)
            },
            IambWindow::PinnedList(w, room_id, last_fetch) => {
                IambWindow::PinnedList(w.dup(store), room_id.clone(), *last_fetch)
            },
            IambWindow::RoomList(w) => Self::RoomList(w.dup(store)),
            IambWindow::SpaceList(w) => Self::SpaceList(w.dup(store)),
            IambWindow::VerifyList(w) => w.dup(store).into(),
            IambWindow::Welcome(w) => w.dup(store).into(),
            IambWindow::ChatList(w) => Self::ChatList(w.dup(store)),
            IambWindow::UnreadList(w) => Self::UnreadList(w.dup(store)),
            IambWindow::MentionsList(w) => Self::MentionsList(w.dup(store)),
            IambWindow::InvitesList(w) => Self::InvitesList(w.dup(store)),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        delegate!(self, w => w.close(flags, store))
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        delegate!(self, w => w.write(path, flags, store))
    }

    fn get_completions(&self) -> Option<CompletionList> {
        delegate!(self, w => w.get_completions())
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        delegate!(self, w => w.get_cursor_word(style))
    }

    fn get_selected_word(&self) -> Option<String> {
        delegate!(self, w => w.get_selected_word())
    }
}

impl Window<IambInfo> for IambWindow {
    fn id(&self) -> IambId {
        match self {
            IambWindow::Room(room) => room.window_id(),
            IambWindow::DirectList(_) => IambId::DirectList,
            IambWindow::MemberList(_, room_id, _) => IambId::MemberList(room_id.clone()),
            IambWindow::PinnedList(_, room_id, _) => IambId::PinnedList(room_id.clone()),
            IambWindow::RoomList(_) => IambId::RoomList,
            IambWindow::SpaceList(_) => IambId::SpaceList,
            IambWindow::VerifyList(_) => IambId::VerifyList,
            IambWindow::Welcome(_) => IambId::Welcome,
            IambWindow::ChatList(_) => IambId::ChatList,
            IambWindow::UnreadList(_) => IambId::UnreadList,
            IambWindow::MentionsList(_) => IambId::MentionsList,
            IambWindow::InvitesList(_) => IambId::InvitesList,
        }
    }

    fn get_tab_title(&self, store: &mut ProgramStore) -> Line<'_> {
        match self {
            IambWindow::DirectList(_) => Line::from("Direct Messages"),
            IambWindow::RoomList(_) => Line::from("Rooms"),
            IambWindow::SpaceList(_) => Line::from("Spaces"),
            IambWindow::VerifyList(_) => Line::from("Verifications"),
            IambWindow::Welcome(_) => Line::from("Welcome to iamb"),
            IambWindow::ChatList(_) => Line::from("DMs & Rooms"),
            IambWindow::UnreadList(_) => Line::from("Unread Messages"),
            IambWindow::MentionsList(_) => Line::from("Unread Mentions"),
            IambWindow::InvitesList(_) => Line::from("Open Invites"),

            IambWindow::Room(w) => w.get_tab_title(store),
            IambWindow::MemberList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    Span::raw("Room Members "),
                    Span::raw(format!("({n}): ")),
                    title.into(),
                ];
                Line::from(v)
            },
            IambWindow::PinnedList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    Span::raw("Pinned Messages "),
                    Span::raw(format!("({n}): ")),
                    Span::raw(title),
                ];
                Line::from(v)
            },
        }
    }

    fn get_win_title(&self, store: &mut ProgramStore) -> Line<'_> {
        let style = store.application.settings.theme.windows.title;
        let default_style = store.application.settings.theme.windows.default;

        match self {
            IambWindow::DirectList(_) => Line::styled("Direct Messages", style),
            IambWindow::RoomList(_) => Line::styled("Rooms", style),
            IambWindow::SpaceList(_) => Line::styled("Spaces", style),
            IambWindow::VerifyList(_) => Line::styled("Verifications", style),
            IambWindow::Welcome(_) => Line::styled("Welcome to iamb", style),
            IambWindow::ChatList(_) => Line::styled("DMs & Rooms", style),
            IambWindow::UnreadList(_) => Line::styled("Unread Messages", style),
            IambWindow::MentionsList(_) => Line::styled("Unread Mentions", style),
            IambWindow::InvitesList(_) => Line::styled("Open Invites", style),

            IambWindow::Room(w) => w.get_title(store),
            IambWindow::MemberList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    Span::styled("Room Members ", style),
                    Span::styled(format!("({n}): "), style),
                    Span::styled(title, default_style),
                ];
                Line::from(v)
            },
            IambWindow::PinnedList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    Span::styled("Pinned Messages ", style),
                    Span::styled(format!("({n}): "), style),
                    Span::styled(title, default_style),
                ];
                Line::from(v)
            },
        }
    }

    fn open(id: IambId, store: &mut ProgramStore) -> IambResult<Self> {
        match id {
            IambId::Room(alias_id, thread) => {
                let alias: &RoomOrAliasId = &alias_id;
                let room_id = match <&RoomId>::try_from(alias) {
                    Ok(room_id) => room_id,
                    Err(alias) => {
                        if let Some(room_id) = store.application.aliases.get(alias) {
                            room_id
                        } else {
                            return Ok(RoomState::not_joined(alias_id, store).into());
                        }
                    },
                };

                if let Some(room) = store.application.worker.client.get_room(room_id) {
                    store.application.need_load.need_members(room_id.to_owned());

                    let room = RoomState::new(room, thread, store);

                    return Ok(room.into());
                }

                return Ok(RoomState::not_joined(alias_id, store).into());
            },
            IambId::DirectList => {
                let list = RoomListState::new(IambBufferId::DirectList, vec![]);

                return Ok(Self::DirectList(list));
            },
            IambId::MemberList(room_id) => {
                let id = IambBufferId::MemberList(room_id.clone());
                let list = MemberListState::new(id, vec![]);
                let win = Self::MemberList(list, room_id, None);

                return Ok(win);
            },
            IambId::PinnedList(room_id) => {
                let id = IambBufferId::PinnedList(room_id.clone());
                let list = PinnedListState::new(id, vec![]);
                let win = IambWindow::PinnedList(list, room_id, None);

                return Ok(win);
            },
            IambId::RoomList => {
                let list = RoomListState::new(IambBufferId::RoomList, vec![]);

                return Ok(Self::RoomList(list));
            },
            IambId::SpaceList => {
                let list = RoomListState::new(IambBufferId::SpaceList, vec![]);

                return Ok(Self::SpaceList(list));
            },
            IambId::VerifyList => {
                let list = VerifyListState::new(IambBufferId::VerifyList, vec![]);

                return Ok(list.into());
            },
            IambId::Welcome => {
                let win = WelcomeState::new(store);

                return Ok(win.into());
            },
            IambId::ChatList => {
                let list = RoomListState::new(IambBufferId::ChatList, vec![]);

                return Ok(Self::ChatList(list));
            },
            IambId::UnreadList => {
                let list = RoomListState::new(IambBufferId::UnreadList, vec![]);

                Ok(Self::UnreadList(list))
            },
            IambId::MentionsList => {
                let list = RoomListState::new(IambBufferId::MentionsList, vec![]);

                Ok(Self::MentionsList(list))
            },
            IambId::InvitesList => {
                let list = RoomListState::new(IambBufferId::InvitesList, vec![]);

                Ok(Self::InvitesList(list))
            },
        }
    }

    fn find(name: String, store: &mut ProgramStore) -> IambResult<Self> {
        let room_alias = if let Ok(alias) = <&RoomAliasId>::try_from(name.as_str()) {
            if let Some(room_id) = store.application.aliases.get(alias) {
                room_id.to_owned().into()
            } else {
                alias.to_owned().into()
            }
        } else if let Ok(room_id) = <&RoomId>::try_from(name.as_str()) {
            room_id.to_owned().into()
        } else if let Ok(user_id) = <&UserId>::try_from(name.as_str()) {
            if let Some(dm) = store.application.worker.client.get_dm_room(user_id) {
                dm.room_id().to_owned().into()
            } else {
                store.application.worker.create_dm(user_id.to_owned())?.into()
            }
        } else {
            // XXX: support passing matrix uris to `:join`

            return Err(UIError::Failure("Could not parse room identifier".to_string()));
        };

        let id = IambId::Room(room_alias, RoomView::Main);
        IambWindow::open(id, store)
    }

    fn posn(index: usize, _: &mut ProgramStore) -> IambResult<Self> {
        let msg = format!("Cannot find indexed buffer (index = {index})");
        let err = UIError::Unimplemented(msg);

        Err(err)
    }

    fn unnamed(store: &mut ProgramStore) -> IambResult<Self> {
        Self::open(IambId::RoomList, store)
    }
}

/// This is used to determine what tag to show on a [`GenericRoomItem`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum RoomType {
    DM,
    Room,
    Space,

    /// Don't show a tag
    Unspecified,
}

impl RoomType {
    fn text(self) -> Option<&'static str> {
        match self {
            RoomType::DM => Some("DM"),
            RoomType::Room => Some("Room"),
            RoomType::Space => Some("Space"),
            RoomType::Unspecified => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericRoomItem {
    room_id: OwnedRoomId,
    name: String,
    alias: Option<OwnedRoomAliasId>,
    tags: Option<Tags>,
    membership: MatrixRoomState,
    unread: UnreadInfo,

    room_type: RoomType,
}

impl GenericRoomItem {
    pub fn new_unspecified(
        room: &MatrixRoom,
        rooms: &mut CompletionMap<OwnedRoomId, RoomInfo>,
        names: &mut CompletionMap<OwnedRoomAliasId, OwnedRoomId>,
    ) -> Self {
        let room_id = room.room_id().to_owned();

        let info = rooms.get_or_default(room_id.to_owned());
        let name = info.name.clone().unwrap_or_default();
        let alias = room.canonical_alias();
        let unread = info.unreads(room);
        let tags = info.tags.clone();

        if let Some(alias) = &alias {
            names.insert(alias.to_owned(), room_id.to_owned());
        }

        Self {
            name,
            room_id,
            alias,
            tags,
            unread,
            membership: room.state(),
            room_type: RoomType::Unspecified,
        }
    }

    pub fn new(
        room: &MatrixRoom,
        rooms: &mut CompletionMap<OwnedRoomId, RoomInfo>,
        names: &mut CompletionMap<OwnedRoomAliasId, OwnedRoomId>,
    ) -> Self {
        let room_type = if room.is_space() {
            RoomType::Space
        } else if room.is_dm() {
            RoomType::DM
        } else {
            RoomType::Room
        };

        assign!(Self::new_unspecified(room, rooms, names), { room_type })
    }
}

impl RoomLikeItem for GenericRoomItem {
    fn room_id(&self) -> &RoomId {
        &self.room_id
    }

    fn has_tag(&self, tag: TagName) -> bool {
        self.tags.as_ref().is_some_and(|tags| tags.contains_key(&tag))
    }

    fn is_unread(&self) -> bool {
        // XXX: check space children for space
        self.unread.is_unread()
    }

    fn recent_ts(&self) -> Option<&MessageTimeStamp> {
        // XXX: check space children for space
        self.unread.latest()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn is_invite(&self) -> bool {
        self.membership == MatrixRoomState::Invited
    }

    fn has_mention(&self) -> bool {
        // XXX: check space children for space
        self.unread.has_mention()
    }
}

impl Display for GenericRoomItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl ListItem<IambInfo> for GenericRoomItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        store: &mut ProgramStore,
    ) -> Text<'_> {
        let theme = &store.application.settings.theme;

        let name_style = if self.unread.is_unread() {
            theme.rooms.unread
        } else {
            theme.rooms.default
        };

        let name_style = selected_style(selected, name_style);
        let tags_style = selected_style(selected, theme.rooms.labels);
        let (name, mut labels) =
            name_and_labels(&self.name, &self.unread, self.membership, name_style, tags_style);
        let mut spans = vec![name];

        if let Some(label) = self.room_type.text() {
            labels.push(vec![Span::styled(label, tags_style)]);
        }

        if let Some(tags) = &self.tags {
            labels.extend(tags.keys().map(|t| tag_to_span(t, tags_style)));
        }

        append_tags(labels, &mut spans, tags_style);
        Text::from(Line::from(spans))
    }

    fn get_word(&self) -> Option<String> {
        // Return the room identifier so that `gf`/`<C-W>gf`/etc. go to the room:
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for GenericRoomItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        room_prompt(self.room_id(), act, ctx)
    }
}

#[derive(Clone)]
pub struct MemberItem {
    member: RoomMember,
    room_id: OwnedRoomId,
}

impl MemberItem {
    fn new(member: RoomMember, room_id: OwnedRoomId) -> Self {
        Self { member, room_id }
    }

    fn is_knock(&self) -> bool {
        self.member.membership() == &MembershipState::Knock
    }

    fn is_invite(&self) -> bool {
        self.member.membership() == &MembershipState::Invite
    }
}

impl Display for MemberItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.member.user_id())
    }
}

impl ListItem<IambInfo> for MemberItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        store: &mut ProgramStore,
    ) -> Text<'_> {
        use matrix_sdk::ruma::events::room::power_levels::UserPowerLevel;

        let info = store.application.rooms.get_or_default(self.room_id.clone());
        let user_id = self.member.user_id();

        let theme = &store.application.settings.theme;

        let (color, name) = store.application.settings.get_user_overrides(self.member.user_id());
        let user_style = theme.users.style(user_id.as_str(), color);
        let color = user_style.fg.unwrap_or(Color::Reset);

        let style = if selected {
            // Ensure the whole item has the same color as `user_style` when it's selected:
            theme.default.fg(color).add_modifier(StyleModifier::REVERSED)
        } else {
            theme.default
        };

        let role_style = style.add_modifier(StyleModifier::BOLD);
        let user_style = style.patch(user_style);

        let mut spans = vec![];
        let mut tags = vec![];

        if let Some(name) = name {
            spans.push(Span::styled(name, user_style));
            tags.push(Span::styled(user_id.as_str(), user_style));
        } else if let Some(display) = info.display_names.get(user_id) {
            spans.push(Span::styled(display.into_owned(), user_style));
            tags.push(Span::styled(user_id.as_str(), user_style));
        } else {
            spans.push(Span::styled(user_id.as_str(), user_style));
        }

        let roles = match self.member.power_level() {
            UserPowerLevel::Infinite => {
                vec![
                    Span::styled("Admin", role_style),
                    Span::styled("Creator", role_style),
                ]
            },
            UserPowerLevel::Int(n) => {
                match i64::from(n) {
                    0 => vec![],
                    50 => vec![Span::styled("Moderator", role_style)],
                    100 => vec![Span::styled("Admin", role_style)],
                    _ => {
                        let custom = format!("Power Level {n}");
                        vec![Span::styled(custom, role_style)]
                    },
                }
            },
            _ => vec![],
        };

        let state = match self.member.membership() {
            MembershipState::Ban => Span::styled("banned", style.fg(Color::LightRed)).into(),
            MembershipState::Invite => Span::styled("invited", style).into(),
            MembershipState::Knock => Span::styled("wants to join", style).into(),
            MembershipState::Leave => Span::styled("left", style).into(),
            MembershipState::Join => None,
            _ => None,
        };

        tags.extend(roles);
        tags.extend(state);

        if !tags.is_empty() {
            spans.push(Span::styled(" (", style));
            for (i, tag) in tags.into_iter().enumerate() {
                if i > 0 {
                    spans.push(Span::styled(", ", style));
                }
                spans.push(tag);
            }
            spans.push(Span::styled(")", style));
        }

        return Line::from(spans).into();
    }

    fn get_word(&self) -> Option<String> {
        self.member.user_id().to_string().into()
    }

    fn matches(&self, needle: &regex::Regex) -> bool {
        needle.is_match(self.member.name()) || needle.is_match(self.member.user_id().as_str())
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for MemberItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        match act {
            PromptAction::Submit => Ok(vec![]),
            PromptAction::Abort(_) => {
                let msg = "Cannot abort entry inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            PromptAction::Recall(..) => {
                let msg = "Cannot recall history inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
        }
    }
}

#[derive(Clone)]
pub struct PinnedItem {
    room_id: OwnedRoomId,
    event_id: OwnedEventId,
}

impl PinnedItem {
    fn new(room_id: OwnedRoomId, event_id: OwnedEventId) -> Self {
        Self { room_id, event_id }
    }
}

impl Display for PinnedItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.event_id)
    }
}

impl ListItem<IambInfo> for PinnedItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        store: &mut ProgramStore,
    ) -> Text<'_> {
        let info = store.application.rooms.get_or_default(self.room_id.clone());
        let settings = &store.application.settings;

        let style = store.application.settings.theme.default;
        let style = if selected {
            style.add_modifier(StyleModifier::REVERSED)
        } else {
            style
        };

        let Some(msg) = info.get_pinned(&self.event_id) else {
            let text = if info.pinned_unavailable(&self.event_id) {
                "Unable to load message"
            } else {
                "Loading pinned message..."
            };

            return Span::styled(text, style.fg(Color::Gray)).into();
        };

        let sender = settings.get_user_span(&msg.sender, info);
        let sender = Span::styled(sender.content.into_owned(), sender.style.patch(style));
        let time = format!(" [{}]: ", msg.timestamp.show_datetime());
        let body = msg.event.body().lines().next().unwrap_or_default().to_string();

        Line::from(vec![sender, Span::styled(time, style), Span::styled(body, style)]).into()
    }

    fn get_word(&self) -> Option<String> {
        self.event_id.to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for PinnedItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        match act {
            PromptAction::Submit => {
                let info = store.application.rooms.get_or_default(self.room_id.clone());
                let thread = info
                    .get_message_location(&self.event_id)
                    .and_then(|(thread, _)| thread)
                    .map(ToOwned::to_owned);

                let room = IambId::Room(self.room_id.clone().into(), thread.into());
                let open = WindowAction::Switch(OpenTarget::Application(room));
                let jump = IambAction::from(TimelineAction::GotoEvent(self.event_id.clone()));

                Ok(vec![(open.into(), ctx.clone()), (jump.into(), ctx.clone())])
            },
            PromptAction::Abort(_) => {
                let msg = "Cannot abort entry inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            PromptAction::Recall(..) => {
                let msg = "Cannot recall history inside a list";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use matrix_sdk::ruma::{MilliSecondsSinceUnixEpoch, room_alias_id, server_name};

    #[derive(Debug, Eq, PartialEq)]
    struct TestRoomItem {
        room_id: OwnedRoomId,
        tags: Vec<TagName>,
        alias: Option<OwnedRoomAliasId>,
        name: &'static str,
        unread: UnreadInfo,
        invite: bool,
    }

    impl RoomLikeItem for &TestRoomItem {
        fn room_id(&self) -> &RoomId {
            self.room_id.as_ref()
        }

        fn has_tag(&self, tag: TagName) -> bool {
            self.tags.contains(&tag)
        }

        fn alias(&self) -> Option<&RoomAliasId> {
            self.alias.as_deref()
        }

        fn name(&self) -> &str {
            self.name
        }

        fn recent_ts(&self) -> Option<&MessageTimeStamp> {
            self.unread.latest()
        }

        fn is_unread(&self) -> bool {
            self.unread.is_unread()
        }

        fn is_invite(&self) -> bool {
            self.invite
        }

        fn has_mention(&self) -> bool {
            false
        }
    }

    #[test]
    fn test_list_item_points_to_room() {
        let server = server_name!("example.com");
        let room_id = RoomId::new_v1(server).to_owned();
        let item = GenericRoomItem {
            room_id: room_id.clone(),
            name: "Watercooler Discussion".into(),
            alias: Some(room_alias_id!("#room1:example.com").to_owned()),
            tags: None,
            membership: MatrixRoomState::Joined,
            unread: UnreadInfo::default(),
            room_type: RoomType::Room,
        };

        // This should return the room ID, and not the alias or name:
        assert_eq!(item.get_word(), Some(room_id.to_string()));
    }

    #[test]
    fn test_sort_rooms() {
        let mut collator = Collator::default();
        let collator = &mut collator;
        let server = server_name!("example.com");

        let room1 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![TagName::Favorite],
            alias: Some(room_alias_id!("#room1:example.com").to_owned()),
            name: "Z",
            unread: UnreadInfo::default(),
            invite: false,
        };

        let room2 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: Some(room_alias_id!("#a:example.com").to_owned()),
            name: "Unnamed Room",
            unread: UnreadInfo::default(),
            invite: false,
        };

        let room3 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "Cool Room",
            unread: UnreadInfo::default(),
            invite: false,
        };

        // Sort by Name ascending.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[SortColumn(SortFieldRoom::Name, SortOrder::Ascending)];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room3, &room2, &room1]);

        // Sort by Name descending.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[SortColumn(SortFieldRoom::Name, SortOrder::Descending)];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room1, &room2, &room3]);

        // Sort by Favorite and Alias before Name to show order matters.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[
            SortColumn(SortFieldRoom::Favorite, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Alias, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room1, &room2, &room3]);

        // Now flip order of Favorite with Descending
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[
            SortColumn(SortFieldRoom::Favorite, SortOrder::Descending),
            SortColumn(SortFieldRoom::Alias, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room2, &room3, &room1]);
    }

    #[test]
    fn test_sort_room_recents() {
        let mut collator = Collator::default();
        let collator = &mut collator;
        let server = server_name!("example.com");

        let room1 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "Room 1",
            unread: UnreadInfo {
                latest: None,
                unread_mark: false,
                unread_messages: 0,
                unread_notifications: 0,
                unread_mentions: 0,
            },
            invite: false,
        };

        let room2 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "Room 2",
            unread: UnreadInfo {
                latest: Some(MessageTimeStamp(MilliSecondsSinceUnixEpoch(40u32.into()))),
                unread_mark: false,
                unread_messages: 0,
                unread_notifications: 0,
                unread_mentions: 0,
            },
            invite: false,
        };

        let room3 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "Room 3",
            unread: UnreadInfo {
                latest: Some(MessageTimeStamp(MilliSecondsSinceUnixEpoch(20u32.into()))),
                unread_mark: false,
                unread_messages: 0,
                unread_notifications: 0,
                unread_mentions: 0,
            },
            invite: false,
        };

        // Sort by Recent ascending.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[SortColumn(SortFieldRoom::Recent, SortOrder::Ascending)];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room2, &room3, &room1]);

        // Sort by Recent descending.
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[SortColumn(SortFieldRoom::Recent, SortOrder::Descending)];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room1, &room3, &room2]);
    }

    #[test]
    fn test_sort_room_invites() {
        let mut collator = Collator::default();
        let collator = &mut collator;
        let server = server_name!("example.com");

        let room1 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "Old room 1",
            unread: UnreadInfo::default(),
            invite: false,
        };

        let room2 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "Old room 2",
            unread: UnreadInfo::default(),
            invite: false,
        };

        let room3 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            tags: vec![],
            alias: None,
            name: "New Fancy Room",
            unread: UnreadInfo::default(),
            invite: true,
        };

        // Sort invites first
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[
            SortColumn(SortFieldRoom::Invite, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room3, &room1, &room2]);

        // Sort invites after
        let mut rooms = vec![&room1, &room2, &room3];
        let fields = &[
            SortColumn(SortFieldRoom::Invite, SortOrder::Descending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room1, &room2, &room3]);
    }

    #[test]
    fn sort_room_servers() {
        let mut collator = Collator::default();
        let collator = &mut collator;
        let server1 = server_name!("a.com");
        let server3 = server_name!("c.com");

        // No alias, fallback to namespace of V1 room ID:
        let room1 = TestRoomItem {
            room_id: RoomId::new_v1(server3).to_owned(),
            tags: vec![],
            alias: None,
            name: "Room E",
            unread: UnreadInfo::default(),
            invite: false,
        };

        // Alias and V1 room ID agree:
        let room2 = TestRoomItem {
            room_id: RoomId::new_v1(server1).to_owned(),
            tags: vec![],
            alias: Some(room_alias_id!("#name:a.com").to_owned()),
            name: "Room D",
            unread: UnreadInfo::default(),
            invite: false,
        };

        // Alias, V2 room id:
        let room3 = TestRoomItem {
            room_id: RoomId::new_v2("refhash").unwrap().to_owned(),
            tags: vec![],
            alias: Some(room_alias_id!("#alias:b.com").to_owned()),
            name: "Room C",
            unread: UnreadInfo::default(),
            invite: true,
        };

        // Alias and V2 room ID disagree, alias is used:
        let room4 = TestRoomItem {
            room_id: RoomId::new_v1(server3).to_owned(),
            tags: vec![],
            alias: Some(room_alias_id!("#alias:a.com").to_owned()),
            name: "Room B",
            unread: UnreadInfo::default(),
            invite: true,
        };

        // No alias and V2 room ID:
        let room5 = TestRoomItem {
            room_id: RoomId::new_v2("refhash").unwrap().to_owned(),
            tags: vec![],
            alias: None,
            name: "Room A",
            unread: UnreadInfo::default(),
            invite: true,
        };

        // Sort servers first ascending, name tie breaks:
        let mut rooms = vec![&room1, &room2, &room3, &room4, &room5];
        let fields = &[
            SortColumn(SortFieldRoom::Server, SortOrder::Ascending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room4, &room2, &room3, &room1, &room5]);

        // Sort servers first descending, name tie breaks:
        let mut rooms = vec![&room1, &room2, &room3, &room4, &room5];
        let fields = &[
            SortColumn(SortFieldRoom::Server, SortOrder::Descending),
            SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
        ];
        rooms.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));
        assert_eq!(rooms, vec![&room5, &room1, &room3, &room4, &room2]);
    }

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
