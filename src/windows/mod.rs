//! # Windows for the User Interface
//!
//! This module contains the logic for rendering windows, and handling UI actions that get
//! delegated to individual windows/UI elements (e.g., typing text or selecting a list item).
//!
//! Additionally, some of the iamb commands delegate behaviour to the current UI element. For
//! example, [sending messages][crate::base::SendAction] delegate to the [room window][RoomState],
//! where we have the message bar and room ID easily accessible and resettable.
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::ops::Deref;
use std::sync::Arc;
use std::time::{Duration, Instant};

use matrix_sdk::notification_settings::RoomNotificationMode;
use matrix_sdk::ruma::events::room::canonical_alias::RoomCanonicalAliasEventContent;
use matrix_sdk::ruma::events::room::history_visibility::{
    HistoryVisibility,
    RoomHistoryVisibilityEventContent,
};
use matrix_sdk::ruma::events::room::name::RoomNameEventContent;
use matrix_sdk::ruma::events::room::topic::RoomTopicEventContent;
use matrix_sdk::ruma::events::tag::TagInfo;
use matrix_sdk::{
    RoomState as MatrixRoomState,
    encryption::verification::{SasVerification, format_emojis},
    room::{Room as MatrixRoom, RoomMember},
    ruma::{
        OwnedRoomAliasId,
        OwnedRoomId,
        OwnedUserId,
        RoomAliasId,
        RoomId,
        api::error::ErrorKind as ClientApiErrorKind,
        events::room::member::MembershipState,
        events::tag::{TagName, Tags},
    },
};

use modalkit::keybindings::dialog::PromptYesNo;
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Color, Modifier as StyleModifier, Style},
    text::{Line, Span, Text},
    widgets::StatefulWidget,
};

use modalkit::{
    actions::{
        Action,
        Editable,
        EditorAction,
        Jumpable,
        PromptAction,
        Promptable,
        Scrollable,
        WindowAction,
    },
    editing::completion::CompletionList,
    errors::{EditError, EditResult, UIError},
    prelude::*,
};

use modalkit_ratatui::{
    TermOffset,
    TerminalCursor,
    Window,
    WindowOps,
    list::{List, ListCursor, ListItem, ListState},
};

use crate::base::{
    ChatStore,
    IambAction,
    IambBufferId,
    IambError,
    IambId,
    IambInfo,
    IambResult,
    MemberUpdateAction,
    MessageAction,
    ProgramAction,
    ProgramContext,
    ProgramStore,
    RoomAction,
    RoomField,
    RoomView,
    SendAction,
    SortColumn,
    SortFieldRoom,
    SortFieldUser,
    SortOrder,
    SpaceAction,
    UnreadInfo,
};

use self::{room::RoomState, welcome::WelcomeState};
use crate::message::MessageTimeStamp;
use feruca::Collator;

pub mod room;
pub mod welcome;

type MatrixRoomInfo = Arc<(MatrixRoom, Option<Tags>)>;

const MEMBER_FETCH_DEBOUNCE: Duration = Duration::from_secs(5);

#[inline]
fn bold_style() -> Style {
    Style::default().add_modifier(StyleModifier::BOLD)
}

#[inline]
fn bold_span(s: &str) -> Span<'_> {
    Span::styled(s, bold_style())
}

#[inline]
fn bold_spans(s: &str) -> Line<'_> {
    bold_span(s).into()
}

#[inline]
fn selected_style(selected: bool) -> Style {
    if selected {
        Style::default().add_modifier(StyleModifier::REVERSED)
    } else {
        Style::default()
    }
}

#[inline]
fn selected_span(s: &str, selected: bool) -> Span<'_> {
    Span::styled(s, selected_style(selected))
}

#[inline]
fn selected_text(s: &str, selected: bool) -> Text<'_> {
    Text::from(selected_span(s, selected))
}

fn name_and_labels<'a>(
    name: &'a str,
    unread: &UnreadInfo,
    style: Style,
) -> (Span<'a>, Vec<Vec<Span<'static>>>) {
    // TODO: use different colors for "mention", "notification", "muted room"
    let name_style = if unread.is_unread() {
        style.add_modifier(StyleModifier::BOLD)
    } else {
        style
    };

    let name = Span::styled(name, name_style);

    let mut labels = vec![];

    if unread.unread_mentions > 0 {
        labels.push(vec![Span::styled("Unread Mention", style)]);
    } else if unread.is_unread() {
        labels.push(vec![Span::styled("Unread", style)]);
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
}

#[inline]
fn room_prompt(
    room_id: &RoomId,
    act: &PromptAction,
    ctx: &ProgramContext,
) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
    match act {
        PromptAction::Submit => {
            let room = IambId::Room(room_id.to_owned(), RoomView::Main);
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
            IambWindow::RoomList($id) => $e,
            IambWindow::SpaceList($id) => $e,
            IambWindow::VerifyList($id) => $e,
            IambWindow::Welcome($id) => $e,
            IambWindow::ChatList($id) => $e,
            IambWindow::UnreadList($id) => $e,
            IambWindow::MentionsList($id) => $e,
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
    DirectList(DirectListState),
    MemberList(MemberListState, OwnedRoomId, Option<Instant>),
    Room(RoomState),
    VerifyList(VerifyListState),
    RoomList(RoomListState),
    SpaceList(SpaceListState),
    Welcome(WelcomeState),
    ChatList(ChatListState),
    UnreadList(UnreadListState),
    MentionsList(MentionsListState),
}

impl IambWindow {
    pub fn focus_toggle(&mut self) {
        if let IambWindow::Room(w) = self {
            w.focus_toggle()
        } else {
            return;
        }
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
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
        &self,
        act: RoomAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        let id = match self {
            IambWindow::Room(state) => Some(state.id()),
            IambWindow::MemberList(_, room_id, _) => Some(&**room_id),

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

        match act {
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
                let id = IambId::MemberList(id.to_owned());
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
                    IambId::Room(id.to_owned(), RoomView::Message(msg_id)),
                )));

                Ok(vec![(act, cmd.context.clone())])
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
                    let user_id = store.application.settings.profile.user_id.clone();
                    let info = store.application.get_room_info(id.to_owned());
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
                    RoomField::Aliases => {
                        // This never happens, aliases is only used for showing
                    },
                    RoomField::Id => {
                        // This never happens, id is only used for showing
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
                    RoomField::Aliases => {
                        // This will not happen, you cannot unset all aliases
                    },
                    RoomField::Id => {
                        // This never happens, id is only used for showing
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
                };

                let msg = InfoMessage::Pager(msg);
                let act = Action::ShowInfoMessage(msg);

                Ok(vec![(act, ctx)])
            },
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

pub type DirectListState = ListState<DirectItem, IambInfo>;
pub type MemberListState = ListState<MemberItem, IambInfo>;
pub type RoomListState = ListState<RoomItem, IambInfo>;
pub type ChatListState = ListState<GenericChatItem, IambInfo>;
pub type UnreadListState = ListState<GenericChatItem, IambInfo>;
pub type MentionsListState = ListState<GenericChatItem, IambInfo>;
pub type SpaceListState = ListState<SpaceItem, IambInfo>;
pub type VerifyListState = ListState<VerifyItem, IambInfo>;

impl From<ChatListState> for IambWindow {
    fn from(list: ChatListState) -> Self {
        IambWindow::ChatList(list)
    }
}

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

impl From<DirectListState> for IambWindow {
    fn from(list: DirectListState) -> Self {
        IambWindow::DirectList(list)
    }
}

impl From<RoomListState> for IambWindow {
    fn from(list: RoomListState) -> Self {
        IambWindow::RoomList(list)
    }
}

impl From<SpaceListState> for IambWindow {
    fn from(list: SpaceListState) -> Self {
        IambWindow::SpaceList(list)
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
}

impl WindowOps<IambInfo> for IambWindow {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        match self {
            IambWindow::Room(state) => state.draw(area, buf, focused, store),
            IambWindow::DirectList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .dms
                    .clone()
                    .into_iter()
                    .map(|room_info| DirectItem::new(room_info, store))
                    .collect::<Vec<_>>();
                let fields = &store.application.settings.tunables.sort.dms;
                let collator = &mut store.application.collator;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);

                List::new(store)
                    .empty_message("No direct messages yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::MemberList(state, room_id, last_fetch) => {
                let need_fetch = match last_fetch {
                    Some(i) => i.elapsed() >= MEMBER_FETCH_DEBOUNCE,
                    None => true,
                };

                if need_fetch && let Ok(mems) = store.application.worker.members(room_id.clone()) {
                    let mut items = mems
                        .into_iter()
                        .map(|m| MemberItem::new(m, room_id.clone()))
                        .collect::<Vec<_>>();
                    let fields = &store.application.settings.tunables.sort.members;
                    items.sort_by(|a, b| user_fields_cmp(a, b, fields));
                    state.set(items);
                    *last_fetch = Some(Instant::now());
                }

                List::new(store)
                    .empty_message("No users here yet!")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::RoomList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .rooms
                    .clone()
                    .into_iter()
                    .map(|room_info| RoomItem::new(room_info, store))
                    .collect::<Vec<_>>();
                let fields = &store.application.settings.tunables.sort.rooms;
                let collator = &mut store.application.collator;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);

                List::new(store)
                    .empty_message("You haven't joined any rooms yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::ChatList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .rooms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, false))
                    .collect::<Vec<_>>();

                let dms = store
                    .application
                    .sync_info
                    .dms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, true));

                items.extend(dms);

                let fields = &store.application.settings.tunables.sort.chats;
                let collator = &mut store.application.collator;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);

                List::new(store)
                    .empty_message("You do not have rooms or dms yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::UnreadList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .rooms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, false))
                    .filter(RoomLikeItem::is_unread)
                    .collect::<Vec<_>>();

                let dms = store
                    .application
                    .sync_info
                    .dms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, true))
                    .filter(RoomLikeItem::is_unread);

                items.extend(dms);

                let fields = &store.application.settings.tunables.sort.chats;
                let collator = &mut store.application.collator;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);

                List::new(store)
                    .empty_message("You do not have any unreads yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::MentionsList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .rooms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, false))
                    .filter(GenericChatItem::has_mention)
                    .collect::<Vec<_>>();

                let dms = store
                    .application
                    .sync_info
                    .dms
                    .clone()
                    .into_iter()
                    .map(|room_info| GenericChatItem::new(room_info, store, true))
                    .filter(GenericChatItem::has_mention);

                items.extend(dms);

                let fields = &store.application.settings.tunables.sort.chats;
                let collator = &mut store.application.collator;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);

                List::new(store)
                    .empty_message("You do not have any unread mentions yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::SpaceList(state) => {
                let mut items = store
                    .application
                    .sync_info
                    .spaces
                    .clone()
                    .into_iter()
                    .map(|room| SpaceItem::new(room, store))
                    .collect::<Vec<_>>();
                let fields = &store.application.settings.tunables.sort.spaces;
                let collator = &mut store.application.collator;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.set(items);

                List::new(store)
                    .empty_message("You haven't joined any spaces yet")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::VerifyList(state) => {
                let verifications = &store.application.verifications;
                let mut items = verifications.iter().map(VerifyItem::from).collect::<Vec<_>>();

                // Sort the active verifications towards the top.
                items.sort();

                state.set(items);

                List::new(store)
                    .empty_message("No in-progress verifications")
                    .empty_alignment(Alignment::Center)
                    .focus(focused)
                    .render(area, buf, state);
            },
            IambWindow::Welcome(state) => state.draw(area, buf, focused, store),
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            IambWindow::Room(w) => w.dup(store).into(),
            IambWindow::DirectList(w) => w.dup(store).into(),
            IambWindow::MemberList(w, room_id, last_fetch) => {
                IambWindow::MemberList(w.dup(store), room_id.clone(), *last_fetch)
            },
            IambWindow::RoomList(w) => w.dup(store).into(),
            IambWindow::SpaceList(w) => w.dup(store).into(),
            IambWindow::VerifyList(w) => w.dup(store).into(),
            IambWindow::Welcome(w) => w.dup(store).into(),
            IambWindow::ChatList(w) => w.dup(store).into(),
            IambWindow::UnreadList(w) => w.dup(store).into(),
            IambWindow::MentionsList(w) => w.dup(store).into(),
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
            IambWindow::Room(room) => IambId::Room(room.id().to_owned(), room.view()),
            IambWindow::DirectList(_) => IambId::DirectList,
            IambWindow::MemberList(_, room_id, _) => IambId::MemberList(room_id.clone()),
            IambWindow::RoomList(_) => IambId::RoomList,
            IambWindow::SpaceList(_) => IambId::SpaceList,
            IambWindow::VerifyList(_) => IambId::VerifyList,
            IambWindow::Welcome(_) => IambId::Welcome,
            IambWindow::ChatList(_) => IambId::ChatList,
            IambWindow::UnreadList(_) => IambId::UnreadList,
            IambWindow::MentionsList(_) => IambId::MentionsList,
        }
    }

    fn get_tab_title(&self, store: &mut ProgramStore) -> Line<'_> {
        match self {
            IambWindow::DirectList(_) => bold_spans("Direct Messages"),
            IambWindow::RoomList(_) => bold_spans("Rooms"),
            IambWindow::SpaceList(_) => bold_spans("Spaces"),
            IambWindow::VerifyList(_) => bold_spans("Verifications"),
            IambWindow::Welcome(_) => bold_spans("Welcome to iamb"),
            IambWindow::ChatList(_) => bold_spans("DMs & Rooms"),
            IambWindow::UnreadList(_) => bold_spans("Unread Messages"),
            IambWindow::MentionsList(_) => bold_spans("Unread Mentions"),

            IambWindow::Room(w) => {
                let title = store.application.get_room_title(w.id());

                Line::from(title)
            },
            IambWindow::MemberList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    bold_span("Room Members "),
                    Span::styled(format!("({n}): "), bold_style()),
                    title.into(),
                ];
                Line::from(v)
            },
        }
    }

    fn get_win_title(&self, store: &mut ProgramStore) -> Line<'_> {
        match self {
            IambWindow::DirectList(_) => bold_spans("Direct Messages"),
            IambWindow::RoomList(_) => bold_spans("Rooms"),
            IambWindow::SpaceList(_) => bold_spans("Spaces"),
            IambWindow::VerifyList(_) => bold_spans("Verifications"),
            IambWindow::Welcome(_) => bold_spans("Welcome to iamb"),
            IambWindow::ChatList(_) => bold_spans("DMs & Rooms"),
            IambWindow::UnreadList(_) => bold_spans("Unread Messages"),
            IambWindow::MentionsList(_) => bold_spans("Unread Mentions"),

            IambWindow::Room(w) => w.get_title(store),
            IambWindow::MemberList(state, room_id, _) => {
                let title = store.application.get_room_title(room_id.as_ref());
                let n = state.len();
                let v = vec![
                    bold_span("Room Members "),
                    Span::styled(format!("({n}): "), bold_style()),
                    title.into(),
                ];
                Line::from(v)
            },
        }
    }

    fn open(id: IambId, store: &mut ProgramStore) -> IambResult<Self> {
        match id {
            IambId::Room(room_id, thread) => {
                let (room, name, tags) = store.application.worker.get_room(room_id)?;
                let room = RoomState::new(room, thread, name, tags, store);

                store.application.need_load.need_members(room.id().to_owned());
                return Ok(room.into());
            },
            IambId::DirectList => {
                let list = DirectListState::new(IambBufferId::DirectList, vec![]);

                return Ok(list.into());
            },
            IambId::MemberList(room_id) => {
                let id = IambBufferId::MemberList(room_id.clone());
                let list = MemberListState::new(id, vec![]);
                let win = IambWindow::MemberList(list, room_id, None);

                return Ok(win);
            },
            IambId::RoomList => {
                let list = RoomListState::new(IambBufferId::RoomList, vec![]);

                return Ok(list.into());
            },
            IambId::SpaceList => {
                let list = SpaceListState::new(IambBufferId::SpaceList, vec![]);

                return Ok(list.into());
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
                let list = ChatListState::new(IambBufferId::ChatList, vec![]);

                Ok(list.into())
            },
            IambId::UnreadList => {
                let list = UnreadListState::new(IambBufferId::UnreadList, vec![]);

                Ok(IambWindow::UnreadList(list))
            },
            IambId::MentionsList => {
                let list = MentionsListState::new(IambBufferId::MentionsList, vec![]);

                Ok(IambWindow::MentionsList(list))
            },
        }
    }

    fn find(name: String, store: &mut ProgramStore) -> IambResult<Self> {
        let ChatStore { names, worker, .. } = &mut store.application;

        if let Some(room) = names.get_mut(&name) {
            let id = IambId::Room(room.clone(), RoomView::Main);

            IambWindow::open(id, store)
        } else {
            let room_id = worker.join_room(name.clone())?;
            names.insert(name, room_id.clone());

            let (room, name, tags) = store.application.worker.get_room(room_id)?;
            let room = RoomState::new(room, RoomView::Main, name, tags, store);

            store.application.need_load.need_members(room.id().to_owned());
            Ok(room.into())
        }
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

#[derive(Clone)]
pub struct GenericChatItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
    unread: UnreadInfo,
    is_dm: bool,
}

impl GenericChatItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore, is_dm: bool) -> Self {
        let room = &room_info.deref().0;
        let room_id = room.room_id();

        let info = store.application.rooms.get_or_default(room_id.to_owned());
        let name = info.name.clone().unwrap_or_default();
        let alias = room.canonical_alias();
        let unread = info.unreads(room);
        info.tags.clone_from(&room_info.deref().1);

        if let Some(alias) = &alias {
            store.application.names.insert(alias.to_string(), room_id.to_owned());
        }

        GenericChatItem { room_info, name, alias, is_dm, unread }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }

    #[inline]
    fn tags(&self) -> &Option<Tags> {
        &self.room_info.deref().1
    }

    #[inline]
    fn has_mention(&self) -> bool {
        self.unread.has_mention()
    }
}

impl RoomLikeItem for GenericChatItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    fn has_tag(&self, tag: TagName) -> bool {
        if let Some(tags) = &self.room_info.deref().1 {
            tags.contains_key(&tag)
        } else {
            false
        }
    }

    fn recent_ts(&self) -> Option<&MessageTimeStamp> {
        self.unread.latest()
    }

    fn is_unread(&self) -> bool {
        self.unread.is_unread()
    }

    fn is_invite(&self) -> bool {
        self.room().state() == MatrixRoomState::Invited
    }
}

impl Display for GenericChatItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl ListItem<IambInfo> for GenericChatItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        _: &mut ProgramStore,
    ) -> Text<'_> {
        let style = selected_style(selected);
        let (name, mut labels) = name_and_labels(&self.name, &self.unread, style);
        let mut spans = vec![name];

        labels.push(if self.is_dm {
            vec![Span::styled("DM", style)]
        } else {
            vec![Span::styled("Room", style)]
        });

        if let Some(tags) = &self.tags() {
            labels.extend(tags.keys().map(|t| tag_to_span(t, style)));
        }

        append_tags(labels, &mut spans, style);
        Text::from(Line::from(spans))
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for GenericChatItem {
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
pub struct RoomItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
    unread: UnreadInfo,
}

impl RoomItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore) -> Self {
        let room = &room_info.deref().0;
        let room_id = room.room_id();

        let info = store.application.rooms.get_or_default(room_id.to_owned());
        let name = info.name.clone().unwrap_or_default();
        let alias = room.canonical_alias();
        let unread = info.unreads(room);
        info.tags.clone_from(&room_info.deref().1);

        if let Some(alias) = &alias {
            store.application.names.insert(alias.to_string(), room_id.to_owned());
        }

        RoomItem { room_info, name, alias, unread }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }

    #[inline]
    fn tags(&self) -> &Option<Tags> {
        &self.room_info.deref().1
    }
}

impl RoomLikeItem for RoomItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    fn has_tag(&self, tag: TagName) -> bool {
        if let Some(tags) = &self.room_info.deref().1 {
            tags.contains_key(&tag)
        } else {
            false
        }
    }

    fn recent_ts(&self) -> Option<&MessageTimeStamp> {
        self.unread.latest()
    }

    fn is_unread(&self) -> bool {
        self.unread.is_unread()
    }

    fn is_invite(&self) -> bool {
        self.room().state() == MatrixRoomState::Invited
    }
}

impl Display for RoomItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl ListItem<IambInfo> for RoomItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        _: &mut ProgramStore,
    ) -> Text<'_> {
        let style = selected_style(selected);
        let (name, mut labels) = name_and_labels(&self.name, &self.unread, style);
        let mut spans = vec![name];

        if let Some(tags) = &self.tags() {
            labels.extend(tags.keys().map(|t| tag_to_span(t, style)));
        }

        append_tags(labels, &mut spans, style);

        Text::from(Line::from(spans))
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for RoomItem {
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
pub struct DirectItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
    unread: UnreadInfo,
}

impl DirectItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore) -> Self {
        let room = &room_info.deref().0;
        let room_id = room_info.0.room_id().to_owned();
        let alias = room_info.0.canonical_alias();

        let info = store.application.rooms.get_or_default(room_id);
        let name = info.name.clone().unwrap_or_default();
        let unread = info.unreads(room);
        info.tags.clone_from(&room_info.deref().1);

        DirectItem { room_info, name, alias, unread }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }

    #[inline]
    fn tags(&self) -> &Option<Tags> {
        &self.room_info.deref().1
    }
}

impl RoomLikeItem for DirectItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn has_tag(&self, tag: TagName) -> bool {
        if let Some(tags) = &self.room_info.deref().1 {
            tags.contains_key(&tag)
        } else {
            false
        }
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    fn recent_ts(&self) -> Option<&MessageTimeStamp> {
        self.unread.latest()
    }

    fn is_unread(&self) -> bool {
        self.unread.is_unread()
    }

    fn is_invite(&self) -> bool {
        self.room().state() == MatrixRoomState::Invited
    }
}

impl Display for DirectItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ":verify request {}", self.name)
    }
}

impl ListItem<IambInfo> for DirectItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        _: &mut ProgramStore,
    ) -> Text<'_> {
        let style = selected_style(selected);
        let (name, mut labels) = name_and_labels(&self.name, &self.unread, style);
        let mut spans = vec![name];

        if let Some(tags) = &self.tags() {
            labels.extend(tags.keys().map(|t| tag_to_span(t, style)));
        }

        append_tags(labels, &mut spans, style);

        Text::from(Line::from(spans))
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for DirectItem {
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
pub struct SpaceItem {
    room_info: MatrixRoomInfo,
    name: String,
    alias: Option<OwnedRoomAliasId>,
}

impl SpaceItem {
    fn new(room_info: MatrixRoomInfo, store: &mut ProgramStore) -> Self {
        let room_id = room_info.0.room_id();
        let name = store
            .application
            .get_room_info(room_id.to_owned())
            .name
            .clone()
            .unwrap_or_default();
        let alias = room_info.0.canonical_alias();

        if let Some(alias) = &alias {
            store.application.names.insert(alias.to_string(), room_id.to_owned());
        }

        SpaceItem { room_info, name, alias }
    }

    #[inline]
    fn room(&self) -> &MatrixRoom {
        &self.room_info.deref().0
    }
}

impl RoomLikeItem for SpaceItem {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn room_id(&self) -> &RoomId {
        self.room().room_id()
    }

    fn alias(&self) -> Option<&RoomAliasId> {
        self.alias.as_deref()
    }

    fn has_tag(&self, _: TagName) -> bool {
        // I think that spaces can technically have tags, but afaik no client
        // exposes them, so we'll just always return false here for now.
        false
    }

    fn recent_ts(&self) -> Option<&MessageTimeStamp> {
        // XXX: this needs to determine the room with most recent message and return its timestamp.
        None
    }

    fn is_unread(&self) -> bool {
        // XXX: this needs to check whether the space contains rooms with unread messages
        false
    }

    fn is_invite(&self) -> bool {
        self.room().state() == MatrixRoomState::Invited
    }
}

impl Display for SpaceItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl ListItem<IambInfo> for SpaceItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        _: &mut ProgramStore,
    ) -> Text<'_> {
        selected_text(self.name.as_str(), selected)
    }

    fn get_word(&self) -> Option<String> {
        self.room_id().to_string().into()
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for SpaceItem {
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
pub struct VerifyItem {
    user_dev: String,
    sasv1: SasVerification,
}

impl VerifyItem {
    fn new(user_dev: String, sasv1: SasVerification) -> Self {
        VerifyItem { user_dev, sasv1 }
    }

    fn show_item(&self) -> String {
        let state = if self.sasv1.is_done() {
            "done"
        } else if self.sasv1.is_cancelled() {
            "cancelled"
        } else if self.sasv1.emoji().is_some() {
            "accepted"
        } else {
            "not accepted"
        };

        if self.sasv1.is_self_verification() {
            let device = self.sasv1.other_device();

            if let Some(display_name) = device.display_name() {
                format!("Device verification with {display_name} ({state})")
            } else {
                format!("Device verification with device {} ({})", device.device_id(), state)
            }
        } else {
            format!("User Verification with {} ({})", self.sasv1.other_user_id(), state)
        }
    }
}

impl PartialEq for VerifyItem {
    fn eq(&self, other: &Self) -> bool {
        self.user_dev == other.user_dev
    }
}

impl Eq for VerifyItem {}

impl Ord for VerifyItem {
    fn cmp(&self, other: &Self) -> Ordering {
        fn state_val(sas: &SasVerification) -> usize {
            if sas.is_done() {
                return 3;
            } else if sas.is_cancelled() {
                return 2;
            } else {
                return 1;
            }
        }

        fn device_val(sas: &SasVerification) -> usize {
            if sas.is_self_verification() {
                return 1;
            } else {
                return 2;
            }
        }

        let state1 = state_val(&self.sasv1);
        let state2 = state_val(&other.sasv1);

        let dev1 = device_val(&self.sasv1);
        let dev2 = device_val(&other.sasv1);

        let scmp = state1.cmp(&state2);
        let dcmp = dev1.cmp(&dev2);

        scmp.then(dcmp).then_with(|| {
            let did1 = self.sasv1.other_device().device_id();
            let did2 = other.sasv1.other_device().device_id();

            did1.cmp(did2)
        })
    }
}

impl PartialOrd for VerifyItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<(&String, &SasVerification)> for VerifyItem {
    fn from((user_dev, sasv1): (&String, &SasVerification)) -> Self {
        VerifyItem::new(user_dev.clone(), sasv1.clone())
    }
}

impl Display for VerifyItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.sasv1.is_done() {
            return Ok(());
        }

        if self.sasv1.is_cancelled() {
            write!(f, ":verify request {}", self.sasv1.other_user_id())
        } else if self.sasv1.emoji().is_some() {
            write!(f, ":verify confirm {}", self.user_dev)
        } else {
            write!(f, ":verify accept {}", self.user_dev)
        }
    }
}

impl ListItem<IambInfo> for VerifyItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        _: &mut ProgramStore,
    ) -> Text<'_> {
        let mut lines = vec![];

        let bold = Style::default().add_modifier(StyleModifier::BOLD);
        let item = Span::styled(self.show_item(), selected_style(selected));
        lines.push(Line::from(item));

        if self.sasv1.is_done() {
            // Print nothing.
        } else if self.sasv1.is_cancelled() {
            if let Some(info) = self.sasv1.cancel_info() {
                lines.push(Line::from(format!("    Cancelled: {}", info.reason())));
                lines.push(Line::from(""));
            }

            lines.push(Line::from("    You can start a new verification request with:"));
        } else if let Some(emoji) = self.sasv1.emoji() {
            lines.push(Line::from(
                "    Both devices should see the following Emoji sequence:".to_string(),
            ));
            lines.push(Line::from(""));

            for line in format_emojis(emoji).lines() {
                lines.push(Line::from(format!("    {line}")));
            }

            lines.push(Line::from(""));
            lines.push(Line::from("    If they don't match, run:"));
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                format!(":verify mismatch {}", self.user_dev),
                bold,
            )));
            lines.push(Line::from(""));
            lines.push(Line::from("    If everything looks right, you can confirm with:"));
        } else {
            lines.push(Line::from("    To accept this request, run:"));
        }

        let cmd = self.to_string();

        if !cmd.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::from(vec![Span::from("        "), Span::styled(cmd, bold)]));
            lines.push(Line::from(""));
            lines.push(Line::from(vec![
                Span::from("You can copy the above command with "),
                Span::styled("yy", bold),
                Span::from(" and then execute it with "),
                Span::styled("@\"", bold),
            ]));
        }

        Text::from(lines)
    }

    fn get_word(&self) -> Option<String> {
        None
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for VerifyItem {
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
pub struct MemberItem {
    member: RoomMember,
    room_id: OwnedRoomId,
}

impl MemberItem {
    fn new(member: RoomMember, room_id: OwnedRoomId) -> Self {
        Self { member, room_id }
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

        let (color, name) = store
            .application
            .settings
            .tunables
            .get_user_overrides(self.member.user_id());
        let color = color.unwrap_or_else(|| super::config::user_color(user_id.as_str()));

        let style = if selected {
            // Ensure the whole item has the same color when it's selected:
            Style::default().fg(color).add_modifier(StyleModifier::REVERSED)
        } else {
            Style::default()
        };
        let user_style = style.patch(super::config::user_style_from_color(color));
        let role_style = style.add_modifier(StyleModifier::BOLD);

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
