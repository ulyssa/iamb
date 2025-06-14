//! # Windows for Matrix rooms and spaces
use std::collections::HashSet;

use matrix_sdk::{
    notification_settings::RoomNotificationMode,
    room::Room as MatrixRoom,
    ruma::{
        api::client::{
            alias::{
                create_alias::v3::Request as CreateAliasRequest,
                delete_alias::v3::Request as DeleteAliasRequest,
            },
            error::ErrorKind as ClientApiErrorKind,
        },
        events::{
            room::{
                canonical_alias::RoomCanonicalAliasEventContent,
                history_visibility::{HistoryVisibility, RoomHistoryVisibilityEventContent},
                name::RoomNameEventContent,
                topic::RoomTopicEventContent,
            },
            tag::{TagInfo, Tags},
        },
        OwnedEventId,
        OwnedRoomAliasId,
        OwnedUserId,
        RoomId,
    },
    RoomDisplayName,
    RoomState as MatrixRoomState,
};

use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Modifier as StyleModifier, Style},
    text::{Line, Span, Text},
    widgets::{Paragraph, StatefulWidget, Widget},
};

use modalkit::actions::{
    Action,
    Editable,
    EditorAction,
    Jumpable,
    PromptAction,
    Promptable,
    Scrollable,
    WindowAction,
};
use modalkit::errors::{EditResult, UIError};
use modalkit::prelude::*;
use modalkit::{editing::completion::CompletionList, keybindings::dialog::PromptYesNo};
use modalkit_ratatui::{TermOffset, TerminalCursor, WindowOps};

use crate::base::{
    IambAction,
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
    SendAction,
    SpaceAction,
};

use self::chat::ChatState;
use self::space::{Space, SpaceState};

use std::convert::TryFrom;

mod chat;
mod scrollback;
mod space;

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s {
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

/// State for a Matrix room or space.
///
/// Since spaces function as special rooms within Matrix, we wrap their window state together, so
/// that operations like sending and accepting invites, opening the members window, etc., all work
/// similarly.
pub enum RoomState {
    Chat(ChatState),
    Space(SpaceState),
}

impl From<ChatState> for RoomState {
    fn from(chat: ChatState) -> Self {
        RoomState::Chat(chat)
    }
}

impl From<SpaceState> for RoomState {
    fn from(space: SpaceState) -> Self {
        RoomState::Space(space)
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

    pub fn thread(&self) -> Option<&OwnedEventId> {
        match self {
            RoomState::Chat(chat) => chat.thread(),
            RoomState::Space(_) => None,
        }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        match self {
            RoomState::Chat(chat) => chat.refresh_room(store),
            RoomState::Space(space) => space.refresh_room(store),
        }
    }

    fn draw_invite(
        &self,
        invited: MatrixRoom,
        area: Rect,
        buf: &mut Buffer,
        store: &mut ProgramStore,
    ) {
        let inviter = store.application.worker.get_inviter(invited.clone());

        let name = match invited.canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(self.id())),
        };

        let mut invited = vec![Span::from(format!("You have been invited to join {name}"))];

        if let Ok(Some(inviter)) = &inviter {
            let info = store.application.rooms.get_or_default(self.id().to_owned());
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

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::Chat(chat) => chat.message_command(act, ctx, store).await,
            RoomState::Space(_) => Err(IambError::NoSelectedMessage.into()),
        }
    }

    pub async fn space_command(
        &mut self,
        act: SpaceAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::Space(space) => space.space_command(act, ctx, store).await,
            RoomState::Chat(_) => Err(IambError::NoSelectedSpace.into()),
        }
    }

    pub async fn send_command(
        &mut self,
        act: SendAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match self {
            RoomState::Chat(chat) => chat.send_command(act, ctx, store).await,
            RoomState::Space(_) => Err(IambError::NoSelectedRoom.into()),
        }
    }

    pub async fn room_command(
        &mut self,
        act: RoomAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        match act {
            RoomAction::InviteAccept => {
                if let Some(room) = store.application.worker.client.get_room(self.id()) {
                    let details = room.invite_details().await.map_err(IambError::from)?;
                    let details = details.invitee.event().original_content();
                    let is_direct = details.and_then(|ev| ev.is_direct).unwrap_or_default();

                    room.join().await.map_err(IambError::from)?;

                    if is_direct {
                        room.set_is_direct(true).await.map_err(IambError::from)?;
                    }

                    Ok(vec![])
                } else {
                    Err(IambError::NotInvited.into())
                }
            },
            RoomAction::InviteReject => {
                if let Some(room) = store.application.worker.client.get_room(self.id()) {
                    room.leave().await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    Err(IambError::NotInvited.into())
                }
            },
            RoomAction::InviteSend(user) => {
                if let Some(room) = store.application.worker.client.get_room(self.id()) {
                    room.invite_user_by_id(user.as_ref()).await.map_err(IambError::from)?;

                    Ok(vec![])
                } else {
                    Err(IambError::NotJoined.into())
                }
            },
            RoomAction::Leave(skip_confirm) => {
                if let Some(room) = store.application.worker.client.get_room(self.id()) {
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
                let Some(room) = store.application.worker.client.get_room(self.id()) else {
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
            RoomAction::Members(cmd) => {
                let act = Action::Window(WindowAction::Switch(OpenTarget::Application(
                    IambId::MemberList(self.id().to_owned()),
                )));

                Ok(vec![(act, cmd.context.clone())])
            },
            RoomAction::SetDirect(is_direct) => {
                let room = store
                    .application
                    .get_joined_room(self.id())
                    .ok_or(UIError::Application(IambError::NotJoined))?;

                room.set_is_direct(is_direct).await.map_err(IambError::from)?;

                Ok(vec![])
            },
            RoomAction::Set(field, value) => {
                let room = store
                    .application
                    .get_joined_room(self.id())
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
                            .set_room_notification_mode(self.id(), mode)
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
                        let alias_create_req =
                            CreateAliasRequest::new(orai.clone(), room.room_id().into());
                        if let Err(e) = client.send(alias_create_req).await {
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
                        let alias_create_req = CreateAliasRequest::new(orai, room.room_id().into());
                        if let Err(e) = client.send(alias_create_req).await {
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
                    .get_joined_room(self.id())
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
                            .delete_user_defined_room_rules(self.id())
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
                        let del_req = DeleteAliasRequest::new(alias_to_destroy);
                        let _ = store
                            .application
                            .worker
                            .client
                            .send(del_req)
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
                        let del_req = DeleteAliasRequest::new(orai);
                        let _ = store
                            .application
                            .worker
                            .client
                            .send(del_req)
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
                    .get_joined_room(self.id())
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
                        let mode =
                            notifications.get_user_defined_room_notification_mode(self.id()).await;

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

    pub fn get_title(&self, store: &mut ProgramStore) -> Line {
        let title = store.application.get_room_title(self.id());
        let style = Style::default().add_modifier(StyleModifier::BOLD);
        let mut spans = vec![];

        if let RoomState::Chat(chat) = self {
            if chat.thread().is_some() {
                spans.push("Thread in ".into());
            }
        }

        spans.push(Span::styled(title, style));

        match self.room().topic() {
            Some(desc) if !desc.is_empty() => {
                spans.push(" (".into());
                spans.push(desc.into());
                spans.push(")".into());
            },
            _ => {},
        }

        Line::from(spans)
    }

    pub fn focus_toggle(&mut self) {
        match self {
            RoomState::Chat(chat) => chat.focus_toggle(),
            RoomState::Space(_) => return,
        }
    }

    pub fn room(&self) -> &MatrixRoom {
        match self {
            RoomState::Chat(chat) => chat.room(),
            RoomState::Space(space) => space.room(),
        }
    }

    pub fn id(&self) -> &RoomId {
        match self {
            RoomState::Chat(chat) => chat.id(),
            RoomState::Space(space) => space.id(),
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
}

impl WindowOps<IambInfo> for RoomState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        if self.room().state() == MatrixRoomState::Invited {
            self.refresh_room(store);
        }

        if self.room().state() == MatrixRoomState::Invited {
            self.draw_invite(self.room().clone(), area, buf, store);
        }

        match self {
            RoomState::Chat(chat) => chat.draw(area, buf, focused, store),
            RoomState::Space(space) => {
                Space::new(store).focus(focused).render(area, buf, space);
            },
        }
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        match self {
            RoomState::Chat(chat) => RoomState::Chat(chat.dup(store)),
            RoomState::Space(space) => RoomState::Space(space.dup(store)),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        match self {
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
            RoomState::Chat(chat) => chat.write(path, flags, store),
            RoomState::Space(space) => space.write(path, flags, store),
        }
    }

    fn get_completions(&self) -> Option<CompletionList> {
        match self {
            RoomState::Chat(chat) => chat.get_completions(),
            RoomState::Space(space) => space.get_completions(),
        }
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        match self {
            RoomState::Chat(chat) => chat.get_cursor_word(style),
            RoomState::Space(space) => space.get_cursor_word(style),
        }
    }

    fn get_selected_word(&self) -> Option<String> {
        match self {
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
