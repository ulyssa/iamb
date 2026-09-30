//! Window for Matrix spaces

use matrix_sdk::ruma::OwnedSpaceChildOrder;
use matrix_sdk::ruma::events::StateEventType;
use matrix_sdk::ruma::events::space::child::SpaceChildEventContent;
use modalkit_ratatui::list::{List, ListState};

use crate::prelude::*;
use crate::windows::{GenericRoomItem, RoomLikeItem, room_fields_cmp};

/// State needed for rendering [Space].
pub struct SpaceState {
    room_id: OwnedRoomId,
    room: MatrixRoom,
    list: ListState<GenericRoomItem, IambInfo>,
    last_fetch: Option<Instant>,
}

impl SpaceState {
    pub fn new(room: MatrixRoom) -> Self {
        let room_id = room.room_id().to_owned();
        let content = IambBufferId::Room(room_id.clone(), None, RoomFocus::Scrollback);
        let list = ListState::new(content, vec![]);
        let last_fetch = None;

        SpaceState { room_id, room, list, last_fetch }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        if let Some(room) = store.application.worker.client.get_room(self.id()) {
            self.room = room;
        }
    }

    pub fn room(&self) -> &MatrixRoom {
        &self.room
    }

    pub fn id(&self) -> &RoomId {
        &self.room_id
    }

    pub fn dup(&self, store: &mut ProgramStore) -> Self {
        SpaceState {
            room_id: self.room_id.clone(),
            room: self.room.clone(),
            list: self.list.dup(store),
            last_fetch: self.last_fetch,
        }
    }

    pub async fn space_command(
        &mut self,
        act: SpaceAction,
        _: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        match act {
            SpaceAction::SetChild { child, order, suggested } => {
                if !self
                    .room
                    .power_levels()
                    .await
                    .map_err(matrix_sdk::Error::from)
                    .map_err(IambError::from)?
                    .user_can_send_state(
                        &store.application.settings.profile.user_id,
                        StateEventType::SpaceChild,
                    )
                {
                    return Err(IambError::InsufficientPermission.into());
                }

                let (child_id, via) = match OwnedRoomId::try_from(child) {
                    Ok(room_id) => {
                        // assume the new child is reachable the same way as the parent
                        let via = self.room.route().await.map_err(IambError::from)?;

                        (room_id, via)
                    },
                    Err(alias) => {
                        let resp = store
                            .application
                            .worker
                            .client
                            .resolve_room_alias(&alias)
                            .await
                            .map_err(IambError::from)?;

                        (resp.room_id, resp.servers)
                    },
                };

                let mut ev = SpaceChildEventContent::new(via);
                ev.order = order
                    .as_deref()
                    .map(OwnedSpaceChildOrder::from_str)
                    .transpose()
                    .map_err(IambError::InvalidSpaceChildOrder)?;
                ev.suggested = suggested;
                let _ = self
                    .room
                    .send_state_event_for_key(&child_id, ev)
                    .await
                    .map_err(IambError::from)?;

                Ok(InfoMessage::from("Space updated").into())
            },
            SpaceAction::RemoveChild => {
                let space = self.list.get().ok_or(IambError::NoSelectedRoomOrSpaceItem)?;
                if !self
                    .room
                    .power_levels()
                    .await
                    .map_err(matrix_sdk::Error::from)
                    .map_err(IambError::from)?
                    .user_can_send_state(
                        &store.application.settings.profile.user_id,
                        StateEventType::SpaceChild,
                    )
                {
                    return Err(IambError::InsufficientPermission.into());
                }

                let ev = SpaceChildEventContent::new(vec![]);
                let event_id = self
                    .room
                    .send_state_event_for_key(&space.room_id().to_owned(), ev)
                    .await
                    .map_err(IambError::from)?;

                // Fix for element (see https://github.com/element-hq/element-web/issues/29606)
                let _ = self
                    .room
                    .redact(&event_id.event_id, Some("workaround for element bug"), None)
                    .await
                    .map_err(IambError::from)?;

                Ok(InfoMessage::from("Room removed").into())
            },
        }
    }
}

impl TerminalCursor for SpaceState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.list.get_term_cursor()
    }

    fn hide_term_cursor(&self) -> bool {
        self.list.hide_term_cursor()
    }
}

impl Deref for SpaceState {
    type Target = ListState<GenericRoomItem, IambInfo>;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

impl DerefMut for SpaceState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.list
    }
}

/// [StatefulWidget] for Matrix spaces.
pub struct Space<'a> {
    focused: bool,
    store: &'a mut ProgramStore,
}

impl<'a> Space<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Self {
        Space { focused: false, store }
    }

    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl StatefulWidget for Space<'_> {
    type State = SpaceState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        let ChatStore {
            rooms,
            spaces,
            worker,
            settings,
            collator,
            need_load,
            room_previews,
            ..
        } = &mut self.store.application;
        let default_rooms_style = settings.theme.rooms.default;

        let mut items = spaces
            .entry(state.room_id.clone())
            .or_default()
            .children
            .keys()
            .map(|id| {
                if let Some(room) = worker.client.get_room(id) {
                    GenericRoomItem::new(&room, rooms.get_or_default(id.to_owned()))
                } else {
                    GenericRoomItem::new_unknown(id.to_owned(), room_previews, need_load)
                }
            })
            .collect::<Vec<_>>();

        let fields = &settings.tunables.sort.rooms;
        items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

        state.list.set(items);
        state.set_ignorecase(settings.tunables.ignorecase);

        List::new(self.store)
            .empty_message("This space is empty")
            .empty_alignment(Alignment::Center)
            .focus(self.focused)
            .style(default_rooms_style)
            .render(area, buffer, &mut state.list)
    }
}
