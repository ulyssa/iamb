//! Window for Matrix spaces

use feruca::Collator;
use matrix_sdk::ruma::OwnedSpaceChildOrder;
use matrix_sdk::ruma::events::StateEventType;
use matrix_sdk::ruma::events::space::child::SpaceChildEventContent;
use modalkit_ratatui::list::{List, ListState};

use crate::base::{SortColumn, SortFieldRoom, SortFieldSpace, SortOrder, SpaceInfo};
use crate::prelude::*;
use crate::windows::{GenericRoomItem, RoomLikeItem, room_cmp};

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

        let info = spaces.entry(state.room_id.clone()).or_default();

        let mut items = info
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

        let fields = &settings.tunables.sort.space;
        items.sort_by(|a, b| space_fields_cmp(a, b, fields, collator, info));

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

fn space_child_cmp<T: RoomLikeItem>(
    a: &T,
    b: &T,
    field: &SortFieldSpace,
    collator: &mut Collator,
    space: &SpaceInfo,
) -> Ordering {
    match field {
        SortFieldSpace::Room(field) => room_cmp(a, b, field, collator),
        SortFieldSpace::SpaceOrder => {
            let (Some(a_child), Some(b_child)) =
                (space.children.get(a.room_id()), space.children.get(b.room_id()))
            else {
                // This should never happen since we got both room ids from the space info, but fall
                // back to room id order anyway and hope for the best.
                return room_cmp(a, b, &SortFieldRoom::RoomId, collator);
            };

            match (a_child.0.order.as_ref(), b_child.0.order.as_ref()) {
                (Some(a), Some(b)) => a.cmp(b),
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => (a_child.1, a.room_id()).cmp(&(b_child.1, b.room_id())),
            }
        },
    }
}

/// Compare two space children according the configured sort criteria.
fn space_fields_cmp<T: RoomLikeItem>(
    a: &T,
    b: &T,
    fields: &[SortColumn<SortFieldSpace>],
    collator: &mut Collator,
    space: &SpaceInfo,
) -> Ordering {
    for SortColumn(field, order) in fields {
        match (space_child_cmp(a, b, field, collator, space), order) {
            (Ordering::Equal, _) => continue,
            (o, SortOrder::Ascending) => return o,
            (o, SortOrder::Descending) => return o.reverse(),
        }
    }

    // Break ties on space order.
    space_child_cmp(a, b, &SortFieldSpace::SpaceOrder, collator, space)
}

#[cfg(test)]
mod tests {
    use matrix_sdk::ruma::{assign, server_name};

    use crate::windows::tests::TestRoomItem;

    use super::*;

    #[test]
    fn test_sort_space_children() {
        let mut collator = Collator::default();
        let collator = &mut collator;
        let server = server_name!("example.com");

        let room1 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            name: "1",
            alias: None,
            tags: vec![],
            unread: Default::default(),
            invite: false,
        };
        let room2 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            name: "2",
            alias: None,
            tags: vec![],
            unread: Default::default(),
            invite: false,
        };
        let room3 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            name: "3",
            alias: None,
            tags: vec![],
            unread: Default::default(),
            invite: false,
        };
        let room4 = TestRoomItem {
            room_id: RoomId::new_v1(server).to_owned(),
            name: "4",
            alias: None,
            tags: vec![],
            unread: Default::default(),
            invite: false,
        };

        let space = SpaceInfo {
            children: [
                (
                    room1.room_id.clone(),
                    (
                        assign!(SpaceChildEventContent::new(vec![]), {
                            order: Some("b".try_into().unwrap())
                        }),
                        MilliSecondsSinceUnixEpoch(5.try_into().unwrap()),
                    ),
                ),
                (
                    room2.room_id.clone(),
                    (
                        assign!(SpaceChildEventContent::new(vec![]), {
                            order: Some("a".try_into().unwrap())
                        }),
                        MilliSecondsSinceUnixEpoch(6.try_into().unwrap()),
                    ),
                ),
                (
                    room3.room_id.clone(),
                    (
                        SpaceChildEventContent::new(vec![]),
                        MilliSecondsSinceUnixEpoch(7.try_into().unwrap()),
                    ),
                ),
                (
                    room4.room_id.clone(),
                    (
                        SpaceChildEventContent::new(vec![]),
                        MilliSecondsSinceUnixEpoch(4.try_into().unwrap()),
                    ),
                ),
            ]
            .into(),
        };

        // Sort by space order
        let mut rooms = vec![&room1, &room2, &room3, &room4];
        let fields = &[SortColumn(SortFieldSpace::SpaceOrder, SortOrder::Ascending)];
        rooms.sort_by(|a, b| space_fields_cmp(a, b, fields, collator, &space));
        assert_eq!(rooms, vec![&room2, &room1, &room4, &room3]);
    }
}
