use std::ops::{Deref, DerefMut};

use matrix_sdk::{
    room::Room as MatrixRoom,
    ruma::{OwnedRoomId, RoomId},
};

use modalkit::tui::{buffer::Buffer, layout::Rect, widgets::StatefulWidget};

use modalkit::{
    widgets::list::{List, ListState},
    widgets::{TermOffset, TerminalCursor, WindowOps},
};

use crate::base::{IambBufferId, IambInfo, ProgramStore, RoomFocus};

use crate::windows::RoomItem;

pub struct SpaceState {
    room_id: OwnedRoomId,
    list: ListState<RoomItem, IambInfo>,
}

impl SpaceState {
    pub fn new(room: MatrixRoom) -> Self {
        let room_id = room.room_id().to_owned();
        let content = IambBufferId::Room(room_id.clone(), RoomFocus::Scrollback);
        let list = ListState::new(content, vec![]);

        SpaceState { room_id, list }
    }

    pub fn id(&self) -> &RoomId {
        &self.room_id
    }

    pub fn dup(&self, store: &mut ProgramStore) -> Self {
        SpaceState {
            room_id: self.room_id.clone(),
            list: self.list.dup(store),
        }
    }
}

impl TerminalCursor for SpaceState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.list.get_term_cursor()
    }
}

impl Deref for SpaceState {
    type Target = ListState<RoomItem, IambInfo>;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

impl DerefMut for SpaceState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.list
    }
}

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

impl<'a> StatefulWidget for Space<'a> {
    type State = SpaceState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        let members = self.store.application.worker.space_members(state.room_id.clone()).unwrap();
        let items = members
            .into_iter()
            .filter_map(|id| {
                let (room, name) = self.store.application.worker.get_room(id.clone()).ok()?;

                if id != state.room_id {
                    Some(RoomItem::new(room, name, self.store))
                } else {
                    None
                }
            })
            .collect();

        state.list.set(items);

        List::new(self.store)
            .focus(self.focused)
            .render(area, buffer, &mut state.list)
    }
}
