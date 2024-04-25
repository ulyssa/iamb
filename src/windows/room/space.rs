//! Window for Matrix spaces
use std::ops::{Deref, DerefMut};
use std::time::{Duration, Instant};

use matrix_sdk::{
    room::Room as MatrixRoom,
    ruma::{OwnedRoomId, RoomId},
};

use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Style},
    text::{Line, Span, Text},
    widgets::StatefulWidget,
};

use modalkit_ratatui::{
    list::{List, ListState},
    TermOffset,
    TerminalCursor,
    WindowOps,
};

use crate::base::{IambBufferId, IambInfo, ProgramStore, RoomFocus};

use crate::windows::{room_fields_cmp, RoomItem};

const SPACE_HIERARCHY_DEBOUNCE: Duration = Duration::from_secs(5);

/// State needed for rendering [Space].
pub struct SpaceState {
    room_id: OwnedRoomId,
    room: MatrixRoom,
    list: ListState<RoomItem, IambInfo>,
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

impl<'a> StatefulWidget for Space<'a> {
    type State = SpaceState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        let mut empty_message = None;
        let need_fetch = match state.last_fetch {
            Some(i) => i.elapsed() >= SPACE_HIERARCHY_DEBOUNCE,
            None => true,
        };

        if need_fetch {
            let res = self.store.application.worker.space_members(state.room_id.clone());

            match res {
                Ok(members) => {
                    let mut items = members
                        .into_iter()
                        .filter_map(|id| {
                            let (room, _, tags) =
                                self.store.application.worker.get_room(id.clone()).ok()?;
                            let room_info = std::sync::Arc::new((room, tags));

                            if id != state.room_id {
                                Some(RoomItem::new(room_info, self.store))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    let fields = &self.store.application.settings.tunables.sort.rooms;
                    items.sort_by(|a, b| room_fields_cmp(a, b, fields));

                    state.list.set(items);
                    state.last_fetch = Some(Instant::now());
                },
                Err(e) => {
                    let lines = vec![
                        Line::from("Unable to fetch space room hierarchy:"),
                        Span::styled(e.to_string(), Style::default().fg(Color::Red)).into(),
                    ];

                    empty_message = Text::from(lines).into();
                },
            }
        }

        let mut list = List::new(self.store).focus(self.focused);

        if let Some(text) = empty_message {
            list = list.empty_message(text);
        }

        list.render(area, buffer, &mut state.list)
    }
}
