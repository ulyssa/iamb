use std::time::{Duration, Instant};

use modalkit::{
    actions::{Editable, EditorAction, Jumpable, PromptAction, Promptable, Scrollable},
    editing::completion::CompletionList,
    errors::EditResult,
    prelude::*,
};
use modalkit_ratatui::{
    list::{List, ListState},
    TermOffset,
    TerminalCursor,
    WindowOps,
};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Style},
    text::{Line, Span, Text},
    widgets::StatefulWidget,
};

use crate::base::{
    IambBufferId,
    IambInfo,
    IambResult,
    ProgramAction,
    ProgramContext,
    ProgramStore,
};

use crate::windows::RoomItem;

use super::room_fields_cmp;

const SPACE_HIERARCHY_DEBOUNCE: Duration = Duration::from_secs(15);

/// [StatefulWidget] for Matrix space tree.
pub struct SpaceTree<'a> {
    focused: bool,
    store: &'a mut ProgramStore,
}

impl<'a> SpaceTree<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Self {
        SpaceTree { focused: false, store }
    }

    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl StatefulWidget for SpaceTree<'_> {
    type State = SpaceTreeState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        let mut empty_message = None;
        let need_fetch = match state.last_fetch {
            Some(i) => i.elapsed() >= SPACE_HIERARCHY_DEBOUNCE,
            None => true,
        };

        if need_fetch {
            let mut children = vec![];
            let res = self.store.application.sync_info.spaces.iter().try_for_each(|room| {
                let id = room.0.room_id();
                let res = self.store.application.worker.space_members(id.to_owned());

                res.map(|members| children.extend(members.into_iter().filter(|child| child != id)))
            });

            if let Err(e) = res {
                let lines = vec![
                    Line::from("Unable to fetch space room hierarchy:"),
                    Span::styled(e.to_string(), Style::default().fg(Color::Red)).into(),
                ];

                empty_message = Text::from(lines).into();
            } else {
                let mut items = self
                    .store
                    .application
                    .sync_info
                    .spaces
                    .clone()
                    .into_iter()
                    .filter(|space| !children.contains(&space.0.room_id().to_owned()))
                    .map(|room| RoomItem::new(room, self.store))
                    .collect::<Vec<_>>();
                let fields = &self.store.application.settings.tunables.sort.spaces;
                let collator = &mut self.store.application.collator;
                items.sort_by(|a, b| room_fields_cmp(a, b, fields, collator));

                state.list.set(items);
                state.last_fetch = Some(Instant::now());
            }
        }

        let mut list = List::new(self.store).focus(self.focused);

        if let Some(text) = empty_message {
            list = list.empty_message(text);
        } else {
            list = list.empty_message(Text::from("You haven't joined any spaces yet"));
        }

        list.render(area, buffer, &mut state.list)
    }
}

/// State for the list of toplevel spaces
pub struct SpaceTreeState {
    list: ListState<RoomItem, IambInfo>,
    last_fetch: Option<Instant>,
}

impl SpaceTreeState {
    pub fn new() -> Self {
        let content = IambBufferId::SpaceTree;
        let list = ListState::new(content, vec![]);

        SpaceTreeState { list, last_fetch: None }
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for SpaceTreeState {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        self.list.editor_command(act, ctx, store)
    }
}

impl Jumpable<ProgramContext, IambInfo> for SpaceTreeState {
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &ProgramContext,
    ) -> IambResult<usize> {
        self.list.jump(list, dir, count, ctx)
    }
}

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for SpaceTreeState {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        self.list.scroll(style, ctx, store)
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for SpaceTreeState {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        self.list.prompt(act, ctx, store)
    }
}

impl TerminalCursor for SpaceTreeState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.list.get_term_cursor()
    }
}

impl WindowOps<IambInfo> for SpaceTreeState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        SpaceTree::new(store).focus(focused).render(area, buf, self);
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        SpaceTreeState {
            list: self.list.dup(store),
            last_fetch: self.last_fetch,
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        self.list.close(flags, store)
    }

    fn get_completions(&self) -> Option<CompletionList> {
        self.list.get_completions()
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        self.list.get_cursor_word(style)
    }

    fn get_selected_word(&self) -> Option<String> {
        self.list.get_selected_word()
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        self.list.write(path, flags, store)
    }
}
