use crate::prelude::*;
use crate::worker::ClientResponse;

pub struct JoiningState {
    pub(super) room: String,
    room_id: Option<IambResult<OwnedRoomId>>,
    chan: ClientResponse<IambResult<OwnedRoomId>>,
}

impl JoiningState {
    pub fn new(room: String, store: &mut ProgramStore) -> Self {
        let via = store.application.settings.tunables.default_via.clone();
        let chan = store.application.worker.join_room_chan(room.clone(), via);
        Self { room, room_id: None, chan }
    }

    pub fn try_recv(&mut self) -> Option<&IambResult<OwnedRoomId>> {
        if self.room_id.is_none() &&
            let Some(id) = self.chan.try_recv()
        {
            self.room_id = Some(id);
        }

        self.room_id.as_ref()
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for JoiningState {
    fn editor_command(
        &mut self,
        _: &EditorAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Ok(None)
    }
}

impl Jumpable<ProgramContext, IambInfo> for JoiningState {
    fn jump(
        &mut self,
        _: PositionList,
        _: MoveDir1D,
        count: usize,
        _: &ProgramContext,
    ) -> IambResult<usize> {
        Ok(count)
    }
}

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for JoiningState {
    fn scroll(
        &mut self,
        _: &ScrollStyle,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Ok(None)
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for JoiningState {
    fn prompt(
        &mut self,
        _: &PromptAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        Ok(vec![])
    }
}

impl TerminalCursor for JoiningState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        None
    }

    fn hide_term_cursor(&self) -> bool {
        true
    }
}

/// [StatefulWidget] for Matrix rooms being joined.
pub struct Joining;

impl StatefulWidget for Joining {
    type State = JoiningState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        let line = Line::from(vec![
            Span::raw("Joining "),
            Span::styled(state.room.as_str(), StyleModifier::BOLD),
            Span::raw("..."),
        ]);

        Paragraph::new(line).alignment(Alignment::Center).render(area, buffer);
    }
}
