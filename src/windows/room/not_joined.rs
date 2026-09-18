use crate::prelude::*;

#[derive(Clone)]
pub struct NotJoinedState {
    pub(super) room: String,
    err: Option<String>,
}

impl NotJoinedState {
    /// Create a window for a room the user hasn't yet joined.
    pub fn new(room: String) -> Self {
        Self { room, err: None }
    }

    /// Create a window for a room whose `:join` command failed.
    pub fn failed(room: String, err: &UIError<IambInfo>) -> Self {
        Self { room, err: Some(err.to_string()) }
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for NotJoinedState {
    fn editor_command(
        &mut self,
        _: &EditorAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Ok(None)
    }
}

impl Jumpable<ProgramContext, IambInfo> for NotJoinedState {
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

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for NotJoinedState {
    fn scroll(
        &mut self,
        _: &ScrollStyle,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Ok(None)
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for NotJoinedState {
    fn prompt(
        &mut self,
        _: &PromptAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        Ok(vec![])
    }
}

impl TerminalCursor for NotJoinedState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        None
    }

    fn hide_term_cursor(&self) -> bool {
        true
    }
}

/// [StatefulWidget] for Matrix rooms that haven't been joined.
pub struct NotJoined;

impl StatefulWidget for NotJoined {
    type State = NotJoinedState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        let mut lines = vec![];

        lines.push(Line::from(vec![
            Span::raw("Use "),
            Span::styled(format!("`:join {}`", state.room), StyleModifier::BOLD),
            Span::raw(" to join this room"),
        ]));

        if let Some(err) = &state.err {
            let style = Style::default().fg(Color::Red);
            lines.push(Line::from(Span::styled(err.as_str(), style)));
        }

        Paragraph::new(Text::from(lines))
            .alignment(Alignment::Center)
            .render(area, buffer);
    }
}
