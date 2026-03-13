use matrix_sdk::room::Invite;
use matrix_sdk::Room as MatrixRoom;
use modalkit::actions::{Editable, EditorAction, Jumpable, PromptAction, Promptable, Scrollable};
use modalkit::editing::completion::CompletionList;
use modalkit::errors::{EditError, EditResult, UIResult};
use modalkit::prelude::{
    CloseFlags,
    EditInfo,
    MoveDir1D,
    PositionList,
    ScrollStyle,
    WordStyle,
    WriteFlags,
};
use modalkit_ratatui::{TermOffset, TerminalCursor, WindowOps};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Rect};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Paragraph, Widget};

use crate::base::{IambError, IambInfo, IambResult, ProgramAction, ProgramContext, ProgramStore};

pub struct InviteState {
    room: MatrixRoom,
    invite: Invite,
}

impl InviteState {
    pub fn room(&self) -> &MatrixRoom {
        &self.room
    }

    pub fn new(room: MatrixRoom, store: &mut ProgramStore) -> IambResult<Self> {
        let invite = store
            .application
            .worker
            .get_inviter(room.clone())
            .map_err(IambError::from)?;

        Ok(Self { room, invite })
    }
}

impl WindowOps<IambInfo> for InviteState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, _focused: bool, store: &mut ProgramStore) {
        let inviter = &self.invite.inviter;

        let name = match self.room().canonical_alias() {
            Some(alias) => alias.to_string(),
            None => format!("{:?}", store.application.get_room_title(self.room().room_id())),
        };

        let mut invited = vec![Span::from(format!("You have been invited to join {name}"))];

        if let Some(inviter) = inviter {
            invited.push(Span::from(" by "));
            invited.push(
                store
                    .application
                    .settings
                    .get_user_span(inviter.user_id(), inviter.display_name()),
            );
        }

        let l1 = Line::from(invited);
        let l2 = Line::from(
            "You can run `:invite accept` or `:invite reject` to accept or reject this invitation.",
        );
        let text = Text::from(vec![l1, l2]);

        Paragraph::new(text).alignment(Alignment::Center).render(area, buf);
    }

    fn dup(&self, _store: &mut ProgramStore) -> Self {
        Self {
            room: self.room.clone(),
            invite: self.invite.clone(),
        }
    }

    fn close(&mut self, _flags: CloseFlags, _store: &mut ProgramStore) -> bool {
        true
    }

    fn get_completions(&self) -> Option<CompletionList> {
        None
    }

    fn get_cursor_word(&self, _style: &WordStyle) -> Option<String> {
        None
    }

    fn get_selected_word(&self) -> Option<String> {
        None
    }

    fn write(
        &mut self,
        _path: Option<&str>,
        _flags: WriteFlags,
        _store: &mut ProgramStore,
    ) -> UIResult<EditInfo, IambInfo> {
        Ok(None)
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for InviteState {
    fn editor_command(
        &mut self,
        _act: &EditorAction,
        _ctx: &ProgramContext,
        _store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Err(EditError::ReadOnly)
    }
}

impl Jumpable<ProgramContext, IambInfo> for InviteState {
    fn jump(
        &mut self,
        _list: PositionList,
        _dir: MoveDir1D,
        count: usize,
        _ctx: &ProgramContext,
    ) -> UIResult<usize, IambInfo> {
        Ok(count)
    }
}

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for InviteState {
    fn scroll(
        &mut self,
        _style: &ScrollStyle,
        _ctx: &ProgramContext,
        _store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Ok(None)
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for InviteState {
    fn prompt(
        &mut self,
        _act: &PromptAction,
        _ctx: &ProgramContext,
        _store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        Ok(vec![])
    }
}

impl TerminalCursor for InviteState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        None
    }
}
