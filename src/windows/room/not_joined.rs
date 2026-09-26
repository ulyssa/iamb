use matrix_sdk::ruma::RoomOrAliasId;
use matrix_sdk::ruma::room::{JoinRuleSummary, RestrictedSummary};

use crate::base::{HomeserverAction, SyncInfo};
use crate::prelude::*;
use crate::windows::room::RoomState;
use crate::worker::ClientResponse;

const ROOM_PREVIEW_DEBOUNCE: Duration = Duration::from_secs(15);

fn can_join_restricted(summary: &RestrictedSummary, sync_info: &SyncInfo) -> bool {
    sync_info
        .spaces
        .iter()
        .map(|r| r.room_id())
        .chain(sync_info.chats())
        .any(|room_id| summary.allowed_room_ids.iter().any(|allowed| allowed == room_id))
}

fn preview_join_rule(
    preview: &RoomPreview,
    store: &ProgramStore,
) -> (&'static str, Option<&'static str>) {
    match &preview.join_rule {
        Some(JoinRuleSummary::Public) => ("This room is public.", Some(":join")),
        Some(JoinRuleSummary::Knock) => ("This room is knock-only.", Some(":knock")),
        Some(JoinRuleSummary::Invite) => ("This room is invite-only.", None),
        Some(JoinRuleSummary::Restricted(summary)) => {
            if can_join_restricted(summary, &store.application.sync_info) {
                ("This room is restricted to members of a room you are in.", Some(":join"))
            } else {
                ("This room is restricted to members of a room you are not in.", None)
            }
        },
        Some(JoinRuleSummary::KnockRestricted(summary)) => {
            if can_join_restricted(summary, &store.application.sync_info) {
                ("This room is knock-restricted to members of a room you are in.", Some(":knock"))
            } else {
                ("This room is knock-restricted to members of a room you are not in.", None)
            }
        },
        _ => ("Room join rules unknown. Try `:join` or `:knock` to join this room.", None),
    }
}

fn preview_state(
    preview: &RoomPreview,
    store: &ProgramStore,
) -> (&'static str, Option<&'static str>) {
    match preview.state {
        Some(MatrixRoomState::Invited) => {
            ("You have been invited to join this room.", Some(":invite accept"))
        },
        Some(MatrixRoomState::Knocked) => ("You have knocked on this room.", None),
        Some(MatrixRoomState::Banned) => ("You have been banned from this room.", None),
        Some(MatrixRoomState::Joined) => ("You have already joined this room.", None),
        Some(MatrixRoomState::Left) | None => preview_join_rule(preview, store),
    }
}

pub struct NotJoinedState {
    alias: OwnedRoomOrAliasId,
    /// The resolved [`alias`](`Self::alias`).
    room_id: Option<OwnedRoomId>,
    err: Option<String>,
    joining: Option<ClientResponse<IambResult<OwnedRoomId>>>,

    term_cursor: (u16, u16),
}

impl NotJoinedState {
    /// Create a window for a room the user hasn't joined.
    pub fn new(alias: OwnedRoomOrAliasId, store: &mut ProgramStore) -> Self {
        store.application.need_load.need_preview(alias.clone());

        Self {
            alias,
            room_id: None,
            err: None,
            joining: None,
            term_cursor: (0, 0),
        }
    }

    pub fn alias(&self) -> &RoomOrAliasId {
        &self.alias
    }

    pub fn room_id(&mut self, store: &ProgramStore) -> Option<&RoomId> {
        let alias_or_id: &RoomOrAliasId = &self.alias;
        match <&RoomId>::try_from(alias_or_id) {
            Ok(id) => Some(id),
            Err(alias) => {
                let room_id = store.application.aliases.get(alias);
                self.room_id = room_id.cloned();
                self.room_id.as_deref()
            },
        }
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) -> Option<RoomState> {
        let room = if let Some(room) = self
            .room_id(store)
            .and_then(|id| store.application.worker.client.get_room(id))
        {
            room
        } else {
            if let Some(Err(e)) = self.joining.as_ref().and_then(|chan| chan.try_recv()) {
                self.err = Some(e.to_string());
                self.joining = None;
            }
            return None;
        };

        if room.state() == MatrixRoomState::Left {
            // room previews have more information than left rooms
            return None;
        }

        store.application.need_load.need_members(room.room_id().to_owned());

        Some(RoomState::new(room, RoomView::Main, store))
    }

    pub fn dup(&self) -> Self {
        Self {
            alias: self.alias.clone(),
            room_id: self.room_id.clone(),
            err: self.err.clone(),
            joining: None,
            term_cursor: (0, 0),
        }
    }

    pub fn get_tab_title(&self, store: &ProgramStore) -> Line<'_> {
        if let Some((Ok(preview), _)) = store.application.room_previews.get(self.alias()) {
            if let Some(name) = &preview.name {
                return Line::from(name.to_string());
            }

            if let Some(alias) = &preview.canonical_alias {
                return Line::from(alias.to_string());
            }
        }

        Line::from(self.alias.as_str())
    }

    pub fn get_title(&self, store: &ProgramStore) -> Line<'_> {
        if let Some((Ok(preview), _)) = store.application.room_previews.get(self.alias()) {
            if let Some(name) = &preview.name {
                return Line::from(vec![
                    Span::styled(name.to_string(), StyleModifier::BOLD),
                    Span::raw(" (unjoined)"),
                ]);
            }

            if let Some(alias) = &preview.canonical_alias {
                return Line::from(vec![
                    Span::styled(alias.to_string(), StyleModifier::BOLD),
                    Span::raw(" (unjoined)"),
                ]);
            }
        }

        Line::from(vec![
            Span::styled(self.alias.as_str(), StyleModifier::BOLD),
            Span::raw(" (unjoined)"),
        ])
    }

    pub async fn join_command(
        &mut self,
        act: JoinAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        match act {
            JoinAction::Join => {
                let via = store
                    .application
                    .room_via
                    .get(self.alias())
                    .unwrap_or(&store.application.settings.tunables.default_via)
                    .to_vec();

                let chan = store.application.worker.join_room_chan(self.alias().to_owned(), via);
                self.joining = Some(chan);

                Ok(vec![])
            },
            JoinAction::Knock => {
                let act = HomeserverAction::KnockSend(self.alias().to_owned(), None);

                Ok(vec![(IambAction::from(act).into(), ctx)])
            },
        }
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
        self.term_cursor.into()
    }

    fn hide_term_cursor(&self) -> bool {
        true
    }
}

/// [StatefulWidget] for Matrix rooms that haven't been joined.
pub struct NotJoined<'a> {
    store: &'a mut ProgramStore,
}

impl<'a> NotJoined<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Self {
        Self { store }
    }
}

impl StatefulWidget for NotJoined<'_> {
    type State = NotJoinedState;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut Self::State) {
        // Ensure the whole window receives the default timeline styling:
        let theme = &self.store.application.settings.theme;
        let default_style = theme.timeline.default;
        let err_style = default_style.fg(Color::Red);
        buffer.set_style(area, default_style);

        // Save the top-left corner for indicating cursor placement:
        state.term_cursor = (area.left(), area.top());

        let mut lines = vec![];

        let preview = self.store.application.room_previews.get(state.alias());
        if let Some((_, fetched)) = &preview &&
            fetched.elapsed() > ROOM_PREVIEW_DEBOUNCE
        {
            self.store.application.need_load.need_preview(state.alias().to_owned());
        }

        match preview {
            Some((Ok(preview), _)) => {
                let mut name_line = vec![];
                if let Some(name) = &preview.name {
                    name_line.push(Span::styled(name, StyleModifier::BOLD));
                    name_line.push(Span::raw(" "));
                }
                if let Some(alias) = &preview.canonical_alias {
                    name_line.push(Span::raw(alias.as_str()));
                    name_line.push(Span::raw(" "));
                }

                name_line.push(Span::raw("("));
                name_line.push(Span::raw(state.alias().as_str()));
                name_line.push(Span::raw(")"));
                lines.push(Line::from(name_line));
                if let Some(topic) = &preview.topic {
                    lines.push(Line::raw(topic));
                }

                lines.push(Line::raw(""));

                let (status, join_cmd) = preview_state(preview, self.store);

                lines.push(Line::raw(status));
                if let Some(join_cmd) = join_cmd {
                    lines.push(Line::from(vec![
                        Span::raw("Use `"),
                        Span::styled(join_cmd, StyleModifier::BOLD),
                        Span::raw("` to join this room"),
                    ]));
                }
            },
            Some((Err(matrix_sdk::Error::InsufficientData), _)) => {
                lines.push(Line::from(Span::styled(
                    "This room doesn't exist or room previews are disabled",
                    err_style,
                )));
            },
            Some((Err(err), _)) => {
                lines.push(Line::from(Span::styled(err.to_string(), err_style)));
            },
            None => {
                self.store.application.need_load.need_preview(state.alias().to_owned());
                lines.push(Line::raw("Loading room preview..."));
            },
        }

        if state.joining.is_some() {
            lines.push(Line::raw(""));
            lines.push(Line::raw("Joining this room..."));
        }

        if let Some(err) = &state.err {
            lines.push(Line::from(Span::styled(err.as_str(), err_style)));
        }

        Paragraph::new(Text::from(lines))
            .alignment(Alignment::Center)
            .render(area, buffer);
    }
}
