use std::collections::HashSet;

use regex::Regex;

use matrix_sdk::ruma::OwnedRoomId;

use modalkit::tui::{buffer::Buffer, layout::Rect, widgets::StatefulWidget};
use modalkit::widgets::{ScrollActions, TerminalCursor, WindowOps};

use modalkit::editing::{
    action::{
        Action,
        CursorAction,
        EditAction,
        EditError,
        EditInfo,
        EditResult,
        Editable,
        EditorAction,
        EditorActions,
        HistoryAction,
        InsertTextAction,
        Jumpable,
        PromptAction,
        Promptable,
        Scrollable,
        Searchable,
        SelectionAction,
        UIError,
        UIResult,
    },
    base::{
        Axis,
        CloseFlags,
        CompletionDisplay,
        CompletionSelection,
        CompletionType,
        Count,
        EditRange,
        EditTarget,
        Mark,
        MoveDir1D,
        MoveDir2D,
        MoveDirMod,
        MovePosition,
        MoveTerminus,
        MoveType,
        PositionList,
        RangeType,
        Register,
        ScrollSize,
        ScrollStyle,
        SearchType,
        TargetShape,
        ViewportContext,
        WordStyle,
        WriteFlags,
    },
    completion::CompletionList,
    context::{EditContext, Resolve},
    cursor::{CursorGroup, CursorState},
    history::HistoryList,
    rope::EditRope,
    store::{RegisterCell, RegisterPutFlags},
};

use crate::{
    base::{IambBufferId, IambInfo, IambResult, ProgramContext, ProgramStore, RoomFocus, RoomInfo},
    config::ApplicationSettings,
    message::{Message, MessageCursor, MessageKey, Messages},
};

fn nth_key_before(pos: MessageKey, n: usize, info: &RoomInfo) -> MessageKey {
    let mut end = &pos;
    let iter = info.messages.range(..=&pos).rev().enumerate();

    for (i, (key, _)) in iter {
        end = key;

        if i >= n {
            break;
        }
    }

    end.clone()
}

fn nth_before(pos: MessageKey, n: usize, info: &RoomInfo) -> MessageCursor {
    nth_key_before(pos, n, info).into()
}

fn nth_key_after(pos: MessageKey, n: usize, info: &RoomInfo) -> MessageKey {
    let mut end = &pos;
    let iter = info.messages.range(&pos..).enumerate();

    for (i, (key, _)) in iter {
        end = key;

        if i >= n {
            break;
        }
    }

    end.clone()
}

fn nth_after(pos: MessageKey, n: usize, info: &RoomInfo) -> MessageCursor {
    nth_key_after(pos, n, info).into()
}

fn prevmsg<'a>(key: &MessageKey, info: &'a RoomInfo) -> Option<&'a Message> {
    info.messages.range(..key).next_back().map(|(_, v)| v)
}

pub struct ScrollbackState {
    /// The room identifier.
    room_id: OwnedRoomId,

    /// The buffer identifier used for saving marks, etc.
    id: IambBufferId,

    /// The currently selected message in the scrollback.
    cursor: MessageCursor,

    /// Contextual info about the viewport used during rendering.
    viewctx: ViewportContext<MessageCursor>,

    /// The jumplist of visited messages.
    jumped: HistoryList<MessageCursor>,

    /// Whether the full message should be drawn during the next render() call.
    ///
    /// This is used to ensure that ^E/^Y work nicely when the cursor is currently
    /// on a multiline message.
    show_full_on_redraw: bool,
}

impl ScrollbackState {
    pub fn new(room_id: OwnedRoomId) -> ScrollbackState {
        let id = IambBufferId::Room(room_id.to_owned(), RoomFocus::Scrollback);
        let cursor = MessageCursor::default();
        let viewctx = ViewportContext::default();
        let jumped = HistoryList::default();
        let show_full_on_redraw = false;

        ScrollbackState {
            room_id,
            id,
            cursor,
            viewctx,
            jumped,
            show_full_on_redraw,
        }
    }

    pub fn goto_latest(&mut self) {
        self.cursor = MessageCursor::latest();
    }

    /// Set the dimensions and placement within the terminal window for this list.
    pub fn set_term_info(&mut self, area: Rect) {
        self.viewctx.dimensions = (area.width as usize, area.height as usize);
    }

    pub fn get_key(&self, info: &mut RoomInfo) -> Option<MessageKey> {
        self.cursor
            .timestamp
            .clone()
            .or_else(|| info.messages.last_key_value().map(|kv| kv.0.clone()))
    }

    pub fn get_mut<'a>(&mut self, messages: &'a mut Messages) -> Option<&'a mut Message> {
        if let Some(k) = &self.cursor.timestamp {
            messages.get_mut(k)
        } else {
            messages.last_entry().map(|o| o.into_mut())
        }
    }

    pub fn messages<'a>(
        &self,
        range: EditRange<MessageCursor>,
        info: &'a RoomInfo,
    ) -> impl Iterator<Item = (&'a MessageKey, &'a Message)> {
        let start = range.start.to_key(info);
        let end = range.end.to_key(info);

        let (start, end) = if let (Some(start), Some(end)) = (start, end) {
            (start, end)
        } else if let Some((last, _)) = info.messages.last_key_value() {
            (last, last)
        } else {
            return info.messages.range(..);
        };

        if range.inclusive {
            info.messages.range(start..=end)
        } else {
            info.messages.range(start..end)
        }
    }

    fn scrollview(
        &mut self,
        idx: MessageKey,
        pos: MovePosition,
        info: &RoomInfo,
        settings: &ApplicationSettings,
    ) {
        let selidx = if let Some(key) = self.cursor.to_key(info) {
            key
        } else {
            return;
        };

        match pos {
            MovePosition::Beginning => {
                self.viewctx.corner = idx.into();
            },
            MovePosition::Middle => {
                let mut lines = 0;
                let target = self.viewctx.get_height() / 2;

                for (key, item) in info.messages.range(..=&idx).rev() {
                    let sel = selidx == key;
                    let prev = prevmsg(key, info);
                    let len = item.show(prev, sel, &self.viewctx, info, settings).lines.len();

                    if key == &idx {
                        lines += len / 2;
                    } else {
                        lines += len;
                    }

                    if lines >= target {
                        // We've moved back far enough.
                        self.viewctx.corner.timestamp = key.clone().into();
                        self.viewctx.corner.text_row = lines - target;
                        break;
                    }
                }
            },
            MovePosition::End => {
                let mut lines = 0;
                let target = self.viewctx.get_height();

                for (key, item) in info.messages.range(..=&idx).rev() {
                    let sel = key == selidx;
                    let prev = prevmsg(key, info);
                    let len = item.show(prev, sel, &self.viewctx, info, settings).lines.len();

                    lines += len;

                    if lines >= target {
                        // We've moved back far enough.
                        self.viewctx.corner.timestamp = key.clone().into();
                        self.viewctx.corner.text_row = lines - target;
                        break;
                    }
                }
            },
        }
    }

    fn shift_cursor(&mut self, info: &RoomInfo, settings: &ApplicationSettings) {
        let last_key = if let Some(k) = info.messages.last_key_value() {
            k.0
        } else {
            return;
        };

        let corner_key = self.viewctx.corner.timestamp.as_ref().unwrap_or(last_key);

        if self.cursor < self.viewctx.corner {
            // Cursor is above the viewport; move it inside.
            self.cursor = corner_key.clone().into();
        }

        // Check whether the cursor is below the viewport.
        let mut lines = 0;

        let cursor_key = self.cursor.timestamp.as_ref().unwrap_or(last_key);
        let mut prev = prevmsg(cursor_key, info);

        for (idx, item) in info.messages.range(corner_key.clone()..) {
            if idx == cursor_key {
                // Cursor is already within the viewport.
                break;
            }

            lines += item.show(prev, false, &self.viewctx, info, settings).height().max(1);

            if lines >= self.viewctx.get_height() {
                // We've reached the end of the viewport; move cursor into it.
                self.cursor = idx.clone().into();
                break;
            }

            prev = Some(item);
        }
    }

    fn _range_to(&self, cursor: MessageCursor) -> EditRange<MessageCursor> {
        EditRange::inclusive(self.cursor.clone(), cursor, TargetShape::LineWise)
    }

    fn movement(
        &self,
        pos: MessageKey,
        movement: &MoveType,
        count: &Count,
        ctx: &ProgramContext,
        info: &RoomInfo,
    ) -> Option<MessageCursor> {
        let count = ctx.resolve(count);

        match movement {
            // These movements don't map meaningfully onto the scrollback history.
            MoveType::BufferByteOffset => None,
            MoveType::Column(_, _) => None,
            MoveType::ItemMatch => None,
            MoveType::LineColumnOffset => None,
            MoveType::LinePercent => None,
            MoveType::LinePos(_) => None,
            MoveType::SentenceBegin(_) => None,
            MoveType::ScreenFirstWord(_) => None,
            MoveType::ScreenLinePos(_) => None,
            MoveType::WordBegin(_, _) => None,
            MoveType::WordEnd(_, _) => None,

            MoveType::BufferLineOffset => None,
            MoveType::BufferLinePercent => None,
            MoveType::BufferPos(MovePosition::Beginning) => {
                let start = info.messages.first_key_value()?.0.clone();

                Some(start.into())
            },
            MoveType::BufferPos(MovePosition::Middle) => None,
            MoveType::BufferPos(MovePosition::End) => Some(MessageCursor::latest()),
            MoveType::FinalNonBlank(dir) |
            MoveType::FirstWord(dir) |
            MoveType::Line(dir) |
            MoveType::ScreenLine(dir) |
            MoveType::ParagraphBegin(dir) |
            MoveType::SectionBegin(dir) |
            MoveType::SectionEnd(dir) => {
                match dir {
                    MoveDir1D::Previous => nth_before(pos, count, info).into(),
                    MoveDir1D::Next => nth_after(pos, count, info).into(),
                }
            },
            MoveType::ViewportPos(MovePosition::Beginning) => {
                return self.viewctx.corner.timestamp.as_ref().map(|k| k.clone().into());
            },
            MoveType::ViewportPos(MovePosition::Middle) => {
                // XXX: Need to calculate an accurate middle position.
                return None;
            },
            MoveType::ViewportPos(MovePosition::End) => {
                // XXX: Need store to calculate an accurate end position.
                return None;
            },

            _ => None,
        }
    }

    fn range_of_movement(
        &self,
        pos: MessageKey,
        movement: &MoveType,
        count: &Count,
        ctx: &ProgramContext,
        info: &RoomInfo,
    ) -> Option<EditRange<MessageCursor>> {
        let other = self.movement(pos.clone(), movement, count, ctx, info)?;

        Some(EditRange::inclusive(pos.into(), other, TargetShape::LineWise))
    }

    fn range(
        &self,
        pos: MessageKey,
        range: &RangeType,
        _: bool,
        count: &Count,
        ctx: &ProgramContext,
        info: &RoomInfo,
    ) -> Option<EditRange<MessageCursor>> {
        match range {
            RangeType::Bracketed(_, _) => None,
            RangeType::Item => None,
            RangeType::Quote(_) => None,
            RangeType::Word(_) => None,
            RangeType::XmlTag => None,

            RangeType::Buffer => {
                let start = info.messages.first_key_value()?.0.clone();
                let end = info.messages.last_key_value()?.0.clone();

                Some(EditRange::inclusive(start.into(), end.into(), TargetShape::LineWise))
            },
            RangeType::Line | RangeType::Paragraph | RangeType::Sentence => {
                let count = ctx.resolve(count);

                if count == 0 {
                    return None;
                }

                let mut end = &pos;

                for (i, (key, _)) in info.messages.range(&pos..).enumerate() {
                    if i >= count {
                        break;
                    }

                    end = key;
                }

                let end = end.clone().into();
                let start = pos.into();

                Some(EditRange::inclusive(start, end, TargetShape::LineWise))
            },

            _ => None,
        }
    }

    fn find_message_next(
        &self,
        start: MessageKey,
        needle: &Regex,
        mut count: usize,
        info: &RoomInfo,
    ) -> Option<MessageCursor> {
        let mut mc = None;

        for (key, msg) in info.messages.range(&start..) {
            if count == 0 {
                break;
            }

            if key == &start {
                continue;
            }

            if needle.is_match(msg.event.body().as_ref()) {
                mc = MessageCursor::from(key.clone()).into();
                count -= 1;
            }
        }

        return mc;
    }

    fn find_message_prev(
        &self,
        end: MessageKey,
        needle: &Regex,
        mut count: usize,
        info: &RoomInfo,
        need_load: &mut HashSet<OwnedRoomId>,
    ) -> Option<MessageCursor> {
        let mut mc = None;

        for (key, msg) in info.messages.range(..&end).rev() {
            if count == 0 {
                break;
            }

            if needle.is_match(msg.event.body().as_ref()) {
                mc = MessageCursor::from(key.clone()).into();
                count -= 1;
            }
        }

        if count > 0 {
            need_load.insert(self.room_id.clone());
        }

        return mc;
    }

    fn find_message(
        &self,
        key: MessageKey,
        dir: MoveDir1D,
        needle: &Regex,
        count: usize,
        info: &RoomInfo,
        need_load: &mut HashSet<OwnedRoomId>,
    ) -> Option<MessageCursor> {
        match dir {
            MoveDir1D::Next => self.find_message_next(key, needle, count, info),
            MoveDir1D::Previous => self.find_message_prev(key, needle, count, info, need_load),
        }
    }
}

impl WindowOps<IambInfo> for ScrollbackState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        Scrollback::new(store).focus(focused).render(area, buf, self)
    }

    fn dup(&self, _: &mut ProgramStore) -> Self {
        ScrollbackState {
            room_id: self.room_id.clone(),
            id: self.id.clone(),
            cursor: self.cursor.clone(),
            viewctx: self.viewctx.clone(),
            jumped: self.jumped.clone(),
            show_full_on_redraw: false,
        }
    }

    fn close(&mut self, _: CloseFlags, _: &mut ProgramStore) -> bool {
        // XXX: what's the right closing behaviour for a room?
        // Should write send a message?
        true
    }

    fn write(
        &mut self,
        _: Option<&str>,
        flags: WriteFlags,
        _: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if flags.contains(WriteFlags::FORCE) {
            Ok(None)
        } else {
            Err(EditError::ReadOnly.into())
        }
    }

    fn get_completions(&self) -> Option<CompletionList> {
        None
    }

    fn get_cursor_word(&self, _: &WordStyle) -> Option<String> {
        None
    }

    fn get_selected_word(&self) -> Option<String> {
        None
    }
}

impl EditorActions<ProgramContext, ProgramStore, IambInfo> for ScrollbackState {
    fn edit(
        &mut self,
        operation: &EditAction,
        motion: &EditTarget,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        let info = store.application.rooms.get_or_default(self.room_id.clone());
        let key = if let Some(k) = self.cursor.to_key(info) {
            k.clone()
        } else {
            let msg = "No messages to select.";
            let err = EditError::Failure(msg.to_string());
            return Err(err);
        };

        match operation {
            EditAction::Motion => {
                if motion.is_jumping() {
                    self.jumped.push(self.cursor.clone());
                }

                let pos = match motion {
                    EditTarget::CurrentPosition | EditTarget::Selection => {
                        return Ok(None);
                    },
                    EditTarget::Boundary(rt, inc, term, count) => {
                        self.range(key, rt, *inc, count, ctx, info).map(|r| {
                            match term {
                                MoveTerminus::Beginning => r.start,
                                MoveTerminus::End => r.end,
                            }
                        })
                    },
                    EditTarget::CharJump(mark) | EditTarget::LineJump(mark) => {
                        let mark = ctx.resolve(mark);
                        let cursor = store.cursors.get_mark(self.id.clone(), mark)?;

                        if let mc @ Some(_) = MessageCursor::from_cursor(&cursor, info) {
                            mc
                        } else {
                            let msg = "Failed to restore mark";
                            let err = EditError::Failure(msg.into());

                            return Err(err);
                        }
                    },
                    EditTarget::Motion(mt, count) => self.movement(key, mt, count, ctx, info),
                    EditTarget::Range(_, _, _) => {
                        return Err(EditError::Failure("Cannot use ranges in a list".to_string()));
                    },
                    EditTarget::Search(SearchType::Char(_), _, _) => {
                        let msg = "Cannot perform character search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },
                    EditTarget::Search(SearchType::Regex, flip, count) => {
                        let count = ctx.resolve(count);

                        let dir = ctx.get_search_regex_dir();
                        let dir = flip.resolve(&dir);

                        let needle = match ctx.get_search_regex() {
                            Some(re) => re,
                            None => {
                                let lsearch = store.registers.get(&Register::LastSearch)?;
                                let lsearch = lsearch.value.to_string();

                                Regex::new(lsearch.as_ref())?
                            },
                        };

                        self.find_message(
                            key,
                            dir,
                            &needle,
                            count,
                            info,
                            &mut store.application.need_load,
                        )
                    },
                    EditTarget::Search(SearchType::Word(_, _), _, _) => {
                        let msg = "Cannot perform word search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },

                    _ => {
                        let msg = format!("Unknown editing target: {motion:?}");
                        let err = EditError::Unimplemented(msg);

                        return Err(err);
                    },
                };

                if let Some(pos) = pos {
                    self.cursor = pos;
                }

                self.show_full_on_redraw = true;

                return Ok(None);
            },
            EditAction::Yank => {
                let range = match motion {
                    EditTarget::CurrentPosition | EditTarget::Selection => {
                        Some(self._range_to(key.into()))
                    },
                    EditTarget::Boundary(rt, inc, term, count) => {
                        self.range(key, rt, *inc, count, ctx, info).map(|r| {
                            self._range_to(match term {
                                MoveTerminus::Beginning => r.start,
                                MoveTerminus::End => r.end,
                            })
                        })
                    },
                    EditTarget::CharJump(mark) | EditTarget::LineJump(mark) => {
                        let mark = ctx.resolve(mark);
                        let cursor = store.cursors.get_mark(self.id.clone(), mark)?;

                        if let Some(c) = MessageCursor::from_cursor(&cursor, info) {
                            self._range_to(c).into()
                        } else {
                            let msg = "Failed to restore mark";
                            let err = EditError::Failure(msg.into());

                            return Err(err);
                        }
                    },
                    EditTarget::Motion(mt, count) => {
                        self.range_of_movement(key, mt, count, ctx, info)
                    },
                    EditTarget::Range(rt, inc, count) => {
                        self.range(key, rt, *inc, count, ctx, info)
                    },
                    EditTarget::Search(SearchType::Char(_), _, _) => {
                        let msg = "Cannot perform character search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },
                    EditTarget::Search(SearchType::Regex, flip, count) => {
                        let count = ctx.resolve(count);

                        let dir = ctx.get_search_regex_dir();
                        let dir = flip.resolve(&dir);

                        let needle = match ctx.get_search_regex() {
                            Some(re) => re,
                            None => {
                                let lsearch = store.registers.get(&Register::LastSearch)?;
                                let lsearch = lsearch.value.to_string();

                                Regex::new(lsearch.as_ref())?
                            },
                        };

                        self.find_message(
                            key,
                            dir,
                            &needle,
                            count,
                            info,
                            &mut store.application.need_load,
                        )
                        .map(|c| self._range_to(c))
                    },
                    EditTarget::Search(SearchType::Word(_, _), _, _) => {
                        let msg = "Cannot perform word search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },

                    _ => {
                        let msg = format!("Unknown motion: {motion:?}");
                        let err = EditError::Unimplemented(msg);

                        return Err(err);
                    },
                };

                if let Some(range) = range {
                    let mut yanked = EditRope::from("");

                    for (_, msg) in self.messages(range, info) {
                        yanked += EditRope::from(msg.event.body());
                        yanked += EditRope::from('\n');
                    }

                    let cell = RegisterCell::new(TargetShape::LineWise, yanked);
                    let register = ctx.get_register().unwrap_or(Register::Unnamed);
                    let mut flags = RegisterPutFlags::NONE;

                    if ctx.get_register_append() {
                        flags |= RegisterPutFlags::APPEND;
                    }

                    store.registers.put(&register, cell, flags)?;
                }

                return Ok(None);
            },

            // Everything else is a modifying action.
            EditAction::ChangeCase(_) => Err(EditError::ReadOnly),
            EditAction::ChangeNumber(_, _) => Err(EditError::ReadOnly),
            EditAction::Delete => Err(EditError::ReadOnly),
            EditAction::Format => Err(EditError::ReadOnly),
            EditAction::Indent(_) => Err(EditError::ReadOnly),
            EditAction::Join(_) => Err(EditError::ReadOnly),
            EditAction::Replace(_) => Err(EditError::ReadOnly),
        }
    }

    fn mark(
        &mut self,
        name: Mark,
        _: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        let info = store.application.get_room_info(self.room_id.clone());

        if let Some(cursor) = self.cursor.to_cursor(info) {
            store.cursors.set_mark(self.id.clone(), name, cursor);

            Ok(None)
        } else {
            let msg = "Failed to set mark for message";
            let err = EditError::Failure(msg.into());

            Err(err)
        }
    }

    fn complete(
        &mut self,
        _: &CompletionType,
        _: &CompletionSelection,
        _: &CompletionDisplay,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Err(EditError::ReadOnly)
    }

    fn insert_text(
        &mut self,
        _: &InsertTextAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Err(EditError::ReadOnly)
    }

    fn selection_command(
        &mut self,
        _: &SelectionAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        Err(EditError::Failure("Cannot perform selection actions in a list".into()))
    }

    fn history_command(
        &mut self,
        act: &HistoryAction,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        match act {
            HistoryAction::Checkpoint => Ok(None),
            HistoryAction::Undo(_) => Err(EditError::Failure("Nothing to undo".into())),
            HistoryAction::Redo(_) => Err(EditError::Failure("Nothing to redo".into())),
            _ => Err(EditError::Unimplemented(format!("Unknown action: {act:?}"))),
        }
    }

    fn cursor_command(
        &mut self,
        act: &CursorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        let info = store.application.get_room_info(self.room_id.clone());

        match act {
            CursorAction::Close(_) => Ok(None),
            CursorAction::Rotate(_, _) => Ok(None),
            CursorAction::Split(_) => Ok(None),

            CursorAction::Restore(_) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedCursorGroup);

                // Get saved group.
                let ngroup = store.cursors.get_group(self.id.clone(), &reg)?;

                // Lists don't have groups; override current position.
                if self.jumped.current() != &self.cursor {
                    self.jumped.push(self.cursor.clone());
                }

                if let Some(mc) = MessageCursor::from_cursor(ngroup.leader.cursor(), info) {
                    self.cursor = mc;

                    Ok(None)
                } else {
                    let msg = "Cannot restore position in message history";
                    let err = EditError::Failure(msg.into());

                    Err(err)
                }
            },
            CursorAction::Save(_) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedCursorGroup);

                // Lists don't have groups; override any previously saved group.
                let cursor = self.cursor.to_cursor(info).ok_or_else(|| {
                    let msg = "Cannot save position in message history";
                    EditError::Failure(msg.into())
                })?;

                let group = CursorGroup {
                    leader: CursorState::Location(cursor),
                    members: vec![],
                };

                store.cursors.set_group(self.id.clone(), reg, group)?;

                Ok(None)
            },
            _ => Err(EditError::Unimplemented(format!("Unknown action: {act:?}"))),
        }
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for ScrollbackState {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        match act {
            EditorAction::Cursor(act) => self.cursor_command(act, ctx, store),
            EditorAction::Edit(ea, et) => self.edit(&ctx.resolve(ea), et, ctx, store),
            EditorAction::History(act) => self.history_command(act, ctx, store),
            EditorAction::InsertText(act) => self.insert_text(act, ctx, store),
            EditorAction::Mark(name) => self.mark(ctx.resolve(name), ctx, store),
            EditorAction::Selection(act) => self.selection_command(act, ctx, store),

            EditorAction::Complete(_, _, _) => {
                let msg = "Nothing to complete in message scrollback";
                let err = EditError::Failure(msg.into());

                Err(err)
            },

            _ => Err(EditError::Unimplemented(format!("Unknown action: {act:?}"))),
        }
    }
}

impl Jumpable<ProgramContext, IambInfo> for ScrollbackState {
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        _: &ProgramContext,
    ) -> UIResult<usize, IambInfo> {
        match list {
            PositionList::ChangeList => {
                let msg = "No changes to jump to within the list";
                let err = UIError::Failure(msg.into());

                return Err(err);
            },
            PositionList::JumpList => {
                let (len, pos) = match dir {
                    MoveDir1D::Previous => {
                        if self.jumped.future_len() == 0 && *self.jumped.current() != self.cursor {
                            // Push current position if this is the first jump backwards.
                            self.jumped.push(self.cursor.clone());
                        }

                        let plen = self.jumped.past_len();
                        let pos = self.jumped.prev(count);

                        (plen, pos)
                    },
                    MoveDir1D::Next => {
                        let flen = self.jumped.future_len();
                        let pos = self.jumped.next(count);

                        (flen, pos)
                    },
                };

                if len > 0 {
                    self.cursor = pos.clone();
                }

                return Ok(count.saturating_sub(len));
            },
        }
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for ScrollbackState {
    fn prompt(
        &mut self,
        act: &PromptAction,
        _: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(Action<IambInfo>, ProgramContext)>, IambInfo> {
        let info = store.application.get_room_info(self.room_id.clone());

        let _ = if let Some(key) = self.cursor.to_key(info) {
            key
        } else {
            let msg = "No message currently selected";
            let err = EditError::Failure(msg.into());

            return Err(err);
        };

        match act {
            PromptAction::Submit => {
                // XXX: I'm not sure exactly what to do here yet. I think I want this to display a
                // pop-over ListState with actions that can then be submitted:
                //
                // - Create a reply
                // - Edit a message
                // - Redact a message
                // - React to a message
                // - Report a message
                // - Download an attachment
                //
                // Each of these should correspond to a command that a user can run. For example,
                // running `:reply` when hovering over a message should be equivalent to opening
                // the pop-up and selecting "Reply To This Message".
                return Ok(vec![]);
            },
            PromptAction::Abort(..) => {
                let msg = "Cannot abort a message.";
                let err = EditError::Failure(msg.into());

                return Err(err);
            },
            PromptAction::Recall(..) => {
                let msg = "Cannot recall previous messages.";
                let err = EditError::Failure(msg.into());

                return Err(err);
            },
            _ => {
                let msg = format!("Messages scrollback doesn't support {act:?}");
                let err = EditError::Unimplemented(msg);

                return Err(err);
            },
        }
    }
}

impl ScrollActions<ProgramContext, ProgramStore, IambInfo> for ScrollbackState {
    fn dirscroll(
        &mut self,
        dir: MoveDir2D,
        size: ScrollSize,
        count: &Count,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        let info = store.application.rooms.get_or_default(self.room_id.clone());
        let settings = &store.application.settings;
        let mut corner = self.viewctx.corner.clone();

        let last_key = if let Some(k) = info.messages.last_key_value() {
            k.0
        } else {
            return Ok(None);
        };

        let corner_key = corner.timestamp.as_ref().unwrap_or(last_key).clone();
        let cursor_key = self.cursor.timestamp.as_ref().unwrap_or(last_key);

        let count = ctx.resolve(count);
        let height = self.viewctx.get_height();
        let mut rows = match size {
            ScrollSize::Cell => count,
            ScrollSize::HalfPage => count.saturating_mul(height) / 2,
            ScrollSize::Page => count.saturating_mul(height),
        };

        match dir {
            MoveDir2D::Up => {
                let first_key = info.messages.first_key_value().map(|f| f.0.clone());

                for (key, item) in info.messages.range(..=&corner_key).rev() {
                    let sel = key == cursor_key;
                    let prev = prevmsg(key, info);
                    let txt = item.show(prev, sel, &self.viewctx, info, settings);
                    let len = txt.height().max(1);
                    let max = len.saturating_sub(1);

                    if key != &corner_key {
                        corner.text_row = max;
                    }

                    corner.timestamp = key.clone().into();

                    if rows == 0 {
                        break;
                    } else if corner.text_row >= rows {
                        corner.text_row -= rows;
                        break;
                    } else if corner.timestamp == first_key {
                        corner.text_row = 0;
                        break;
                    }

                    rows -= corner.text_row + 1;
                }
            },
            MoveDir2D::Down => {
                let mut prev = prevmsg(&corner_key, info);

                for (key, item) in info.messages.range(&corner_key..) {
                    let sel = key == cursor_key;
                    let txt = item.show(prev, sel, &self.viewctx, info, settings);
                    let len = txt.height().max(1);
                    let max = len.saturating_sub(1);

                    prev = Some(item);

                    if key != &corner_key {
                        corner.text_row = 0;
                    }

                    corner.timestamp = key.clone().into();

                    if rows == 0 {
                        break;
                    } else if key == last_key {
                        corner.text_row = corner.text_row.saturating_add(rows).min(max);
                        break;
                    } else if corner.text_row >= max {
                        rows -= 1;
                        continue;
                    } else if corner.text_row + rows <= max {
                        corner.text_row += rows;
                        break;
                    } else {
                        rows -= len - corner.text_row;
                        continue;
                    }
                }
            },
            MoveDir2D::Left | MoveDir2D::Right => {
                let msg = "Cannot scroll vertically in message scrollback";
                let err = EditError::Failure(msg.into());

                return Err(err);
            },
        }

        self.viewctx.corner = corner;
        self.shift_cursor(info, settings);

        Ok(None)
    }

    fn cursorpos(
        &mut self,
        pos: MovePosition,
        axis: Axis,
        _: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        match axis {
            Axis::Horizontal => {
                let msg = "Cannot scroll vertically in message scrollback";
                let err = EditError::Failure(msg.into());

                Err(err)
            },
            Axis::Vertical => {
                let info = store.application.rooms.get_or_default(self.room_id.clone());
                let settings = &store.application.settings;

                if let Some(key) = self.cursor.to_key(info).cloned() {
                    self.scrollview(key, pos, info, settings);
                }

                Ok(None)
            },
        }
    }

    fn linepos(
        &mut self,
        _: MovePosition,
        _: &Count,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        let msg = "Cannot scroll in message scrollback using line numbers";
        let err = EditError::Failure(msg.into());

        Err(err)
    }
}

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for ScrollbackState {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        match style {
            ScrollStyle::Direction2D(dir, size, count) => {
                return self.dirscroll(*dir, *size, count, ctx, store);
            },
            ScrollStyle::CursorPos(pos, axis) => {
                return self.cursorpos(*pos, *axis, ctx, store);
            },
            ScrollStyle::LinePos(pos, count) => {
                return self.linepos(*pos, count, ctx, store);
            },
        }
    }
}

impl Searchable<ProgramContext, ProgramStore, IambInfo> for ScrollbackState {
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> UIResult<EditInfo, IambInfo> {
        let search = EditTarget::Search(SearchType::Regex, dir, count);

        Ok(self.edit(&EditAction::Motion, &search, ctx, store)?)
    }
}

impl TerminalCursor for ScrollbackState {
    fn get_term_cursor(&self) -> Option<(u16, u16)> {
        None
    }
}

pub struct Scrollback<'a> {
    focused: bool,
    store: &'a mut ProgramStore,
}

impl<'a> Scrollback<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Self {
        Scrollback { focused: false, store }
    }

    /// Indicate whether the scrollback is currently focused.
    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl<'a> StatefulWidget for Scrollback<'a> {
    type State = ScrollbackState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let info = self.store.application.rooms.get_or_default(state.room_id.clone());
        let settings = &self.store.application.settings;
        let area = info.render_typing(area, buf, &self.store.application.settings);

        state.set_term_info(area);

        let height = state.viewctx.get_height();

        if height == 0 {
            return;
        }

        if state.cursor.timestamp < state.viewctx.corner.timestamp {
            state.viewctx.corner = state.cursor.clone();
        }

        let cursor = &state.cursor;
        let cursor_key = if let Some(k) = cursor.to_key(info) {
            k
        } else {
            self.store.application.mark_for_load(state.room_id.clone());
            return;
        };

        let corner = &state.viewctx.corner;
        let corner_key = if let Some(k) = &corner.timestamp {
            k.clone()
        } else {
            nth_key_before(cursor_key.clone(), height, info)
        };

        let foc = self.focused || cursor.timestamp.is_some();
        let full = std::mem::take(&mut state.show_full_on_redraw) || cursor.timestamp.is_none();
        let mut lines = vec![];
        let mut sawit = false;
        let mut prev = prevmsg(&corner_key, info);

        for (key, item) in info.messages.range(&corner_key..) {
            let sel = key == cursor_key;
            let txt = item.show(prev, foc && sel, &state.viewctx, info, settings);

            prev = Some(item);

            let incomplete_ok = !full || !sel;

            for (row, line) in txt.lines.into_iter().enumerate() {
                if sawit && lines.len() >= height && incomplete_ok {
                    // Check whether we've seen the first line of the
                    // selected message and can fill the screen.
                    break;
                }

                if key == &corner_key && row < corner.text_row {
                    // Skip rows above the viewport corner.
                    continue;
                }

                lines.push((key, row, line));
                sawit |= sel;
            }
        }

        if lines.len() > height {
            let n = lines.len() - height;
            let _ = lines.drain(..n);
        }

        if let Some(((ts, event_id), row, _)) = lines.first() {
            state.viewctx.corner.timestamp = Some((*ts, event_id.clone()));
            state.viewctx.corner.text_row = *row;
        }

        let mut y = area.top();
        let x = area.left();

        for (_, _, txt) in lines.into_iter() {
            let _ = buf.set_spans(x, y, &txt, area.width);

            y += 1;
        }

        if settings.tunables.read_receipt_send && state.cursor.timestamp.is_none() {
            // If the cursor is at the last message, then update the read marker.
            info.read_till = info.messages.last_key_value().map(|(k, _)| k.1.clone());
        }

        // Check whether we should load older messages for this room.
        let first_key = info.messages.first_key_value().map(|(k, _)| k.clone());
        if first_key == state.viewctx.corner.timestamp {
            // If the top of the screen is the older message, load more.
            self.store.application.mark_for_load(state.room_id.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[tokio::test]
    async fn test_search_messages() {
        let room_id = TEST_ROOM1_ID.clone();
        let mut store = mock_store().await;
        let mut scrollback = ScrollbackState::new(room_id.clone());
        let ctx = ProgramContext::default();

        let next = MoveDirMod::Exact(MoveDir1D::Next);
        let prev = MoveDirMod::Exact(MoveDir1D::Previous);

        // Search through the messages:
        //
        // MSG2: "helium"
        // MSG3: "this\nis\na\nmultiline\nmessage"
        // MSG4: "help"
        // MSG5: "character"
        // MSG1: "writhe"
        store.set_last_search("he");

        assert_eq!(scrollback.cursor, MessageCursor::latest());

        // Search backwards to MSG4.
        scrollback.search(prev.clone(), 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG4_KEY.clone().into());

        // Search backwards to MSG2.
        scrollback.search(prev.clone(), 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG2_KEY.clone().into());
        assert_eq!(store.application.need_load.contains(&room_id), false);

        // Can't go any further; need_load now contains the room ID.
        scrollback.search(prev.clone(), 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG2_KEY.clone().into());
        assert_eq!(store.application.need_load.contains(&room_id), true);

        // Search forward twice to MSG1.
        scrollback.search(next.clone(), 2.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG1_KEY.clone().into());

        // Can't go any further.
        scrollback.search(next.clone(), 2.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG1_KEY.clone().into());
    }

    #[tokio::test]
    async fn test_movement() {
        let mut store = mock_store().await;
        let mut scrollback = ScrollbackState::new(TEST_ROOM1_ID.clone());
        let ctx = ProgramContext::default();

        let prev = |n: usize| EditTarget::Motion(MoveType::Line(MoveDir1D::Previous), n.into());

        let next = |n: usize| EditTarget::Motion(MoveType::Line(MoveDir1D::Next), n.into());

        assert_eq!(scrollback.cursor, MessageCursor::latest());

        scrollback.edit(&EditAction::Motion, &prev(1), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG5_KEY.clone().into());

        scrollback.edit(&EditAction::Motion, &prev(2), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG3_KEY.clone().into());

        scrollback.edit(&EditAction::Motion, &prev(5), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG2_KEY.clone().into());

        scrollback.edit(&EditAction::Motion, &next(1), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG3_KEY.clone().into());

        scrollback.edit(&EditAction::Motion, &next(1), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG4_KEY.clone().into());

        scrollback.edit(&EditAction::Motion, &next(1), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG5_KEY.clone().into());

        scrollback.edit(&EditAction::Motion, &next(1), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG1_KEY.clone().into());
    }

    #[tokio::test]
    async fn test_dirscroll() {
        let mut store = mock_store().await;
        let mut scrollback = ScrollbackState::new(TEST_ROOM1_ID.clone());
        let ctx = ProgramContext::default();

        let prev = MoveDir2D::Up;
        let next = MoveDir2D::Down;

        // Skip rendering typing notices.
        store.application.settings.tunables.typing_notice_display = false;

        assert_eq!(scrollback.cursor, MessageCursor::latest());
        assert_eq!(scrollback.viewctx.dimensions, (0, 0));
        assert_eq!(scrollback.viewctx.corner, MessageCursor::latest());

        // Set a terminal width of 60, and height of 4, rendering in scrollback as:
        //
        //       |------------------------------------------------------------|
        // MSG2: |                Wednesday, December 31 1969                 |
        //       |           @user2:example.com  helium                       |
        // MSG3: |           @user2:example.com  this                         |
        //       |                               is                           |
        //       |                               a                            |
        //       |                               multiline                    |
        //       |                               message                      |
        // MSG4: |           @user1:example.com  help                         |
        // MSG5: |           @user2:example.com  character                    |
        // MSG1: |                   XXXday, Month NN 20XX                    |
        //       |           @user1:example.com  writhe                       |
        //       |------------------------------------------------------------|
        let area = Rect::new(0, 0, 60, 4);
        let mut buffer = Buffer::empty(area);
        scrollback.draw(area, &mut buffer, true, &mut store);

        assert_eq!(scrollback.cursor, MessageCursor::latest());
        assert_eq!(scrollback.viewctx.dimensions, (60, 4));
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG4_KEY.clone(), 0));

        // Scroll up a line at a time until we hit the first message.
        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 4));

        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 3));

        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 2));

        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 1));

        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 0));

        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG2_KEY.clone(), 1));

        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG2_KEY.clone(), 0));

        // Cannot scroll any further.
        scrollback
            .dirscroll(prev, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG2_KEY.clone(), 0));

        // Now scroll back down one line at a time.
        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG2_KEY.clone(), 1));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 0));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 1));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 2));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 3));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 4));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG4_KEY.clone(), 0));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG5_KEY.clone(), 0));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG1_KEY.clone(), 0));

        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG1_KEY.clone(), 1));

        // Cannot scroll down any further.
        scrollback
            .dirscroll(next, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG1_KEY.clone(), 1));

        // Scroll up two Pages (eight lines).
        scrollback
            .dirscroll(prev, ScrollSize::Page, &2.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 0));

        // Scroll down two HalfPages (four lines).
        scrollback
            .dirscroll(next, ScrollSize::HalfPage, &2.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 4));
    }

    #[tokio::test]
    async fn test_cursorpos() {
        let mut store = mock_store().await;
        let mut scrollback = ScrollbackState::new(TEST_ROOM1_ID.clone());
        let ctx = ProgramContext::default();

        // Skip rendering typing notices.
        store.application.settings.tunables.typing_notice_display = false;

        // Set a terminal width of 60, and height of 3, rendering in scrollback as:
        //
        //       |------------------------------------------------------------|
        // MSG2: |                Wednesday, December 31 1969                 |
        //       |           @user2:example.com  helium                       |
        // MSG3: |           @user2:example.com  this                         |
        //       |                               is                           |
        //       |                               a                            |
        //       |                               multiline                    |
        //       |                               message                      |
        // MSG4: |           @user1:example.com  help                         |
        // MSG5: |           @user2:example.com  character                    |
        // MSG1: |                   XXXday, Month NN 20XX                    |
        //       |           @user1:example.com  writhe                       |
        //       |------------------------------------------------------------|
        let area = Rect::new(0, 0, 60, 3);
        let mut buffer = Buffer::empty(area);
        scrollback.cursor = MSG4_KEY.clone().into();
        scrollback.draw(area, &mut buffer, true, &mut store);

        assert_eq!(scrollback.cursor, MSG4_KEY.clone().into());
        assert_eq!(scrollback.viewctx.dimensions, (60, 3));
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 3));

        // Scroll so that the cursor is at the top of the screen.
        scrollback
            .cursorpos(MovePosition::Beginning, Axis::Vertical, &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.cursor, MSG4_KEY.clone().into());
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG4_KEY.clone(), 0));

        // Scroll so that the cursor is at the bottom of the screen.
        scrollback
            .cursorpos(MovePosition::End, Axis::Vertical, &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.cursor, MSG4_KEY.clone().into());
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 3));

        // Scroll so that the cursor is in the middle of the screen.
        scrollback
            .cursorpos(MovePosition::Middle, Axis::Vertical, &ctx, &mut store)
            .unwrap();
        assert_eq!(scrollback.cursor, MSG4_KEY.clone().into());
        assert_eq!(scrollback.viewctx.corner, MessageCursor::new(MSG3_KEY.clone(), 4));
    }
}
