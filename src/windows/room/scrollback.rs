//! Message scrollback
use ratatui_image::Image;
use regex::Regex;

use matrix_sdk::ruma::{OwnedEventId, OwnedRoomId};

use modalkit_ratatui::{ScrollActions, TerminalCursor, WindowOps};
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Modifier as StyleModifier, Style},
    text::{Line, Span},
    widgets::{Paragraph, StatefulWidget, Widget},
};

use modalkit::actions::{
    Action,
    CursorAction,
    EditAction,
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
    WindowAction,
};
use modalkit::editing::{
    completion::CompletionList,
    context::Resolve,
    cursor::{CursorGroup, CursorState},
    history::HistoryList,
    rope::EditRope,
    store::{RegisterCell, RegisterPutFlags},
};
use modalkit::errors::{EditError, EditResult, UIError, UIResult};
use modalkit::prelude::*;

use crate::{
    base::{
        IambBufferId,
        IambId,
        IambInfo,
        IambResult,
        Need,
        ProgramContext,
        ProgramStore,
        RoomFetchStatus,
        RoomFocus,
        RoomInfo,
    },
    config::ApplicationSettings,
    message::{Message, MessageCursor, MessageKey, Messages},
};

fn no_msgs() -> EditError<IambInfo> {
    let msg = "No messages to select.";
    EditError::Failure(msg.to_string())
}

fn nth_key_before(pos: MessageKey, n: usize, thread: &Messages) -> MessageKey {
    let mut end = &pos;
    let iter = thread.range(..=&pos).rev().enumerate();

    for (i, (key, _)) in iter {
        end = key;

        if i >= n {
            break;
        }
    }

    end.clone()
}

fn nth_before(pos: MessageKey, n: usize, thread: &Messages) -> MessageCursor {
    nth_key_before(pos, n, thread).into()
}

fn nth_key_after(pos: MessageKey, n: usize, thread: &Messages) -> MessageKey {
    let mut end = &pos;
    let iter = thread.range(&pos..).enumerate();

    for (i, (key, _)) in iter {
        end = key;

        if i >= n {
            break;
        }
    }

    end.clone()
}

fn nth_after(pos: MessageKey, n: usize, thread: &Messages) -> MessageCursor {
    nth_key_after(pos, n, thread).into()
}

fn prevmsg<'a>(key: &MessageKey, thread: &'a Messages) -> Option<&'a Message> {
    thread.range(..key).next_back().map(|(_, v)| v)
}

pub struct ScrollbackState {
    /// The room identifier.
    room_id: OwnedRoomId,

    /// The buffer identifier used for saving marks, etc.
    id: IambBufferId,

    /// The currently focused thread in this room.
    thread: Option<OwnedEventId>,

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
    pub fn new(room_id: OwnedRoomId, thread: Option<OwnedEventId>) -> ScrollbackState {
        let id = IambBufferId::Room(room_id.to_owned(), thread.clone(), RoomFocus::Scrollback);
        let cursor = MessageCursor::default();
        let viewctx = ViewportContext::default();
        let jumped = HistoryList::default();
        let show_full_on_redraw = false;

        ScrollbackState {
            room_id,
            id,
            thread,
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
            .or_else(|| self.get_thread(info)?.last_key_value().map(|kv| kv.0.clone()))
    }

    pub fn get_mut<'a>(&mut self, info: &'a mut RoomInfo) -> Option<&'a mut Message> {
        let thread = self.get_thread_mut(info);

        if let Some(k) = &self.cursor.timestamp {
            thread.get_mut(k)
        } else {
            thread.last_entry().map(|o| o.into_mut())
        }
    }

    pub fn thread(&self) -> Option<&OwnedEventId> {
        self.thread.as_ref()
    }

    pub fn get_thread<'a>(&self, info: &'a RoomInfo) -> Option<&'a Messages> {
        info.get_thread(self.thread.as_deref())
    }

    pub fn get_thread_mut<'a>(&self, info: &'a mut RoomInfo) -> &'a mut Messages {
        info.get_thread_mut(self.thread.clone())
    }

    pub fn messages<'a>(
        &self,
        range: EditRange<MessageCursor>,
        info: &'a RoomInfo,
    ) -> impl Iterator<Item = (&'a MessageKey, &'a Message)> {
        let Some(thread) = self.get_thread(info) else {
            return Default::default();
        };

        let start = range.start.to_key(thread);
        let end = range.end.to_key(thread);

        let (start, end) = if let (Some(start), Some(end)) = (start, end) {
            (start, end)
        } else if let Some((last, _)) = thread.last_key_value() {
            (last, last)
        } else {
            return thread.range(..);
        };

        if range.inclusive {
            thread.range(start..=end)
        } else {
            thread.range(start..end)
        }
    }

    fn need_more_messages(&self, info: &RoomInfo) -> bool {
        match info.fetch_id {
            // Don't fetch if we've already hit the end of history.
            RoomFetchStatus::Done => return false,
            // Fetch at least once if we're viewing a room.
            RoomFetchStatus::NotStarted => return true,
            _ => {},
        }

        let first_key = self.get_thread(info).and_then(|t| t.first_key_value()).map(|(k, _)| k);
        let at_top = first_key == self.viewctx.corner.timestamp.as_ref();

        match (at_top, self.thread.as_ref()) {
            (false, _) => {
                // Not scrolled to top, don't fetch.
                false
            },
            (true, None) => {
                // Scrolled to top in non-thread, fetch.
                true
            },
            (true, Some(thread_root)) => {
                // Scrolled to top in thread, fetch until we have the thread root.
                //
                // Typically, if the user has entered a thread view, we should already have fetched
                // all the way back to the thread root, but it is technically possible via :threads
                // or when restoring a thread view in the layout at startup to not have the message
                // yet.
                !info.keys.contains_key(thread_root)
            },
        }
    }

    fn scrollview(
        &mut self,
        idx: MessageKey,
        pos: MovePosition,
        info: &RoomInfo,
        settings: &ApplicationSettings,
    ) {
        let Some(thread) = self.get_thread(info) else {
            return;
        };

        let selidx = if let Some(key) = self.cursor.to_key(thread) {
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

                for (key, item) in thread.range(..=&idx).rev() {
                    let sel = selidx == key;
                    let prev = prevmsg(key, thread);
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

                for (key, item) in thread.range(..=&idx).rev() {
                    let sel = key == selidx;
                    let prev = prevmsg(key, thread);
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

    fn jump_changed(&mut self) -> bool {
        self.jumped.current() != &self.cursor
    }

    fn push_jump(&mut self) {
        self.jumped.push(self.cursor.clone());
    }

    fn shift_cursor(&mut self, info: &RoomInfo, settings: &ApplicationSettings) {
        let Some(thread) = self.get_thread(info) else {
            return;
        };

        let last_key = if let Some(k) = thread.last_key_value() {
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
        let mut prev = prevmsg(cursor_key, thread);

        for (idx, item) in thread.range(corner_key.clone()..) {
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
                let start = self.get_thread(info)?.first_key_value()?.0.clone();

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
                let thread = self.get_thread(info)?;

                match dir {
                    MoveDir1D::Previous => nth_before(pos, count, thread).into(),
                    MoveDir1D::Next => nth_after(pos, count, thread).into(),
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
                let thread = self.get_thread(info)?;
                let start = thread.first_key_value()?.0.clone();
                let end = thread.last_key_value()?.0.clone();

                Some(EditRange::inclusive(start.into(), end.into(), TargetShape::LineWise))
            },
            RangeType::Line | RangeType::Paragraph | RangeType::Sentence => {
                let thread = self.get_thread(info)?;
                let count = ctx.resolve(count);

                if count == 0 {
                    return None;
                }

                let mut end = &pos;

                for (i, (key, _)) in thread.range(&pos..).enumerate() {
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
        let thread = self.get_thread(info)?;
        let mut mc = None;

        for (key, msg) in thread.range(&start..) {
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
    ) -> (Option<MessageCursor>, bool) {
        let mut mc = None;

        let Some(thread) = self.get_thread(info) else {
            return (None, false);
        };

        for (key, msg) in thread.range(..&end).rev() {
            if count == 0 {
                break;
            }

            if needle.is_match(msg.event.body().as_ref()) {
                mc = MessageCursor::from(key.clone()).into();
                count -= 1;
            }
        }

        return (mc, count > 0);
    }

    fn find_message(
        &self,
        key: MessageKey,
        dir: MoveDir1D,
        needle: &Regex,
        count: usize,
        info: &RoomInfo,
    ) -> (Option<MessageCursor>, bool) {
        match dir {
            MoveDir1D::Next => (self.find_message_next(key, needle, count, info), false),
            MoveDir1D::Previous => self.find_message_prev(key, needle, count, info),
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
            thread: self.thread.clone(),
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
        let thread = self.get_thread(info).ok_or_else(no_msgs)?;
        let key = self.cursor.to_key(thread).ok_or_else(no_msgs)?.clone();

        match operation {
            EditAction::Motion => {
                if motion.is_jumping() {
                    self.push_jump();
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

                        if let Some(mc) = MessageCursor::from_cursor(&cursor, thread) {
                            Some(mc)
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

                        let lsearch = store.registers.get_last_search().to_string();
                        let needle = Regex::new(lsearch.as_ref())?;

                        let (mc, needs_load) = self.find_message(key, dir, &needle, count, info);
                        if needs_load {
                            store
                                .application
                                .need_load
                                .insert(self.room_id.clone(), Need::MESSAGES);
                        }
                        mc
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

                        if let Some(c) = MessageCursor::from_cursor(&cursor, thread) {
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

                        let lsearch = store.registers.get_last_search().to_string();
                        let needle = Regex::new(lsearch.as_ref())?;

                        let (mc, needs_load) = self.find_message(key, dir, &needle, count, info);
                        if needs_load {
                            store
                                .application
                                .need_load
                                .insert(self.room_id.to_owned(), Need::MESSAGES);
                        }

                        mc.map(|c| self._range_to(c))
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
        let thread = self.get_thread(info).ok_or_else(no_msgs)?;
        let cursor = self.cursor.to_cursor(thread).ok_or_else(no_msgs)?;
        store.cursors.set_mark(self.id.clone(), name, cursor);

        Ok(None)
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
        }
    }

    fn cursor_command(
        &mut self,
        act: &CursorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        let info = store.application.get_room_info(self.room_id.clone());
        let thread = self.get_thread(info).ok_or_else(no_msgs)?;

        match act {
            CursorAction::Close(_) => Ok(None),
            CursorAction::Rotate(_, _) => Ok(None),
            CursorAction::Split(_) => Ok(None),

            CursorAction::Restore(_) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedCursorGroup);

                // Get saved group.
                let ngroup = store.cursors.get_group(self.id.clone(), &reg)?;

                // Lists don't have groups; override current position.
                if self.jump_changed() {
                    self.push_jump();
                }

                if let Some(mc) = MessageCursor::from_cursor(ngroup.leader.cursor(), thread) {
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
                let cursor = self.cursor.to_cursor(thread).ok_or_else(|| {
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

                Err(err)
            },
            PositionList::JumpList => {
                let (len, pos) = match dir {
                    MoveDir1D::Previous => {
                        if self.jumped.future_len() == 0 && self.jump_changed() {
                            // Push current position if this is the first jump backwards.
                            self.push_jump();
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

                Ok(count.saturating_sub(len))
            },
        }
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for ScrollbackState {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(Action<IambInfo>, ProgramContext)>, IambInfo> {
        let info = store.application.get_room_info(self.room_id.clone());
        let thread = self.get_thread(info).ok_or_else(no_msgs)?;

        let Some(key) = self.cursor.to_key(thread) else {
            let msg = "No message currently selected";
            let err = EditError::Failure(msg.into());
            return Err(err);
        };

        match act {
            PromptAction::Submit => {
                if self.thread.is_some() {
                    let msg =
                        "You are already in a thread. Use :reply to reply to a specific message.";
                    let err = EditError::Failure(msg.into());
                    Err(err)
                } else {
                    let root = key.1.clone();
                    let room_id = self.room_id.clone();
                    let id = IambId::Room(room_id, Some(root));
                    let open = WindowAction::Switch(OpenTarget::Application(id));
                    Ok(vec![(open.into(), ctx.clone())])
                }
            },
            PromptAction::Abort(..) => {
                let msg = "Cannot abort a message.";
                let err = EditError::Failure(msg.into());
                Err(err)
            },
            PromptAction::Recall(..) => {
                let msg = "Cannot recall previous messages.";
                let err = EditError::Failure(msg.into());
                Err(err)
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
        let thread = self.get_thread(info).ok_or_else(no_msgs)?;

        let last_key = if let Some(k) = thread.last_key_value() {
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
                let first_key = thread.first_key_value().map(|f| f.0.clone());

                for (key, item) in thread.range(..=&corner_key).rev() {
                    let sel = key == cursor_key;
                    let prev = prevmsg(key, thread);
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
                let mut prev = prevmsg(&corner_key, thread);

                for (key, item) in thread.range(&corner_key..) {
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
                let thread = self.get_thread(info).ok_or_else(no_msgs)?;

                if let Some(key) = self.cursor.to_key(thread).cloned() {
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

fn render_jump_to_recent(area: Rect, buf: &mut Buffer, focused: bool) -> Rect {
    if area.height <= 5 || area.width <= 20 {
        return area;
    }

    let top = Rect::new(area.x, area.y, area.width, area.height - 1);
    let bar = Rect::new(area.x, area.y + top.height, area.width, 1);
    let msg = vec![
        Span::raw("Use "),
        Span::styled("G", Style::default().add_modifier(StyleModifier::BOLD)),
        Span::raw(if focused { "" } else { " in scrollback" }),
        Span::raw(" to jump to latest message"),
    ];

    Paragraph::new(Line::from(msg))
        .alignment(Alignment::Center)
        .render(bar, buf);

    return top;
}

pub struct Scrollback<'a> {
    room_focused: bool,
    focused: bool,
    store: &'a mut ProgramStore,
}

impl<'a> Scrollback<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Self {
        Scrollback { room_focused: false, focused: false, store }
    }

    /// Indicate whether the room window is currently focused, regardless of whether the scrollback
    /// also is.
    pub fn room_focus(mut self, focused: bool) -> Self {
        self.room_focused = focused;
        self
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
        let area = if state.cursor.timestamp.is_some() {
            render_jump_to_recent(area, buf, self.focused)
        } else {
            info.render_typing(area, buf, &self.store.application.settings)
        };

        state.set_term_info(area);

        let height = state.viewctx.get_height();

        if height == 0 {
            return;
        }

        let Some(thread) = state.get_thread(info) else {
            return;
        };

        if state.cursor.timestamp < state.viewctx.corner.timestamp {
            state.viewctx.corner = state.cursor.clone();
        }

        let cursor = &state.cursor;
        let cursor_key = if let Some(k) = cursor.to_key(thread) {
            k
        } else {
            if state.need_more_messages(info) {
                self.store
                    .application
                    .need_load
                    .insert(state.room_id.to_owned(), Need::MESSAGES);
            }
            return;
        };

        let corner = &state.viewctx.corner;
        let corner_key = if let Some(k) = &corner.timestamp {
            k.clone()
        } else {
            nth_key_before(cursor_key.clone(), height, thread)
        };

        let foc = self.focused || cursor.timestamp.is_some();
        let full = std::mem::take(&mut state.show_full_on_redraw) || cursor.timestamp.is_none();
        let mut lines = vec![];
        let mut sawit = false;
        let mut prev = prevmsg(&corner_key, thread);

        for (key, item) in thread.range(&corner_key..) {
            let sel = key == cursor_key;
            let (txt, [mut msg_preview, mut reply_preview]) =
                item.show_with_preview(prev, foc && sel, &state.viewctx, info, settings);

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

                // Only take the preview into the matching row number.
                // `reply` and `msg` previews are on rows,
                // so an `or` works to pick the one that matches (if any)
                let line_preview = match msg_preview {
                    Some((_, _, y)) if y as usize == row => msg_preview.take(),
                    _ => None,
                }
                .or(match reply_preview {
                    Some((_, _, y)) if y as usize == row => reply_preview.take(),
                    _ => None,
                });

                lines.push((key, row, line, line_preview));
                sawit |= sel;
            }

            prev = Some(item);
        }

        if lines.len() > height {
            let n = lines.len() - height;
            let _ = lines.drain(..n);
        }

        if let Some(((ts, event_id), row, _, _)) = lines.first() {
            state.viewctx.corner.timestamp = Some((*ts, event_id.clone()));
            state.viewctx.corner.text_row = *row;
        }

        let mut y = area.top();
        let x = area.left();

        let mut image_previews = vec![];
        for ((_, _), _, txt, line_preview) in lines.into_iter() {
            let _ = buf.set_line(x, y, &txt, area.width);
            if let Some((backend, msg_x, _)) = line_preview {
                image_previews.push((x + msg_x, y, backend));
            }

            y += 1;
        }
        // Render image previews after all text lines have been drawn, as the render might draw below the current
        // line.
        for (x, y, backend) in image_previews {
            let image_widget = Image::new(backend);
            let mut rect = backend.rect();
            rect.x = x;
            rect.y = y;
            // Don't render outside of scrollback area
            if rect.bottom() <= area.bottom() && rect.right() <= area.right() {
                image_widget.render(rect, buf);
            }
        }

        if self.room_focused &&
            settings.tunables.read_receipt_send &&
            state.cursor.timestamp.is_none()
        {
            // If the cursor is at the last message, then update the read marker.
            if let Some((k, _)) = thread.last_key_value() {
                info.set_receipt(settings.profile.user_id.clone(), k.1.clone());
            }
        }

        // Check whether we should load older messages for this room.
        if state.need_more_messages(info) {
            // If the top of the screen is the older message, load more.
            self.store
                .application
                .need_load
                .insert(state.room_id.to_owned(), Need::MESSAGES);
        }

        info.draw_last = self.store.application.draw_curr;
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
        let mut scrollback = ScrollbackState::new(room_id.clone(), None);
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
        store.registers.set_last_search("he");

        assert_eq!(scrollback.cursor, MessageCursor::latest());

        // Search backwards to MSG4.
        scrollback.search(prev, 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG4_KEY.clone().into());

        // Search backwards to MSG2.
        scrollback.search(prev, 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG2_KEY.clone().into());
        assert_eq!(
            std::mem::take(&mut store.application.need_load)
                .into_iter()
                .collect::<Vec<(OwnedRoomId, Need)>>()
                .is_empty(),
            true,
        );

        // Can't go any further; need_load now contains the room ID.
        scrollback.search(prev, 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG2_KEY.clone().into());
        assert_eq!(
            std::mem::take(&mut store.application.need_load)
                .into_iter()
                .collect::<Vec<(OwnedRoomId, Need)>>(),
            vec![(room_id.clone(), Need::MESSAGES)]
        );

        // Search forward twice to MSG1.
        scrollback.search(next, 2.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG1_KEY.clone().into());

        // Can't go any further.
        scrollback.search(next, 2.into(), &ctx, &mut store).unwrap();
        assert_eq!(scrollback.cursor, MSG1_KEY.clone().into());
    }

    #[tokio::test]
    async fn test_movement() {
        let mut store = mock_store().await;
        let mut scrollback = ScrollbackState::new(TEST_ROOM1_ID.clone(), None);
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
        let mut scrollback = ScrollbackState::new(TEST_ROOM1_ID.clone(), None);
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
        let mut scrollback = ScrollbackState::new(TEST_ROOM1_ID.clone(), None);
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
