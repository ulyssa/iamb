//! Window for Matrix rooms
use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::ops::Deref;
use std::path::{Path, PathBuf};

use edit::edit_with_builder as external_edit;
use edit::Builder;
use modalkit::editing::store::RegisterError;
use std::process::Command;
use tokio;
use url::Url;

use matrix_sdk::{
    attachment::AttachmentConfig,
    media::{MediaFormat, MediaRequestParameters},
    room::Room as MatrixRoom,
    ruma::{
        events::reaction::ReactionEventContent,
        events::relation::{Annotation, Replacement},
        events::room::message::{
            AddMentions,
            ForwardThread,
            MessageType,
            OriginalRoomMessageEvent,
            Relation,
            ReplyWithinThread,
            RoomMessageEventContent,
            TextMessageEventContent,
        },
        OwnedEventId,
        OwnedRoomId,
        RoomId,
    },
    RoomState,
};

use ratatui::{
    buffer::Buffer,
    layout::Rect,
    text::{Line, Span},
    widgets::{Paragraph, StatefulWidget, Widget},
};

use modalkit::keybindings::dialog::{MultiChoice, MultiChoiceItem, PromptYesNo};

use modalkit_ratatui::{
    textbox::{TextBox, TextBoxState},
    PromptActions,
    TerminalCursor,
    WindowOps,
};

use modalkit::actions::{
    Action,
    Editable,
    EditorAction,
    Jumpable,
    PromptAction,
    Promptable,
    Scrollable,
};
use modalkit::editing::{
    completion::CompletionList,
    context::Resolve,
    history::{self, HistoryList},
    rope::EditRope,
};
use modalkit::errors::{EditError, EditResult, UIError};
use modalkit::prelude::*;

use crate::base::{
    DownloadFlags,
    IambAction,
    IambBufferId,
    IambError,
    IambInfo,
    IambResult,
    MessageAction,
    Need,
    ProgramAction,
    ProgramContext,
    ProgramStore,
    RoomFocus,
    RoomInfo,
    SendAction,
};

use crate::message::{text_to_message, Message, MessageEvent, MessageKey, MessageTimeStamp};
use crate::worker::Requester;

use super::scrollback::{Scrollback, ScrollbackState};

/// State needed for rendering [Chat].
pub struct ChatState {
    room_id: OwnedRoomId,
    room: MatrixRoom,

    tbox: TextBoxState<IambInfo>,
    sent: HistoryList<EditRope>,
    sent_scrollback: history::ScrollbackState,

    scrollback: ScrollbackState,
    focus: RoomFocus,

    reply_to: Option<MessageKey>,
    editing: Option<MessageKey>,
}

impl ChatState {
    pub fn new(room: MatrixRoom, thread: Option<OwnedEventId>, store: &mut ProgramStore) -> Self {
        let room_id = room.room_id().to_owned();
        let scrollback = ScrollbackState::new(room_id.clone(), thread.clone());
        let id = IambBufferId::Room(room_id.clone(), thread, RoomFocus::MessageBar);
        let ebuf = store.load_buffer(id);
        let tbox = TextBoxState::new(ebuf);

        ChatState {
            room_id,
            room,

            tbox,
            sent: HistoryList::new(EditRope::from(""), 100),
            sent_scrollback: history::ScrollbackState::Pending,

            scrollback,
            focus: RoomFocus::MessageBar,

            reply_to: None,
            editing: None,
        }
    }

    pub fn thread(&self) -> Option<&OwnedEventId> {
        self.scrollback.thread()
    }

    fn get_joined(&self, worker: &Requester) -> Result<MatrixRoom, IambError> {
        let Some(room) = worker.client.get_room(self.id()) else {
            return Err(IambError::NotJoined);
        };

        if room.state() == RoomState::Joined {
            Ok(room)
        } else {
            Err(IambError::NotJoined)
        }
    }

    fn get_reply_to<'a>(&self, info: &'a RoomInfo) -> Option<&'a OriginalRoomMessageEvent> {
        let thread = self.scrollback.get_thread(info)?;
        let key = self.reply_to.as_ref()?;
        let msg = thread.get(key)?;

        if let MessageEvent::Original(ev) = &msg.event {
            Some(ev)
        } else {
            None
        }
    }

    fn reset(&mut self) -> EditRope {
        self.reply_to = None;
        self.editing = None;
        self.tbox.reset()
    }

    pub fn refresh_room(&mut self, store: &mut ProgramStore) {
        if let Some(room) = store.application.worker.client.get_room(self.id()) {
            self.room = room;
        }
    }

    pub async fn message_command(
        &mut self,
        act: MessageAction,
        _: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        let client = &store.application.worker.client;

        let settings = &store.application.settings;
        let info = store.application.rooms.get_or_default(self.room_id.clone());

        let msg = self.scrollback.get_mut(info).ok_or(IambError::NoSelectedMessage)?;

        match act {
            MessageAction::Cancel(skip_confirm) => {
                if skip_confirm {
                    self.reset();

                    return Ok(None);
                }

                self.reply_to = None;
                self.editing = None;

                let msg = "Would you like to clear the message bar?";
                let act = PromptAction::Abort(false);
                let prompt = PromptYesNo::new(msg, vec![Action::from(act)]);
                let prompt = Box::new(prompt);

                Err(UIError::NeedConfirm(prompt))
            },
            MessageAction::Download(filename, flags) => {
                if let MessageEvent::Original(ev) = &msg.event {
                    let media = client.media();

                    let mut filename = match (filename, &settings.dirs.downloads) {
                        (Some(f), _) => PathBuf::from(f),
                        (None, Some(downloads)) => downloads.clone(),
                        (None, None) => return Err(IambError::NoDownloadDir.into()),
                    };

                    let (source, msg_filename) = match &ev.content.msgtype {
                        MessageType::Audio(c) => (c.source.clone(), c.body.as_str()),
                        MessageType::File(c) => {
                            (c.source.clone(), c.filename.as_deref().unwrap_or(c.body.as_str()))
                        },
                        MessageType::Image(c) => (c.source.clone(), c.body.as_str()),
                        MessageType::Video(c) => (c.source.clone(), c.body.as_str()),
                        _ => {
                            if !flags.contains(DownloadFlags::OPEN) {
                                return Err(IambError::NoAttachment.into());
                            }

                            let links = if let Some(html) = &msg.html {
                                html.get_links()
                            } else if let Ok(url) = Url::parse(&msg.event.body()) {
                                vec![('0', url)]
                            } else {
                                vec![]
                            };

                            if links.is_empty() {
                                return Err(IambError::NoAttachment.into());
                            }

                            let choices = links
                                .into_iter()
                                .map(|l| {
                                    let url = l.1.to_string();
                                    let act = IambAction::OpenLink(url.clone()).into();
                                    MultiChoiceItem::new(l.0, url, vec![act])
                                })
                                .collect();
                            let dialog = MultiChoice::new(choices);
                            let err = UIError::NeedConfirm(Box::new(dialog));

                            return Err(err);
                        },
                    };

                    if filename.is_dir() {
                        filename.push(msg_filename);
                    }

                    if filename.exists() && !flags.contains(DownloadFlags::FORCE) {
                        // Find an incrementally suffixed filename, e.g. image-2.jpg -> image-3.jpg
                        if let Some(stem) = filename.file_stem().and_then(OsStr::to_str) {
                            let ext = filename.extension();
                            let mut filename_incr = filename.clone();
                            for n in 1..=1000 {
                                if let Some(ext) = ext.and_then(OsStr::to_str) {
                                    filename_incr.set_file_name(format!("{}-{}.{}", stem, n, ext));
                                } else {
                                    filename_incr.set_file_name(format!("{}-{}", stem, n));
                                }

                                if !filename_incr.exists() {
                                    filename = filename_incr;
                                    break;
                                }
                            }
                        }
                    }

                    if !filename.exists() || flags.contains(DownloadFlags::FORCE) {
                        let req = MediaRequestParameters { source, format: MediaFormat::File };

                        let bytes =
                            media.get_media_content(&req, true).await.map_err(IambError::from)?;

                        fs::write(filename.as_path(), bytes.as_slice())?;

                        msg.downloaded = true;
                    } else if !flags.contains(DownloadFlags::OPEN) {
                        let msg = format!(
                            "The file {} already exists; add ! to end of command to overwrite it.",
                            filename.display()
                        );
                        let err = UIError::Failure(msg);

                        return Err(err);
                    }

                    let info = if flags.contains(DownloadFlags::OPEN) {
                        let target = filename.clone().into_os_string();
                        match open_command(
                            store.application.settings.tunables.open_command.as_ref(),
                            target,
                        ) {
                            Ok(_) => {
                                InfoMessage::from(format!(
                                    "Attachment downloaded to {} and opened",
                                    filename.display()
                                ))
                            },
                            Err(err) => {
                                return Err(err);
                            },
                        }
                    } else {
                        InfoMessage::from(format!(
                            "Attachment downloaded to {}",
                            filename.display()
                        ))
                    };

                    return Ok(info.into());
                }

                Err(IambError::NoAttachment.into())
            },
            MessageAction::Edit => {
                if msg.sender != settings.profile.user_id {
                    let msg = "Cannot edit messages sent by someone else";
                    let err = UIError::Failure(msg.into());

                    return Err(err);
                }

                let ev = match &msg.event {
                    MessageEvent::Original(ev) => &ev.content,
                    MessageEvent::Local(_, ev) => ev.deref(),
                    _ => {
                        let msg = "Cannot edit a redacted message";
                        let err = UIError::Failure(msg.into());

                        return Err(err);
                    },
                };

                let text = match &ev.msgtype {
                    MessageType::Text(msg) => msg.body.as_str(),
                    _ => {
                        let msg = "Cannot edit a non-text message";
                        let err = UIError::Failure(msg.into());

                        return Err(err);
                    },
                };

                self.tbox.set_text(text);
                self.reply_to = msg.reply_to().and_then(|id| info.get_message_key(&id)).cloned();
                self.editing = self.scrollback.get_key(info);
                self.focus = RoomFocus::MessageBar;

                Ok(None)
            },
            MessageAction::React(reaction, literal) => {
                let emoji = if literal {
                    reaction
                } else if let Some(emoji) =
                    emojis::get(&reaction).or_else(|| emojis::get_by_shortcode(&reaction))
                {
                    emoji.to_string()
                } else {
                    let msg = format!("{reaction:?} is not a known Emoji shortcode; do you want to react with exactly {reaction:?}?");
                    let act = IambAction::Message(MessageAction::React(reaction, true));
                    let prompt = PromptYesNo::new(msg, vec![Action::from(act)]);
                    let prompt = Box::new(prompt);

                    return Err(UIError::NeedConfirm(prompt));
                };

                let room = self.get_joined(&store.application.worker)?;
                let event_id = match &msg.event {
                    MessageEvent::EncryptedOriginal(ev) => ev.event_id.clone(),
                    MessageEvent::EncryptedRedacted(ev) => ev.event_id.clone(),
                    MessageEvent::Original(ev) => ev.event_id.clone(),
                    MessageEvent::Local(event_id, _) => event_id.clone(),
                    MessageEvent::State(ev) => ev.event_id().to_owned(),
                    MessageEvent::Redacted(_) => {
                        let msg = "Cannot react to a redacted message";
                        let err = UIError::Failure(msg.into());

                        return Err(err);
                    },
                };

                if info.user_reactions_contains(&settings.profile.user_id, &event_id, &emoji) {
                    let msg = format!("You’ve already reacted to this message with {}", emoji);
                    let err = UIError::Failure(msg);

                    return Err(err);
                }

                let reaction = Annotation::new(event_id, emoji);
                let msg = ReactionEventContent::new(reaction);
                let _ = room.send(msg).await.map_err(IambError::from)?;

                Ok(None)
            },
            MessageAction::Redact(reason, skip_confirm) => {
                if !skip_confirm {
                    let msg = "Are you sure you want to redact this message?";
                    let act = IambAction::Message(MessageAction::Redact(reason, true));
                    let prompt = PromptYesNo::new(msg, vec![Action::from(act)]);
                    let prompt = Box::new(prompt);

                    return Err(UIError::NeedConfirm(prompt));
                }

                let room = self.get_joined(&store.application.worker)?;
                let event_id = match &msg.event {
                    MessageEvent::EncryptedOriginal(ev) => ev.event_id.clone(),
                    MessageEvent::EncryptedRedacted(ev) => ev.event_id.clone(),
                    MessageEvent::Original(ev) => ev.event_id.clone(),
                    MessageEvent::Local(event_id, _) => event_id.clone(),
                    MessageEvent::State(ev) => ev.event_id().to_owned(),
                    MessageEvent::Redacted(_) => {
                        let msg = "Cannot redact already redacted message";
                        let err = UIError::Failure(msg.into());

                        return Err(err);
                    },
                };

                let event_id = event_id.as_ref();
                let reason = reason.as_deref();
                let _ = room.redact(event_id, reason, None).await.map_err(IambError::from)?;

                Ok(None)
            },
            MessageAction::Reply => {
                self.reply_to = self.scrollback.get_key(info);
                self.focus = RoomFocus::MessageBar;

                Ok(None)
            },
            MessageAction::Replied => {
                let Some(reply) = msg.reply_to() else {
                    let msg = "Selected message is not a reply";
                    return Err(UIError::Failure(msg.into()));
                };

                let Some(key) = info.get_message_key(&reply) else {
                    store.application.need_load.insert(self.room_id.clone(), Need::MESSAGES);
                    let msg = "Replied to message not loaded";
                    return Err(UIError::Failure(msg.into()));
                };

                self.scrollback.goto_message(key.clone());
                Ok(None)
            },
            MessageAction::Unreact(reaction, literal) => {
                let emoji = match reaction {
                    reaction if literal => reaction,
                    Some(reaction) => {
                        if let Some(emoji) =
                            emojis::get(&reaction).or_else(|| emojis::get_by_shortcode(&reaction))
                        {
                            Some(emoji.to_string())
                        } else {
                            let msg = format!("{reaction:?} is not a known Emoji shortcode; do you want to remove exactly {reaction:?}?");
                            let act =
                                IambAction::Message(MessageAction::Unreact(Some(reaction), true));
                            let prompt = PromptYesNo::new(msg, vec![Action::from(act)]);
                            let prompt = Box::new(prompt);

                            return Err(UIError::NeedConfirm(prompt));
                        }
                    },
                    None => None,
                };

                let room = self.get_joined(&store.application.worker)?;
                let event_id = match &msg.event {
                    MessageEvent::EncryptedOriginal(ev) => ev.event_id.clone(),
                    MessageEvent::EncryptedRedacted(ev) => ev.event_id.clone(),
                    MessageEvent::Original(ev) => ev.event_id.clone(),
                    MessageEvent::Local(event_id, _) => event_id.clone(),
                    MessageEvent::State(ev) => ev.event_id().to_owned(),
                    MessageEvent::Redacted(_) => {
                        let msg = "Cannot unreact to a redacted message";
                        let err = UIError::Failure(msg.into());

                        return Err(err);
                    },
                };

                let reactions = match info.reactions.get(&event_id) {
                    Some(r) => r,
                    None => return Ok(None),
                };

                let reactions = reactions.iter().filter_map(|(event_id, (reaction, user_id))| {
                    if user_id != &settings.profile.user_id {
                        return None;
                    }

                    if let Some(emoji) = &emoji {
                        if emoji == reaction {
                            return Some(event_id);
                        } else {
                            return None;
                        }
                    } else {
                        return Some(event_id);
                    }
                });

                for reaction in reactions {
                    let _ = room.redact(reaction, None, None).await.map_err(IambError::from)?;
                }

                Ok(None)
            },
        }
    }

    pub async fn send_command(
        &mut self,
        act: SendAction,
        _: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        let room = self.get_joined(&store.application.worker)?;
        let info = store.application.rooms.get_or_default(self.id().to_owned());
        let mut show_echo = true;

        let (event_id, msg) = match act {
            SendAction::Submit | SendAction::SubmitFromEditor => {
                let msg = self.tbox.get();

                let msg = if let SendAction::SubmitFromEditor = act {
                    let suffix =
                        store.application.settings.tunables.external_edit_file_suffix.as_str();
                    let edited_msg =
                        external_edit(msg.trim_end().to_string(), Builder::new().suffix(suffix))?
                            .trim_end()
                            .to_string();
                    if edited_msg.is_empty() {
                        return Ok(None);
                    }
                    edited_msg
                } else if msg.is_blank() {
                    return Ok(None);
                } else {
                    msg.trim_end().to_string()
                };

                let mut msg = text_to_message(msg);

                if let Some((_, event_id)) = &self.editing {
                    msg.relates_to = Some(Relation::Replacement(Replacement::new(
                        event_id.clone(),
                        msg.msgtype.clone().into(),
                    )));

                    show_echo = false;
                } else if let Some(thread_root) = self.scrollback.thread() {
                    if let Some(m) = self.get_reply_to(info) {
                        msg = msg.make_for_thread(m, ReplyWithinThread::Yes, AddMentions::No);
                    } else if let Some(m) = info.get_thread_last(thread_root) {
                        msg = msg.make_for_thread(m, ReplyWithinThread::No, AddMentions::No);
                    } else {
                        // Internal state is wonky?
                    }
                } else if let Some(m) = self.get_reply_to(info) {
                    msg = msg.make_reply_to(m, ForwardThread::Yes, AddMentions::No);
                }

                // XXX: second parameter can be a locally unique transaction id.
                // Useful for doing retries.
                let resp = room.send(msg.clone()).await.map_err(IambError::from)?;
                let event_id = resp.event_id;

                // Reset message bar state now that it's been sent.
                self.reset();

                (event_id, msg)
            },
            SendAction::Upload(file) => {
                let path = Path::new(file.as_str());
                let mime = mime_guess::from_path(path).first_or(mime::APPLICATION_OCTET_STREAM);

                let bytes = fs::read(path)?;
                let name = path
                    .file_name()
                    .map(OsStr::to_string_lossy)
                    .unwrap_or_else(|| Cow::from("Attachment"));
                let config = AttachmentConfig::new();

                let resp = room
                    .send_attachment(name.as_ref(), &mime, bytes, config)
                    .await
                    .map_err(IambError::from)?;

                // Mock up the local echo message for the scrollback.
                let msg = TextMessageEventContent::plain(format!("[Attached File: {name}]"));
                let msg = MessageType::Text(msg);
                let msg = RoomMessageEventContent::new(msg);

                (resp.event_id, msg)
            },
            SendAction::UploadImage(width, height, bytes) => {
                // Convert to png because arboard does not give us the mime type.
                let bytes =
                    image::ImageBuffer::from_raw(width as _, height as _, bytes.into_owned())
                        .ok_or(IambError::Clipboard)
                        .and_then(|imagebuf| {
                            let dynimage = image::DynamicImage::ImageRgba8(imagebuf);
                            let bytes = Vec::<u8>::new();
                            let mut buff = std::io::Cursor::new(bytes);
                            dynimage.write_to(&mut buff, image::ImageFormat::Png)?;
                            Ok(buff.into_inner())
                        })
                        .map_err(IambError::from)?;
                let mime = mime::IMAGE_PNG;

                let name = "Clipboard.png";
                let config = AttachmentConfig::new();

                let resp = room
                    .send_attachment(name, &mime, bytes, config)
                    .await
                    .map_err(IambError::from)?;

                // Mock up the local echo message for the scrollback.
                let msg = TextMessageEventContent::plain(format!("[Attached File: {name}]"));
                let msg = MessageType::Text(msg);
                let msg = RoomMessageEventContent::new(msg);

                (resp.event_id, msg)
            },
        };

        if show_echo {
            let user = store.application.settings.profile.user_id.clone();
            let key = (MessageTimeStamp::LocalEcho, event_id.clone());
            let msg = MessageEvent::Local(event_id, msg.into());
            let msg = Message::new(msg, user, MessageTimeStamp::LocalEcho);
            let thread = self.scrollback.get_thread_mut(info);
            thread.insert(key, msg);
        }

        // Jump to the end of the scrollback to show the message.
        self.scrollback.goto_latest();

        Ok(None)
    }

    pub fn focus_toggle(&mut self) {
        self.focus.toggle();
    }

    pub fn room(&self) -> &MatrixRoom {
        &self.room
    }

    pub fn id(&self) -> &RoomId {
        &self.room_id
    }

    pub fn auto_toggle_focus(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
    ) -> Option<EditorAction> {
        auto_toggle_focus(&mut self.focus, act, ctx, &self.scrollback, &mut self.tbox)
    }

    pub fn typing_notice(
        &self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) {
        if !self.focus.is_msgbar() || act.is_readonly(ctx) {
            return;
        }

        if !store.application.settings.tunables.typing_notice_send {
            return;
        }

        store.application.worker.typing_notice(self.room_id.clone());
    }
}

macro_rules! delegate {
    ($s: expr, $id: ident => $e: expr) => {
        match $s.focus {
            RoomFocus::Scrollback => {
                match $s {
                    ChatState { scrollback: $id, .. } => $e,
                }
            },
            RoomFocus::MessageBar => {
                match $s {
                    ChatState { tbox: $id, .. } => $e,
                }
            },
        }
    };
}

impl WindowOps<IambInfo> for ChatState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        Chat::new(store).focus(focused).render(area, buf, self)
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        // XXX: I want each WindowSlot to have its own shared buffer, instead of each Room; need to
        // find a good way to pass that info here so that it can be part of the content id.
        let room_id = self.room_id.clone();
        let thread = self.thread().cloned();
        let id = IambBufferId::Room(room_id.clone(), thread, RoomFocus::MessageBar);
        let ebuf = store.load_buffer(id);
        let tbox = TextBoxState::new(ebuf);

        ChatState {
            room_id,
            room: self.room.clone(),

            tbox,
            sent: self.sent.clone(),
            sent_scrollback: history::ScrollbackState::Pending,

            scrollback: self.scrollback.dup(store),
            focus: self.focus,

            reply_to: None,
            editing: None,
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
        _: WriteFlags,
        _: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        // XXX: what's the right writing behaviour for a room?
        // Should write send a message?
        Ok(None)
    }

    fn get_completions(&self) -> Option<CompletionList> {
        delegate!(self, w => w.get_completions())
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        delegate!(self, w => w.get_cursor_word(style))
    }

    fn get_selected_word(&self) -> Option<String> {
        delegate!(self, w => w.get_selected_word())
    }
}

impl Editable<ProgramContext, ProgramStore, IambInfo> for ChatState {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        // Check whether we should automatically switch between the message bar
        // or message scrollback, and use an adjusted action if we do so.
        let adjusted = self.auto_toggle_focus(act, ctx);
        let act = adjusted.as_ref().unwrap_or(act);

        // Send typing notice if needed.
        self.typing_notice(act, ctx, store);

        // And now we can finally run the editor command.
        match delegate!(self, w => w.editor_command(act, ctx, store)) {
            res @ Ok(_) => res,
            Err(EditError::WrongBuffer(IambBufferId::Room(room_id, thread, focus)))
                if room_id == self.room_id &&
                    thread.as_ref() == self.thread() &&
                    act.is_switchable(ctx) =>
            {
                // Switch focus.
                self.focus = focus;

                // Run command again.
                delegate!(self, w => w.editor_command(act, ctx, store))
            },
            Err(EditError::Register(RegisterError::ClipboardImage(data))) => {
                let msg = "Do you really want to upload the image from your system clipboard?";
                let send =
                    IambAction::Send(SendAction::UploadImage(data.width, data.height, data.bytes));
                let prompt = PromptYesNo::new(msg, vec![Action::from(send)]);
                let prompt = Box::new(prompt);

                Err(EditError::NeedConfirm(prompt))
            },
            res @ Err(_) => res,
        }
    }
}

impl TerminalCursor for ChatState {
    fn get_term_cursor(&self) -> Option<(u16, u16)> {
        delegate!(self, w => w.get_term_cursor())
    }
}

impl Jumpable<ProgramContext, IambInfo> for ChatState {
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &ProgramContext,
    ) -> IambResult<usize> {
        delegate!(self, w => w.jump(list, dir, count, ctx))
    }
}

impl Scrollable<ProgramContext, ProgramStore, IambInfo> for ChatState {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<EditInfo, IambInfo> {
        // Send all scroll commands to the scrollback.
        //
        // If there's enough message text for scrolling to be necessary,
        // navigating with movement keys should be enough to do the job.
        self.scrollback.scroll(style, ctx, store)
    }
}

impl PromptActions<ProgramContext, ProgramStore, IambInfo> for ChatState {
    fn submit(
        &mut self,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        let act = SendAction::Submit;

        Ok(vec![(IambAction::from(act).into(), ctx.clone())])
    }

    fn abort(
        &mut self,
        empty: bool,
        _: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        let text = self.tbox.get();

        if empty && text.is_blank() {
            return Ok(vec![]);
        }

        let text = self.reset().trim();

        if text.is_empty() {
            let _ = self.sent.end();
        } else {
            self.sent.select(text);
        }

        return Ok(vec![]);
    }

    fn recall(
        &mut self,
        dir: &MoveDir1D,
        count: &Count,
        prefixed: bool,
        ctx: &ProgramContext,
        _: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        let count = ctx.resolve(count);
        let rope = self.tbox.get();

        let text = self.sent.recall(&rope, &mut self.sent_scrollback, *dir, prefixed, count);

        if let Some(text) = text {
            self.tbox.set_text(text);
        }

        Ok(vec![])
    }
}

impl Promptable<ProgramContext, ProgramStore, IambInfo> for ChatState {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &ProgramContext,
        store: &mut ProgramStore,
    ) -> EditResult<Vec<(ProgramAction, ProgramContext)>, IambInfo> {
        if let RoomFocus::Scrollback = self.focus {
            return self.scrollback.prompt(act, ctx, store);
        }

        match act {
            PromptAction::Submit => self.submit(ctx, store),
            PromptAction::Abort(empty) => self.abort(*empty, ctx, store),
            PromptAction::Recall(dir, count, prefixed) => {
                self.recall(dir, count, *prefixed, ctx, store)
            },
        }
    }
}

/// [StatefulWidget] for Matrix rooms.
pub struct Chat<'a> {
    store: &'a mut ProgramStore,
    focused: bool,
}

impl<'a> Chat<'a> {
    pub fn new(store: &'a mut ProgramStore) -> Chat<'a> {
        Chat { store, focused: false }
    }

    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl StatefulWidget for Chat<'_> {
    type State = ChatState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        // Determine whether we have a description to show for the message bar.
        let desc_spans = match (&state.editing, &state.reply_to, state.thread()) {
            (None, None, None) => None,
            (None, None, Some(_)) => Some(Line::from("Replying in thread")),
            (Some(_), None, None) => Some(Line::from("Editing message")),
            (Some(_), None, Some(_)) => Some(Line::from("Editing message in thread")),
            (editing, Some(_), thread) => {
                self.store.application.rooms.get(state.id()).and_then(|room| {
                    let msg = state.get_reply_to(room)?;
                    let user =
                        self.store.application.settings.get_user_span(msg.sender.as_ref(), room);
                    let prefix = match (editing.is_some(), thread.is_some()) {
                        (true, false) => Span::from("Editing reply to "),
                        (true, true) => Span::from("Editing reply in thread to "),
                        (false, false) => Span::from("Replying to "),
                        (false, true) => Span::from("Replying in thread to "),
                    };
                    let spans = Line::from(vec![prefix, user]);

                    spans.into()
                })
            },
        };

        // Determine the region to show each UI element.
        let lines = state.tbox.has_lines(5).max(1) as u16;
        let drawh = area.height;
        let texth = lines.min(drawh).clamp(1, 5);
        let desch = if desc_spans.is_some() {
            drawh.saturating_sub(texth).min(1)
        } else {
            0
        };
        let scrollh = drawh.saturating_sub(texth).saturating_sub(desch);

        let scrollarea = Rect::new(area.x, area.y, area.width, scrollh);
        let descarea = Rect::new(area.x, scrollarea.y + scrollh, area.width, desch);
        let textarea = Rect::new(area.x, descarea.y + desch, area.width, texth);

        // Render the message bar and any description for it.
        if let Some(desc_spans) = desc_spans {
            Paragraph::new(desc_spans).render(descarea, buf);
        }

        let prompt = if self.focused { "> " } else { "  " };

        let tbox = TextBox::new().prompt(prompt);
        tbox.render(textarea, buf, &mut state.tbox);

        // Render the message scrollback.
        let scrollback_focused = state.focus.is_scrollback() && self.focused;
        let scrollback = Scrollback::new(self.store)
            .focus(scrollback_focused)
            .room_focus(self.focused);
        scrollback.render(scrollarea, buf, &mut state.scrollback);
    }
}

fn open_command(open_command: Option<&Vec<String>>, target: OsString) -> IambResult<()> {
    if let Some(mut cmd) = open_command.and_then(cmd) {
        cmd.arg(target);
        cmd.spawn()?;
        return Ok(());
    } else {
        // open::that may not return until the spawned program closes.
        tokio::task::spawn_blocking(move || {
            return open::that(target);
        });
        return Ok(());
    }
}

fn cmd(open_command: &Vec<String>) -> Option<Command> {
    if let [program, args @ ..] = open_command.as_slice() {
        let mut cmd = Command::new(program);
        cmd.args(args);
        return Some(cmd);
    }
    None
}

pub fn auto_toggle_focus(
    focus: &mut RoomFocus,
    act: &EditorAction,
    ctx: &ProgramContext,
    scrollback: &ScrollbackState,
    tbox: &mut TextBoxState<IambInfo>,
) -> Option<EditorAction> {
    let is_insert = ctx.get_insert_style().is_some();

    match (focus, act) {
        (f @ RoomFocus::Scrollback, _) if is_insert => {
            // Insert mode commands should switch focus.
            f.toggle();
            None
        },
        (f @ RoomFocus::Scrollback, EditorAction::InsertText(_)) => {
            // Pasting or otherwise inserting text should switch.
            f.toggle();
            None
        },
        (
            f @ RoomFocus::Scrollback,
            EditorAction::Edit(
                op,
                EditTarget::Motion(mov @ MoveType::Line(MoveDir1D::Next), count),
            ),
        ) if ctx.resolve(op).is_motion() => {
            let count = ctx.resolve(count);

            if count > 0 && scrollback.is_latest() {
                // Trying to move down a line when already at the end of room history should
                // switch.
                f.toggle();

                // And decrement the count for the action.
                let count = count.saturating_sub(1).into();
                let target = EditTarget::Motion(mov.clone(), count);
                let dec = EditorAction::Edit(op.clone(), target);

                Some(dec)
            } else {
                None
            }
        },
        (
            f @ RoomFocus::MessageBar,
            EditorAction::Edit(
                op,
                EditTarget::Motion(mov @ MoveType::Line(MoveDir1D::Previous), count),
            ),
        ) if !is_insert && ctx.resolve(op).is_motion() => {
            let count = ctx.resolve(count);

            if count > 0 && tbox.get_cursor().y == 0 {
                // Trying to move up a line when already at the top of the msgbar should
                // switch as long as we're not in Insert mode.
                f.toggle();

                // And decrement the count for the action.
                let count = count.saturating_sub(1).into();
                let target = EditTarget::Motion(mov.clone(), count);
                let dec = EditorAction::Edit(op.clone(), target);

                Some(dec)
            } else {
                None
            }
        },
        (RoomFocus::Scrollback, _) | (RoomFocus::MessageBar, _) => {
            // Do not switch.
            None
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use modalkit::actions::{EditAction, InsertTextAction};

    use crate::tests::{mock_store, TEST_ROOM1_ID};

    macro_rules! move_line {
        ($dir: expr, $count: expr) => {
            EditorAction::Edit(
                EditAction::Motion.into(),
                EditTarget::Motion(MoveType::Line($dir), $count.into()),
            )
        };
    }

    #[tokio::test]
    async fn test_auto_focus() {
        let mut store = mock_store().await;
        let ctx = ProgramContext::default();

        let room_id = TEST_ROOM1_ID.clone();
        let scrollback = ScrollbackState::new(room_id.clone(), None);

        let id = IambBufferId::Room(room_id, None, RoomFocus::MessageBar);
        let ebuf = store.load_buffer(id);
        let mut tbox = TextBoxState::new(ebuf);

        // Start out focused on the scrollback.
        let mut focused = RoomFocus::Scrollback;

        // Inserting text toggles:
        let act = EditorAction::InsertText(InsertTextAction::Type(
            Char::from('a').into(),
            MoveDir1D::Next,
            1.into(),
        ));
        let res = auto_toggle_focus(&mut focused, &act, &ctx, &scrollback, &mut tbox);
        assert_eq!(focused, RoomFocus::MessageBar);
        assert!(res.is_none());

        // Going down in message bar doesn't toggle:
        let act = move_line!(MoveDir1D::Next, 1);
        let res = auto_toggle_focus(&mut focused, &act, &ctx, &scrollback, &mut tbox);
        assert_eq!(focused, RoomFocus::MessageBar);
        assert!(res.is_none());

        // But going up will:
        let act = move_line!(MoveDir1D::Previous, 1);
        let res = auto_toggle_focus(&mut focused, &act, &ctx, &scrollback, &mut tbox);
        assert_eq!(focused, RoomFocus::Scrollback);
        assert_eq!(res, Some(move_line!(MoveDir1D::Previous, 0)));

        // Going up in scrollback doesn't toggle:
        let act = move_line!(MoveDir1D::Previous, 1);
        let res = auto_toggle_focus(&mut focused, &act, &ctx, &scrollback, &mut tbox);
        assert_eq!(focused, RoomFocus::Scrollback);
        assert_eq!(res, None);

        // And then go back down:
        let act = move_line!(MoveDir1D::Next, 1);
        let res = auto_toggle_focus(&mut focused, &act, &ctx, &scrollback, &mut tbox);
        assert_eq!(focused, RoomFocus::MessageBar);
        assert_eq!(res, Some(move_line!(MoveDir1D::Next, 0)));

        // Go up 2 will go up 1 in scrollback:
        let act = move_line!(MoveDir1D::Previous, 2);
        let res = auto_toggle_focus(&mut focused, &act, &ctx, &scrollback, &mut tbox);
        assert_eq!(focused, RoomFocus::Scrollback);
        assert_eq!(res, Some(move_line!(MoveDir1D::Previous, 1)));

        // Go down 3 will go down 2 in messagebar:
        let act = move_line!(MoveDir1D::Next, 3);
        let res = auto_toggle_focus(&mut focused, &act, &ctx, &scrollback, &mut tbox);
        assert_eq!(focused, RoomFocus::MessageBar);
        assert_eq!(res, Some(move_line!(MoveDir1D::Next, 2)));
    }
}
