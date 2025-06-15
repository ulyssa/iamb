//! # iamb
//!
//! The iamb client loops over user input and commands, and turns them into actions, [some of
//! which][IambAction] are specific to iamb, and [some of which][Action] come from [modalkit]. When
//! adding new functionality, you will usually want to extend [IambAction] or one of its variants
//! (like [RoomAction][base::RoomAction]), and then add an appropriate [command][commands] or
//! [keybinding][keybindings].
//!
//! For more complicated changes, you may need to update [the async worker thread][worker], which
//! handles background Matrix tasks with [matrix-rust-sdk][matrix_sdk].
//!
//! Most rendering logic lives under the [windows] module, but [Matrix messages][message] have
//! their own module.
#![allow(clippy::manual_range_contains)]
#![allow(clippy::needless_return)]
#![allow(clippy::result_large_err)]
#![allow(clippy::bool_assert_comparison)]
use std::collections::VecDeque;
use std::convert::TryFrom;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::{stdout, BufWriter, Stdout, Write};
use std::ops::DerefMut;
use std::process;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use clap::Parser;
use matrix_sdk::crypto::encrypt_room_key_export;
use matrix_sdk::ruma::api::client::error::ErrorKind;
use matrix_sdk::ruma::OwnedUserId;
use modalkit::keybindings::InputBindings;
use rand::{distributions::Alphanumeric, Rng};
use temp_dir::TempDir;
use tokio::sync::Mutex as AsyncMutex;
use tracing_subscriber::FmtSubscriber;

use modalkit::crossterm::{
    self,
    cursor::Show as CursorShow,
    event::{
        poll,
        read,
        DisableBracketedPaste,
        DisableFocusChange,
        DisableMouseCapture,
        EnableBracketedPaste,
        EnableFocusChange,
        EnableMouseCapture,
        Event,
        KeyEventKind,
        KeyboardEnhancementFlags,
        MouseEventKind,
        PopKeyboardEnhancementFlags,
        PushKeyboardEnhancementFlags,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, SetTitle},
};

use ratatui::{
    backend::CrosstermBackend,
    layout::Rect,
    style::{Color, Style},
    text::Span,
    widgets::Paragraph,
    Terminal,
};

mod base;
mod commands;
mod config;
mod keybindings;
mod message;
mod notifications;
mod preview;
mod sled_export;
mod util;
mod windows;
mod worker;

#[cfg(test)]
mod tests;

use crate::{
    base::{
        AsyncProgramStore,
        ChatStore,
        HomeserverAction,
        IambAction,
        IambError,
        IambId,
        IambInfo,
        IambResult,
        KeysAction,
        ProgramAction,
        ProgramContext,
        ProgramStore,
    },
    config::{ApplicationSettings, Iamb},
    windows::IambWindow,
    worker::{create_room, ClientWorker, LoginStyle, Requester},
};

use modalkit::{
    actions::{
        Action,
        Commandable,
        Editable,
        EditorAction,
        InsertTextAction,
        Jumpable,
        Promptable,
        Scrollable,
        TabAction,
        TabContainer,
        TabCount,
        WindowAction,
        WindowContainer,
    },
    editing::{context::Resolve, key::KeyManager, store::Store},
    errors::{EditError, UIError},
    key::TerminalKey,
    keybindings::{
        dialog::{Pager, PromptYesNo},
        BindingMachine,
    },
    prelude::*,
    ui::FocusList,
};

use modalkit_ratatui::{
    cmdbar::CommandBarState,
    screen::{Screen, ScreenState, TabbedLayoutDescription},
    windows::{WindowLayoutDescription, WindowLayoutState},
    TerminalCursor,
    TerminalExtOps,
    Window,
};

fn config_tab_to_desc(
    layout: config::WindowLayout,
    store: &mut ProgramStore,
) -> IambResult<WindowLayoutDescription<IambInfo>> {
    let desc = match layout {
        config::WindowLayout::Window { window } => {
            let ChatStore { names, worker, .. } = &mut store.application;

            let window = match window {
                config::WindowPath::UserId(user_id) => {
                    let name = user_id.to_string();
                    let room_id = worker.join_room(name.clone())?;
                    names.insert(name, room_id.clone());
                    IambId::Room(room_id, None)
                },
                config::WindowPath::RoomId(room_id) => IambId::Room(room_id, None),
                config::WindowPath::AliasId(alias) => {
                    let name = alias.to_string();
                    let room_id = worker.join_room(name.clone())?;
                    names.insert(name, room_id.clone());
                    IambId::Room(room_id, None)
                },
                config::WindowPath::Window(id) => id,
            };

            WindowLayoutDescription::Window { window, length: None }
        },
        config::WindowLayout::Split { split } => {
            let children = split
                .into_iter()
                .map(|child| config_tab_to_desc(child, store))
                .collect::<IambResult<Vec<_>>>()?;

            WindowLayoutDescription::Split { children, length: None }
        },
    };

    Ok(desc)
}

fn restore_layout(
    area: Rect,
    settings: &ApplicationSettings,
    store: &mut ProgramStore,
) -> IambResult<FocusList<WindowLayoutState<IambWindow, IambInfo>>> {
    let layout = std::fs::read(&settings.layout_json)?;
    let tabs: TabbedLayoutDescription<IambInfo> =
        serde_json::from_slice(&layout).map_err(IambError::from)?;
    tabs.to_layout(area.into(), store)
}

fn setup_screen(
    settings: ApplicationSettings,
    store: &mut ProgramStore,
) -> IambResult<ScreenState<IambWindow, IambInfo>> {
    let cmd = CommandBarState::new(store);
    let dims = crossterm::terminal::size()?;
    let area = Rect::new(0, 0, dims.0, dims.1);

    match settings.layout {
        config::Layout::Restore => {
            match restore_layout(area, &settings, store) {
                Ok(tabs) => {
                    return Ok(ScreenState::from_list(tabs, cmd));
                },
                Err(e) => {
                    // Log the issue with restoring and then continue.
                    tracing::warn!(err = %e, "Failed to restore layout from disk");
                },
            }
        },
        config::Layout::New => {},
        config::Layout::Config { tabs } => {
            let mut list = FocusList::default();

            for tab in tabs.into_iter() {
                let tab = config_tab_to_desc(tab, store)?;
                let tab = tab.to_layout(area.into(), store)?;
                list.push(tab);
            }

            return Ok(ScreenState::from_list(list, cmd));
        },
    }

    let win = settings
        .tunables
        .default_room
        .and_then(|room| IambWindow::find(room, store).ok())
        .or_else(|| IambWindow::open(IambId::Welcome, store).ok())
        .unwrap();

    return Ok(ScreenState::new(win, cmd));
}

/// The main application state and event loop.
struct Application {
    /// Terminal backend.
    terminal: Terminal<CrosstermBackend<Stdout>>,

    /// State for the Matrix client, editing, etc.
    store: AsyncProgramStore,

    /// UI state (open tabs, command bar, etc.) to use when rendering.
    screen: ScreenState<IambWindow, IambInfo>,

    /// Handle to communicate synchronously with the Matrix worker task.
    worker: Requester,

    /// Mapped keybindings.
    bindings: KeyManager<TerminalKey, ProgramAction, RepeatType>,

    /// Pending actions to run.
    actstack: VecDeque<(ProgramAction, ProgramContext)>,

    /// Whether or not the terminal is currently focused.
    focused: bool,

    /// The tab layout before the last executed [TabAction].
    last_layout: Option<TabbedLayoutDescription<IambInfo>>,

    /// Whether we need to do a full redraw (e.g., after running a subprocess).
    dirty: bool,
}

impl Application {
    pub async fn new(
        settings: ApplicationSettings,
        store: AsyncProgramStore,
    ) -> IambResult<Application> {
        let backend = CrosstermBackend::new(stdout());
        let terminal = Terminal::new(backend)?;

        let mut bindings = crate::keybindings::setup_keybindings();
        settings.setup(&mut bindings);
        let bindings = KeyManager::new(bindings);

        let mut locked = store.lock().await;
        let screen = setup_screen(settings, locked.deref_mut())?;

        let worker = locked.application.worker.clone();

        drop(locked);

        let actstack = VecDeque::new();

        Ok(Application {
            store,
            worker,
            terminal,
            bindings,
            actstack,
            screen,
            focused: true,
            last_layout: None,
            dirty: true,
        })
    }

    fn redraw(&mut self, full: bool, store: &mut ProgramStore) -> Result<(), std::io::Error> {
        let bindings = &mut self.bindings;
        let focused = self.focused;
        let sstate = &mut self.screen;
        let term = &mut self.terminal;

        if store.application.ring_bell {
            store.application.ring_bell = term.backend_mut().write_all(&[7]).is_err();
        }

        if full {
            term.clear()?;
        }

        term.draw(|f| {
            let area = f.area();

            let modestr = bindings.show_mode();
            let cursor = bindings.get_cursor_indicator();
            let dialogstr = bindings.show_dialog(area.height as usize, area.width as usize);

            // Don't show terminal cursor when we show a dialog.
            let hide_cursor = !dialogstr.is_empty();

            store.application.draw_curr = Some(Instant::now());
            let screen = Screen::new(store)
                .show_dialog(dialogstr)
                .show_mode(modestr)
                .borders(true)
                .focus(focused);
            f.render_stateful_widget(screen, area, sstate);

            if hide_cursor {
                return;
            }

            if let Some((cx, cy)) = sstate.get_term_cursor() {
                if let Some(c) = cursor {
                    let style = Style::default().fg(Color::Green);
                    let span = Span::styled(c.to_string(), style);
                    let para = Paragraph::new(span);
                    let inner = Rect::new(cx, cy, 1, 1);
                    f.render_widget(para, inner)
                }
                f.set_cursor_position((cx, cy));
            }
        })?;

        Ok(())
    }

    async fn step(&mut self) -> Result<TerminalKey, std::io::Error> {
        loop {
            self.redraw(self.dirty, self.store.clone().lock().await.deref_mut())?;
            self.dirty = false;

            if !poll(Duration::from_secs(1))? {
                // Redraw in case there's new messages to show.
                continue;
            }

            match read()? {
                Event::Key(ke) => {
                    if ke.kind == KeyEventKind::Release {
                        continue;
                    }

                    return Ok(ke.into());
                },
                Event::Mouse(me) => {
                    let dir = match me.kind {
                        MouseEventKind::ScrollUp => MoveDir2D::Up,
                        MouseEventKind::ScrollDown => MoveDir2D::Down,
                        MouseEventKind::ScrollLeft => MoveDir2D::Left,
                        MouseEventKind::ScrollRight => MoveDir2D::Right,
                        _ => continue,
                    };

                    let size = ScrollSize::Cell;
                    let style = ScrollStyle::Direction2D(dir, size, 1.into());
                    let ctx = ProgramContext::default();
                    let mut store = self.store.lock().await;

                    match self.screen.scroll(&style, &ctx, store.deref_mut()) {
                        Ok(None) => {},
                        Ok(Some(info)) => {
                            drop(store);
                            self.handle_info(info);
                        },
                        Err(e) => {
                            self.screen.push_error(e);
                        },
                    }
                },
                Event::FocusGained => {
                    let mut store = self.store.lock().await;
                    store.application.focused = true;
                    self.focused = true;
                },
                Event::FocusLost => {
                    let mut store = self.store.lock().await;
                    store.application.focused = false;
                    self.focused = false;
                },
                Event::Resize(_, _) => {
                    // We'll redraw for the new size next time step() is called.
                },
                Event::Paste(s) => {
                    let act = InsertTextAction::Transcribe(s, MoveDir1D::Previous, 1.into());
                    let act = EditorAction::from(act);
                    let ctx = ProgramContext::default();
                    let mut store = self.store.lock().await;

                    match self.screen.editor_command(&act, &ctx, store.deref_mut()) {
                        Ok(None) => {},
                        Ok(Some(info)) => {
                            drop(store);
                            self.handle_info(info);
                        },
                        Err(e) => {
                            self.screen.push_error(e);
                        },
                    }
                },
            }
        }
    }

    fn action_prepend(&mut self, acts: Vec<(ProgramAction, ProgramContext)>) {
        let mut acts = VecDeque::from(acts);
        acts.append(&mut self.actstack);
        self.actstack = acts;
    }

    fn action_pop(&mut self, keyskip: bool) -> Option<(ProgramAction, ProgramContext)> {
        if let res @ Some(_) = self.actstack.pop_front() {
            return res;
        }

        if keyskip {
            return None;
        } else {
            return self.bindings.pop();
        }
    }

    async fn action_run(
        &mut self,
        action: ProgramAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        let info = match action {
            // Do nothing.
            Action::NoOp => None,

            Action::Editor(act) => {
                match self.screen.editor_command(&act, &ctx, store) {
                    Ok(info) => info,
                    Err(EditError::WrongBuffer(content)) if act.is_switchable(&ctx) => {
                        // Switch to the right window.
                        if let Some(winid) = content.to_window() {
                            let open = OpenTarget::Application(winid);
                            let open = WindowAction::Switch(open);
                            let _ = self.screen.window_command(&open, &ctx, store)?;

                            // Run command again.
                            self.screen.editor_command(&act, &ctx, store)?
                        } else {
                            return Err(EditError::WrongBuffer(content).into());
                        }
                    },
                    Err(err) => return Err(err.into()),
                }
            },

            // Simple delegations.
            Action::Application(act) => self.iamb_run(act, ctx, store).await?,
            Action::CommandBar(act) => self.screen.command_bar(&act, &ctx)?,
            Action::Macro(act) => self.bindings.macro_command(&act, &ctx, store)?,
            Action::Scroll(style) => self.screen.scroll(&style, &ctx, store)?,
            Action::ShowInfoMessage(info) => Some(info),
            Action::Window(cmd) => self.screen.window_command(&cmd, &ctx, store)?,

            Action::Jump(l, dir, count) => {
                let count = ctx.resolve(&count);
                let _ = self.screen.jump(l, dir, count, &ctx)?;

                None
            },
            Action::Suspend => {
                self.terminal.program_suspend()?;

                None
            },

            // UI actions.
            Action::Tab(cmd) => {
                if let TabAction::Close(_, _) = &cmd {
                    self.last_layout = self.screen.as_description().into();
                }

                self.screen.tab_command(&cmd, &ctx, store)?
            },
            Action::RedrawScreen => {
                self.screen.clear_message();
                self.redraw(true, store)?;

                None
            },

            // Actions that create more Actions.
            Action::Prompt(act) => {
                let acts = self.screen.prompt(&act, &ctx, store)?;
                self.action_prepend(acts);

                None
            },
            Action::Command(act) => {
                let acts = store.application.cmds.command(&act, &ctx, &mut store.registers)?;
                self.action_prepend(acts);

                None
            },
            Action::Repeat(rt) => {
                self.bindings.repeat(rt, Some(ctx));

                None
            },

            // Unimplemented.
            Action::KeywordLookup => {
                // XXX: implement
                None
            },

            _ => {
                // XXX: log unhandled actions? print message?
                None
            },
        };

        return Ok(info);
    }

    async fn iamb_run(
        &mut self,
        action: IambAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        if action.scribbles() {
            self.dirty = true;
        }

        let info = match action {
            IambAction::ClearUnreads => {
                let user_id = &store.application.settings.profile.user_id;

                for room_id in store.application.sync_info.chats() {
                    if let Some(room) = store.application.rooms.get_mut(room_id) {
                        room.fully_read(user_id);
                    }
                }

                None
            },

            IambAction::ToggleScrollbackFocus => {
                self.screen.current_window_mut()?.focus_toggle();

                None
            },

            IambAction::Homeserver(act) => {
                let acts = self.homeserver_command(act, ctx, store).await?;
                self.action_prepend(acts);

                None
            },
            IambAction::Keys(act) => self.keys_command(act, ctx, store).await?,
            IambAction::Message(act) => {
                self.screen.current_window_mut()?.message_command(act, ctx, store).await?
            },
            IambAction::Space(act) => {
                self.screen.current_window_mut()?.space_command(act, ctx, store).await?
            },
            IambAction::Room(act) => {
                let acts = self.screen.current_window_mut()?.room_command(act, ctx, store).await?;
                self.action_prepend(acts);

                None
            },
            IambAction::Send(act) => {
                self.bindings.reset_mode();
                self.screen.current_window_mut()?.send_command(act, ctx, store).await?
            },

            IambAction::OpenLink(url) => {
                tokio::task::spawn_blocking(move || {
                    return open::that(url);
                });

                None
            },

            IambAction::Verify(act, user_dev) => {
                if let Some(sas) = store.application.verifications.get(&user_dev) {
                    self.worker.verify(act, sas.clone())?
                } else {
                    return Err(IambError::InvalidVerificationId(user_dev).into());
                }
            },
            IambAction::VerifyRequest(user_id) => {
                if let Ok(user_id) = OwnedUserId::try_from(user_id.as_str()) {
                    self.worker.verify_request(user_id)?
                } else {
                    return Err(IambError::InvalidUserId(user_id).into());
                }
            },
        };

        Ok(info)
    }

    async fn homeserver_command(
        &mut self,
        action: HomeserverAction,
        ctx: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<Vec<(Action<IambInfo>, ProgramContext)>> {
        match action {
            HomeserverAction::CreateRoom(alias, vis, flags) => {
                let client = &store.application.worker.client;
                let room_id = create_room(client, alias, vis, flags).await?;
                let room = IambId::Room(room_id, None);
                let target = OpenTarget::Application(room);
                let action = WindowAction::Switch(target);

                Ok(vec![(action.into(), ctx)])
            },
            HomeserverAction::Logout(user, true) => {
                self.worker.logout(user)?;
                let flags = CloseFlags::QUIT | CloseFlags::FORCE;
                let act = TabAction::Close(TabTarget::All, flags);

                Ok(vec![(act.into(), ctx)])
            },
            HomeserverAction::Logout(user, false) => {
                let msg = "Would you like to logout?";
                let act = IambAction::from(HomeserverAction::Logout(user, true));
                let prompt = PromptYesNo::new(msg, vec![Action::from(act)]);
                let prompt = Box::new(prompt);

                Err(UIError::NeedConfirm(prompt))
            },
        }
    }

    async fn keys_command(
        &mut self,
        action: KeysAction,
        _: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        let encryption = store.application.worker.client.encryption();

        match action {
            KeysAction::Export(path, passphrase) => {
                encryption
                    .export_room_keys(path.into(), &passphrase, |_| true)
                    .await
                    .map_err(IambError::from)?;

                Ok(Some("Successfully exported room keys".into()))
            },
            KeysAction::Import(path, passphrase) => {
                let res = encryption
                    .import_room_keys(path.into(), &passphrase)
                    .await
                    .map_err(IambError::from)?;

                let msg = format!("Imported {} of {} keys", res.imported_count, res.total_count);

                Ok(Some(msg.into()))
            },
        }
    }

    fn handle_info(&mut self, info: InfoMessage) {
        match info {
            InfoMessage::Message(info) => {
                self.screen.push_info(info);
            },
            InfoMessage::Pager(text) => {
                let pager = Box::new(Pager::new(text, vec![]));
                self.bindings.run_dialog(pager);
            },
        }
    }

    pub async fn run(&mut self) -> Result<(), std::io::Error> {
        self.terminal.clear()?;

        let store = self.store.clone();

        while self.screen.tabs() != 0 {
            let key = self.step().await?;

            self.bindings.input_key(key);

            let mut locked = store.lock().await;
            let mut keyskip = false;

            while let Some((action, ctx)) = self.action_pop(keyskip) {
                match self.action_run(action, ctx, locked.deref_mut()).await {
                    Ok(None) => {
                        // Continue processing.
                        continue;
                    },
                    Ok(Some(info)) => {
                        self.handle_info(info);

                        // Continue processing; we'll redraw later.
                        continue;
                    },
                    Err(
                        UIError::NeedConfirm(dialog) |
                        UIError::EditingFailure(EditError::NeedConfirm(dialog)),
                    ) => {
                        self.bindings.run_dialog(dialog);
                        continue;
                    },
                    Err(e) => {
                        self.screen.push_error(e);

                        // Skip processing any more keypress Actions until the next key.
                        keyskip = true;
                        continue;
                    },
                }
            }
        }

        if let Some(ref layout) = self.last_layout {
            let locked = self.store.lock().await;
            let path = locked.application.settings.layout_json.as_path();
            path.parent().map(create_dir_all).transpose()?;

            let file = File::create(path)?;
            let writer = BufWriter::new(file);

            if let Err(e) = serde_json::to_writer(writer, layout) {
                tracing::error!("Failed to save window layout while exiting: {}", e);
            }
        }

        crossterm::terminal::disable_raw_mode()?;
        execute!(self.terminal.backend_mut(), LeaveAlternateScreen)?;
        self.terminal.show_cursor()?;

        return Ok(());
    }
}

fn gen_passphrase() -> String {
    rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(20)
        .map(char::from)
        .collect()
}

fn read_response(question: &str) -> String {
    println!("{question}");
    let mut input = String::new();
    let _ = std::io::stdin().read_line(&mut input);
    input
}

fn read_yesno(question: &str) -> Option<char> {
    read_response(question).chars().next().map(|c| c.to_ascii_lowercase())
}

async fn login(worker: &Requester, settings: &ApplicationSettings) -> IambResult<()> {
    if settings.session_json.is_file() {
        let session = settings.read_session(&settings.session_json)?;
        worker.login(LoginStyle::SessionRestore(session.into()))?;

        return Ok(());
    }

    if settings.session_json_old.is_file() && !settings.sled_dir.is_dir() {
        let session = settings.read_session(&settings.session_json_old)?;
        worker.login(LoginStyle::SessionRestore(session.into()))?;

        return Ok(());
    }

    loop {
        let login_style =
            match read_response("Please select login type: [p]assword / [s]ingle sign on")
                .chars()
                .next()
                .map(|c| c.to_ascii_lowercase())
            {
                None | Some('p') => {
                    let password = rpassword::prompt_password("Password: ")?;
                    LoginStyle::Password(password)
                },
                Some('s') => LoginStyle::SingleSignOn,
                Some(_) => {
                    println!("Failed to login. Please enter 'p' or 's'");
                    continue;
                },
            };

        match worker.login(login_style) {
            Ok(info) => {
                if let Some(msg) = info {
                    println!("{msg}");
                }

                break;
            },
            Err(err) => {
                println!("Failed to login: {err}");
                continue;
            },
        }
    }

    Ok(())
}

fn print_exit<T: Display, N>(v: T) -> N {
    eprintln!("{v}");
    process::exit(2);
}

// We can't access the OlmMachine directly, so write the keys to a temporary
// file first, and then import them later.
async fn check_import_keys(
    settings: &ApplicationSettings,
) -> IambResult<Option<(temp_dir::TempDir, String)>> {
    let do_import = settings.sled_dir.is_dir() && !settings.sqlite_dir.is_dir();

    if !do_import {
        return Ok(None);
    }

    let question = format!(
        "Found old sled store in {}. Would you like to export room keys from it? [y]es/[n]o",
        settings.sled_dir.display()
    );

    loop {
        match read_yesno(&question) {
            Some('y') => {
                break;
            },
            Some('n') => {
                return Ok(None);
            },
            Some(_) | None => {
                continue;
            },
        }
    }

    let keys = sled_export::export_room_keys(&settings.sled_dir).await?;
    let passphrase = gen_passphrase();

    println!("* Encrypting {} room keys with the passphrase {passphrase:?}...", keys.len());

    let encrypted = match encrypt_room_key_export(&keys, &passphrase, 500000) {
        Ok(encrypted) => encrypted,
        Err(e) => {
            println!("* Failed to encrypt room keys during export: {e}");
            process::exit(2);
        },
    };

    let tmpdir = TempDir::new()?;
    let exported = tmpdir.child("keys");

    println!("* Writing encrypted room keys to {}...", exported.display());
    tokio::fs::write(&exported, &encrypted).await?;

    Ok(Some((tmpdir, passphrase)))
}

async fn login_upgrade(
    keydir: TempDir,
    passphrase: String,
    worker: &Requester,
    settings: &ApplicationSettings,
    store: &AsyncProgramStore,
) -> IambResult<()> {
    println!(
        "Please log in for {} to import the room keys into a new session",
        settings.profile.user_id
    );

    login(worker, settings).await?;

    println!("* Importing room keys...");

    let exported = keydir.child("keys");
    let imported = worker.client.encryption().import_room_keys(exported, &passphrase).await;

    match imported {
        Ok(res) => {
            println!(
                "* Successfully imported {} out of {} keys",
                res.imported_count, res.total_count
            );
            let _ = keydir.cleanup();
        },
        Err(e) => {
            println!(
                "Failed to import room keys from {}/keys: {e}\n\n\
                They have been encrypted with the passphrase {passphrase:?}.\
                Please save them and try importing them manually instead\n",
                keydir.path().display()
            );

            loop {
                match read_yesno("Would you like to continue logging in? [y]es/[n]o") {
                    Some('y') => break,
                    Some('n') => print_exit("* Exiting..."),
                    Some(_) | None => continue,
                }
            }
        },
    }

    println!("* Syncing...");
    worker::do_first_sync(&worker.client, store)
        .await
        .map_err(IambError::from)?;

    Ok(())
}

async fn login_normal(
    worker: &Requester,
    settings: &ApplicationSettings,
    store: &AsyncProgramStore,
) -> IambResult<()> {
    println!("* Logging in for {}...", settings.profile.user_id);
    login(worker, settings).await?;
    println!("* Syncing...");
    worker::do_first_sync(&worker.client, store)
        .await
        .map_err(IambError::from)?;
    Ok(())
}

/// Set up the terminal for drawing the TUI, and getting additional info.
fn setup_tty(settings: &ApplicationSettings, enable_enhanced_keys: bool) -> std::io::Result<()> {
    let title = format!("iamb ({})", settings.profile.user_id.as_str());

    // Enable raw mode and enter the alternate screen.
    crossterm::terminal::enable_raw_mode()?;
    crossterm::execute!(stdout(), EnterAlternateScreen)?;

    if enable_enhanced_keys {
        // Enable the Kitty keyboard enhancement protocol for improved keypresses.
        crossterm::queue!(
            stdout(),
            PushKeyboardEnhancementFlags(KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES)
        )?;
    }

    if settings.tunables.mouse.enabled {
        crossterm::execute!(stdout(), EnableMouseCapture)?;
    }

    crossterm::execute!(stdout(), EnableBracketedPaste, EnableFocusChange, SetTitle(title))
}

// Do our best to reverse what we did in setup_tty() when we exit or crash.
fn restore_tty(enable_enhanced_keys: bool, enable_mouse: bool) {
    if enable_enhanced_keys {
        let _ = crossterm::queue!(stdout(), PopKeyboardEnhancementFlags);
    }

    if enable_mouse {
        let _ = crossterm::queue!(stdout(), DisableMouseCapture);
    }

    let _ = crossterm::execute!(
        stdout(),
        DisableBracketedPaste,
        DisableFocusChange,
        LeaveAlternateScreen,
        CursorShow,
    );

    let _ = crossterm::terminal::disable_raw_mode();
}

async fn run(settings: ApplicationSettings) -> IambResult<()> {
    // Get old keys the first time we run w/ the upgraded SDK.
    let import_keys = check_import_keys(&settings).await?;

    // Set up client state.
    create_dir_all(settings.sqlite_dir.as_path())?;
    let client = worker::create_client(&settings).await;

    // Set up the async worker thread and global store.
    let worker = ClientWorker::spawn(client.clone(), settings.clone()).await;
    let store = ChatStore::new(worker.clone(), settings.clone());
    let store = Store::new(store);
    let store = Arc::new(AsyncMutex::new(store));
    worker.init(store.clone());

    let res = if let Some((keydir, pass)) = import_keys {
        login_upgrade(keydir, pass, &worker, &settings, &store).await
    } else {
        login_normal(&worker, &settings, &store).await
    };

    match res {
        Err(UIError::Application(IambError::Matrix(e))) => {
            if let Some(ErrorKind::UnknownToken { .. }) = e.client_api_error_kind() {
                print_exit("Server did not recognize our API token; did you log out from this session elsewhere?")
            } else {
                print_exit(e)
            }
        },
        Err(e) => print_exit(e),
        Ok(()) => (),
    }

    // Set up the terminal for drawing, and cleanup properly on panics.
    let enable_enhanced_keys = match crossterm::terminal::supports_keyboard_enhancement() {
        Ok(supported) => supported,
        Err(e) => {
            tracing::warn!(err = %e,
               "Failed to determine whether the terminal supports keyboard enhancements");
            false
        },
    };
    setup_tty(&settings, enable_enhanced_keys)?;

    let orig_hook = std::panic::take_hook();
    let enable_mouse = settings.tunables.mouse.enabled;
    std::panic::set_hook(Box::new(move |panic_info| {
        restore_tty(enable_enhanced_keys, enable_mouse);
        orig_hook(panic_info);
        process::exit(1);
    }));

    // And finally, start running the terminal UI.
    let mut application = Application::new(settings, store).await?;
    application.run().await?;

    // Clean up the terminal on exit.
    restore_tty(enable_enhanced_keys, enable_mouse);

    Ok(())
}

fn main() -> IambResult<()> {
    // Parse command-line flags.
    let iamb = Iamb::parse();

    // Load configuration and set up the Matrix SDK.
    let settings = ApplicationSettings::load(iamb).unwrap_or_else(print_exit);

    // Set umask on Unix platforms so that tokens, keys, etc. are only readable by the user.
    #[cfg(unix)]
    unsafe {
        libc::umask(0o077);
    };

    // Set up the tracing subscriber so we can log client messages.
    let log_prefix = format!("iamb-log-{}", settings.profile_name);
    let log_dir = settings.dirs.logs.as_path();

    let appender = tracing_appender::rolling::daily(log_dir, log_prefix);
    let (appender, guard) = tracing_appender::non_blocking(appender);

    let subscriber = FmtSubscriber::builder()
        .with_writer(appender)
        .with_max_level(settings.tunables.log_level)
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .worker_threads(2)
        .thread_name_fn(|| {
            static ATOMIC_ID: AtomicUsize = AtomicUsize::new(0);
            let id = ATOMIC_ID.fetch_add(1, Ordering::SeqCst);
            format!("iamb-worker-{id}")
        })
        .build()
        .unwrap();

    rt.block_on(async move { run(settings).await })?;

    drop(guard);
    process::exit(0);
}
