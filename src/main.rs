#![allow(clippy::manual_range_contains)]
#![allow(clippy::needless_return)]
#![allow(clippy::result_large_err)]
#![allow(clippy::bool_assert_comparison)]
use std::collections::VecDeque;
use std::convert::TryFrom;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::{stdout, BufReader, BufWriter, Stdout};
use std::ops::DerefMut;
use std::process;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use clap::Parser;
use tokio::sync::Mutex as AsyncMutex;
use tracing_subscriber::FmtSubscriber;

use matrix_sdk::{
    config::SyncSettings,
    ruma::{
        api::client::filter::{FilterDefinition, LazyLoadOptions, RoomEventFilter, RoomFilter},
        OwnedUserId,
    },
};

use modalkit::crossterm::{
    self,
    cursor::Show as CursorShow,
    event::{
        poll,
        read,
        DisableBracketedPaste,
        DisableFocusChange,
        EnableBracketedPaste,
        EnableFocusChange,
        Event,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, SetTitle},
};

use modalkit::tui::{
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
        ProgramAction,
        ProgramContext,
        ProgramStore,
    },
    config::{ApplicationSettings, Iamb},
    windows::IambWindow,
    worker::{create_room, ClientWorker, LoginStyle, Requester},
};

use modalkit::{
    editing::{
        action::{
            Action,
            Commandable,
            EditError,
            EditInfo,
            Editable,
            EditorAction,
            InfoMessage,
            InsertTextAction,
            Jumpable,
            Promptable,
            Scrollable,
            TabAction,
            TabContainer,
            TabCount,
            UIError,
            WindowAction,
            WindowContainer,
        },
        base::{MoveDir1D, OpenTarget, RepeatType},
        context::Resolve,
        key::KeyManager,
        store::Store,
    },
    input::{bindings::BindingMachine, dialog::Pager, key::TerminalKey},
    widgets::{
        cmdbar::CommandBarState,
        screen::{FocusList, Screen, ScreenState, TabLayoutDescription},
        windows::WindowLayoutDescription,
        TerminalCursor,
        TerminalExtOps,
        Window,
    },
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
                    IambId::Room(room_id)
                },
                config::WindowPath::RoomId(room_id) => IambId::Room(room_id),
                config::WindowPath::AliasId(alias) => {
                    let name = alias.to_string();
                    let room_id = worker.join_room(name.clone())?;
                    names.insert(name, room_id.clone());
                    IambId::Room(room_id)
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

fn setup_screen(
    settings: ApplicationSettings,
    store: &mut ProgramStore,
) -> IambResult<ScreenState<IambWindow, IambInfo>> {
    let cmd = CommandBarState::new(store);
    let dims = crossterm::terminal::size()?;
    let area = Rect::new(0, 0, dims.0, dims.1);

    match settings.layout {
        config::Layout::Restore => {
            if let Ok(layout) = std::fs::read(&settings.layout_json) {
                let tabs: TabLayoutDescription<IambInfo> =
                    serde_json::from_slice(&layout).map_err(IambError::from)?;
                let tabs = tabs.to_layout(area.into(), store)?;

                return Ok(ScreenState::from_list(tabs, cmd));
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
    bindings: KeyManager<TerminalKey, ProgramAction, RepeatType, ProgramContext>,

    /// Pending actions to run.
    actstack: VecDeque<(ProgramAction, ProgramContext)>,

    /// Whether or not the terminal is currently focused.
    focused: bool,

    /// The tab layout before the last executed [TabAction].
    last_layout: Option<TabLayoutDescription<IambInfo>>,

    /// Whether we need to do a full redraw (e.g., after running a subprocess).
    dirty: bool,
}

impl Application {
    pub async fn new(
        settings: ApplicationSettings,
        store: AsyncProgramStore,
    ) -> IambResult<Application> {
        let mut stdout = stdout();
        crossterm::terminal::enable_raw_mode()?;
        crossterm::execute!(stdout, EnterAlternateScreen)?;
        crossterm::execute!(stdout, EnableBracketedPaste)?;
        crossterm::execute!(stdout, EnableFocusChange)?;

        let title = format!("iamb ({})", settings.profile.user_id);
        crossterm::execute!(stdout, SetTitle(title))?;

        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;

        let bindings = crate::keybindings::setup_keybindings();
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

        if full {
            term.clear()?;
        }

        term.draw(|f| {
            let area = f.size();

            let modestr = bindings.show_mode();
            let cursor = bindings.get_cursor_indicator();
            let dialogstr = bindings.show_dialog(area.height as usize, area.width as usize);

            // Don't show terminal cursor when we show a dialog.
            let hide_cursor = !dialogstr.is_empty();

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
                f.set_cursor(cx, cy);
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
                Event::Key(ke) => return Ok(ke.into()),
                Event::Mouse(_) => {
                    // Do nothing for now.
                },
                Event::FocusGained => {
                    self.focused = true;
                },
                Event::FocusLost => {
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
                let acts = store.application.cmds.command(&act, &ctx)?;
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
            IambAction::ToggleScrollbackFocus => {
                self.screen.current_window_mut()?.focus_toggle();

                None
            },

            IambAction::Homeserver(act) => {
                let acts = self.homeserver_command(act, ctx, store).await?;
                self.action_prepend(acts);

                None
            },
            IambAction::Message(act) => {
                self.screen.current_window_mut()?.message_command(act, ctx, store).await?
            },
            IambAction::Room(act) => {
                let acts = self.screen.current_window_mut()?.room_command(act, ctx, store).await?;
                self.action_prepend(acts);

                None
            },
            IambAction::Send(act) => {
                self.screen.current_window_mut()?.send_command(act, ctx, store).await?
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
                let room_id = create_room(client, alias.as_deref(), vis, flags).await?;
                let room = IambId::Room(room_id);
                let target = OpenTarget::Application(room);
                let action = WindowAction::Switch(target);

                Ok(vec![(action.into(), ctx)])
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

async fn login(worker: Requester, settings: &ApplicationSettings) -> IambResult<()> {
    println!("Logging in for {}...", settings.profile.user_id);

    if settings.session_json.is_file() {
        let file = File::open(settings.session_json.as_path())?;
        let reader = BufReader::new(file);
        let session = serde_json::from_reader(reader).map_err(IambError::from)?;

        worker.login(LoginStyle::SessionRestore(session))?;

        return Ok(());
    }

    loop {
        println!("Please select login type: [p]assword / [s]ingle sign on");

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let login_style = match input.chars().next().map(|c| c.to_ascii_lowercase()) {
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

    // Perform an initial, lazily-loaded sync.
    let mut room = RoomEventFilter::default();
    room.lazy_load_options = LazyLoadOptions::Enabled { include_redundant_members: false };

    let mut room_ev = RoomFilter::default();
    room_ev.state = room;

    let mut filter = FilterDefinition::default();
    filter.room = room_ev;

    let settings = SyncSettings::new().filter(filter.into());

    worker.client.sync_once(settings).await.map_err(IambError::from)?;

    Ok(())
}

fn print_exit<T: Display, N>(v: T) -> N {
    println!("{v}");
    process::exit(2);
}

async fn run(settings: ApplicationSettings) -> IambResult<()> {
    // Set up the async worker thread and global store.
    let worker = ClientWorker::spawn(settings.clone()).await;
    let store = ChatStore::new(worker.clone(), settings.clone());
    let store = Store::new(store);
    let store = Arc::new(AsyncMutex::new(store));
    worker.init(store.clone());

    login(worker, &settings).await.unwrap_or_else(print_exit);

    fn restore_tty() {
        let _ = crossterm::terminal::disable_raw_mode();
        let _ = crossterm::execute!(stdout(), DisableBracketedPaste);
        let _ = crossterm::execute!(stdout(), DisableFocusChange);
        let _ = crossterm::execute!(stdout(), LeaveAlternateScreen);
        let _ = crossterm::execute!(stdout(), CursorShow);
    }

    // Make sure panics clean up the terminal properly.
    let orig_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        restore_tty();
        orig_hook(panic_info);
        process::exit(1);
    }));

    let mut application = Application::new(settings, store).await?;

    // We can now run the application.
    application.run().await?;
    restore_tty();

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

    create_dir_all(settings.matrix_dir.as_path())?;
    create_dir_all(log_dir)?;

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
