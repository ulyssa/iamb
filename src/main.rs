#![allow(clippy::manual_range_contains)]
#![allow(clippy::needless_return)]
#![allow(clippy::result_large_err)]
#![allow(clippy::bool_assert_comparison)]
use std::collections::VecDeque;
use std::convert::TryFrom;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::{stdout, BufReader, Stdout};
use std::ops::DerefMut;
use std::process;
use std::sync::Arc;
use std::time::Duration;

use clap::Parser;
use tokio::sync::Mutex as AsyncMutex;
use tracing::{self, Level};
use tracing_subscriber::FmtSubscriber;

use matrix_sdk::ruma::OwnedUserId;

use modalkit::crossterm::{
    self,
    cursor::Show as CursorShow,
    event::{poll, read, Event},
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
mod windows;
mod worker;

#[cfg(test)]
mod tests;

use crate::{
    base::{
        AsyncProgramStore,
        ChatStore,
        IambAction,
        IambBufferId,
        IambError,
        IambId,
        IambInfo,
        IambResult,
        ProgramAction,
        ProgramCommands,
        ProgramContext,
        ProgramStore,
    },
    config::{ApplicationSettings, Iamb},
    message::{Message, MessageContent, MessageTimeStamp},
    windows::IambWindow,
    worker::{ClientWorker, LoginStyle, Requester},
};

use modalkit::{
    editing::{
        action::{
            Action,
            Commandable,
            EditError,
            EditInfo,
            Editable,
            Jumpable,
            Promptable,
            Scrollable,
            TabContainer,
            TabCount,
            WindowAction,
            WindowContainer,
        },
        base::{OpenTarget, RepeatType},
        context::Resolve,
        key::KeyManager,
        store::Store,
    },
    input::{bindings::BindingMachine, key::TerminalKey},
    widgets::{
        cmdbar::CommandBarState,
        screen::{Screen, ScreenState},
        TerminalCursor,
        TerminalExtOps,
        Window,
    },
};

struct Application {
    store: AsyncProgramStore,
    worker: Requester,
    terminal: Terminal<CrosstermBackend<Stdout>>,
    bindings: KeyManager<TerminalKey, ProgramAction, RepeatType, ProgramContext>,
    actstack: VecDeque<(ProgramAction, ProgramContext)>,
    cmds: ProgramCommands,
    screen: ScreenState<IambWindow, IambInfo>,
}

impl Application {
    pub async fn new(
        settings: ApplicationSettings,
        store: AsyncProgramStore,
    ) -> IambResult<Application> {
        let mut stdout = stdout();
        crossterm::terminal::enable_raw_mode()?;
        crossterm::execute!(stdout, EnterAlternateScreen)?;

        let title = format!("iamb ({})", settings.profile.user_id);
        crossterm::execute!(stdout, SetTitle(title))?;

        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;

        let bindings = crate::keybindings::setup_keybindings();
        let bindings = KeyManager::new(bindings);
        let cmds = crate::commands::setup_commands();

        let mut locked = store.lock().await;
        let win = IambWindow::open(IambId::Welcome, locked.deref_mut()).unwrap();
        let cmd = CommandBarState::new(IambBufferId::Command, locked.deref_mut());
        let screen = ScreenState::new(win, cmd);

        let worker = locked.application.worker.clone();
        drop(locked);

        let actstack = VecDeque::new();

        Ok(Application {
            store,
            worker,
            terminal,
            bindings,
            actstack,
            cmds,
            screen,
        })
    }

    fn redraw(&mut self, full: bool, store: &mut ProgramStore) -> Result<(), std::io::Error> {
        let modestr = self.bindings.showmode();
        let cursor = self.bindings.get_cursor_indicator();
        let sstate = &mut self.screen;
        let term = &mut self.terminal;

        if full {
            term.clear()?;
        }

        term.draw(|f| {
            let area = f.size();

            let screen = Screen::new(store).showmode(modestr);
            f.render_stateful_widget(screen, area, sstate);

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

            store.application.load_older(area.height as u32);
        })?;

        Ok(())
    }

    async fn step(&mut self) -> Result<TerminalKey, std::io::Error> {
        loop {
            self.redraw(false, self.store.clone().lock().await.deref_mut())?;

            if !poll(Duration::from_millis(500))? {
                continue;
            }

            match read()? {
                Event::Key(ke) => return Ok(ke.into()),
                Event::Mouse(_) => {
                    // Do nothing for now.
                },
                Event::FocusGained | Event::FocusLost => {
                    // Do nothing for now.
                },
                Event::Resize(_, _) => {
                    // We'll redraw for the new size next time step() is called.
                },
                Event::Paste(_) => {
                    // Do nothing for now.
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

    fn action_run(
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
            Action::Application(act) => self.iamb_run(act, ctx, store)?,
            Action::CommandBar(act) => self.screen.command_bar(&act, &ctx)?,
            Action::Macro(act) => self.bindings.macro_command(&act, &ctx, store)?,
            Action::Scroll(style) => self.screen.scroll(&style, &ctx, store)?,
            Action::Suspend => self.terminal.program_suspend()?,
            Action::Tab(cmd) => self.screen.tab_command(&cmd, &ctx, store)?,
            Action::Window(cmd) => self.screen.window_command(&cmd, &ctx, store)?,

            Action::Jump(l, dir, count) => {
                let count = ctx.resolve(&count);
                let _ = self.screen.jump(l, dir, count, &ctx)?;

                None
            },

            // UI actions.
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
                let acts = self.cmds.command(&act, &ctx)?;
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

    fn iamb_run(
        &mut self,
        action: IambAction,
        _: ProgramContext,
        store: &mut ProgramStore,
    ) -> IambResult<EditInfo> {
        let info = match action {
            IambAction::ToggleScrollbackFocus => {
                self.screen.current_window_mut()?.focus_toggle();

                None
            },

            IambAction::SendMessage(room_id, msg) => {
                let (event_id, msg) = self.worker.send_message(room_id.clone(), msg)?;
                let user = store.application.settings.profile.user_id.clone();
                let info = store.application.get_room_info(room_id);
                let key = (MessageTimeStamp::LocalEcho, event_id);
                let msg = MessageContent::Original(msg.into());
                let msg = Message::new(msg, user, MessageTimeStamp::LocalEcho);
                info.messages.insert(key, msg);

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

    pub async fn run(&mut self) -> Result<(), std::io::Error> {
        self.terminal.clear()?;

        let store = self.store.clone();

        while self.screen.tabs() != 0 {
            let key = self.step().await?;

            self.bindings.input_key(key);

            let mut locked = store.lock().await;
            let mut keyskip = false;

            while let Some((action, ctx)) = self.action_pop(keyskip) {
                match self.action_run(action, ctx, locked.deref_mut()) {
                    Ok(None) => {
                        // Continue processing.
                        continue;
                    },
                    Ok(Some(info)) => {
                        self.screen.push_info(info);

                        // Continue processing; we'll redraw later.
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

        crossterm::terminal::disable_raw_mode()?;
        execute!(self.terminal.backend_mut(), LeaveAlternateScreen)?;
        self.terminal.show_cursor()?;

        return Ok(());
    }
}

fn login(worker: Requester, settings: &ApplicationSettings) -> IambResult<()> {
    println!("Logging in for {}...", settings.profile.user_id);

    if settings.session_json.is_file() {
        let file = File::open(settings.session_json.as_path())?;
        let reader = BufReader::new(file);
        let session = serde_json::from_reader(reader).map_err(IambError::from)?;

        worker.login(LoginStyle::SessionRestore(session))?;

        return Ok(());
    }

    loop {
        let password = rpassword::prompt_password("Password: ")?;

        match worker.login(LoginStyle::Password(password)) {
            Ok(info) => {
                if let Some(msg) = info {
                    println!("{}", msg);
                }

                break;
            },
            Err(err) => {
                println!("Failed to login: {}", err);
                continue;
            },
        }
    }

    Ok(())
}

fn print_exit<T: Display, N>(v: T) -> N {
    println!("{}", v);
    process::exit(2);
}

#[tokio::main]
async fn main() -> IambResult<()> {
    // Parse command-line flags.
    let iamb = Iamb::parse();

    // Load configuration and set up the Matrix SDK.
    let settings = ApplicationSettings::load(iamb).unwrap_or_else(print_exit);

    // Set up the tracing subscriber so we can log client messages.
    let log_prefix = format!("iamb-log-{}", settings.profile_name);
    let mut log_dir = settings.dirs.cache.clone();
    log_dir.push("logs");

    create_dir_all(settings.matrix_dir.as_path())?;
    create_dir_all(log_dir.as_path())?;

    let appender = tracing_appender::rolling::daily(log_dir, log_prefix);
    let (appender, _) = tracing_appender::non_blocking(appender);

    let subscriber = FmtSubscriber::builder()
        .with_writer(appender)
        .with_max_level(Level::WARN)
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    // Set up the async worker thread and global store.
    let worker = ClientWorker::spawn(settings.clone());
    let store = ChatStore::new(worker.clone(), settings.clone());
    let store = Store::new(store);
    let store = Arc::new(AsyncMutex::new(store));
    worker.init(store.clone());

    login(worker, &settings).unwrap_or_else(print_exit);

    // Make sure panics clean up the terminal properly.
    let orig_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        let _ = crossterm::terminal::disable_raw_mode();
        let _ = crossterm::execute!(stdout(), LeaveAlternateScreen);
        let _ = crossterm::execute!(stdout(), CursorShow);
        orig_hook(panic_info);
        process::exit(1);
    }));

    let mut application = Application::new(settings, store).await?;

    // We can now run the application.
    application.run().await?;

    process::exit(0);
}
