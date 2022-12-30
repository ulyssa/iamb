use std::ops::{Deref, DerefMut};

use modalkit::tui::{buffer::Buffer, layout::Rect};

use modalkit::{
    widgets::textbox::TextBoxState,
    widgets::WindowOps,
    widgets::{TermOffset, TerminalCursor},
};

use modalkit::editing::base::{CloseFlags, WordStyle};

use crate::base::{IambBufferId, IambInfo, ProgramStore};

const WELCOME_TEXT: &str = include_str!("welcome.md");

pub struct WelcomeState {
    tbox: TextBoxState<IambInfo>,
}

impl WelcomeState {
    pub fn new(store: &mut ProgramStore) -> Self {
        let buf = store.buffers.load_str(IambBufferId::Welcome, WELCOME_TEXT);

        WelcomeState { tbox: TextBoxState::new(buf) }
    }
}

impl Deref for WelcomeState {
    type Target = TextBoxState<IambInfo>;

    fn deref(&self) -> &Self::Target {
        return &self.tbox;
    }
}

impl DerefMut for WelcomeState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        return &mut self.tbox;
    }
}

impl TerminalCursor for WelcomeState {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.tbox.get_term_cursor()
    }
}

impl WindowOps<IambInfo> for WelcomeState {
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut ProgramStore) {
        self.tbox.draw(area, buf, focused, store)
    }

    fn dup(&self, store: &mut ProgramStore) -> Self {
        let tbox = self.tbox.dup(store);

        WelcomeState { tbox }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut ProgramStore) -> bool {
        self.tbox.close(flags, store)
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        self.tbox.get_cursor_word(style)
    }

    fn get_selected_word(&self) -> Option<String> {
        self.tbox.get_selected_word()
    }
}
