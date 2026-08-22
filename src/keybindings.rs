//! # Default Keybindings
//!
//! The keybindings are set up here. We define some iamb-specific keybindings, but the default Vim
//! keys come from [modalkit::env::vim::keybindings].
use modalkit::{
    actions::{InsertTextAction, MacroAction, WindowAction},
    env::CommonKeyClass,
    env::vim::VimMode,
    env::vim::keybindings::{InputStep, VimBindings},
    key::TerminalKey,
    keybindings::{EdgeEvent, EdgeRepeat, InputBindings},
    prelude::*,
};

use crate::base::{IambAction, IambInfo, Keybindings, MATRIX_ID_WORD};
use crate::config::{ApplicationSettings, Keys, SplitDirection};

pub type IambStep = InputStep<IambInfo>;

fn once(key: &TerminalKey) -> (EdgeRepeat, EdgeEvent<TerminalKey, CommonKeyClass>) {
    (EdgeRepeat::Once, EdgeEvent::Key(*key))
}

/// Initialize the default keybinding state.
pub fn setup_keybindings() -> Keybindings {
    let mut ism = Keybindings::empty();

    let vim = VimBindings::default()
        .submit_on_enter()
        .cursor_open(MATRIX_ID_WORD.clone());

    vim.setup(&mut ism);

    let ctrl_w = "<C-W>".parse::<TerminalKey>().unwrap();
    let ctrl_m = "<C-M>".parse::<TerminalKey>().unwrap();
    let ctrl_z = "<C-Z>".parse::<TerminalKey>().unwrap();
    let key_m_lc = "m".parse::<TerminalKey>().unwrap();
    let key_z_lc = "z".parse::<TerminalKey>().unwrap();
    let shift_enter = "<S-Enter>".parse::<TerminalKey>().unwrap();

    let cwz = vec![once(&ctrl_w), once(&key_z_lc)];
    let cwcz = vec![once(&ctrl_w), once(&ctrl_z)];
    let zoom = IambStep::new()
        .actions(vec![WindowAction::ZoomToggle.into()])
        .goto(VimMode::Normal);

    ism.add_mapping(VimMode::Normal, &cwz, &zoom);
    ism.add_mapping(VimMode::Visual, &cwz, &zoom);
    ism.add_mapping(VimMode::Normal, &cwcz, &zoom);
    ism.add_mapping(VimMode::Visual, &cwcz, &zoom);

    let cwm = vec![once(&ctrl_w), once(&key_m_lc)];
    let cwcm = vec![once(&ctrl_w), once(&ctrl_m)];
    let stoggle = IambStep::new()
        .actions(vec![IambAction::ToggleScrollbackFocus.into()])
        .goto(VimMode::Normal);
    ism.add_mapping(VimMode::Normal, &cwm, &stoggle);
    ism.add_mapping(VimMode::Visual, &cwm, &stoggle);
    ism.add_mapping(VimMode::Normal, &cwcm, &stoggle);
    ism.add_mapping(VimMode::Visual, &cwcm, &stoggle);

    let shift_enter = vec![once(&shift_enter)];
    let newline = IambStep::new().actions(vec![
        InsertTextAction::Type(Char::Single('\n').into(), MoveDir1D::Previous, 1.into()).into(),
    ]);
    ism.add_mapping(VimMode::Insert, &cwm, &newline);
    ism.add_mapping(VimMode::Insert, &shift_enter, &newline);

    ism
}

impl InputBindings<TerminalKey, IambStep> for ApplicationSettings {
    fn setup(&self, bindings: &mut Keybindings) {
        for (modes, keys) in &self.macros {
            for (Keys(input, _), Keys(_, run)) in keys {
                let act = MacroAction::Run(run.clone(), Count::Contextual);
                let step = IambStep::new().actions(vec![act.into()]);
                let input = input.iter().map(once).collect::<Vec<_>>();

                for mode in &modes.0 {
                    bindings.add_mapping(*mode, &input, &step);
                }
            }
        }

        if self.tunables.default_split == SplitDirection::Vertical {
            let ctrl_w = "<C-W>".parse::<TerminalKey>().unwrap();
            let key_f = "f".parse::<TerminalKey>().unwrap();
            let ctrl_f = "<C-F>".parse::<TerminalKey>().unwrap();

            let vsplit_open = IambStep::new()
                .actions(vec![
                    WindowAction::Split(
                        OpenTarget::Cursor(MATRIX_ID_WORD.clone()),
                        Axis::Vertical,
                        MoveDir1D::Next,
                        1.into(),
                    )
                    .into(),
                ])
                .goto(VimMode::Normal);

            let cwf = vec![once(&ctrl_w), once(&key_f)];
            let cwcf = vec![once(&ctrl_w), once(&ctrl_f)];

            bindings.add_mapping(VimMode::Normal, &cwf, &vsplit_open);
            bindings.add_mapping(VimMode::Normal, &cwcf, &vsplit_open);
        }
    }
}
