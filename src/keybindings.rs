//! # Default Keybindings
//!
//! The keybindings are set up here. We define some iamb-specific keybindings, but the default Vim
//! keys come from [modalkit::env::vim::keybindings].
use modalkit::{
    editing::action::WindowAction,
    env::vim::keybindings::{InputStep, VimBindings},
    env::vim::VimMode,
    key::TerminalKey,
    keybindings::{EdgeEvent, EdgeRepeat, InputBindings},
};

use crate::base::{IambAction, IambInfo, Keybindings, MATRIX_ID_WORD};

type IambStep = InputStep<IambInfo>;

/// Initialize the default keybinding state.
pub fn setup_keybindings() -> Keybindings {
    let mut ism = Keybindings::empty();

    let vim = VimBindings::default()
        .submit_on_enter()
        .cursor_open(MATRIX_ID_WORD.clone());

    vim.setup(&mut ism);

    let ctrl_w = EdgeEvent::Key("<C-W>".parse::<TerminalKey>().unwrap());
    let ctrl_m = EdgeEvent::Key("<C-M>".parse::<TerminalKey>().unwrap());
    let ctrl_z = EdgeEvent::Key("<C-Z>".parse::<TerminalKey>().unwrap());
    let key_m_lc = EdgeEvent::Key("m".parse::<TerminalKey>().unwrap());
    let key_z_lc = EdgeEvent::Key("z".parse::<TerminalKey>().unwrap());

    let cwz = vec![
        (EdgeRepeat::Once, ctrl_w.clone()),
        (EdgeRepeat::Once, key_z_lc),
    ];
    let cwcz = vec![
        (EdgeRepeat::Once, ctrl_w.clone()),
        (EdgeRepeat::Once, ctrl_z),
    ];
    let zoom = IambStep::new()
        .actions(vec![WindowAction::ZoomToggle.into()])
        .goto(VimMode::Normal);

    ism.add_mapping(VimMode::Normal, &cwz, &zoom);
    ism.add_mapping(VimMode::Visual, &cwz, &zoom);
    ism.add_mapping(VimMode::Normal, &cwcz, &zoom);
    ism.add_mapping(VimMode::Visual, &cwcz, &zoom);

    let cwm = vec![
        (EdgeRepeat::Once, ctrl_w.clone()),
        (EdgeRepeat::Once, key_m_lc),
    ];
    let cwcm = vec![(EdgeRepeat::Once, ctrl_w), (EdgeRepeat::Once, ctrl_m)];
    let stoggle = IambStep::new()
        .actions(vec![IambAction::ToggleScrollbackFocus.into()])
        .goto(VimMode::Normal);
    ism.add_mapping(VimMode::Normal, &cwm, &stoggle);
    ism.add_mapping(VimMode::Visual, &cwm, &stoggle);
    ism.add_mapping(VimMode::Normal, &cwcm, &stoggle);
    ism.add_mapping(VimMode::Visual, &cwcm, &stoggle);

    return ism;
}
