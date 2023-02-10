use modalkit::{
    editing::action::WindowAction,
    editing::base::WordStyle,
    env::vim::keybindings::{InputStep, VimBindings},
    env::vim::VimMode,
    input::bindings::{EdgeEvent, EdgeRepeat, InputBindings},
    input::key::TerminalKey,
};

use crate::base::{IambAction, IambInfo, Keybindings};

type IambStep = InputStep<IambInfo>;

/// Find the boundaries for a Matrix username, room alias, or room ID.
///
/// Technically "[" and "]" should be here since IPv6 addresses are allowed
/// in the server name, but in practice that should be uncommon, and people
/// can just use `gf` and friends in Visual mode instead.
fn is_mxid_char(c: char) -> bool {
    return c >= 'a' && c <= 'z' ||
        c >= 'A' && c <= 'Z' ||
        c >= '0' && c <= '9' ||
        ":-./@_#!".contains(c);
}

pub fn setup_keybindings() -> Keybindings {
    let mut ism = Keybindings::empty();

    let vim = VimBindings::default()
        .submit_on_enter()
        .cursor_open(WordStyle::CharSet(is_mxid_char));

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
