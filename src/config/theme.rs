//! # Logic for parsing styling configuration into [Style].
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use anyhow::Context;
use ratatui::widgets::BorderType;
use serde::de::Error as SerdeError;
use serde::de::Visitor;
use serde::{Deserialize, Deserializer};

use crate::prelude::*;

pub fn default_theme() -> Theme {
    Theme {
        messages: ThemeMessages {
            blockquote: LineStylable {
                line: Some(LineName::Thick),
                stylable: Stylable::fg(Color::Indexed(236)),
            },
            code: Stylable::bg(Color::Indexed(236)),
            code_block: LineStylable {
                line: Some(LineName::Plain),
                stylable: Stylable::default(),
            },
            emphasis: Stylable::with_modifiers(StyleModifier::ITALIC),
            strikethrough: Stylable::with_modifiers(StyleModifier::CROSSED_OUT),
            strong: Stylable::with_modifiers(StyleModifier::BOLD),
            underlined: Stylable::with_modifiers(StyleModifier::UNDERLINED),
            ..Default::default()
        },
        encryption: ThemeEncryption {
            icon_encrypted: Stylable::fg(Color::LightGreen),
            icon_unencrypted: Stylable::fg(Color::Red),
            icon_unknown: Stylable::fg(Color::Yellow),
            ..Default::default()
        },
        tabs: ThemeTabs {
            title: Stylable::with_modifiers(StyleModifier::DIM | StyleModifier::BOLD),
            title_focused: Stylable::without_modifiers(StyleModifier::DIM),
        },
        windows: ThemeWindows {
            border: Stylable::with_modifiers(StyleModifier::DIM).into(),
            border_focused: Stylable::without_modifiers(StyleModifier::DIM).into(),
            title: Stylable::with_modifiers(StyleModifier::BOLD),
            ..Default::default()
        },
        completion: ThemeCompletion {
            default: Stylable {
                color: Some(Color::Reset),
                background: Some(Color::Reset),
                modifiers: vec![
                    ModifierChange::Remove(StyleModifier::all()),
                    ModifierChange::Insert(StyleModifier::REVERSED),
                ],
            },
            selected: Stylable {
                color: Some(Color::Yellow),
                background: Some(Color::Black),
                modifiers: vec![ModifierChange::Remove(StyleModifier::REVERSED)],
            },
        },
        timeline: ThemeTimeline {
            date: Stylable::with_modifiers(StyleModifier::BOLD),
            unread_marker: Stylable::with_modifiers(StyleModifier::DIM),
            ..Default::default()
        },
        rooms: ThemeRooms {
            unread: ThemeRoomsUnreads {
                number: Stylable::fg(Color::Gray),
                ..Default::default()
            },
            notification: ThemeRoomsUnreads {
                name: Stylable::with_modifiers(StyleModifier::BOLD),
                number: Stylable::fg(Color::Yellow),
                ..Default::default()
            },
            mention: ThemeRoomsUnreads {
                name: Stylable::with_modifiers(StyleModifier::BOLD),
                number: Stylable::fg(Color::Red),
                ..Default::default()
            },
            marked_unread: ThemeRoomsUnreads {
                name: Stylable::with_modifiers(StyleModifier::BOLD),
                number: Stylable::fg(Color::Green),
                ..Default::default()
            },
            ..Default::default()
        },
        users: ThemeUsers {
            colors: Some(vec![
                Color::Blue,
                Color::Cyan,
                Color::Green,
                Color::LightBlue,
                Color::LightGreen,
                Color::LightCyan,
                Color::LightMagenta,
                Color::LightRed,
                Color::LightYellow,
                Color::Magenta,
                Color::Red,
                Color::Reset,
                Color::Yellow,
            ]),
            stylable: Stylable::with_modifiers(StyleModifier::BOLD),
        },
        ..Default::default()
    }
}

pub fn find_themes(dir: &Path) -> anyhow::Result<Vec<(String, Theme)>> {
    let entries = std::fs::read_dir(dir)
        .with_context(|| format!("Cannot list {} contents", dir.display()))?;
    let mut themes = vec![];

    for res in entries {
        let Ok(entry) = res else {
            continue;
        };

        let path = entry.path();

        if !path.is_file() {
            // Skip non-files.
            continue;
        }

        if path.extension().is_none_or(|ext| ext != "toml") {
            // Skip non-`.toml` files.
            continue;
        }

        let Some(name) = path.file_stem() else {
            continue;
        };

        let file = ThemeFile::load(&path)?;
        let name = name.to_string_lossy().into_owned();
        themes.push((name, file.theme));
    }

    Ok(themes)
}

/// A restricted subset of `config.toml` that only allows specifying themes.
///
/// This exists mainly to prevent people from thinking that values they put
/// into the theme files are taking effect when they aren't: theme files are
/// only used for sourcing theme information to prevent any weirdness around
/// what happens when changing the theme with `:theme`.
#[derive(Clone, Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ThemeFile {
    pub theme: Theme,
}

impl ThemeFile {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        let input = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        toml::de::from_str(&input).with_context(|| format!("failed to read {}", path.display()))
    }
}

/// Internal variant to get lowercase, hyphenated config options.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum LineName {
    #[default]
    Plain,
    Rounded,
    Double,
    Thick,
    DoubleDashedLight,
    DoubleDashedHeavy,
    TripleDashedLight,
    TripleDashedHeavy,
    QuadrupleDashedLight,
    QuadrupleDashedHeavy,
}

impl LineName {
    pub fn to_table_set(&self) -> &'static ratatui::symbols::line::Set<'static> {
        match self {
            Self::Plain => &ratatui::symbols::line::NORMAL,
            Self::Rounded => &ratatui::symbols::line::ROUNDED,
            Self::Double => &ratatui::symbols::line::DOUBLE,
            Self::Thick => &ratatui::symbols::line::THICK,
            Self::DoubleDashedLight => &ratatui::symbols::line::LIGHT_DOUBLE_DASHED,
            Self::DoubleDashedHeavy => &ratatui::symbols::line::HEAVY_DOUBLE_DASHED,
            Self::TripleDashedLight => &ratatui::symbols::line::LIGHT_TRIPLE_DASHED,
            Self::TripleDashedHeavy => &ratatui::symbols::line::HEAVY_TRIPLE_DASHED,
            Self::QuadrupleDashedLight => &ratatui::symbols::line::LIGHT_QUADRUPLE_DASHED,
            Self::QuadrupleDashedHeavy => &ratatui::symbols::line::HEAVY_QUADRUPLE_DASHED,
        }
    }
}

impl From<LineName> for BorderType {
    fn from(name: LineName) -> Self {
        match name {
            LineName::Plain => Self::Plain,
            LineName::Rounded => Self::Rounded,
            LineName::Double => Self::Double,
            LineName::Thick => Self::Thick,
            LineName::DoubleDashedLight => Self::LightDoubleDashed,
            LineName::DoubleDashedHeavy => Self::HeavyDoubleDashed,
            LineName::TripleDashedLight => Self::LightTripleDashed,
            LineName::TripleDashedHeavy => Self::HeavyTripleDashed,
            LineName::QuadrupleDashedLight => Self::LightQuadrupleDashed,
            LineName::QuadrupleDashedHeavy => Self::HeavyQuadrupleDashed,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum ModifierChange {
    Insert(StyleModifier),
    Remove(StyleModifier),
}

impl ModifierChange {
    fn apply(self, style: &mut Style) {
        *style = match self {
            Self::Insert(m) => style.add_modifier(m),
            Self::Remove(m) => style.remove_modifier(m),
        };
    }
}

struct ModifierChangeVisitor;

impl<'de> Deserialize<'de> for ModifierChange {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(ModifierChangeVisitor)
    }
}

impl Visitor<'_> for ModifierChangeVisitor {
    type Value = ModifierChange;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid style modifier (e.g. \"bold\" or \"~reversed\")")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        let name = value.strip_prefix("~");
        let remove = name.is_some();

        let modifier = match name.unwrap_or(value) {
            "bold" => StyleModifier::BOLD,
            "crossed-out" => StyleModifier::CROSSED_OUT,
            "dim" => StyleModifier::DIM,
            "hidden" => StyleModifier::HIDDEN,
            "italic" => StyleModifier::ITALIC,
            "rapid-blink" => StyleModifier::RAPID_BLINK,
            "reversed" => StyleModifier::REVERSED,
            "slow-blink" => StyleModifier::SLOW_BLINK,
            "underlined" => StyleModifier::UNDERLINED,
            other => {
                let msg = format!("{other:?} is not a valid style modifier");
                let err = E::custom(msg);
                return Err(err);
            },
        };

        if remove {
            Ok(ModifierChange::Remove(modifier))
        } else {
            Ok(ModifierChange::Insert(modifier))
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize)]
struct Stylable {
    background: Option<Color>,
    color: Option<Color>,
    #[serde(default)]
    modifiers: Vec<ModifierChange>,
}

impl Stylable {
    fn bg(c: Color) -> Self {
        Self {
            background: Some(c),
            color: None,
            modifiers: vec![],
        }
    }

    fn fg(c: Color) -> Self {
        Self {
            background: None,
            color: Some(c),
            modifiers: vec![],
        }
    }

    fn with_modifiers(m: StyleModifier) -> Self {
        Self {
            background: None,
            color: None,
            modifiers: vec![ModifierChange::Insert(m)],
        }
    }

    fn without_modifiers(m: StyleModifier) -> Self {
        Self {
            background: None,
            color: None,
            modifiers: vec![ModifierChange::Remove(m)],
        }
    }

    fn merge(self, other: Self) -> Self {
        Self {
            background: self.background.or(other.background),
            color: self.color.or(other.color),
            modifiers: other.modifiers.iter().chain(&self.modifiers).copied().collect(),
        }
    }
}

impl From<Stylable> for Style {
    fn from(styled: Stylable) -> Self {
        let mut style = Style::default();

        if let Some(bg) = styled.background {
            style = style.bg(bg);
        }

        if let Some(fg) = styled.color {
            style = style.fg(fg);
        }

        for m in styled.modifiers.iter() {
            m.apply(&mut style);
        }

        style
    }
}

#[derive(Clone, Debug, Default, Deserialize)]
struct LineStylable {
    #[serde(rename = "line")]
    line: Option<LineName>,
    #[serde(flatten)]
    stylable: Stylable,
}

impl LineStylable {
    fn merge(self, other: Self) -> Self {
        Self {
            line: self.line.or(other.line),
            stylable: self.stylable.merge(other.stylable),
        }
    }
}

impl From<Stylable> for LineStylable {
    fn from(stylable: Stylable) -> Self {
        Self { stylable, line: None }
    }
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct Theme {
    /// Default styling when others aren't specified.
    #[serde(default)]
    default: Stylable,

    /// Configuration for styling the command bar.
    #[serde(default)]
    cmdbar: ThemeCommandBar,

    /// Configuration for styling the encryption indicators.
    #[serde(default)]
    encryption: ThemeEncryption,

    /// Configuration for styling the message bar.
    #[serde(default)]
    msgbar: ThemeMessageBar,

    /// Configuration specificaly for messages shown within a room's timeline.
    #[serde(default)]
    messages: ThemeMessages,

    /// Configuration for styling a room's timeline.
    #[serde(default)]
    timeline: ThemeTimeline,

    /// Configuration for styling items within room lists.
    #[serde(default)]
    rooms: ThemeRooms,

    /// Configuration for styling tabs.
    #[serde(default)]
    tabs: ThemeTabs,

    /// Configuration for styling users.
    #[serde(default)]
    users: ThemeUsers,

    /// Configuration for styling windows.
    #[serde(default)]
    windows: ThemeWindows,

    /// Configuration for the completion menu.
    #[serde(default)]
    completion: ThemeCompletion,
}

impl Theme {
    pub fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            cmdbar: self.cmdbar.merge(other.cmdbar),
            encryption: self.encryption.merge(other.encryption),
            msgbar: self.msgbar.merge(other.msgbar),
            messages: self.messages.merge(other.messages),
            tabs: self.tabs.merge(other.tabs),
            timeline: self.timeline.merge(other.timeline),
            rooms: self.rooms.merge(other.rooms),
            users: self.users.merge(other.users),
            windows: self.windows.merge(other.windows),
            completion: self.completion.merge(other.completion),
        }
    }

    pub fn values(self) -> ThemeValues {
        let base = Style::from(self.default);

        ThemeValues {
            default: base,
            cmdbar: self.cmdbar.values(base),
            encryption: self.encryption.values(base),
            msgbar: self.msgbar.values(base),
            messages: self.messages.values(base),
            timeline: self.timeline.values(base),
            rooms: self.rooms.values(base),
            tabs: self.tabs.values(base),
            users: self.users.values(base),
            windows: self.windows.values(base),
            completion: self.completion.values(base),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeValues {
    /// Styling to use when nothing more specific applies.
    pub default: Style,

    /// Styling for the command bar.
    pub cmdbar: ThemeCommandBarValues,

    /// Styling for the encryption indicators.
    pub encryption: ThemeEncryptionValues,

    /// Styling for the message bar.
    pub msgbar: ThemeMessageBarValues,

    /// Styling specificaly for messages shown within a room's timeline.
    pub messages: ThemeMessagesValues,

    /// Styling for a room's timeline.
    pub timeline: ThemeTimelineValues,

    /// Styling for items within room lists.
    pub rooms: ThemeRoomsValues,

    /// Styling for rendering tabs.
    pub tabs: ThemeTabsValues,

    /// Styling for rendering users.
    pub users: ThemeUsersValues,

    /// Styling for rendering windows.
    pub windows: ThemeWindowsValues,

    /// Styling for the completion menu.
    pub completion: ThemeCompletionValues,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeMessages {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    blockquote: LineStylable,

    #[serde(default)]
    code: Stylable,

    #[serde(default)]
    code_block: LineStylable,

    #[serde(default)]
    ruler: LineStylable,

    #[serde(default)]
    table: LineStylable,

    #[serde(default)]
    emphasis: Stylable,

    #[serde(default)]
    strikethrough: Stylable,

    #[serde(default)]
    strong: Stylable,

    #[serde(default)]
    underlined: Stylable,
}

impl ThemeMessages {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            blockquote: self.blockquote.merge(other.blockquote),
            code: self.code.merge(other.code),
            code_block: self.code_block.merge(other.code_block),
            ruler: self.ruler.merge(other.ruler),
            table: self.table.merge(other.table),
            emphasis: self.emphasis.merge(other.emphasis),
            strong: self.strong.merge(other.strong),
            strikethrough: self.strikethrough.merge(other.strikethrough),
            underlined: self.underlined.merge(other.underlined),
        }
    }

    fn values(self, base: Style) -> ThemeMessagesValues {
        let default = base.patch(self.default);
        let blockquote = default.patch(self.blockquote.stylable);
        let ruler = default.patch(self.ruler.stylable);
        let table = default.patch(self.table.stylable);
        let emphasis = default.patch(self.emphasis);
        let strong = default.patch(self.strong);
        let strikethrough = default.patch(self.strikethrough);
        let underlined = default.patch(self.underlined);

        let code = default.patch(self.code);
        let code_block = code.patch(self.code_block.stylable);

        let blockquote_line = self.blockquote.line.unwrap_or_default();
        let code_block_line = self.code_block.line.unwrap_or_default().into();
        let ruler_line = self.ruler.line.unwrap_or_default();
        let table_line = self.table.line.unwrap_or_default();

        ThemeMessagesValues {
            default,
            blockquote,
            blockquote_line,
            code,
            code_block,
            code_block_line,
            ruler,
            ruler_line,
            table,
            table_line,
            emphasis,
            strong,
            strikethrough,
            underlined,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeMessagesValues {
    pub default: Style,
    pub blockquote: Style,
    pub blockquote_line: LineName,
    pub code: Style,
    pub code_block: Style,
    pub code_block_line: BorderType,
    pub ruler: Style,
    pub ruler_line: LineName,
    pub table: Style,
    pub table_line: LineName,
    pub emphasis: Style,
    pub strong: Style,
    pub strikethrough: Style,
    pub underlined: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeRooms {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    labels: Stylable,

    #[serde(default)]
    unread: ThemeRoomsUnreads,

    #[serde(default)]
    notification: ThemeRoomsUnreads,

    #[serde(default)]
    mention: ThemeRoomsUnreads,

    #[serde(default)]
    marked_unread: ThemeRoomsUnreads,
}

impl ThemeRooms {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            labels: self.labels.merge(other.labels),
            unread: self.unread.merge(other.unread),
            notification: self.notification.merge(other.notification),
            mention: self.mention.merge(other.mention),
            marked_unread: self.marked_unread.merge(other.marked_unread),
        }
    }

    fn values(self, base: Style) -> ThemeRoomsValues {
        let default = base.patch(self.default);
        let labels = default.patch(self.labels);
        let unread = self.unread.values(default, labels);
        let notification = self.notification.values(default, labels);
        let mention = self.mention.values(default, labels);
        let marked_unread = self.marked_unread.values(default, labels);

        ThemeRoomsValues {
            default,
            labels,
            unread,
            notification,
            mention,
            marked_unread,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeRoomsValues {
    pub default: Style,
    pub labels: Style,
    pub unread: ThemeRoomsUnreadsValues,
    pub notification: ThemeRoomsUnreadsValues,
    pub mention: ThemeRoomsUnreadsValues,
    pub marked_unread: ThemeRoomsUnreadsValues,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeRoomsUnreads {
    #[serde(default)]
    name: Stylable,

    #[serde(default)]
    number: Stylable,

    #[serde(default)]
    labels: Stylable,
}

impl ThemeRoomsUnreads {
    fn merge(self, other: Self) -> Self {
        Self {
            name: self.name.merge(other.name),
            number: self.number.merge(other.number),
            labels: self.labels.merge(other.labels),
        }
    }

    fn values(self, base: Style, labels_base: Style) -> ThemeRoomsUnreadsValues {
        let name = base.patch(self.name);
        let number = base.patch(self.number);
        let labels = labels_base.patch(self.labels);

        ThemeRoomsUnreadsValues { name, number, labels }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeRoomsUnreadsValues {
    pub name: Style,
    pub number: Style,
    pub labels: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeTabs {
    #[serde(default)]
    title: Stylable,

    #[serde(default)]
    title_focused: Stylable,
}

impl ThemeTabs {
    fn merge(self, other: Self) -> Self {
        Self {
            title: self.title.merge(other.title),
            title_focused: self.title_focused.merge(other.title_focused),
        }
    }

    fn values(self, base: Style) -> ThemeTabsValues {
        let title = base.patch(self.title);
        let title_focused = title.patch(self.title_focused);

        ThemeTabsValues { title, title_focused }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeTabsValues {
    pub title: Style,
    pub title_focused: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeEncryption {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    icon_encrypted: Stylable,

    #[serde(default)]
    icon_unencrypted: Stylable,

    #[serde(default)]
    icon_unknown: Stylable,
}

impl ThemeEncryption {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            icon_encrypted: self.icon_encrypted.merge(other.icon_encrypted),
            icon_unencrypted: self.icon_unencrypted.merge(other.icon_unencrypted),
            icon_unknown: self.icon_unknown.merge(other.icon_unknown),
        }
    }

    fn values(self, base: Style) -> ThemeEncryptionValues {
        let default = base.patch(self.default);
        let icon_encrypted = default.patch(self.icon_encrypted);
        let icon_unencrypted = default.patch(self.icon_unencrypted);
        let icon_unknown = default.patch(self.icon_unknown);

        ThemeEncryptionValues {
            default,
            icon_encrypted,
            icon_unencrypted,
            icon_unknown,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeEncryptionValues {
    pub default: Style,
    pub icon_encrypted: Style,
    pub icon_unencrypted: Style,
    pub icon_unknown: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeMessageBar {
    #[serde(default)]
    default: Stylable,
}

impl ThemeMessageBar {
    fn merge(self, other: Self) -> Self {
        Self { default: self.default.merge(other.default) }
    }

    fn values(self, base: Style) -> ThemeMessageBarValues {
        let default = base.patch(self.default);

        ThemeMessageBarValues { default }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeMessageBarValues {
    pub default: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeCommandBar {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    completions: Stylable,

    #[serde(default)]
    prompt: Stylable,

    #[serde(default)]
    info: Stylable,

    #[serde(default)]
    error: Stylable,
}

impl ThemeCommandBar {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            completions: self.completions.merge(other.completions),
            prompt: self.prompt.merge(other.prompt),
            info: self.info.merge(other.info),
            error: self.error.merge(other.error),
        }
    }

    fn values(self, base: Style) -> ThemeCommandBarValues {
        let default = base.patch(self.default);
        let completions = default.patch(self.completions);
        let prompt = default.patch(self.prompt);
        let info = default.patch(self.info);
        let error = default.patch(self.error);
        ThemeCommandBarValues { default, completions, prompt, info, error }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeCommandBarValues {
    pub default: Style,
    pub completions: Style,
    pub prompt: Style,
    pub info: Style,
    pub error: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeTimeline {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    date: Stylable,

    #[serde(default)]
    time: Stylable,

    #[serde(default)]
    state: Stylable,

    #[serde(default)]
    sticker: Stylable,

    #[serde(default)]
    poll: Stylable,

    #[serde(default)]
    notice: Stylable,

    #[serde(default)]
    redacted: Stylable,

    #[serde(default)]
    unread_marker: Stylable,
}

impl ThemeTimeline {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            date: self.date.merge(other.date),
            time: self.time.merge(other.time),
            state: self.state.merge(other.state),
            sticker: self.sticker.merge(other.sticker),
            poll: self.poll.merge(other.poll),
            notice: self.notice.merge(other.notice),
            redacted: self.redacted.merge(other.redacted),
            unread_marker: self.unread_marker.merge(other.unread_marker),
        }
    }

    fn values(self, base: Style) -> ThemeTimelineValues {
        let default = base.patch(self.default);
        let date = default.patch(self.date);
        let time = default.patch(self.time);
        let state = default.patch(self.state);
        let sticker = default.patch(self.sticker);
        let poll = default.patch(self.poll);
        let notice = default.patch(self.notice);
        let redacted = default.patch(self.redacted);
        let unread_marker = default.patch(self.unread_marker);

        ThemeTimelineValues {
            default,
            date,
            time,
            state,
            sticker,
            poll,
            notice,
            redacted,
            unread_marker,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeTimelineValues {
    pub default: Style,
    pub date: Style,
    pub time: Style,
    pub state: Style,
    pub sticker: Style,
    pub poll: Style,
    pub notice: Style,
    pub redacted: Style,
    pub unread_marker: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeCompletion {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    selected: Stylable,
}

impl ThemeCompletion {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            selected: self.selected.merge(other.selected),
        }
    }

    fn values(self, base: Style) -> ThemeCompletionValues {
        let default = base.patch(self.default);
        let selected = default.patch(self.selected);

        ThemeCompletionValues { default, selected }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeCompletionValues {
    pub default: Style,
    pub selected: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeWindows {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    border: LineStylable,

    #[serde(default)]
    border_focused: LineStylable,

    #[serde(default)]
    title: Stylable,
}

impl ThemeWindows {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            border: self.border.merge(other.border),
            border_focused: self.border_focused.merge(other.border_focused),
            title: self.title.merge(other.title),
        }
    }

    fn values(self, base: Style) -> ThemeWindowsValues {
        let default = base.patch(self.default);
        let border = default.patch(self.border.stylable);
        let border_focused = border.patch(self.border_focused.stylable);
        let title = default.patch(self.title);
        let border_line: BorderType = self.border.line.unwrap_or_default().into();
        let border_focused_line =
            self.border_focused.line.map(BorderType::from).unwrap_or(border_line);

        ThemeWindowsValues {
            default,
            border,
            border_line,
            border_focused,
            border_focused_line,
            title,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeWindowsValues {
    pub default: Style,
    pub border: Style,
    pub border_line: BorderType,
    pub border_focused: Style,
    pub border_focused_line: BorderType,
    pub title: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeUsers {
    colors: Option<Vec<Color>>,
    #[serde(flatten)]
    stylable: Stylable,
}

impl ThemeUsers {
    fn merge(self, other: Self) -> Self {
        Self {
            colors: self.colors.or(other.colors),
            stylable: self.stylable.merge(other.stylable),
        }
    }

    fn values(self, base: Style) -> ThemeUsersValues {
        let colors = self.colors.unwrap_or_default();
        let style = base.patch(self.stylable);

        ThemeUsersValues { colors, style }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeUsersValues {
    colors: Vec<Color>,
    style: Style,
}

impl ThemeUsersValues {
    pub fn color(&self, user_id: &str) -> Color {
        if self.colors.is_empty() {
            return self.style.fg.unwrap_or(Color::Reset);
        }

        let mut hasher = DefaultHasher::new();
        user_id.hash(&mut hasher);
        let color = hasher.finish() as usize % self.colors.len();
        self.colors[color]
    }

    pub fn style(&self, user_id: &str, explicit: Option<Color>) -> Style {
        let style = self.style;

        if let Some(c) = explicit {
            // Use explicit color from config file:
            style.fg(c)
        } else if self.colors.is_empty() {
            // User has explicitly set an empty array:
            style
        } else {
            // Hash on user ID to pick a color from the array:
            style.fg(self.color(user_id))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remove_modifier() {
        let res: Theme = serde_json::from_str(
            r#"
            {
                "default": {
                    "color":"red",
                    "modifiers":["bold"]
                },
                "windows":{
                    "border": {"color":"green"},
                    "border_focused": {"modifiers":["~bold"]}
                }
            }"#,
        )
        .unwrap();
        let values = res.values();
        assert_eq!(values.default, Style::default().fg(Color::Red).bold());
        assert_eq!(values.windows.border, Style::default().fg(Color::Green).bold());
        assert_eq!(values.windows.border_focused, Style::default().fg(Color::Green).not_bold());
        assert_eq!(values.windows.title, Style::default().fg(Color::Red).bold());
    }

    #[test]
    fn test_border_names() {
        let res: LineName = serde_json::from_str(r#""plain""#).unwrap();
        assert_eq!(res, LineName::Plain);

        let res: LineName = serde_json::from_str(r#""rounded""#).unwrap();
        assert_eq!(res, LineName::Rounded);

        let res: LineName = serde_json::from_str(r#""double-dashed-light""#).unwrap();
        assert_eq!(res, LineName::DoubleDashedLight);

        let res: LineName = serde_json::from_str(r#""double-dashed-heavy""#).unwrap();
        assert_eq!(res, LineName::DoubleDashedHeavy);
    }
}
