//! # Logic for parsing styling configuration into [Style].
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use serde::de::Error as SerdeError;
use serde::de::Visitor;
use serde::{Deserialize, Deserializer};

use crate::prelude::*;

pub fn default_theme() -> Theme {
    Theme {
        messages: ThemeMessages {
            code: Stylable::bg(Color::Indexed(236)),
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
            border: Stylable::with_modifiers(StyleModifier::DIM),
            border_focused: Stylable::without_modifiers(StyleModifier::DIM),
            title: Stylable::with_modifiers(StyleModifier::BOLD),
            ..Default::default()
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
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeMessages {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    code: Stylable,

    #[serde(default)]
    code_block: Stylable,

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
            code: self.code.merge(other.code),
            code_block: self.code_block.merge(other.code_block),
            emphasis: self.emphasis.merge(other.emphasis),
            strong: self.strong.merge(other.strong),
            strikethrough: self.strikethrough.merge(other.strikethrough),
            underlined: self.underlined.merge(other.underlined),
        }
    }

    fn values(self, base: Style) -> ThemeMessagesValues {
        let default = base.patch(self.default);
        let code = default.patch(self.code);
        let code_block = code.patch(self.code_block);
        let emphasis = default.patch(self.emphasis);
        let strong = default.patch(self.strong);
        let strikethrough = default.patch(self.strikethrough);
        let underlined = default.patch(self.underlined);

        ThemeMessagesValues {
            default,
            code,
            code_block,
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
    pub code: Style,
    pub code_block: Style,
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
            prompt: self.prompt.merge(other.prompt),
            info: self.info.merge(other.info),
            error: self.error.merge(other.error),
        }
    }

    fn values(self, base: Style) -> ThemeCommandBarValues {
        let default = base.patch(self.default);
        let prompt = default.patch(self.prompt);
        let info = default.patch(self.info);
        let error = default.patch(self.error);
        ThemeCommandBarValues { default, prompt, info, error }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeCommandBarValues {
    pub default: Style,
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
struct ThemeWindows {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    border: Stylable,

    #[serde(default)]
    border_focused: Stylable,

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
        let border = default.patch(self.border);
        let border_focused = border.patch(self.border_focused);
        let title = default.patch(self.title);

        ThemeWindowsValues { default, border, border_focused, title }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeWindowsValues {
    pub default: Style,
    pub border: Style,
    pub border_focused: Style,
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
}
