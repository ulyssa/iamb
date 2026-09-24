//! # Logic for parsing styling configuration into [Style].
use serde::Deserialize;

use crate::prelude::*;

pub fn default_theme() -> Theme {
    Theme {
        messages: ThemeMessages {
            code: Stylable { color: None, background: Some(Color::Indexed(236)) },
            ..Default::default()
        },
        ..Default::default()
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize)]
pub struct Stylable {
    pub background: Option<Color>,
    pub color: Option<Color>,
}

impl Stylable {
    fn merge(self, other: Self) -> Self {
        Self {
            background: self.background.or(other.background),
            color: self.color.or(other.color),
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

        style
    }
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct Theme {
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

    /// Configuration for styling windows.
    #[serde(default)]
    windows: ThemeWindows,
}

impl Theme {
    pub fn merge(self, other: Self) -> Self {
        Self {
            messages: self.messages.merge(other.messages),
            tabs: self.tabs.merge(other.tabs),
            timeline: self.timeline.merge(other.timeline),
            rooms: self.rooms.merge(other.rooms),
            windows: self.windows.merge(other.windows),
        }
    }

    pub fn values(self) -> ThemeValues {
        let base = Style::default();

        ThemeValues {
            messages: self.messages.values(base),
            timeline: self.timeline.values(base),
            rooms: self.rooms.values(base),
            tabs: self.tabs.values(base),
            windows: self.windows.values(base),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeValues {
    /// Styling specificaly for messages shown within a room's timeline.
    pub messages: ThemeMessagesValues,

    /// Styling for a room's timeline.
    pub timeline: ThemeTimelineValues,

    /// Styling for items within room lists.
    pub rooms: ThemeRoomsValues,

    /// Styling for rendering tabs.
    pub tabs: ThemeTabsValues,

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
}

impl ThemeMessages {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            code: self.code.merge(other.code),
            code_block: self.code_block.merge(other.code_block),
        }
    }

    fn values(self, base: Style) -> ThemeMessagesValues {
        let code = self.code.merge(self.default);
        let code_block = self.code_block.merge(code);

        ThemeMessagesValues {
            default: base.patch(self.default),
            code: base.patch(code),
            code_block: base.patch(code_block),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeMessagesValues {
    pub default: Style,
    pub code: Style,
    pub code_block: Style,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeRooms {
    #[serde(default)]
    default: Stylable,

    #[serde(default)]
    unread: Stylable,
}

impl ThemeRooms {
    fn merge(self, other: Self) -> Self {
        Self {
            default: self.default.merge(other.default),
            unread: self.unread.merge(other.unread),
        }
    }

    fn values(self, base: Style) -> ThemeRoomsValues {
        let unread = self.unread.merge(self.default);

        ThemeRoomsValues {
            default: base.patch(self.default),
            unread: base.patch(unread),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeRoomsValues {
    pub default: Style,
    pub unread: Style,
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
        let title_focused = self.title_focused.merge(self.title);

        ThemeTabsValues {
            title: base.patch(self.title),
            title_focused: base.patch(title_focused),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeTabsValues {
    pub title: Style,
    pub title_focused: Style,
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
        }
    }

    fn values(self, base: Style) -> ThemeTimelineValues {
        let base = base.patch(self.default);

        ThemeTimelineValues {
            default: base,
            date: base.patch(self.date),
            time: base.patch(self.time),
            state: base.patch(self.state),
            sticker: base.patch(self.sticker),
            poll: base.patch(self.poll),
            notice: base.patch(self.notice),
            redacted: base.patch(self.redacted),
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
}

#[derive(Clone, Debug, Default, Deserialize)]
struct ThemeWindows {
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
            border: self.border.merge(other.border),
            border_focused: self.border_focused.merge(other.border_focused),
            title: self.title.merge(other.title),
        }
    }

    fn values(self, base: Style) -> ThemeWindowsValues {
        ThemeWindowsValues {
            border: base.patch(self.border),
            border_focused: base.patch(self.border_focused.merge(self.border)),
            title: base.patch(self.title),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ThemeWindowsValues {
    pub border: Style,
    pub border_focused: Style,
    pub title: Style,
}
