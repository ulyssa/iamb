//! # Logic for loading and validating application configuration
use std::borrow::Cow;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap};
use std::env;
use std::fmt;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process;

use clap::Parser;
use matrix_sdk::authentication::matrix::MatrixSession;
use matrix_sdk::ruma::{OwnedDeviceId, OwnedRoomAliasId, OwnedRoomId, OwnedUserId, UserId};
use ratatui::style::{Color, Modifier as StyleModifier, Style};
use ratatui::text::Span;
use ratatui_image::picker::ProtocolType;
use serde::{de::Error as SerdeError, de::Visitor, Deserialize, Deserializer, Serialize};
use tracing::Level;
use url::Url;

use modalkit::{env::vim::VimMode, key::TerminalKey, keybindings::InputKey};

use super::base::{
    IambError,
    IambId,
    RoomInfo,
    SortColumn,
    SortFieldRoom,
    SortFieldUser,
    SortOrder,
};

type Macros = HashMap<VimModes, HashMap<Keys, Keys>>;

macro_rules! usage {
    ( $($args: tt)* ) => {
        println!($($args)*);
        process::exit(2);
    }
}

const DEFAULT_MEMBERS_SORT: [SortColumn<SortFieldUser>; 2] = [
    SortColumn(SortFieldUser::PowerLevel, SortOrder::Ascending),
    SortColumn(SortFieldUser::UserId, SortOrder::Ascending),
];

const DEFAULT_ROOM_SORT: [SortColumn<SortFieldRoom>; 5] = [
    SortColumn(SortFieldRoom::Favorite, SortOrder::Ascending),
    SortColumn(SortFieldRoom::Invite, SortOrder::Ascending),
    SortColumn(SortFieldRoom::LowPriority, SortOrder::Ascending),
    SortColumn(SortFieldRoom::Unread, SortOrder::Ascending),
    SortColumn(SortFieldRoom::Name, SortOrder::Ascending),
];

const DEFAULT_REQ_TIMEOUT: u64 = 120;

const COLORS: [Color; 13] = [
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
];

pub fn user_color(user: &str) -> Color {
    let mut hasher = DefaultHasher::new();
    user.hash(&mut hasher);
    let color = hasher.finish() as usize % COLORS.len();

    COLORS[color]
}

pub fn user_style_from_color(color: Color) -> Style {
    Style::default().fg(color).add_modifier(StyleModifier::BOLD)
}

fn is_profile_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '.' || c == '-'
}

fn default_true() -> bool {
    true
}

fn validate_profile_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    let mut chars = name.chars();

    if !chars.next().is_some_and(|c| c.is_ascii_alphanumeric()) {
        return false;
    }

    name.chars().all(is_profile_char)
}

fn validate_profile_names(names: &BTreeMap<String, ProfileConfig>) {
    for name in names.keys() {
        if validate_profile_name(name.as_str()) {
            continue;
        }

        usage!(
            "{:?} is not a valid profile name.\n\n\
            Profile names can only contain the characters \
            a-z, A-Z, and 0-9. Period (.) and hyphen (-) are allowed after the first character.",
            name
        );
    }
}

const VERSION: &str = match option_env!("VERGEN_GIT_SHA") {
    None => env!("CARGO_PKG_VERSION"),
    Some(_) => concat!(env!("CARGO_PKG_VERSION"), " (", env!("VERGEN_GIT_SHA"), ")"),
};

#[derive(Parser)]
#[clap(version = VERSION, about, long_about = None)]
#[clap(propagate_version = true)]
pub struct Iamb {
    #[clap(short = 'P', long, value_parser)]
    pub profile: Option<String>,

    #[clap(short = 'C', long, value_parser)]
    pub config_directory: Option<PathBuf>,
}

#[derive(thiserror::Error, Debug)]
pub enum ConfigError {
    #[error("Error reading configuration file: {0}")]
    IO(#[from] std::io::Error),

    #[error("Error loading configuration file: {0}")]
    Invalid(#[from] toml::de::Error),

    #[error("Error loading JSON configuration file: {0}")]
    InvalidJSON(#[from] serde_json::Error),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Keys(pub Vec<TerminalKey>, pub String);
pub struct KeysVisitor;

impl Visitor<'_> for KeysVisitor {
    type Value = Keys;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid Vim mode (e.g. \"normal\" or \"insert\")")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        match TerminalKey::from_macro_str(value) {
            Ok(keys) => Ok(Keys(keys, value.to_string())),
            Err(e) => Err(E::custom(format!("Could not parse key sequence: {e}"))),
        }
    }
}

impl<'de> Deserialize<'de> for Keys {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(KeysVisitor)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct VimModes(pub Vec<VimMode>);
pub struct VimModesVisitor;

impl Visitor<'_> for VimModesVisitor {
    type Value = VimModes;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid Vim mode (e.g. \"normal\" or \"insert\")")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        let mut modes = vec![];

        for mode in value.split('|') {
            let mode = match mode.to_ascii_lowercase().as_str() {
                "insert" | "i" => VimMode::Insert,
                "normal" | "n" => VimMode::Normal,
                "visual" | "v" => VimMode::Visual,
                "command" | "c" => VimMode::Command,
                "select" => VimMode::Select,
                "operator-pending" => VimMode::OperationPending,
                _ => return Err(E::custom("Could not parse into a Vim mode")),
            };

            modes.push(mode);
        }

        Ok(VimModes(modes))
    }
}

impl<'de> Deserialize<'de> for VimModes {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(VimModesVisitor)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LogLevel(pub Level);
pub struct LogLevelVisitor;

impl From<LogLevel> for Level {
    fn from(level: LogLevel) -> Level {
        level.0
    }
}

impl Visitor<'_> for LogLevelVisitor {
    type Value = LogLevel;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid log level (e.g. \"warn\" or \"debug\")")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        match value {
            "info" => Ok(LogLevel(Level::INFO)),
            "debug" => Ok(LogLevel(Level::DEBUG)),
            "warn" => Ok(LogLevel(Level::WARN)),
            "error" => Ok(LogLevel(Level::ERROR)),
            "trace" => Ok(LogLevel(Level::TRACE)),
            _ => Err(E::custom("Could not parse log level")),
        }
    }
}

impl<'de> Deserialize<'de> for LogLevel {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(LogLevelVisitor)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UserColor(pub Color);
pub struct UserColorVisitor;

impl Visitor<'_> for UserColorVisitor {
    type Value = UserColor;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid color")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        match value {
            "none" => Ok(UserColor(Color::Reset)),
            "red" => Ok(UserColor(Color::Red)),
            "black" => Ok(UserColor(Color::Black)),
            "green" => Ok(UserColor(Color::Green)),
            "yellow" => Ok(UserColor(Color::Yellow)),
            "blue" => Ok(UserColor(Color::Blue)),
            "magenta" => Ok(UserColor(Color::Magenta)),
            "cyan" => Ok(UserColor(Color::Cyan)),
            "gray" => Ok(UserColor(Color::Gray)),
            "dark-gray" => Ok(UserColor(Color::DarkGray)),
            "light-red" => Ok(UserColor(Color::LightRed)),
            "light-green" => Ok(UserColor(Color::LightGreen)),
            "light-yellow" => Ok(UserColor(Color::LightYellow)),
            "light-blue" => Ok(UserColor(Color::LightBlue)),
            "light-magenta" => Ok(UserColor(Color::LightMagenta)),
            "light-cyan" => Ok(UserColor(Color::LightCyan)),
            "white" => Ok(UserColor(Color::White)),
            _ => Err(E::custom("Could not parse color")),
        }
    }
}

impl<'de> Deserialize<'de> for UserColor {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(UserColorVisitor)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Session {
    access_token: String,
    refresh_token: Option<String>,
    user_id: OwnedUserId,
    device_id: OwnedDeviceId,
}

impl From<Session> for MatrixSession {
    fn from(session: Session) -> Self {
        MatrixSession {
            tokens: matrix_sdk::authentication::SessionTokens {
                access_token: session.access_token,
                refresh_token: session.refresh_token,
            },
            meta: matrix_sdk::SessionMeta {
                user_id: session.user_id,
                device_id: session.device_id,
            },
        }
    }
}

impl From<MatrixSession> for Session {
    fn from(session: MatrixSession) -> Self {
        Session {
            access_token: session.tokens.access_token,
            refresh_token: session.tokens.refresh_token,
            user_id: session.meta.user_id,
            device_id: session.meta.device_id,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
pub struct UserDisplayTunables {
    pub color: Option<UserColor>,
    pub name: Option<String>,
}

pub type UserOverrides = HashMap<OwnedUserId, UserDisplayTunables>;

fn merge_sorts(profile: SortOverrides, global: SortOverrides) -> SortOverrides {
    SortOverrides {
        chats: profile.chats.or(global.chats),
        dms: profile.dms.or(global.dms),
        rooms: profile.rooms.or(global.rooms),
        spaces: profile.spaces.or(global.spaces),
        members: profile.members.or(global.members),
    }
}

fn merge_maps<K, V>(
    profile: Option<HashMap<K, V>>,
    global: Option<HashMap<K, V>>,
) -> Option<HashMap<K, V>>
where
    K: Eq + Hash,
{
    match (global, profile) {
        (Some(m), None) | (None, Some(m)) => Some(m),
        (Some(mut global), Some(profile)) => {
            for (k, v) in profile {
                global.insert(k, v);
            }

            Some(global)
        },
        (None, None) => None,
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum UserDisplayStyle {
    // The Matrix username for the sender (e.g., "@user:example.com").
    #[default]
    Username,

    // The localpart of the Matrix username (e.g., "@user").
    LocalPart,

    // The display name for the Matrix user, calculated according to the rules from the spec.
    //
    // This is usually something like "Ada Lovelace" if the user has configured a display name, but
    // it can wind up being the Matrix username if there are display name collisions in the room,
    // in order to avoid any confusion.
    DisplayName,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NotifyVia {
    /// Deliver notifications via terminal bell.
    pub bell: bool,
    /// Deliver notifications via desktop mechanism.
    #[cfg(feature = "desktop")]
    pub desktop: bool,
}
pub struct NotifyViaVisitor;

impl Default for NotifyVia {
    fn default() -> Self {
        Self {
            bell: cfg!(not(feature = "desktop")),
            #[cfg(feature = "desktop")]
            desktop: true,
        }
    }
}

impl Visitor<'_> for NotifyViaVisitor {
    type Value = NotifyVia;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid notify destination (e.g. \"bell\" or \"desktop\")")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: SerdeError,
    {
        let mut via = NotifyVia {
            bell: false,
            #[cfg(feature = "desktop")]
            desktop: false,
        };

        for value in value.split('|') {
            match value.to_ascii_lowercase().as_str() {
                "bell" => {
                    via.bell = true;
                },
                #[cfg(feature = "desktop")]
                "desktop" => {
                    via.desktop = true;
                },
                #[cfg(not(feature = "desktop"))]
                "desktop" => {
                    return Err(E::custom("desktop notification support was compiled out"))
                },
                _ => return Err(E::custom("could not parse into a notify destination")),
            };
        }

        Ok(via)
    }
}

impl<'de> Deserialize<'de> for NotifyVia {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(NotifyViaVisitor)
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
pub struct Mouse {
    #[serde(default)]
    pub enabled: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
pub struct Notifications {
    #[serde(default)]
    pub enabled: bool,
    #[serde(default)]
    pub via: NotifyVia,
    #[serde(default = "default_true")]
    pub show_message: bool,
    #[serde(default)]
    pub sound_hint: Option<String>,
}

#[derive(Clone)]
pub struct ImagePreviewValues {
    pub size: ImagePreviewSize,
    pub protocol: Option<ImagePreviewProtocolValues>,
}

#[derive(Clone, Default, Deserialize)]
pub struct ImagePreview {
    pub size: Option<ImagePreviewSize>,
    pub protocol: Option<ImagePreviewProtocolValues>,
}

impl ImagePreview {
    fn values(self) -> ImagePreviewValues {
        ImagePreviewValues {
            size: self.size.unwrap_or_default(),
            protocol: self.protocol,
        }
    }
}

#[derive(Clone, Deserialize)]
pub struct ImagePreviewSize {
    pub width: usize,
    pub height: usize,
}

impl Default for ImagePreviewSize {
    fn default() -> Self {
        ImagePreviewSize { width: 66, height: 10 }
    }
}

#[derive(Clone, Deserialize)]
pub struct ImagePreviewProtocolValues {
    pub r#type: Option<ProtocolType>,
    pub font_size: Option<(u16, u16)>,
}

#[derive(Clone)]
pub struct SortValues {
    pub chats: Vec<SortColumn<SortFieldRoom>>,
    pub dms: Vec<SortColumn<SortFieldRoom>>,
    pub rooms: Vec<SortColumn<SortFieldRoom>>,
    pub spaces: Vec<SortColumn<SortFieldRoom>>,
    pub members: Vec<SortColumn<SortFieldUser>>,
}

#[derive(Clone, Default, Deserialize)]
pub struct SortOverrides {
    pub chats: Option<Vec<SortColumn<SortFieldRoom>>>,
    pub dms: Option<Vec<SortColumn<SortFieldRoom>>>,
    pub rooms: Option<Vec<SortColumn<SortFieldRoom>>>,
    pub spaces: Option<Vec<SortColumn<SortFieldRoom>>>,
    pub members: Option<Vec<SortColumn<SortFieldUser>>>,
}

impl SortOverrides {
    pub fn values(self) -> SortValues {
        let rooms = self.rooms.unwrap_or_else(|| Vec::from(DEFAULT_ROOM_SORT));
        let chats = self.chats.unwrap_or_else(|| rooms.clone());
        let dms = self.dms.unwrap_or_else(|| rooms.clone());
        let spaces = self.spaces.unwrap_or_else(|| rooms.clone());
        let members = self.members.unwrap_or_else(|| Vec::from(DEFAULT_MEMBERS_SORT));

        SortValues { rooms, members, chats, dms, spaces }
    }
}

#[derive(Clone)]
pub struct TunableValues {
    pub log_level: Level,
    pub message_shortcode_display: bool,
    pub normal_after_send: bool,
    pub reaction_display: bool,
    pub reaction_shortcode_display: bool,
    pub read_receipt_send: bool,
    pub read_receipt_display: bool,
    pub request_timeout: u64,
    pub sort: SortValues,
    pub state_event_display: bool,
    pub typing_notice_send: bool,
    pub typing_notice_display: bool,
    pub users: UserOverrides,
    pub username_display: UserDisplayStyle,
    pub message_user_color: bool,
    pub default_room: Option<String>,
    pub open_command: Option<Vec<String>>,
    pub mouse: Mouse,
    pub notifications: Notifications,
    pub image_preview: Option<ImagePreviewValues>,
    pub user_gutter_width: usize,
    pub external_edit_file_suffix: String,
    pub tabstop: usize,
}

#[derive(Clone, Default, Deserialize)]
pub struct Tunables {
    pub log_level: Option<LogLevel>,
    pub message_shortcode_display: Option<bool>,
    pub normal_after_send: Option<bool>,
    pub reaction_display: Option<bool>,
    pub reaction_shortcode_display: Option<bool>,
    pub read_receipt_send: Option<bool>,
    pub read_receipt_display: Option<bool>,
    pub request_timeout: Option<u64>,
    #[serde(default)]
    pub sort: SortOverrides,
    pub state_event_display: Option<bool>,
    pub typing_notice_send: Option<bool>,
    pub typing_notice_display: Option<bool>,
    pub users: Option<UserOverrides>,
    pub username_display: Option<UserDisplayStyle>,
    pub message_user_color: Option<bool>,
    pub default_room: Option<String>,
    pub open_command: Option<Vec<String>>,
    pub mouse: Option<Mouse>,
    pub notifications: Option<Notifications>,
    pub image_preview: Option<ImagePreview>,
    pub user_gutter_width: Option<usize>,
    pub external_edit_file_suffix: Option<String>,
    pub tabstop: Option<usize>,
}

impl Tunables {
    fn merge(self, other: Self) -> Self {
        Tunables {
            log_level: self.log_level.or(other.log_level),
            message_shortcode_display: self
                .message_shortcode_display
                .or(other.message_shortcode_display),
            normal_after_send: self.normal_after_send.or(other.normal_after_send),
            reaction_display: self.reaction_display.or(other.reaction_display),
            reaction_shortcode_display: self
                .reaction_shortcode_display
                .or(other.reaction_shortcode_display),
            read_receipt_send: self.read_receipt_send.or(other.read_receipt_send),
            read_receipt_display: self.read_receipt_display.or(other.read_receipt_display),
            request_timeout: self.request_timeout.or(other.request_timeout),
            sort: merge_sorts(self.sort, other.sort),
            state_event_display: self.state_event_display.or(other.state_event_display),
            typing_notice_send: self.typing_notice_send.or(other.typing_notice_send),
            typing_notice_display: self.typing_notice_display.or(other.typing_notice_display),
            users: merge_maps(self.users, other.users),
            username_display: self.username_display.or(other.username_display),
            message_user_color: self.message_user_color.or(other.message_user_color),
            default_room: self.default_room.or(other.default_room),
            open_command: self.open_command.or(other.open_command),
            mouse: self.mouse.or(other.mouse),
            notifications: self.notifications.or(other.notifications),
            image_preview: self.image_preview.or(other.image_preview),
            user_gutter_width: self.user_gutter_width.or(other.user_gutter_width),
            external_edit_file_suffix: self
                .external_edit_file_suffix
                .or(other.external_edit_file_suffix),
            tabstop: self.tabstop.or(other.tabstop),
        }
    }

    fn values(self) -> TunableValues {
        TunableValues {
            log_level: self.log_level.map(Level::from).unwrap_or(Level::INFO),
            message_shortcode_display: self.message_shortcode_display.unwrap_or(false),
            normal_after_send: self.normal_after_send.unwrap_or(false),
            reaction_display: self.reaction_display.unwrap_or(true),
            reaction_shortcode_display: self.reaction_shortcode_display.unwrap_or(false),
            read_receipt_send: self.read_receipt_send.unwrap_or(true),
            read_receipt_display: self.read_receipt_display.unwrap_or(true),
            request_timeout: self.request_timeout.unwrap_or(DEFAULT_REQ_TIMEOUT),
            sort: self.sort.values(),
            state_event_display: self.state_event_display.unwrap_or(true),
            typing_notice_send: self.typing_notice_send.unwrap_or(true),
            typing_notice_display: self.typing_notice_display.unwrap_or(true),
            users: self.users.unwrap_or_default(),
            username_display: self.username_display.unwrap_or_default(),
            message_user_color: self.message_user_color.unwrap_or(false),
            default_room: self.default_room,
            open_command: self.open_command,
            mouse: self.mouse.unwrap_or_default(),
            notifications: self.notifications.unwrap_or_default(),
            image_preview: self.image_preview.map(ImagePreview::values),
            user_gutter_width: self.user_gutter_width.unwrap_or(30),
            external_edit_file_suffix: self
                .external_edit_file_suffix
                .unwrap_or_else(|| ".md".to_string()),
            tabstop: self.tabstop.unwrap_or(4),
        }
    }
}

#[derive(Clone)]
pub struct DirectoryValues {
    pub cache: PathBuf,
    pub data: PathBuf,
    pub logs: PathBuf,
    pub downloads: Option<PathBuf>,
    pub image_previews: PathBuf,
}

impl DirectoryValues {
    fn create_dir_all(&self) -> std::io::Result<()> {
        use std::fs::create_dir_all;

        let Self { cache, data, logs, downloads, image_previews } = self;

        create_dir_all(cache)?;
        create_dir_all(data)?;
        create_dir_all(logs)?;
        create_dir_all(image_previews)?;

        if let Some(downloads) = downloads {
            create_dir_all(downloads)?;
        }

        Ok(())
    }
}

#[derive(Clone, Default, Deserialize)]
pub struct Directories {
    pub cache: Option<String>,
    pub data: Option<String>,
    pub logs: Option<String>,
    pub downloads: Option<String>,
    pub image_previews: Option<String>,
}

impl Directories {
    fn merge(self, other: Self) -> Self {
        Directories {
            cache: self.cache.or(other.cache),
            data: self.data.or(other.data),
            logs: self.logs.or(other.logs),
            downloads: self.downloads.or(other.downloads),
            image_previews: self.image_previews.or(other.image_previews),
        }
    }

    fn values(self) -> DirectoryValues {
        let cache = self
            .cache
            .map(|dir| {
                let dir = shellexpand::full(&dir)
                    .expect("unable to expand shell variables in dirs.cache");
                Path::new(dir.as_ref()).to_owned()
            })
            .or_else(|| {
                let mut dir = dirs::cache_dir()?;
                dir.push("iamb");
                dir.into()
            })
            .expect("no dirs.cache value configured!");

        let data = self
            .data
            .map(|dir| {
                let dir = shellexpand::full(&dir)
                    .expect("unable to expand shell variables in dirs.cache");
                Path::new(dir.as_ref()).to_owned()
            })
            .or_else(|| {
                let mut dir = dirs::data_dir()?;
                dir.push("iamb");
                dir.into()
            })
            .expect("no dirs.data value configured!");

        let logs = self
            .logs
            .map(|dir| {
                let dir = shellexpand::full(&dir)
                    .expect("unable to expand shell variables in dirs.cache");
                Path::new(dir.as_ref()).to_owned()
            })
            .unwrap_or_else(|| {
                let mut dir = cache.clone();
                dir.push("logs");
                dir
            });

        let downloads = self
            .downloads
            .map(|dir| {
                let dir = shellexpand::full(&dir)
                    .expect("unable to expand shell variables in dirs.cache");
                Path::new(dir.as_ref()).to_owned()
            })
            .or_else(dirs::download_dir);

        let image_previews = self
            .image_previews
            .map(|dir| {
                let dir = shellexpand::full(&dir)
                    .expect("unable to expand shell variables in dirs.cache");
                Path::new(dir.as_ref()).to_owned()
            })
            .unwrap_or_else(|| {
                let mut dir = cache.clone();
                dir.push("image_preview_downloads");
                dir
            });

        DirectoryValues { cache, data, logs, downloads, image_previews }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(untagged)]
pub enum WindowPath {
    AliasId(OwnedRoomAliasId),
    RoomId(OwnedRoomId),
    UserId(OwnedUserId),
    Window(IambId),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(untagged, deny_unknown_fields)]
pub enum WindowLayout {
    Window { window: WindowPath },
    Split { split: Vec<WindowLayout> },
}

#[derive(Clone, Default, Deserialize)]
pub struct ProxyConfig {
    /// Proxy URL (e.g. socks5://127.0.0.1:1080). Requires reqwest's socks feature.
    pub url: Option<String>,
}

impl ProxyConfig {
    fn merge(self, other: Self) -> Self {
        ProxyConfig {
            url: self.url.or(other.url),
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "lowercase", tag = "style")]
pub enum Layout {
    /// Restore the layout from the previous session.
    #[default]
    Restore,

    /// Open a single window using the `default_room` value.
    New,

    /// Open the window layouts described under `tabs`.
    Config { tabs: Vec<WindowLayout> },
}

#[derive(Clone, Deserialize)]
pub struct ProfileConfig {
    pub user_id: OwnedUserId,
    pub url: Option<Url>,
    pub settings: Option<Tunables>,
    pub dirs: Option<Directories>,
    pub layout: Option<Layout>,
    pub macros: Option<Macros>,
    pub proxy: Option<ProxyConfig>,
}

#[derive(Clone, Deserialize)]
pub struct IambConfig {
    pub profiles: BTreeMap<String, ProfileConfig>,
    pub default_profile: Option<String>,
    pub settings: Option<Tunables>,
    pub dirs: Option<Directories>,
    pub layout: Option<Layout>,
    pub macros: Option<Macros>,
    pub proxy: Option<ProxyConfig>,
}

impl IambConfig {
    pub fn load_toml(path: &Path) -> Result<Self, ConfigError> {
        let s = std::fs::read_to_string(path)?;
        let config = toml::from_str(&s)?;

        Ok(config)
    }

    pub fn load_json(path: &Path) -> Result<Self, ConfigError> {
        let s = std::fs::read_to_string(path)?;
        let config = serde_json::from_str(&s)?;

        Ok(config)
    }
}

#[derive(Clone)]
pub struct ApplicationSettings {
    pub layout_json: PathBuf,
    pub session_json: PathBuf,
    pub session_json_old: PathBuf,
    pub sled_dir: PathBuf,
    pub sqlite_dir: PathBuf,
    pub profile_name: String,
    pub profile: ProfileConfig,
    pub tunables: TunableValues,
    pub dirs: DirectoryValues,
    pub layout: Layout,
    pub macros: Macros,
    /// Proxy URL for the HTTP client (e.g. socks5://127.0.0.1:1080), from [proxy] url.
    pub proxy_url: Option<String>,
}

impl ApplicationSettings {
    fn get_xdg_config_home() -> Option<PathBuf> {
        env::var("XDG_CONFIG_HOME").ok().map(PathBuf::from)
    }

    pub fn load(cli: Iamb) -> Result<Self, Box<dyn std::error::Error>> {
        let mut config_dir = cli
            .config_directory
            .or_else(Self::get_xdg_config_home)
            .or_else(dirs::config_dir)
            .unwrap_or_else(|| {
                usage!(
                    "No user configuration directory found;\
                    please specify one via -C.\n\n
                    For more information try '--help'"
                );
            });

        config_dir.push("iamb");
        let config_json = config_dir.join("config.json");
        let config_toml = config_dir.join("config.toml");

        let config = if config_toml.is_file() {
            IambConfig::load_toml(config_toml.as_path())?
        } else if config_json.is_file() {
            IambConfig::load_json(config_json.as_path())?
        } else {
            usage!(
                "Please create a configuration file at {}\n\n\
                For more information try '--help'",
                config_toml.display(),
            );
        };

        let IambConfig {
            mut profiles,
            default_profile,
            dirs,
            settings: global,
            layout,
            macros,
            proxy: global_proxy,
        } = config;

        validate_profile_names(&profiles);

        let (profile_name, mut profile) = if let Some(profile) = cli.profile.or(default_profile) {
            profiles.remove_entry(&profile).unwrap_or_else(|| {
                usage!(
                    "No configured profile with the name {:?} in {}",
                    profile,
                    config_json.display()
                );
            })
        } else if profiles.len() == 1 {
            profiles.into_iter().next().unwrap()
        } else {
            loop {
                println!("\nNo profile specified. Available profiles:");
                profiles.keys().enumerate().for_each(|(i, name)| println!("{i}: {name}"));

                print!("Select a number or 'q' to quit: ");
                let _ = std::io::stdout().flush();

                let mut input = String::new();
                let _ = std::io::stdin().read_line(&mut input);

                if input.trim() == "q" {
                    usage!(
                        "No profile specified. \
                        Please use -P or add \"default_profile\" to your configuration.\n\n\
                        For more information try '--help'",
                    );
                }
                if let Ok(i) = input.trim().parse::<usize>() {
                    if i < profiles.len() {
                        break profiles.into_iter().nth(i).unwrap();
                    }
                }
                println!("\nInvalid index.");
            }
        };

        let macros = merge_maps(profile.macros.take(), macros).unwrap_or_default();
        let layout = profile.layout.take().or(layout).unwrap_or_default();
        let proxy_url = profile.proxy.take().unwrap_or_default().merge(global_proxy.unwrap_or_default()).url;

        let tunables = global.unwrap_or_default();
        let tunables = profile.settings.take().unwrap_or_default().merge(tunables);
        let tunables = tunables.values();

        let dirs = dirs.unwrap_or_default();
        let dirs = profile.dirs.take().unwrap_or_default().merge(dirs);
        let dirs = dirs.values();

        // Create directories
        dirs.create_dir_all()?;

        // Set up paths that live inside the profile's data directory.
        let mut profile_dir = config_dir.clone();
        profile_dir.push("profiles");
        profile_dir.push(profile_name.as_str());

        let mut profile_data_dir = dirs.data.clone();
        profile_data_dir.push("profiles");
        profile_data_dir.push(profile_name.as_str());

        let mut sled_dir = profile_dir.clone();
        sled_dir.push("matrix");

        let mut sqlite_dir = profile_data_dir.clone();
        sqlite_dir.push("sqlite");

        let mut session_json = profile_data_dir.clone();
        session_json.push("session.json");

        let mut session_json_old = profile_dir;
        session_json_old.push("session.json");

        // Set up paths that live inside the profile's cache directory.
        let mut cache_dir = dirs.cache.clone();
        cache_dir.push("profiles");
        cache_dir.push(profile_name.as_str());

        let mut layout_json = cache_dir.clone();
        layout_json.push("layout.json");

        let settings = ApplicationSettings {
            sled_dir,
            layout_json,
            session_json,
            session_json_old,
            sqlite_dir,
            profile_name,
            profile,
            tunables,
            dirs,
            layout,
            macros,
            proxy_url,
        };

        Ok(settings)
    }

    pub fn read_session(&self, path: impl AsRef<Path>) -> Result<Session, IambError> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        let session = serde_json::from_reader(reader).map_err(IambError::from)?;
        Ok(session)
    }

    pub fn write_session(&self, session: MatrixSession) -> Result<(), IambError> {
        let file = File::create(self.session_json.as_path())?;
        let writer = BufWriter::new(file);
        let session = Session::from(session);
        serde_json::to_writer(writer, &session).map_err(IambError::from)?;
        Ok(())
    }

    pub fn get_user_char_span(&self, user_id: &UserId) -> Span<'_> {
        let (color, c) = self
            .tunables
            .users
            .get(user_id)
            .map(|user| {
                (
                    user.color.as_ref().map(|c| c.0),
                    user.name.as_ref().and_then(|s| s.chars().next()),
                )
            })
            .unwrap_or_default();

        let color = color.unwrap_or_else(|| user_color(user_id.as_str()));
        let style = user_style_from_color(color);

        let c = c.unwrap_or_else(|| user_id.localpart().chars().next().unwrap_or(' '));

        Span::styled(String::from(c), style)
    }

    pub fn get_user_overrides(
        &self,
        user_id: &UserId,
    ) -> (Option<Color>, Option<Cow<'static, str>>) {
        self.tunables
            .users
            .get(user_id)
            .map(|user| (user.color.as_ref().map(|c| c.0), user.name.clone().map(Cow::Owned)))
            .unwrap_or_default()
    }

    pub fn get_user_color(&self, user_id: &UserId) -> Color {
        self.tunables
            .users
            .get(user_id)
            .and_then(|user| user.color.as_ref().map(|c| c.0))
            .unwrap_or_else(|| user_color(user_id.as_str()))
    }

    pub fn get_user_style(&self, user_id: &UserId) -> Style {
        user_style_from_color(self.get_user_color(user_id))
    }

    pub fn get_user_span<'a>(&self, user_id: &'a UserId, info: &'a RoomInfo) -> Span<'a> {
        let (color, name) = self.get_user_overrides(user_id);

        let color = color.unwrap_or_else(|| user_color(user_id.as_str()));
        let style = user_style_from_color(color);
        let name = match (name, &self.tunables.username_display) {
            (Some(name), _) => name,
            (None, UserDisplayStyle::Username) => Cow::Borrowed(user_id.as_str()),
            (None, UserDisplayStyle::LocalPart) => Cow::Borrowed(user_id.localpart()),
            (None, UserDisplayStyle::DisplayName) => {
                if let Some(display) = info.display_names.get(user_id) {
                    Cow::Borrowed(display.as_str())
                } else {
                    Cow::Borrowed(user_id.as_str())
                }
            },
        };

        Span::styled(name, style)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use matrix_sdk::ruma::user_id;
    use std::convert::TryFrom;

    #[test]
    fn test_profile_name_invalid() {
        assert_eq!(validate_profile_name(""), false);
        assert_eq!(validate_profile_name(" "), false);
        assert_eq!(validate_profile_name("a b"), false);
        assert_eq!(validate_profile_name("foo^bar"), false);
        assert_eq!(validate_profile_name("FOO/BAR"), false);
        assert_eq!(validate_profile_name("-b-c"), false);
        assert_eq!(validate_profile_name("-B-c"), false);
        assert_eq!(validate_profile_name(".b-c"), false);
        assert_eq!(validate_profile_name(".B-c"), false);
    }

    #[test]
    fn test_profile_name_valid() {
        assert_eq!(validate_profile_name("foo"), true);
        assert_eq!(validate_profile_name("FOO"), true);
        assert_eq!(validate_profile_name("a-b-c"), true);
        assert_eq!(validate_profile_name("a-B-c"), true);
        assert_eq!(validate_profile_name("a.b-c"), true);
        assert_eq!(validate_profile_name("a.B-c"), true);
    }

    #[test]
    fn test_merge_users() {
        let a = None;
        let b = vec![(user_id!("@a:b.c").to_owned(), UserDisplayTunables {
            color: Some(UserColor(Color::Red)),
            name: Some("Hello".into()),
        })]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let c = vec![(user_id!("@a:b.c").to_owned(), UserDisplayTunables {
            color: Some(UserColor(Color::Green)),
            name: Some("World".into()),
        })]
        .into_iter()
        .collect::<HashMap<_, _>>();

        let res = merge_maps(a.clone(), a.clone());
        assert_eq!(res, None);

        let res = merge_maps(a.clone(), Some(b.clone()));
        assert_eq!(res, Some(b.clone()));

        let res = merge_maps(Some(b.clone()), a.clone());
        assert_eq!(res, Some(b.clone()));

        let res = merge_maps(Some(b.clone()), Some(b.clone()));
        assert_eq!(res, Some(b.clone()));

        let res = merge_maps(Some(b.clone()), Some(c.clone()));
        assert_eq!(res, Some(b.clone()));

        let res = merge_maps(Some(c.clone()), Some(b.clone()));
        assert_eq!(res, Some(c.clone()));
    }

    #[test]
    fn test_parse_tunables() {
        let res: Tunables = serde_json::from_str("{}").unwrap();
        assert_eq!(res.typing_notice_send, None);
        assert_eq!(res.typing_notice_display, None);
        assert_eq!(res.users, None);

        let res: Tunables = serde_json::from_str("{\"typing_notice_send\": true}").unwrap();
        assert_eq!(res.typing_notice_send, Some(true));
        assert_eq!(res.typing_notice_display, None);
        assert_eq!(res.users, None);

        let res: Tunables = serde_json::from_str("{\"typing_notice_send\": false}").unwrap();
        assert_eq!(res.typing_notice_send, Some(false));
        assert_eq!(res.typing_notice_display, None);
        assert_eq!(res.users, None);

        let res: Tunables = serde_json::from_str("{\"users\": {}}").unwrap();
        assert_eq!(res.typing_notice_send, None);
        assert_eq!(res.typing_notice_display, None);
        assert_eq!(res.users, Some(HashMap::new()));

        let res: Tunables = serde_json::from_str(
            "{\"users\": {\"@a:b.c\": {\"color\": \"black\", \"name\": \"Tim\"}}}",
        )
        .unwrap();
        assert_eq!(res.typing_notice_send, None);
        assert_eq!(res.typing_notice_display, None);
        let users = vec![(user_id!("@a:b.c").to_owned(), UserDisplayTunables {
            color: Some(UserColor(Color::Black)),
            name: Some("Tim".into()),
        })];
        assert_eq!(res.users, Some(users.into_iter().collect()));
    }

    #[test]
    fn test_parse_tunables_username_display() {
        let res: Tunables = serde_json::from_str("{\"username_display\": \"username\"}").unwrap();
        assert_eq!(res.username_display, Some(UserDisplayStyle::Username));

        let res: Tunables = serde_json::from_str("{\"username_display\": \"localpart\"}").unwrap();
        assert_eq!(res.username_display, Some(UserDisplayStyle::LocalPart));

        let res: Tunables =
            serde_json::from_str("{\"username_display\": \"displayname\"}").unwrap();
        assert_eq!(res.username_display, Some(UserDisplayStyle::DisplayName));
    }

    #[test]
    fn test_parse_tunables_sort() {
        let res: Tunables = serde_json::from_str(
            r#"{"sort": {"members": ["server","~localpart"],"spaces":["~favorite", "alias"]}}"#,
        )
        .unwrap();
        assert_eq!(
            res.sort.members,
            Some(vec![
                SortColumn(SortFieldUser::Server, SortOrder::Ascending),
                SortColumn(SortFieldUser::LocalPart, SortOrder::Descending),
            ])
        );
        assert_eq!(
            res.sort.spaces,
            Some(vec![
                SortColumn(SortFieldRoom::Favorite, SortOrder::Descending),
                SortColumn(SortFieldRoom::Alias, SortOrder::Ascending),
            ])
        );
        assert_eq!(res.sort.rooms, None);
        assert_eq!(res.sort.dms, None);

        // Check that we get the right default "rooms" and "dms" values.
        let res = res.values();
        assert_eq!(res.sort.members, vec![
            SortColumn(SortFieldUser::Server, SortOrder::Ascending),
            SortColumn(SortFieldUser::LocalPart, SortOrder::Descending),
        ]);
        assert_eq!(res.sort.spaces, vec![
            SortColumn(SortFieldRoom::Favorite, SortOrder::Descending),
            SortColumn(SortFieldRoom::Alias, SortOrder::Ascending),
        ]);
        assert_eq!(res.sort.rooms, Vec::from(DEFAULT_ROOM_SORT));
        assert_eq!(res.sort.dms, Vec::from(DEFAULT_ROOM_SORT));
    }

    #[test]
    fn test_parse_layout() {
        let user = WindowPath::UserId(user_id!("@user:example.com").to_owned());
        let alias = WindowPath::AliasId(OwnedRoomAliasId::try_from("#room:example.com").unwrap());
        let room = WindowPath::RoomId(OwnedRoomId::try_from("!room:example.com").unwrap());
        let dms = WindowPath::Window(IambId::DirectList);
        let welcome = WindowPath::Window(IambId::Welcome);

        let res: Layout = serde_json::from_str("{\"style\": \"restore\"}").unwrap();
        assert_eq!(res, Layout::Restore);

        let res: Layout = serde_json::from_str("{\"style\": \"new\"}").unwrap();
        assert_eq!(res, Layout::New);

        let res: Layout = serde_json::from_str(
            "{\"style\": \"config\", \"tabs\": [{\"window\":\"@user:example.com\"}]}",
        )
        .unwrap();
        assert_eq!(res, Layout::Config {
            tabs: vec![WindowLayout::Window { window: user.clone() }]
        });

        let res: Layout = serde_json::from_str(
            "{\
            \"style\": \"config\",\
            \"tabs\": [\
                {\"split\":[\
                    {\"window\":\"@user:example.com\"},\
                    {\"window\":\"#room:example.com\"}\
                ]},\
                {\"split\":[\
                    {\"window\":\"!room:example.com\"},\
                    {\"split\":[\
                        {\"window\":\"iamb://dms\"},\
                        {\"window\":\"iamb://welcome\"}\
                    ]}\
                ]}\
            ]}",
        )
        .unwrap();
        let split1 = WindowLayout::Split {
            split: vec![
                WindowLayout::Window { window: user.clone() },
                WindowLayout::Window { window: alias },
            ],
        };
        let split2 = WindowLayout::Split {
            split: vec![WindowLayout::Window { window: dms }, WindowLayout::Window {
                window: welcome,
            }],
        };
        let split3 = WindowLayout::Split {
            split: vec![WindowLayout::Window { window: room }, split2],
        };
        let tabs = vec![split1, split3];
        assert_eq!(res, Layout::Config { tabs });
    }

    #[test]
    fn test_parse_macros() {
        let res: Macros = serde_json::from_str("{\"i|c\":{\"jj\":\"<Esc>\"}}").unwrap();
        assert_eq!(res.len(), 1);

        let modes = VimModes(vec![VimMode::Insert, VimMode::Command]);
        let mapped = res.get(&modes).unwrap();
        assert_eq!(mapped.len(), 1);

        let j = "j".parse::<TerminalKey>().unwrap();
        let esc = "<Esc>".parse::<TerminalKey>().unwrap();

        let jj = Keys(vec![j, j], "jj".into());
        let run = mapped.get(&jj).unwrap();
        let exp = Keys(vec![esc], "<Esc>".into());
        assert_eq!(run, &exp);
    }

    #[test]
    fn test_parse_notify_via() {
        assert_eq!(NotifyVia { bell: false, desktop: true }, NotifyVia::default());
        assert_eq!(
            NotifyVia { bell: false, desktop: true },
            serde_json::from_str(r#""desktop""#).unwrap()
        );
        assert_eq!(
            NotifyVia { bell: true, desktop: false },
            serde_json::from_str(r#""bell""#).unwrap()
        );
        assert_eq!(
            NotifyVia { bell: true, desktop: true },
            serde_json::from_str(r#""bell|desktop""#).unwrap()
        );
        assert_eq!(
            NotifyVia { bell: true, desktop: true },
            serde_json::from_str(r#""desktop|bell""#).unwrap()
        );
        assert!(serde_json::from_str::<NotifyVia>(r#""other""#).is_err());
        assert!(serde_json::from_str::<NotifyVia>(r#""""#).is_err());
    }

    #[test]
    fn test_load_example_config_toml() {
        let path = PathBuf::from("config.example.toml");
        let config = IambConfig::load_toml(&path).expect("can load example_config.toml");

        let IambConfig {
            profiles,
            default_profile,
            settings,
            dirs,
            layout,
            macros,
        } = &config;

        // There should be an example object for each top-level field.
        assert!(!profiles.is_empty());
        assert!(default_profile.is_some());
        assert!(settings.is_some());
        assert!(dirs.is_some());
        assert!(layout.is_some());
        assert!(macros.is_some());
    }
}
