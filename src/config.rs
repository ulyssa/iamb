use std::borrow::Cow;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::process;

use clap::Parser;
use matrix_sdk::ruma::{OwnedUserId, UserId};
use serde::{de::Error as SerdeError, de::Visitor, Deserialize, Deserializer};
use tracing::Level;
use url::Url;

use modalkit::tui::{
    style::{Color, Modifier as StyleModifier, Style},
    text::Span,
};

macro_rules! usage {
    ( $($args: tt)* ) => {
        println!($($args)*);
        process::exit(2);
    }
}

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

fn validate_profile_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    let mut chars = name.chars();

    if !chars.next().map_or(false, |c| c.is_ascii_alphanumeric()) {
        return false;
    }

    name.chars().all(is_profile_char)
}

fn validate_profile_names(names: &HashMap<String, ProfileConfig>) {
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

#[derive(Parser)]
#[clap(version, about, long_about = None)]
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
    Invalid(#[from] serde_json::Error),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LogLevel(pub Level);
pub struct LogLevelVisitor;

impl From<LogLevel> for Level {
    fn from(level: LogLevel) -> Level {
        level.0
    }
}

impl<'de> Visitor<'de> for LogLevelVisitor {
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

impl<'de> Visitor<'de> for UserColorVisitor {
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

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq)]
pub struct UserDisplayTunables {
    pub color: Option<UserColor>,
    pub name: Option<String>,
}

pub type UserOverrides = HashMap<OwnedUserId, UserDisplayTunables>;

fn merge_users(a: Option<UserOverrides>, b: Option<UserOverrides>) -> Option<UserOverrides> {
    match (a, b) {
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (Some(mut a), Some(b)) => {
            for (k, v) in b {
                a.insert(k, v);
            }

            Some(a)
        },
        (None, None) => None,
    }
}

#[derive(Clone)]
pub struct TunableValues {
    pub log_level: Level,
    pub reaction_display: bool,
    pub reaction_shortcode_display: bool,
    pub read_receipt_send: bool,
    pub read_receipt_display: bool,
    pub request_timeout: u64,
    pub typing_notice_send: bool,
    pub typing_notice_display: bool,
    pub users: UserOverrides,
    pub default_room: Option<String>,
}

#[derive(Clone, Default, Deserialize)]
pub struct Tunables {
    pub log_level: Option<LogLevel>,
    pub reaction_display: Option<bool>,
    pub reaction_shortcode_display: Option<bool>,
    pub read_receipt_send: Option<bool>,
    pub read_receipt_display: Option<bool>,
    pub request_timeout: Option<u64>,
    pub typing_notice_send: Option<bool>,
    pub typing_notice_display: Option<bool>,
    pub users: Option<UserOverrides>,
    pub default_room: Option<String>,
}

impl Tunables {
    fn merge(self, other: Self) -> Self {
        Tunables {
            log_level: self.log_level.or(other.log_level),
            reaction_display: self.reaction_display.or(other.reaction_display),
            reaction_shortcode_display: self
                .reaction_shortcode_display
                .or(other.reaction_shortcode_display),
            read_receipt_send: self.read_receipt_send.or(other.read_receipt_send),
            read_receipt_display: self.read_receipt_display.or(other.read_receipt_display),
            request_timeout: self.request_timeout.or(other.request_timeout),
            typing_notice_send: self.typing_notice_send.or(other.typing_notice_send),
            typing_notice_display: self.typing_notice_display.or(other.typing_notice_display),
            users: merge_users(self.users, other.users),
            default_room: self.default_room.or(other.default_room),
        }
    }

    fn values(self) -> TunableValues {
        TunableValues {
            log_level: self.log_level.map(Level::from).unwrap_or(Level::INFO),
            reaction_display: self.reaction_display.unwrap_or(true),
            reaction_shortcode_display: self.reaction_shortcode_display.unwrap_or(false),
            read_receipt_send: self.read_receipt_send.unwrap_or(true),
            read_receipt_display: self.read_receipt_display.unwrap_or(true),
            request_timeout: self.request_timeout.unwrap_or(DEFAULT_REQ_TIMEOUT),
            typing_notice_send: self.typing_notice_send.unwrap_or(true),
            typing_notice_display: self.typing_notice_display.unwrap_or(true),
            users: self.users.unwrap_or_default(),
            default_room: self.default_room,
        }
    }
}

#[derive(Clone)]
pub struct DirectoryValues {
    pub cache: PathBuf,
    pub logs: PathBuf,
    pub downloads: PathBuf,
}

#[derive(Clone, Default, Deserialize)]
pub struct Directories {
    pub cache: Option<PathBuf>,
    pub logs: Option<PathBuf>,
    pub downloads: Option<PathBuf>,
}

impl Directories {
    fn merge(self, other: Self) -> Self {
        Directories {
            cache: self.cache.or(other.cache),
            logs: self.logs.or(other.logs),
            downloads: self.downloads.or(other.downloads),
        }
    }

    fn values(self) -> DirectoryValues {
        let cache = self
            .cache
            .or_else(|| {
                let mut dir = dirs::cache_dir()?;
                dir.push("iamb");
                dir.into()
            })
            .expect("no dirs.cache value configured!");

        let logs = self.logs.unwrap_or_else(|| {
            let mut dir = cache.clone();
            dir.push("logs");
            dir
        });

        let downloads = self
            .downloads
            .or_else(dirs::download_dir)
            .expect("no dirs.downloads value configured!");

        DirectoryValues { cache, logs, downloads }
    }
}

#[derive(Clone, Deserialize)]
pub struct ProfileConfig {
    pub user_id: OwnedUserId,
    pub url: Url,
    pub settings: Option<Tunables>,
    pub dirs: Option<Directories>,
}

#[derive(Clone, Deserialize)]
pub struct IambConfig {
    pub profiles: HashMap<String, ProfileConfig>,
    pub default_profile: Option<String>,
    pub settings: Option<Tunables>,
    pub dirs: Option<Directories>,
}

impl IambConfig {
    pub fn load(config_json: &Path) -> Result<Self, ConfigError> {
        if !config_json.is_file() {
            usage!(
                "Please create a configuration file at {}\n\n\
                For more information try '--help'",
                config_json.display(),
            );
        }

        let file = File::open(config_json)?;
        let reader = BufReader::new(file);
        let config = serde_json::from_reader(reader)?;

        Ok(config)
    }
}

#[derive(Clone)]
pub struct ApplicationSettings {
    pub matrix_dir: PathBuf,
    pub session_json: PathBuf,
    pub profile_name: String,
    pub profile: ProfileConfig,
    pub tunables: TunableValues,
    pub dirs: DirectoryValues,
}

impl ApplicationSettings {
    pub fn load(cli: Iamb) -> Result<Self, Box<dyn std::error::Error>> {
        let mut config_dir = cli.config_directory.or_else(dirs::config_dir).unwrap_or_else(|| {
            usage!(
                "No user configuration directory found;\
                please specify one via -C.\n\n
                For more information try '--help'"
            );
        });
        config_dir.push("iamb");
        let mut config_json = config_dir.clone();
        config_json.push("config.json");

        let IambConfig {
            mut profiles,
            default_profile,
            dirs,
            settings: global,
        } = IambConfig::load(config_json.as_path())?;

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
            usage!(
                "No profile specified. \
                Please use -P or add \"default_profile\" to {}.\n\n\
                For more information try '--help'",
                config_json.display()
            );
        };

        let tunables = global.unwrap_or_default();
        let tunables = profile.settings.take().unwrap_or_default().merge(tunables);
        let tunables = tunables.values();

        let mut profile_dir = config_dir.clone();
        profile_dir.push("profiles");
        profile_dir.push(profile_name.as_str());

        let mut matrix_dir = profile_dir.clone();
        matrix_dir.push("matrix");

        let mut session_json = profile_dir;
        session_json.push("session.json");

        let dirs = dirs.unwrap_or_default();
        let dirs = profile.dirs.take().unwrap_or_default().merge(dirs);
        let dirs = dirs.values();

        let settings = ApplicationSettings {
            matrix_dir,
            session_json,
            profile_name,
            profile,
            tunables,
            dirs,
        };

        Ok(settings)
    }

    pub fn get_user_char_span<'a>(&self, user_id: &'a UserId) -> Span<'a> {
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

    pub fn get_user_span<'a>(&self, user_id: &'a UserId) -> Span<'a> {
        let (color, name) = self
            .tunables
            .users
            .get(user_id)
            .map(|user| (user.color.as_ref().map(|c| c.0), user.name.clone().map(Cow::Owned)))
            .unwrap_or_default();

        let user_id = user_id.as_str();
        let color = color.unwrap_or_else(|| user_color(user_id));
        let style = user_style_from_color(color);
        let name = name.unwrap_or(Cow::Borrowed(user_id));

        Span::styled(name, style)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use matrix_sdk::ruma::user_id;

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

        let res = merge_users(a.clone(), a.clone());
        assert_eq!(res, None);

        let res = merge_users(a.clone(), Some(b.clone()));
        assert_eq!(res, Some(b.clone()));

        let res = merge_users(Some(b.clone()), a.clone());
        assert_eq!(res, Some(b.clone()));

        let res = merge_users(Some(b.clone()), Some(b.clone()));
        assert_eq!(res, Some(b.clone()));

        let res = merge_users(Some(b.clone()), Some(c.clone()));
        assert_eq!(res, Some(c.clone()));

        let res = merge_users(Some(c.clone()), Some(b.clone()));
        assert_eq!(res, Some(b.clone()));
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
}
