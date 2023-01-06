use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::process;

use clap::Parser;
use matrix_sdk::ruma::OwnedUserId;
use serde::Deserialize;
use url::Url;

macro_rules! usage {
    ( $($args: tt)* ) => {
        println!($($args)*);
        process::exit(2);
    }
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

#[derive(Clone)]
pub struct TunableValues {
    pub typing_notice: bool,
    pub typing_notice_display: bool,
}

#[derive(Clone, Default, Deserialize)]
pub struct Tunables {
    pub typing_notice: Option<bool>,
    pub typing_notice_display: Option<bool>,
}

impl Tunables {
    fn merge(self, other: Self) -> Self {
        Tunables {
            typing_notice: self.typing_notice.or(other.typing_notice),
            typing_notice_display: self.typing_notice_display.or(other.typing_notice_display),
        }
    }

    fn values(self) -> TunableValues {
        TunableValues {
            typing_notice: self.typing_notice.unwrap_or(true),
            typing_notice_display: self.typing_notice.unwrap_or(true),
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
            .or_else(dirs::cache_dir)
            .expect("no dirs.cache value configured!");

        let logs = self.logs.unwrap_or_else(|| {
            let mut dir = cache.clone();
            dir.push("logs");
            dir
        });

        let downloads = self
            .downloads
            .or_else(dirs::download_dir)
            .expect("no dirs.download value configured!");

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
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
