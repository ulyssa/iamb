use std::fmt;

use modalkit::editing::store::RegisterStore;
use ratatui::crossterm::cursor::SetCursorStyle;
use ratatui_image::FilterType;
use ratatui_image::picker::ProtocolType;
use serde::de::Visitor as _;
use serde::de::value::StringDeserializer;
use strum::{EnumDiscriminants, EnumProperty, IntoStaticStr, VariantArray, VariantNames};
use tracing_subscriber::EnvFilter;

use crate::base::{SortColumn, SortFieldRoom, SortFieldUser, SortRoomVisitor, SortUserVisitor};
use crate::config::{
    ConfigError,
    CursorShape,
    EncryptionIndicator,
    EncryptionIndicatorLocation,
    EncryptionIndicatorLocationVisitor,
    NotifyVia,
    NotifyViaVisitor,
    parse_env_logger,
};
use crate::config::{
    IambConfig,
    MarkupFormat,
    ReadReceiptTrigger,
    SettingsFile,
    SplitDirection,
    UserDisplayStyle,
    deserialize_register,
};
use crate::prelude::*;

#[derive(thiserror::Error, Debug)]
pub enum ReloadError {
    #[error(transparent)]
    Config(#[from] ConfigError),

    #[error("invalid `log_level`: {0}")]
    LogLevel(#[from] tracing_subscriber::filter::ParseError),

    #[error("The current profile is not in the new config file")]
    ProfileNotFound,

    #[error("The user_id in the new config is different")]
    UserIdChanged,
}

#[derive(Debug, Clone)]
pub struct LogLevelUpdate {
    filter: EnvFilter,
    directives: String,
}

impl LogLevelUpdate {
    fn parse(directives: String) -> Result<Box<Self>, tracing_subscriber::filter::ParseError> {
        let filter = parse_env_logger(&directives)?;
        Ok(Box::new(Self { filter, directives }))
    }
}

impl PartialEq for LogLevelUpdate {
    fn eq(&self, other: &Self) -> bool {
        self.directives == other.directives
    }
}
impl Eq for LogLevelUpdate {}

/// Error returned by [`TunablesUpdate::new`].
#[derive(thiserror::Error, Debug)]
pub enum TunablesUpdateError {
    #[error("Unknown option")]
    UnknownOption,

    #[error("Invalid argument")]
    InvalidArgument,

    #[error(transparent)]
    LogLevel(#[from] tracing_subscriber::filter::ParseError),

    #[error("This option requires an argument")]
    NoArguments,

    #[error(transparent)]
    ParseEnum(#[from] strum::ParseError),

    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),

    #[error(transparent)]
    ParseColor(#[from] ratatui::style::ParseColorError),

    #[error("Invalid user id: {0}")]
    UserId(#[from] matrix_sdk::IdParseError),

    #[error("{0}")]
    Custom(String),
}

impl serde::de::Error for TunablesUpdateError {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self::Custom(format!("{msg}"))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, EnumDiscriminants)]
#[strum_discriminants(
    strum(serialize_all = "snake_case"),
    derive(IntoStaticStr, VariantArray)
)]
pub enum SortUpdate {
    Chats(Vec<SortColumn<SortFieldRoom>>),
    Dms(Vec<SortColumn<SortFieldRoom>>),
    Rooms(Vec<SortColumn<SortFieldRoom>>),
    Spaces(Vec<SortColumn<SortFieldRoom>>),
    Members(Vec<SortColumn<SortFieldUser>>),
}

impl SortUpdate {
    fn new(option: &str, value: &str) -> Result<Self, TunablesUpdateError> {
        if option == "members" {
            let order: Result<Vec<_>, TunablesUpdateError> = value
                .split(',')
                .filter(|v| !v.is_empty())
                .map(|v| SortUserVisitor.visit_str(v))
                .collect();
            return Ok(Self::Members(order?));
        }

        let order: Result<Vec<_>, TunablesUpdateError> = value
            .split(',')
            .filter(|v| !v.is_empty())
            .map(|v| SortRoomVisitor.visit_str(v))
            .collect();

        Ok(match option {
            "chats" => Self::Chats(order?),
            "dms" => Self::Dms(order?),
            "rooms" => Self::Rooms(order?),
            "spaces" => Self::Spaces(order?),
            _ => return Err(TunablesUpdateError::UnknownOption),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, EnumDiscriminants)]
#[strum_discriminants(
    strum(serialize_all = "snake_case"),
    derive(IntoStaticStr, EnumProperty, VariantArray)
)]
pub enum NotificationsUpdate {
    Via(NotifyVia),
    SoundHint(Option<String>),

    #[strum_discriminants(strum(props(is_bool = true)))]
    Enabled(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    ShowMessage(bool),
}

impl NotificationsUpdate {
    fn new(option: &str, value: Option<&str>) -> Result<Self, TunablesUpdateError> {
        let res = match option {
            "via" => {
                if let Some(value) = value {
                    let via = NotifyViaVisitor.visit_str::<TunablesUpdateError>(value)?;
                    Self::Via(via)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "soundhint" => {
                if let Some(value) = value {
                    if value.is_empty() {
                        Self::SoundHint(None)
                    } else {
                        Self::SoundHint(Some(value.to_owned()))
                    }
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },

            "enabled" => Self::Enabled(true),
            "noenabled" => Self::Enabled(false),
            "showmessage" => Self::ShowMessage(true),
            "noshowmessage" => Self::ShowMessage(false),

            _ => return Err(TunablesUpdateError::UnknownOption),
        };

        Ok(res)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, EnumDiscriminants)]
#[strum_discriminants(derive(IntoStaticStr, VariantArray))]
pub enum UserDisplayUpdate {
    Name(Option<String>),
    Color(Option<Color>),
}

impl UserDisplayUpdate {
    fn new(option: &str, value: &str) -> Result<Self, TunablesUpdateError> {
        let res = match option {
            "name" => {
                if value.is_empty() {
                    Self::Name(None)
                } else {
                    Self::Name(Some(value.to_owned()))
                }
            },
            "color" => {
                if value.is_empty() {
                    Self::Color(None)
                } else {
                    let color = Color::from_str(value)?;
                    Self::Color(Some(color))
                }
            },

            _ => return Err(TunablesUpdateError::UnknownOption),
        };

        Ok(res)
    }
}

/// This should always mirrir [`ProtocolType`]
#[derive(Clone, Copy, PartialEq, Eq, Debug, VariantNames)]
#[strum(serialize_all = "lowercase")]
pub enum IambProtocolType {
    Halfblocks,
    Sixel,
    Kitty,
    Iterm2,
}

impl From<IambProtocolType> for ProtocolType {
    fn from(value: IambProtocolType) -> Self {
        match value {
            IambProtocolType::Halfblocks => Self::Halfblocks,
            IambProtocolType::Sixel => Self::Sixel,
            IambProtocolType::Kitty => Self::Kitty,
            IambProtocolType::Iterm2 => Self::Iterm2,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumDiscriminants)]
#[strum_discriminants(derive(IntoStaticStr, VariantArray))]
pub enum ImagePreviewUpdate {
    Enabled(bool),
    Width(u16),
    Height(u16),
    ProtocolType(IambProtocolType),
    ProtocolFilter(FilterType),

    /// Reload the image previews without chaning a setting (used by `:reload`)
    Reload,
}

impl ImagePreviewUpdate {
    fn new(option: &str, value: Option<&str>) -> Result<Self, TunablesUpdateError> {
        let res = match option {
            "size.width" => {
                if let Some(value) = value {
                    let width = u16::from_str(value)?;
                    Self::Width(width)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "size.height" => {
                if let Some(value) = value {
                    let height = u16::from_str(value)?;
                    Self::Height(height)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "protocol.type" => {
                let Some(value) = value else {
                    return Err(TunablesUpdateError::NoArguments);
                };
                let protocol_type = match value {
                    "sixel" => IambProtocolType::Sixel,
                    "kitty" => IambProtocolType::Kitty,
                    "iterm2" => IambProtocolType::Iterm2,
                    "halfblocks" => IambProtocolType::Halfblocks,
                    _ => return Err(TunablesUpdateError::InvalidArgument),
                };

                Self::ProtocolType(protocol_type)
            },

            "protocol.filter" => {
                let Some(value) = value else {
                    return Err(TunablesUpdateError::NoArguments);
                };
                let filter = match value {
                    "Nearest" => FilterType::Nearest,
                    "Triangle" => FilterType::Triangle,
                    "CatmullRom" => FilterType::CatmullRom,
                    "Gaussian" => FilterType::Gaussian,
                    "Lanczos3" => FilterType::Lanczos3,
                    _ => return Err(TunablesUpdateError::InvalidArgument),
                };

                Self::ProtocolFilter(filter)
            },

            "enabled" => Self::Enabled(true),
            "noenabled" => Self::Enabled(false),

            _ => return Err(TunablesUpdateError::UnknownOption),
        };

        Ok(res)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, EnumDiscriminants)]
#[strum_discriminants(
    strum(serialize_all = "snake_case"),
    derive(IntoStaticStr, VariantArray)
)]
pub enum EncryptionUpdate {
    Indicator(EncryptionIndicator),
    IndicatorLocation(EncryptionIndicatorLocation),
    IconEncrypted(String),
    IconUnencrypted(String),
    IconUnknown(String),
}

impl EncryptionUpdate {
    fn new(option: &str, value: &str) -> Result<Self, TunablesUpdateError> {
        let res = match option {
            "indicator" => Self::Indicator(EncryptionIndicator::from_str(value)?),
            "indicatorlocation" => {
                let via =
                    EncryptionIndicatorLocationVisitor.visit_str::<TunablesUpdateError>(value)?;
                Self::IndicatorLocation(via)
            },
            "iconencrypted" => Self::IconEncrypted(value.to_string()),
            "iconunencrypted" => Self::IconUnencrypted(value.to_string()),
            "iconunknown" => Self::IconUnknown(value.to_string()),

            _ => return Err(TunablesUpdateError::UnknownOption),
        };

        Ok(res)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, EnumDiscriminants)]
#[strum_discriminants(
    strum(serialize_all = "snake_case"),
    derive(IntoStaticStr, VariantArray)
)]
pub enum TerminalUpdate {
    CursorShape(CursorShape),
}

impl TerminalUpdate {
    fn new(option: &str, value: Option<&str>) -> Result<Self, TunablesUpdateError> {
        let res = match option {
            "cursorshape" => {
                if let Some(value) = value {
                    Self::CursorShape(CursorShape::from_str(value)?)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },

            _ => return Err(TunablesUpdateError::UnknownOption),
        };

        Ok(res)
    }
}

/// A update for the [`TunableValues`] after invoking the `:set` command.
#[derive(Debug, PartialEq, Eq, Clone, EnumDiscriminants)]
#[strum_discriminants(
    strum(serialize_all = "snake_case"),
    derive(IntoStaticStr, EnumProperty, VariantArray)
)]
pub enum TunablesUpdate {
    // multilevel options
    Sort(SortUpdate),
    Notifications(NotificationsUpdate),
    Users(OwnedUserId, UserDisplayUpdate),
    ImagePreview(ImagePreviewUpdate),
    Encryption(EncryptionUpdate),
    Terminal(TerminalUpdate),

    // value options
    DefaultMarkup(MarkupFormat),
    DefaultRegister(Register),
    DefaultSplit(SplitDirection),
    DefaultVia(Vec<OwnedServerName>),
    InputPrompt(Option<String>),
    LogLevel(Box<LogLevelUpdate>),
    MembersSplit(Option<SplitDirection>),
    ReadReceiptTrigger(ReadReceiptTrigger),
    UsernameDisplay(UserDisplayStyle),
    OpenCommand(Vec<String>),
    ExternalEditFileSuffix(String),
    UserGutterWidth(usize),
    Tabstop(usize),

    // bool options
    #[strum_discriminants(strum(props(is_bool = true)))]
    MessageShortcodeDisplay(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    NormalAfterSend(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    SendOnEnter(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    ReactionDisplay(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    ReactionShortcodeDisplay(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    ReadReceiptSend(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    ReadReceiptDisplay(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    TypingNoticeSend(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    TypingNoticeDisplay(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    MessageUserColor(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    MessageFormattedDisplay(bool),
    #[strum_discriminants(strum(props(is_bool = true)))]
    Ignorecase(bool),
}

impl TunablesUpdate {
    pub fn new(mut option: String, value: Option<&str>) -> Result<Self, TunablesUpdateError> {
        option.retain(|c| c != '_');

        // sort options
        if let Some(sort_option) = option.strip_prefix("sort.") {
            let Some(value) = value else {
                return Err(TunablesUpdateError::NoArguments);
            };

            return Ok(Self::Sort(SortUpdate::new(sort_option, value)?));
        }
        // notifications
        if let Some(notification_option) = option.strip_prefix("notifications.") {
            return Ok(Self::Notifications(NotificationsUpdate::new(notification_option, value)?));
        }
        // user overrides
        if let Some(users_option) = option.strip_prefix("users.") {
            let Some((user_id, user_option)) = users_option.rsplit_once('.') else {
                return Err(TunablesUpdateError::UnknownOption);
            };

            let user_id = OwnedUserId::from_str(user_id)?;

            let Some(value) = value else {
                return Err(TunablesUpdateError::NoArguments);
            };

            let update = UserDisplayUpdate::new(user_option, value)?;

            return Ok(Self::Users(user_id, update));
        }
        // image previews
        if let Some(image_preview_option) = option.strip_prefix("imagepreview.") {
            return Ok(Self::ImagePreview(ImagePreviewUpdate::new(image_preview_option, value)?));
        }
        // encryption indicator
        if let Some(encryption_option) = option.strip_prefix("encryption.") {
            let Some(value) = value else {
                return Err(TunablesUpdateError::NoArguments);
            };

            return Ok(Self::Encryption(EncryptionUpdate::new(encryption_option, value)?));
        }
        // terminal
        if let Some(terminal_option) = option.strip_prefix("terminal.") {
            return Ok(Self::Terminal(TerminalUpdate::new(terminal_option, value)?));
        }

        let res = match option.as_str() {
            // value options
            "loglevel" => {
                if let Some(value) = value {
                    Self::LogLevel(LogLevelUpdate::parse(value.to_owned())?)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "defaultmarkup" => {
                if let Some(value) = value {
                    Self::DefaultMarkup(MarkupFormat::from_str(value)?)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "defaultsplit" => {
                if let Some(value) = value {
                    Self::DefaultSplit(SplitDirection::from_str(value)?)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "defaultregister" => {
                if let Some(value) = value {
                    let deserializer: StringDeserializer<serde::de::value::Error> =
                        StringDeserializer::new(value.to_string());

                    let Some(reg) = deserialize_register(deserializer)
                        .map_err(|e| TunablesUpdateError::Custom(e.to_string()))?
                    else {
                        return Err(TunablesUpdateError::InvalidArgument);
                    };

                    Self::DefaultRegister(reg)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "defaultvia" => {
                if let Some(value) = value {
                    let via: Result<_, _> =
                        value.split(",").map(OwnedServerName::try_from).collect();
                    Self::DefaultVia(via?)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "inputprompt" => {
                match value {
                    Some("") => Self::InputPrompt(None),
                    Some(value) => Self::InputPrompt(Some(value.to_string())),
                    None => return Err(TunablesUpdateError::NoArguments),
                }
            },
            "memberssplit" => {
                if let Some(value) = value {
                    if value.is_empty() {
                        Self::MembersSplit(None)
                    } else {
                        Self::MembersSplit(Some(SplitDirection::from_str(value)?))
                    }
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "readreceipttrigger" => {
                if let Some(value) = value {
                    Self::ReadReceiptTrigger(ReadReceiptTrigger::from_str(value)?)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "usernamedisplay" => {
                if let Some(value) = value {
                    let display = UserDisplayStyle::from_str(value)?;
                    Self::UsernameDisplay(display)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "opencommand" => {
                if let Some(value) = value {
                    // TODO: use command parsing
                    let args = value
                        .split(' ')
                        .filter(|arg| !arg.is_empty())
                        .map(str::to_string)
                        .collect();
                    Self::OpenCommand(args)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "externaleditfilesuffix" => {
                if let Some(value) = value {
                    Self::ExternalEditFileSuffix(value.to_string())
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "usergutterwidth" => {
                if let Some(value) = value {
                    let width = usize::from_str(value)?;
                    Self::UserGutterWidth(width)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },
            "tabstop" => {
                if let Some(value) = value {
                    let tabstop = usize::from_str(value)?;
                    Self::Tabstop(tabstop)
                } else {
                    return Err(TunablesUpdateError::NoArguments);
                }
            },

            // bool options
            "messageshortcodedisplay" => Self::MessageShortcodeDisplay(true),
            "nomessageshortcodedisplay" => Self::MessageShortcodeDisplay(false),
            "messageformatteddisplay" => Self::MessageFormattedDisplay(true),
            "nomessageformatteddisplay" => Self::MessageFormattedDisplay(false),
            "normalaftersend" => Self::NormalAfterSend(true),
            "nonormalaftersend" => Self::NormalAfterSend(false),
            "sendonenter" => Self::SendOnEnter(true),
            "nosendonenter" => Self::SendOnEnter(false),
            "reactiondisplay" => Self::ReactionDisplay(true),
            "noreactiondisplay" => Self::ReactionDisplay(false),
            "reactionshortcodedisplay" => Self::ReactionShortcodeDisplay(true),
            "noreactionshortcodedisplay" => Self::ReactionShortcodeDisplay(false),
            "readreceiptsend" => Self::ReadReceiptSend(true),
            "noreadreceiptsend" => Self::ReadReceiptSend(false),
            "readreceiptdisplay" => Self::ReadReceiptDisplay(true),
            "noreadreceiptdisplay" => Self::ReadReceiptDisplay(false),
            "typingnoticesend" => Self::TypingNoticeSend(true),
            "notypingnoticesend" => Self::TypingNoticeSend(false),
            "typingnoticedisplay" => Self::TypingNoticeDisplay(true),
            "notypingnoticedisplay" => Self::TypingNoticeDisplay(false),
            "messageusercolor" => Self::MessageUserColor(true),
            "nomessageusercolor" => Self::MessageUserColor(false),
            "ignorecase" => Self::Ignorecase(true),
            "noignorecase" => Self::Ignorecase(false),

            _ => return Err(TunablesUpdateError::UnknownOption),
        };

        Ok(res)
    }
}

impl ApplicationSettings {
    pub fn reload(
        &mut self,
        path: Option<SettingsFile>,
        previews: &mut PreviewManager,
        registers: &mut RegisterStore,
    ) -> Result<(), ReloadError> {
        let load_file = path.unwrap_or_else(|| self.load_file.clone());

        let config = match &load_file {
            SettingsFile::Toml(path) => IambConfig::load_toml(path.as_path())?,
            SettingsFile::Json(path) => IambConfig::load_json(path.as_path())?,
        };

        let IambConfig { mut profiles, dirs, settings: global, theme, .. } = config;

        // TODO: validate profiles?

        let mut profile =
            profiles.remove(&self.profile_name).ok_or(ReloadError::ProfileNotFound)?;

        if profile.user_id != self.profile.user_id {
            return Err(ReloadError::UserIdChanged);
        }

        // TODO: update macros

        let tunables = global.unwrap_or_default();
        let tunables = profile.settings.take().unwrap_or_default().merge(tunables);
        let tunables = tunables.values();

        let dirs = dirs.unwrap_or_default();
        let dirs = profile.dirs.take().unwrap_or_default().merge(dirs);
        let dirs = dirs.values();

        let theme = theme.unwrap_or_default().merge(super::theme::default_theme());
        let theme = profile.theme.take().unwrap_or_default().merge(theme);
        let theme = Arc::new(theme.values());

        let image_preview_changed = tunables.image_preview != self.tunables.image_preview;

        // update values
        self.tunables = tunables;
        self.profile = profile;
        self.load_file = load_file;
        self.dirs.downloads = dirs.downloads;
        self.theme = theme;

        // apply changes that need more setup

        self.update(
            TunablesUpdate::LogLevel(LogLevelUpdate::parse(self.tunables.log_level.to_owned())?),
            previews,
            registers,
        );

        if image_preview_changed {
            self.update(
                TunablesUpdate::ImagePreview(ImagePreviewUpdate::Reload),
                previews,
                registers,
            );
        }

        if let Some(reg) = &self.tunables.default_register {
            self.update(TunablesUpdate::DefaultRegister(reg.to_owned()), previews, registers);
        }

        Ok(())
    }

    /// Update [`self.tunables`](`Self::tunables`) with `new`.
    /// This will make sure that the updated value is applied.
    pub fn update(
        &mut self,
        update: TunablesUpdate,
        previews: &mut PreviewManager,
        registers: &mut RegisterStore,
    ) {
        match update {
            TunablesUpdate::LogLevel(update) => {
                if let Some(handle) = &mut self.log_level_handle {
                    handle
                        .reload(update.filter)
                        .expect("cannot update appending tracing logger");
                    self.tunables.log_level = update.directives;
                }
            },
            TunablesUpdate::ImagePreview(image_preview_update) => {
                let image_preview = &mut self.tunables.image_preview;
                match image_preview_update {
                    ImagePreviewUpdate::Width(width) => image_preview.size.width = width,
                    ImagePreviewUpdate::Height(height) => image_preview.size.height = height,
                    ImagePreviewUpdate::ProtocolType(protocol_type) => {
                        image_preview.protocol.r#type = Some(protocol_type.into())
                    },
                    ImagePreviewUpdate::Enabled(enabled) => image_preview.enabled = enabled,
                    ImagePreviewUpdate::ProtocolFilter(filter) => {
                        image_preview.protocol.filter = Some(filter)
                    },
                    ImagePreviewUpdate::Reload => (),
                }

                if matches!(
                    image_preview_update,
                    ImagePreviewUpdate::Reload | ImagePreviewUpdate::ProtocolType(_)
                ) && let Some(protocol_type) = image_preview.protocol.r#type
                {
                    previews.update_protocol_type(protocol_type);
                }

                previews.mark_all_queued(image_preview.size);
            },
            TunablesUpdate::Sort(sort_update) => {
                match sort_update {
                    SortUpdate::Chats(order) => self.tunables.sort.chats = order,
                    SortUpdate::Dms(order) => self.tunables.sort.dms = order,
                    SortUpdate::Rooms(order) => self.tunables.sort.rooms = order,
                    SortUpdate::Spaces(order) => self.tunables.sort.spaces = order,
                    SortUpdate::Members(order) => self.tunables.sort.members = order,
                }
            },
            TunablesUpdate::Notifications(notify_update) => {
                match notify_update {
                    NotificationsUpdate::Enabled(value) => {
                        self.tunables.notifications.enabled = value
                    },
                    NotificationsUpdate::Via(value) => self.tunables.notifications.via = value,
                    NotificationsUpdate::ShowMessage(value) => {
                        self.tunables.notifications.show_message = value
                    },
                    NotificationsUpdate::SoundHint(value) => {
                        self.tunables.notifications.sound_hint = value
                    },
                }
            },
            TunablesUpdate::Users(user_id, user_update) => {
                let user = self.tunables.users.entry(user_id).or_default();

                match user_update {
                    UserDisplayUpdate::Name(name) => user.name = name,
                    UserDisplayUpdate::Color(color) => user.color = color,
                }
            },
            TunablesUpdate::OpenCommand(open_command) => {
                if open_command.is_empty() {
                    self.tunables.open_command = None;
                } else {
                    self.tunables.open_command = Some(open_command);
                }
            },
            TunablesUpdate::DefaultRegister(reg) => {
                self.tunables.default_register = Some(reg.clone());
                registers.set_default_register(reg);
            },
            TunablesUpdate::Terminal(TerminalUpdate::CursorShape(shape)) => {
                self.tunables.terminal.cursor_shape = shape;

                let cursor_shape = SetCursorStyle::from(shape);
                let _ = modalkit::crossterm::execute!(std::io::stdout(), cursor_shape);
            },
            TunablesUpdate::DefaultMarkup(format) => {
                self.tunables.default_markup = format;
            },
            TunablesUpdate::DefaultSplit(direction) => {
                self.tunables.default_split = direction;
            },
            TunablesUpdate::DefaultVia(via) => {
                self.tunables.default_via = via;
            },
            TunablesUpdate::MembersSplit(direction) => {
                self.tunables.members_split = direction;
            },
            TunablesUpdate::ReadReceiptTrigger(trigger) => {
                self.tunables.read_receipt_trigger = trigger;
            },
            TunablesUpdate::Encryption(EncryptionUpdate::Indicator(indicator)) => {
                self.tunables.encryption.indicator = indicator;
            },
            TunablesUpdate::Encryption(EncryptionUpdate::IndicatorLocation(indicator_location)) => {
                self.tunables.encryption.indicator_location = indicator_location;
            },
            TunablesUpdate::Encryption(EncryptionUpdate::IconEncrypted(icon)) => {
                self.tunables.encryption.icon_encrypted = icon.into();
            },
            TunablesUpdate::Encryption(EncryptionUpdate::IconUnencrypted(icon)) => {
                self.tunables.encryption.icon_unencrypted = icon.into();
            },
            TunablesUpdate::Encryption(EncryptionUpdate::IconUnknown(icon)) => {
                self.tunables.encryption.icon_unknown = icon.into();
            },
            TunablesUpdate::UsernameDisplay(username_display) => {
                self.tunables.username_display = username_display
            },
            TunablesUpdate::ExternalEditFileSuffix(external_edit_file_suffix) => {
                self.tunables.external_edit_file_suffix = external_edit_file_suffix
            },
            TunablesUpdate::UserGutterWidth(user_gutter_width) => {
                self.tunables.user_gutter_width = user_gutter_width
            },
            TunablesUpdate::Tabstop(tabstop) => self.tunables.tabstop = tabstop,
            TunablesUpdate::MessageShortcodeDisplay(message_shortcode_display) => {
                self.tunables.message_shortcode_display = message_shortcode_display
            },
            TunablesUpdate::MessageFormattedDisplay(message_formatted_display) => {
                self.tunables.message_formatted_display = message_formatted_display
            },
            TunablesUpdate::NormalAfterSend(normal_after_send) => {
                self.tunables.normal_after_send = normal_after_send
            },
            TunablesUpdate::SendOnEnter(send_on_enter) => {
                self.tunables.send_on_enter = send_on_enter
            },
            TunablesUpdate::ReactionDisplay(reaction_display) => {
                self.tunables.reaction_display = reaction_display
            },
            TunablesUpdate::ReactionShortcodeDisplay(reaction_shortcode_display) => {
                self.tunables.reaction_shortcode_display = reaction_shortcode_display
            },
            TunablesUpdate::ReadReceiptSend(read_receipt_send) => {
                self.tunables.read_receipt_send = read_receipt_send
            },
            TunablesUpdate::ReadReceiptDisplay(read_receipt_display) => {
                self.tunables.read_receipt_display = read_receipt_display
            },
            TunablesUpdate::TypingNoticeSend(typing_notice_send) => {
                self.tunables.typing_notice_send = typing_notice_send
            },
            TunablesUpdate::TypingNoticeDisplay(typing_notice_display) => {
                self.tunables.typing_notice_display = typing_notice_display
            },
            TunablesUpdate::MessageUserColor(message_user_color) => {
                self.tunables.message_user_color = message_user_color
            },
            TunablesUpdate::Ignorecase(ic) => {
                self.tunables.ignorecase = ic;
            },
            TunablesUpdate::InputPrompt(prompt) => {
                self.tunables.input_prompt = prompt;
            },
        }
    }
}
