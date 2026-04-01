//! Tab completion for iamb
use std::borrow::Cow;
use std::ops::Deref;
use std::str::FromStr;

use matrix_sdk::ruma::{RoomId, UserId};
use modalkit::editing::completion::{complete_path, Completer};
use modalkit::editing::cursor::Cursor;
use modalkit::editing::rope::EditRope;
use modalkit::env::vim::command::CommandDescription;
use modalkit::prelude::{
    CommandType,
    Count,
    CursorMovements,
    CursorMovementsContext,
    MoveDir1D,
    MoveType,
    WordStyle,
};
use strum::{EnumProperty, VariantArray, VariantNames};

use crate::base::{
    ChatStore,
    IambBufferId,
    IambInfo,
    RoomFocus,
    SortFieldRoom,
    SortFieldUser,
    MATRIX_ID_WORD,
};
use crate::config::{
    ColorsUpdateDiscriminants,
    EncryptionIndicator,
    NotificationsUpdateDiscriminants,
    SortUpdateDiscriminants,
    TunablesUpdateDiscriminants,
    UserDisplayStyle,
};

mod parse {
    use nom::branch::alt;
    use nom::bytes::complete::{escaped_transform, is_not, tag};
    use nom::character::complete::{char, space0, space1};
    use nom::combinator::{cut, eof, opt, value};
    use nom::error::{ErrorKind, ParseError};
    use nom::{IResult, Parser};

    fn parse_text(input: &str) -> IResult<&str, String> {
        if input.is_empty() {
            let err = ParseError::from_error_kind(input, ErrorKind::Eof);
            let err = nom::Err::Error(err);
            return Err(err);
        }

        let _ = is_not("\"")(input)?;

        escaped_transform(
            is_not("\t\n\\ |\""),
            '\\',
            alt((
                value("\\", tag("\\")),
                value(" ", tag(" ")),
                value("#", tag("#")),
                value("%", tag("%")),
                value("|", tag("|")),
                value("\"", tag("\"")),
            )),
        )
        .parse(input)
    }

    fn parse_unclosed_quote(input: &str) -> IResult<&str, String> {
        if input.is_empty() {
            let err = ParseError::from_error_kind(input, ErrorKind::Eof);
            let err = nom::Err::Error(err);
            return Err(err);
        }

        let (input, _) = char('\"')(input)?;
        let (input, text) = cut(escaped_transform(
            is_not("\t\n\\\""),
            '\\',
            alt((
                value("\t", tag("t")),
                value("\r", tag("r")),
                value("\n", tag("n")),
                value("\\", tag("\\")),
                value("\"", tag("\"")),
            )),
        ))
        .parse(input)?;

        Ok((input, text))
    }

    fn parse_quote(input: &str) -> IResult<&str, String> {
        let (input, text) = parse_unclosed_quote(input)?;
        let (input, _) = char('\"')(input)?;

        Ok((input, text))
    }

    fn parse_string(input: &str) -> IResult<&str, String> {
        alt((parse_quote, parse_text)).parse(input)
    }

    /// Acts linke [`separated_list0`](nom::multi::separated_list0) but additionally returns a copy of the last element unparsed.
    fn separated_list0_last_raw<'a, F, G>(
        mut i: &'a str,
        mut separator: G,
        mut parser: F,
    ) -> IResult<&'a str, (Vec<String>, &'a str)>
    where
        F: Parser<&'a str, Output = String, Error = nom::error::Error<&'a str>>,
        G: Parser<&'a str, Error = nom::error::Error<&'a str>>,
    {
        let mut res = vec![];
        let mut old_i = i;

        match parser.parse(i) {
            Err(nom::Err::Error(_)) => return Ok((i, (res, old_i))),
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                res.push(o);
                i = i1;
            },
        }

        loop {
            let len = i.len();
            match separator.parse(i) {
                Err(nom::Err::Error(_)) => return Ok((i, (res, old_i))),
                Err(e) => return Err(e),
                Ok((i1, _)) => {
                    match parser.parse(i1) {
                        Err(nom::Err::Error(_)) => return Ok((i, (res, old_i))),
                        Err(e) => return Err(e),
                        Ok((i2, o)) => {
                            if i2.len() == len {
                                return Err(nom::Err::Error(nom::error::Error::from_error_kind(
                                    i,
                                    ErrorKind::SeparatedList,
                                )));
                            }

                            res.push(o);
                            i = i2;
                            old_i = i1;
                        },
                    }
                },
            }
        }
    }

    fn parse_last_arg(input: &str) -> IResult<&str, (String, &str)> {
        let (input, _) = space0(input)?;

        let old_input = input;
        let (input, arg) = opt(parse_unclosed_quote).parse(input)?;

        Ok((input, (arg.unwrap_or_default(), old_input)))
    }

    fn parse_trailing_last_arg(input: &str) -> IResult<&str, (String, &str)> {
        let (input, _) = space1(input)?;

        parse_last_arg(input)
    }

    /// Returns a list with the parsed strings and a raw version of the last string to be stripped
    /// from the input before completing.
    pub fn parse_started_strings(input: &str) -> IResult<&str, (Vec<String>, &str)> {
        let (input, (mut args, mut last_arg_raw)) =
            separated_list0_last_raw(input, space1, parse_string)?;

        let (input, end_arg) = if args.is_empty() {
            opt(parse_last_arg).parse(input)?
        } else {
            opt(parse_trailing_last_arg).parse(input)?
        };

        let (input, _) = eof(input)?;

        if let Some((arg, end_arg_raw)) = end_arg {
            args.push(arg);
            last_arg_raw = end_arg_raw;
        }

        if args.is_empty() {
            args.push(String::new());
        }

        Ok((input, (args, last_arg_raw)))
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parse_strings() {
            let text = "some normal args";
            let parsed = parse_started_strings(text).unwrap();
            assert_eq!(("", (vec!["some".into(), "normal".into(), "args".into()], "args")), parsed);

            let text = "started ";
            let parsed = parse_started_strings(text).unwrap();
            assert_eq!(("", (vec!["started".into(), "".into()], "")), parsed);

            let text = "args \"with quotes\"";
            let parsed = parse_started_strings(text).unwrap();
            assert_eq!(
                ("", (vec!["args".into(), "with quotes".into()], "\"with quotes\"")),
                parsed
            );

            let text = "and \"started ";
            let parsed = parse_started_strings(text).unwrap();
            assert_eq!(("", (vec!["and".into(), "started ".into()], "\"started ")), parsed);

            let text = "\"only started ";
            let parsed = parse_started_strings(text).unwrap();
            assert_eq!(("", (vec!["only started ".into()], "\"only started ")), parsed);

            let text = "   ";
            let parsed = parse_started_strings(text).unwrap();
            assert_eq!(("", (vec!["".into()], "")), parsed);

            let text = "escaped\\ spaces here";
            let parsed = parse_started_strings(text).unwrap();
            assert_eq!(("", (vec!["escaped spaces".into(), "here".into()], "here")), parsed);
        }
    }
}

/// Tab completion for user IDs.
fn complete_users(input: &str, store: &ChatStore) -> Vec<String> {
    store
        .presences
        .complete(input)
        .into_iter()
        .map(|i| i.to_string())
        .collect()
}

/// Tab completion for Matrix room aliases
fn complete_matrix_aliases(input: &str, store: &ChatStore) -> Vec<String> {
    let list = store.aliases.complete(input);
    if !list.is_empty() {
        return list.into_iter().map(|i| i.to_string()).collect();
    }

    let list = store.presences.complete(input);
    if !list.is_empty() {
        return list.into_iter().map(|i| i.to_string()).collect();
    }

    store.rooms.complete(input).into_iter().map(|i| i.to_string()).collect()
}

/// Tab completion for open verification requests
fn complete_verification(input: &str, store: &ChatStore) -> Vec<String> {
    store.verifications.complete(input)
}

/// Tab completion for Emoji shortcode names.
fn complete_emoji(input: &str, store: &ChatStore) -> Vec<String> {
    store.emojis.complete(input)
}

/// Tab completion for compile-time known strings
fn complete_choices(input: &str, options: &[&'static str]) -> Vec<String> {
    options
        .iter()
        .filter(|opt| opt.starts_with(input))
        .map(|opt| opt.to_string())
        .collect()
}

/// Tab completion for vim-like options
fn complete_options(args: &[String], options: &[&'static str]) -> Vec<String> {
    let opts: Vec<_> = options
        .iter()
        .filter(|o| {
            !args
                .iter()
                .any(|arg| arg.as_str() == **o || (o.ends_with('=') && arg.starts_with(*o)))
        })
        .copied()
        .collect();
    complete_choices(args.last().unwrap(), opts.as_slice())
}

/// Tab completion for [`Color`](ratatui::style::Color).
fn complete_colors(input: &str) -> Vec<String> {
    complete_choices(input, &[
        "black",
        "red",
        "green",
        "yellow",
        "blue",
        "magenta",
        "cyan",
        "gray",
        "dark-gray",
        "light-red",
        "light-green",
        "light-yellow",
        "light-blue",
        "light-magenta",
        "light-cyan",
        "white",
        "reset",
    ])
}

/// Tab completion for `:invite`
fn complete_iamb_invite(args: Vec<String>, store: &ChatStore) -> Vec<String> {
    match args.len() {
        1 => complete_choices(&args[0], &["accept", "reject", "send"]),
        2 if args[0] == "send" => complete_users(&args[1], store),
        _ => vec![],
    }
}

/// Tab completion for `:keys`
fn complete_iamb_keys(
    args: Vec<String>,
    input: &EditRope,
    orig_cursor: Cursor,
    cursor: &mut Cursor,
) -> Vec<String> {
    let subcmds = ["export", "import"];
    match args.len() {
        1 => complete_choices(&args[0], &subcmds),
        2 if subcmds.contains(&args[0].as_str()) => {
            *cursor = orig_cursor;
            complete_path(input, cursor)
        },
        _ => vec![],
    }
}

/// Tab completion for `:verify`
fn complete_iamb_verify(args: Vec<String>, store: &ChatStore) -> Vec<String> {
    let subcmds = [
        "request",
        "accept",
        "confirm",
        "cancel",
        "missmatch",
        "emoji",
    ];
    match args.len() {
        1 => complete_choices(&args[0], &subcmds),
        2 if args[0] == "request" => complete_users(&args[1], store),
        2 if subcmds.contains(&args[0].as_str()) => complete_verification(&args[1], store),
        _ => vec![],
    }
}

/// Tab completion for `:unreads`
fn complete_iamb_unreads(args: Vec<String>) -> Vec<String> {
    match args.len() {
        1 if "clear".starts_with(&args[0]) => vec!["clear".to_string()],
        _ => vec![],
    }
}

/// Tab completion for `:create`
fn complete_iamb_create(args: Vec<String>) -> Vec<String> {
    let options = ["++alias=", "++public", "++space", "++encrypted"];
    complete_options(args.as_slice(), &options)
}

/// Tab completion for `:room`
// TODO: Check whether we can get the id of the focused room to improve
// "kick","ban","unban", ".. unset" and "dm/tag set/unset"
fn complete_iamb_room(args: Vec<String>, store: &ChatStore) -> Vec<String> {
    let subcmds = [
        "dm",
        "unread",
        "kick",
        "ban",
        "unban",
        "history",
        "name",
        "topic",
        "tag",
        "notify",
        "alias",
        "canonicalalias",
        "id",
    ];
    if args.len() == 1 {
        complete_choices(&args[0], &subcmds)
    } else if args.len() == 2 {
        let input = &args[1];
        match args[0].as_str() {
            "kick" | "ban" | "unban" => complete_users(input, store),
            "id" => complete_choices(input, &["show"]),
            "dm" | "name" | "tag" => complete_choices(input, &["set", "unset"]),
            "unread" => complete_choices(input, &["set", "unset", "clear"]),

            "history" | "topic" | "notify" | "alias" | "canonicalalias" | "canon" => {
                complete_choices(input, &["show", "set", "unset"])
            },

            _ => vec![],
        }
    } else if args.len() == 3 {
        let input = &args[2];
        match (args[0].as_str(), args[1].as_str()) {
            ("history", "set") => {
                complete_choices(input, &["invited", "joined", "shared", "world_readable"])
            },
            ("tag", "set") | ("tag", "unset") => {
                complete_choices(input, &["favourite", "lowpriority", "server_notice", "u."])
            },
            ("notify", "set") => complete_choices(input, &["mute", "mentions", "keywords", "all"]),

            _ => vec![],
        }
    } else {
        vec![]
    }
}

/// Tab completion for `:space`
fn complete_iamb_space(args: Vec<String>, store: &ChatStore) -> Vec<String> {
    if args.len() == 1 {
        complete_choices(&args[0], &["child"])
    } else if args.len() == 2 && &args[0] == "child" {
        complete_choices(&args[1], &["set", "remove"])
    } else if args.len() > 2 && &args[0] == "child" && args[1] == "set" {
        let options = ["++suggested", "++order="];

        let has_room = args.iter().skip(2).rev().skip(1).any(|arg| !arg.starts_with('+'));

        let arg = args.last().unwrap();

        if arg.is_empty() {
            let mut opts = complete_options(args.as_slice(), &options);
            if !has_room {
                opts.extend(complete_matrix_aliases(arg, store));
            }
            opts
        } else if arg.starts_with('+') {
            complete_options(args.as_slice(), &options)
        } else if !has_room {
            complete_matrix_aliases(arg, store)
        } else {
            vec![]
        }
    } else {
        vec![]
    }
}

/// Tab completion for `:logout`
fn complete_iamb_logout(args: Vec<String>, store: &ChatStore) -> Vec<String> {
    let id = store.settings.profile.user_id.as_str();
    match args.len() {
        1 if id.starts_with(&args[0]) => vec![id.to_string()],
        _ => vec![],
    }
}

/// Tab completion for `:set`
fn complete_iamb_set(arg: &str, store: &ChatStore) -> Vec<String> {
    if let Some((orig_option, value)) = arg.split_once('=') {
        let mut option = orig_option.to_string();
        option.retain(|c| c != '_');

        match option.as_str() {
            "loglevel" => {
                complete_choices(value, &["off", "error", "warn", "info", "debug", "trace"])
                    .into_iter()
                    .map(|mut s| {
                        s.insert(0, '=');
                        s.insert_str(0, orig_option);
                        s
                    })
                    .collect()
            },
            "usernamedisplay" => {
                complete_choices(value, UserDisplayStyle::VARIANTS)
                    .into_iter()
                    .map(|mut s| {
                        s.insert(0, '=');
                        s.insert_str(0, orig_option);
                        s
                    })
                    .collect()
            },
            "encryptionindicator" => {
                complete_choices(value, EncryptionIndicator::VARIANTS)
                    .into_iter()
                    .map(|mut s| {
                        s.insert(0, '=');
                        s.insert_str(0, orig_option);
                        s
                    })
                    .collect()
            },
            "colors.border" |
            "colors.borderunfocused" |
            "colors.windowtitle" |
            "colors.tabtitle" |
            "colors.tabtitleunfocused" |
            "colors.roomlist" |
            "colors.roomlistunread" |
            "colors.roomlistnotification" |
            "colors.roomlistmention" |
            "colors.roomlistunreadnumber" |
            "colors.roomlistnotificationnumber" |
            "colors.roomlistmentionnumber" |
            "colors.fullyreadmarker" |
            "colors.messagetime" |
            "colors.messagedate" |
            "colors.messagenormal" |
            "colors.messagestate" |
            "colors.messageredacted" |
            "colors.messagenotice" |
            "colors.messageother" |
            "colors.codeblockbackground" => {
                complete_colors(value)
                    .into_iter()
                    .map(|mut s| {
                        s.insert(0, '=');
                        s.insert_str(0, orig_option);
                        s
                    })
                    .collect()
            },
            "sort.chats" | "sort.dms" | "sort.rooms" | "sort.spaces" => {
                let last = value.rsplit_once(',').map(|(_, v)| v).unwrap_or(value);
                let prev = arg.strip_suffix(last).unwrap();

                SortFieldRoom::VARIANTS
                    .iter()
                    .flat_map(|option| vec![format!("{prev}{option}"), format!("{prev}~{option}")])
                    .filter(|option| option.starts_with(arg))
                    .collect()
            },
            "sort.members" => {
                let last = value.rsplit_once(',').map(|(_, v)| v).unwrap_or(value);
                let prev = arg.strip_suffix(last).unwrap();

                SortFieldUser::VARIANTS
                    .iter()
                    .flat_map(|option| vec![format!("{prev}{option}"), format!("{prev}~{option}")])
                    .filter(|option| option.starts_with(arg))
                    .collect()
            },
            "notifications.via" => {
                #[cfg(feature = "desktop")]
                let choices = ["bell", "desktop", "desktop|bell", "bell|desktop"];
                #[cfg(not(feature = "desktop"))]
                let choices = ["bell"];

                complete_choices(value, &choices)
                    .into_iter()
                    .map(|mut s| {
                        s.insert(0, '=');
                        s.insert_str(0, orig_option);
                        s
                    })
                    .collect()
            },
            opt if opt.starts_with("users.") && opt.ends_with(".color") => {
                complete_colors(value)
                    .into_iter()
                    .map(|mut s| {
                        s.insert(0, '=');
                        s.insert_str(0, orig_option);
                        s
                    })
                    .collect()
            },
            _ => vec![],
        }
    } else {
        let mut orig_option = arg.to_string();
        orig_option.retain(|c| c != '_');

        match orig_option.split_once('.') {
            Some(("colors", _)) => {
                ColorsUpdateDiscriminants::VARIANTS
                    .iter()
                    .map(|variant| {
                        let name = <_ as Into<&'static str>>::into(variant);
                        format!("colors.{name}")
                    })
                    .filter(|option| option.starts_with(&orig_option) | option.starts_with(arg))
                    .collect()
            },
            Some(("sort", _)) => {
                SortUpdateDiscriminants::VARIANTS
                    .iter()
                    .map(|variant| {
                        let name = <_ as Into<&'static str>>::into(variant);
                        format!("sort.{name}")
                    })
                    .filter(|option| option.starts_with(&orig_option) | option.starts_with(arg))
                    .collect()
            },
            Some(("notifications", _)) => {
                NotificationsUpdateDiscriminants::VARIANTS
                    .iter()
                    .flat_map(|variant| {
                        let name = <_ as Into<&'static str>>::into(variant);
                        if variant.get_bool("is_bool") == Some(true) {
                            vec![
                                format!("notifications.no{name}"),
                                format!("notifications.{name}"),
                            ]
                        } else {
                            vec![format!("notifications.{name}")]
                        }
                    })
                    .filter(|option| option.starts_with(&orig_option) | option.starts_with(arg))
                    .collect()
            },
            Some(("users", _)) => {
                let suboption = arg.strip_prefix("users.").unwrap();
                let mut completions = complete_users(suboption, store);

                for completion in &mut completions {
                    completion.insert_str(0, "users.");
                }

                if let Some((user, end)) = suboption.rsplit_once('.') {
                    if UserId::parse(user).and_then(|user| user.validate_strict()).is_ok() {
                        if "name".starts_with(end) {
                            completions.push(format!("users.{user}.name"));
                        }
                        if "color".starts_with(end) {
                            completions.push(format!("users.{user}.color"));
                        }
                    }
                }

                completions
            },
            None => {
                TunablesUpdateDiscriminants::VARIANTS
                    .iter()
                    .flat_map(|variant| {
                        let name = <_ as Into<&'static str>>::into(variant);
                        if variant.get_bool("is_bool") == Some(true) {
                            vec![format!("no{name}"), name.to_string()]
                        } else {
                            vec![name.to_string()]
                        }
                    })
                    .filter(|option| option.starts_with(arg))
                    .collect()
            },
            _ => vec![],
        }
    }
}

/// Tab completion for command arguments.
fn complete_cmdarg(
    desc: CommandDescription,
    input: &EditRope,
    cursor: &mut Cursor,
    store: &ChatStore,
) -> Vec<String> {
    let cmd = match store.cmds.get(desc.command.as_str()) {
        Ok(cmd) => cmd,
        Err(_) => return vec![],
    };

    let Ok((_, (args, to_strip))) = parse::parse_started_strings(&desc.arg.text) else {
        return vec![];
    };
    debug_assert!(!args.is_empty()); // empty string is inserted if text is empty

    // move cursor to start of last arg
    let ctx = CursorMovementsContext {
        action: &Default::default(),
        view: &Default::default(),
        context: &Default::default(),
    };
    let movement = MoveType::Column(MoveDir1D::Previous, true);
    let count = Count::Exact(to_strip.len());
    let Some(new_cursor) = input.movement(cursor, &movement, &count, &ctx) else {
        return vec![];
    };
    let orig_cursor = cursor.clone();
    *cursor = new_cursor;

    let mut completions = match cmd.name.as_str() {
        "invite" => complete_iamb_invite(args, store),
        "invites" => vec![],

        "keys" => complete_iamb_keys(args, input, orig_cursor, cursor),

        "verify" => complete_iamb_verify(args, store),

        // These have no arguments
        "dms" | "members" | "m" | "leave" | "forget" | "cancel" | "edit" | "e" => vec![],

        "react" | "rc" | "reac" | "rct" if args.len() == 1 => complete_emoji(&args[0], store),
        "react" | "rc" | "reac" | "rct" => vec![],

        // TODO: Check whether we can get the id of the focused message to improve completion
        "unreact" | "unr" if args.len() == 1 => complete_emoji(&args[0], store),
        "unreact" | "unr" => vec![],

        // The redaction reason is free text
        "redact" | "red" => vec![],

        // These have no arguments
        "reply" | "rep" | "replied" | "editor" | "ed" | "rooms" | "ro" | "chats" | "c" => vec![],

        "unreads" | "u" => complete_iamb_unreads(args),

        // These have no arguments
        "mentions" | "spaces" | "s" | "welcome" => vec![],

        "join" if args.len() == 1 => complete_matrix_aliases(&args[0], store),
        "join" => vec![],

        "create" => complete_iamb_create(args),

        "room" => complete_iamb_room(args, store),

        // This has no arguments
        "message" => vec![],

        "space" => complete_iamb_space(args, store),

        "upload" | "up" | "download" | "d" | "open" | "o" | "reload" => {
            if input.get_char_at_cursor(cursor) == Some('"') {
                // Use the escaped instead of the qouted filename.
                let mut args = args;
                vec![args.pop().unwrap()]
            } else {
                *cursor = orig_cursor;
                return complete_path(input, cursor);
            }
        },

        "logout" => complete_iamb_logout(args, store),

        "set" => complete_iamb_set(args.last().map(Deref::deref).unwrap_or_default(), store),

        "vertical" | "vert" | "horizontal" | "hor" | "aboveleft" | "lefta" | "leftabove" |
        "abo" | "belowright" | "rightb" | "rightbelow" | "bel" | "tab" => {
            complete_cmd(desc.arg.text.as_str(), input, cursor, store)
        },

        _cmd => {
            #[cfg(test)]
            panic!("trying to complete unknown subcommand `{}`", _cmd);

            #[cfg(not(test))]
            vec![]
        },
    };

    completions.iter_mut().for_each(|completion| {
        if completion.contains(['\\', ' ', '#', '%', '"', '|']) {
            *completion = completion
                .replace('\\', "\\\\")
                .replace(' ', "\\ ")
                .replace('#', "\\#")
                .replace('%', "\\%")
                .replace('"', "\\\"")
                .replace('|', "\\|");
        }
    });

    completions
}

/// Tab completion for command names.
fn complete_cmdname(
    desc: CommandDescription,
    text: &EditRope,
    cursor: &mut Cursor,
    store: &ChatStore,
) -> Vec<String> {
    // Complete command name and set cursor position.
    let _ = text.get_prefix_word_mut(cursor, &WordStyle::Little);
    let mut comps = store.cmds.complete_name(desc.command.as_str());

    comps.extend(store.cmds.complete_aliases(desc.command.as_str()));

    comps
}

/// Tab completion for commands.
fn complete_cmd(cmd: &str, text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    match CommandDescription::from_str(cmd) {
        Ok(desc) => {
            if desc.arg.untrimmed.is_empty() {
                complete_cmdname(desc, text, cursor, store)
            } else {
                // Complete command argument.
                complete_cmdarg(desc, text, cursor, store)
            }
        },

        // Can't parse command text, so return zero completions.
        Err(_) => vec![],
    }
}

/// Tab completion for the command bar.
fn complete_cmdbar(text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    let eo = text.cursor_to_offset(cursor);
    let slice = text.slice(..eo);
    let cow = Cow::from(&slice);

    complete_cmd(cow.as_ref(), text, cursor, store)
}

/// Tab completion within the message bar.
fn complete_msgbar(
    text: &EditRope,
    cursor: &mut Cursor,
    store: &mut ChatStore,
    room_id: &RoomId,
) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    match id.chars().next() {
        // Complete room aliases.
        Some('#') => {
            store
                .aliases
                .complete(id.as_ref())
                .into_iter()
                .map(|i| format!("[{}]({})", i, i.matrix_uri(false)))
                .collect()
        },

        // Complete room identifiers.
        Some('!') => {
            store
                .rooms
                .complete(id.as_ref())
                .into_iter()
                .map(|i| format!("[{}]({})", i, i.matrix_uri(false)))
                .collect()
        },

        // Complete Emoji shortcodes.
        Some(':') => {
            let list = store.emojis.complete(&id[1..]);
            let iter = list.into_iter().take(200).map(|s| format!(":{s}:"));

            iter.collect()
        },

        // Complete usernames for @ and empty strings.
        Some('@') | None => {
            // spec says to mention with display name in anchor text
            let Ok(members) = store.worker.members(room_id.to_owned()) else {
                return vec![];
            };

            let search_name = id.strip_prefix('@').unwrap_or(&id);

            let mut result: Vec<_> = members
                .iter()
                .filter(|member| {
                    member.user_id().as_str().starts_with(&*id) ||
                        member.name().starts_with(search_name)
                })
                .map(|member| {
                    let name = member.display_name().unwrap_or(member.user_id().as_str());
                    let link = member.user_id().matrix_uri(false);
                    format!("[{name}]({link})")
                })
                .collect();

            result.sort_unstable();
            result
        },

        // Unknown sigil.
        Some(_) => vec![],
    }
}

pub struct IambCompleter;

impl Completer<IambInfo> for IambCompleter {
    fn complete(
        &mut self,
        text: &EditRope,
        cursor: &mut Cursor,
        content: &IambBufferId,
        store: &mut ChatStore,
    ) -> Vec<String> {
        match content {
            IambBufferId::Command(CommandType::Command) => complete_cmdbar(text, cursor, store),
            IambBufferId::Room(room_id, _, RoomFocus::MessageBar) => {
                complete_msgbar(text, cursor, store, room_id)
            },
            IambBufferId::Command(CommandType::Search) |
            IambBufferId::Room(_, _, RoomFocus::Scrollback) |
            IambBufferId::DirectList |
            IambBufferId::MemberList(_) |
            IambBufferId::RoomList |
            IambBufferId::SpaceList |
            IambBufferId::SpaceTree |
            IambBufferId::VerifyList |
            IambBufferId::Welcome |
            IambBufferId::ChatList |
            IambBufferId::UnreadList |
            IambBufferId::InviteList => vec![],
            IambBufferId::MentionsList => vec![],
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        base::{ProgramCommand, ProgramCommands},
        commands::add_iamb_commands,
        tests::*,
    };
    use modalkit::{commands::CommandResult, env::vim::command::CommandContext};
    use pretty_assertions::assert_eq;

    #[tokio::test]
    async fn test_complete_msgbar() {
        let store = mock_store().await;
        let mut store = store.application;
        let room_id = TEST_ROOM1_ID.clone();

        let text = EditRope::from("going for a walk :walk ");
        let mut cursor = Cursor::new(0, 22);
        let res = complete_msgbar(&text, &mut cursor, &mut store, &room_id);
        assert_eq!(res, vec![":walking:", ":walking_man:", ":walking_woman:"]);
        assert_eq!(cursor, Cursor::new(0, 17));

        let text = EditRope::from("see #room ");
        let mut cursor = Cursor::new(0, 9);
        let res = complete_msgbar(&text, &mut cursor, &mut store, &room_id);
        assert_eq!(res, vec!["[#room1:example.com](matrix:r/room1:example.com)"]);
        assert_eq!(cursor, Cursor::new(0, 4));
    }

    #[tokio::test]
    async fn test_complete_cmdbar() {
        let store = mock_store().await;
        let store = store.application;
        let cmds = vec!["accept", "reject", "send"];
        let users = vec![
            "@user1:example.com",
            "@user2:example.com",
            "@user3:example.com",
            "@user4:example.com",
            "@user5:example.com",
        ];

        let text = EditRope::from("invite    ");
        let mut cursor = Cursor::new(0, 7);
        let id = text
            .get_prefix_word_mut(&mut cursor, &MATRIX_ID_WORD)
            .unwrap_or_else(EditRope::empty);
        assert_eq!(id.to_string(), "");
        assert_eq!(cursor, Cursor::new(0, 7));

        let text = EditRope::from("invite    ");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, cmds);

        let text = EditRope::from("invite ignored");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, cmds);

        let text = EditRope::from("invite send @user1ignored");
        let mut cursor = Cursor::new(0, 18);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["@user1:example.com"]);

        let text = EditRope::from("abo hori");
        let mut cursor = Cursor::new(0, 8);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["horizontal"]);

        let text = EditRope::from("abo hor inv");
        let mut cursor = Cursor::new(0, 11);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["invite", "invites", "inv"]);

        let text = EditRope::from("abo hor invite send \n");
        let mut cursor = Cursor::new(0, 20);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, users);
    }

    #[tokio::test]
    async fn test_all_commands_complete() {
        let mut cmds = ProgramCommands::new();
        add_iamb_commands(&mut cmds);

        let store = mock_store().await;
        let mut store = store.application;
        store.cmds = cmds;
        let cmds = &store.cmds;

        for command in cmds.complete_name("") {
            let mut text = EditRope::from(command);
            text += " ".into();
            let mut cursor = text.last();
            cursor.right(1);
            complete_cmdbar(&text, &mut cursor, &store);
        }
    }

    fn mock_command(
        _: CommandDescription,
        _: &mut CommandContext,
    ) -> CommandResult<ProgramCommand> {
        panic!("mock command called");
    }

    #[tokio::test]
    #[should_panic(expected = "trying to complete unknown subcommand `testmockcommand`")]
    async fn test_complete_unknown_panics() {
        let mut cmds = ProgramCommands::new();
        cmds.add_command(ProgramCommand {
            name: "testmockcommand".into(),
            aliases: vec![],
            f: mock_command,
        });

        let store = mock_store().await;
        let mut store = store.application;
        store.cmds = cmds;
        let cmds = &store.cmds;

        for command in cmds.complete_name("") {
            let mut text = EditRope::from(command);
            text += " ".into();
            let mut cursor = text.last();
            cursor.right(1);
            complete_cmdbar(&text, &mut cursor, &store);
        }
    }
}
