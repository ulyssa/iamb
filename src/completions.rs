//! Tab completion for iamb
use std::{borrow::Cow, str::FromStr};

use modalkit::{
    editing::{
        completion::{complete_path, Completer},
        cursor::Cursor,
        rope::EditRope,
    },
    env::vim::command::CommandDescription,
    prelude::{
        CommandType,
        Count,
        CursorMovements,
        CursorMovementsContext,
        MoveDir1D,
        MoveType,
        WordStyle,
    },
};

use crate::base::{ChatStore, IambBufferId, IambInfo, RoomFocus, MATRIX_ID_WORD};

mod parse {
    use nom::{
        branch::alt,
        bytes::complete::{escaped_transform, is_not, tag},
        character::complete::{char, space1},
        combinator::{cut, eof, opt, value},
        error::{ErrorKind, ParseError},
        IResult,
        InputLength,
        Parser,
    };

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
        )(input)
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
        ))(input)?;

        Ok((input, text))
    }

    fn parse_quote(input: &str) -> IResult<&str, String> {
        let (input, text) = parse_unclosed_quote(input)?;
        let (input, _) = char('\"')(input)?;

        Ok((input, text))
    }

    fn parse_string(input: &str) -> IResult<&str, String> {
        alt((parse_quote, parse_text))(input)
    }

    /// Acts linke [`separated_list0`](nom::multi::separated_list0) but additionally returns a copy of the last element unparsed.
    fn separated_list0_last_raw<I, O, O2, E, F, G>(
        mut sep: G,
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, (Vec<O>, I), E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: Parser<I, O2, E>,
        E: ParseError<I>,
    {
        move |mut i: I| {
            let mut res = Vec::new();
            let mut old_i = i.clone();

            match f.parse(i.clone()) {
                Err(nom::Err::Error(_)) => return Ok((i, (res, old_i))),
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    res.push(o);
                    i = i1;
                },
            }

            loop {
                let len = i.input_len();
                match sep.parse(i.clone()) {
                    Err(nom::Err::Error(_)) => return Ok((i, (res, old_i))),
                    Err(e) => return Err(e),
                    Ok((i1, _)) => {
                        // infinite loop check: the parser must always consume
                        if i1.input_len() == len {
                            return Err(nom::Err::Error(E::from_error_kind(
                                i1,
                                ErrorKind::SeparatedList,
                            )));
                        }

                        match f.parse(i1.clone()) {
                            Err(nom::Err::Error(_)) => return Ok((i, (res, old_i))),
                            Err(e) => return Err(e),
                            Ok((i2, o)) => {
                                res.push(o);
                                i = i2;
                                old_i = i1.clone();
                            },
                        }
                    },
                }
            }
        }
    }

    fn parse_last_arg(input: &str) -> IResult<&str, (String, &str)> {
        let (input, _) = space1(input)?;

        let old_input = input;
        let (input, arg) = opt(parse_unclosed_quote)(input)?;

        Ok((input, (arg.unwrap_or_default(), old_input)))
    }

    /// Returns a list with the parsed strings and a raw version of the last string to be stripped
    /// from the input before completing.
    pub fn parse_started_strings(input: &str) -> IResult<&str, (Vec<String>, &str)> {
        let (input, (mut args, mut last_arg_raw)) =
            separated_list0_last_raw(space1, parse_string)(input)?;
        let (input, end_arg) = opt(parse_last_arg)(input)?;
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
    let list = store.names.complete(input);
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
    let subcmds = ["request", "accept", "confirm", "cancel", "missmatch"];
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

    let completions = match cmd.name.as_str() {
        "invite" => complete_iamb_invite(args, store),

        "keys" => complete_iamb_keys(args, input, orig_cursor, cursor),

        "verify" => complete_iamb_verify(args, store),

        // These have no arguments
        "dms" | "members" | "leave" | "forget" | "cancel" | "edit" => vec![],

        "react" if args.len() == 1 => complete_emoji(&args[0], store),
        "react" => vec![],

        // TODO: Check whether we can get the id of the focused message to improve completion
        "unreact" if args.len() == 1 => complete_emoji(&args[0], store),
        "unreact" => vec![],

        // The redaction reason is free text
        "redact" => vec![],

        // These have no arguments
        "reply" | "replied" | "editor" | "rooms" | "chats" => vec![],

        "unreads" => complete_iamb_unreads(args),

        // These have no arguments
        "spaces" | "welcome" => vec![],

        "join" if args.len() == 1 => complete_matrix_aliases(&args[0], store),
        "join" => vec![],

        "create" => complete_iamb_create(args),

        "room" => complete_iamb_room(args, store),

        "space" => complete_iamb_space(args, store),

        "upload" | "download" | "open" => {
            *cursor = orig_cursor;
            complete_path(input, cursor)
        },

        "logout" => complete_iamb_logout(args, store),

        "vertical" | "vert" | "horizontal" | "hor" | "aboveleft" | "lefta" | "leftabove" |
        "abo" | "belowright" | "rightb" | "rightbelow" | "bel" | "tab" => {
            complete_cmd(desc.arg.text.as_str(), input, cursor, store)
        },

        _cmd => {
            #[cfg(test)]
            panic!("trying to complete unknown subcommand `{}`", _cmd);

            vec![]
        },
    };

    // TODO: escape stuff including with paths

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
fn complete_msgbar(text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    match id.chars().next() {
        // Complete room aliases.
        Some('#') => {
            return store.names.complete(id.as_ref());
        },

        // Complete room identifiers.
        Some('!') => {
            return store
                .rooms
                .complete(id.as_ref())
                .into_iter()
                .map(|i| i.to_string())
                .collect();
        },

        // Complete Emoji shortcodes.
        Some(':') => {
            let list = store.emojis.complete(&id[1..]);
            let iter = list.into_iter().take(200).map(|s| format!(":{s}:"));

            return iter.collect();
        },

        // Complete usernames for @ and empty strings.
        Some('@') | None => {
            return store
                .presences
                .complete(id.as_ref())
                .into_iter()
                .map(|i| i.to_string())
                .collect();
        },

        // Unknown sigil.
        Some(_) => return vec![],
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
            IambBufferId::Command(CommandType::Search) => vec![],
            IambBufferId::Room(_, _, RoomFocus::MessageBar) => complete_msgbar(text, cursor, store),
            IambBufferId::Room(_, _, RoomFocus::Scrollback) => vec![],

            IambBufferId::DirectList => vec![],
            IambBufferId::MemberList(_) => vec![],
            IambBufferId::RoomList => vec![],
            IambBufferId::SpaceList => vec![],
            IambBufferId::VerifyList => vec![],
            IambBufferId::Welcome => vec![],
            IambBufferId::ChatList => vec![],
            IambBufferId::UnreadList => vec![],
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
        let store = store.application;

        let text = EditRope::from("going for a walk :walk ");
        let mut cursor = Cursor::new(0, 22);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec![":walking:", ":walking_man:", ":walking_woman:"]);
        assert_eq!(cursor, Cursor::new(0, 17));

        let text = EditRope::from("hello @user1 ");
        let mut cursor = Cursor::new(0, 12);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["@user1:example.com"]);
        assert_eq!(cursor, Cursor::new(0, 6));

        let text = EditRope::from("see #room ");
        let mut cursor = Cursor::new(0, 9);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["#room1:example.com"]);
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
        assert_eq!(res, vec!["invite"]);

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
