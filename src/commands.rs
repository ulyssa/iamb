//! # Default Commands
//!
//! The command-bar commands are set up here, and iamb-specific commands are defined here. See
//! [modalkit::env::vim::command] for additional Vim commands we pull in.
use std::{convert::TryFrom, str::FromStr as _};

use matrix_sdk::ruma::{
    OwnedMxcUri,
    OwnedRoomId,
    OwnedRoomOrAliasId,
    OwnedUserId,
    RoomVersionId,
    events::tag::TagName,
    profile::{ProfileFieldName, ProfileFieldValue},
};

use modalkit::{
    commands::{CommandError, CommandResult, CommandStep},
    env::vim::command::{CommandContext, CommandDescription, OptionType},
    prelude::{MoveDir1D, OpenTarget},
};

use crate::base::{
    CreateRoomFlags,
    CreateRoomType,
    DownloadFlags,
    HomeserverAction,
    IambAction,
    IambId,
    KeysAction,
    MemberUpdateAction,
    MessageAction,
    ProgramCommand,
    ProgramCommands,
    RoomAction,
    RoomField,
    SendAction,
    SpaceAction,
    VerifyAction,
};

type ProgContext = CommandContext;
type ProgResult = CommandResult<ProgramCommand>;

/// Convert strings the user types into a tag name.
fn tag_name(name: String) -> Result<TagName, CommandError> {
    let tag = match name.to_lowercase().as_str() {
        "fav" | "favorite" | "favourite" | "m.favourite" => TagName::Favorite,
        "low" | "lowpriority" | "low_priority" | "low-priority" | "m.lowpriority" => {
            TagName::LowPriority
        },
        "servernotice" | "server_notice" | "server-notice" | "m.server_notice" => {
            TagName::ServerNotice
        },
        _ => {
            if let Ok(tag) = name.parse() {
                TagName::User(tag)
            } else {
                let msg = format!("Invalid user tag name: {name}");

                return Err(CommandError::Error(msg));
            }
        },
    };

    Ok(tag)
}

fn iamb_invite(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let args = desc.arg.strings()?;

    if args.is_empty() {
        return Err(CommandError::InvalidArgument);
    }

    let ract = match args[0].as_str() {
        "accept" => {
            if args.len() != 1 {
                return Err(CommandError::InvalidArgument);
            }

            RoomAction::InviteAccept
        },
        "reject" => {
            if args.len() != 1 {
                return Err(CommandError::InvalidArgument);
            }

            RoomAction::InviteReject
        },
        "send" => {
            if args.len() != 2 {
                return Err(CommandError::InvalidArgument);
            }

            if let Ok(user) = OwnedUserId::try_from(args[1].as_str()) {
                RoomAction::InviteSend(user)
            } else {
                let msg = format!("Invalid user identifier: {}", args[1]);
                let err = CommandError::Error(msg);

                return Err(err);
            }
        },
        _ => {
            return Err(CommandError::InvalidArgument);
        },
    };

    let iact = IambAction::from(ract);
    let step = CommandStep::Continue(iact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_keys(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() != 3 {
        return Err(CommandError::InvalidArgument);
    }

    let act = args.remove(0);
    let path = args.remove(0);
    let passphrase = args.remove(0);

    let act = match act.as_str() {
        "export" => KeysAction::Export(path, passphrase),
        "import" => KeysAction::Import(path, passphrase),
        _ => return Err(CommandError::InvalidArgument),
    };

    let vact = IambAction::Keys(act);
    let step = CommandStep::Continue(vact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_knock(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() < 2 || args.len() > 3 {
        return Err(CommandError::InvalidArgument);
    }

    let cmd = args.remove(0);
    let arg = args.remove(0);
    let reason = args.pop();

    let act: IambAction = match (cmd.as_str(), arg, reason) {
        // :knock send #room:example.org "reason"
        ("send", alias, reason) => {
            let alias = OwnedRoomOrAliasId::from_str(&alias).map_err(|e| {
                CommandError::Error(format!(
                    "{alias:?} is not a valid alias or room identifier: {e}"
                ))
            })?;
            HomeserverAction::KnockSend(alias, reason).into()
        },

        // :knock accept @user:example.org
        ("accept", user, None) => {
            let user = OwnedUserId::from_str(&user).map_err(|e| {
                CommandError::Error(format!("{user:?} is not a valid user identifier: {e}"))
            })?;
            RoomAction::KnockAccept(user).into()
        },
        ("accept", _, Some(_)) => return Err(CommandError::InvalidArgument),

        // :knock reject @user:example.org "reason"
        ("reject", user, reason) => {
            let user = OwnedUserId::from_str(&user).map_err(|e| {
                CommandError::Error(format!("{user:?} is not a valid user identifier: {e}"))
            })?;
            RoomAction::KnockReject(user, reason).into()
        },

        // :knock ban @user:example.org "reason"
        ("ban", user, reason) => {
            let user = OwnedUserId::from_str(&user).map_err(|e| {
                CommandError::Error(format!("{user:?} is not a valid user identifier: {e}"))
            })?;
            RoomAction::KnockBan(user, reason).into()
        },

        (cmd, _, _) => {
            return Err(CommandError::Error(format!("unrecognized `knock` subcommand: {cmd:?}")));
        },
    };

    let step = CommandStep::Continue(act.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_verify(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    match args.len() {
        0 => {
            let open = ctx.switch(OpenTarget::Application(IambId::VerifyList));
            let step = CommandStep::Continue(open, ctx.context.clone());

            return Ok(step);
        },
        1 => {
            return Result::Err(CommandError::InvalidArgument);
        },
        2 => {
            let act = match args[0].as_str() {
                "accept" => VerifyAction::Accept,
                "cancel" => VerifyAction::Cancel,
                "confirm" => VerifyAction::Confirm,
                "mismatch" => VerifyAction::Mismatch,
                "request" => {
                    let iact = IambAction::VerifyRequest(args.remove(1));
                    let step = CommandStep::Continue(iact.into(), ctx.context.clone());

                    return Ok(step);
                },
                _ => return Result::Err(CommandError::InvalidArgument),
            };

            let vact = IambAction::Verify(act, args.remove(1));
            let step = CommandStep::Continue(vact.into(), ctx.context.clone());

            return Ok(step);
        },
        _ => {
            return Result::Err(CommandError::InvalidArgument);
        },
    }
}

fn iamb_dms(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::DirectList));
    let step = CommandStep::Continue(open, ctx.context.clone());

    return Ok(step);
}

fn iamb_members(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = IambAction::Room(RoomAction::Members(ctx.clone().into()));
    let step = CommandStep::Continue(open.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_leave(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let leave = IambAction::Room(RoomAction::Leave(desc.bang));
    let step = CommandStep::Continue(leave.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_follow(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() > 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let dir = match args.pop().as_deref() {
        None | Some("next") => MoveDir1D::Next,
        Some("prev") | Some("previous") => MoveDir1D::Previous,
        Some(_) => return Result::Err(CommandError::InvalidArgument),
    };

    let context = Box::new(ctx.clone());
    let follow = RoomAction::Follow(context, dir);
    let follow = IambAction::Room(follow);
    let step = CommandStep::Continue(follow.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_forget(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let forget = IambAction::Homeserver(HomeserverAction::Forget);
    let step = CommandStep::Continue(forget.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_cancel(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let mact = IambAction::from(MessageAction::Cancel(desc.bang));
    let step = CommandStep::Continue(mact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_edit(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let mact = IambAction::from(MessageAction::Edit);
    let step = CommandStep::Continue(mact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_react(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() != 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let react = args.remove(0);
    let mact = IambAction::from(MessageAction::React(react, desc.bang));
    let step = CommandStep::Continue(mact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_unreact(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() > 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let reaction = args.pop();
    let mact = IambAction::from(MessageAction::Unreact(reaction, desc.bang));
    let step = CommandStep::Continue(mact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_redact(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let args = desc.arg.strings()?;

    if args.len() > 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let reason = args.into_iter().next();
    let ract = IambAction::from(MessageAction::Redact(reason, desc.bang));
    let step = CommandStep::Continue(ract.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_reply(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let ract = IambAction::from(MessageAction::Reply);
    let step = CommandStep::Continue(ract.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_replied(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let ract = IambAction::from(MessageAction::Replied);
    let step = CommandStep::Continue(ract.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_editor(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let sact = IambAction::from(SendAction::SubmitFromEditor);
    let step = CommandStep::Continue(sact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_rooms(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::RoomList));
    let step = CommandStep::Continue(open, ctx.context.clone());

    return Ok(step);
}

fn iamb_chats(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::ChatList));
    let step = CommandStep::Continue(open, ctx.context.clone());

    return Ok(step);
}

fn iamb_unreads(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() > 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    match args.pop().as_deref() {
        Some("clear") => {
            let clear = IambAction::ClearUnreads;
            let step = CommandStep::Continue(clear.into(), ctx.context.clone());

            return Ok(step);
        },
        Some(_) => return Result::Err(CommandError::InvalidArgument),
        None => {
            let open = ctx.switch(OpenTarget::Application(IambId::UnreadList));
            let step = CommandStep::Continue(open, ctx.context.clone());

            return Ok(step);
        },
    }
}

fn iamb_mentions(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::MentionsList));
    let step = CommandStep::Continue(open, ctx.context.clone());

    return Ok(step);
}

fn iamb_self(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut iter = desc.arg.strings()?.into_iter();
    let field = iter.next().ok_or(CommandError::InvalidArgument)?;
    let action = iter.next().ok_or(CommandError::InvalidArgument)?;
    let arg = iter.next();
    let trailing = iter.collect::<Vec<_>>();

    if !trailing.is_empty() {
        // Reject if we have any trailing arguments:
        return Result::Err(CommandError::InvalidArgument);
    }

    let act: IambAction = match (field.as_str(), action.as_str(), arg) {
        // :self avatar show
        ("avatar", "show", None) => {
            HomeserverAction::ProfileFieldShow(ProfileFieldName::AvatarUrl).into()
        },
        ("avatar", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :self avatar set
        ("avatar", "set", Some(s)) => {
            let url = OwnedMxcUri::from(s.as_str());
            if let Err(e) = url.validate() {
                return Err(CommandError::Error(format!(
                    "{s:?} is not a valid Matrix content URI: {e}"
                )));
            }
            HomeserverAction::ProfileFieldSet(ProfileFieldValue::AvatarUrl(url)).into()
        },
        ("avatar", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :self avatar unset
        ("avatar", "unset", None) => {
            HomeserverAction::ProfileFieldUnset(ProfileFieldName::AvatarUrl).into()
        },
        ("avatar", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :self name show
        ("name" | "nick", "show", None) => {
            HomeserverAction::ProfileFieldShow(ProfileFieldName::DisplayName).into()
        },
        ("name" | "nick", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :self name set
        ("name" | "nick", "set", Some(s)) => {
            HomeserverAction::ProfileFieldSet(ProfileFieldValue::DisplayName(s)).into()
        },
        ("name" | "nick", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :self name unset
        ("name" | "nick", "unset", None) => {
            HomeserverAction::ProfileFieldUnset(ProfileFieldName::DisplayName).into()
        },
        ("name" | "nick", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :self timezone show
        ("timezone" | "tz", "show", None) => {
            HomeserverAction::ProfileFieldShow(ProfileFieldName::TimeZone).into()
        },
        ("timezone" | "tz", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :self timezone set
        ("timezone" | "tz", "set", Some(s)) => {
            HomeserverAction::ProfileFieldSet(ProfileFieldValue::TimeZone(s)).into()
        },
        ("timezone" | "tz", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :self timezone set
        ("timezone" | "tz", "unset", None) => {
            HomeserverAction::ProfileFieldUnset(ProfileFieldName::TimeZone).into()
        },
        ("timezone" | "tz", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // Reject anything we don't recognize:
        (f, a, _) => {
            return Result::Err(CommandError::Error(format!("unrecognized command: {f} {a}")));
        },
    };

    let step = CommandStep::Continue(act.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_spaces(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::SpaceList));
    let step = CommandStep::Continue(open, ctx.context.clone());

    return Ok(step);
}

fn iamb_welcome(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::Welcome));
    let step = CommandStep::Continue(open, ctx.context.clone());

    return Ok(step);
}

fn iamb_join(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.filenames()?;

    if args.len() != 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(args.remove(0));
    let step = CommandStep::Continue(open, ctx.context.clone());

    return Ok(step);
}

fn iamb_create(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let args = desc.arg.options()?;
    let mut flags = CreateRoomFlags::NONE;
    let mut alias = None;
    let mut ct = CreateRoomType::Room;

    for arg in args {
        match arg {
            OptionType::Flag(name, Some(arg)) => {
                match name.as_str() {
                    "alias" => {
                        if alias.is_some() {
                            let msg = "Multiple ++alias arguments are not allowed";
                            let err = CommandError::Error(msg.into());

                            return Err(err);
                        } else {
                            alias = Some(arg);
                        }
                    },
                    _ => return Err(CommandError::InvalidArgument),
                }
            },
            OptionType::Flag(name, None) => {
                match name.as_str() {
                    "public" => flags |= CreateRoomFlags::PUBLIC,
                    "space" => ct = CreateRoomType::Space,
                    "enc" | "encrypted" => flags |= CreateRoomFlags::ENCRYPTED,
                    _ => return Err(CommandError::InvalidArgument),
                }
            },
            OptionType::Positional(_) => {
                let msg = ":create doesn't take any positional arguments";
                let err = CommandError::Error(msg.into());

                return Err(err);
            },
        }
    }

    let hact = HomeserverAction::CreateRoom(alias, ct, flags);
    let iact = IambAction::from(hact);
    let step = CommandStep::Continue(iact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_room(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut iter = desc.arg.strings()?.into_iter();
    let field = iter.next().ok_or(CommandError::InvalidArgument)?;

    if field == "user" {
        return iamb_room_user(iter.collect(), ctx);
    }

    let action = iter.next().ok_or(CommandError::InvalidArgument)?;
    let arg = iter.next();
    let trailing = iter.collect::<Vec<_>>();

    match (field.as_str(), action.as_str()) {
        // Skip check for commands that takes a variable number of arguments:
        ("version", "upgrade") => (),

        // Reject if we have any trailing arguments:
        (_, _) if !trailing.is_empty() => return Result::Err(CommandError::InvalidArgument),
        (_, _) => (),
    }

    let act: IambAction = match (field.as_str(), action.as_str(), arg) {
        // :room dm set
        ("dm", "set", None) => RoomAction::SetDirect(true).into(),
        ("dm", "set", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room dm set
        ("dm", "unset", None) => RoomAction::SetDirect(false).into(),
        ("dm", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room [kick|ban|unban] <user>
        ("kick", u, r) => {
            RoomAction::MemberUpdate(MemberUpdateAction::Kick, u.into(), r, desc.bang).into()
        },
        ("ban", u, r) => {
            RoomAction::MemberUpdate(MemberUpdateAction::Ban, u.into(), r, desc.bang).into()
        },
        ("unban", u, r) => {
            RoomAction::MemberUpdate(MemberUpdateAction::Unban, u.into(), r, desc.bang).into()
        },

        // :room history set <visibility>
        ("history", "set", Some(s)) => RoomAction::Set(RoomField::History, s).into(),
        ("history", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :room history unset
        ("history", "unset", None) => RoomAction::Unset(RoomField::History).into(),
        ("history", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room history show
        ("history", "show", None) => RoomAction::Show(RoomField::History).into(),
        ("history", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room name set <room-name>
        ("name", "set", Some(s)) => RoomAction::Set(RoomField::Name, s).into(),
        ("name", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :room name unset
        ("name", "unset", None) => RoomAction::Unset(RoomField::Name).into(),
        ("name", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room topic set <topic>
        ("topic", "set", Some(s)) => RoomAction::Set(RoomField::Topic, s).into(),
        ("topic", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :room topic unset
        ("topic", "unset", None) => RoomAction::Unset(RoomField::Topic).into(),
        ("topic", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room topic show
        ("topic", "show", None) => RoomAction::Show(RoomField::Topic).into(),
        ("topic", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room tag set <tag-name>
        ("tag", "set", Some(s)) => RoomAction::Set(RoomField::Tag(tag_name(s)?), "".into()).into(),
        ("tag", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :room tag unset <tag-name>
        ("tag", "unset", Some(s)) => RoomAction::Unset(RoomField::Tag(tag_name(s)?)).into(),
        ("tag", "unset", None) => return Result::Err(CommandError::InvalidArgument),

        // :room notify set <notification-level>
        ("notify", "set", Some(s)) => RoomAction::Set(RoomField::NotificationMode, s).into(),
        ("notify", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :room notify unset <notification-level>
        ("notify", "unset", None) => RoomAction::Unset(RoomField::NotificationMode).into(),
        ("notify", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room notify show
        ("notify", "show", None) => RoomAction::Show(RoomField::NotificationMode).into(),
        ("notify", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room version show
        ("version", "show", None) => RoomAction::Show(RoomField::Version).into(),
        ("version", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room version upgrade
        ("version", "upgrade", Some(s)) => {
            let version = RoomVersionId::from_str(&s).map_err(|e| {
                CommandError::Error(format!("{s:?} is not a valid room version: {e}"))
            })?;
            let additional_creators = trailing
                .iter()
                .map(|u| {
                    OwnedUserId::from_str(u).map_err(|e| {
                        let msg = format!("{u:?} is not a valid user identifier: {e}");
                        CommandError::Error(msg)
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            RoomAction::Upgrade(version, additional_creators, desc.bang).into()
        },
        ("version", "upgrade", None) => return Result::Err(CommandError::InvalidArgument),

        // :room aliases show
        ("alias", "show", None) => RoomAction::Show(RoomField::Aliases).into(),
        ("alias", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room aliases unset <alias>
        ("alias", "unset", Some(s)) => RoomAction::Unset(RoomField::Alias(s)).into(),
        ("alias", "unset", None) => return Result::Err(CommandError::InvalidArgument),

        // :room aliases set <alias>
        ("alias", "set", Some(s)) => RoomAction::Set(RoomField::Alias(s), "".into()).into(),
        ("alias", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :room canonicalalias show
        ("canonicalalias" | "canon", "show", None) => {
            RoomAction::Show(RoomField::CanonicalAlias).into()
        },
        ("canonicalalias" | "canon", "show", Some(_)) => {
            return Result::Err(CommandError::InvalidArgument);
        },

        // :room canonicalalias set
        ("canonicalalias" | "canon", "set", Some(s)) => {
            RoomAction::Set(RoomField::CanonicalAlias, s).into()
        },
        ("canonicalalias" | "canon", "set", None) => {
            return Result::Err(CommandError::InvalidArgument);
        },

        // :room canonicalalias unset
        ("canonicalalias" | "canon", "unset", None) => {
            RoomAction::Unset(RoomField::CanonicalAlias).into()
        },
        ("canonicalalias" | "canon", "unset", Some(_)) => {
            return Result::Err(CommandError::InvalidArgument);
        },

        // :room id show
        ("id", "show", None) => RoomAction::Show(RoomField::Id).into(),
        ("id", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room unread set
        ("unread", "set", None) => RoomAction::SetUnread(true).into(),

        // :room unread [unset|clear]
        ("unread", "unset" | "clear", None) => RoomAction::SetUnread(false).into(),

        _ => return Result::Err(CommandError::InvalidArgument),
    };

    let step = CommandStep::Continue(act.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_room_user(args: Vec<String>, ctx: &mut ProgContext) -> ProgResult {
    let mut iter = args.into_iter();
    let field = iter.next().ok_or(CommandError::InvalidArgument)?;
    let action = iter.next().ok_or(CommandError::InvalidArgument)?;
    let arg = iter.next();
    let trailing = iter.collect::<Vec<_>>();

    // Reject if we have any trailing arguments:
    if !trailing.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let act: IambAction = match (field.as_str(), action.as_str(), arg) {
        // :room user name set <name>
        ("name" | "nick", "set", Some(s)) => RoomAction::Set(RoomField::UserName, s).into(),
        ("name" | "nick", "set", None) => return Result::Err(CommandError::InvalidArgument),

        // :room user name unset
        ("name" | "nick", "unset", None) => RoomAction::Unset(RoomField::UserName).into(),
        ("name" | "nick", "unset", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        // :room user name show
        ("name" | "nick", "show", None) => RoomAction::Show(RoomField::UserName).into(),
        ("name" | "nick", "show", Some(_)) => return Result::Err(CommandError::InvalidArgument),

        _ => return Result::Err(CommandError::InvalidArgument),
    };

    let step = CommandStep::Continue(act.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_space(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.options()?;

    if args.len() < 2 {
        return Err(CommandError::InvalidArgument);
    }

    let OptionType::Positional(field) = args.remove(0) else {
        return Err(CommandError::InvalidArgument);
    };
    let OptionType::Positional(action) = args.remove(0) else {
        return Err(CommandError::InvalidArgument);
    };

    let act: IambAction = match (field.as_str(), action.as_str()) {
        // :space child remove
        ("child", "remove") => {
            if !(args.is_empty()) {
                return Err(CommandError::InvalidArgument);
            }
            SpaceAction::RemoveChild.into()
        },
        // :space child set <child>
        ("child", "set") => {
            let mut order = None;
            let mut suggested = false;
            let mut raw_child = None;

            for arg in args {
                match arg {
                    OptionType::Flag(name, Some(arg)) => {
                        match name.as_str() {
                            "order" => {
                                if order.is_some() {
                                    let msg = "Multiple ++order arguments are not allowed";
                                    let err = CommandError::Error(msg.into());

                                    return Err(err);
                                } else {
                                    order = Some(arg);
                                }
                            },
                            _ => return Err(CommandError::InvalidArgument),
                        }
                    },
                    OptionType::Flag(name, None) => {
                        match name.as_str() {
                            "suggested" => suggested = true,
                            _ => return Err(CommandError::InvalidArgument),
                        }
                    },
                    OptionType::Positional(arg) => {
                        if raw_child.is_some() {
                            let msg = "Multiple room arguments are not allowed";
                            let err = CommandError::Error(msg.into());

                            return Err(err);
                        }
                        raw_child = Some(arg);
                    },
                }
            }

            let child = if let Some(child) = raw_child {
                OwnedRoomId::from_str(&child)
                    .map_err(|_| CommandError::Error("Invalid room id specified".into()))?
            } else {
                let msg = "Must specify a room to add";
                return Err(CommandError::Error(msg.into()));
            };

            SpaceAction::SetChild(child, order, suggested).into()
        },
        _ => return Result::Err(CommandError::InvalidArgument),
    };

    let step = CommandStep::Continue(act.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_upload(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() != 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let sact = SendAction::Upload(args.remove(0), None);
    let iact = IambAction::from(sact);
    let step = CommandStep::Continue(iact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_download(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() > 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let mut flags = DownloadFlags::NONE;
    if desc.bang {
        flags |= DownloadFlags::FORCE;
    };
    let mact = MessageAction::Download(args.pop(), flags);
    let iact = IambAction::from(mact);
    let step = CommandStep::Continue(iact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_open(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() > 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let mut flags = DownloadFlags::OPEN;
    if desc.bang {
        flags |= DownloadFlags::FORCE;
    };
    let mact = MessageAction::Download(args.pop(), flags);
    let iact = IambAction::from(mact);
    let step = CommandStep::Continue(iact.into(), ctx.context.clone());

    return Ok(step);
}

fn iamb_logout(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let args = desc.arg.strings()?;

    if args.is_empty() {
        return Result::Err(CommandError::Error("Missing username".to_string()));
    }
    if args.len() != 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let iact = IambAction::from(HomeserverAction::Logout(args[0].clone(), desc.bang));
    let step = CommandStep::Continue(iact.into(), ctx.context.clone());

    return Ok(step);
}

fn add_iamb_commands(cmds: &mut ProgramCommands) {
    cmds.add_command(ProgramCommand {
        name: "cancel".into(),
        aliases: vec![],
        f: iamb_cancel,
    });
    cmds.add_command(ProgramCommand {
        name: "create".into(),
        aliases: vec![],
        f: iamb_create,
    });
    cmds.add_command(ProgramCommand {
        name: "chats".into(),
        aliases: vec![],
        f: iamb_chats,
    });
    cmds.add_command(ProgramCommand { name: "dms".into(), aliases: vec![], f: iamb_dms });
    cmds.add_command(ProgramCommand {
        name: "download".into(),
        aliases: vec![],
        f: iamb_download,
    });
    cmds.add_command(ProgramCommand { name: "open".into(), aliases: vec![], f: iamb_open });
    cmds.add_command(ProgramCommand { name: "edit".into(), aliases: vec![], f: iamb_edit });
    cmds.add_command(ProgramCommand {
        name: "follow".into(),
        aliases: vec![],
        f: iamb_follow,
    });
    cmds.add_command(ProgramCommand {
        name: "forget".into(),
        aliases: vec![],
        f: iamb_forget,
    });
    cmds.add_command(ProgramCommand {
        name: "invite".into(),
        aliases: vec![],
        f: iamb_invite,
    });
    cmds.add_command(ProgramCommand { name: "join".into(), aliases: vec![], f: iamb_join });
    cmds.add_command(ProgramCommand { name: "keys".into(), aliases: vec![], f: iamb_keys });
    cmds.add_command(ProgramCommand {
        name: "knock".into(),
        aliases: vec![],
        f: iamb_knock,
    });
    cmds.add_command(ProgramCommand {
        name: "leave".into(),
        aliases: vec![],
        f: iamb_leave,
    });
    cmds.add_command(ProgramCommand {
        name: "members".into(),
        aliases: vec![],
        f: iamb_members,
    });
    cmds.add_command(ProgramCommand {
        name: "react".into(),
        aliases: vec![],
        f: iamb_react,
    });
    cmds.add_command(ProgramCommand {
        name: "redact".into(),
        aliases: vec![],
        f: iamb_redact,
    });
    cmds.add_command(ProgramCommand {
        name: "reply".into(),
        aliases: vec![],
        f: iamb_reply,
    });
    cmds.add_command(ProgramCommand {
        name: "replied".into(),
        aliases: vec![],
        f: iamb_replied,
    });
    cmds.add_command(ProgramCommand {
        name: "rooms".into(),
        aliases: vec![],
        f: iamb_rooms,
    });
    cmds.add_command(ProgramCommand { name: "room".into(), aliases: vec![], f: iamb_room });
    cmds.add_command(ProgramCommand {
        name: "space".into(),
        aliases: vec![],
        f: iamb_space,
    });
    cmds.add_command(ProgramCommand {
        name: "spaces".into(),
        aliases: vec![],
        f: iamb_spaces,
    });
    cmds.add_command(ProgramCommand {
        name: "unreads".into(),
        aliases: vec![],
        f: iamb_unreads,
    });
    cmds.add_command(ProgramCommand {
        name: "mentions".into(),
        aliases: vec![],
        f: iamb_mentions,
    });
    cmds.add_command(ProgramCommand { name: "self".into(), aliases: vec![], f: iamb_self });
    cmds.add_command(ProgramCommand {
        name: "unreact".into(),
        aliases: vec![],
        f: iamb_unreact,
    });
    cmds.add_command(ProgramCommand {
        name: "upload".into(),
        aliases: vec![],
        f: iamb_upload,
    });
    cmds.add_command(ProgramCommand {
        name: "verify".into(),
        aliases: vec![],
        f: iamb_verify,
    });
    cmds.add_command(ProgramCommand {
        name: "welcome".into(),
        aliases: vec![],
        f: iamb_welcome,
    });
    cmds.add_command(ProgramCommand {
        name: "editor".into(),
        aliases: vec![],
        f: iamb_editor,
    });
    cmds.add_command(ProgramCommand {
        name: "logout".into(),
        aliases: vec![],
        f: iamb_logout,
    });
}

/// Initialize the default command state.
pub fn setup_commands() -> ProgramCommands {
    let mut cmds = ProgramCommands::default();

    add_iamb_commands(&mut cmds);

    return cmds;
}

#[cfg(test)]
mod tests {
    use super::*;
    use matrix_sdk::ruma::{room_id, user_id};
    use modalkit::actions::WindowAction;
    use modalkit::editing::context::EditContext;

    #[test]
    fn test_cmd_verify() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd(":verify", ctx.clone()).unwrap();
        let act = WindowAction::Switch(OpenTarget::Application(IambId::VerifyList));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd(":verify request @user1:example.com", ctx.clone()).unwrap();
        let act = IambAction::VerifyRequest("@user1:example.com".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd(":verify accept @user1:example.com/FOOBAR", ctx.clone())
            .unwrap();
        let act = IambAction::Verify(VerifyAction::Accept, "@user1:example.com/FOOBAR".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd(":verify mismatch @user2:example.com/QUUXBAZ", ctx.clone())
            .unwrap();
        let act = IambAction::Verify(VerifyAction::Mismatch, "@user2:example.com/QUUXBAZ".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd(":verify cancel @user3:example.com/MYDEVICE", ctx.clone())
            .unwrap();
        let act = IambAction::Verify(VerifyAction::Cancel, "@user3:example.com/MYDEVICE".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd(":verify confirm @user4:example.com/GOODDEV", ctx.clone())
            .unwrap();
        let act = IambAction::Verify(VerifyAction::Confirm, "@user4:example.com/GOODDEV".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd(":verify confirm", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd(":verify cancel @user4:example.com MYDEVICE", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd(":verify mismatch a b c d e f", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_join() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("join #foobar:example.com", ctx.clone()).unwrap();
        let act = WindowAction::Switch(OpenTarget::Name("#foobar:example.com".into()));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("join #", ctx.clone()).unwrap();
        let act = WindowAction::Switch(OpenTarget::Alternate);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("join", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("join foo bar", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_invalid() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room foo", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room set topic", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_topic_set() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds
            .input_cmd("room topic set \"Lots of fun discussion!\"", ctx.clone())
            .unwrap();
        let act = RoomAction::Set(RoomField::Topic, "Lots of fun discussion!".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("room topic set The\\ Discussion\\ Room", ctx.clone())
            .unwrap();
        let act = RoomAction::Set(RoomField::Topic, "The Discussion Room".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room topic set Development", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Topic, "Development".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room topic", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room topic set", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room topic set A B C", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_name_invalid() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room name", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room name foo", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_name_set() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room name set Development", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Name, "Development".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("room name set \"Application Development\"", ctx.clone())
            .unwrap();
        let act = RoomAction::Set(RoomField::Name, "Application Development".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room name set", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_name_unset() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room name unset", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Name);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room name unset foo", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_dm_set() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room dm set", ctx.clone()).unwrap();
        let act = RoomAction::SetDirect(true);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room dm set true", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_dm_unset() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room dm unset", ctx.clone()).unwrap();
        let act = RoomAction::SetDirect(false);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room dm unset true", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_tag_set() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room tag set favourite", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::Favorite), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set favorite", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::Favorite), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set fav", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::Favorite), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set low_priority", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::LowPriority), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set low-priority", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::LowPriority), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set low", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::LowPriority), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set servernotice", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::ServerNotice), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set server_notice", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::ServerNotice), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set server_notice", ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::Tag(TagName::ServerNotice), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set u.custom-tag", ctx.clone()).unwrap();
        let act = RoomAction::Set(
            RoomField::Tag(TagName::User("u.custom-tag".parse().unwrap())),
            "".into(),
        );
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag set u.irc", ctx.clone()).unwrap();
        let act =
            RoomAction::Set(RoomField::Tag(TagName::User("u.irc".parse().unwrap())), "".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room tag set", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room tag set unknown", ctx.clone());
        assert_eq!(res, Err(CommandError::Error("Invalid user tag name: unknown".into())));

        let res = cmds.input_cmd("room tag set needs-leading-u-dot", ctx.clone());
        assert_eq!(
            res,
            Err(CommandError::Error("Invalid user tag name: needs-leading-u-dot".into()))
        );
    }

    #[test]
    fn test_cmd_room_tag_unset() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room tag unset favourite", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::Favorite));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset favorite", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::Favorite));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset fav", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::Favorite));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset low_priority", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::LowPriority));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset low-priority", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::LowPriority));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset low", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::LowPriority));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset servernotice", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::ServerNotice));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset server_notice", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::ServerNotice));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset server_notice", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::ServerNotice));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset u.custom-tag", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::User("u.custom-tag".parse().unwrap())));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag unset u.irc", ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::Tag(TagName::User("u.irc".parse().unwrap())));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room tag", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room tag set", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("room tag unset unknown", ctx.clone());
        assert_eq!(res, Err(CommandError::Error("Invalid user tag name: unknown".into())));

        let res = cmds.input_cmd("room tag unset needs-leading-u-dot", ctx.clone());
        assert_eq!(
            res,
            Err(CommandError::Error("Invalid user tag name: needs-leading-u-dot".into()))
        );
    }

    #[test]
    fn test_cmd_room_notification_mode_set() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let cmd = "room notify set mute";
        let res = cmds.input_cmd(cmd, ctx.clone()).unwrap();
        let act = RoomAction::Set(RoomField::NotificationMode, "mute".into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let cmd = "room notify unset";
        let res = cmds.input_cmd(cmd, ctx.clone()).unwrap();
        let act = RoomAction::Unset(RoomField::NotificationMode);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let cmd = "room notify show";
        let res = cmds.input_cmd(cmd, ctx.clone()).unwrap();
        let act = RoomAction::Show(RoomField::NotificationMode);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);
    }

    #[test]
    fn test_cmd_room_id_show() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room id show", ctx.clone()).unwrap();
        let act = RoomAction::Show(RoomField::Id);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room id show foo", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_space_child() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let cmd = "space";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let cmd = "space ++foo bar baz";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let cmd = "space child foo";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_space_child_set() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let cmd = "space child set !roomid:example.org";
        let res = cmds.input_cmd(cmd, ctx.clone()).unwrap();
        let act = SpaceAction::SetChild(room_id!("!roomid:example.org").to_owned(), None, false);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let cmd = "space child set ++order=abcd ++suggested !roomid:example.org";
        let res = cmds.input_cmd(cmd, ctx.clone()).unwrap();
        let act = SpaceAction::SetChild(
            room_id!("!roomid:example.org").to_owned(),
            Some("abcd".into()),
            true,
        );
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let cmd = "space child set ++order=abcd ++order=1234 !roomid:example.org";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(
            res,
            Err(CommandError::Error("Multiple ++order arguments are not allowed".into()))
        );

        let cmd = "space child set !roomid:example.org !otherroom:example.org";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::Error("Multiple room arguments are not allowed".into())));

        let cmd = "space child set ++foo=abcd !roomid:example.org";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let cmd = "space child set ++foo !roomid:example.org";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let cmd = "space child ++order=abcd ++suggested set !roomid:example.org";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let cmd = "space child set foo";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::Error("Invalid room id specified".into())));

        let cmd = "space child set";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::Error("Must specify a room to add".into())));
    }

    #[test]
    fn test_cmd_space_child_remove() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let cmd = "space child remove";
        let res = cmds.input_cmd(cmd, ctx.clone()).unwrap();
        let act = SpaceAction::RemoveChild;
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let cmd = "space child remove foo";
        let res = cmds.input_cmd(cmd, ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_invite() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("invite accept", ctx.clone()).unwrap();
        let act = IambAction::Room(RoomAction::InviteAccept);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("invite reject", ctx.clone()).unwrap();
        let act = IambAction::Room(RoomAction::InviteReject);
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("invite send @user:example.com", ctx.clone()).unwrap();
        let act =
            IambAction::Room(RoomAction::InviteSend(user_id!("@user:example.com").to_owned()));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("invite", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("invite foo", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("invite accept @user:example.com", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("invite reject @user:example.com", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("invite send", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("invite @user:example.com", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_room_kick() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("room kick @user:example.com", ctx.clone()).unwrap();
        let act = IambAction::Room(RoomAction::MemberUpdate(
            MemberUpdateAction::Kick,
            "@user:example.com".into(),
            None,
            false,
        ));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("room! kick @user:example.com", ctx.clone()).unwrap();
        let act = IambAction::Room(RoomAction::MemberUpdate(
            MemberUpdateAction::Kick,
            "@user:example.com".into(),
            None,
            true,
        ));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("room! kick @user:example.com \"reason here\"", ctx.clone())
            .unwrap();
        let act = IambAction::Room(RoomAction::MemberUpdate(
            MemberUpdateAction::Kick,
            "@user:example.com".into(),
            Some("reason here".into()),
            true,
        ));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);
    }

    #[test]
    fn test_cmd_room_ban_unban() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds
            .input_cmd("room! ban @user:example.com \"spam\"", ctx.clone())
            .unwrap();
        let act = IambAction::Room(RoomAction::MemberUpdate(
            MemberUpdateAction::Ban,
            "@user:example.com".into(),
            Some("spam".into()),
            true,
        ));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("room unban @user:example.com \"reconciled\"", ctx.clone())
            .unwrap();
        let act = IambAction::Room(RoomAction::MemberUpdate(
            MemberUpdateAction::Unban,
            "@user:example.com".into(),
            Some("reconciled".into()),
            false,
        ));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);
    }

    #[test]
    fn test_cmd_redact() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("redact", ctx.clone()).unwrap();
        let act = IambAction::Message(MessageAction::Redact(None, false));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("redact!", ctx.clone()).unwrap();
        let act = IambAction::Message(MessageAction::Redact(None, true));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("redact Removed", ctx.clone()).unwrap();
        let act = IambAction::Message(MessageAction::Redact(Some("Removed".into()), false));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("redact \"Removed\"", ctx.clone()).unwrap();
        let act = IambAction::Message(MessageAction::Redact(Some("Removed".into()), false));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("redact Removed Removed", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_keys() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        let res = cmds.input_cmd("keys import /a/b/c pword", ctx.clone()).unwrap();
        let act = IambAction::Keys(KeysAction::Import("/a/b/c".into(), "pword".into()));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("keys export /a/b/c pword", ctx.clone()).unwrap();
        let act = IambAction::Keys(KeysAction::Export("/a/b/c".into(), "pword".into()));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        // Invalid invocations.
        let res = cmds.input_cmd("keys", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("keys import", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("keys import foo", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("keys import foo bar baz", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }

    #[test]
    fn test_cmd_multiple_trailing() {
        let mut cmds = setup_commands();
        let ctx = EditContext::default();

        // Trailing arguments disallowed on commands that don't take any:
        let res = cmds.input_cmd("room version show foo", ctx.clone()).unwrap_err();
        let err = CommandError::InvalidArgument;
        assert_eq!(res, err);

        // Trailing arguments allowed on commands that take them:
        let res = cmds.input_cmd("room version upgrade 12", ctx.clone()).unwrap();
        let act = IambAction::Room(RoomAction::Upgrade(RoomVersionId::V12, vec![], false));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("room version upgrade 12 @foo:example.com", ctx.clone())
            .unwrap();
        let act = IambAction::Room(RoomAction::Upgrade(
            RoomVersionId::V12,
            vec![user_id!("@foo:example.com").to_owned()],
            false,
        ));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("room version upgrade 12 @foo:example.com @bar:example.com", ctx.clone())
            .unwrap();
        let act = IambAction::Room(RoomAction::Upgrade(
            RoomVersionId::V12,
            vec![
                user_id!("@foo:example.com").to_owned(),
                user_id!("@bar:example.com").to_owned(),
            ],
            false,
        ));
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        // But the command must take *some* arguments:
        let res = cmds.input_cmd("room version upgrade", ctx.clone()).unwrap_err();
        let err = CommandError::InvalidArgument;
        assert_eq!(res, err);
    }
}
