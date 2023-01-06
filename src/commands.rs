use modalkit::{
    editing::base::OpenTarget,
    env::vim::command::{CommandContext, CommandDescription},
    input::commands::{CommandError, CommandResult, CommandStep},
    input::InputContext,
};

use crate::base::{
    IambAction,
    IambId,
    ProgramCommand,
    ProgramCommands,
    ProgramContext,
    RoomAction,
    SetRoomField,
    VerifyAction,
};

type ProgContext = CommandContext<ProgramContext>;
type ProgResult = CommandResult<ProgramCommand>;

fn iamb_verify(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    match args.len() {
        0 => {
            let open = ctx.switch(OpenTarget::Application(IambId::VerifyList));
            let step = CommandStep::Continue(open, ctx.context.take());

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
                    let step = CommandStep::Continue(iact.into(), ctx.context.take());

                    return Ok(step);
                },
                _ => return Result::Err(CommandError::InvalidArgument),
            };

            let vact = IambAction::Verify(act, args.remove(1));
            let step = CommandStep::Continue(vact.into(), ctx.context.take());

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
    let step = CommandStep::Continue(open, ctx.context.take());

    return Ok(step);
}

fn iamb_members(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = IambAction::Room(RoomAction::Members(ctx.clone().into()));
    let step = CommandStep::Continue(open.into(), ctx.context.take());

    return Ok(step);
}

fn iamb_rooms(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::RoomList));
    let step = CommandStep::Continue(open, ctx.context.take());

    return Ok(step);
}

fn iamb_spaces(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::SpaceList));
    let step = CommandStep::Continue(open, ctx.context.take());

    return Ok(step);
}

fn iamb_welcome(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    if !desc.arg.text.is_empty() {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(OpenTarget::Application(IambId::Welcome));
    let step = CommandStep::Continue(open, ctx.context.take());

    return Ok(step);
}

fn iamb_join(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.filenames()?;

    if args.len() != 1 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let open = ctx.switch(args.remove(0));
    let step = CommandStep::Continue(open, ctx.context.take());

    return Ok(step);
}

fn iamb_set(desc: CommandDescription, ctx: &mut ProgContext) -> ProgResult {
    let mut args = desc.arg.strings()?;

    if args.len() != 2 {
        return Result::Err(CommandError::InvalidArgument);
    }

    let field = args.remove(0);
    let value = args.remove(0);

    let act: IambAction = match field.as_str() {
        "room.name" => RoomAction::Set(SetRoomField::Name(value)).into(),
        "room.topic" => RoomAction::Set(SetRoomField::Topic(value)).into(),
        _ => {
            return Result::Err(CommandError::InvalidArgument);
        },
    };

    let step = CommandStep::Continue(act.into(), ctx.context.take());

    return Ok(step);
}

fn add_iamb_commands(cmds: &mut ProgramCommands) {
    cmds.add_command(ProgramCommand { names: vec!["dms".into()], f: iamb_dms });
    cmds.add_command(ProgramCommand { names: vec!["join".into()], f: iamb_join });
    cmds.add_command(ProgramCommand { names: vec!["members".into()], f: iamb_members });
    cmds.add_command(ProgramCommand { names: vec!["rooms".into()], f: iamb_rooms });
    cmds.add_command(ProgramCommand { names: vec!["set".into()], f: iamb_set });
    cmds.add_command(ProgramCommand { names: vec!["spaces".into()], f: iamb_spaces });
    cmds.add_command(ProgramCommand { names: vec!["verify".into()], f: iamb_verify });
    cmds.add_command(ProgramCommand { names: vec!["welcome".into()], f: iamb_welcome });
}

pub fn setup_commands() -> ProgramCommands {
    let mut cmds = ProgramCommands::default();

    add_iamb_commands(&mut cmds);

    return cmds;
}

#[cfg(test)]
mod tests {
    use super::*;
    use modalkit::editing::action::WindowAction;

    #[test]
    fn test_cmd_verify() {
        let mut cmds = setup_commands();
        let ctx = ProgramContext::default();

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
        let ctx = ProgramContext::default();

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
    fn test_cmd_set() {
        let mut cmds = setup_commands();
        let ctx = ProgramContext::default();

        let res = cmds
            .input_cmd("set room.topic \"Lots of fun discussion!\"", ctx.clone())
            .unwrap();
        let act = IambAction::Room(SetRoomField::Topic("Lots of fun discussion!".into()).into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("set room.topic The\\ Discussion\\ Room", ctx.clone())
            .unwrap();
        let act = IambAction::Room(SetRoomField::Topic("The Discussion Room".into()).into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("set room.topic Development", ctx.clone()).unwrap();
        let act = IambAction::Room(SetRoomField::Topic("Development".into()).into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("set room.name Development", ctx.clone()).unwrap();
        let act = IambAction::Room(SetRoomField::Name("Development".into()).into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds
            .input_cmd("set room.name \"Application Development\"", ctx.clone())
            .unwrap();
        let act = IambAction::Room(SetRoomField::Name("Application Development".into()).into());
        assert_eq!(res, vec![(act.into(), ctx.clone())]);

        let res = cmds.input_cmd("set", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("set room.name", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("set room.topic", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        let res = cmds.input_cmd("set room.topic A B C", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));
    }
}
