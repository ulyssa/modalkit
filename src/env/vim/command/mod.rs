//! # Vim Commands
//!
//! ## Overview
//!
//! These components allow parsing Vim commands and turning them into
//! [actions](crate::editing::base::Action).
use std::cmp::Ordering;
use std::fmt;

use crate::input::commands::{Command, CommandError, CommandMachine, CommandStep, InputCmdContext};
use crate::input::InputContext;

use crate::editing::base::{
    Action,
    Application,
    Axis,
    CloseFlags,
    CloseTarget,
    Count,
    Flip,
    FocusChange,
    MoveDir1D,
    MovePosition,
    RangeEnding,
    RangeEndingModifier,
    RangeEndingType,
    RangeSpec,
    Resolve,
    TabAction,
    WindowAction,
};

use super::VimContext;

mod parse;

pub use self::parse::CommandDescription;

/// Result type for a processed command.
pub type CommandResult<P, T = CommandStep<VimCommand<P>>> = Result<T, CommandError<VimCommand<P>>>;

/// Handler for a mapped command.
pub type CommandFunc<P> = fn(CommandDescription, &mut CommandContext<P>) -> CommandResult<P>;

/// Description of a mapped Vim command.
#[derive(Clone)]
pub struct VimCommand<P: Application = ()> {
    /// Aliases for this command.
    pub names: Vec<String>,

    /// Function that handles command.
    pub f: CommandFunc<P>,
}

impl<P: Application> fmt::Debug for VimCommand<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VimCommand")
            .field("names", &self.names)
            .finish_non_exhaustive()
    }
}

impl<P: Application> Command for VimCommand<P> {
    type Parsed = CommandDescription;
    type Action = Action<P>;
    type Context = VimContext<P>;
    type CommandContext = CommandContext<P>;

    fn names(&self) -> Vec<String> {
        self.names.clone()
    }

    fn exec(&self, cmd: Self::Parsed, ctx: &mut Self::CommandContext) -> CommandResult<P> {
        (self.f)(cmd, ctx)
    }
}

/// Context object passed to each [CommandFunc].
#[derive(Clone)]
#[non_exhaustive]
pub struct CommandContext<P: Application> {
    /// Contextual information from user input.
    pub context: Box<VimContext<P>>,

    axis: Option<Axis>,
    rel: Option<MoveDir1D>,
}

impl<P: Application> CommandContext<P> {}

impl<P: Application> From<VimContext<P>> for CommandContext<P> {
    fn from(ctx: VimContext<P>) -> Self {
        CommandContext { axis: None, rel: None, context: Box::new(ctx) }
    }
}

impl<P: Application> Default for CommandContext<P> {
    fn default() -> Self {
        unimplemented!();
    }
}

impl<P: Application> InputContext for CommandContext<P> {
    fn overrides(&mut self, _: &Self) {
        unimplemented!();
    }

    fn reset(&mut self) {
        unimplemented!();
    }

    fn take(&mut self) -> Self {
        unimplemented!();
    }
}

impl<P: Application> InputCmdContext for CommandContext<P> {}

fn sum_mods<P: Application>(
    mods: &Vec<RangeEndingModifier>,
    ctx: &VimContext<P>,
) -> Option<(MoveDir1D, usize)> {
    let mut dir = MoveDir1D::Next;
    let mut off = 0;

    for modifier in mods.iter() {
        match modifier {
            RangeEndingModifier::Offset(md, count) => {
                let count = ctx.resolve(count);

                match (*md == dir, count.cmp(&off)) {
                    (true, _) => {
                        off += count;
                    },
                    (false, Ordering::Less) => {
                        dir = dir.flip();
                        off -= count;
                    },
                    (false, Ordering::Equal) => {
                        dir = dir.flip();
                        off = 0;
                    },
                    (false, Ordering::Greater) => {
                        dir = dir.flip();
                        off = count - off;
                    },
                }
            },
        }
    }

    Some((dir, off))
}

fn range_to_fc<P: Application>(
    range: &RangeSpec,
    wrapdir: bool,
    context: &VimContext<P>,
) -> CommandResult<P, FocusChange> {
    let fc = match range {
        RangeSpec::Single(RangeEnding(RangeEndingType::Absolute(count), mods)) => {
            let count = context.resolve(count);

            match sum_mods(mods, context) {
                Some((MoveDir1D::Previous, off)) => {
                    FocusChange::Offset(count.saturating_sub(off).into(), false)
                },
                Some((MoveDir1D::Next, off)) => {
                    FocusChange::Offset(count.saturating_add(off).into(), false)
                },
                None => {
                    return Err(CommandError::InvalidRange);
                },
            }
        },
        RangeSpec::Single(RangeEnding(
            RangeEndingType::Unspecified | RangeEndingType::Current,
            mods,
        )) => {
            if let Some((dir, off)) = sum_mods(mods, context) {
                FocusChange::Direction1D(dir, Count::Exact(off), wrapdir)
            } else {
                return Err(CommandError::InvalidRange);
            }
        },
        RangeSpec::Single(RangeEnding(RangeEndingType::Last, mods)) => {
            if mods.len() != 0 {
                return Err(CommandError::InvalidRange);
            }

            FocusChange::Position(MovePosition::End)
        },
        _ => {
            return Err(CommandError::InvalidRange);
        },
    };

    return Ok(fc);
}

/// Interpret the range provided to a window command.
///
/// If no range is specified, then act on the current window by default.
fn window_range_target<P: Application>(
    desc: &CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P, FocusChange> {
    desc.range
        .as_ref()
        .map(|r| range_to_fc(r, false, &*ctx.context))
        .unwrap_or(Ok(FocusChange::Current))
}

fn window_close<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let flags = if desc.bang {
        CloseFlags::FORCE
    } else {
        CloseFlags::NONE
    };

    let focus = window_range_target(&desc, ctx)?;
    let target = CloseTarget::Single(focus);
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn window_only<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let flags = if desc.bang {
        CloseFlags::QUIT | CloseFlags::FORCE
    } else {
        CloseFlags::QUIT
    };

    let focus = window_range_target(&desc, ctx)?;
    let target = CloseTarget::AllBut(focus);
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn window_quit<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let flags = if desc.bang {
        CloseFlags::QUIT | CloseFlags::FORCE
    } else {
        CloseFlags::QUIT
    };

    let focus = window_range_target(&desc, ctx)?;
    let target = CloseTarget::Single(focus);
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn window_quitall<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let flags = if desc.bang {
        CloseFlags::QUIT | CloseFlags::FORCE
    } else {
        CloseFlags::QUIT
    };

    let target = CloseTarget::All;
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn window_split_horizontal<P: Application>(
    _: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let rel = ctx.rel.unwrap_or(MoveDir1D::Previous);
    let axis = ctx.axis.unwrap_or(Axis::Horizontal);
    let action = WindowAction::Split(axis, rel, Count::Exact(1)).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn window_split_vertical<P: Application>(
    _: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let rel = ctx.rel.unwrap_or(MoveDir1D::Previous);
    let axis = ctx.axis.unwrap_or(Axis::Vertical);
    let action = WindowAction::Split(axis, rel, Count::Exact(1)).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_next<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let range = match (desc.range, desc.arg.text.len() > 0) {
        (None, true) => {
            if let Ok((_, range)) = desc.arg.range() {
                Some(range)
            } else {
                return Err(CommandError::InvalidArgument);
            }
        },
        (Some(_), true) => {
            return Err(CommandError::InvalidArgument);
        },
        (None, false) => None,
        (r @ Some(_), false) => r,
    };

    let change = range
        .map(|r| range_to_fc(&r, true, &*ctx.context))
        .unwrap_or(Ok(FocusChange::Direction1D(MoveDir1D::Next, Count::Contextual, true)))?;

    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_prev<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let range = match (desc.range, desc.arg.text.len() > 0) {
        (None, true) => {
            if let Ok((_, range)) = desc.arg.range() {
                Some(range)
            } else {
                return Err(CommandError::InvalidArgument);
            }
        },
        (Some(_), true) => {
            return Err(CommandError::InvalidArgument);
        },
        (None, false) => None,
        (r @ Some(_), false) => r,
    };

    let change = match range {
        Some(RangeSpec::Single(RangeEnding(RangeEndingType::Absolute(count), mods))) => {
            if mods.len() != 0 {
                return Err(CommandError::InvalidRange);
            }

            // Move back {count} pages.
            FocusChange::Direction1D(MoveDir1D::Previous, count, true)
        },
        Some(_) => {
            return Err(CommandError::InvalidRange);
        },
        None => {
            // Focus on the previous tab by default.
            FocusChange::Direction1D(MoveDir1D::Previous, Count::Contextual, true)
        },
    };

    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_first<P: Application>(
    _: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let change = FocusChange::Position(MovePosition::Beginning);
    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_last<P: Application>(
    _: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let change = FocusChange::Position(MovePosition::End);
    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_new<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &*ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?;
    let action = TabAction::Open(change);

    Ok(CommandStep::Continue(action.into(), ctx.context.take()))
}

fn tab_close<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &*ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?;
    let target = CloseTarget::Single(change);
    let flags = if desc.bang {
        CloseFlags::FQ
    } else {
        CloseFlags::QUIT
    };
    let action = TabAction::Close(target, flags);

    Ok(CommandStep::Continue(action.into(), ctx.context.take()))
}

fn tab_only<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &*ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?;
    let target = CloseTarget::AllBut(change);
    let flags = if desc.bang {
        CloseFlags::FQ
    } else {
        CloseFlags::QUIT
    };
    let action = TabAction::Close(target, flags);

    Ok(CommandStep::Continue(action.into(), ctx.context.take()))
}

fn tab_move<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &*ctx.context))
        .unwrap_or(Ok(FocusChange::Position(MovePosition::End)))?;
    let action = TabAction::Move(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn run_split_before<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    ctx.rel = Some(MoveDir1D::Previous);

    Ok(CommandStep::Again(desc.arg.text))
}

fn run_split_after<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    ctx.rel = Some(MoveDir1D::Next);

    Ok(CommandStep::Again(desc.arg.text))
}

fn run_vertical<P: Application>(
    desc: CommandDescription,
    ctx: &mut CommandContext<P>,
) -> CommandResult<P> {
    ctx.axis = Some(Axis::Vertical);

    Ok(CommandStep::Again(desc.arg.text))
}

fn filter<P: Application>(_: CommandDescription, _: &mut CommandContext<P>) -> CommandResult<P> {
    Err(CommandError::Error("filtering is not yet implemented".into()))
}

fn read<P: Application>(_: CommandDescription, _: &mut CommandContext<P>) -> CommandResult<P> {
    Err(CommandError::Error("read is not yet implemented".into()))
}

fn print<P: Application>(_: CommandDescription, _: &mut CommandContext<P>) -> CommandResult<P> {
    Err(CommandError::Error("print is not yet implemented".into()))
}

fn substitute<P: Application>(
    _: CommandDescription,
    _: &mut CommandContext<P>,
) -> CommandResult<P> {
    Err(CommandError::Error("substitution is not yet implemented".into()))
}

fn substitute_repeat<P: Application>(
    _: CommandDescription,
    _: &mut CommandContext<P>,
) -> CommandResult<P> {
    Err(CommandError::Error("substitution repetition is not yet implemented".into()))
}

fn default_cmds<P: Application>() -> Vec<VimCommand<P>> {
    vec![
        VimCommand { names: strs!["!"], f: filter },
        VimCommand {
            names: strs!["&", "&&", "~", "~&"],
            f: substitute_repeat,
        },
        VimCommand { names: strs!["r", "read"], f: read },
        VimCommand { names: strs!["p", "print"], f: print },
        VimCommand { names: strs!["s", "substitute"], f: substitute },
        VimCommand { names: strs!["clo", "close"], f: window_close },
        VimCommand { names: strs!["on", "only"], f: window_only },
        VimCommand { names: strs!["q", "quit"], f: window_quit },
        VimCommand {
            names: strs!["qa", "qall", "quita", "quitall"],
            f: window_quitall,
        },
        VimCommand {
            names: strs!["sp", "split"],
            f: window_split_horizontal,
        },
        VimCommand {
            names: strs!["vs", "vsp", "vsplit"],
            f: window_split_vertical,
        },
        VimCommand { names: strs!["tabc", "tabclose"], f: tab_close },
        VimCommand {
            names: strs!["tabe", "tabedit", "tabnew"],
            f: tab_new,
        },
        VimCommand { names: strs!["tabm", "tabmove"], f: tab_move },
        VimCommand { names: strs!["tabn", "tabnext"], f: tab_next },
        VimCommand { names: strs!["tabo", "tabonly"], f: tab_only },
        VimCommand {
            names: strs!["tabp", "tabprevious", "tabN", "tabNext"],
            f: tab_prev,
        },
        VimCommand {
            names: strs!["tabr", "tabrewind", "tabfir", "tabfirst"],
            f: tab_first,
        },
        VimCommand { names: strs!["tabl", "tablast"], f: tab_last },
        VimCommand { names: strs!["vert", "vertical"], f: run_vertical },
        VimCommand {
            names: strs!["lefta", "leftabove", "abo", "aboveleft"],
            f: run_split_before,
        },
        VimCommand {
            names: strs!["rightb", "rightbelow", "bel", "belowright"],
            f: run_split_after,
        },
    ]
}

/// Manage parsing and mapping Vim commands.
pub type VimCommandMachine<P = ()> = CommandMachine<VimCommand<P>>;

impl<P: Application> Default for VimCommandMachine<P> {
    fn default() -> Self {
        let mut m = Self::new();

        for cmd in default_cmds().into_iter() {
            m.add_command(cmd);
        }

        return m;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::editing::base::Axis::{Horizontal, Vertical};
    use crate::editing::base::MoveDir1D::{Next, Previous};

    fn mkcmd() -> (VimCommandMachine, VimContext) {
        let cmds = VimCommandMachine::default();
        let ctx = VimContext::default();

        return (cmds, ctx);
    }

    #[test]
    fn test_empty() {
        let (cmds, ctx) = mkcmd();

        let res = cmds.input_cmd("", ctx.clone());
        assert_eq!(res.unwrap().len(), 0);

        let res = cmds.input_cmd("      ", ctx.clone());
        assert_eq!(res.unwrap().len(), 0);

        let res = cmds.input_cmd("::::::", ctx.clone());
        assert_eq!(res.unwrap().len(), 0);
    }

    #[test]
    fn test_unmapped() {
        let (cmds, ctx) = mkcmd();

        let res = cmds.input_cmd("unmapped", ctx);
        assert!(matches!(res, Err(CommandError::InvalidCommand(c)) if c == "unmapped"));
    }

    #[test]
    fn test_aliases() {
        let (cmds, ctx) = mkcmd();

        // Check that "q" and "quit" return the same Action.
        let act: Action =
            WindowAction::Close(CloseTarget::Single(FocusChange::Current), CloseFlags::QUIT).into();
        let res = cmds.input_cmd("q", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);

        let res = cmds.input_cmd("quit", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);

        // Check that "qa", "qall", "quita" and "quitall" return the same Actions, different from
        // the "q"/"quit" actions.
        let act: Action = WindowAction::Close(CloseTarget::All, CloseFlags::QUIT).into();
        let res = cmds.input_cmd("qa", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);

        let res = cmds.input_cmd("qall", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);

        let res = cmds.input_cmd("quita", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);

        let res = cmds.input_cmd("quitall", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);
    }

    #[test]
    fn test_split_direction() {
        let (cmds, ctx) = mkcmd();

        // Unprefixed split command.
        let expect =
            vec![(WindowAction::Split(Horizontal, Previous, 1.into()).into(), ctx.clone())];
        let res = cmds.input_cmd("split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should be made vertical.
        let expect = vec![(WindowAction::Split(Vertical, Previous, 1.into()).into(), ctx.clone())];
        let res = cmds.input_cmd("vertical split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Unprefixed vsplit command.
        let res = cmds.input_cmd("vsplit", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should remain vertical.
        let res = cmds.input_cmd("vertical split", ctx.clone());
        assert_eq!(res.unwrap(), expect);
    }

    #[test]
    fn test_split_side() {
        let (cmds, ctx) = mkcmd();

        // Unprefixed split command.
        let expect =
            vec![(WindowAction::Split(Horizontal, Previous, 1.into()).into(), ctx.clone())];
        let res = cmds.input_cmd("split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should be MoveDir1D::Next.
        let expect = vec![(WindowAction::Split(Horizontal, Next, 1.into()).into(), ctx.clone())];
        let res = cmds.input_cmd("bel split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should be MoveDir1D::Previous.
        let expect =
            vec![(WindowAction::Split(Horizontal, Previous, 1.into()).into(), ctx.clone())];
        let res = cmds.input_cmd("abo split", ctx.clone());
        assert_eq!(res.unwrap(), expect);
    }
}
