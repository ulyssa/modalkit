//! # Vim Commands
//!
//! ## Overview
//!
//! These components allow parsing Vim commands and turning them into
//! [actions](crate::editing::action::Action).
//!
//! ## Default Commands
//!
//! ### `read`
//!
//! *Aliases:* `r`
//!
//! Read the contents of a file or program output into the buffer.
//!
//! ### `print`
//!
//! *Aliases:* `p`
//!
//! Print lines in the given range.
//!
//! ### `substitute`
//!
//! *Aliases:* `s`
//!
//! Replace regular expression matches with a substitution.
//!
//! ### `close`
//!
//! *Aliases:* `clo`
//!
//! Close a window.
//!
//! ### `only`
//!
//! *Aliases:* `on`
//!
//! Close all windows but one.
//!
//! ### `quit`
//!
//! *Aliases:* `q`
//!
//! Quit a window.
//!
//! ### `quitall`
//!
//! *Aliases:* `qa`, `qall`, `quita`
//!
//! Quit all windows in the current tab.
//!
//! ### `split`
//!
//! *Aliases:* `sp`
//!
//! Split the window. If an argument is given, then it will be opened with [OpenTarget::Name].
//!
//! ### `vsplit`
//!
//! *Aliases:* `vs`, `vsp`
//!
//! Split the window vertically. If an argument is given, then it will be opened with
//! [OpenTarget::Name].
//!
//! ### `tabclose`
//!
//! *Aliases:* `tabc`
//!
//! Close a tab.
//!
//! ### `tabedit`
//!
//! *Aliases:* `tabe`, `tabnew`
//!
//! Open a new tab. If an argument is given, then a window will be opened with [OpenTarget::Name]
//! and inserted into the new tab.
//!
//! ### `tabnext`
//!
//! *Aliases:* `tabn`
//!
//! Switch focus to a following tab.
//!
//! ### `tabonly`
//!
//! *Aliases:* `tabo`
//!
//! Close all tabs but one.
//!
//! ### `tabprevious`
//!
//! *Aliases:* `tabp`, `tabNext`, `tabN`
//!
//! Switch focus to a previous tab.
//!
//! ### `tabrewind`
//!
//! *Aliases:* `tabr`, `tabfirst`, `tabfir`
//!
//! Switch focus to the first tab.
//!
//! ### `tablast`
//!
//! *Aliases:* `tabl`
//!
//! Switch focus to the last tab.
//!
//! ### `vertical`
//!
//! *Aliases:* `vert`
//!
//! Modify the following command to open a window vertically.
//!
//! For example, `:vertical split` will behave like `:vsplit`.
//!
//! ### `leftabove`
//!
//! *Aliases:* `lefta`, `aboveleft`, `abo`
//!
//! Modify the following command to open the window before the current one.
//!
//! ### `rightbelow`
//!
//! *Aliases:* `rightb`, `belowright`, `bel`
//!
//! Modify the following command to open the window after the current one.
//!
use std::cmp::Ordering;
use std::fmt;

use crate::input::commands::{Command, CommandError, CommandMachine, CommandStep, InputCmdContext};
use crate::input::InputContext;

use crate::editing::{
    action::{Action, TabAction, WindowAction},
    application::{ApplicationInfo, ApplicationWindowId, EmptyInfo},
    base::{
        Axis,
        CloseFlags,
        CloseTarget,
        Count,
        Flip,
        FocusChange,
        MoveDir1D,
        MovePosition,
        OpenTarget,
        RangeEnding,
        RangeEndingModifier,
        RangeEndingType,
        RangeSpec,
    },
    context::EditContext,
};

use super::VimContext;

mod parse;

pub use self::parse::CommandDescription;

/// Result type for a processed command.
pub type CommandResult<C, I> = Result<CommandStep<VimCommand<C, I>>, CommandError>;

/// Handler for a mapped command.
pub type CommandFunc<C, I> = fn(CommandDescription, &mut CommandContext<C>) -> CommandResult<C, I>;

/// Description of a mapped Vim command.
pub struct VimCommand<C: EditContext, I: ApplicationInfo = EmptyInfo> {
    /// Aliases for this command.
    pub names: Vec<String>,

    /// Function that handles command.
    pub f: CommandFunc<C, I>,
}

impl<C, I> Clone for VimCommand<C, I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn clone(&self) -> Self {
        Self { names: self.names.clone(), f: self.f }
    }
}

impl<C, I> fmt::Debug for VimCommand<C, I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VimCommand")
            .field("names", &self.names)
            .finish_non_exhaustive()
    }
}

impl<C, I> Command for VimCommand<C, I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    type Parsed = CommandDescription;
    type Action = Action<I>;
    type Context = C;
    type CommandContext = CommandContext<C>;

    fn names(&self) -> Vec<String> {
        self.names.clone()
    }

    fn exec(&self, cmd: Self::Parsed, ctx: &mut Self::CommandContext) -> CommandResult<C, I> {
        (self.f)(cmd, ctx)
    }
}

/// Context object passed to each [CommandFunc].
#[non_exhaustive]
pub struct CommandContext<C: EditContext> {
    /// Contextual information from user input.
    pub context: C,

    axis: Option<Axis>,
    rel: Option<MoveDir1D>,
}

impl<C> Clone for CommandContext<C>
where
    C: EditContext,
{
    fn clone(&self) -> Self {
        Self {
            context: self.context.clone(),
            axis: self.axis.clone(),
            rel: self.rel.clone(),
        }
    }
}

impl<C> From<C> for CommandContext<C>
where
    C: EditContext,
{
    fn from(context: C) -> Self {
        CommandContext { axis: None, rel: None, context }
    }
}

impl<C> Default for CommandContext<C>
where
    C: EditContext,
{
    fn default() -> Self {
        unimplemented!();
    }
}

impl<C> InputContext for CommandContext<C>
where
    C: EditContext,
{
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

impl<C: EditContext> InputCmdContext for CommandContext<C> {}

fn sum_mods<C: EditContext>(
    mods: &Vec<RangeEndingModifier>,
    ctx: &C,
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

fn range_to_fc<C: EditContext>(
    range: &RangeSpec,
    wrapdir: bool,
    context: &C,
) -> Result<FocusChange, CommandError> {
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
fn window_range_target<C: EditContext>(
    desc: &CommandDescription,
    ctx: &mut CommandContext<C>,
) -> Result<FocusChange, CommandError> {
    desc.range
        .as_ref()
        .map(|r| range_to_fc(r, false, &ctx.context))
        .unwrap_or(Ok(FocusChange::Current))
}

fn open_target<I: ApplicationWindowId>(
    desc: &CommandDescription,
) -> Result<Option<OpenTarget<I>>, CommandError> {
    let args = desc.arg.filenames()?;

    if args.len() > 1 {
        return Err(CommandError::InvalidArgument);
    }

    Ok(args.into_iter().next())
}

fn window_open_target<I: ApplicationWindowId>(
    desc: &CommandDescription,
) -> Result<OpenTarget<I>, CommandError> {
    Ok(open_target(desc)?.unwrap_or(OpenTarget::Current))
}

fn window_close<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
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

fn window_only<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
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

fn window_quit<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
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

fn window_quitall<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let flags = if desc.bang {
        CloseFlags::QUIT | CloseFlags::FORCE
    } else {
        CloseFlags::QUIT
    };

    let target = CloseTarget::All;
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn window_split_horizontal<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let rel = ctx.rel.unwrap_or(MoveDir1D::Previous);
    let axis = ctx.axis.unwrap_or(Axis::Horizontal);
    let target = window_open_target(&desc)?;
    let action = WindowAction::Split(target, axis, rel, Count::Exact(1));

    Ok(CommandStep::Continue(action.into(), ctx.context.take()))
}

fn window_split_vertical<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let rel = ctx.rel.unwrap_or(MoveDir1D::Previous);
    let axis = ctx.axis.unwrap_or(Axis::Vertical);
    let target = window_open_target(&desc)?;
    let action = WindowAction::Split(target, axis, rel, Count::Exact(1));

    Ok(CommandStep::Continue(action.into(), ctx.context.take()))
}

fn tab_next<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
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
        .map(|r| range_to_fc(&r, true, &ctx.context))
        .unwrap_or(Ok(FocusChange::Direction1D(MoveDir1D::Next, Count::Contextual, true)))?;

    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_prev<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
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

fn tab_first<C: EditContext, I: ApplicationInfo>(
    _: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let change = FocusChange::Position(MovePosition::Beginning);
    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_last<C: EditContext, I: ApplicationInfo>(
    _: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let change = FocusChange::Position(MovePosition::End);
    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn tab_new<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let target = open_target(&desc)?;
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?;
    let action = match target {
        Some(target) => TabAction::Open(target, change),
        None => TabAction::New(change),
    };

    Ok(CommandStep::Continue(action.into(), ctx.context.take()))
}

fn tab_close<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
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

fn tab_only<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
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

fn tab_move<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
        .unwrap_or(Ok(FocusChange::Position(MovePosition::End)))?;
    let action = TabAction::Move(change).into();

    Ok(CommandStep::Continue(action, ctx.context.take()))
}

fn run_split_before<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    ctx.rel = Some(MoveDir1D::Previous);

    Ok(CommandStep::Again(desc.arg.text))
}

fn run_split_after<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    ctx.rel = Some(MoveDir1D::Next);

    Ok(CommandStep::Again(desc.arg.text))
}

fn run_vertical<C: EditContext, I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    ctx.axis = Some(Axis::Vertical);

    Ok(CommandStep::Again(desc.arg.text))
}

fn filter<C: EditContext, I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    Err(CommandError::Error("filtering is not yet implemented".into()))
}

fn read<C: EditContext, I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    Err(CommandError::Error("read is not yet implemented".into()))
}

fn print<C: EditContext, I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    Err(CommandError::Error("print is not yet implemented".into()))
}

fn substitute<C: EditContext, I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    Err(CommandError::Error("substitution is not yet implemented".into()))
}

fn substitute_repeat<C: EditContext, I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext<C>,
) -> CommandResult<C, I> {
    Err(CommandError::Error("substitution repetition is not yet implemented".into()))
}

fn default_cmds<C: EditContext, I: ApplicationInfo>() -> Vec<VimCommand<C, I>> {
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

/// Manages parsing and mapping Vim commands.
pub type VimCommandMachine<C = VimContext, I = EmptyInfo> = CommandMachine<VimCommand<C, I>>;

impl<C: EditContext, I: ApplicationInfo> Default for VimCommandMachine<C, I> {
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

    fn mkcmd() -> (VimCommandMachine<VimContext>, VimContext) {
        let cmds = VimCommandMachine::default();
        let ctx = VimContext::default();

        return (cmds, ctx);
    }

    #[test]
    fn test_empty() {
        let (mut cmds, ctx) = mkcmd();

        let res = cmds.input_cmd("", ctx.clone());
        assert_eq!(res.unwrap().len(), 0);

        let res = cmds.input_cmd("      ", ctx.clone());
        assert_eq!(res.unwrap().len(), 0);

        let res = cmds.input_cmd("::::::", ctx.clone());
        assert_eq!(res.unwrap().len(), 0);
    }

    #[test]
    fn test_unmapped() {
        let (mut cmds, ctx) = mkcmd();

        let res = cmds.input_cmd("unmapped", ctx);
        assert!(matches!(res, Err(CommandError::InvalidCommand(c)) if c == "unmapped"));
    }

    #[test]
    fn test_aliases() {
        let (mut cmds, ctx) = mkcmd();

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
        let (mut cmds, ctx) = mkcmd();

        // Unprefixed split command.
        let expect = vec![(
            WindowAction::Split(OpenTarget::Current, Horizontal, Previous, 1.into()).into(),
            ctx.clone(),
        )];
        let res = cmds.input_cmd("split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should be made vertical.
        let expect = vec![(
            WindowAction::Split(OpenTarget::Current, Vertical, Previous, 1.into()).into(),
            ctx.clone(),
        )];
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
        let (mut cmds, ctx) = mkcmd();

        // Unprefixed split command.
        let act = WindowAction::Split(OpenTarget::Current, Horizontal, Previous, 1.into());
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should be MoveDir1D::Next.
        let act = WindowAction::Split(OpenTarget::Current, Horizontal, Next, 1.into());
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("bel split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should be MoveDir1D::Previous.
        let act = WindowAction::Split(OpenTarget::Current, Horizontal, Previous, 1.into());
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("abo split", ctx.clone());
        assert_eq!(res.unwrap(), expect);
    }
}
