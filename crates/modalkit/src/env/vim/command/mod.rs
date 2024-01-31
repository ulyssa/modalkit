//! # Vim Commands
//!
//! ## Overview
//!
//! These components allow parsing Vim commands and turning them into
//! [actions](crate::editing::action::Action).
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;

use crate::commands::{Command, CommandError, CommandMachine, CommandStep};

use crate::editing::{
    action::{Action, TabAction, WindowAction},
    application::{ApplicationInfo, ApplicationWindowId, EmptyInfo},
    completion::complete_path,
    context::{EditContext, Resolve},
    cursor::Cursor,
    rope::EditRope,
};
use crate::prelude::*;

mod parse;

pub use self::parse::{CommandArgument, CommandDescription, OptionType};

/// Result type for a processed command.
pub type CommandResult<I> = Result<CommandStep<VimCommand<I>>, CommandError>;

/// Handler for a mapped command.
pub type CommandFunc<I> = fn(CommandDescription, &mut CommandContext) -> CommandResult<I>;

/// Description of a mapped Vim command.
pub struct VimCommand<I: ApplicationInfo = EmptyInfo> {
    /// Primary name of this command.
    pub name: String,

    /// Additional names for this command.
    pub aliases: Vec<String>,

    /// Function that handles command.
    pub f: CommandFunc<I>,
}

impl<I> Clone for VimCommand<I>
where
    I: ApplicationInfo,
{
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            aliases: self.aliases.clone(),
            f: self.f,
        }
    }
}

impl<I> fmt::Debug for VimCommand<I>
where
    I: ApplicationInfo,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VimCommand")
            .field("name", &self.name)
            .field("aliases", &self.aliases)
            .finish_non_exhaustive()
    }
}

impl<I> Command for VimCommand<I>
where
    I: ApplicationInfo,
{
    type Parsed = CommandDescription;
    type Action = Action<I>;
    type Context = EditContext;
    type CommandContext = CommandContext;

    fn name(&self) -> String {
        self.name.clone()
    }

    fn aliases(&self) -> Vec<String> {
        self.aliases.clone()
    }

    fn exec(&self, cmd: Self::Parsed, ctx: &mut Self::CommandContext) -> CommandResult<I> {
        (self.f)(cmd, ctx)
    }
}

/// Context object passed to each [CommandFunc].
#[derive(Clone, Debug)]
pub struct CommandContext {
    /// Contextual information from user input.
    pub context: EditContext,

    tab: Option<FocusChange>,
    axis: Option<Axis>,
    rel: Option<MoveDir1D>,

    axis_default: Axis,
    rel_default: MoveDir1D,
}

impl CommandContext {
    /// Indicate a default [Axis] to use when one hasn't been specified via a prefix command (e.g.
    /// `:vertical`).
    pub fn default_axis(&mut self, axis: Axis) -> &mut Self {
        self.axis_default = axis;
        self
    }

    /// Indicate a default [MoveDir1D] value to use when one hasn't been specified via a prefix command (e.g.
    /// `:aboveleft`).
    pub fn default_relation(&mut self, relation: MoveDir1D) -> &mut Self {
        self.rel_default = relation;
        self
    }

    /// Get the contextual [Axis] for newly opened windows.
    pub fn axis(&self) -> Axis {
        self.axis.unwrap_or(self.axis_default)
    }

    /// Get the contextual [MoveDir1D] relation for newly opened windows.
    pub fn relation(&self) -> MoveDir1D {
        self.rel.unwrap_or(self.rel_default)
    }

    /// Use the contextual information to create a new [WindowAction::Open] for the given target
    /// and size.
    pub fn open<I>(&self, target: OpenTarget<I::WindowId>, count: Count) -> Action<I>
    where
        I: ApplicationInfo,
    {
        let rel = self.rel.unwrap_or(self.rel_default);
        let axis = self.axis.unwrap_or(self.axis_default);

        WindowAction::Open(target, axis, rel, count).into()
    }

    /// Use the contextual information to create a new [WindowAction::Split] for the given target
    /// and number of splits.
    pub fn split<I>(&self, target: OpenTarget<I::WindowId>, count: Count) -> Action<I>
    where
        I: ApplicationInfo,
    {
        let rel = self.rel.unwrap_or(self.rel_default);
        let axis = self.axis.unwrap_or(self.axis_default);

        WindowAction::Split(target, axis, rel, count).into()
    }

    /// If no contextual information has been provided, create a new [WindowAction::Switch] for the
    /// given target. If contextual information has been provided via commands like `:tab`,
    /// `:vertical`, or `:aboveleft`, then open the target in a new window instead.
    ///
    /// This is meant to be a more user-friendly way to handle commands that switch to a new window
    /// by doing what the user like wanted when they used a prefix command.
    pub fn switch<I>(&self, target: OpenTarget<I::WindowId>) -> Action<I>
    where
        I: ApplicationInfo,
    {
        if let Some(fc) = &self.tab {
            TabAction::Open(target, fc.clone()).into()
        } else if self.axis.is_some() || self.rel.is_some() {
            self.split(target, 1.into())
        } else {
            WindowAction::Switch(target).into()
        }
    }

    /// Open a new window for the given target.
    ///
    /// If `size` is specified, the window will be opened with that width or height.
    pub fn window<I>(&self, target: OpenTarget<I::WindowId>, size: Option<Count>) -> Action<I>
    where
        I: ApplicationInfo,
    {
        if let Some(fc) = &self.tab {
            TabAction::Open(target, fc.clone()).into()
        } else if let Some(size) = size {
            self.open(target, size)
        } else {
            self.split(target, 1.into())
        }
    }
}

impl From<EditContext> for CommandContext {
    fn from(context: EditContext) -> Self {
        CommandContext {
            context,

            tab: None,
            axis: None,
            rel: None,

            axis_default: Axis::Horizontal,
            rel_default: MoveDir1D::Previous,
        }
    }
}

impl Default for CommandContext {
    fn default() -> Self {
        Self::from(EditContext::default())
    }
}

fn sum_mods(mods: &[RangeEndingModifier], ctx: &EditContext) -> Option<(MoveDir1D, usize)> {
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

fn range_to_count(range: &RangeSpec, context: &EditContext) -> Result<Option<Count>, CommandError> {
    let count = match range {
        RangeSpec::Single(RangeEnding(RangeEndingType::Absolute(count), mods)) |
        RangeSpec::Double(_, RangeEnding(RangeEndingType::Absolute(count), mods), _) => {
            let count = context.resolve(count);

            match sum_mods(mods, context) {
                Some((MoveDir1D::Previous, off)) => Count::Exact(count.saturating_sub(off).max(1)),
                Some((MoveDir1D::Next, off)) => Count::Exact(count.saturating_add(off).max(1)),
                None => {
                    return Err(CommandError::InvalidRange);
                },
            }
        },

        RangeSpec::Single(RangeEnding(_, _)) | RangeSpec::Double(_, RangeEnding(_, _), _) => {
            usize::MAX.into()
        },
    };

    return Ok(Some(count));
}

fn range_to_fc(
    range: &RangeSpec,
    wrapdir: bool,
    context: &EditContext,
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
            if mods.is_empty() {
                FocusChange::Position(MovePosition::End)
            } else {
                return Err(CommandError::InvalidRange);
            }
        },
        _ => {
            return Err(CommandError::InvalidRange);
        },
    };

    return Ok(fc);
}

/// Interpret the range as an optional window height or width.
fn window_size(
    desc: &CommandDescription,
    ctx: &mut CommandContext,
) -> Result<Option<Count>, CommandError> {
    desc.range
        .as_ref()
        .map(|r| range_to_count(r, &ctx.context))
        .unwrap_or(Ok(None))
}

/// Interpret the range provided to a window command.
///
/// If no range is specified, then act on the current window by default.
fn window_range_target(
    desc: &CommandDescription,
    ctx: &mut CommandContext,
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

/// The `:close` command.
///
/// *Aliases:* `clo`
///
/// Close a window.
pub fn vim_cmd_close<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let flags = if desc.bang {
        CloseFlags::FORCE
    } else {
        CloseFlags::NONE
    };

    let focus = window_range_target(&desc, ctx)?;
    let target = WindowTarget::Single(focus);
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:only` command.
///
/// *Aliases:* `on`
///
/// Close all windows but one.
pub fn vim_cmd_only<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let flags = if desc.bang {
        CloseFlags::QUIT | CloseFlags::FORCE
    } else {
        CloseFlags::QUIT
    };

    let focus = window_range_target(&desc, ctx)?;
    let target = WindowTarget::AllBut(focus);
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:quit` command.
///
/// *Aliases:* `q`
///
/// Quit a window.
pub fn vim_cmd_quit<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let flags = if desc.bang {
        CloseFlags::QUIT | CloseFlags::FORCE
    } else {
        CloseFlags::QUIT
    };

    let focus = window_range_target(&desc, ctx)?;
    let target = WindowTarget::Single(focus);
    let action = WindowAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:quitall` command.
///
/// *Aliases:* `qa`, `qall`, `quita`
///
/// Quit all windows in the current tab.
pub fn vim_cmd_quitall<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let flags = if desc.bang {
        CloseFlags::QUIT | CloseFlags::FORCE
    } else {
        CloseFlags::QUIT
    };

    let target = TabTarget::All;
    let action = TabAction::Close(target, flags).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:resize` command.
///
/// *Aliases:* `res`
///
/// Resize the current window.
pub fn vim_cmd_resize<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    if desc.arg.text.is_empty() {
        return Err(CommandError::InvalidArgument);
    }

    let RangeSpec::Single(range) = desc.arg.range()? else {
        return Err(CommandError::InvalidRange);
    };

    let change = match range {
        RangeEnding(RangeEndingType::Absolute(count), mods) => {
            let count = ctx.context.resolve(&count);
            let count = match sum_mods(&mods, &ctx.context) {
                Some((MoveDir1D::Previous, off)) => count.saturating_sub(off),
                Some((MoveDir1D::Next, off)) => count.saturating_add(off),
                None => {
                    return Err(CommandError::InvalidArgument);
                },
            };
            SizeChange::Exact(Count::from(count))
        },
        RangeEnding(RangeEndingType::Unspecified, mods) => {
            match sum_mods(&mods, &ctx.context) {
                Some((MoveDir1D::Previous, off)) => SizeChange::Decrease(off.into()),
                Some((MoveDir1D::Next, off)) => SizeChange::Increase(off.into()),
                None => {
                    return Err(CommandError::InvalidArgument);
                },
            }
        },
        _ => {
            return Err(CommandError::InvalidRange);
        },
    };

    let axis = ctx.axis.unwrap_or(ctx.axis_default);
    let focus = window_range_target(&desc, ctx)?;
    let action = WindowAction::Resize(focus, axis, change).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:split` command.
///
/// *Aliases:* `sp`
///
/// Split the window. If an argument is given, then it will be opened with [OpenTarget::Name].
pub fn vim_cmd_sp<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let size = window_size(&desc, ctx)?;
    let target = window_open_target(&desc)?;
    let action = ctx.window(target, size);

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:vsplit` command.
///
/// *Aliases:* `vs`, `vsp`
///
/// Split the window vertically. If an argument is given, then it will be opened with
/// [OpenTarget::Name].
pub fn vim_cmd_vs<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    // Always override axis.
    ctx.axis = Some(Axis::Vertical);

    let size = window_size(&desc, ctx)?;
    let target = window_open_target(&desc)?;
    let action = ctx.window(target, size);

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:tabnext` command.
///
/// *Aliases:* `tabn`
///
/// Switch focus to a following tab.
pub fn vim_cmd_tabnext<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let range = match (desc.range, desc.arg.text.is_empty()) {
        (None, false) => {
            if let Ok(range) = desc.arg.range() {
                Some(range)
            } else {
                return Err(CommandError::InvalidArgument);
            }
        },
        (Some(_), false) => {
            return Err(CommandError::InvalidArgument);
        },
        (None, true) => None,
        (r @ Some(_), true) => r,
    };

    let change = range
        .map(|r| range_to_fc(&r, true, &ctx.context))
        .unwrap_or(Ok(FocusChange::Direction1D(MoveDir1D::Next, Count::Contextual, true)))?;

    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:tabprevious` command.
///
/// *Aliases:* `tabp`, `tabNext`, `tabN`
///
/// Switch focus to a previous tab.
pub fn vim_cmd_tabprev<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let range = match (desc.range, desc.arg.text.is_empty()) {
        (None, false) => {
            if let Ok(range) = desc.arg.range() {
                Some(range)
            } else {
                return Err(CommandError::InvalidArgument);
            }
        },
        (Some(_), false) => {
            return Err(CommandError::InvalidArgument);
        },
        (None, true) => None,
        (r @ Some(_), true) => r,
    };

    let change = match range {
        Some(RangeSpec::Single(RangeEnding(RangeEndingType::Absolute(count), mods))) => {
            if mods.is_empty() {
                // Move back {count} pages.
                FocusChange::Direction1D(MoveDir1D::Previous, count, true)
            } else {
                return Err(CommandError::InvalidRange);
            }
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

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `tabfirst` command.
///
/// *Aliases:* `tabfir`, `tabr`, `tabrewind`
///
/// Switch focus to the first tab.
pub fn vim_cmd_tabfirst<I: ApplicationInfo>(
    _: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let change = FocusChange::Position(MovePosition::Beginning);
    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:tablast` command.
///
/// *Aliases:* `tabl`
///
/// Switch focus to the last tab.
pub fn vim_cmd_tablast<I: ApplicationInfo>(
    _: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let change = FocusChange::Position(MovePosition::End);
    let action = TabAction::Focus(change).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:tab` command.
///
/// Run a command and, if it opens a window, open it in a new tab instead.
pub fn vim_cmd_tab<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    ctx.tab = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?
        .into();

    Ok(CommandStep::Again(desc.arg.text))
}

/// The `:tabedit` command.
///
/// *Aliases:* `tabe`, `tabnew`
///
/// Open a new tab. If an argument is given, then a window will be opened with [OpenTarget::Name]
/// and inserted into the new tab.
pub fn vim_cmd_tabedit<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let target = open_target(&desc)?.unwrap_or(OpenTarget::Unnamed);
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?;
    let action = TabAction::Open(target, change);

    Ok(CommandStep::Continue(action.into(), ctx.context.clone()))
}

/// The `:tabclose` command.
///
/// *Aliases:* `tabc`
///
/// Close a tab.
pub fn vim_cmd_tabclose<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?;
    let target = TabTarget::Single(change);
    let flags = if desc.bang {
        CloseFlags::FQ
    } else {
        CloseFlags::QUIT
    };
    let action = TabAction::Close(target, flags);

    Ok(CommandStep::Continue(action.into(), ctx.context.clone()))
}

/// The `:tabonly` command.
///
/// *Aliases:* `tabo`
///
/// Close all tabs but one.
pub fn vim_cmd_tabonly<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let change = desc
        .range
        .map(|r| range_to_fc(&r, false, &ctx.context))
        .unwrap_or(Ok(FocusChange::Current))?;
    let target = TabTarget::AllBut(change);
    let flags = if desc.bang {
        CloseFlags::FQ
    } else {
        CloseFlags::QUIT
    };
    let action = TabAction::Close(target, flags);

    Ok(CommandStep::Continue(action.into(), ctx.context.clone()))
}

/// The `:tabmove` command.
///
/// *Aliases:* `tabm`
///
/// Move a tab to a different position.
pub fn vim_cmd_tabmove<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let change = if let Some(r) = desc.range {
        range_to_fc(&r, false, &ctx.context)?
    } else if desc.arg.text.is_empty() {
        FocusChange::Position(MovePosition::End)
    } else {
        let r = desc.arg.range()?;

        range_to_fc(&r, false, &ctx.context)?
    };

    let action = TabAction::Move(change).into();

    Ok(CommandStep::Continue(action, ctx.context.clone()))
}

/// The `:leftabove` command.
///
/// *Aliases:* `lefta`, `aboveleft`, `abo`
///
/// Modify the following command to open the window before the current one.
pub fn vim_cmd_above<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    ctx.rel = Some(MoveDir1D::Previous);

    Ok(CommandStep::Again(desc.arg.text))
}

/// The `:rightbelow` command.
///
/// *Aliases:* `rightb`, `belowright`, `bel`
///
/// Modify the following command to open the window after the current one.
pub fn vim_cmd_below<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    ctx.rel = Some(MoveDir1D::Next);

    Ok(CommandStep::Again(desc.arg.text))
}

/// The `:horizontal` command.
///
/// *Aliases:* `hor`
///
/// Modify the following command to open a window horizontally.
pub fn vim_cmd_horizontal<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    ctx.axis = Some(Axis::Horizontal);

    Ok(CommandStep::Again(desc.arg.text))
}

/// The `:vertical` command.
///
/// *Aliases:* `vert`
///
/// Modify the following command to open a window vertically.
///
/// For example, `:vertical split` will behave like `:vsplit`.
pub fn vim_cmd_vertical<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    ctx.axis = Some(Axis::Vertical);

    Ok(CommandStep::Again(desc.arg.text))
}

/// The `:write` command.
///
/// *Aliases:* `w`
///
/// Write the window contents.
pub fn vim_cmd_write<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let mut args = desc.arg.strings()?;

    if args.len() > 1 {
        return Err(CommandError::InvalidArgument);
    }

    let filename = args.pop();
    let target = WindowTarget::Single(FocusChange::Current);
    let flags = if desc.bang {
        WriteFlags::FORCE
    } else {
        WriteFlags::NONE
    };
    let action = WindowAction::Write(target, filename, flags);

    Ok(CommandStep::Continue(action.into(), ctx.context.clone()))
}

/// The `:wall` command.
///
/// Write the contents of all windows in the current tab.
pub fn vim_cmd_write_all<I: ApplicationInfo>(
    desc: CommandDescription,
    ctx: &mut CommandContext,
) -> CommandResult<I> {
    let target = WindowTarget::All;
    let flags = if desc.bang {
        WriteFlags::FORCE
    } else {
        WriteFlags::NONE
    };
    let action = WindowAction::Write(target, None, flags);

    Ok(CommandStep::Continue(action.into(), ctx.context.clone()))
}

fn vim_cmd_filter<I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext,
) -> CommandResult<I> {
    Err(CommandError::Error("filtering is not yet implemented".into()))
}

/// The `:read` command.
///
/// *Aliases:* `r`
///
/// Read the contents of a file or program output into the buffer.
pub fn vim_cmd_read<I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext,
) -> CommandResult<I> {
    Err(CommandError::Error("read is not yet implemented".into()))
}

/// The `:print` command.
///
/// *Aliases:* `p`
///
/// Print lines in the given range.
pub fn vim_cmd_print<I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext,
) -> CommandResult<I> {
    Err(CommandError::Error("print is not yet implemented".into()))
}

fn vim_cmd_substitute<I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext,
) -> CommandResult<I> {
    Err(CommandError::Error("substitution is not yet implemented".into()))
}

fn vim_cmd_substitute_repeat<I: ApplicationInfo>(
    _: CommandDescription,
    _: &mut CommandContext,
) -> CommandResult<I> {
    Err(CommandError::Error("substitution repetition is not yet implemented".into()))
}

fn default_cmds<I: ApplicationInfo>() -> Vec<VimCommand<I>> {
    vec![
        VimCommand {
            name: "!".into(),
            aliases: strs![],
            f: vim_cmd_filter,
        },
        VimCommand {
            name: "&".into(),
            aliases: strs!["&&", "~", "~&"],
            f: vim_cmd_substitute_repeat,
        },
        VimCommand {
            name: "read".into(),
            aliases: strs!["r"],
            f: vim_cmd_read,
        },
        VimCommand {
            name: "print".into(),
            aliases: strs!["p"],
            f: vim_cmd_print,
        },
        VimCommand {
            name: "substitute".into(),
            aliases: strs!["s"],
            f: vim_cmd_substitute,
        },
        VimCommand {
            name: "close".into(),
            aliases: strs!["clo", "close"],
            f: vim_cmd_close,
        },
        VimCommand {
            name: "only".into(),
            aliases: strs!["on", "only"],
            f: vim_cmd_only,
        },
        VimCommand {
            name: "quit".into(),
            aliases: strs!["q"],
            f: vim_cmd_quit,
        },
        VimCommand {
            name: "quitall".into(),
            aliases: strs!["qa", "qall", "quita"],
            f: vim_cmd_quitall,
        },
        VimCommand {
            name: "resize".into(),
            aliases: strs!["res"],
            f: vim_cmd_resize,
        },
        VimCommand {
            name: "split".into(),
            aliases: strs!["sp"],
            f: vim_cmd_sp,
        },
        VimCommand {
            name: "vsplit".into(),
            aliases: strs!["vs", "vsp"],
            f: vim_cmd_vs,
        },
        VimCommand {
            name: "tab".into(),
            aliases: strs![],
            f: vim_cmd_tab,
        },
        VimCommand {
            name: "tabclose".into(),
            aliases: strs!["tabc"],
            f: vim_cmd_tabclose,
        },
        VimCommand {
            name: "tabedit".into(),
            aliases: strs!["tabe", "tabnew"],
            f: vim_cmd_tabedit,
        },
        VimCommand {
            name: "tabmove".into(),
            aliases: strs!["tabm"],
            f: vim_cmd_tabmove,
        },
        VimCommand {
            name: "tabnext".into(),
            aliases: strs!["tabn"],
            f: vim_cmd_tabnext,
        },
        VimCommand {
            name: "tabonly".into(),
            aliases: strs!["tabo"],
            f: vim_cmd_tabonly,
        },
        VimCommand {
            name: "tabprevious".into(),
            aliases: strs!["tabp", "tabN", "tabNext"],
            f: vim_cmd_tabprev,
        },
        VimCommand {
            name: "tabfirst".into(),
            aliases: strs!["tabr", "tabrewind", "tabfir"],
            f: vim_cmd_tabfirst,
        },
        VimCommand {
            name: "tablast".into(),
            aliases: strs!["tabl"],
            f: vim_cmd_tablast,
        },
        VimCommand {
            name: "write".into(),
            aliases: strs!["w"],
            f: vim_cmd_write,
        },
        VimCommand {
            name: "wall".into(),
            aliases: strs!["wa"],
            f: vim_cmd_write_all,
        },
        VimCommand {
            name: "horizontal".into(),
            aliases: strs!["hor"],
            f: vim_cmd_horizontal,
        },
        VimCommand {
            name: "vertical".into(),
            aliases: strs!["vert"],
            f: vim_cmd_vertical,
        },
        VimCommand {
            name: "aboveleft".into(),
            aliases: strs!["lefta", "leftabove", "abo"],
            f: vim_cmd_above,
        },
        VimCommand {
            name: "belowright".into(),
            aliases: strs!["rightb", "rightbelow", "bel"],
            f: vim_cmd_below,
        },
    ]
}

/// Manages parsing and mapping Vim commands.
pub type VimCommandMachine<I = EmptyInfo> = CommandMachine<VimCommand<I>>;

impl<I> Default for VimCommandMachine<I>
where
    I: ApplicationInfo,
{
    fn default() -> Self {
        let mut m = Self::new();

        for cmd in default_cmds().into_iter() {
            m.add_command(cmd);
        }

        return m;
    }
}

/// Complete text in the command-bar.
pub fn complete_cmdbar<I>(
    input: &EditRope,
    cursor: &mut Cursor,
    cmds: &VimCommandMachine<I>,
) -> Vec<String>
where
    I: ApplicationInfo,
{
    let eo = input.cursor_to_offset(cursor);
    let slice = input.slice(0.into()..eo);
    let cow = Cow::from(&slice);

    match CommandDescription::from_str(cow.as_ref()) {
        Ok(cmd) => {
            if cmd.arg.untrimmed.is_empty() {
                // Complete command name and set cursor position.
                let _ = input.get_prefix_word_mut(cursor, &WordStyle::Little);
                cmds.complete_name(cmd.command.as_str())
            } else {
                // Complete command argument.
                complete_path(input, cursor)
            }
        },

        // Can't parse command text, so return zero completions.
        Err(_) => vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::prelude::Axis::{Horizontal, Vertical};
    use crate::prelude::MoveDir1D::{Next, Previous};

    fn mkcmd() -> (VimCommandMachine<EmptyInfo>, EditContext) {
        let cmds = VimCommandMachine::default();
        let ctx = EditContext::default();

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
            WindowAction::Close(WindowTarget::Single(FocusChange::Current), CloseFlags::QUIT)
                .into();
        let res = cmds.input_cmd("q", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);

        let res = cmds.input_cmd("quit", ctx.clone());
        assert_eq!(res.unwrap(), vec![(act.clone(), ctx.clone())]);

        // Check that "qa", "qall", "quita" and "quitall" return the same Actions, different from
        // the "q"/"quit" actions.
        let act: Action = TabAction::Close(TabTarget::All, CloseFlags::QUIT).into();
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

    #[test]
    fn test_split_tab() {
        let (mut cmds, ctx) = mkcmd();

        // Unprefixed split command.
        let act = WindowAction::Split(OpenTarget::Current, Horizontal, Previous, 1.into());
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("split", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Prefixed split command should become TabAction::Open.
        let act = TabAction::Open(OpenTarget::Current, FocusChange::Offset(5.into(), false));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("5tab split", ctx.clone());
        assert_eq!(res.unwrap(), expect);
    }

    #[test]
    fn test_tab_move() {
        let (mut cmds, ctx) = mkcmd();

        // No arguments.
        let act = TabAction::Move(FocusChange::Position(MovePosition::End));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Move to the right.
        let act = TabAction::Move(FocusChange::Direction1D(MoveDir1D::Next, 1.into(), false));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("+tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Move two to the right.
        let act = TabAction::Move(FocusChange::Direction1D(MoveDir1D::Next, 2.into(), false));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("+2tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Move to the left.
        let act = TabAction::Move(FocusChange::Direction1D(MoveDir1D::Previous, 1.into(), false));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("-tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Move four to the left.
        let act = TabAction::Move(FocusChange::Direction1D(MoveDir1D::Previous, 4.into(), false));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("-4tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Move to the beginning.
        let act = TabAction::Move(FocusChange::Offset(0.into(), false));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("0tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Move to after the third tab.
        let act = TabAction::Move(FocusChange::Offset(3.into(), false));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("3tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Move to the end.
        let act = TabAction::Move(FocusChange::Position(MovePosition::End));
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("$tabmove", ctx.clone());
        assert_eq!(res.unwrap(), expect);
    }

    #[test]
    fn test_resize() {
        let (mut cmds, ctx) = mkcmd();

        // No arguments.
        let res = cmds.input_cmd("resize", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidArgument));

        // :resize only accepts a subset of range types.
        let res = cmds.input_cmd("resize 2,3", ctx.clone());
        assert_eq!(res, Err(CommandError::InvalidRange));

        // Resize current window to 5.
        let act = WindowAction::Resize(
            FocusChange::Current,
            Axis::Horizontal,
            SizeChange::Exact(5.into()),
        );
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("resize 5", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Increase current window size by 7 vertically.
        let act = WindowAction::Resize(
            FocusChange::Current,
            Axis::Vertical,
            SizeChange::Increase(7.into()),
        );
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("vert resize +7", ctx.clone());
        assert_eq!(res.unwrap(), expect);

        // Decrease window number 4's size by 2.
        let act = WindowAction::Resize(
            FocusChange::Offset(4.into(), false),
            Axis::Horizontal,
            SizeChange::Decrease(2.into()),
        );
        let expect = vec![(act.into(), ctx.clone())];
        let res = cmds.input_cmd("4resize -2", ctx.clone());
        assert_eq!(res.unwrap(), expect);
    }
}
