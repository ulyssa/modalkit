//! # Dynamically-determined environments
//!
//! ## Overview
//!
//! This module contains wrappers that allow creating environments where users can specify what
//! flavor of keybindings they want to use during or after program startup.
use std::borrow::Cow;

use regex::Regex;

use crate::{
    editing::{
        action::{Action, EditAction},
        application::{ApplicationInfo, EmptyInfo},
        base::{
            Char,
            Count,
            CursorEnd,
            InsertStyle,
            Mark,
            MoveDir1D,
            Register,
            RepeatType,
            Specifier,
            TargetShape,
        },
        context::{EditContext, Resolve},
    },
    input::{
        bindings::{BindingMachine, Step},
        dialog::Dialog,
        key::{InputKey, TerminalKey},
        InputContext,
    },
};

use super::{
    emacs::{
        keybindings::{EmacsMachine, InputStep as EmacsStep},
        EmacsContext,
    },
    vim::{
        keybindings::{InputStep as VimStep, VimMachine},
        VimContext,
    },
};

/// Multiple keybinding styles that users can select.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum MixedChoice {
    /// Choose Emacs keybindings.
    Emacs,

    /// Choose Vim keybindings.
    Vim,
}

/// Wrapper for the contexts used with different keybinding environments.
pub enum MixedContext<I: ApplicationInfo = EmptyInfo> {
    /// Wrapper for the Emacs context.
    Emacs(EmacsContext<I>),

    /// Wrapper for the Vim context.
    Vim(VimContext<I>),
}

impl<I> Clone for MixedContext<I>
where
    I: ApplicationInfo,
{
    fn clone(&self) -> Self {
        match self {
            MixedContext::Emacs(c) => MixedContext::Emacs(c.clone()),
            MixedContext::Vim(c) => MixedContext::Vim(c.clone()),
        }
    }
}

impl<I> Default for MixedContext<I>
where
    I: ApplicationInfo,
{
    fn default() -> Self {
        panic!("Cannot create a default MixedContext")
    }
}

impl<I> From<EmacsContext<I>> for MixedContext<I>
where
    I: ApplicationInfo,
{
    fn from(ctx: EmacsContext<I>) -> Self {
        MixedContext::Emacs(ctx)
    }
}

impl<I> From<VimContext<I>> for MixedContext<I>
where
    I: ApplicationInfo,
{
    fn from(ctx: VimContext<I>) -> Self {
        MixedContext::Vim(ctx)
    }
}

macro_rules! delegate_context {
    ($s: expr, $invoke: expr) => {
        match $s {
            MixedContext::Emacs(c) => $invoke(c),
            MixedContext::Vim(c) => $invoke(c),
        }
    };
    ($s: expr, $invoke: expr, $arg: expr) => {
        match $s {
            MixedContext::Emacs(c) => $invoke(c, $arg),
            MixedContext::Vim(c) => $invoke(c, $arg),
        }
    };
}

macro_rules! delegate_bindings {
    ($s: expr, $invoke: expr) => {
        match $s {
            MixedBindings::Emacs(c) => $invoke(c),
            MixedBindings::Vim(c) => $invoke(c),
        }
    };
    ($s: expr, $invoke: expr, $arg: expr) => {
        match $s {
            MixedBindings::Emacs(c) => $invoke(c, $arg),
            MixedBindings::Vim(c) => $invoke(c, $arg),
        }
    };
    ($s: expr, $invoke: expr, $arg1: expr, $arg2: expr) => {
        match $s {
            MixedBindings::Emacs(c) => $invoke(c, $arg1, $arg2),
            MixedBindings::Vim(c) => $invoke(c, $arg1, $arg2),
        }
    };
}

impl<I> InputContext for MixedContext<I>
where
    I: ApplicationInfo,
{
    fn overrides(&mut self, other: &Self) {
        match (self, other) {
            (MixedContext::Emacs(e), MixedContext::Emacs(c)) => e.overrides(c),
            (MixedContext::Vim(e), MixedContext::Vim(c)) => e.overrides(c),

            (MixedContext::Emacs(_), _) => {
                panic!("Must use MixedContext::Emacs with MixedContext::Emacs.overrides()!")
            },
            (MixedContext::Vim(_), _) => {
                panic!("Must use MixedContext::Vim with MixedContext::Vim.overrides()!")
            },
        }
    }

    fn reset(&mut self) {
        match self {
            MixedContext::Emacs(c) => c.reset(),
            MixedContext::Vim(c) => c.reset(),
        }
    }

    fn take(&mut self) -> Self {
        match self {
            MixedContext::Emacs(c) => c.take().into(),
            MixedContext::Vim(c) => c.take().into(),
        }
    }
}

impl<I: ApplicationInfo> Resolve<Count, usize> for MixedContext<I> {
    fn resolve(&self, count: &Count) -> usize {
        delegate_context!(self, Resolve::resolve, count)
    }
}

impl<I: ApplicationInfo> Resolve<Specifier<Char>, Option<Char>> for MixedContext<I> {
    fn resolve(&self, c: &Specifier<Char>) -> Option<Char> {
        delegate_context!(self, Resolve::resolve, c)
    }
}

impl<I: ApplicationInfo> Resolve<Specifier<EditAction>, EditAction> for MixedContext<I> {
    fn resolve(&self, ea: &Specifier<EditAction>) -> EditAction {
        delegate_context!(self, Resolve::resolve, ea)
    }
}

impl<I: ApplicationInfo> Resolve<Specifier<Mark>, Mark> for MixedContext<I> {
    fn resolve(&self, mark: &Specifier<Mark>) -> Mark {
        delegate_context!(self, Resolve::resolve, mark)
    }
}

impl<I: ApplicationInfo> EditContext for MixedContext<I> {
    fn get_cursor_end(&self) -> CursorEnd {
        delegate_context!(self, EditContext::get_cursor_end)
    }

    fn get_replace_char(&self) -> Option<Char> {
        delegate_context!(self, EditContext::get_replace_char)
    }

    fn get_search_regex(&self) -> Option<Regex> {
        delegate_context!(self, EditContext::get_search_regex)
    }

    fn get_search_regex_dir(&self) -> MoveDir1D {
        delegate_context!(self, EditContext::get_search_regex_dir)
    }

    fn get_search_char(&self) -> Option<(MoveDir1D, bool, Char)> {
        delegate_context!(self, EditContext::get_search_char)
    }

    fn get_target_shape(&self) -> Option<TargetShape> {
        delegate_context!(self, EditContext::get_target_shape)
    }

    fn get_insert_style(&self) -> Option<InsertStyle> {
        delegate_context!(self, EditContext::get_insert_style)
    }

    fn get_last_column(&self) -> bool {
        delegate_context!(self, EditContext::get_last_column)
    }

    fn get_register(&self) -> Option<Register> {
        delegate_context!(self, EditContext::get_register)
    }

    fn get_register_append(&self) -> bool {
        delegate_context!(self, EditContext::get_register_append)
    }

    fn is_search_incremental(&self) -> bool {
        delegate_context!(self, EditContext::is_search_incremental)
    }
}

/// Type for wrapping different keybindings in contexts where keybindings can be determined
/// dynamically.
pub enum MixedBindings<K, I = EmptyInfo>
where
    K: InputKey,
    I: ApplicationInfo,
    EmacsStep<I>: Step<K>,
    VimStep<I>: Step<K>,
{
    /// Wrap Emacs bindings.
    Emacs(EmacsMachine<K, I>),

    /// Wrap Vim bindings.
    Vim(VimMachine<K, I>),
}

impl<I> From<MixedChoice> for MixedBindings<TerminalKey, I>
where
    I: ApplicationInfo,
{
    fn from(choice: MixedChoice) -> Self {
        match choice {
            MixedChoice::Emacs => MixedBindings::Emacs(EmacsMachine::default()),
            MixedChoice::Vim => MixedBindings::Vim(VimMachine::default()),
        }
    }
}

impl<K, I> BindingMachine<K, Action<I>, RepeatType, MixedContext<I>> for MixedBindings<K, I>
where
    K: InputKey,
    I: ApplicationInfo,
    EmacsStep<I>: Step<K, A = Action<I>, Sequence = RepeatType, C = EmacsContext<I>>,
    VimStep<I>: Step<K, A = Action<I>, Sequence = RepeatType, C = VimContext<I>>,
{
    fn input_key(&mut self, key: K) {
        delegate_bindings!(self, BindingMachine::input_key, key)
    }

    fn pop(&mut self) -> Option<(Action<I>, MixedContext<I>)> {
        match self {
            MixedBindings::Emacs(e) => e.pop().map(|(a, c)| (a, c.into())),
            MixedBindings::Vim(v) => v.pop().map(|(a, c)| (a, c.into())),
        }
    }

    fn context(&self) -> MixedContext<I> {
        match self {
            MixedBindings::Emacs(m) => m.context().into(),
            MixedBindings::Vim(m) => m.context().into(),
        }
    }

    fn show_dialog(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>> {
        delegate_bindings!(self, BindingMachine::show_dialog, max_rows, max_cols)
    }

    fn show_mode(&self) -> Option<String> {
        delegate_bindings!(self, BindingMachine::show_mode)
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        delegate_bindings!(self, BindingMachine::get_cursor_indicator)
    }

    fn repeat(&mut self, rt: RepeatType, other: Option<MixedContext<I>>) {
        match (self, other) {
            (MixedBindings::Emacs(m), Some(MixedContext::Emacs(c))) => m.repeat(rt, Some(c)),
            (MixedBindings::Emacs(m), None) => m.repeat(rt, None),

            (MixedBindings::Vim(m), Some(MixedContext::Vim(c))) => m.repeat(rt, Some(c)),
            (MixedBindings::Vim(m), None) => m.repeat(rt, None),

            (MixedBindings::Emacs(_), Some(_)) => {
                panic!("Must use Emacs context with Emacs keybindings");
            },
            (MixedBindings::Vim(_), Some(_)) => {
                panic!("Must use Vim context with Vim keybindings");
            },
        }
    }

    fn run_dialog(&mut self, dialog: Box<dyn Dialog<Action<I>>>) {
        delegate_bindings!(self, BindingMachine::run_dialog, dialog)
    }
}
