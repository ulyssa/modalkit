//! # Dynamically-determined environments
//!
//! ## Overview
//!
//! This module contains wrappers that allow creating environments where users can specify what
//! flavor of keybindings they want to use during or after program startup.
use regex::Regex;

use crate::{
    editing::action::{Action, EditAction},
    editing::base::{
        Application,
        Char,
        Count,
        CursorEnd,
        EditContext,
        InsertStyle,
        Mark,
        MoveDir1D,
        Register,
        RepeatType,
        Resolve,
        Specifier,
        TargetShape,
    },
    input::{
        bindings::{BindingMachine, Step},
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
pub enum MixedContext<P: Application = ()> {
    /// Wrapper for the Emacs context.
    Emacs(EmacsContext<P>),

    /// Wrapper for the Vim context.
    Vim(VimContext<P>),
}

impl<P> Clone for MixedContext<P>
where
    P: Application,
{
    fn clone(&self) -> Self {
        match self {
            MixedContext::Emacs(c) => MixedContext::Emacs(c.clone()),
            MixedContext::Vim(c) => MixedContext::Vim(c.clone()),
        }
    }
}

impl<P> Default for MixedContext<P>
where
    P: Application,
{
    fn default() -> Self {
        panic!("Cannot create a default MixedContext")
    }
}

impl<P> From<EmacsContext<P>> for MixedContext<P>
where
    P: Application,
{
    fn from(ctx: EmacsContext<P>) -> Self {
        MixedContext::Emacs(ctx)
    }
}

impl<P> From<VimContext<P>> for MixedContext<P>
where
    P: Application,
{
    fn from(ctx: VimContext<P>) -> Self {
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
}

impl<P> InputContext for MixedContext<P>
where
    P: Application,
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

impl<P: Application> Resolve<Count, usize> for MixedContext<P> {
    fn resolve(&self, count: &Count) -> usize {
        delegate_context!(self, Resolve::resolve, count)
    }
}

impl<P: Application> Resolve<Specifier<Char>, Option<Char>> for MixedContext<P> {
    fn resolve(&self, c: &Specifier<Char>) -> Option<Char> {
        delegate_context!(self, Resolve::resolve, c)
    }
}

impl<P: Application> Resolve<Specifier<EditAction>, EditAction> for MixedContext<P> {
    fn resolve(&self, ea: &Specifier<EditAction>) -> EditAction {
        delegate_context!(self, Resolve::resolve, ea)
    }
}

impl<P: Application> Resolve<Specifier<Mark>, Mark> for MixedContext<P> {
    fn resolve(&self, mark: &Specifier<Mark>) -> Mark {
        delegate_context!(self, Resolve::resolve, mark)
    }
}

impl<P: Application> EditContext for MixedContext<P> {
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
}

/// Type for wrapping different keybindings in contexts where keybindings can be determined
/// dynamically.
pub enum MixedBindings<K, P = ()>
where
    K: InputKey,
    P: Application,
    EmacsStep<P>: Step<K>,
    VimStep<P>: Step<K>,
{
    /// Wrap Emacs bindings.
    Emacs(EmacsMachine<K, P>),

    /// Wrap Vim bindings.
    Vim(VimMachine<K, P>),
}

impl<P> From<MixedChoice> for MixedBindings<TerminalKey, P>
where
    P: Application,
{
    fn from(choice: MixedChoice) -> Self {
        match choice {
            MixedChoice::Emacs => MixedBindings::Emacs(EmacsMachine::default()),
            MixedChoice::Vim => MixedBindings::Vim(VimMachine::default()),
        }
    }
}

impl<K, P> BindingMachine<K, Action<P>, RepeatType, MixedContext<P>> for MixedBindings<K, P>
where
    K: InputKey,
    P: Application,
    EmacsStep<P>: Step<K, A = Action<P>, Sequence = RepeatType, C = EmacsContext<P>>,
    VimStep<P>: Step<K, A = Action<P>, Sequence = RepeatType, C = VimContext<P>>,
{
    fn input_key(&mut self, key: K) {
        delegate_bindings!(self, BindingMachine::input_key, key)
    }

    fn pop(&mut self) -> Option<(Action<P>, MixedContext<P>)> {
        match self {
            MixedBindings::Emacs(e) => e.pop().map(|(a, c)| (a, c.into())),
            MixedBindings::Vim(v) => v.pop().map(|(a, c)| (a, c.into())),
        }
    }

    fn context(&self) -> MixedContext<P> {
        match self {
            MixedBindings::Emacs(m) => m.context().into(),
            MixedBindings::Vim(m) => m.context().into(),
        }
    }

    fn showmode(&self) -> Option<String> {
        delegate_bindings!(self, BindingMachine::showmode)
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        delegate_bindings!(self, BindingMachine::get_cursor_indicator)
    }

    fn repeat(&mut self, rt: RepeatType, other: Option<MixedContext<P>>) {
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
}
