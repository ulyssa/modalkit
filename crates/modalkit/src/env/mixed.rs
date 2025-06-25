//! # Dynamically-determined environments
//!
//! ## Overview
//!
//! This module contains wrappers that allow creating environments where users can specify what
//! flavor of keybindings they want to use during or after program startup.
use std::borrow::Cow;

use crate::{
    actions::Action,
    editing::application::{ApplicationInfo, EmptyInfo},
    editing::context::EditContext,
    key::TerminalKey,
    keybindings::{dialog::Dialog, BindingMachine, InputKey, Step},
    prelude::RepeatType,
};

use super::{
    emacs::{
        keybindings::{default_emacs_keys, EmacsMachine, InputStep as EmacsStep},
        EmacsState,
    },
    vim::{
        keybindings::{default_vim_keys, InputStep as VimStep, VimMachine},
        VimState,
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
            MixedChoice::Emacs => MixedBindings::Emacs(default_emacs_keys()),
            MixedChoice::Vim => MixedBindings::Vim(default_vim_keys()),
        }
    }
}

impl<K, I> BindingMachine<K, Action<I>, RepeatType, EditContext> for MixedBindings<K, I>
where
    K: InputKey,
    I: ApplicationInfo,
    EmacsStep<I>: Step<K, A = Action<I>, Sequence = RepeatType, State = EmacsState<I>>,
    VimStep<I>: Step<K, A = Action<I>, Sequence = RepeatType, State = VimState<I>>,
{
    fn input_key(&mut self, key: K) {
        delegate_bindings!(self, BindingMachine::input_key, key)
    }

    fn pop(&mut self) -> Option<(Action<I>, EditContext)> {
        delegate_bindings!(self, BindingMachine::pop)
    }

    fn context(&mut self) -> EditContext {
        delegate_bindings!(self, BindingMachine::context)
    }

    fn show_dialog(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>> {
        delegate_bindings!(self, BindingMachine::show_dialog, max_rows, max_cols)
    }

    fn show_mode(&self) -> Option<String> {
        delegate_bindings!(self, BindingMachine::show_mode)
    }

    fn reset_mode(&mut self) {
        delegate_bindings!(self, BindingMachine::reset_mode)
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        delegate_bindings!(self, BindingMachine::get_cursor_indicator)
    }

    fn repeat(&mut self, rt: RepeatType, other: Option<EditContext>) {
        delegate_bindings!(self, BindingMachine::repeat, rt, other)
    }

    fn run_dialog(&mut self, dialog: Box<dyn Dialog<Action<I>>>) {
        delegate_bindings!(self, BindingMachine::run_dialog, dialog)
    }
}
