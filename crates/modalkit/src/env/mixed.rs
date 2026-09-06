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
    editing::cursor::CursorStyle,
    key::TerminalKey,
    keybindings::{dialog::Dialog, BindingMachine, InputBindings, InputKey, Step},
    prelude::RepeatType,
};

use super::{
    emacs::{
        keybindings::{default_emacs_keys, EmacsBindings, EmacsMachine, InputStep as EmacsStep},
        EmacsState,
    },
    kak::{
        keybindings::{
            default_kakoune_keys,
            InputStep as KakouneStep,
            KakouneBindings,
            KakouneMachine,
        },
        KakouneState,
    },
    vim::{
        keybindings::{default_vim_keys, InputStep as VimStep, VimBindings, VimMachine},
        VimState,
    },
    ShellBindings,
};

/// Multiple keybinding styles that users can select.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum MixedChoice {
    /// Choose Emacs keybindings.
    Emacs,

    /// Choose Kakoune keybindings.
    Kakoune,

    /// Choose Vim keybindings.
    Vim,
}

macro_rules! delegate_bindings {
    ($s: expr, $invoke: expr) => {
        match $s {
            MixedMachine::Emacs(c) => $invoke(c),
            MixedMachine::Kakoune(c) => $invoke(c),
            MixedMachine::Vim(c) => $invoke(c),
        }
    };
    ($s: expr, $invoke: expr, $arg: expr) => {
        match $s {
            MixedMachine::Emacs(c) => $invoke(c, $arg),
            MixedMachine::Kakoune(c) => $invoke(c, $arg),
            MixedMachine::Vim(c) => $invoke(c, $arg),
        }
    };
    ($s: expr, $invoke: expr, $arg1: expr, $arg2: expr) => {
        match $s {
            MixedMachine::Emacs(c) => $invoke(c, $arg1, $arg2),
            MixedMachine::Kakoune(c) => $invoke(c, $arg1, $arg2),
            MixedMachine::Vim(c) => $invoke(c, $arg1, $arg2),
        }
    };
}

/// Type for wrapping different keybindings in contexts where keybindings can be determined
/// dynamically.
#[non_exhaustive]
pub enum MixedBindings<I = EmptyInfo>
where
    I: ApplicationInfo,
{
    /// Wrap Emacs bindings.
    Emacs(EmacsBindings<I>),

    /// Wrap Kakoune bindings.
    Kakoune(KakouneBindings<I>),

    /// Wrap Vim bindings.
    Vim(VimBindings<I>),
}

impl<I> ShellBindings for MixedBindings<I>
where
    I: ApplicationInfo,
{
    fn shell(self) -> Self {
        match self {
            MixedBindings::Emacs(b) => MixedBindings::Emacs(b.shell()),
            MixedBindings::Kakoune(b) => MixedBindings::Kakoune(b.shell()),
            MixedBindings::Vim(b) => MixedBindings::Vim(b.shell()),
        }
    }
}

impl<I> From<MixedChoice> for MixedBindings<I>
where
    I: ApplicationInfo,
{
    fn from(choice: MixedChoice) -> Self {
        match choice {
            MixedChoice::Emacs => MixedBindings::Emacs(EmacsBindings::default()),
            MixedChoice::Vim => MixedBindings::Vim(VimBindings::default()),
            MixedChoice::Kakoune => MixedBindings::Kakoune(KakouneBindings::default()),
        }
    }
}

/// Type for wrapping different [BindingMachine] values in contexts where keybindings can be
/// determined dynamically.
#[non_exhaustive]
pub enum MixedMachine<K, I = EmptyInfo>
where
    K: InputKey,
    I: ApplicationInfo,
    EmacsStep<I>: Step<K>,
    KakouneStep<I>: Step<K>,
    VimStep<I>: Step<K>,
{
    /// Wrap Emacs bindings.
    Emacs(EmacsMachine<K, I>),

    /// Wrap Kakoune bindings.
    Kakoune(KakouneMachine<K, I>),

    /// Wrap Vim bindings.
    Vim(VimMachine<K, I>),
}

impl<I> From<MixedChoice> for MixedMachine<TerminalKey, I>
where
    I: ApplicationInfo,
{
    fn from(choice: MixedChoice) -> Self {
        match choice {
            MixedChoice::Emacs => MixedMachine::Emacs(default_emacs_keys()),
            MixedChoice::Kakoune => MixedMachine::Kakoune(default_kakoune_keys()),
            MixedChoice::Vim => MixedMachine::Vim(default_vim_keys()),
        }
    }
}

impl<I> From<MixedBindings<I>> for MixedMachine<TerminalKey, I>
where
    I: ApplicationInfo,
{
    fn from(bindings: MixedBindings<I>) -> Self {
        match bindings {
            MixedBindings::Emacs(b) => {
                let mut machine = EmacsMachine::empty();
                b.setup(&mut machine);

                MixedMachine::Emacs(machine)
            },
            MixedBindings::Kakoune(b) => {
                let mut machine = KakouneMachine::empty();
                b.setup(&mut machine);

                MixedMachine::Kakoune(machine)
            },
            MixedBindings::Vim(b) => {
                let mut machine = VimMachine::empty();
                b.setup(&mut machine);

                MixedMachine::Vim(machine)
            },
        }
    }
}

impl<K, I> BindingMachine<K, Action<I>, RepeatType, EditContext, CursorStyle> for MixedMachine<K, I>
where
    K: InputKey,
    I: ApplicationInfo,
    EmacsStep<I>: Step<K, A = Action<I>, Sequence = RepeatType, State = EmacsState<I>>,
    KakouneStep<I>: Step<K, A = Action<I>, Sequence = RepeatType, State = KakouneState<I>>,
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

    fn get_cursor_hint(&self) -> CursorStyle {
        delegate_bindings!(self, BindingMachine::get_cursor_hint)
    }

    fn repeat(&mut self, rt: RepeatType, other: Option<EditContext>) {
        delegate_bindings!(self, BindingMachine::repeat, rt, other)
    }

    fn run_dialog(&mut self, dialog: Box<dyn Dialog<Action<I>>>) {
        delegate_bindings!(self, BindingMachine::run_dialog, dialog)
    }
}
