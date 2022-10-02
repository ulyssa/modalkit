//! # Emacs-like User Interfaces
//!
//! ## Overview
//!
//! This module contains components to help with building applications that mimic Emac's user
//! interfaces.
//!
use std::marker::PhantomData;

use regex::Regex;

use crate::input::{
    bindings::{EdgeEvent, InputKeyContext, Mode, ModeKeys, ModeSequence, SequenceStatus},
    key::TerminalKey,
    InputContext,
};

use crate::editing::base::{
    Action,
    Application,
    Char,
    CommandBarAction,
    Count,
    EditAction,
    EditContext,
    InsertStyle,
    InsertTextAction,
    Mark,
    MoveDir1D,
    Register,
    RepeatType,
    Resolve,
    Specifier,
    TargetShape,
};

use crate::util::{keycode_to_num, option_muladd_u32, option_muladd_usize};

use super::{CharacterContext, CommonKeyClass};

pub mod keybindings;

/// Emacs' modes
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum EmacsMode {
    /// Insert mode keypresses.
    Insert,

    /// Command bar keypresses.
    Command,

    /// Search bar keypresses.
    Search,
}

impl Default for EmacsMode {
    fn default() -> Self {
        EmacsMode::Insert
    }
}

impl<P: Application> Mode<Action<P>, EmacsContext<P>> for EmacsMode {
    fn enter(&self, _: Self, ctx: &mut EmacsContext<P>) -> Vec<Action<P>> {
        match self {
            EmacsMode::Insert => {
                return vec![];
            },
            EmacsMode::Command | EmacsMode::Search => {
                ctx.persist.shape = None;

                return vec![];
            },
        }
    }

    fn show(&self, ctx: &EmacsContext<P>) -> Option<String> {
        match self {
            EmacsMode::Insert => {
                match ctx.persist.insert {
                    InsertStyle::Insert => {
                        return None;
                    },
                    InsertStyle::Replace => {
                        return Some("Overwrite mode enabled".into());
                    },
                }
            },
            EmacsMode::Command | EmacsMode::Search => {
                return None;
            },
        }
    }
}

impl<P: Application> ModeSequence<RepeatType, Action<P>, EmacsContext<P>> for EmacsMode {
    fn sequences(
        &self,
        action: &Action<P>,
        ctx: &EmacsContext<P>,
    ) -> Vec<(RepeatType, SequenceStatus)> {
        match self {
            EmacsMode::Insert | EmacsMode::Command => {
                vec![
                    (RepeatType::EditSequence, action.is_edit_sequence(SequenceStatus::Break, ctx)),
                    (RepeatType::LastAction, action.is_last_action(ctx)),
                    (RepeatType::LastSelection, action.is_last_selection(ctx)),
                ]
            },
            EmacsMode::Search => {
                // Don't track anything done in Search mode.
                vec![]
            },
        }
    }
}

impl<P: Application> ModeKeys<TerminalKey, Action<P>, EmacsContext<P>> for EmacsMode {
    fn unmapped(
        &self,
        ke: &TerminalKey,
        ctx: &mut EmacsContext<P>,
    ) -> (Vec<Action<P>>, Option<Self>) {
        ctx.persist.repeating = false;

        match self {
            EmacsMode::Insert => {
                if let Some(c) = ke.get_char() {
                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, Count::Contextual);

                    (vec![it.into()], None)
                } else {
                    (vec![], None)
                }
            },
            EmacsMode::Command => {
                if let Some(c) = ke.get_char() {
                    let it = InsertTextAction::Type(
                        Char::Single(c).into(),
                        MoveDir1D::Previous,
                        Count::Contextual,
                    );

                    (vec![it.into()], None)
                } else {
                    (vec![], None)
                }
            },
            EmacsMode::Search => {
                if let Some(c) = ke.get_char() {
                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, Count::Contextual);

                    (vec![it.into()], None)
                } else {
                    (vec![CommandBarAction::Abort.into()], Some(EmacsMode::Insert))
                }
            },
        }
    }
}

/// This is the context specific to an action, and gets reset every time a full sequence of
/// keybindings is pressed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ActionContext {
    // Prefix arguments to key sequences.
    pub(crate) count: Option<usize>,
    pub(crate) counting: Option<usize>,
    pub(crate) register: Option<Register>,
}

impl Default for ActionContext {
    fn default() -> Self {
        Self { count: None, counting: None, register: None }
    }
}

/// This is the context preserved across actions, and changes either with the mode, or through
/// future keybinding sequences.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PersistentContext {
    repeating: bool,
    insert: InsertStyle,
    shape: Option<TargetShape>,
    shift: bool,
}

impl Default for PersistentContext {
    fn default() -> Self {
        Self {
            repeating: false,
            insert: InsertStyle::Insert,
            shape: None,
            shift: false,
        }
    }
}

/// This wraps both action specific context, and persistent context.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EmacsContext<P: Application = ()> {
    pub(crate) action: ActionContext,
    pub(crate) persist: PersistentContext,
    pub(self) ch: CharacterContext,

    _p: PhantomData<P>,
}

impl<P: Application> InputContext for EmacsContext<P> {
    fn overrides(&mut self, other: &Self) {
        // Allow overriding count.
        if other.action.count.is_some() {
            self.action.count = other.action.count;
        }
    }

    fn reset(&mut self) {
        self.action = ActionContext::default();
    }

    fn take(&mut self) -> Self {
        Self {
            persist: self.persist.clone(),
            action: std::mem::take(&mut self.action),
            ch: std::mem::take(&mut self.ch),

            _p: PhantomData,
        }
    }
}

impl<P: Application> EditContext for EmacsContext<P> {
    fn get_replace_char(&self) -> Option<Char> {
        None
    }

    fn get_search_regex(&self) -> Option<Regex> {
        None
    }

    fn get_search_regex_dir(&self) -> MoveDir1D {
        MoveDir1D::Next
    }

    fn get_search_char(&self) -> Option<(MoveDir1D, bool, Char)> {
        None
    }

    fn get_target_shape(&self) -> Option<TargetShape> {
        self.persist.shape.clone()
    }

    fn get_insert_style(&self) -> Option<InsertStyle> {
        self.persist.insert.into()
    }

    fn get_last_column(&self) -> bool {
        true
    }

    fn get_register(&self) -> Option<Register> {
        self.action.register.clone()
    }

    fn get_register_append(&self) -> bool {
        false
    }
}

impl<P: Application> Default for EmacsContext<P> {
    fn default() -> Self {
        Self {
            action: ActionContext::default(),
            persist: PersistentContext::default(),
            ch: CharacterContext::default(),

            _p: PhantomData,
        }
    }
}

impl<P: Application> Resolve<Count, usize> for EmacsContext<P> {
    fn resolve(&self, count: &Count) -> usize {
        match count {
            Count::Contextual => self.action.count.unwrap_or(1),
            Count::MinusOne => self.action.count.unwrap_or(0).saturating_sub(1),
            Count::Exact(n) => *n,
        }
    }
}

impl<P: Application> Resolve<Specifier<Char>, Option<Char>> for EmacsContext<P> {
    fn resolve(&self, c: &Specifier<Char>) -> Option<Char> {
        match c {
            Specifier::Contextual => self.ch.get_typed(),
            Specifier::Exact(c) => Some(c.clone()),
        }
    }
}

impl<P: Application> Resolve<Specifier<Mark>, Mark> for EmacsContext<P> {
    fn resolve(&self, mark: &Specifier<Mark>) -> Mark {
        match mark {
            Specifier::Contextual => Mark::LastJump,
            Specifier::Exact(m) => *m,
        }
    }
}

impl<P: Application> Resolve<Specifier<EditAction>, EditAction> for EmacsContext<P> {
    fn resolve(&self, mark: &Specifier<EditAction>) -> EditAction {
        match mark {
            Specifier::Contextual => EditAction::Motion,
            Specifier::Exact(a) => a.clone(),
        }
    }
}

impl<P: Application> InputKeyContext<TerminalKey, CommonKeyClass> for EmacsContext<P> {
    fn event(&mut self, ev: &EdgeEvent<TerminalKey, CommonKeyClass>, ke: &TerminalKey) {
        match ev {
            EdgeEvent::Key(_) | EdgeEvent::Fallthrough => {
                // Do nothing.
            },
            EdgeEvent::Class(CommonKeyClass::Count) => {
                if let Some(n) = keycode_to_num(ke, 10) {
                    let new = option_muladd_usize(&self.action.counting, 10, n as usize);

                    self.action.counting = Some(new);
                }
            },

            // Track literals, codepoints, etc.
            EdgeEvent::Any => {
                self.ch.any = Some(ke.clone());
            },
            EdgeEvent::Class(CommonKeyClass::Octal) => {
                if let Some(n) = keycode_to_num(ke, 8) {
                    let new = option_muladd_u32(&self.ch.oct, 8, n);

                    self.ch.oct = Some(new);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Decimal) => {
                if let Some(n) = keycode_to_num(ke, 10) {
                    let new = option_muladd_u32(&self.ch.dec, 10, n);

                    self.ch.dec = Some(new);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Hexadecimal) => {
                if let Some(n) = keycode_to_num(ke, 16) {
                    let new = option_muladd_u32(&self.ch.hex, 16, n);

                    self.ch.hex = Some(new);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Digraph1) => {
                if let Some(c) = ke.get_char() {
                    self.ch.digraph1 = Some(c);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Digraph2) => {
                if let Some(c) = ke.get_char() {
                    self.ch.digraph2 = Some(c);
                }
            },

            // Other classes are currently unused.
            EdgeEvent::Class(_) => {
                // Do nothing
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mode_show() {
        let mut ctx: EmacsContext = EmacsContext::default();

        assert_eq!(EmacsMode::Search.show(&ctx), None);

        ctx.persist.insert = InsertStyle::Insert;
        assert_eq!(EmacsMode::Insert.show(&ctx), None);

        ctx.persist.insert = InsertStyle::Replace;
        assert_eq!(EmacsMode::Insert.show(&ctx), Some("Overwrite mode enabled".into()));
    }
}
