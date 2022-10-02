//! # Emacs-like User Interfaces
//!
//! ## Overview
//!
//! This module contains components to help with building applications that mimic Emac's user
//! interfaces.
//!
use std::marker::PhantomData;

use crossterm::event::KeyEvent;
use regex::Regex;

use crate::{
    input::bindings::{EdgeEvent, InputKeyContext, Mode, ModeKeys},
    input::InputContext,
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
    Resolve,
    Specifier,
    TargetShape,
};

use crate::util::{get_char, get_literal_char, keycode_to_num, option_muladd_u32};

use super::CommonKeyClass;

pub mod keybindings;

/// Emacs' modes
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum EmacsMode {
    /// Insert mode keypresses.
    Insert,

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
            EmacsMode::Search => {
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
            EmacsMode::Search => {
                return None;
            },
        }
    }
}

impl<P: Application> ModeKeys<KeyEvent, Action<P>, EmacsContext<P>> for EmacsMode {
    fn unmapped(&self, ke: &KeyEvent, _: &mut EmacsContext<P>) -> (Vec<Action<P>>, Option<Self>) {
        match self {
            EmacsMode::Insert => {
                if let Some(c) = get_char(ke) {
                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, Count::Contextual);

                    (vec![it.into()], None)
                } else {
                    (vec![], None)
                }
            },
            EmacsMode::Search => {
                if let Some(c) = get_char(ke) {
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
    // Fields for storing entered codepoints, literals and digraphs.
    pub(crate) oct: Option<u32>,
    pub(crate) any: Option<KeyEvent>,

    // Other arguments to key sequences.
    pub(crate) register: Option<Register>,
}

impl Default for ActionContext {
    fn default() -> Self {
        Self { oct: None, any: None, register: None }
    }
}

/// This is the context preserved across actions, and changes either with the mode, or through
/// future keybinding sequences.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PersistentContext {
    insert: InsertStyle,
    shape: Option<TargetShape>,
    shift: bool,
}

impl Default for PersistentContext {
    fn default() -> Self {
        Self {
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

    _p: PhantomData<P>,
}

impl<P: Application> EmacsContext<P> {
    fn get_typed(&self) -> Option<Char> {
        if let Some(cp) = self.get_codepoint() {
            let c = char::from_u32(cp)?;
            let c = Char::Single(c);

            Some(c)
        } else if let Some(c) = self.get_literal_char() {
            let c = Char::Single(c);

            Some(c)
        } else if let Some(s) = self.get_literal_string() {
            let c = Char::CtrlSeq(s);

            Some(c)
        } else {
            None
        }
    }

    fn get_codepoint(&self) -> Option<u32> {
        self.action.oct
    }

    fn get_literal_char(&self) -> Option<char> {
        self.action.any.as_ref().and_then(get_literal_char)
    }

    fn get_literal_string(&self) -> Option<String> {
        unimplemented!();
    }
}

impl<P: Application> InputContext for EmacsContext<P> {
    fn reset(&mut self) {
        self.action = ActionContext::default();
    }

    fn take(&mut self) -> Self {
        Self {
            persist: self.persist.clone(),
            action: std::mem::take(&mut self.action),

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

            _p: PhantomData,
        }
    }
}

impl<P: Application> Resolve<Count, usize> for EmacsContext<P> {
    fn resolve(&self, count: &Count) -> usize {
        match count {
            Count::Contextual => 1,
            Count::MinusOne => 0,
            Count::Exact(n) => *n,
        }
    }
}

impl<P: Application> Resolve<Specifier<Char>, Option<Char>> for EmacsContext<P> {
    fn resolve(&self, c: &Specifier<Char>) -> Option<Char> {
        match c {
            Specifier::Contextual => self.get_typed(),
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

impl<P: Application> InputKeyContext<KeyEvent, CommonKeyClass> for EmacsContext<P> {
    fn event(&mut self, ev: &EdgeEvent<KeyEvent, CommonKeyClass>, ke: &KeyEvent) {
        match ev {
            EdgeEvent::Key(_) | EdgeEvent::Fallthrough => {
                // Do nothing.
            },
            EdgeEvent::Any => {
                self.action.any = Some(ke.clone());
            },
            EdgeEvent::Class(CommonKeyClass::Octal) => {
                if let Some(n) = keycode_to_num(ke, 8) {
                    let new = option_muladd_u32(&self.action.oct, 8, n);

                    self.action.oct = Some(new);
                }
            },
            EdgeEvent::Class(_) => {
                // Other classes are currently unused.
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
