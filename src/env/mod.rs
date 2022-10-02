//! # Editing environments
//!
//! ## Overview
//!
//! This module contains components for recreating different flavors of editing environments.
//!
use crossterm::event::KeyModifiers;

use crate::{
    editing::base::Char,
    input::{
        bindings::{EdgeEvent, EdgePath, EdgePathPart, InputKeyClass},
        key::TerminalKey,
    },
};

#[macro_use]
mod macros;

mod keyparse;

pub mod emacs;
pub mod mixed;
pub mod vim;

pub(crate) type CommonEdgeEvent = EdgeEvent<TerminalKey, CommonKeyClass>;
pub(crate) type CommonEdgePathPart = EdgePathPart<TerminalKey, CommonKeyClass>;
pub(crate) type CommonEdgePath = EdgePath<TerminalKey, CommonKeyClass>;

/// Classes of characters that input keys can belong to.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CommonKeyClass {
    /// A number between 0 and 9.
    Count,

    /// A character representing a register.
    Register,

    /// A character representing a mark.
    Mark,

    /// A number between 0 and 7.
    Octal,

    /// A number between 0 and 9.
    Decimal,

    /// A number between 0 and F.
    Hexadecimal,

    /// The first character in a digraph sequence.
    Digraph1,

    /// The second character in a digraph sequence.
    Digraph2,
}

impl InputKeyClass<TerminalKey> for CommonKeyClass {
    fn memberships(ke: &TerminalKey) -> Vec<Self> {
        let mut classes = Vec::new();

        if let Some(('0'..='9', mods)) = ke.get_char_mods() {
            if !mods.contains(KeyModifiers::CONTROL) {
                classes.push(CommonKeyClass::Count);
            }
        }

        if let Some(c) = ke.get_char() {
            if is_register_char(c) {
                classes.push(CommonKeyClass::Register);
            }

            if is_mark_char(c) {
                classes.push(CommonKeyClass::Mark);
            }

            if let '0'..='7' = c {
                classes.push(CommonKeyClass::Octal);
            }

            if let '0'..='9' = c {
                classes.push(CommonKeyClass::Decimal);
            }

            if let '0'..='9' | 'a'..='f' | 'A'..='F' = c {
                classes.push(CommonKeyClass::Hexadecimal);
            }

            classes.push(CommonKeyClass::Digraph1);
            classes.push(CommonKeyClass::Digraph2);
        }

        return classes;
    }
}

#[inline]
fn is_register_char(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '0'..='9' => true,
        '"' => true,
        '-' => true,
        '#' => true,
        '_' => true,
        '%' => true,
        ':' => true,
        '.' => true,
        '/' => true,
        '*' => true,
        '+' => true,
        _ => false,
    }
}

#[inline]
fn is_mark_char(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '0'..='9' => true,
        '\'' | '`' => true,
        '<' | '>' => true,
        '[' | ']' => true,
        '"' => true,
        '^' => true,
        '.' => true,

        _ => false,
    }
}

/// Fields for tracking entered codepoints, literals and digraphs.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(self) struct CharacterContext {
    pub(self) dec: Option<u32>,
    pub(self) oct: Option<u32>,
    pub(self) hex: Option<u32>,
    pub(self) any: Option<TerminalKey>,
    pub(self) digraph1: Option<char>,
    pub(self) digraph2: Option<char>,
}

impl Default for CharacterContext {
    fn default() -> Self {
        CharacterContext {
            dec: None,
            oct: None,
            hex: None,
            any: None,
            digraph1: None,
            digraph2: None,
        }
    }
}

impl CharacterContext {
    fn get_typed(&self) -> Option<Char> {
        if let Some((d1, d2)) = self.get_digraph() {
            let c = Char::Digraph(d1, d2);

            Some(c)
        } else if let Some(cp) = self.get_codepoint() {
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

    fn get_digraph(&self) -> Option<(char, char)> {
        if let (Some(a), Some(b)) = (self.digraph1, self.digraph2) {
            Some((a, b))
        } else {
            None
        }
    }

    fn get_codepoint(&self) -> Option<u32> {
        self.hex.or(self.dec).or(self.oct)
    }

    fn get_literal_char(&self) -> Option<char> {
        self.any.as_ref().and_then(TerminalKey::get_literal_char)
    }

    fn get_literal_string(&self) -> Option<String> {
        self.any.as_ref().map(ToString::to_string)
    }
}
