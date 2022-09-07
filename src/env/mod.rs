//! # Editing Environments
//!
//! ## Overview
//!
//! This module contains components for recreating different flavors of editing environments.
//!
use crossterm::event::KeyEvent;

use crate::input::bindings::{EdgeEvent, EdgePath, EdgePathPart, InputKeyClass};

use crate::util::get_char;

#[macro_use]
mod macros;

mod keyparse;

pub mod emacs;
pub mod vim;

pub(crate) type CommonEdgeEvent = EdgeEvent<KeyEvent, CommonKeyClass>;
pub(crate) type CommonEdgePathPart = EdgePathPart<KeyEvent, CommonKeyClass>;
pub(crate) type CommonEdgePath = EdgePath<KeyEvent, CommonKeyClass>;

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

impl InputKeyClass<KeyEvent> for CommonKeyClass {
    fn memberships(ke: &KeyEvent) -> Vec<Self> {
        let mut classes = Vec::new();

        if let Some(c) = get_char(ke) {
            if let '0'..='9' = c {
                classes.push(CommonKeyClass::Count);
            }

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
