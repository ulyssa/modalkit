//! # modalkit
//!
//! ## Overview
//!
//! This crate allows building terminal applications that support modal input, such as
//! the Vim text editor.
//!
//! The [mod@env] module contains keybindings for Vim and Emacs, which can be used along with
//! [key::TerminalKey] in the terminal to generate [actions] that can be used to drive [editing].
//!
//! ## Examples
//!
//! For examples using this crate, check out:
//!
//! - [modalkit-ratatui], for how to build TUI interfaces
//! - [scansion] for building a shell-like application
//! - [iamb] for a larger example of building a TUI application with custom actions
//!
//! [modalkit-ratatui]: https://docs.rs/modalkit-ratatui/latest/modalkit_ratatui/
//! [scansion]: https://docs.rs/scansion/latest/scansion/
//! [iamb]: https://github.com/ulyssa/iamb

// Require docs for public APIs, and disable the more annoying clippy lints.
#![deny(missing_docs)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::manual_range_contains)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::needless_return)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::type_complexity)]

#[macro_use]
mod util;

pub mod actions;
pub mod commands;
pub mod editing;
pub mod env;
pub mod errors;
pub mod key;
pub mod prelude;
pub mod ui;

pub use crossterm;
pub use keybindings;
