//! # modalkit
//!
//! ## Overview
//!
//! This crate allows building terminal applications that support modal input, such as
//! the Vim text editor.
//!

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

pub mod editing;
pub mod env;
pub mod input;

#[cfg(feature = "readline")]
pub mod readline;

pub use crossterm;

#[cfg(feature = "tui")]
pub use tui;

#[cfg(feature = "widgets")]
pub mod widgets;
