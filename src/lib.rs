//! # modalkit
//!
//! ## Overview
//!
//! This crate allows building terminal applications that support modal input, such as
//! the Vim text editor.
//!

#[macro_use]
mod util;

#[deny(missing_docs)]
pub mod editing;
#[deny(missing_docs)]
pub mod env;
#[deny(missing_docs)]
pub mod input;

#[cfg(feature = "readline")]
#[deny(missing_docs)]
pub mod readline;

pub use crossterm;

#[cfg(feature = "tui")]
pub use tui;

#[cfg(feature = "widgets")]
#[deny(missing_docs)]
pub mod widgets;
