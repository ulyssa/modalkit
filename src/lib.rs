//! # modalkit
//!
//! ## Overview
//!
//! This crate allows building terminal applications that support modal input, such as
//! the Vim text editor.
//!

#[macro_use]
mod util;

pub mod editing;
pub mod input;
pub mod vim;

#[cfg(feature = "widgets")]
pub mod widgets;
