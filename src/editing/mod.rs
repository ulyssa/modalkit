//! # Editing support
//!
//! ## Overview
//!
//! This module contains the types and code needed to help build user interfaces and process text
//! input independent of specific keybindings, and UI environment.
//!
//! The [readline] and [widgets] modules build upon this module's contents.
//!
//! [readline]: crate::readline
//! [widgets]: crate::widgets
//!

pub mod action;
pub mod application;
pub mod base;
pub mod buffer;
pub mod context;
pub mod cursor;
pub mod history;
pub mod key;
pub mod lineinfo;
pub mod rope;
pub mod store;
