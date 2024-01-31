//! # Editing support
//!
//! ## Overview
//!
//! This module contains the types and code needed to help build user interfaces and process text
//! input independent of specific keybindings, and UI environment.
//!
//! The [scansion] and [modalkit-ratatui] crates build upon this module's contents.
//!
//! [scansion]: https://docs.rs/scansion/latest/scansion/
//! [modalkit-ratatui]: https://docs.rs/modalkit-ratatui/latest/modalkit_ratatui/
pub mod action;
pub mod application;
pub mod buffer;
pub mod completion;
pub mod context;
pub mod cursor;
pub mod history;
pub mod key;
pub mod lineinfo;
pub mod rope;
pub mod store;
