//! # Input processing
//!
//! ## Overview
//!
//! This module contains components to help consumers process keyboard input and  commands.
//!
pub mod bindings;
pub mod commands;
pub mod dialog;
pub mod key;

/// Represents contextual information that is updated upon user input.
pub trait InputState {
    /// The output context type returned along with actions.
    type Output: Clone + Default;

    /// Reset any action-specific state.
    fn reset(&mut self);

    /// Return a copy of the InputState, and reset any action-specific state.
    fn take(&mut self) -> Self::Output;

    /// Copy any overriding values into an `Output` object.
    fn merge(original: Self::Output, overrides: &Self::Output) -> Self::Output;
}
