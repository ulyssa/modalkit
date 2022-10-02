//! # Input processing
//!
//! ## Overview
//!
//! This module contains components to help consumers process keyboard input and  commands.
//!
pub mod bindings;
pub mod commands;
pub mod key;

/// Represents contextual information that is updated upon user input.
pub trait InputContext: Clone + Default {
    /// Override implementor-determined fields in this context using values from another.
    fn overrides(&mut self, other: &Self);

    /// Reset any action-specific state.
    fn reset(&mut self);

    /// Return a copy of the InputContext, and reset any action-specific state.
    fn take(&mut self) -> Self;
}
