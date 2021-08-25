pub mod bindings;

pub trait InputContext: Clone + Default {
    /// Reset any action-specific state.
    fn reset(&mut self);

    /// Return a copy of the InputContext, and reset any action-specific state.
    fn take(&mut self) -> Self;
}
