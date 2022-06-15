//! # Terminal UI widgets
//!
//! ## Overview
//!
//! This module contains components that can be used with the [tui] crate.
//!
//! [tui]: https://docs.rs/tui/latest/tui/
//!
use tui::buffer::Buffer;
use tui::layout::Rect;

use crate::input::commands::{Command, CommandError};
use crate::input::InputContext;

use crate::editing::buffer::Editable;

use crate::editing::base::{
    Axis,
    CloseFlags,
    Count,
    EditError,
    EditResult,
    MoveDir1D,
    ScrollStyle,
    WindowAction,
};

pub mod textbox;
pub mod windows;

mod util;

pub type TermOffset = (u16, u16);

/// A widget that the user's cursor can be placed into.
pub trait TerminalCursor {
    /// Returns the current offset of the cursor, relative to the upper left corner of the
    /// terminal.
    fn get_term_cursor(&self) -> TermOffset;
}

/// A widget that contains content that can be converted into an action when the user is done
/// entering text.
pub trait Submitable<A, C: InputContext> {
    fn submit(&mut self, ctx: &mut C) -> Option<A>;
}

/// A widget that the user can switch focus of keyboard input to.
pub trait Focusable<C>: Editable<C> + TerminalCursor {
    /// Scroll the viewable content in this widget.
    fn scroll(&mut self, style: &ScrollStyle, ctx: &C) -> EditResult;
}

/// A widget that the user can open and close on the screen.
pub trait Window: TerminalCursor {
    /// Draw this window into the buffer for the prescribed area.
    fn draw(&mut self, area: Rect, buf: &mut Buffer);

    /// Create a copy of this window during a window split.
    fn dup(&self) -> Self;

    /// Perform any necessary cleanup for this window and close it.
    ///
    /// If this function returns false, it's because the window cannot be closed, at least not with
    /// the provided set of flags.
    fn close(&mut self, flags: CloseFlags) -> bool;
}

/// A widget that contains [Windows](Window).
pub trait WindowContainer<W: Window, C> {
    fn windows(&self) -> usize;

    /// Open a new [Window] with height or width [*n*](Count) (depending on the [Axis]).
    fn window_open(
        &mut self,
        window: W,
        axis: Axis,
        rel: MoveDir1D,
        open: Option<Count>,
        ctx: &C,
    ) -> EditResult;

    fn window_command(&mut self, action: WindowAction, ctx: &C) -> EditResult;
}

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum UIError<C: Command> {
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Input/Output Error: {0}")]
    TerminalError(#[from] crossterm::ErrorKind),
    #[error("Editing error: {0}")]
    EditingFailure(#[from] EditError),
    #[error("Failed command: {0}")]
    CommandFailure(#[from] CommandError<C>),
}
