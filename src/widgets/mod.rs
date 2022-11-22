//! # Terminal UI widgets
//!
//! ## Overview
//!
//! This module contains components that can be used with the [tui] crate.
//!
//! [tui]: https://docs.rs/tui/latest/tui/
//!
use std::io::{stdout, Stdout};
use std::process;

use libc;

use tui::{backend::CrosstermBackend, buffer::Buffer, layout::Rect, Terminal};

use crossterm::{
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};

use crate::editing::action::{EditInfo, EditResult, UIResult, WindowAction};

use crate::editing::base::{
    Axis,
    CloseFlags,
    Count,
    EditContext,
    MoveDir1D,
    MoveDir2D,
    MovePosition,
    ScrollSize,
};

pub mod cmdbar;
pub mod screen;
pub mod textbox;
pub mod windows;

mod util;

/// An offset from the upper-left corner of the terminal.
pub type TermOffset = (u16, u16);

/// A widget that the user's cursor can be placed into.
pub trait TerminalCursor {
    /// Returns the current offset of the cursor, relative to the upper left corner of the
    /// terminal.
    fn get_term_cursor(&self) -> Option<TermOffset>;
}

/// A widget whose content can be scrolled in multiple ways.
pub trait ScrollActions<C: EditContext> {
    /// Pan the viewport.
    fn dirscroll(&mut self, dir: MoveDir2D, size: ScrollSize, count: &Count, ctx: &C)
        -> EditResult;

    /// Scroll so that the cursor is placed along a viewport boundary.
    fn cursorpos(&mut self, pos: MovePosition, axis: Axis, ctx: &C) -> EditResult;

    /// Scroll so that a specific line is placed at a given place in the viewport.
    fn linepos(&mut self, pos: MovePosition, count: &Count, ctx: &C) -> EditResult;
}

/// A widget that contains content that can be converted into an action when the user is done
/// entering text.
pub trait PromptActions<A, C: EditContext> {
    /// Submit the currently entered text.
    fn submit(&mut self, ctx: &C) -> EditResult<Vec<(A, C)>>;

    /// Abort command entry and reset the current contents.
    ///
    /// If `empty` is true, and there is currently entered text, do nothing.
    fn abort(&mut self, empty: bool, ctx: &C) -> EditResult<Vec<(A, C)>>;

    /// Recall previously entered text.
    fn recall(&mut self, dir: &MoveDir1D, count: &Count, ctx: &C) -> EditResult<Vec<(A, C)>>;
}

/// A widget that the user can open and close on the screen.
pub trait Window: TerminalCursor {
    /// Draw this window into the buffer for the prescribed area.
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool);

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
    /// Number of currently open windows.
    fn windows(&self) -> usize;

    /// Open a new [Window] with height or width [*n*](Count) (depending on the [Axis]).
    fn window_open(
        &mut self,
        window: W,
        axis: Axis,
        rel: MoveDir1D,
        open: Option<Count>,
        ctx: &C,
    ) -> UIResult;

    /// Execute a window action.
    fn window_command(&mut self, action: WindowAction, ctx: &C) -> UIResult;
}

/// Extended operations for [Terminal].
pub trait TerminalExtOps {
    /// Result type for terminal operations.
    type Result;

    /// Suspend the process.
    fn program_suspend(&mut self) -> Self::Result;
}

impl TerminalExtOps for Terminal<CrosstermBackend<Stdout>> {
    type Result = crossterm::Result<Option<EditInfo>>;

    fn program_suspend(&mut self) -> Self::Result {
        let mut stdout = stdout();

        // Restore old terminal state.
        crossterm::terminal::disable_raw_mode()?;
        execute!(self.backend_mut(), LeaveAlternateScreen)?;
        self.show_cursor()?;

        // Send SIGTSTP to process.
        let pid = process::id();

        #[cfg(unix)]
        unsafe {
            libc::kill(pid as i32, libc::SIGTSTP);
        }

        // Restore application terminal state.
        crossterm::terminal::enable_raw_mode()?;
        crossterm::execute!(stdout, EnterAlternateScreen)?;
        self.clear()?;

        Ok(None)
    }
}
