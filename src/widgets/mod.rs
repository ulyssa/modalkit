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

use tui::{
    backend::CrosstermBackend,
    buffer::Buffer,
    layout::Rect,
    style::{Color, Style},
    text::{Span, Spans},
    widgets::Paragraph,
    Frame,
    Terminal,
};

use crossterm::{
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};

use crate::editing::{
    action::{Action, EditInfo, EditResult, UIResult},
    application::ApplicationInfo,
    base::{
        Axis,
        CloseFlags,
        Count,
        MoveDir1D,
        MoveDir2D,
        MovePosition,
        ScrollSize,
        WordStyle,
        WriteFlags,
    },
    completion::CompletionList,
    store::Store,
};

pub mod cmdbar;
pub mod list;
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
pub trait ScrollActions<C, S, I>
where
    I: ApplicationInfo,
{
    /// Pan the viewport.
    fn dirscroll(
        &mut self,
        dir: MoveDir2D,
        size: ScrollSize,
        count: &Count,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    /// Scroll so that the cursor is placed along a viewport boundary.
    fn cursorpos(
        &mut self,
        pos: MovePosition,
        axis: Axis,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    /// Scroll so that a specific line is placed at a given place in the viewport.
    fn linepos(
        &mut self,
        pos: MovePosition,
        count: &Count,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;
}

/// A widget that contains content that can be converted into an action when the user is done
/// entering text.
pub trait PromptActions<C, S, I>
where
    I: ApplicationInfo,
{
    /// Submit the currently entered text.
    fn submit(&mut self, ctx: &C, store: &mut S) -> EditResult<Vec<(Action<I>, C)>, I>;

    /// Abort command entry and reset the current contents.
    ///
    /// If `empty` is true, and there is currently entered text, do nothing.
    fn abort(&mut self, empty: bool, ctx: &C, store: &mut S) -> EditResult<Vec<(Action<I>, C)>, I>;

    /// Recall previously entered text.
    fn recall(
        &mut self,
        dir: &MoveDir1D,
        count: &Count,
        prefixed: bool,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<Vec<(Action<I>, C)>, I>;
}

/// Trait to allow widgets to control how they get drawn onto the screen when they are either
/// focused or unfocused.
pub trait WindowOps<I: ApplicationInfo>: TerminalCursor {
    /// Create a copy of this window during a window split.
    fn dup(&self, store: &mut Store<I>) -> Self;

    /// Perform any necessary cleanup for this window and close it.
    ///
    /// If this function returns false, it's because the window cannot be closed, at least not with
    /// the provided set of flags.
    fn close(&mut self, flags: CloseFlags, store: &mut Store<I>) -> bool;

    /// Draw this window into the buffer for the prescribed area.
    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut Store<I>);

    /// Get completion candidates to show the user.
    fn get_completions(&self) -> Option<CompletionList>;

    /// Returns the word following the current cursor position in this window.
    fn get_cursor_word(&self, style: &WordStyle) -> Option<String>;

    /// Returns the currently selected text in this window.
    fn get_selected_word(&self) -> Option<String>;

    /// Write the contents of the window.
    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I>;
}

/// A widget that the user can open and close on the screen.
pub trait Window<I: ApplicationInfo>: WindowOps<I> + Sized {
    /// Get the identifier for this window.
    fn id(&self) -> I::WindowId;

    /// Get the title to show in the window layout.
    fn get_win_title(&self, store: &mut Store<I>) -> Spans;

    /// Get the title to show in the tab list when this is the currently focused window.
    ///
    /// The default implementation will use the same title as shown in the window.
    fn get_tab_title(&self, store: &mut Store<I>) -> Spans {
        self.get_win_title(store)
    }

    /// Open a window that displays the content referenced by `id`.
    fn open(id: I::WindowId, store: &mut Store<I>) -> UIResult<Self, I>;

    /// Open a window given a name to lookup.
    fn find(name: String, store: &mut Store<I>) -> UIResult<Self, I>;

    /// Open a globally indexed window given a position.
    fn posn(index: usize, store: &mut Store<I>) -> UIResult<Self, I>;

    /// Open a default window when no target has been specified.
    fn unnamed(store: &mut Store<I>) -> UIResult<Self, I>;
}

/// Position and draw a terminal cursor.
pub fn render_cursor<T: TerminalCursor>(
    f: &mut Frame<CrosstermBackend<Stdout>>,
    widget: &T,
    cursor: Option<char>,
) {
    if let Some((cx, cy)) = widget.get_term_cursor() {
        if let Some(c) = cursor {
            let style = Style::default().fg(Color::Green);
            let span = Span::styled(c.to_string(), style);
            let para = Paragraph::new(span);
            let inner = Rect::new(cx, cy, 1, 1);
            f.render_widget(para, inner)
        }
        f.set_cursor(cx, cy);
    }
}

/// Extended operations for [Terminal].
pub trait TerminalExtOps {
    /// Result type for terminal operations.
    type Result;

    /// Suspend the process.
    fn program_suspend(&mut self) -> Self::Result;
}

impl TerminalExtOps for Terminal<CrosstermBackend<Stdout>> {
    type Result = Result<EditInfo, std::io::Error>;

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
