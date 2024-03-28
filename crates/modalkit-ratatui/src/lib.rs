//! # modalkit-ratatui
//!
//! ## Overview
//!
//! This crate contains widgets that can be used to build modal editing TUI applications using
//! the [ratatui] and [modalkit] crates.
//!
//! ## Example
//!
//! The following example shows a program that opens a single textbox where the user can enter text
//! using Vim keybindings.
//!
//! For a more complete example that includes a command bar and window splitting, see
//! `examples/editor.rs` in the source repository.
//!
//! ```no_run
//! use modalkit::{
//!     actions::{Action, Editable, Jumpable, Scrollable},
//!     editing::{context::Resolve, key::KeyManager, store::Store},
//!     editing::application::EmptyInfo,
//!     errors::UIResult,
//!     env::vim::keybindings::default_vim_keys,
//!     key::TerminalKey,
//!     keybindings::BindingMachine,
//! };
//! use modalkit_ratatui::textbox::{TextBoxState, TextBox};
//! use modalkit_ratatui::TerminalExtOps;
//!
//! use modalkit::crossterm::event::{read, Event};
//! use modalkit::crossterm::terminal::EnterAlternateScreen;
//! use ratatui::{backend::CrosstermBackend, Terminal};
//! use std::io::stdout;
//!
//! fn main() -> UIResult<(), EmptyInfo> {
//!     let mut stdout = stdout();
//!
//!     crossterm::terminal::enable_raw_mode()?;
//!     crossterm::execute!(stdout, EnterAlternateScreen)?;
//!
//!     let backend = CrosstermBackend::new(stdout);
//!     let mut terminal = Terminal::new(backend)?;
//!     let mut store = Store::default();
//!     let mut bindings = KeyManager::new(default_vim_keys::<EmptyInfo>());
//!     let mut tbox = TextBoxState::new(store.load_buffer(String::from("*scratch*")));
//!
//!     terminal.clear()?;
//!
//!     loop {
//!         terminal.draw(|f| f.render_stateful_widget(TextBox::new(), f.size(), &mut tbox))?;
//!
//!         if let Event::Key(key) = read()? {
//!             bindings.input_key(key.into());
//!         } else {
//!             continue;
//!         };
//!
//!         while let Some((act, ctx)) = bindings.pop() {
//!             let store = &mut store;
//!
//!             let _ = match act {
//!                 Action::Editor(act) => tbox.editor_command(&act, &ctx, store)?,
//!                 Action::Macro(act) => bindings.macro_command(&act, &ctx, store)?,
//!                 Action::Scroll(style) => tbox.scroll(&style, &ctx, store)?,
//!                 Action::Repeat(rt) => {
//!                     bindings.repeat(rt, Some(ctx));
//!                     None
//!                 },
//!                 Action::Jump(l, dir, count) => {
//!                     let _ = tbox.jump(l, dir, ctx.resolve(&count), &ctx)?;
//!                     None
//!                 },
//!                 Action::Suspend => terminal.program_suspend()?,
//!                 Action::NoOp => None,
//!                 _ => continue,
//!             };
//!         }
//!     }
//! }
//! ```

// Require docs for public APIs, and disable the more annoying clippy lints.
#![deny(missing_docs)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::manual_range_contains)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::needless_return)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::type_complexity)]
use std::io::{stdout, Stdout};
use std::process;

use ratatui::{
    backend::CrosstermBackend,
    buffer::Buffer,
    layout::Rect,
    style::{Color, Style},
    text::{Line, Span},
    widgets::Paragraph,
    Frame,
    Terminal,
};

use crossterm::{
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};

use modalkit::actions::Action;
use modalkit::editing::{application::ApplicationInfo, completion::CompletionList, store::Store};
use modalkit::errors::{EditResult, UIResult};
use modalkit::prelude::*;

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
    fn get_win_title(&self, store: &mut Store<I>) -> Line;

    /// Get the title to show in the tab list when this is the currently focused window.
    ///
    /// The default implementation will use the same title as shown in the window.
    fn get_tab_title(&self, store: &mut Store<I>) -> Line {
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
pub fn render_cursor<T: TerminalCursor>(f: &mut Frame, widget: &T, cursor: Option<char>) {
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
