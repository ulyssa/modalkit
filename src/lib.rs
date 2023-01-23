//! # modalkit
//!
//! ## Overview
//!
//! This crate allows building terminal applications that support modal input, such as
//! the Vim text editor.
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
//!     editing::{context::Resolve, key::KeyManager, store::Store},
//!     editing::action::{Action, Editable, Jumpable, Scrollable, UIResult},
//!     editing::application::EmptyInfo,
//!     env::vim::keybindings::VimMachine,
//!     input::{bindings::BindingMachine, key::TerminalKey},
//!     widgets::textbox::{TextBoxState, TextBox},
//!     widgets::TerminalExtOps,
//! };
//!
//! use modalkit::crossterm::event::{read, Event};
//! use modalkit::crossterm::terminal::EnterAlternateScreen;
//! use modalkit::tui::{backend::CrosstermBackend, Terminal};
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
//!     let mut bindings = KeyManager::new(VimMachine::<TerminalKey>::default());
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

#[macro_use]
mod util;

pub mod editing;
pub mod env;
pub mod input;

#[cfg(feature = "readline")]
pub mod readline;

pub use crossterm;

#[cfg(feature = "tui")]
pub use tui;

#[cfg(feature = "widgets")]
pub mod widgets;
