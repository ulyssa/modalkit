//! # Readline-style editor
//!
//! ## Overview
//!
//! This module provides a readline-style editor for getting user input. It is meant to work with
//! any [BindingMachine] implementor that maps input to an [Action].
//!
//! ## Example
//!
//! ```no_run
//! use modalkit::{
//!     env::vim::keybindings::{VimBindings, VimMachine},
//!     input::bindings::InputBindings,
//!     input::key::TerminalKey,
//!     readline::ReadLine,
//! };
//!
//! fn main() -> Result<(), std::io::Error> {
//!     let mut vi = VimMachine::<TerminalKey>::empty();
//!     VimBindings::default().submit_on_enter().setup(&mut vi);
//!
//!     let mut rl = ReadLine::new(vi)?;
//!
//!     loop {
//!         match rl.readline(Some("> ".to_string())) {
//!             Ok(s) => match s.trim() {
//!                 "q" | "quit" => {
//!                     return Ok(());
//!                 },
//!                 _ => {
//!                     println!("User typed: {:?}", s);
//!                 }
//!             },
//!             Err(e) => {
//!                 // Print out editor error messages.
//!                 println!("{}", e);
//!             },
//!         }
//!     }
//! }
//! ```
//!
use std::io::Write;
use std::process;
use std::time::Duration;

use regex::Regex;

use crossterm::{
    cursor::{Hide as CursorHide, MoveTo, Show as CursorShow},
    event::{poll, read, Event},
    style::Print,
    terminal::{Clear, ClearType},
    QueueableCommand,
};

use crate::editing::{
    base::{
        Action,
        Application,
        CommandBarAction,
        CommandType,
        Count,
        EditAction,
        EditContext,
        EditError,
        EditResult,
        EditTarget,
        InsertTextAction,
        MoveDir1D,
        MoveDirMod,
        MoveType,
        Register,
        RepeatType,
    },
    buffer::Editable,
    history::HistoryList,
    key::KeyManager,
    rope::EditRope,
    store::{SharedStore, Store},
};

use crate::{
    input::{
        bindings::BindingMachine,
        key::{MacroError, TerminalKey},
        InputContext,
    },
    util::is_newline,
};

mod editor;

use self::editor::{Editor, EditorContext};

const HISTORY_LENGTH: usize = 100;

enum InternalResult {
    Submitted(EditRope),
    Nothing,
}

fn command_to_str(ct: &CommandType) -> Option<String> {
    match ct {
        &CommandType::Search(MoveDir1D::Next, _) => {
            return "/".to_string().into();
        },
        &CommandType::Search(MoveDir1D::Previous, _) => {
            return "?".to_string().into();
        },
        &CommandType::Command => {
            return ":".to_string().into();
        },
    }
}

/// Error type for [ReadLine] editor.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum ReadLineError {
    /// Failure during I/O.
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),

    /// Failure during editing.
    #[error("Editing error: {0}")]
    EditingFailure(#[from] EditError),

    /// Failure during editing.
    #[error("Macro error: {0}")]
    MacroFailure(#[from] MacroError),

    /// Failure due to using an unknown command.
    #[error("Unknown command: {0:?}")]
    UnknownCommand(String),
}

/// Result type when using [ReadLine::readline].
pub type ReadLineResult = Result<String, ReadLineError>;

/// Simple editor for collecting user input.
pub struct ReadLine<C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    bindings: KeyManager<TerminalKey, Action<P>, RepeatType, C, P>,
    store: SharedStore<C, P>,

    history: HistoryList<EditRope>,

    context: EditorContext,
    dimensions: (u16, u16),
    ct: Option<CommandType>,

    line: Editor<C, P>,
    cmd: Editor<C, P>,
}

impl<C, P> ReadLine<C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    /// Create a new instance.
    pub fn new<B: BindingMachine<TerminalKey, Action<P>, RepeatType, C> + 'static>(
        bindings: B,
    ) -> Result<Self, std::io::Error> {
        let dimensions = crossterm::terminal::size()?;
        let context = EditorContext::default();

        let store = Store::<C, P>::new();

        let mut line = Editor::new(store.clone());
        let mut cmd = Editor::new(store.clone());
        line.resize(dimensions.0, dimensions.1);
        cmd.resize(dimensions.0, dimensions.1);

        let bindings = KeyManager::new(bindings, store.clone());
        let history = HistoryList::new("".into(), HISTORY_LENGTH);

        let rl = ReadLine {
            bindings,
            store,

            history,

            context,
            dimensions,
            ct: None,

            line,
            cmd,
        };

        return Ok(rl);
    }

    /// Prompt the user for input.
    pub fn readline(&mut self, prompt: Option<String>) -> ReadLineResult {
        crossterm::terminal::enable_raw_mode()?;

        self.init()?;

        loop {
            let key = self.step(&prompt)?;

            self.bindings.input_key(key);

            while let Some((action, ctx)) = self.bindings.pop() {
                match self.act(action, ctx) {
                    Ok(InternalResult::Nothing) => continue,
                    Ok(InternalResult::Submitted(res)) => {
                        self.linebreak()?;

                        crossterm::terminal::disable_raw_mode()?;

                        return Ok(res.to_string());
                    },
                    Err(e) => {
                        self.linebreak()?;

                        crossterm::terminal::disable_raw_mode()?;

                        return Err(e.into());
                    },
                }
            }
        }
    }

    fn step(&mut self, prompt: &Option<String>) -> Result<TerminalKey, ReadLineError> {
        loop {
            self.redraw(prompt)?;

            if !poll(Duration::from_millis(500))? {
                continue;
            }

            match read()? {
                Event::Key(ke) => {
                    return Ok(ke.into());
                },
                Event::FocusGained | Event::FocusLost => {
                    // Do nothing for now.
                },
                Event::Mouse(_) => {
                    // Do nothing for now.
                },
                Event::Paste(s) => {
                    let mut ctx = self.bindings.context().clone();
                    let act = InsertTextAction::Transcribe(s, MoveDir1D::Previous, 1.into());

                    // Reset action-specific state.
                    ctx.reset();

                    let _ = self.focused_mut().insert_text(act, &ctx)?;
                },
                Event::Resize(width, height) => {
                    self.resize(width, height);
                },
            }
        }
    }

    fn suspend(&mut self) -> Result<InternalResult, ReadLineError> {
        // Restore old terminal state.
        crossterm::terminal::disable_raw_mode()?;
        self.context.stdout.queue(CursorShow)?.flush()?;

        // Send SIGTSTP to process.
        let pid = process::id();

        #[cfg(unix)]
        unsafe {
            libc::kill(pid as i32, libc::SIGTSTP);
        }

        // Restore application terminal state.
        crossterm::terminal::enable_raw_mode()?;
        self.init()?;

        Ok(InternalResult::Nothing)
    }

    fn reset_cmd(&mut self) -> EditRope {
        self.cmd.reset().trim_end_matches(is_newline)
    }

    fn command_bar(
        &mut self,
        act: CommandBarAction,
        ctx: C,
    ) -> Result<InternalResult, ReadLineError> {
        match act {
            CommandBarAction::Submit => {
                let res = self.submit();
                self.ct = None;

                return res;
            },
            CommandBarAction::Focus(ct) => {
                self.ct = Some(ct);

                Ok(InternalResult::Nothing)
            },
            CommandBarAction::Abort => {
                self.abort();

                Ok(InternalResult::Nothing)
            },
            CommandBarAction::Recall(dir, count) => {
                self.recall(dir, ctx.resolve(&count));

                Ok(InternalResult::Nothing)
            },
        }
    }

    fn abort(&mut self) {
        match self.ct {
            None => {},
            Some(CommandType::Search(_, _)) => {
                let entry = self.reset_cmd();
                let mut locked = self.store.write().unwrap();

                if entry.len() > 0 {
                    locked.searches.select(entry);
                } else {
                    let _ = locked.searches.end();
                }

                self.ct = None;
            },
            Some(CommandType::Command) => {
                let _ = self.reset_cmd();

                self.ct = None;
            },
        }
    }

    fn recall(&mut self, dir: MoveDir1D, count: usize) {
        match self.ct {
            None => {
                let text = self.line.recall(&mut self.history, dir, count);

                if let Some(text) = text {
                    self.line.set_text(text);
                }
            },
            Some(CommandType::Search(_, _)) => {
                let text = {
                    let mut locked = self.store.write().unwrap();
                    self.cmd.recall(&mut locked.searches, dir, count)
                };

                if let Some(text) = text {
                    self.cmd.set_text(text);
                }
            },
            Some(CommandType::Command) => {
                // Does nothing for now.

                return;
            },
        }
    }

    fn get_cmd_regex(&mut self) -> EditResult<Regex> {
        let text = self.cmd.get_trim();

        if text.len() > 0 {
            let re = Regex::new(text.to_string().as_ref())?;

            return Ok(re);
        }

        // If the search bar is focused, but nothing has been typed, we move backwards to the
        // previously typed search and use that.

        let text = {
            let mut locked = self.store.write().unwrap();
            let text = self.cmd.recall(&mut locked.searches, MoveDir1D::Previous, 1);

            text.ok_or(EditError::NoSearch)?
        };

        let re = Regex::new(text.to_string().as_ref())?;

        self.cmd.set_text(text);

        return Ok(re);
    }

    fn get_regex(&mut self) -> EditResult<Regex> {
        let re = if let Some(CommandType::Search(_, _)) = self.ct {
            self.get_cmd_regex()?
        } else {
            let locked = self.store.write().unwrap();
            let text = locked.registers.get(&Some(Register::LastSearch)).value;

            Regex::new(text.to_string().as_ref())?
        };

        return Ok(re);
    }

    fn search(&mut self, flip: MoveDirMod, count: Count, ctx: C) -> EditResult {
        let count = ctx.resolve(&count);
        let needle = self.get_regex()?;
        let dir = ctx.get_search_regex_dir();
        let dir = flip.resolve(&dir);

        let mut res = None;

        for _ in 0..count {
            if let Some(v) = self.line.find(&mut self.history, &needle, dir, false) {
                let _ = res.insert(v);
            }
        }

        if let Some(text) = res {
            self.line.set_text(text);
        }

        Ok(None)
    }

    fn incsearch(&mut self) -> Result<(), EditError> {
        if let Some(CommandType::Search(dir, true)) = self.ct {
            let needle = self.cmd.get_trim().to_string();
            let needle = Regex::new(needle.as_ref())?;

            if let Some(text) = self.line.find(&mut self.history, &needle, dir, true) {
                self.line.set_text(text);
            }
        }

        Ok(())
    }

    fn edit(&mut self, action: EditAction, mov: EditTarget, ctx: C) -> EditResult {
        match (action, mov) {
            (ea @ EditAction::Motion, EditTarget::Motion(MoveType::Line(dir), count)) => {
                let n = self.focused_mut().line_leftover(dir, ctx.resolve(&count));

                if n > 0 {
                    // If we move by more lines than there are in the buffer, then we
                    // treat the remainder as history recall.
                    self.recall(dir, n);

                    Ok(None)
                } else {
                    let mov = EditTarget::Motion(MoveType::Line(dir), count);

                    self.focused_mut().edit(&ea, &mov, &ctx)
                }
            },
            (ea, mov) => {
                let res = self.focused_mut().edit(&ea, &mov, &ctx)?;

                // Perform an incremental search if we need to.
                self.incsearch()?;

                return Ok(res);
            },
        }
    }

    fn submit(&mut self) -> Result<InternalResult, ReadLineError> {
        match self.ct {
            None => {
                let text = self.focused_mut().reset();

                self.history.select(text.clone());

                return Ok(InternalResult::Submitted(text));
            },
            Some(CommandType::Search(dir, _)) => {
                let text = self.reset_cmd();
                let needle = match Regex::new(text.to_string().as_ref()) {
                    Err(e) => return Err(EditError::from(e).into()),
                    Ok(r) => r,
                };

                Store::set_last_search(text, &self.store);

                if let Some(text) = self.line.find(&mut self.history, &needle, dir, false) {
                    self.line.set_text(text);
                }

                return Ok(InternalResult::Nothing);
            },
            Some(CommandType::Command) => {
                let cmd = self.reset_cmd().trim().to_string();
                let err = ReadLineError::UnknownCommand(cmd);

                return Err(err);
            },
        }
    }

    fn redraw(&mut self, prompt: &Option<String>) -> crossterm::Result<()> {
        self.context.stdout.queue(CursorHide)?;

        self.context
            .stdout
            .queue(MoveTo(0, self.context.top))?
            .queue(Clear(ClearType::FromCursorDown))?;

        let lines = self.line.redraw(prompt, 0, &mut self.context)?;

        if self.ct.is_some() {
            let p = self.ct.as_ref().and_then(command_to_str);
            let _ = self.cmd.redraw(&p, lines, &mut self.context);
        }

        self.context.stdout.queue(CursorShow)?;
        self.context.stdout.flush()?;

        Ok(())
    }

    fn linebreak(&mut self) -> crossterm::Result<()> {
        self.context.stdout.queue(Print("\r\n"))?;
        self.context.stdout.flush()?;

        Ok(())
    }

    fn init(&mut self) -> crossterm::Result<()> {
        let (col, mut row) = crossterm::cursor::position()?;

        if col > 0 {
            // Move to next line if the cursor is after text.
            row += 1;
        }

        if row >= self.dimensions.1 {
            self.linebreak()?;

            row = row.saturating_sub(1);
        }

        self.context.top = row.into();

        Ok(())
    }

    fn resize(&mut self, width: u16, height: u16) {
        let oldh = self.dimensions.1 as u16;
        let oldt = self.context.top;

        // Update terminal dimensions; we'll redraw when we loop.
        self.dimensions = (width, height);

        // Update editors.
        self.line.resize(width, height);
        self.cmd.resize(width, height);

        if oldt >= height - 1 {
            self.context.top = height - 2;
        } else if oldh < height {
            self.context.top = oldt + height - oldh;
        }
    }

    fn act(&mut self, action: Action<P>, ctx: C) -> Result<InternalResult, ReadLineError> {
        let _ = match action {
            // Do nothing.
            Action::NoOp => None,

            // Application-specific action.
            Action::Application(_) => {
                // XXX: implement a way to let the consumer handle this.
                None
            },

            Action::CommandBar(cb) => return self.command_bar(cb, ctx),
            Action::Suspend => return self.suspend(),

            // Simple delegations.
            Action::Edit(action, mov) => self.edit(ctx.resolve(&action), mov, ctx)?,
            Action::Macro(act) => self.bindings.macro_command(act, &ctx)?,
            Action::Mark(mark) => self.focused_mut().mark(ctx.resolve(&mark), &ctx)?,
            Action::Cursor(act) => self.focused_mut().cursor_command(act, &ctx)?,
            Action::Selection(act) => self.focused_mut().selection_command(act, &ctx)?,
            Action::History(act) => self.focused_mut().history_command(act, &ctx)?,
            Action::Search(flip, count) => self.search(flip, count, ctx)?,

            Action::RedrawScreen => {
                self.context.top = 0;

                None
            },

            Action::InsertText(act) => {
                let res = self.focused_mut().insert_text(act, &ctx)?;

                // Perform an incremental search if we need to.
                self.incsearch()?;

                res
            },

            Action::Repeat(rt) => {
                self.bindings.repeat(rt, Some(ctx));

                None
            },

            // Unimplemented.
            Action::Jump(_, _, _) => {
                // XXX: implement
                None
            },
            Action::Complete(_, _) => {
                // XXX: implement
                None
            },
            Action::Command(_) => {
                // XXX: implement
                None
            },
            Action::KeywordLookup => {
                // XXX: implement
                None
            },

            // Irrelevant to readline behaviour.
            Action::Tab(_) => None,
            Action::Window(_) => None,
            Action::Scroll(_) => None,
        };

        return Ok(InternalResult::Nothing);
    }

    fn focused_mut(&mut self) -> &mut Editor<C, P> {
        if self.ct.is_some() {
            return &mut self.cmd;
        } else {
            return &mut self.line;
        }
    }
}
