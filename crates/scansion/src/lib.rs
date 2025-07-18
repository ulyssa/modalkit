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
//!     keybindings::InputBindings,
//!     key::TerminalKey,
//! };
//! use scansion::{ReadLine, ReadLineInfo};
//!
//! fn main() -> Result<(), std::io::Error> {
//!     let mut vi = VimMachine::<TerminalKey, ReadLineInfo>::empty();
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
use std::collections::VecDeque;
use std::io::{self, Write};
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

use modalkit::actions::{
    Action,
    CommandBarAction,
    Editable,
    EditorAction,
    InsertTextAction,
    Jumpable,
    PromptAction,
};

use modalkit::editing::{
    application::{ApplicationContentId, ApplicationInfo, ApplicationWindowId},
    context::{EditContext, Resolve},
    history::HistoryList,
    key::KeyManager,
    rope::EditRope,
    store::Store,
};

use modalkit::{
    errors::{EditError, EditResult, UIError},
    key::{MacroError, TerminalKey},
    keybindings::BindingMachine,
    prelude::*,
};

mod editor;

use self::editor::{Editor, EditorContext};

const HISTORY_LENGTH: usize = 100;

fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

enum InternalResult<I: ApplicationInfo> {
    Submitted(EditRope),
    Actions(Vec<(Action<I>, EditContext)>),
    Nothing,
}

macro_rules! focused_mut {
    ($s: expr) => {
        match &$s.ct {
            Some(CommandType::Command) => &mut $s.cmd,
            Some(CommandType::Search) => &mut $s.search,
            None => &mut $s.line,
        }
    };
}

/// Identifiers for the buffers used by [ReadLine].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ReadLineId {
    /// The line editor.
    Line,

    /// A command bar.
    Command(CommandType),
}

impl ApplicationWindowId for ReadLineId {}
impl ApplicationContentId for ReadLineId {}

/// Default [ApplicationInfo] used by [ReadLine].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ReadLineInfo {}

impl ApplicationInfo for ReadLineInfo {
    type Error = String;
    type Action = ();
    type Store = ();
    type WindowId = ReadLineId;
    type ContentId = ReadLineId;

    fn content_of_command(ct: CommandType) -> ReadLineId {
        ReadLineId::Command(ct)
    }
}

/// Error type for [ReadLine] editor.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum ReadLineError<I>
where
    I: ApplicationInfo,
{
    /// Failure during I/O.
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),

    /// Failure during editing.
    #[error("Editing error: {0}")]
    EditingFailure(#[from] EditError<I>),

    /// Failure in the user interface.
    #[error("{0}")]
    UserInterfaceError(#[from] UIError<I>),

    /// Failure during editing.
    #[error("Macro error: {0}")]
    MacroFailure(#[from] MacroError),

    /// Failure due to using an unknown command.
    #[error("Unknown command: {0:?}")]
    UnknownCommand(String),
}

/// Result type when using [ReadLine::readline].
pub type ReadLineResult<I> = Result<String, ReadLineError<I>>;

/// Simple editor for collecting user input.
pub struct ReadLine<I = ReadLineInfo>
where
    I: ApplicationInfo<ContentId = ReadLineId>,
{
    actstack: VecDeque<(Action<I>, EditContext)>,
    bindings: KeyManager<TerminalKey, Action<I>, RepeatType>,
    store: Store<I>,

    history: HistoryList<EditRope>,

    context: EditorContext,
    dimensions: (u16, u16),

    ct: Option<CommandType>,
    act: Option<(Action<I>, EditContext)>,
    cprompt: String,

    line: Editor<I>,
    cmd: Editor<I>,
    search: Editor<I>,
}

impl<I> ReadLine<I>
where
    I: ApplicationInfo<ContentId = ReadLineId>,
{
    /// Create a new instance.
    pub fn new<B: BindingMachine<TerminalKey, Action<I>, RepeatType, EditContext> + 'static>(
        bindings: B,
    ) -> Result<Self, std::io::Error>
    where
        I::Store: Default,
    {
        let dimensions = crossterm::terminal::size()?;
        let context = EditorContext::default();

        let store = Store::<I>::default();

        let mut line = Editor::new(ReadLineId::Line);
        let mut cmd = Editor::new(ReadLineId::Command(CommandType::Command));
        let mut search = Editor::new(ReadLineId::Command(CommandType::Search));
        line.resize(dimensions.0, dimensions.1);
        cmd.resize(dimensions.0, dimensions.1);
        search.resize(dimensions.0, dimensions.1);

        let actstack = VecDeque::default();
        let bindings = KeyManager::new(bindings);
        let history = HistoryList::new("".into(), HISTORY_LENGTH);

        Ok(ReadLine {
            actstack,
            bindings,
            store,

            history,

            context,
            dimensions,

            ct: None,
            act: None,
            cprompt: String::new(),

            line,
            cmd,
            search,
        })
    }

    /// Prompt the user for input.
    pub fn readline(&mut self, prompt: Option<String>) -> ReadLineResult<I> {
        crossterm::terminal::enable_raw_mode()?;

        self.init()?;

        loop {
            let key = self.step(&prompt)?;

            self.bindings.input_key(key);

            while let Some((action, ctx)) = self.action_pop() {
                match self.act(action, ctx) {
                    Ok(InternalResult::Nothing) => continue,
                    Ok(InternalResult::Actions(acts)) => {
                        self.action_prepend(acts);
                        continue;
                    },
                    Ok(InternalResult::Submitted(res)) => {
                        self.linebreak()?;

                        crossterm::terminal::disable_raw_mode()?;

                        return Ok(res.to_string());
                    },
                    Err(e) => {
                        self.linebreak()?;

                        crossterm::terminal::disable_raw_mode()?;

                        return Err(e);
                    },
                }
            }
        }
    }

    fn action_prepend(&mut self, acts: Vec<(Action<I>, EditContext)>) {
        let mut acts = VecDeque::from(acts);
        acts.append(&mut self.actstack);
        self.actstack = acts;
    }

    fn action_pop(&mut self) -> Option<(Action<I>, EditContext)> {
        if let res @ Some(_) = self.actstack.pop_front() {
            res
        } else {
            self.bindings.pop()
        }
    }

    fn step(&mut self, prompt: &Option<String>) -> Result<TerminalKey, ReadLineError<I>> {
        loop {
            self.redraw(prompt.as_deref())?;

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
                    let ctx = self.bindings.context().clone();
                    let act = InsertTextAction::Transcribe(s, MoveDir1D::Previous, 1.into());
                    let act = EditorAction::InsertText(act);

                    let _ = focused_mut!(self).editor_command(&act, &ctx, &mut self.store)?;
                },
                Event::Resize(width, height) => {
                    self.resize(width, height);
                },
            }
        }
    }

    fn suspend(&mut self) -> Result<InternalResult<I>, ReadLineError<I>> {
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
        act: &CommandBarAction<I>,
        ctx: EditContext,
    ) -> Result<InternalResult<I>, ReadLineError<I>> {
        match act {
            CommandBarAction::Focus(s, ct, act) => {
                self.ct = Some(*ct);
                self.act = Some((act.as_ref().clone(), ctx));
                self.cprompt = s.clone();

                Ok(InternalResult::Nothing)
            },
            CommandBarAction::Unfocus => {
                self.ct = None;

                Ok(InternalResult::Nothing)
            },
        }
    }

    fn prompt(
        &mut self,
        act: PromptAction,
        ctx: EditContext,
    ) -> Result<InternalResult<I>, ReadLineError<I>> {
        match act {
            PromptAction::Submit => {
                let res = self.submit();
                self.ct = None;

                Ok(res)
            },
            PromptAction::Abort(empty) => {
                self.abort(empty);

                Ok(InternalResult::Nothing)
            },
            PromptAction::Recall(filter, dir, count) => {
                self.recall(filter, dir, ctx.resolve(&count));

                Ok(InternalResult::Nothing)
            },
        }
    }

    fn abort(&mut self, empty: bool) {
        if empty && !self.cmd.is_blank() {
            return;
        }

        if let Some(ct) = self.ct {
            let txt = self.reset_cmd();
            self.store.registers.set_aborted_command(ct, txt);
            self.ct = None;
        }
    }

    fn recall(&mut self, filter: RecallFilter, dir: MoveDir1D, count: usize) {
        match self.ct {
            None => {
                let text = self.line.recall(&mut self.history, filter, dir, count);

                if let Some(text) = text {
                    self.line.set_text(text);
                }
            },
            Some(ct) => {
                let hist = self.store.registers.get_command_history(ct);
                let text = self.cmd.recall(hist, filter, dir, count);

                if let Some(text) = text {
                    self.cmd.set_text(text);
                }
            },
        }
    }

    fn get_cmd_regex(&mut self) -> EditResult<Regex, I> {
        let text = self.cmd.get_trim();

        if !text.is_empty() {
            let re = Regex::new(text.to_string().as_ref())?;

            return Ok(re);
        }

        // If the search bar is focused, but nothing has been typed, we move backwards to the
        // previously typed search and use that.

        let hist = self.store.registers.get_command_history(CommandType::Search);
        let text = self
            .cmd
            .recall(hist, RecallFilter::All, MoveDir1D::Previous, 1)
            .ok_or(EditError::NoSearch)?;

        let re = Regex::new(text.to_string().as_ref())?;

        self.cmd.set_text(text);

        return Ok(re);
    }

    fn get_regex(&mut self) -> EditResult<Regex, I> {
        let re = if let Some(CommandType::Search) = self.ct {
            self.get_cmd_regex()?
        } else {
            let text = self.store.registers.get_last_search();

            Regex::new(text.to_string().as_ref())?
        };

        return Ok(re);
    }

    fn search(
        &mut self,
        flip: MoveDirMod,
        count: Count,
        ctx: &EditContext,
    ) -> EditResult<EditInfo, I> {
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

    fn incsearch(&mut self, ctx: &EditContext) -> EditResult<(), I> {
        let Some(CommandType::Search) = self.ct else {
            return Ok(());
        };

        if !ctx.is_search_incremental() {
            return Ok(());
        }

        let needle = self.cmd.get_trim().to_string();
        let needle = Regex::new(needle.as_ref())?;
        let dir = ctx.get_search_regex_dir();

        if let Some(text) = self.line.find(&mut self.history, &needle, dir, true) {
            self.line.set_text(text);
        }

        Ok(())
    }

    fn edit(&mut self, act: EditorAction, ctx: &EditContext) -> EditResult<EditInfo, I> {
        match act {
            EditorAction::Edit(ea, EditTarget::Motion(MoveType::Line(dir), count)) => {
                let ea = ctx.resolve(&ea);

                if ea.is_motion() {
                    let n = focused_mut!(self).line_leftover(dir, ctx.resolve(&count));

                    if n > 0 {
                        // If we move by more lines than there are in the buffer, then we
                        // treat the remainder as history recall.
                        self.recall(RecallFilter::All, dir, n);

                        return Ok(None);
                    }
                }

                let mov = EditTarget::Motion(MoveType::Line(dir), count);
                let act = EditorAction::Edit(ea.into(), mov);

                focused_mut!(self).editor_command(&act, ctx, &mut self.store)
            },
            act => {
                let res = focused_mut!(self).editor_command(&act, ctx, &mut self.store)?;

                // Perform an incremental search if we need to.
                self.incsearch(ctx)?;

                return Ok(res);
            },
        }
    }

    fn submit(&mut self) -> InternalResult<I> {
        if let Some(ct) = self.ct {
            let text = self.reset_cmd();
            self.store.registers.set_last_command(ct, text);

            if let Some(act) = self.act.take() {
                InternalResult::Actions(vec![act])
            } else {
                InternalResult::Nothing
            }
        } else {
            let text = self.line.reset();
            self.history.select(text.clone());
            InternalResult::Submitted(text)
        }
    }

    fn redraw(&mut self, prompt: Option<&str>) -> Result<(), io::Error> {
        self.context.stdout.queue(CursorHide)?;

        self.context
            .stdout
            .queue(MoveTo(0, self.context.top))?
            .queue(Clear(ClearType::FromCursorDown))?;

        let lines = self.line.redraw(prompt, 0, &mut self.context)?;

        if self.ct.is_some() {
            let p = Some(self.cprompt.as_str());
            let _ = self.cmd.redraw(p, lines, &mut self.context);
        }

        self.context.stdout.queue(CursorShow)?;
        self.context.stdout.flush()?;

        Ok(())
    }

    fn linebreak(&mut self) -> Result<(), io::Error> {
        self.context.stdout.queue(Print("\r\n"))?;
        self.context.stdout.flush()?;

        Ok(())
    }

    fn init(&mut self) -> Result<(), io::Error> {
        let (col, mut row) = crossterm::cursor::position()?;

        if col > 0 {
            // Move to next line if the cursor is after text.
            row += 1;
        }

        if row >= self.dimensions.1 {
            self.linebreak()?;

            row = row.saturating_sub(1);
        }

        self.context.top = row;

        Ok(())
    }

    fn resize(&mut self, width: u16, height: u16) {
        let oldh = self.dimensions.1;
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

    fn act(
        &mut self,
        action: Action<I>,
        ctx: EditContext,
    ) -> Result<InternalResult<I>, ReadLineError<I>> {
        let store = &mut self.store;

        let _ = match action {
            // Do nothing.
            Action::NoOp => None,

            // Application-specific action.
            Action::Application(_) => {
                // XXX: implement a way to let the consumer handle this.
                None
            },

            // Simple delegations.
            Action::CommandBar(cb) => return self.command_bar(&cb, ctx),
            Action::Macro(act) => self.bindings.macro_command(&act, &ctx, store)?,
            Action::Prompt(p) => return self.prompt(p, ctx),
            Action::Search(flip, count) => self.search(flip, count, &ctx)?,
            Action::Suspend => return self.suspend(),

            Action::Editor(act) => {
                let res = self.edit(act, &ctx)?;

                // Perform an incremental search if we need to.
                self.incsearch(&ctx)?;

                res
            },

            Action::RedrawScreen => {
                self.context.top = 0;

                None
            },

            Action::Jump(list, dir, ref count) => {
                let _ = focused_mut!(self).jump(list, dir, ctx.resolve(count), &ctx)?;

                None
            },
            Action::Repeat(rt) => {
                self.bindings.repeat(rt, Some(ctx));

                None
            },

            // EditInfo currently goes unused.
            Action::ShowInfoMessage(msg) => Some(msg),

            // Unimplemented.
            Action::Command(_) => {
                // XXX: implement
                None
            },
            Action::KeywordLookup(_) => {
                // XXX: implement
                None
            },

            // Irrelevant to readline behaviour.
            Action::Tab(_) => None,
            Action::Window(_) => None,
            Action::Scroll(_) => None,

            // Handle non-exhaustive pattern.
            act => {
                let msg = format!("unknown action: {act:?}");
                return Err(UIError::Unimplemented(msg).into());
            },
        };

        return Ok(InternalResult::Nothing);
    }
}
