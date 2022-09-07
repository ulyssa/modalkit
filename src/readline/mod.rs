//! # Readline-style editor
//!
//! ## Overview
//!
//! This module provides a readline-style editor for getting user input. It is meant to work with
//! any [ModalMachine] instance that maps input to an [Action].
//!
//! ## Example
//!
//! ```no_run
//! use modalkit::{
//!     input::bindings::InputBindings,
//!     readline::ReadLine,
//!     vim::keybindings::{VimBindings, VimMachine},
//! };
//!
//! use crossterm::event::KeyEvent;
//!
//! fn main() -> Result<(), std::io::Error> {
//!     let mut vi = VimMachine::<KeyEvent>::empty();
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
use std::cmp::Ordering;
use std::io::{stdout, BufWriter, Stdout, Write};
use std::ops::RangeInclusive;
use std::process;
use std::time::Duration;

use crossterm::{
    cursor::{Hide as CursorHide, MoveTo, Show as CursorShow},
    event::{poll, read, Event, KeyEvent},
    style::{Print, PrintStyledContent, Stylize},
    terminal::{Clear, ClearType, ScrollUp},
    QueueableCommand,
};

use crate::editing::{
    base::{
        Action,
        Application,
        EditContext,
        EditError,
        EditInfo,
        Resolve,
        TargetShape,
        ViewportContext,
        Wrappable,
    },
    buffer::{CursorGroupId, EditBuffer, Editable},
    cursor::Cursor,
    store::{BufferId, Store},
};

use crate::input::bindings::{ModalMachine, Step};

/// Error type for [ReadLine] editor.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum ReadLineError {
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Editing error: {0}")]
    EditingFailure(#[from] EditError),
}

/// Result type when using [ReadLine::readline].
pub type ReadLineResult = Result<String, ReadLineError>;

/// Simple editor for collecting user input.
pub struct ReadLine<P: Application, S: Step<KeyEvent>>
where
    S::C: EditContext,
{
    stdout: BufWriter<Stdout>,

    prompt: Option<String>,

    bindings: ModalMachine<KeyEvent, S>,
    buffer: EditBuffer<S::C, P>,

    gid: CursorGroupId,
    viewctx: ViewportContext<Cursor>,
    top: u16,
}

impl<P: Application, S: Step<KeyEvent, A = Action<P>>> ReadLine<P, S>
where
    S::C: EditContext,
{
    /// Create a new instance.
    pub fn new(bindings: ModalMachine<KeyEvent, S>) -> crossterm::Result<Self> {
        let stdout = BufWriter::new(stdout());

        let id = BufferId(0);
        let store = Store::new();
        let mut buffer = EditBuffer::new(id, store);

        let gid = buffer.create_group();
        let mut viewctx = ViewportContext::default();
        viewctx.set_wrap(true);

        let dims = crossterm::terminal::size()?;
        viewctx.dimensions = (dims.0.into(), dims.1.into());

        let rl = ReadLine {
            stdout,

            prompt: None,

            bindings,
            buffer,

            gid,
            viewctx,
            top: 0,
        };

        return Ok(rl);
    }

    /// Prompt the user for input.
    pub fn readline(&mut self, prompt: Option<String>) -> ReadLineResult {
        self.prompt = prompt;

        crossterm::terminal::enable_raw_mode()?;

        self.init()?;

        loop {
            self.step()?;

            while let Some((action, ctx)) = self.bindings.pop() {
                match self.act(action, ctx) {
                    Ok(None) => continue,
                    Ok(Some(res)) => {
                        self.linebreak()?;

                        crossterm::terminal::disable_raw_mode()?;

                        return Ok(res);
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

    fn suspend(&mut self) -> crossterm::Result<Option<EditInfo>> {
        // Restore old terminal state.
        crossterm::terminal::disable_raw_mode()?;
        self.stdout.queue(CursorShow)?.flush()?;

        // Send SIGTSTP to process.
        let pid = process::id();

        #[cfg(unix)]
        unsafe {
            libc::kill(pid as i32, libc::SIGTSTP);
        }

        // Restore application terminal state.
        crossterm::terminal::enable_raw_mode()?;
        self.init()?;

        Ok(None)
    }

    fn _highlight_ranges(
        &self,
        line: usize,
        start: usize,
        end: usize,
    ) -> Vec<RangeInclusive<usize>> {
        let hinfo = self.buffer._selection_intervals(self.gid);
        let mut ranges = vec![];

        for selection in hinfo.query_point(line) {
            let (sb, se, shape) = &selection.value;

            let maxcol = end.saturating_sub(1);
            let range = start..end;

            match shape {
                TargetShape::LineWise => {
                    ranges.push(start..=maxcol);
                    break;
                },
                TargetShape::CharWise => {
                    let x1 = if line == sb.y { sb.x.max(start) } else { start };
                    let x2 = if line == se.y {
                        se.x.min(maxcol)
                    } else {
                        maxcol
                    };

                    if range.contains(&x1) && range.contains(&x2) {
                        ranges.push(x1..=x2);
                    }
                },
                TargetShape::BlockWise => {
                    let lx = sb.x.min(se.x);
                    let rx = sb.x.max(se.x);

                    let x1 = lx.max(start);
                    let x2 = rx.min(maxcol);

                    if range.contains(&x1) && range.contains(&x2) {
                        ranges.push(x1..=x2);
                    }
                },
            }
        }

        ranges.sort_by(|a, b| {
            let res = a.start().cmp(b.start());

            if res != Ordering::Equal {
                return res;
            }

            return a.end().cmp(b.end());
        });

        return ranges;
    }

    fn _redraw_nowrap(&mut self) -> crossterm::Result<()> {
        Ok(())
    }

    fn _redraw_wrap(&mut self) -> crossterm::Result<()> {
        let width = self.viewctx.dimensions.0;
        let height = self.viewctx.dimensions.1;

        let cursor = self.buffer.get_leader(self.gid);

        let cby = self.viewctx.corner.y;
        let cbx = self.viewctx.corner.x;

        let mut line = cby;
        let mut lines = self.buffer.lines_at(line, cbx);

        let mut wrapped = Vec::new();
        let mut sawcursor = false;

        while let Some(s) = lines.next() {
            if wrapped.len() >= height && sawcursor {
                break;
            }

            let mut off = 0;
            let slen = s.len();

            while off < slen && (wrapped.len() < height || !sawcursor) {
                let start = off;
                let end = (start + width).min(slen);
                let swrapped = s[start..end].to_string();

                let cursor_line = line == cursor.y && (start..=end).contains(&cursor.x);

                wrapped.push((line, start, end, swrapped, cursor_line));

                if cursor_line {
                    sawcursor = true;
                }

                off = end;
            }

            if slen == 0 {
                wrapped.push((line, 0, 0, s.to_string(), line == cursor.y));
            }

            line += 1;
        }

        if wrapped.len() > height {
            let n = wrapped.len() - height;
            let _ = wrapped.drain(..n);
            let (line, start, _, _, _) = wrapped.first().unwrap();
            self.viewctx.corner.set_y(*line);
            self.viewctx.corner.set_x(*start);
        }

        let avail = height - self.top as usize;

        if avail < wrapped.len() {
            let amt = wrapped.len().saturating_sub(avail) as u16;

            self.stdout.queue(ScrollUp(amt))?;
            self.top = self.top.saturating_sub(amt);
        }

        let bot = self.viewctx.dimensions.1 as u16;
        let mut x = 0;
        let mut y = self.top;
        let mut term_cursor = (0, 0);

        self.stdout.queue(MoveTo(0, y))?;

        if let Some(ref prompt) = self.prompt {
            self.stdout.queue(Print(prompt))?;
            x = prompt.len() as u16;
        }

        for (line, start, end, s, cursor_line) in wrapped.into_iter() {
            if y >= bot {
                break;
            }

            if cursor_line {
                let coff = (cursor.x - start) as u16;
                term_cursor = (x + coff, y);
            }

            let ranges = self._highlight_ranges(line, start, end);

            let mut prev = 0;

            self.stdout.queue(MoveTo(x, y))?;

            for range in ranges {
                let rs = prev.max(*range.start());
                let re = *range.end();

                self.stdout.queue(Print(&s[prev..rs]))?;

                let neg = s[(rs - start)..=(re - start)].negative();

                prev = re.saturating_add(1);

                self.stdout.queue(PrintStyledContent(neg))?;

                // XXX: need to highlight followers, too.
                // let finfo = self.buffer._follower_intervals(self.gid);
            }

            self.stdout.queue(Print(&s[prev..]))?;

            y += 1;
        }

        self.stdout.queue(MoveTo(term_cursor.0 as u16, term_cursor.1 as u16))?;

        Ok(())
    }

    fn redraw(&mut self) -> crossterm::Result<()> {
        self.stdout.queue(CursorHide)?;

        self.stdout
            .queue(MoveTo(0, self.top))?
            .queue(Clear(ClearType::FromCursorDown))?;

        if self.viewctx.wrap {
            self._redraw_wrap()?;
        } else {
            self._redraw_nowrap()?;
        }

        self.stdout.queue(CursorShow)?;
        self.stdout.flush()?;

        Ok(())
    }

    fn linebreak(&mut self) -> crossterm::Result<()> {
        self.stdout.queue(Print("\r\n"))?;
        self.stdout.flush()?;

        Ok(())
    }

    fn init(&mut self) -> crossterm::Result<()> {
        let (col, mut row) = crossterm::cursor::position()?;

        if col > 0 {
            // Move to next line if the cursor is after text.
            row += 1;
        }

        if row as usize >= self.viewctx.dimensions.1 {
            self.linebreak()?;

            row = row.saturating_sub(1);
        }

        self.top = row.into();

        Ok(())
    }

    fn step(&mut self) -> crossterm::Result<()> {
        loop {
            self.redraw()?;

            if !poll(Duration::from_millis(500))? {
                continue;
            }

            match read()? {
                Event::Key(ke) => self.bindings.input_key(ke),
                Event::Mouse(_) => {
                    // Do nothing for now.
                },
                Event::Resize(width, height) => {
                    let oldh = self.viewctx.dimensions.1 as u16;
                    let oldt = self.top;

                    // Update terminal dimensions; we'll redraw when we loop.
                    self.viewctx.dimensions = (width.into(), height.into());

                    if oldt >= height - 1 {
                        self.top = height - 2;
                    } else if oldh < height {
                        self.top = oldt + height - oldh;
                    }
                },
            }

            return Ok(());
        }
    }

    fn act(&mut self, action: Action<P>, ctx: S::C) -> Result<Option<String>, ReadLineError> {
        let ctx = (self.gid, &self.viewctx, &ctx);

        let _ = match action {
            // Do nothing.
            Action::NoOp => None,

            // Application-specific action.
            Action::Application(_) => {
                // XXX: implement a way to let the consumer handle this.
                None
            },

            // Simple delegations.
            Action::InsertText(act) => self.buffer.insert_text(act, &ctx)?,
            Action::Mark(mark) => self.buffer.mark(ctx.2.resolve(&mark), &ctx)?,
            Action::Cursor(act) => self.buffer.cursor_command(act, &ctx)?,
            Action::SelectionCursorSet(change) => self.buffer.selcursor_set(&change, &ctx)?,
            Action::SelectionSplitLines(filter) => {
                self.buffer.selection_split_lines(filter, &ctx)?
            },
            Action::History(act) => self.buffer.history_command(act, &ctx)?,
            Action::Suspend => self.suspend()?,

            Action::Edit(action, mov) => {
                let action = ctx.2.resolve(&action);

                self.buffer.edit(&action, &mov, &ctx)?
            },
            Action::Submit => {
                let text = self.buffer.reset_text();

                // XXX: push text into history

                return Ok(Some(text));
            },
            Action::RedrawScreen => {
                self.top = 0;

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
            Action::EditRepeat(_) => {
                // XXX: implement
                None
            },
            Action::MacroRecordToggle => {
                // XXX: implement
                None
            },
            Action::MacroExecute(_) => {
                // XXX: implement
                None
            },
            Action::MacroRepeat(_) => {
                // XXX: implement
                None
            },
            Action::CommandRepeat(_) => {
                // XXX: implement
                None
            },
            Action::KeywordLookup => {
                // XXX: implement
                None
            },

            // Irrelevant to readline behaviour.
            Action::CommandRun(_) => None,
            Action::CommandFocus(_) => None,
            Action::CommandUnfocus => None,
            Action::Tab(_) => None,
            Action::Window(_) => None,
            Action::Scroll(_) => None,
        };

        return Ok(None);
    }
}
