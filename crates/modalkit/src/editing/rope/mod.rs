//! # High-level rope manipulation
//!
//! ## Overview
//!
//! This module provides a wrapper around a rope implementation that works with the types from
//! [prelude], and provides a number of convenience functions and trait implementations to support
//! [EditBuffer](crate::editing::buffer::EditBuffer).
//!
//! [prelude]: crate::prelude
use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::{self, Debug, Display};
use std::io::Write;
use std::ops::{Add, AddAssign, Bound, Range, RangeBounds};

use regex::{Match, Regex};
use ropey::{Rope, RopeSlice};

use crate::actions::EditAction;
use crate::editing::{
    context::Resolve,
    cursor::{Adjustable, Cursor, CursorAdjustment, CursorChoice, CursorState},
};
use crate::prelude::*;

mod diff;

type CowStr<'a> = Cow<'a, str>;

/// Character offset into an [EditRope].
#[derive(
    Clone,
    Copy,
    Debug,
    Eq,
    PartialEq,
    derive_more::Add,
    derive_more::Sub,
    derive_more::From,
    derive_more::Into,
)]
pub struct CharOff(usize);

impl PartialOrd for CharOff {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CharOff {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

pub(super) type CursorContext<'a> = (&'a EditRope, usize, bool);

pub(super) trait PrivateCursorOps {
    fn set_column(&mut self, x: usize, ctx: &CursorContext<'_>);
    fn set_line(&mut self, y: usize, ctx: &CursorContext<'_>);
    fn set(&mut self, y: usize, x: usize, ctx: &CursorContext<'_>);
    fn clamp(&mut self, ctx: &CursorContext<'_>);

    fn line(&mut self, dir: MoveDir1D, count: usize, ctx: &CursorContext<'_>);
    fn column(&mut self, dir: MoveDir1D, wrap: bool, count: usize, ctx: &CursorContext<'_>);
    fn textpos(&mut self, pos: MovePosition, start: usize, width: usize, ctx: &CursorContext<'_>);

    fn screen_line(&mut self, dir: MoveDir1D, count: usize, ctx: &CursorContext<'_>);
    fn screen_linepos(&mut self, pos: MovePosition, ctx: &CursorContext<'_>);

    fn bufpos(&mut self, pos: MovePosition, ctx: &CursorContext<'_>);
    fn skip_space(&mut self, ctx: &CursorContext<'_>);
    fn skip_space_rev(&mut self, ctx: &CursorContext<'_>);
    fn first_word(&mut self, ctx: &CursorContext<'_>);
}

impl PrivateCursorOps for Cursor {
    fn clamp(&mut self, ctx: &CursorContext<'_>) {
        self.set(self.y, self.x, ctx);
    }

    fn set(&mut self, y: usize, x: usize, ctx: &CursorContext<'_>) {
        let ymax = ctx.0.max_line_idx();

        self.y = y.min(ymax);

        let xmax = ctx.0.max_column_idx(self.y, ctx.2);

        self.x = x.min(xmax);
        self.xgoal = self.x;
    }

    fn set_column(&mut self, x: usize, ctx: &CursorContext<'_>) {
        self.x = x.min(ctx.0.max_column_idx(self.y, ctx.2));
        self.xgoal = self.x;
    }

    fn set_line(&mut self, y: usize, ctx: &CursorContext<'_>) {
        let nlines = ctx.0.max_line_idx();

        self.y = y.min(nlines);
        self.x = self.xgoal.min(ctx.0.max_column_idx(self.y, ctx.2));
    }

    fn bufpos(&mut self, pos: MovePosition, ctx: &CursorContext<'_>) {
        match pos {
            MovePosition::Beginning => {
                self.set_line(0, ctx);
            },
            MovePosition::Middle => {
                let nlines = ctx.0.max_line_idx();
                self.set_line(nlines / 2, ctx);
            },
            MovePosition::End => {
                let nlines = ctx.0.max_line_idx();
                self.set_line(nlines, ctx);
            },
        }
    }

    fn screen_line(&mut self, dir: MoveDir1D, mut count: usize, ctx: &CursorContext<'_>) {
        let width = ctx.1;

        match dir {
            MoveDir1D::Previous => {
                while count > 0 {
                    if (self.x / width) > 0 {
                        self.set_column(self.x - width, ctx);
                    } else if self.y > 0 {
                        let above = self.y.saturating_sub(1);
                        let lines = ctx.0.max_column_idx(above, ctx.2) / width;
                        let column = lines * width + self.x;
                        self.set(above, column, ctx);
                    }

                    count -= 1;
                }
            },
            MoveDir1D::Next => {
                while count > 0 {
                    let lines = ctx.0.max_column_idx(self.y, ctx.2) / width;

                    if (self.x / width) < lines {
                        self.set_column(self.x + width, ctx);
                    } else {
                        let below = self.y.saturating_add(1);

                        if below <= ctx.0.max_line_idx() {
                            self.set(below, self.x % width, ctx);
                        }
                    }

                    count -= 1;
                }
            },
        }
    }

    fn screen_linepos(&mut self, pos: MovePosition, ctx: &CursorContext<'_>) {
        let width = ctx.1;

        if width == 0 {
            return;
        }

        let start = self.x - (self.x % width);
        self.textpos(pos, start, width, ctx);
    }

    fn textpos(&mut self, pos: MovePosition, start: usize, width: usize, ctx: &CursorContext<'_>) {
        match pos {
            MovePosition::Beginning => {
                self.set_column(start, ctx);
            },
            MovePosition::Middle => {
                self.set_column(start + width / 2, ctx);
            },
            MovePosition::End => {
                self.set_column(start + width, ctx);
            },
        }
    }

    fn skip_space(&mut self, ctx: &CursorContext<'_>) {
        let off = ctx.0.cursor_to_offset(self);
        let cols = ctx.0.get_columns(self.y);
        let mut iter = ctx.0.chars(off);
        let mut x = self.x;

        while x + 1 < cols {
            match iter.next() {
                None => break,
                Some(c) => {
                    if !c.is_ascii_whitespace() {
                        break;
                    }

                    x += 1;
                },
            }
        }

        self.set_column(x, ctx);
    }

    fn skip_space_rev(&mut self, ctx: &CursorContext<'_>) {
        let off = ctx.0.cursor_to_offset(self);
        let mut rc = ctx.0.offset_to_rc(off);
        let mut x = self.x;

        while x > 0 {
            match rc.peek() {
                None => break,
                Some(c) => {
                    if !c.is_ascii_whitespace() {
                        break;
                    }

                    x -= 1;
                },
            }

            rc.prev();
        }

        self.set_column(x, ctx);
    }

    fn first_word(&mut self, ctx: &CursorContext<'_>) {
        self.set_column(0, ctx);
        self.skip_space(ctx);
    }

    fn line(&mut self, dir: MoveDir1D, count: usize, ctx: &CursorContext<'_>) {
        match dir {
            MoveDir1D::Previous => {
                self.set_line(self.y.saturating_sub(count), ctx);
            },
            MoveDir1D::Next => {
                self.set_line(self.y.saturating_add(count), ctx);
            },
        }
    }

    fn column(&mut self, dir: MoveDir1D, wrap: bool, mut count: usize, ctx: &CursorContext<'_>) {
        match (wrap, dir) {
            (false, MoveDir1D::Previous) => {
                let line = self.x.saturating_sub(count);

                self.set_column(line, ctx);
            },
            (false, MoveDir1D::Next) => {
                let line = self.x.saturating_add(count);

                self.set_column(line, ctx);
            },
            (true, MoveDir1D::Previous) => {
                while count > 0 {
                    if self.x > 0 {
                        self.set_x(self.x - 1);
                    } else if self.y > 0 {
                        let above = self.y.saturating_sub(1);
                        let column = ctx.0.max_column_idx(above, ctx.2);
                        self.set(above, column, ctx);
                    }

                    count -= 1;
                }
            },
            (true, MoveDir1D::Next) => {
                while count > 0 {
                    let max = ctx.0.max_column_idx(self.y, ctx.2);

                    if self.x < max {
                        self.set_x(self.x + 1);
                    } else {
                        let below = self.y.saturating_add(1);

                        if below <= ctx.0.max_line_idx() {
                            self.set(below, 0, ctx);
                        }
                    }

                    count -= 1;
                }
            },
        }
    }
}

// stolen from regex
fn next_utf8(text: &[u8], i: usize) -> usize {
    let b = match text.get(i) {
        None => return i + 1,
        Some(&b) => b,
    };
    let inc = if b <= 0x7F {
        1
    } else if b <= 0b1101_1111 {
        2
    } else if b <= 0b1110_1111 {
        3
    } else {
        4
    };
    i + inc
}

/// Iterator over a rope's characters.
pub struct CharacterIterator<'a> {
    rc_pos: RopeCursor<'a>,
    rc_end: RopeCursor<'a>,

    pos: Option<CharOff>,
    end: Option<CharOff>,

    first: usize,
    last: usize,
}

/// Iterator over the lines in an [EditRope].
///
/// The trailing newline for each line is not included.
pub struct LineIterator<'a> {
    iter: ropey::iter::Lines<'a>,
}

/// Iterator over a rope's newlines.
pub struct NewlineIterator<'a> {
    rc: RopeCursor<'a>,
}

impl<'a> CharacterIterator<'a> {
    fn new(rope: &'a EditRope, pos: usize, end: usize) -> Self {
        let rc_pos = RopeCursor::new(rope, pos);
        let rc_end = RopeCursor::new(rope, end);

        CharacterIterator {
            rc_pos,
            rc_end,

            first: pos,
            last: end,

            pos: None,
            end: None,
        }
    }

    #[inline]
    fn done(&self) -> bool {
        match (self.pos, self.end) {
            (Some(pos), Some(end)) => pos >= end,
            (Some(pos), None) => pos.0 > self.last,
            (None, Some(end)) => self.first > end.0,
            (None, None) => false,
        }
    }

    /// Byte offset into the underlying rope of the last character returned from [Iterator::next].
    pub fn pos(&self) -> CharOff {
        self.pos.expect("next() hasn't been called yet")
    }

    /// Byte offset into the underlying rope of the last character returned from
    /// [DoubleEndedIterator::next_back].
    pub fn pos_back(&self) -> CharOff {
        self.end.expect("next_back() hasn't been called yet")
    }
}

impl<'a> Iterator for CharacterIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.rc_pos.peek()?;

        self.pos = self.rc_pos.pos().into();
        self.rc_pos.next();

        if self.done() {
            return None;
        } else {
            return Some(res);
        }
    }
}

impl<'a> DoubleEndedIterator for CharacterIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let res = self.rc_end.peek()?;

        self.end = self.rc_end.pos().into();
        self.rc_end.prev();

        if self.done() {
            return None;
        } else {
            return Some(res);
        }
    }
}

fn trimnl(slice: RopeSlice<'_>) -> RopeSlice<'_> {
    let len = slice.len_chars();
    let max = len.saturating_sub(1);

    if let Some('\n') = slice.get_char(max) {
        return slice.slice(..max);
    } else {
        return slice;
    }
}

impl<'a> Iterator for LineIterator<'a> {
    type Item = EditRope;

    fn next(&mut self) -> Option<Self::Item> {
        Some(EditRope::from_slice(trimnl(self.iter.next()?)))
    }
}

impl<'a> Iterator for NewlineIterator<'a> {
    type Item = CharOff;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(nl) = self.rc.peek() {
            if nl != '\n' {
                self.rc.next();
                continue;
            }

            let pos = self.rc.pos();

            self.rc.next();

            return Some(pos);
        }

        return None;
    }
}

struct BoundaryTestIterator<'a> {
    chars: CharacterIterator<'a>,
    ctx: BoundaryTestContext,

    coff: CharOff,
    aoff: CharOff,
    boff: CharOff,
}

impl<'a> BoundaryTestIterator<'a> {
    fn new(chars: CharacterIterator<'a>, ctx: BoundaryTestContext) -> Self {
        Self {
            chars,
            ctx,

            aoff: CharOff(0),
            boff: CharOff(0),
            coff: CharOff(0),
        }
    }

    fn pos(&self) -> CharOff {
        return self.coff;
    }

    fn init(mut self, skip_first: bool) -> Option<Self> {
        match self.ctx.dir {
            MoveDir1D::Next => {
                if skip_first {
                    self.ctx.before = self.chars.next();
                    self.boff = self.chars.pos();
                }

                if let Some(current) = self.chars.next() {
                    self.ctx.current = current;
                    self.coff = self.chars.pos();

                    self.ctx.after = self.chars.next();
                    self.aoff = self.chars.pos();

                    return Some(self);
                } else {
                    return None;
                }
            },
            MoveDir1D::Previous => {
                if skip_first {
                    self.ctx.after = self.chars.next_back();
                    self.aoff = self.chars.pos_back();
                }

                if let Some(current) = self.chars.next_back() {
                    self.ctx.current = current;
                    self.coff = self.chars.pos_back();

                    self.ctx.before = self.chars.next_back();
                    self.boff = self.chars.pos_back();

                    return Some(self);
                } else {
                    return None;
                }
            },
        }
    }

    fn next_char(&mut self) -> bool {
        match self.ctx.dir {
            MoveDir1D::Next => {
                if let Some(after) = self.ctx.after {
                    // Move ctx.current into ctx.before.
                    self.ctx.before = Some(self.ctx.current);
                    self.boff = self.coff;

                    // Move ctx.after into ctx.current.
                    self.ctx.current = after;
                    self.coff = self.aoff;

                    // Read new value into ctx.after.
                    self.ctx.after = self.chars.next();
                    self.aoff = self.chars.pos();

                    return false;
                } else {
                    return true;
                }
            },
            MoveDir1D::Previous => {
                if let Some(before) = self.ctx.before {
                    // Move ctx.current into ctx.after.
                    self.ctx.after = Some(self.ctx.current);
                    self.aoff = self.coff;

                    // Move ctx.before into ctx.current.
                    self.ctx.current = before;
                    self.coff = self.boff;

                    // Read new value into ctx.before.
                    self.ctx.before = self.chars.next_back();
                    self.boff = self.chars.pos_back();

                    return false;
                } else {
                    return true;
                }
            },
        }
    }
}

struct RopeCursor<'a> {
    rope: &'a EditRope,
    index: Option<usize>,
}

impl<'a> RopeCursor<'a> {
    fn new(rope: &'a EditRope, index: usize) -> Self {
        RopeCursor { rope, index: Some(index) }
    }

    fn pos(&self) -> CharOff {
        CharOff(self.index.expect("invalid position"))
    }

    fn set(&mut self, pos: CharOff) {
        self.index = pos.0.into();
    }

    fn offset(&mut self, dir: MoveDir1D) {
        match dir {
            MoveDir1D::Next => self.next(),
            MoveDir1D::Previous => self.prev(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.index.and_then(|idx| self.rope.rope.get_char(idx))
    }

    fn next(&mut self) {
        if let Some(idx) = self.index {
            let len = self.rope.rope.len_chars();
            let nxt = idx + 1;

            if nxt < len {
                self.index = Some(nxt);
            } else {
                self.index = None;
            }
        }
    }

    fn prev(&mut self) {
        if let Some(idx) = self.index {
            self.index = idx.checked_sub(1);
        }
    }

    fn to_cursor(&self) -> Cursor {
        self.rope.offset_to_cursor(self.index.expect("invalid position").into())
    }
}

fn roperepeat(rope: &Rope, shape: TargetShape, mut times: usize) -> Rope {
    match shape {
        TargetShape::CharWise | TargetShape::LineWise => {
            let mut result = rope.clone();

            while times > 1 {
                result.append(rope.clone());

                times -= 1;
            }

            return result;
        },
        TargetShape::BlockWise => {
            let mut first = true;
            let mut result = Rope::from("");

            for line in rope.lines().map(trimnl) {
                if first {
                    first = false;
                } else {
                    result.append(Rope::from("\n"));
                }

                result.append(Rope::from(line.to_string().repeat(times)));
            }

            return result;
        },
    }
}

fn togglecase_char(c: char) -> String {
    if c.is_lowercase() {
        c.to_uppercase().to_string()
    } else {
        c.to_lowercase().to_string()
    }
}

fn togglecase(s: String) -> String {
    s.chars().map(togglecase_char).collect()
}

fn titlecase(s: String) -> String {
    let mut cap = true;

    s.chars()
        .map(|c| {
            if c.is_ascii_whitespace() {
                cap = true;
                return c.to_string();
            } else if cap {
                cap = false;
                return c.to_uppercase().to_string();
            } else {
                return c.to_lowercase().to_string();
            }
        })
        .collect()
}

fn get_last_column(ctx: &CursorMovementsContext<'_, Cursor>) -> bool {
    let shaped = ctx.context.get_target_shape().is_some();

    if ctx.action.is_motion() {
        return ctx.context.get_last_column() || shaped;
    }

    return !shaped;
}

/// A rope with context-aware movements and high-level operations.
#[derive(Clone, Debug)]
pub struct EditRope {
    rope: Rope,
}

impl EditRope {
    /// Create an empty, zero-length rope.
    pub fn empty() -> EditRope {
        EditRope::from("")
    }

    fn from_slice(slice: RopeSlice) -> EditRope {
        let rope = Rope::from(slice);

        EditRope { rope }
    }

    /// Write the contents of the [EditRope] to a writer.
    pub fn write_to<T: Write>(&self, writer: T) -> Result<(), std::io::Error> {
        self.rope.write_to(writer)
    }

    /// Calculate the max indexable column in a given line given the current context.
    ///
    /// This function expects to be given a valid line number as input.
    pub(crate) fn max_column_idx(&self, y: usize, lastcol: bool) -> usize {
        let columns = self.get_columns(y);

        if lastcol {
            columns
        } else {
            columns.saturating_sub(1)
        }
    }

    /// Calculate the max indexable line for a cursor in this string.
    ///
    /// This function expects every line in the buffer to be terminated with a newline.
    fn max_line_idx(&self) -> usize {
        // We assume that every editing buffer has a trailing newline
        self.get_lines().saturating_sub(1)
    }

    fn _bottom_line_idx(&self, view: &ViewportContext<Cursor>) -> usize {
        let (width, height) = view.dimensions;
        let start = view.corner.y;

        if view.wrap {
            let mut wrapped: usize = 0;
            let mut actual: usize = 0;

            if width == 0 {
                return start;
            }

            for line in self.lines_at(start, view.corner.x) {
                wrapped += 1;
                wrapped += line.len().saturating_sub(1) / width;
                actual += 1;

                if wrapped >= height {
                    break;
                }
            }

            let off = actual.saturating_sub(1);

            return start.saturating_add(off);
        } else {
            let last = self.get_lines();
            let off = height.saturating_sub(1);

            return start.saturating_add(off).min(last);
        }
    }

    fn _middle_line_idx(&self, view: &ViewportContext<Cursor>) -> usize {
        if view.wrap {
            let (width, height) = view.dimensions;
            let mut lines = Vec::new();

            for (l, line) in self.lines_at(view.corner.y, view.corner.x).enumerate() {
                let w = 1 + line.len().saturating_sub(1) / width;

                for _ in 0..w {
                    lines.push(l);
                }

                if lines.len() >= height {
                    break;
                }
            }

            lines.truncate(height);

            let len = lines.len();

            if len == 0 {
                view.corner.y
            } else {
                view.corner.y + lines.get(len / 2).unwrap()
            }
        } else {
            let top = view.corner.y;
            let bot = self._bottom_line_idx(view);
            top + (bot - top) / 2
        }
    }

    /// Split the rope into three different parts.
    pub fn split(
        &self,
        CharOff(start): CharOff,
        CharOff(end): CharOff,
        inclusive: bool,
    ) -> (EditRope, EditRope, EditRope) {
        let size = self.rope.len_chars();
        let beg = start.min(size);
        let end = end.min(size);

        if inclusive {
            let prefix = EditRope::from_slice(self.rope.slice(..beg));
            let middle = EditRope::from_slice(self.rope.slice(beg..=end));
            let suffix = EditRope::from_slice(self.rope.slice(end.saturating_add(1)..));

            return (prefix, middle, suffix);
        } else {
            let prefix = EditRope::from_slice(self.rope.slice(..beg));
            let middle = EditRope::from_slice(self.rope.slice(beg..end));
            let suffix = EditRope::from_slice(self.rope.slice(end..));

            return (prefix, middle, suffix);
        }
    }

    fn _trim_start_idx(&self) -> Option<CharOff> {
        for (i, c) in self.rope.chars().enumerate() {
            if c.is_ascii_whitespace() {
                continue;
            }

            return Some(CharOff(i));
        }

        return None;
    }

    fn _trim_end_idx(&self) -> Option<CharOff> {
        let len = self.rope.len_chars();
        let max = len.saturating_sub(1);

        for (i, c) in self.rope.chars_at(len).reversed().enumerate() {
            if c.is_ascii_whitespace() {
                continue;
            }

            return Some(CharOff(max - i));
        }

        return None;
    }

    /// Remove leading whitespace from the rope.
    pub fn trim_start(&self) -> EditRope {
        if let Some(CharOff(start)) = self._trim_start_idx() {
            EditRope::from_slice(self.rope.slice(start..))
        } else {
            EditRope::empty()
        }
    }

    /// Remove trailing whitespace from the rope.
    pub fn trim_end(&self) -> EditRope {
        if let Some(CharOff(end)) = self._trim_end_idx() {
            EditRope::from_slice(self.rope.slice(..=end))
        } else {
            EditRope::empty()
        }
    }

    /// Remove matching leading characters from the rope.
    pub fn trim_start_matches<F>(&self, matches: F) -> EditRope
    where
        F: Fn(char) -> bool,
    {
        for (i, c) in self.rope.chars().enumerate() {
            if matches(c) {
                continue;
            }

            return EditRope::from_slice(self.rope.slice(i..));
        }

        return EditRope::empty();
    }

    /// Remove matching trailing characters from the rope.
    pub fn trim_end_matches<F>(&self, matches: F) -> EditRope
    where
        F: Fn(char) -> bool,
    {
        let len = self.rope.len_chars();
        let max = len.saturating_sub(1);

        for (i, c) in self.rope.chars_at(len).reversed().enumerate() {
            if matches(c) {
                continue;
            }

            return EditRope::from_slice(self.rope.slice(..=max - i));
        }

        return EditRope::empty();
    }

    /// Remove whitespace from the start and end of the rope.
    pub fn trim(&self) -> EditRope {
        let start = self._trim_start_idx();
        let end = self._trim_end_idx();

        if let (Some(CharOff(start)), Some(CharOff(end))) = (start, end) {
            EditRope::from_slice(self.rope.slice(start..=end))
        } else {
            EditRope::empty()
        }
    }

    /// Returns a slice of a range within the rope.
    pub fn slice<R: RangeBounds<CharOff>>(&self, range: R) -> EditRope {
        let size = self.rope.len_chars();
        // XXX: once MSRV is 1.77, this can just use Bound::map.
        let beg = match range.start_bound() {
            Bound::Included(c) => Bound::Included(c.0.min(size)),
            Bound::Excluded(c) => Bound::Excluded(c.0.min(size)),
            Bound::Unbounded => Bound::Unbounded,
        };
        let end = match range.end_bound() {
            Bound::Included(c) => Bound::Included(c.0.min(size)),
            Bound::Excluded(c) => Bound::Excluded(c.0.min(size)),
            Bound::Unbounded => Bound::Unbounded,
        };

        let rope = self.rope.slice((beg, end));

        EditRope::from_slice(rope)
    }

    fn to_range<R: RangeBounds<CharOff>>(&self, range: R) -> Range<CharOff> {
        let start = match range.start_bound() {
            Bound::Included(s) => *s,
            Bound::Excluded(s) => CharOff(s.0 + 1),
            Bound::Unbounded => CharOff(0),
        };
        let end = match range.end_bound() {
            Bound::Included(s) => CharOff(s.0 + 1),
            Bound::Excluded(s) => *s,
            Bound::Unbounded => CharOff(self.rope.len_chars() + 1),
        };

        Range { start, end }
    }

    /// Replace a range within the rope with some new text.
    pub fn replace<R: RangeBounds<CharOff>>(
        &mut self,
        range: R,
        substitution: EditRope,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        let range = self.to_range(range);
        let replaced = self.slice(range.clone());

        // Cursor positions for the modified range.
        let start = self.offset_to_cursor(range.start);
        let end = self.offset_to_cursor(range.end);

        // Number of columns on last line in both texts.
        let mut rchars: isize = 0;
        let mut schars: isize = 0;

        for c in replaced.chars(0.into()).rev() {
            if c == '\n' {
                break;
            }

            rchars += 1;
        }

        for c in substitution.chars(0.into()).rev() {
            if c == '\n' {
                break;
            }

            schars += 1;
        }

        let slen = substitution.len();
        let rlines = replaced.get_lines() as isize;
        let slines = substitution.get_lines() as isize;

        let adjs = match (rlines, slines) {
            (0, 0) => {
                // Text replaced on the same line.
                let amt_col = schars - rchars;

                vec![CursorAdjustment::Column {
                    line: start.y,
                    column_start: end.x,
                    amt_line: 0,
                    amt_col,
                }]
            },
            (0, n) => {
                // Lines inserted.
                vec![
                    // First, move all lines after the end line downwards.
                    CursorAdjustment::Line {
                        line_start: end.y,
                        line_end: end.y,
                        amount: 0,
                        amount_after: n,
                    },
                    // Then, move any marked text on the line end downwards, and adjust columns.
                    CursorAdjustment::Column {
                        line: end.y,
                        column_start: end.x,
                        amt_line: n,
                        amt_col: schars - end.x as isize,
                    },
                ]
            },
            (n, 0) => {
                // Lines deleted.
                vec![
                    // First, move any marked text on this line upwards, and adjust columns.
                    CursorAdjustment::Column {
                        line: end.y,
                        column_start: rchars as usize,
                        amt_line: -1,
                        amt_col: start.x as isize + schars - rchars,
                    },
                    // Then, move any following lines upwards.
                    CursorAdjustment::Line {
                        line_start: end.y,
                        line_end: end.y,
                        amount: 0,
                        amount_after: -n,
                    },
                ]
            },
            (n, m) if n < m => {
                // Lines replaced with more lines.
                let ldiff = m - n;

                vec![
                    CursorAdjustment::Line {
                        line_start: start.y,
                        line_end: end.y,
                        amount: 0,
                        amount_after: ldiff,
                    },
                    CursorAdjustment::Column {
                        line: end.y,
                        column_start: end.x,
                        amt_line: ldiff,
                        amt_col: schars - rchars,
                    },
                ]
            },
            (n, m) => {
                // Lines replaced with fewer lines.
                let ldiff = m - n;

                vec![
                    CursorAdjustment::Column {
                        line: end.y,
                        column_start: end.x,
                        amt_line: ldiff,
                        amt_col: schars - rchars,
                    },
                    CursorAdjustment::Line {
                        line_start: start.y,
                        line_end: end.y,
                        amount: 0,
                        amount_after: ldiff,
                    },
                ]
            },
        };

        let s = Cow::from(substitution.rope);
        let soff = range.start.0;
        let eoff = range.end.0;
        self.rope.remove(soff..eoff);
        self.rope.insert(soff, s.as_ref());

        let end = self.offset_to_cursor(slen.saturating_sub(1).add(soff).into());
        let start = self.offset_to_cursor(soff.into());
        let default = start.clone();

        (CursorChoice::Range(start, end, default), adjs)
    }

    /// Replace all of the newlines between `start` and `end` according to
    /// [JoinStyle].
    pub fn join_lines(
        &mut self,
        start: CharOff,
        end: CharOff,
        inclusive: bool,
        spaces: JoinStyle,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        let mut cursor = None;
        let mut adjs = vec![];
        let mut nls = vec![];

        for nl in self.newlines(start) {
            if nl > end || (!inclusive && nl == end) {
                break;
            }

            nls.push(nl);
        }

        if nls.len() > 1 {
            /*
             * Normally we ignore the final newline in the range since it's the lines *inside*
             * the range that are being joined together, but since the minimum number of lines
             * joined is 2, we don't remove the last offset if it's the only offset.
             */
            let _ = nls.pop();
        }

        for nl in nls.into_iter().rev() {
            // Leave the buffer's final newline alone.
            if nl == self.last_offset() {
                continue;
            }

            let (choice, mut adjustments) = match spaces {
                JoinStyle::OneSpace => {
                    let mut iter = self.chars(nl + 1.into());
                    let mut blank = false;

                    for c in iter.by_ref() {
                        if c == '\n' {
                            blank = true;
                            break;
                        } else if c.is_ascii_whitespace() {
                            continue;
                        } else {
                            break;
                        }
                    }

                    let jtxt = if blank { "" } else { " " };

                    let stop = iter.pos();

                    self.replace(nl..stop, jtxt.into())
                },
                JoinStyle::NewSpace => self.replace(nl..=nl, " ".into()),
                JoinStyle::NoChange => self.replace(nl..=nl, "".into()),
            };

            cursor
                .get_or_insert_with(|| choice.get(CursorEnd::Auto).cloned().unwrap_or_default())
                .adjust(&adjustments);
            adjs.append(&mut adjustments);
        }

        let choice = cursor.map(CursorChoice::Single).unwrap_or_default();

        (choice, adjs)
    }

    /// Return the rope repeated *n* times.
    pub fn repeat(&self, shape: TargetShape, times: usize) -> EditRope {
        EditRope { rope: roperepeat(&self.rope, shape, times) }
    }

    /// Transform a range within the rope with function *f*.
    pub fn transform(
        &mut self,
        start: CharOff,
        end: CharOff,
        inclusive: bool,
        f: impl Fn(EditRope) -> EditRope,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        if inclusive {
            let range = start..=end;
            let transformed = f(self.slice(range.clone()));
            self.replace(range, transformed)
        } else {
            let range = start..end;
            let transformed = f(self.slice(range.clone()));
            self.replace(range, transformed)
        }
    }

    /// Change the case of this rope.
    pub fn changecase(&self, case: &Case) -> EditRope {
        let s = self.rope.to_string();
        let s = match case {
            Case::Upper => s.to_uppercase(),
            Case::Lower => s.to_lowercase(),
            Case::Title => titlecase(s),
            Case::Toggle => togglecase(s),
        };

        EditRope::from(s)
    }

    /// Do a linewise insertion of some text above the cursor's current line. The text should
    /// already contain trailing newline.
    fn _insert_above(
        &mut self,
        cursor: &Cursor,
        text: EditRope,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        let off = self.offset_of_line(cursor.y);
        let tlines = text.get_lines() as isize;
        let tlen = text.len();

        self._insert_at(off.0, text.rope);

        let adj = CursorAdjustment::Line {
            line_start: cursor.y,
            line_end: usize::MAX,
            amount: tlines,
            amount_after: 0,
        };

        let start = self.offset_to_cursor(off);
        let end = self.offset_to_cursor(CharOff(off.0 + tlen));

        let mut default = start.clone();
        let cctx = &(&*self, 0, true);
        default.first_word(cctx);

        (CursorChoice::Range(start, end, default), vec![adj])
    }

    /// Do a linewise insertion of some text below the cursor's current line. The text should
    /// already contain trailing newline.
    fn _insert_below(
        &mut self,
        cursor: &Cursor,
        text: EditRope,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        let coff = self.cursor_to_offset(cursor);
        let tlines = text.get_lines() as isize;
        let tlen = text.len_offset();

        let soff = match self.line_after(cursor.y) {
            Some(end) => {
                self._insert_at(end.0, text.rope);
                end
            },
            None => {
                let end = self.rope.len_chars();
                self.rope.insert_char(end, '\n');
                self.rope.append(text.rope);
                end.into()
            },
        };

        let lstart = self.line_of_offset(coff) + 1;
        let adj = CursorAdjustment::Line {
            line_start: lstart,
            line_end: usize::MAX,
            amount: tlines,
            amount_after: 0,
        };

        let start = self.offset_to_cursor(soff);
        let end = self.offset_to_cursor(soff + tlen);

        let mut default = start.clone();
        let cctx = &(&*self, 0usize, true);
        default.first_word(cctx);

        return (CursorChoice::Range(start, end, default), vec![adj]);
    }

    /// Do a blockwise insertion of some text. Lines within the block should be separated by a
    /// newline, and there should be no trailing newline.
    fn _insert_block(
        &mut self,
        cursor: &Cursor,
        off: usize,
        text: EditRope,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        let colmax = self.max_column_idx(cursor.y, true);
        let cstart = cursor.x.saturating_add(off).min(colmax);
        let coff = self.lincol_to_offset(cursor.y, cstart);

        let mut eoff = coff;
        let mut adjs = vec![];
        let mut c = cursor.clone();

        let sline = self.offset_of_line(c.y).0;
        let ilines = text.get_lines().saturating_add(1);
        let alines = self.rope.slice(sline..).len_lines();

        let loff = text.offset_of_line(ilines.min(alines));

        for line in text.rope.slice(0..loff.0).lines().map(trimnl) {
            let colmax = self.max_column_idx(c.y, true);
            let cstart = c.x.saturating_add(off).min(colmax);
            let ioff = self.lincol_to_offset(c.y, cstart).0;
            let tlen = line.len_chars();

            adjs.push(CursorAdjustment::Column {
                line: c.y,
                column_start: cstart,
                amt_line: 0,
                amt_col: tlen as isize,
            });

            self._insert_at(ioff, Rope::from(line));

            eoff = CharOff(ioff + tlen);

            c.down(1);
        }

        self.trailing_newline();

        if ilines > alines {
            let start = text.offset_of_line(alines).0;
            let append = text.rope.slice(start..);
            self.rope.append(append.into());
            self.rope.append(Rope::from("\n"));
            eoff = self.len_offset();
        }

        let start = self.offset_to_cursor(coff);
        let end = self.offset_to_cursor(eoff);
        let default = start.clone();

        return (CursorChoice::Range(start, end, default), adjs);
    }

    fn _insert_at(&mut self, off: usize, rope: Rope) {
        let tail = self.rope.split_off(off);
        self.rope.append(rope);
        self.rope.append(tail);
    }

    fn _insert(
        &mut self,
        cursor: &Cursor,
        off: usize,
        co: usize,
        text: EditRope,
        style: InsertStyle,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        let colmax = self.max_column_idx(cursor.y, true);
        let cstart = cursor.x.saturating_add(off).min(colmax);

        let coff = self.cursor_to_offset(cursor);
        let ioff = self.lincol_to_offset(cursor.y, cstart);

        let tlen = text.len();
        let tlines = text.get_lines() as isize;

        match style {
            InsertStyle::Replace => {
                self.rope.remove(ioff.0..ioff.0 + tlen);
                self._insert_at(ioff.0, text.rope);
            },
            InsertStyle::Insert => {
                self._insert_at(ioff.0, text.rope);
            },
        }

        let insend = coff + tlen.into();

        let mut adjs = vec![];
        let lstart = self.line_of_offset(ioff);

        if tlines == 0 {
            adjs.push(CursorAdjustment::Column {
                line: lstart,
                column_start: cstart,
                amt_line: 0,
                amt_col: tlen as isize,
            });
        } else {
            let cinsend = self.offset_to_cursor(insend);
            let cdiff = cinsend.x as isize - cstart as isize;

            adjs.push(CursorAdjustment::Line {
                line_start: lstart.saturating_add(1),
                line_end: usize::MAX,
                amount: tlines,
                amount_after: 0,
            });

            adjs.push(CursorAdjustment::Column {
                line: lstart,
                column_start: cstart,
                amt_line: tlines,
                amt_col: cdiff,
            });
        }

        let start = self.offset_to_cursor(ioff);
        let end = self.offset_to_cursor(CharOff(ioff.0 + tlen));

        let noff = insend.0.saturating_sub(1).saturating_add(co).into();
        let default = self.offset_to_cursor(noff);

        return (CursorChoice::Range(start, end, default), adjs);
    }

    /// Insert or replace text before or after a given cursor position.
    pub fn insert(
        &mut self,
        cursor: &Cursor,
        dir: MoveDir1D,
        text: EditRope,
        style: InsertStyle,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        if text.is_empty() {
            return (CursorChoice::Single(cursor.clone()), vec![]);
        }

        match dir {
            MoveDir1D::Previous => {
                return self._insert(cursor, 0, 1, text, style);
            },
            MoveDir1D::Next => {
                return self._insert(cursor, 1, 0, text, style);
            },
        }
    }

    /// Paste text at a given cursor position.
    pub fn paste(
        &mut self,
        cursor: &Cursor,
        dir: MoveDir1D,
        text: EditRope,
        shape: TargetShape,
    ) -> (CursorChoice, Vec<CursorAdjustment>) {
        if text.is_empty() {
            return (CursorChoice::Single(cursor.clone()), vec![]);
        }

        match (shape, dir) {
            (TargetShape::CharWise, MoveDir1D::Previous) => {
                return self._insert(cursor, 0, 0, text, InsertStyle::Insert);
            },
            (TargetShape::CharWise, MoveDir1D::Next) => {
                return self._insert(cursor, 1, 1, text, InsertStyle::Insert);
            },
            (TargetShape::LineWise, MoveDir1D::Previous) => {
                return self._insert_above(cursor, text);
            },
            (TargetShape::LineWise, MoveDir1D::Next) => {
                return self._insert_below(cursor, text);
            },
            (TargetShape::BlockWise, MoveDir1D::Previous) => {
                return self._insert_block(cursor, 0, text);
            },
            (TargetShape::BlockWise, MoveDir1D::Next) => {
                return self._insert_block(cursor, 1, text);
            },
        }
    }

    /// Mutably force this rope to contain a trailing newline if it doesn't already.
    pub fn trailing_newline(&mut self) {
        let len = self.rope.len_chars();
        let end = len.saturating_sub(1);

        if let Some('\n') = self.rope.get_char(end) {
            return;
        }

        self.rope.insert_char(len, '\n');
    }

    /// Get the text for a line.
    pub fn get_line(&self, line: usize) -> Option<EditRope> {
        self.rope.get_line(line).map(EditRope::from_slice)
    }

    /// Returns the character (if it exists) at a given cursor position.
    pub fn get_char_at_cursor(&self, cursor: &Cursor) -> Option<char> {
        let lmax = self.max_line_idx();

        if cursor.y > lmax {
            return None;
        }

        let cmax = self.max_column_idx(cursor.y, false);

        if cursor.x > cmax {
            return None;
        }

        let off = self.cursor_to_offset(cursor);

        self.chars(off).next()
    }

    /// Returns true if this rope contains only whitespace characters.
    pub fn is_blank(&self) -> bool {
        for c in self.chars(0.into()) {
            if !c.is_ascii_whitespace() {
                return false;
            }
        }

        return true;
    }

    /// Returns true if a line contains only whitespace characters.
    pub fn is_blank_line(&self, line: usize) -> bool {
        let offset = self.offset_of_line(line);

        for c in self.chars(offset) {
            if c == '\n' {
                return true;
            }

            if !c.is_ascii_whitespace() {
                return false;
            }
        }

        return true;
    }

    /// Returns true if the inclusive range of bytes contains only whitespace characters.
    pub fn is_blank_range(&self, start: CharOff, end: CharOff) -> bool {
        for c in self.chars_until(start, end) {
            if !c.is_ascii_whitespace() {
                return false;
            }
        }

        return true;
    }

    /// Return the length in number of characters as a [CharOff].
    pub fn len_offset(&self) -> CharOff {
        CharOff(self.len())
    }

    /// Return the length in number of characters.
    pub fn len(&self) -> usize {
        self.rope.len_chars()
    }

    /// Indicates whether or not this rope is empty (contains no characters).
    pub fn is_empty(&self) -> bool {
        self.rope.len_chars() == 0
    }

    /// Return the number of lines in this rope.
    pub fn get_lines(&self) -> usize {
        self.rope.len_lines().saturating_sub(1)
    }

    /// Get the character offset of the start of the following line.
    pub fn line_after(&self, line: usize) -> Option<CharOff> {
        let len = self.rope.len_lines();
        let max = len.saturating_sub(1);

        if line < max {
            self.offset_of_line(line + 1).into()
        } else {
            None
        }
    }

    /// Return the number of columns on the given line.
    pub fn get_columns(&self, line: usize) -> usize {
        let lbeg_off = self.offset_of_line(line);

        if let Some(lend_off) = self.line_after(line) {
            return (lend_off - lbeg_off).0.saturating_sub(1);
        } else {
            return self.len() - lbeg_off.0;
        }
    }

    pub(crate) fn lines(&self, line: usize) -> LineIterator {
        self.lines_at(line, 0)
    }

    pub(crate) fn lines_at(&self, line: usize, column: usize) -> LineIterator {
        let len = self.rope.len_chars();
        let max = len.saturating_sub(1);
        let off = self.lincol_to_offset(line, column).0;

        let slice = if let Some('\n') = self.rope.get_char(max) {
            self.rope.slice(off..max)
        } else {
            self.rope.slice(off..)
        };

        LineIterator { iter: slice.lines() }
    }

    /// Return the line number of the given character offset.
    pub fn line_of_offset(&self, off: CharOff) -> usize {
        self.rope
            .try_char_to_line(off.0)
            .unwrap_or_else(|_| self.rope.len_lines().saturating_sub(1))
    }

    /// Return the character offset of the start of a given line.
    pub fn offset_of_line(&self, line: usize) -> CharOff {
        CharOff(self.rope.line_to_char(line))
    }

    /// Convert a character offset to a [Cursor].
    pub fn offset_to_cursor(&self, off: CharOff) -> Cursor {
        let off = off.min(self.last_offset());

        let line = self.line_of_offset(off);
        let loff = self.offset_of_line(line);

        Cursor::new(line, off.0 - loff.0)
    }

    fn offset_to_rc(&self, off: CharOff) -> RopeCursor<'_> {
        RopeCursor::new(self, off.0)
    }

    fn lincol_to_offset(&self, line: usize, col: usize) -> CharOff {
        self.offset_of_line(line) + CharOff(col)
    }

    /// Convert a cursor to a character offset.
    pub fn cursor_to_offset(&self, cursor: &Cursor) -> CharOff {
        self.lincol_to_offset(cursor.y, cursor.x)
    }

    /// Return a cursor located at the first character in the rope.
    pub fn first(&self) -> Cursor {
        Cursor::new(0, 0)
    }

    /// Return the last character offset in the rope.
    pub fn last_offset(&self) -> CharOff {
        CharOff(self.rope.len_chars().saturating_sub(1))
    }

    /// Return a cursor located at the last character in the rope.
    pub fn last(&self) -> Cursor {
        self.offset_to_cursor(self.last_offset())
    }

    /// Convert an [EditRange] into a [CursorState::Selection].
    pub fn select(&self, range: EditRange<Cursor>) -> CursorState {
        if range.start >= range.end {
            CursorState::Selection(range.start.clone(), range.start, range.shape)
        } else if range.inclusive {
            CursorState::Selection(range.start, range.end, range.shape)
        } else {
            let off = self.cursor_to_offset(&range.end).0.saturating_sub(1);
            let end = self.offset_to_cursor(off.into());
            CursorState::Selection(range.start, end, range.shape)
        }
    }

    /// Compare this rope with a new version, and return a vector of adjustments needed to fix
    /// cursors and marks when moving to the new version.
    pub fn diff(&self, other: &EditRope) -> Vec<CursorAdjustment> {
        use diff::{compute_delta, DeltaElement};

        let delta = compute_delta(&self.rope, &other.rope);
        let mut adjs = Vec::new();
        let mut last = CharOff(0);
        let mut inserted = 0;

        for el in delta.els.into_iter() {
            match el {
                DeltaElement::Copy(start, end) => {
                    /*
                     * DeltaElement::Copy represents what bytes are copied from the base to create
                     * the other rope. Gaps between the offsets are therefore deletions, and we
                     * need to adjust later lines and anything following removed columns.
                     */
                    let start = CharOff(start);
                    let end = CharOff(end);

                    if start == last {
                        last = end;
                        continue;
                    }

                    let clast = self.offset_to_cursor(last);
                    let sline = self.line_of_offset(start);

                    if clast.y == sline {
                        adjs.push(CursorAdjustment::Column {
                            line: clast.y + inserted,
                            column_start: clast.x,
                            amt_line: 0,
                            amt_col: -(start.0 as isize - last.0 as isize),
                        });
                    } else {
                        adjs.push(CursorAdjustment::Column {
                            line: clast.y + inserted,
                            column_start: clast.x,
                            amt_line: 0,
                            amt_col: isize::MIN,
                        });

                        let dlines = sline - clast.y;
                        adjs.push(CursorAdjustment::Line {
                            line_start: clast.y + inserted,
                            line_end: (sline + inserted).saturating_sub(1),
                            amount: isize::MIN,
                            amount_after: -(dlines as isize),
                        });
                    }

                    last = end;
                },
                DeltaElement::Insert(node) => {
                    let nlines = node.len_lines().saturating_sub(1);

                    if nlines == 0 {
                        let clast = self.offset_to_cursor(last);
                        let added = node.len_chars();

                        adjs.push(CursorAdjustment::Column {
                            line: clast.y + inserted,
                            column_start: clast.x,
                            amt_line: 0,
                            amt_col: added as isize,
                        });
                    } else {
                        let lline = self.line_of_offset(last);

                        adjs.push(CursorAdjustment::Line {
                            line_start: lline + inserted,
                            line_end: usize::MAX,
                            amount: nlines as isize,
                            amount_after: 0,
                        });

                        inserted += nlines;
                    }
                },
            }
        }

        let loff = self.last_offset();

        if last <= loff {
            let clast = self.offset_to_cursor(last);
            let lbyte = self.offset_of_line(clast.y);
            let mline = self.line_of_offset(loff);
            let dlines = (mline - clast.y) as isize;

            if last > lbyte {
                // We aren't deleting the whole line.
                adjs.push(CursorAdjustment::Column {
                    line: clast.y + inserted,
                    column_start: clast.x,
                    amt_line: 0,
                    amt_col: isize::MIN,
                });

                adjs.push(CursorAdjustment::Line {
                    line_start: clast.y + inserted,
                    line_end: mline + inserted,
                    amount: -dlines,
                    amount_after: isize::MIN,
                });
            } else {
                adjs.push(CursorAdjustment::Line {
                    line_start: clast.y + inserted,
                    line_end: mline + inserted,
                    amount: -(dlines + 1),
                    amount_after: isize::MIN,
                });
            }
        }

        return adjs;
    }

    fn _match_to_range(&self, m: Match) -> EditRange<Cursor> {
        let start = self.offset_to_cursor(m.start().into());
        let end = self.offset_to_cursor(m.end().saturating_sub(1).into());

        return EditRange::inclusive(start, end, TargetShape::CharWise);
    }

    fn _find_regex_previous(
        &self,
        start: usize,
        needle: &Regex,
        count: usize,
    ) -> Option<EditRange<Cursor>> {
        let text = CowStr::from(&self.rope);
        let ms: Vec<_> = needle.find_iter(&text).collect();
        let modulus = ms.len();

        for (i, m) in ms.iter().enumerate() {
            let off = m.start();

            if off >= start {
                let offset = count % modulus;
                let idx = (modulus + i - offset) % modulus;

                return self._match_to_range(ms[idx]).into();
            }
        }

        if let Some(m) = ms.last() {
            return self._match_to_range(*m).into();
        }

        return None;
    }

    fn _find_regex_next(
        &self,
        start: usize,
        needle: &Regex,
        mut count: usize,
    ) -> Option<EditRange<Cursor>> {
        let text = CowStr::from(&self.rope);

        // Start search right after the cursor position.
        let mut res: Option<Match> = None;
        let mut pos = next_utf8(text.as_bytes(), start);

        macro_rules! advance {
            () => {
                if let Some(m) = res {
                    let e = m.end();
                    pos = e;

                    if m.start() == e {
                        pos = next_utf8(text.as_bytes(), pos);
                    }
                }
            };
        }

        while count > 0 {
            advance!();

            res = needle.find_at(&text, pos);

            if res.is_none() {
                break;
            }

            count -= 1;
        }

        // Continue search from beginning of text.
        pos = 0;

        while count > 0 {
            advance!();

            res = needle.find_at(&text, pos);

            if res.is_none() {
                if pos == 0 {
                    break;
                } else {
                    pos = 0;
                }
            } else {
                count -= 1;
            }
        }

        if count == 0 {
            return res.map(|m| self._match_to_range(m));
        } else {
            return None;
        }
    }

    fn boundary(
        &self,
        nc: &Cursor,
        dir: MoveDir1D,
        count: usize,
        motion: bool,
    ) -> Option<BoundaryTestIterator> {
        let off = self.cursor_to_offset(nc);

        let boff = off.0.saturating_sub(1).into();
        let aoff = off.0.saturating_add(1).into();

        let (chars, skip_first) = match dir {
            MoveDir1D::Next => {
                if off.0 > 0 {
                    (self.chars(boff), true)
                } else {
                    (self.chars(off), false)
                }
            },
            MoveDir1D::Previous => {
                let last = self.last_offset();

                if aoff > last {
                    (self.chars_until(CharOff(0), last), false)
                } else {
                    (self.chars_until(CharOff(0), aoff), true)
                }
            },
        };

        let ctx = BoundaryTestContext {
            current: ' ',
            before: None,
            after: None,

            dir,
            motion,
            count,
        };

        BoundaryTestIterator::new(chars, ctx).init(skip_first)
    }

    fn find_boundary<O: BoundaryTest>(
        &self,
        nc: &Cursor,
        obj: &O,
        terminus: MoveTerminus,
        dir: MoveDir1D,
        count: usize,
        motion: bool,
        lastcol: bool,
    ) -> Option<Cursor> {
        let mut bti = self.boundary(nc, dir, count, motion)?;
        let first = obj.is_boundary(terminus, &bti.ctx);

        if bti.next_char() {
            if first {
                // If we can't move, and we're at the boundary, count it.
                return Some(nc.clone());
            } else {
                return None;
            }
        }

        let off = self.seek_next(obj, terminus, bti)?;
        let mut cursor = self.offset_to_cursor(off);

        /*
         * Word movements always move, even if they can't do a full count.
         */
        if motion && !lastcol {
            let x = self.max_column_idx(cursor.y, lastcol).min(cursor.x);

            cursor.set_x(x);
        }

        Some(cursor)
    }

    fn find_quote_start(&self, rc: &mut RopeCursor<'_>, quote: char) -> Option<()> {
        loop {
            rc.prev();

            match rc.peek()? {
                c if c == quote => {
                    // Make sure the quote isn't preceded by an escape.
                    let off = rc.pos();

                    rc.prev();

                    match rc.peek() {
                        Some('\\') | None => {
                            continue;
                        },
                        Some(_) => {
                            // Reset cursor position.
                            rc.set(off);

                            return Some(());
                        },
                    }
                },
                _ => {
                    continue;
                },
            }
        }
    }

    fn find_quote_end(&self, rc: &mut RopeCursor<'_>, quote: char) -> Option<()> {
        loop {
            rc.next();

            match rc.peek()? {
                c if c == quote => {
                    return Some(());
                },
                '\\' => {
                    // Skip next character.
                    rc.next();
                },
                _ => {
                    continue;
                },
            }
        }
    }

    fn find_match(
        &self,
        rc: RopeCursor<'_>,
        close: char,
        open: char,
        dir: MoveDir1D,
    ) -> Option<Cursor> {
        self.find_bracket(rc, close, open, dir, 1)
    }

    fn find_bracket(
        &self,
        mut rc: RopeCursor<'_>,
        close: char,
        open: char,
        dir: MoveDir1D,
        mut count: usize,
    ) -> Option<Cursor> {
        while count != 0 {
            rc.offset(dir);

            match rc.peek()? {
                c if c == open => {
                    count += 1;
                },
                c if c == close => {
                    count -= 1;
                },
                c @ ('"' | '\'') => {
                    match dir {
                        MoveDir1D::Next => {
                            self.find_quote_end(&mut rc, c)?;
                        },
                        MoveDir1D::Previous => {
                            self.find_quote_start(&mut rc, c)?;
                        },
                    }
                },
                _ => {
                    continue;
                },
            }
        }

        return rc.to_cursor().into();
    }

    fn find_bracketed(
        &self,
        cursor: &Cursor,
        left: char,
        right: char,
        inclusive: bool,
        count: usize,
    ) -> Option<EditRange<Cursor>> {
        let off = self.cursor_to_offset(cursor);
        let rcl = self.offset_to_rc(off);
        let rcr = self.offset_to_rc(off);

        // Account for any bracket underneath the starting position.
        let c = rcl.peek()?;

        let (lcount, rcount) = if c == left {
            (count.saturating_sub(1), count)
        } else if c == right {
            (count, count.saturating_sub(1))
        } else {
            (count, count)
        };

        // Count the left brackets.
        let mut start = self.find_bracket(rcl, left, right, MoveDir1D::Previous, lcount)?;

        // Count the right brackets.
        let mut end = self.find_bracket(rcr, right, left, MoveDir1D::Next, rcount)?;

        if !inclusive {
            start.right(1);
            end.left(1);
        }

        let shape = TargetShape::CharWise;
        let range = EditRange { start, end, shape, inclusive: true };

        Some(range)
    }

    fn find_item(&self, nc: &Cursor) -> Option<Cursor> {
        let off = self.cursor_to_offset(nc);
        let mut rc = self.offset_to_rc(off);

        while let Some(c) = rc.peek() {
            match c {
                '(' => {
                    return self.find_match(rc, ')', '(', MoveDir1D::Next);
                },
                ')' => {
                    return self.find_match(rc, '(', ')', MoveDir1D::Previous);
                },
                '[' => {
                    return self.find_match(rc, ']', '[', MoveDir1D::Next);
                },
                ']' => {
                    return self.find_match(rc, '[', ']', MoveDir1D::Previous);
                },
                '{' => {
                    return self.find_match(rc, '}', '{', MoveDir1D::Next);
                },
                '}' => {
                    return self.find_match(rc, '{', '}', MoveDir1D::Previous);
                },
                '"' | '\'' => {
                    return None;
                },
                _ => {
                    rc.next();
                    continue;
                },
            }
        }

        return None;
    }

    fn find_item_range(&self, nc: &Cursor) -> Option<EditRange<Cursor>> {
        let off = self.cursor_to_offset(nc);
        let mut rc = self.offset_to_rc(off);

        while let Some(c) = rc.peek() {
            let (start, end) = match c {
                '(' => {
                    let start = rc.to_cursor();
                    let end = self.find_match(rc, ')', '(', MoveDir1D::Next)?;

                    (start, end)
                },
                ')' => {
                    let end = rc.to_cursor();
                    let start = self.find_match(rc, '(', ')', MoveDir1D::Previous)?;

                    (start, end)
                },
                '[' => {
                    let start = rc.to_cursor();
                    let end = self.find_match(rc, ']', '[', MoveDir1D::Next)?;

                    (start, end)
                },
                ']' => {
                    let end = rc.to_cursor();
                    let start = self.find_match(rc, '[', ']', MoveDir1D::Previous)?;

                    (start, end)
                },
                '{' => {
                    let start = rc.to_cursor();
                    let end = self.find_match(rc, '}', '{', MoveDir1D::Next)?;

                    (start, end)
                },
                '}' => {
                    let end = rc.to_cursor();
                    let start = self.find_match(rc, '{', '}', MoveDir1D::Previous)?;

                    (start, end)
                },
                '"' | '\'' => {
                    return None;
                },
                _ => {
                    rc.next();
                    continue;
                },
            };

            return EditRange::inclusive(start, end, TargetShape::CharWise).into();
        }

        return None;
    }

    fn find_quoted(
        &self,
        cursor: &Cursor,
        quote: char,
        inclusive: bool,
    ) -> Option<EditRange<Cursor>> {
        let off = self.cursor_to_offset(cursor);
        let mut rcl = self.offset_to_rc(off);
        let mut rcr = self.offset_to_rc(off);

        // Check if the cursor is already on the quote mark.
        if rcl.peek()? == quote {
            // XXX: implement
            return None;
        } else {
            self.find_quote_start(&mut rcl, quote)?;
            self.find_quote_end(&mut rcr, quote)?;
        }

        let mut start = rcl.to_cursor();
        let mut end = rcr.to_cursor();

        if !inclusive {
            start.right(1);
            end.left(1);
        }

        let shape = TargetShape::CharWise;
        let range = EditRange { start, end, shape, inclusive: true };

        Some(range)
    }

    fn seek_next<'a, O: BoundaryTest>(
        &'a self,
        obj: &O,
        terminus: MoveTerminus,
        mut bti: BoundaryTestIterator<'a>,
    ) -> Option<CharOff> {
        let mut res = None;

        while bti.ctx.count > 0 {
            if obj.is_boundary(terminus, &bti.ctx) {
                res = bti.pos().into();
                bti.ctx.count -= 1;
            }

            if bti.next_char() {
                break;
            }
        }

        return res;
    }

    pub(crate) fn seek<O: BoundaryTest>(
        &self,
        nc: &Cursor,
        obj: &O,
        terminus: MoveTerminus,
        dir: MoveDir1D,
        count: usize,
        motion: bool,
        lastcol: bool,
    ) -> Option<Cursor> {
        let bti = self.boundary(nc, dir, count, motion)?;
        let off = self.seek_next(obj, terminus, bti)?;
        let mut cursor = self.offset_to_cursor(off);

        /*
         * Word movements always move, even if they can't do a full count.
         */
        if motion && !lastcol {
            let x = self.max_column_idx(cursor.y, lastcol).min(cursor.x);

            cursor.set_x(x);
        }

        Some(cursor)
    }

    /// Returns an iterator over the newlines within this rope following `offset`.
    pub fn newlines(&self, offset: CharOff) -> NewlineIterator {
        let rc = self.offset_to_rc(offset);

        NewlineIterator { rc }
    }

    /// Returns an iterator over the characters within this rope following `position`.
    pub fn chars(&self, pos: CharOff) -> CharacterIterator {
        let end = self.last_offset().0;

        CharacterIterator::new(self, pos.0, end)
    }

    /// Returns an iterator over the characters within this rope following `position`.
    pub fn chars_until(&self, pos: CharOff, end: CharOff) -> CharacterIterator {
        CharacterIterator::new(self, pos.0, end.0)
    }

    /// Get the text before the cursor, and update the cursor to point to its start.
    pub fn get_prefix_word_mut(&self, cursor: &mut Cursor, style: &WordStyle) -> Option<Self> {
        let mut bti = self.boundary(cursor, MoveDir1D::Previous, 1, true)?;
        let mut res = None;

        while bti.ctx.count > 0 {
            if bti.next_char() {
                return None;
            }

            if !style.contains(bti.ctx.current) {
                return None;
            }

            if style.is_boundary(MoveTerminus::Beginning, &bti.ctx) {
                res = bti.pos().into();
                bti.ctx.count -= 1;
            }
        }

        let so = res?;
        let eo = self.cursor_to_offset(cursor);
        *cursor = self.offset_to_cursor(so);

        self.slice(so..eo).into()
    }

    /// Get the text before the cursor.
    pub fn get_prefix_word(&self, cursor: &Cursor, style: &WordStyle) -> Option<Self> {
        let mut cursor = cursor.clone();
        self.get_prefix_word_mut(&mut cursor, style)
    }

    /// Returns the [word](WordStyle) underneath the [Cursor], and updates it to point at the
    /// beginning of the word.
    ///
    /// If the cursor is not positioned over a word, this will search for the next word in the
    /// text.
    pub fn get_cursor_word_mut(&self, cursor: &mut Cursor, style: &WordStyle) -> Option<Self> {
        let end = self.seek(cursor, style, MoveTerminus::End, MoveDir1D::Next, 1, false, false)?;
        let start =
            self.seek(&end, style, MoveTerminus::Beginning, MoveDir1D::Previous, 1, false, false)?;

        let so = self.cursor_to_offset(&start);
        let eo = self.cursor_to_offset(&end);

        *cursor = start;

        self.slice(so..=eo).into()
    }

    /// Returns the [word](WordStyle) underneath the [Cursor].
    ///
    /// If the cursor is not positioned over a word, this will search for the next word in the
    /// text.
    pub fn get_cursor_word(&self, cursor: &Cursor, style: &WordStyle) -> Option<Self> {
        let mut cursor = cursor.clone();

        self.get_cursor_word_mut(&mut cursor, style)
    }
}

impl Default for EditRope {
    fn default() -> Self {
        EditRope::empty()
    }
}

impl Add for EditRope {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self {
        self.rope.append(rhs.rope);

        return self;
    }
}

impl AddAssign for EditRope {
    fn add_assign(&mut self, rhs: EditRope) {
        self.rope.append(rhs.rope);
    }
}

impl<'a> From<&'a EditRope> for Cow<'a, str> {
    fn from(s: &'a EditRope) -> Self {
        Cow::from(&s.rope)
    }
}

impl<'a> From<Cow<'a, str>> for EditRope {
    fn from(s: Cow<'a, str>) -> Self {
        EditRope { rope: Rope::from(s) }
    }
}

impl From<&str> for EditRope {
    fn from(s: &str) -> Self {
        EditRope { rope: Rope::from(s) }
    }
}

impl From<String> for EditRope {
    fn from(s: String) -> Self {
        EditRope::from(s.as_str())
    }
}

impl From<char> for EditRope {
    fn from(c: char) -> Self {
        EditRope::from(c.to_string())
    }
}

impl PartialEq<&str> for EditRope {
    fn eq(&self, other: &&str) -> bool {
        self.rope.eq(other)
    }
}

impl PartialEq<EditRope> for EditRope {
    fn eq(&self, other: &Self) -> bool {
        let len = self.len();

        if len != other.len() {
            return false;
        }

        return self.rope.eq(&other.rope);
    }
}

impl Eq for EditRope {}

impl Display for EditRope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.rope)
    }
}

impl CursorMovements<Cursor> for EditRope {
    fn first_word(&self, cursor: &Cursor, _: &CursorMovementsContext<'_, Cursor>) -> Cursor {
        let mut nc = cursor.clone();
        let cctx = &(self, 0usize, false);
        nc.first_word(cctx);

        return nc;
    }

    fn movement(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'_, Cursor>,
    ) -> Option<Cursor> {
        let lastcol = get_last_column(ctx);
        let cctx = &(self, ctx.view.get_width(), lastcol);
        let mut nc = cursor.clone();

        match (movement, ctx.context.resolve(count)) {
            // buffer position movements
            (MoveType::BufferByteOffset, off) => {
                let bmax = self.rope.len_bytes().saturating_sub(1);
                let boff = off.saturating_sub(1).min(bmax);
                let coff = self.rope.byte_to_char(boff);

                nc = self.offset_to_cursor(coff.into());

                PrivateCursorOps::clamp(&mut nc, cctx);
            },
            (MoveType::BufferPos(pos), _) => {
                nc.bufpos(*pos, cctx);
                nc.first_word(cctx);
            },
            (MoveType::BufferLineOffset, count) => {
                nc.set_line(count.saturating_sub(1), cctx);
                nc.first_word(cctx);
            },
            (MoveType::BufferLinePercent, count) => {
                if count > 100 {
                    return None;
                }

                // Calculate the new line number as described in :help N%
                let line = self.get_lines().saturating_mul(count).saturating_add(99) / 100;
                nc.set_line(line.saturating_sub(1), cctx);
                nc.first_word(cctx);
            },

            // linewise movement
            (MoveType::Line(dir), count) => {
                nc.line(*dir, count, cctx);
            },

            // paragraph-wise movement
            (MoveType::ParagraphBegin(_), _) => {
                // XXX: implement
                return None;
            },

            // sentence-wise movement
            (MoveType::SentenceBegin(_), _) => {
                // XXX: implement
                return None;
            },

            // section-wise movement
            (MoveType::SectionBegin(_), _) => {
                return None;
            },
            (MoveType::SectionEnd(_), _) => {
                return None;
            },

            // wordwise movement
            (MoveType::FinalNonBlank(dir), count) => {
                nc.line(*dir, count, cctx);
                nc.set_column(usize::MAX, cctx);
                nc.skip_space_rev(cctx);
            },
            (MoveType::FirstWord(dir), count) => {
                nc.line(*dir, count, cctx);
                nc.first_word(cctx);
            },
            (MoveType::WordBegin(style, dir), count) => {
                return self.find_boundary(
                    &nc,
                    style,
                    MoveTerminus::Beginning,
                    *dir,
                    count,
                    ctx.action.is_motion(),
                    lastcol,
                );
            },
            (MoveType::WordEnd(style, dir), count) => {
                return self.find_boundary(
                    &nc,
                    style,
                    MoveTerminus::End,
                    *dir,
                    count,
                    ctx.action.is_motion(),
                    lastcol,
                );
            },

            // charwise movement
            (MoveType::Column(dir, wrap), count) => {
                nc.column(*dir, *wrap, count, cctx);
            },
            (MoveType::ItemMatch, _) => {
                return self.find_item(&nc);
            },
            (MoveType::LineColumnOffset, count) => {
                nc.set_column(count.saturating_sub(1), cctx);
            },
            (MoveType::LinePercent, count) => {
                if count > 100 {
                    return None;
                }

                // Do the same method as BufferLinePercent.
                let col = self.get_columns(nc.y).saturating_mul(count).saturating_add(99) / 100;
                nc.set_column(col.saturating_sub(1), cctx);
            },
            (MoveType::LinePos(pos), count) => {
                let old = nc.y;
                nc.line(MoveDir1D::Next, count, cctx);

                if old.saturating_add(count) != nc.y {
                    return None;
                }

                // The final column is also allowed here when performing operations.
                let lastcol = cctx.2 || !ctx.action.is_motion();
                let maxcol = self.max_column_idx(nc.y, lastcol);

                // Move the cursor to the line position.
                let cctx = &(cctx.0, cctx.1, lastcol);
                nc.textpos(*pos, 0, maxcol, cctx);

                if let MovePosition::End = pos {
                    nc.xgoal = usize::MAX;
                }
            },

            // screen line movements
            (MoveType::ScreenFirstWord(dir), count) => {
                if ctx.view.wrap {
                    nc.screen_line(*dir, count, cctx);
                    nc.screen_linepos(MovePosition::Beginning, cctx);
                    nc.skip_space(cctx);
                } else {
                    nc.line(*dir, count, cctx);

                    let start = ctx.view.corner.x.min(nc.x);
                    let width = cctx.1.saturating_sub(1);

                    nc.textpos(MovePosition::Beginning, start, width, cctx);
                    nc.skip_space(cctx);
                }
            },
            (MoveType::ScreenLine(dir), count) => {
                if ctx.view.wrap {
                    nc.screen_line(*dir, count, cctx);
                } else {
                    nc.line(*dir, count, cctx);
                }
            },
            (MoveType::ScreenLinePos(pos), count) => {
                if ctx.view.wrap {
                    nc.screen_line(MoveDir1D::Next, count, cctx);
                    nc.screen_linepos(*pos, cctx);
                } else {
                    nc.line(MoveDir1D::Next, count, cctx);

                    let start = ctx.view.corner.x.min(nc.x);
                    let width = cctx.1.saturating_sub(1);

                    nc.textpos(*pos, start, width, cctx);
                }
            },

            // viewport position movements
            (MoveType::ViewportPos(MovePosition::Beginning), count) => {
                let count = count.saturating_sub(1);
                let top = ctx.view.corner.y;
                let bot = self._bottom_line_idx(ctx.view);
                let line = top.saturating_add(count).min(bot);

                nc.set_line(line, cctx);
                nc.first_word(cctx);
            },
            (MoveType::ViewportPos(MovePosition::Middle), _) => {
                let line = self._middle_line_idx(ctx.view);
                nc.set_line(line, cctx);
                nc.first_word(cctx);
            },
            (MoveType::ViewportPos(MovePosition::End), count) => {
                let count = count.saturating_sub(1);
                let top = ctx.view.corner.y;
                let bot = self._bottom_line_idx(ctx.view);
                let line = bot.saturating_sub(count).max(top);

                nc.set_line(line, cctx);
                nc.first_word(cctx);
            },
            (_, _) => {
                // Catch non-exhaustive pattern for future unimplemented movements.
                return None;
            },
        }

        return Some(nc);
    }

    fn range(
        &self,
        cursor: &Cursor,
        range: &RangeType,
        inclusive: bool,
        count: &Count,
        ctx: &CursorMovementsContext<'_, Cursor>,
    ) -> Option<EditRange<Cursor>> {
        match (range, count) {
            (RangeType::Item, _) => self.find_item_range(cursor),
            (RangeType::Word(obj), count) => {
                let motion = ctx.action.is_motion();
                let lastcol = get_last_column(ctx);
                let count = ctx.context.resolve(count);

                let end = self.seek(
                    cursor,
                    obj,
                    MoveTerminus::End,
                    MoveDir1D::Next,
                    count,
                    motion,
                    lastcol,
                )?;
                let start = self.seek(
                    &end,
                    obj,
                    MoveTerminus::Beginning,
                    MoveDir1D::Previous,
                    count,
                    motion,
                    lastcol,
                )?;

                if &start <= cursor && cursor <= &end {
                    EditRange::inclusive(start, end, TargetShape::CharWise).into()
                } else {
                    None
                }
            },
            (RangeType::Buffer, _) => {
                let start = self.first();
                let end = self.last();

                EditRange::inclusive(start, end, TargetShape::LineWise).into()
            },
            (RangeType::Line, count) => {
                let lastcol = get_last_column(ctx);
                let cctx = &(self, ctx.view.get_width(), lastcol);
                let count = ctx.context.resolve(count).saturating_sub(1);

                let start = cursor.clone();
                let mut end = cursor.clone();

                match ctx.action {
                    EditAction::Yank => {
                        // Place end cursor count lines away.
                        end.line(MoveDir1D::Next, count, cctx);
                    },
                    _ => {
                        // Place end cursor on the first word count lines away.
                        end.line(MoveDir1D::Next, count, cctx);
                        end.first_word(cctx);
                    },
                }

                EditRange::exclusive(start, end, TargetShape::LineWise).into()
            },
            (RangeType::Paragraph, _) => {
                // XXX: implement
                None
            },
            (RangeType::Sentence, _) => {
                // XXX: implement
                None
            },
            (RangeType::Bracketed(left, right), count) => {
                let count = ctx.context.resolve(count);

                self.find_bracketed(cursor, *left, *right, inclusive, count)
            },
            (RangeType::Quote(quote), _) => self.find_quoted(cursor, *quote, inclusive),
            (RangeType::XmlTag, _) => {
                // XXX: implement
                None
            },
            (_, _) => {
                // Catch non-exhaustive pattern for future unimplemented movements.
                None
            },
        }
    }

    /**
     * Given an action's movement, calculate the affected range base on the cursor's current
     * position.
     */
    fn range_of_movement(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'_, Cursor>,
    ) -> Option<EditRange<Cursor>> {
        let nc = self.movement(cursor, movement, count, ctx)?;
        let shape = movement.shape();

        let range = {
            let mut cc = cursor.clone();

            if nc < cc {
                // This was a leftwards movements.
                if movement.is_inclusive_motion() {
                    cc.left(1);
                }

                EditRange { start: nc, end: cc, shape, inclusive: false }
            } else {
                // Otherwise, this was a rightwards movement.
                let inclusive = movement.is_inclusive_motion();

                EditRange { start: cc, end: nc, shape, inclusive }
            }
        };

        Some(range)
    }
}

impl CursorSearch<Cursor> for EditRope {
    fn find_char(
        &self,
        cursor: &Cursor,
        inclusive: bool,
        dir: MoveDir1D,
        multiline: bool,
        needle: char,
        mut count: usize,
    ) -> Option<Cursor> {
        let mut nc = cursor.clone();
        let off = self.cursor_to_offset(&nc);
        let mut haystack = self.offset_to_rc(off);
        if !inclusive {
            // Move so we ignore any adjacent, matching character.
            haystack.offset(dir);
        }

        // Count occurrences of the character until we reach our goal.
        while count > 0 {
            haystack.offset(dir);

            match haystack.peek() {
                None => return None,
                Some('\n') if !multiline => return None,
                Some(c) => {
                    if needle == c {
                        count -= 1;
                        nc = haystack.to_cursor();
                    }
                },
            }
        }

        if !inclusive {
            // Reposition the cursor, so we're not on the character.
            match dir {
                MoveDir1D::Previous => nc.right(1),
                MoveDir1D::Next => nc.left(1),
            };
        }

        Some(nc)
    }

    fn find_matches(&self, start: &Cursor, end: &Cursor, needle: &Regex) -> Vec<EditRange<Cursor>> {
        let so = self.cursor_to_offset(start);
        let eo = self.cursor_to_offset(end);
        let rope = self.slice(so..=eo).rope;
        let text = CowStr::from(&rope);

        needle
            .find_iter(&text)
            .map(|m| {
                let mso = rope.byte_to_char(m.start());
                let meo = rope.byte_to_char(m.end());
                let sc = self.offset_to_cursor(so + CharOff(mso));
                let ec = self.offset_to_cursor(so + CharOff(meo));

                EditRange::exclusive(sc, ec, TargetShape::CharWise)
            })
            .collect()
    }

    fn find_regex(
        &self,
        cursor: &Cursor,
        dir: MoveDir1D,
        needle: &Regex,
        count: usize,
    ) -> Option<EditRange<Cursor>> {
        let start = self.cursor_to_offset(cursor);

        if start > self.last_offset() || self.is_empty() {
            return None;
        }

        match dir {
            MoveDir1D::Next => self._find_regex_next(start.0, needle, count),
            MoveDir1D::Previous => self._find_regex_previous(start.0, needle, count),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::application::EmptyInfo;
    use crate::editing::context::EditContext;
    use crate::editing::cursor::CursorState;
    use crate::env::vim::VimState;

    macro_rules! cmctx {
        ($vwctx: expr, $vctx: expr) => {
            &CursorMovementsContext {
                action: &EditAction::Motion,
                view: &$vwctx,
                context: &EditContext::from($vctx.clone()),
            }
        };
    }

    fn mkctx_vim() -> VimState<EmptyInfo> {
        VimState::default()
    }

    /// Make an [EditContext] with an [InsertStyle] and `last_column=true`.
    fn mkctx_last_col() -> EditContext {
        let mut state = mkctx_vim();
        state.persist.insert = Some(InsertStyle::Insert);
        state.into()
    }

    #[test]
    fn test_max_line_idx() {
        let r = EditRope::from("a\nbc\n\ndefg\nhijkl\n");

        assert_eq!(r.max_line_idx(), 4);
    }

    #[test]
    fn test_select() {
        let r = EditRope::from("hello world\nhelp\nkelp\n");

        // Select inclusive charwise range.
        let c = r.select(EditRange::inclusive(
            Cursor::new(0, 2),
            Cursor::new(1, 4),
            TargetShape::CharWise,
        ));
        assert_eq!(
            c,
            CursorState::Selection(Cursor::new(0, 2), Cursor::new(1, 4), TargetShape::CharWise)
        );

        // Select exclusive charwise range.
        let c = r.select(EditRange::exclusive(
            Cursor::new(0, 2),
            Cursor::new(1, 4),
            TargetShape::CharWise,
        ));
        assert_eq!(
            c,
            CursorState::Selection(Cursor::new(0, 2), Cursor::new(1, 3), TargetShape::CharWise)
        );

        // Select inclusive linewise range.
        let c = r.select(EditRange::inclusive(
            Cursor::new(0, 2),
            Cursor::new(1, 4),
            TargetShape::LineWise,
        ));
        assert_eq!(
            c,
            CursorState::Selection(Cursor::new(0, 2), Cursor::new(1, 4), TargetShape::LineWise)
        );

        // Select exclusive linewise range.
        let c = r.select(EditRange::exclusive(
            Cursor::new(0, 2),
            Cursor::new(1, 4),
            TargetShape::LineWise,
        ));
        assert_eq!(
            c,
            CursorState::Selection(Cursor::new(0, 2), Cursor::new(1, 3), TargetShape::LineWise)
        );
    }

    #[test]
    fn test_find_matches() {
        let r = EditRope::from("hello world\nhelp\nkelp\n");
        let needle1 = Regex::new("el").unwrap();
        let needle2 = Regex::new("ell").unwrap();

        // Search all text for /el/.
        let ms = r.find_matches(&Cursor::new(0, 0), &Cursor::new(2, 4), &needle1);
        assert_eq!(ms.len(), 3);
        assert_eq!(
            ms[0],
            EditRange::exclusive(Cursor::new(0, 1), Cursor::new(0, 3), TargetShape::CharWise)
        );
        assert_eq!(
            ms[1],
            EditRange::exclusive(Cursor::new(1, 1), Cursor::new(1, 3), TargetShape::CharWise)
        );
        assert_eq!(
            ms[2],
            EditRange::exclusive(Cursor::new(2, 1), Cursor::new(2, 3), TargetShape::CharWise)
        );

        // Search all text for /ell/.
        let ms = r.find_matches(&Cursor::new(0, 0), &Cursor::new(2, 4), &needle2);
        assert_eq!(ms.len(), 1);
        assert_eq!(
            ms[0],
            EditRange::exclusive(Cursor::new(0, 1), Cursor::new(0, 4), TargetShape::CharWise)
        );

        // Search the second line for /el/.
        let ms = r.find_matches(&Cursor::new(1, 0), &Cursor::new(1, 4), &needle1);
        assert_eq!(ms.len(), 1);
        assert_eq!(
            ms[0],
            EditRange::exclusive(Cursor::new(1, 1), Cursor::new(1, 3), TargetShape::CharWise)
        );

        // Search the second line for /ell/.
        let ms = r.find_matches(&Cursor::new(1, 0), &Cursor::new(1, 4), &needle2);
        assert_eq!(ms.len(), 0);

        // Searching the first two characters of the second line for /el/ should find nothing.
        let ms = r.find_matches(&Cursor::new(1, 0), &Cursor::new(1, 1), &needle1);
        assert_eq!(ms.len(), 0);

        // Searching the last two characters of the second line for /el/ should find nothing.
        let ms = r.find_matches(&Cursor::new(1, 2), &Cursor::new(1, 3), &needle1);
        assert_eq!(ms.len(), 0);

        // Searching the middle two characters of the second line for /el/ should find a match.
        let ms = r.find_matches(&Cursor::new(1, 1), &Cursor::new(1, 2), &needle1);
        assert_eq!(ms.len(), 1);
        assert_eq!(
            ms[0],
            EditRange::exclusive(Cursor::new(1, 1), Cursor::new(1, 3), TargetShape::CharWise)
        );
    }

    #[test]
    fn test_get_line_columns() {
        let r = EditRope::from("a\nbc\n\ndefg\nhijkl\n");

        assert_eq!(r.get_columns(0), 1);
        assert_eq!(r.get_columns(1), 2);
        assert_eq!(r.get_columns(2), 0);
        assert_eq!(r.get_columns(3), 4);
        assert_eq!(r.get_columns(4), 5);

        // No newlines.
        let r = EditRope::from("a b c");

        assert_eq!(r.get_columns(0), 5);

        // 2-byte characters.
        let r = EditRope::from("ЊЊЊ\nߚߚߚߚ\n");

        assert_eq!(r.get_columns(0), 3);
        assert_eq!(r.get_columns(1), 4);

        // 3-byte characters.
        let r = EditRope::from("∀∀∀∀∀\nლლლ\n");

        assert_eq!(r.get_columns(0), 5);
        assert_eq!(r.get_columns(1), 3);

        // 4-byte characters.
        let r = EditRope::from("𐍉𐍉𐍉𐍉\n𐎮𐎮𐎮𐎮𐎮\n");

        assert_eq!(r.get_columns(0), 4);
        assert_eq!(r.get_columns(1), 5);
    }

    #[test]
    fn test_is_blank_line() {
        let r = EditRope::from("a b c\n a b c \n\n    \n\t\na b c\n");

        assert_eq!(r.is_blank_line(0), false);
        assert_eq!(r.is_blank_line(1), false);
        assert_eq!(r.is_blank_line(2), true);
        assert_eq!(r.is_blank_line(3), true);
        assert_eq!(r.is_blank_line(4), true);
        assert_eq!(r.is_blank_line(5), false);
    }

    #[test]
    fn test_is_blank_range() {
        let r = EditRope::from("a b c\n a b c \n\n    \n\t\na b c\n");

        // Non-whitespace range.
        assert_eq!(r.is_blank_range(0.into(), 0.into()), false);

        // Non-whitespace range that ends on whitespace.
        assert_eq!(r.is_blank_range(0.into(), 1.into()), false);

        // Non-whitespace range that starts on whitespace.
        assert_eq!(r.is_blank_range(1.into(), 4.into()), false);

        // Non-whitespace range with whitespace next to both sides.
        assert_eq!(r.is_blank_range(7.into(), 12.into()), false);

        // Whitespace range with whitespace next to both sides.
        assert_eq!(r.is_blank_range(13.into(), 20.into()), true);

        // Whitespace ranges with non-whitespace next to both sides.
        assert_eq!(r.is_blank_range(1.into(), 1.into()), true);
        assert_eq!(r.is_blank_range(5.into(), 6.into()), true);
        assert_eq!(r.is_blank_range(12.into(), 21.into()), true);
    }

    #[test]
    fn test_line_after() {
        let r = EditRope::from("a b c\nd e f\n");

        assert_eq!(r.line_after(0), Some(6.into()));
        assert_eq!(r.line_after(1), Some(12.into()));

        let r = EditRope::from("\n\na b c\nd e f\n");

        assert_eq!(r.line_after(0), Some(1.into()));
        assert_eq!(r.line_after(1), Some(2.into()));
        assert_eq!(r.line_after(2), Some(8.into()));
        assert_eq!(r.line_after(3), Some(14.into()));
        assert_eq!(r.line_after(4), None);
    }

    #[test]
    fn test_lines_at_iter() {
        let r = EditRope::from("a b c d e f\ng h i j k l\n\nm n o p\n");

        let mut iter = r.lines(0);
        assert_eq!(iter.next().unwrap(), "a b c d e f");
        assert_eq!(iter.next().unwrap(), "g h i j k l");
        assert_eq!(iter.next().unwrap(), "");
        assert_eq!(iter.next().unwrap(), "m n o p");
        assert_eq!(iter.next(), None);

        let mut iter = r.lines(2);
        assert_eq!(iter.next().unwrap(), "");
        assert_eq!(iter.next().unwrap(), "m n o p");
        assert_eq!(iter.next(), None);

        let mut iter = r.lines_at(0, 4);
        assert_eq!(iter.next().unwrap(), "c d e f");
        assert_eq!(iter.next().unwrap(), "g h i j k l");
        assert_eq!(iter.next().unwrap(), "");
        assert_eq!(iter.next().unwrap(), "m n o p");
        assert_eq!(iter.next(), None);

        let mut iter = r.lines_at(1, 6);
        assert_eq!(iter.next().unwrap(), "j k l");
        assert_eq!(iter.next().unwrap(), "");
        assert_eq!(iter.next().unwrap(), "m n o p");
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_chars_until_iter() {
        let r = EditRope::from("hello\nworld\n");
        let mut iter = r.chars_until(3.into(), 7.into());

        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.pos(), 3.into());

        assert_eq!(iter.next_back(), Some('o'));
        assert_eq!(iter.pos_back(), 7.into());

        assert_eq!(iter.next(), Some('o'));
        assert_eq!(iter.pos(), 4.into());

        assert_eq!(iter.next_back(), Some('w'));
        assert_eq!(iter.pos_back(), 6.into());

        assert_eq!(iter.next(), Some('\n'));
        assert_eq!(iter.pos(), 5.into());

        assert_eq!(iter.next(), None);
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn test_chars_iter() {
        let r = EditRope::from("hello\nworld\n");
        let mut iter = r.chars(0.into());

        assert_eq!(iter.next(), Some('h'));
        assert_eq!(iter.pos(), 0.into());

        assert_eq!(iter.next(), Some('e'));
        assert_eq!(iter.pos(), 1.into());

        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.pos(), 2.into());

        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.pos(), 3.into());

        assert_eq!(iter.next(), Some('o'));
        assert_eq!(iter.pos(), 4.into());

        assert_eq!(iter.next(), Some('\n'));
        assert_eq!(iter.pos(), 5.into());

        assert_eq!(iter.next(), Some('w'));
        assert_eq!(iter.pos(), 6.into());

        assert_eq!(iter.next(), Some('o'));
        assert_eq!(iter.pos(), 7.into());

        assert_eq!(iter.next(), Some('r'));
        assert_eq!(iter.pos(), 8.into());

        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.pos(), 9.into());

        assert_eq!(iter.next(), Some('d'));
        assert_eq!(iter.pos(), 10.into());

        assert_eq!(iter.next(), Some('\n'));
        assert_eq!(iter.pos(), 11.into());

        assert_eq!(iter.next(), None);

        let r = EditRope::from("foo bar baz\n");
        let mut iter = r.chars(4.into());

        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.pos(), 4.into());

        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.pos(), 5.into());

        assert_eq!(iter.next(), Some('r'));
        assert_eq!(iter.pos(), 6.into());

        assert_eq!(iter.next(), Some(' '));
        assert_eq!(iter.pos(), 7.into());

        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.pos(), 8.into());

        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.pos(), 9.into());

        assert_eq!(iter.next(), Some('z'));
        assert_eq!(iter.pos(), 10.into());

        assert_eq!(iter.next(), Some('\n'));
        assert_eq!(iter.pos(), 11.into());

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_chars_double_ended_iter() {
        let r = EditRope::from("hello\nworld\n");
        let mut iter = r.chars(0.into());

        assert_eq!(iter.next_back(), Some('\n'));
        assert_eq!(iter.pos_back(), 11.into());

        assert_eq!(iter.next_back(), Some('d'));
        assert_eq!(iter.pos_back(), 10.into());

        assert_eq!(iter.next_back(), Some('l'));
        assert_eq!(iter.pos_back(), 9.into());

        assert_eq!(iter.next_back(), Some('r'));
        assert_eq!(iter.pos_back(), 8.into());

        assert_eq!(iter.next_back(), Some('o'));
        assert_eq!(iter.pos_back(), 7.into());

        assert_eq!(iter.next_back(), Some('w'));
        assert_eq!(iter.pos_back(), 6.into());

        assert_eq!(iter.next_back(), Some('\n'));
        assert_eq!(iter.pos_back(), 5.into());

        assert_eq!(iter.next_back(), Some('o'));
        assert_eq!(iter.pos_back(), 4.into());

        assert_eq!(iter.next_back(), Some('l'));
        assert_eq!(iter.pos_back(), 3.into());

        assert_eq!(iter.next_back(), Some('l'));
        assert_eq!(iter.pos_back(), 2.into());

        assert_eq!(iter.next_back(), Some('e'));
        assert_eq!(iter.pos_back(), 1.into());

        assert_eq!(iter.next_back(), Some('h'));
        assert_eq!(iter.pos_back(), 0.into());

        assert_eq!(iter.next_back(), None);

        let r = EditRope::from("foo bar baz\n");
        let mut iter = r.chars(4.into());

        assert_eq!(iter.next_back(), Some('\n'));
        assert_eq!(iter.pos_back(), 11.into());

        assert_eq!(iter.next_back(), Some('z'));
        assert_eq!(iter.pos_back(), 10.into());

        assert_eq!(iter.next_back(), Some('a'));
        assert_eq!(iter.pos_back(), 9.into());

        assert_eq!(iter.next_back(), Some('b'));
        assert_eq!(iter.pos_back(), 8.into());

        assert_eq!(iter.next_back(), Some(' '));
        assert_eq!(iter.pos_back(), 7.into());

        assert_eq!(iter.next_back(), Some('r'));
        assert_eq!(iter.pos_back(), 6.into());

        assert_eq!(iter.next_back(), Some('a'));
        assert_eq!(iter.pos_back(), 5.into());

        assert_eq!(iter.next_back(), Some('b'));
        assert_eq!(iter.pos_back(), 4.into());

        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn test_lines_iter() {
        let r = EditRope::from("a\nbc\ndef\ng\nhijklm\n");
        let mut iter = r.newlines(0.into());

        assert_eq!(iter.next(), Some(1.into()));
        assert_eq!(iter.next(), Some(4.into()));
        assert_eq!(iter.next(), Some(8.into()));
        assert_eq!(iter.next(), Some(10.into()));
        assert_eq!(iter.next(), Some(17.into()));
        assert_eq!(iter.next(), None);

        let r = EditRope::from("a\nb\nc\nd\n");
        let mut iter = r.newlines(3.into());

        assert_eq!(iter.next(), Some(3.into()));
        assert_eq!(iter.next(), Some(5.into()));
        assert_eq!(iter.next(), Some(7.into()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_trailing_newline() {
        // Empty rope.
        let mut r = EditRope::from("");
        r.trailing_newline();
        assert_eq!(r.to_string(), "\n");

        // Rope that doesn't contain any newlines.
        let mut r = EditRope::from("foo");
        r.trailing_newline();
        assert_eq!(r.to_string(), "foo\n");

        // Rope w/ newlines, but not at end.
        let mut r = EditRope::from("foo\nbar");
        r.trailing_newline();
        assert_eq!(r.to_string(), "foo\nbar\n");

        // Rope w/ newline at end.
        let mut r = EditRope::from("foo\nbar\n");
        r.trailing_newline();
        assert_eq!(r.to_string(), "foo\nbar\n");

        // Multibyte character.
        let mut r = EditRope::from("𐎮");
        r.trailing_newline();
        assert_eq!(r.to_string(), "𐎮\n");
    }

    #[test]
    fn test_offset_to_cursor() {
        let rope = EditRope::from("a b c\nd e f\ng h i\n");

        assert_eq!(rope.offset_to_cursor(0.into()), Cursor::new(0, 0));
        assert_eq!(rope.offset_to_cursor(2.into()), Cursor::new(0, 2));
        assert_eq!(rope.offset_to_cursor(5.into()), Cursor::new(0, 5));

        assert_eq!(rope.offset_to_cursor(6.into()), Cursor::new(1, 0));
        assert_eq!(rope.offset_to_cursor(8.into()), Cursor::new(1, 2));
        assert_eq!(rope.offset_to_cursor(11.into()), Cursor::new(1, 5));

        assert_eq!(rope.offset_to_cursor(12.into()), Cursor::new(2, 0));
        assert_eq!(rope.offset_to_cursor(14.into()), Cursor::new(2, 2));
        assert_eq!(rope.offset_to_cursor(17.into()), Cursor::new(2, 5));
    }

    #[test]
    fn test_line_of_offset() {
        let rope = EditRope::from("a b c\nd e f\ng h i\n");

        // All characters in first line are on line 0.
        assert_eq!(rope.line_of_offset(0.into()), 0);
        assert_eq!(rope.line_of_offset(1.into()), 0);
        assert_eq!(rope.line_of_offset(2.into()), 0);
        assert_eq!(rope.line_of_offset(3.into()), 0);
        assert_eq!(rope.line_of_offset(4.into()), 0);

        // Newline character is counted as the line that it ends.
        assert_eq!(rope.line_of_offset(5.into()), 0);

        // First character after newline is on the next line (line 1).
        assert_eq!(rope.line_of_offset(6.into()), 1);
    }

    #[test]
    fn test_offset_of_line_blank_line() {
        let rope = EditRope::from("a b c\n\nd e f\n");

        assert_eq!(rope.offset_of_line(0), 0.into());
        assert_eq!(rope.offset_of_line(1), 6.into());
        assert_eq!(rope.offset_of_line(2), 7.into());
    }

    #[test]
    fn test_offset_of_line_nl_end() {
        let rope = EditRope::from("a b c\nd e f\ng h i\n");

        assert_eq!(rope.offset_of_line(0), 0.into());
        assert_eq!(rope.offset_of_line(1), 6.into());
        assert_eq!(rope.offset_of_line(2), 12.into());
    }

    #[test]
    fn test_offset_of_line_no_nl_end() {
        let rope = EditRope::from("a b c\nd e f\ng h i");

        assert_eq!(rope.offset_of_line(0), 0.into());
        assert_eq!(rope.offset_of_line(1), 6.into());
        assert_eq!(rope.offset_of_line(2), 12.into());
    }

    #[test]
    fn test_rope_replace_nonl() {
        // Replace with longer text.
        let mut rope = EditRope::from("hello world");
        let (choice, adjs) = rope.replace(CharOff(3)..=CharOff(8), "lish go".into());
        assert_eq!(rope.to_string(), "hellish gold");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 3), Cursor::new(0, 9), Cursor::new(0, 3))
        );

        // Cursors located after the replaced text get pushed back.
        assert_eq!(adjs, vec![CursorAdjustment::Column {
            line: 0,
            column_start: 9,
            amt_line: 0,
            amt_col: 1,
        }]);

        // Replace with shorter text.
        let mut rope = EditRope::from("hello world");
        let (choice, adjs) = rope.replace(CharOff(3)..=CharOff(8), "f".into());
        assert_eq!(rope.to_string(), "helfld");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 3), Cursor::new(0, 3), Cursor::new(0, 3))
        );
        assert_eq!(adjs, vec![CursorAdjustment::Column {
            line: 0,
            column_start: 9,
            amt_line: 0,
            amt_col: -5,
        }]);
    }

    #[test]
    fn test_rope_replace_insert_nl() {
        // Replacement ends with newline.
        let mut rope = EditRope::from("hello world");
        let (choice, adjs) = rope.replace(CharOff(4)..CharOff(6), " site\n".into());
        assert_eq!(rope.to_string(), "hell site\nworld");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 4), Cursor::new(0, 9), Cursor::new(0, 4))
        );
        assert_eq!(adjs, vec![
            CursorAdjustment::Line {
                line_start: 0,
                line_end: 0,
                amount: 0,
                amount_after: 1,
            },
            CursorAdjustment::Column { line: 0, column_start: 6, amt_line: 1, amt_col: -6 },
        ]);

        // Replacement doesn't end with newline.
        let mut rope = EditRope::from("hello world");
        let (choice, adjs) = rope.replace(CharOff(2)..=CharOff(5), "avy\ngoodbye ".into());
        assert_eq!(rope.to_string(), "heavy\ngoodbye world");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 2), Cursor::new(1, 7), Cursor::new(0, 2))
        );
        assert_eq!(adjs, vec![
            CursorAdjustment::Line {
                line_start: 0,
                line_end: 0,
                amount: 0,
                amount_after: 1,
            },
            CursorAdjustment::Column { line: 0, column_start: 6, amt_line: 1, amt_col: 2 },
        ]);
    }

    #[test]
    fn test_rope_replace_remove_nl() {
        // Replaced string starts with newline.
        let mut rope = EditRope::from("a b c\nd e f");
        let (choice, adjs) = rope.replace(CharOff(5)..=CharOff(7), "-".into());
        assert_eq!(rope.to_string(), "a b c-e f");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 5), Cursor::new(0, 5), Cursor::new(0, 5))
        );
        assert_eq!(adjs, vec![
            CursorAdjustment::Column { line: 1, column_start: 2, amt_line: -1, amt_col: 4 },
            CursorAdjustment::Line {
                line_start: 1,
                line_end: 1,
                amount: 0,
                amount_after: -1,
            },
        ]);

        // Replaced string doesn't start with newline.
        let mut rope = EditRope::from("a b c\nd e f");
        let (choice, adjs) = rope.replace(CharOff(3)..=CharOff(7), "-".into());
        assert_eq!(rope.to_string(), "a b-e f");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 3), Cursor::new(0, 3), Cursor::new(0, 3))
        );
        assert_eq!(adjs, vec![
            CursorAdjustment::Column { line: 1, column_start: 2, amt_line: -1, amt_col: 2 },
            CursorAdjustment::Line {
                line_start: 1,
                line_end: 1,
                amount: 0,
                amount_after: -1,
            },
        ]);
    }

    #[test]
    fn test_rope_replace_multiline() {
        // Replace with fewer lines.
        let mut rope = EditRope::from("a b c\nd e f\ng h i");
        let (choice, adjs) = rope.replace(CharOff(5)..=CharOff(11), "1\n2".into());
        assert_eq!(rope.to_string(), "a b c1\n2g h i");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 5), Cursor::new(1, 0), Cursor::new(0, 5))
        );
        assert_eq!(adjs, vec![
            CursorAdjustment::Column { line: 2, column_start: 0, amt_line: -1, amt_col: 1 },
            CursorAdjustment::Line {
                line_start: 0,
                line_end: 2,
                amount: 0,
                amount_after: -1,
            },
        ]);

        // Replace with more lines.
        let mut rope = EditRope::from("a b c\nd e f\ng h i");
        let (choice, adjs) = rope.replace(CharOff(5)..=CharOff(11), "1\n2\n3\n4".into());
        assert_eq!(rope.to_string(), "a b c1\n2\n3\n4g h i");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 5), Cursor::new(3, 0), Cursor::new(0, 5))
        );
        assert_eq!(adjs, vec![
            CursorAdjustment::Line {
                line_start: 0,
                line_end: 2,
                amount: 0,
                amount_after: 1,
            },
            CursorAdjustment::Column { line: 2, column_start: 0, amt_line: 1, amt_col: 1 },
        ]);
    }

    #[test]
    fn test_rope_paste_empty() {
        let mut rope = EditRope::from("world");
        let cursor = Cursor::new(0, 2);

        // Paste CharWise
        let shape = TargetShape::CharWise;
        let choice = rope.paste(&cursor, MoveDir1D::Next, "".into(), shape).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));

        let choice = rope.paste(&cursor, MoveDir1D::Previous, "".into(), shape).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));

        // Paste LineWise
        let shape = TargetShape::LineWise;
        let choice = rope.paste(&cursor, MoveDir1D::Next, "".into(), shape).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));

        let choice = rope.paste(&cursor, MoveDir1D::Previous, "".into(), shape).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));

        // Paste BlockWise
        let shape = TargetShape::BlockWise;
        let choice = rope.paste(&cursor, MoveDir1D::Next, "".into(), shape).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));

        let choice = rope.paste(&cursor, MoveDir1D::Previous, "".into(), shape).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));
    }

    #[test]
    fn test_rope_insert_empty() {
        let mut rope = EditRope::from("world");
        let cursor = Cursor::new(0, 2);
        let style = InsertStyle::Insert;

        let choice = rope.insert(&cursor, MoveDir1D::Next, "".into(), style).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));

        let choice = rope.insert(&cursor, MoveDir1D::Previous, "".into(), style).0;
        assert_eq!(rope.to_string(), "world");
        assert_eq!(choice, CursorChoice::Single(Cursor::new(0, 2)));
    }

    #[test]
    fn test_rope_insert() {
        let mut rope = EditRope::from("world");
        let mut state = CursorState::Location(rope.first());
        let mut cursor = state.cursor();
        let style = InsertStyle::Insert;

        let choice = rope.insert(cursor, MoveDir1D::Previous, "h".into(), style).0;
        assert_eq!(rope.to_string(), "hworld");
        assert_eq!(
            choice,
            CursorChoice::Range(Cursor::new(0, 0), Cursor::new(0, 1), Cursor::new(0, 1))
        );

        state = choice.resolve(CursorEnd::Auto).unwrap();
        cursor = state.cursor();
        assert_eq!(cursor, &Cursor::new(0, 1));

        let choice = rope.insert(cursor, MoveDir1D::Previous, "e".into(), style).0;
        assert_eq!(rope.to_string(), "heworld");

        state = choice.resolve(CursorEnd::Auto).unwrap();
        cursor = state.cursor();
        assert_eq!(cursor, &Cursor::new(0, 2));

        let choice = rope.insert(cursor, MoveDir1D::Previous, "l".into(), style).0;
        assert_eq!(rope.to_string(), "helworld");

        state = choice.resolve(CursorEnd::Auto).unwrap();
        cursor = state.cursor();
        assert_eq!(cursor, &Cursor::new(0, 3));

        let choice = rope.insert(&Cursor::new(0, 2), MoveDir1D::Next, " ".into(), style).0;
        assert_eq!(rope.to_string(), "hel world");

        state = choice.resolve(CursorEnd::Auto).unwrap();
        cursor = state.cursor();
        assert_eq!(cursor, &Cursor::new(0, 2));

        let choice = rope.insert(cursor, MoveDir1D::Next, "o".into(), style).0;
        assert_eq!(rope.to_string(), "helo world");

        state = choice.resolve(CursorEnd::Auto).unwrap();
        cursor = state.cursor();
        assert_eq!(cursor, &Cursor::new(0, 2));

        let choice = rope.insert(cursor, MoveDir1D::Next, "l".into(), style).0;
        assert_eq!(rope.to_string(), "hello world");

        state = choice.resolve(CursorEnd::Auto).unwrap();
        cursor = state.cursor();
        assert_eq!(cursor, &Cursor::new(0, 2));
    }

    #[test]
    fn test_rope_repeat() {
        let rope1 = EditRope::from("a b c");
        let rep = rope1.repeat(TargetShape::CharWise, 4);
        assert_eq!(rep.to_string(), "a b ca b ca b ca b c");

        let rope2 = EditRope::from("a b c\n1 2 3\n");
        let rep = rope2.repeat(TargetShape::LineWise, 3);
        assert_eq!(rep.to_string(), "a b c\n1 2 3\na b c\n1 2 3\na b c\n1 2 3\n");

        let rope3 = EditRope::from("a\nb\nc");
        let rep = rope3.repeat(TargetShape::BlockWise, 5);
        assert_eq!(rep.to_string(), "aaaaa\nbbbbb\nccccc");
    }

    #[test]
    fn test_rope_first_last() {
        let rope = EditRope::from("");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(0, 0));

        let rope = EditRope::from("a b c");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(0, 4));

        let rope = EditRope::from("a\nb\nc");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(2, 0));

        let rope = EditRope::from("a\nb\nc\n");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(2, 1));

        let rope = EditRope::from("𐎮𐎮𐎮𐎮");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(0, 3));
    }

    #[test]
    fn test_rope_changecase() {
        let rope = EditRope::from("HeLlO WoRlD");

        let res = rope.changecase(&Case::Lower);
        assert_eq!(res.to_string(), "hello world");

        let res = rope.changecase(&Case::Upper);
        assert_eq!(res.to_string(), "HELLO WORLD");

        let res = rope.changecase(&Case::Title);
        assert_eq!(res.to_string(), "Hello World");

        let res = rope.changecase(&Case::Toggle);
        assert_eq!(res.to_string(), "hElLo wOrLd");

        // Some letters change into multiple characters when case changes.
        let rope = EditRope::from("Reißen");

        let res = rope.changecase(&Case::Lower);
        assert_eq!(res.to_string(), "reißen");

        let res = rope.changecase(&Case::Upper);
        assert_eq!(res.to_string(), "REISSEN");

        let res = rope.changecase(&Case::Title);
        assert_eq!(res.to_string(), "Reißen");

        let res = rope.changecase(&Case::Toggle);
        assert_eq!(res.to_string(), "rEISSEN");
    }

    #[test]
    fn test_rope_trim_start() {
        // Empty string.
        let res = EditRope::from("").trim_start();
        assert_eq!(res.to_string(), "");

        // No whitespace.
        let res = EditRope::from("a b").trim_start();
        assert_eq!(res.to_string(), "a b");

        // All spaces.
        let res = EditRope::from("   ").trim_start();
        assert_eq!(res.to_string(), "");

        // All tabs.
        let res = EditRope::from("\t\t\t").trim_start();
        assert_eq!(res.to_string(), "");

        // All newlines.
        let res = EditRope::from("\n\n\n").trim_start();
        assert_eq!(res.to_string(), "");

        // Spaces on left.
        let res = EditRope::from("  a b").trim_start();
        assert_eq!(res.to_string(), "a b");

        // Spaces on right.
        let res = EditRope::from("a b  ").trim_start();
        assert_eq!(res.to_string(), "a b  ");

        // Spaces on both sides.
        let res = EditRope::from("  a b  ").trim_start();
        assert_eq!(res.to_string(), "a b  ");

        // Newline on left, spaces on right.
        let res = EditRope::from("\na b  ").trim_start();
        assert_eq!(res.to_string(), "a b  ");

        // Spaces on left, newline on right.
        let res = EditRope::from("  a b\n").trim_start();
        assert_eq!(res.to_string(), "a b\n");
    }

    #[test]
    fn test_rope_trim_end() {
        // Empty string.
        let res = EditRope::from("").trim_end();
        assert_eq!(res.to_string(), "");

        // No whitespace.
        let res = EditRope::from("a b").trim_end();
        assert_eq!(res.to_string(), "a b");

        // All spaces.
        let res = EditRope::from("   ").trim_end();
        assert_eq!(res.to_string(), "");

        // All tabs.
        let res = EditRope::from("\t\t\t").trim_end();
        assert_eq!(res.to_string(), "");

        // All newlines.
        let res = EditRope::from("\n\n\n").trim_end();
        assert_eq!(res.to_string(), "");

        // Spaces on left.
        let res = EditRope::from("  a b").trim_end();
        assert_eq!(res.to_string(), "  a b");

        // Spaces on right.
        let res = EditRope::from("a b  ").trim_end();
        assert_eq!(res.to_string(), "a b");

        // Spaces on both sides.
        let res = EditRope::from("  a b  ").trim_end();
        assert_eq!(res.to_string(), "  a b");

        // Newline on left, spaces on right.
        let res = EditRope::from("\na b  ").trim_end();
        assert_eq!(res.to_string(), "\na b");

        // Spaces on left, newline on right.
        let res = EditRope::from("  a b\n").trim_end();
        assert_eq!(res.to_string(), "  a b");
    }

    #[test]
    fn test_rope_trim() {
        // Empty string.
        let res = EditRope::from("").trim();
        assert_eq!(res.to_string(), "");

        // No whitespace.
        let res = EditRope::from("a b").trim();
        assert_eq!(res.to_string(), "a b");

        // All spaces.
        let res = EditRope::from("   ").trim();
        assert_eq!(res.to_string(), "");

        // All tabs.
        let res = EditRope::from("\t\t\t").trim();
        assert_eq!(res.to_string(), "");

        // All newlines.
        let res = EditRope::from("\n\n\n").trim();
        assert_eq!(res.to_string(), "");

        // Spaces on left.
        let res = EditRope::from("  a b").trim();
        assert_eq!(res.to_string(), "a b");

        // Spaces on right.
        let res = EditRope::from("a b  ").trim();
        assert_eq!(res.to_string(), "a b");

        // Spaces on both sides.
        let res = EditRope::from("  a b  ").trim();
        assert_eq!(res.to_string(), "a b");

        // Newline on left, spaces on right.
        let res = EditRope::from("\na b  ").trim();
        assert_eq!(res.to_string(), "a b");

        // Spaces on left, newline on right.
        let res = EditRope::from("  a b\n").trim();
        assert_eq!(res.to_string(), "a b");
    }

    #[test]
    fn test_rope_trim_start_matches() {
        let f = |c| c == 'C';

        // Empty string.
        let res = EditRope::from("").trim_start_matches(f);
        assert_eq!(res.to_string(), "");

        // No C's.
        let res = EditRope::from("a b").trim_start_matches(f);
        assert_eq!(res.to_string(), "a b");

        // All C's.
        let res = EditRope::from("CCC").trim_start_matches(f);
        assert_eq!(res.to_string(), "");

        // C's on left.
        let res = EditRope::from("CCa b").trim_start_matches(f);
        assert_eq!(res.to_string(), "a b");

        // C's on right.
        let res = EditRope::from("a bCC").trim_start_matches(f);
        assert_eq!(res.to_string(), "a bCC");

        // C's on both sides.
        let res = EditRope::from("CCa bCC").trim_start_matches(f);
        assert_eq!(res.to_string(), "a bCC");
    }

    #[test]
    fn test_rope_trim_end_matches() {
        let f = |c| c == 'C';

        // Empty string.
        let res = EditRope::from("").trim_end_matches(f);
        assert_eq!(res.to_string(), "");

        // No C's.
        let res = EditRope::from("a b").trim_end_matches(f);
        assert_eq!(res.to_string(), "a b");

        // All C's.
        let res = EditRope::from("CCC").trim_end_matches(f);
        assert_eq!(res.to_string(), "");

        // C's on left.
        let res = EditRope::from("CCa b").trim_end_matches(f);
        assert_eq!(res.to_string(), "CCa b");

        // C's on right.
        let res = EditRope::from("a bCC").trim_end_matches(f);
        assert_eq!(res.to_string(), "a b");

        // C's on both sides.
        let res = EditRope::from("CCa bCC").trim_end_matches(f);
        assert_eq!(res.to_string(), "CCa b");
    }

    #[test]
    fn test_get_prefix_word_mut() {
        let rope = EditRope::from("foo bar baz\n");

        // No prefix before first character.
        let mut cursor = Cursor::new(0, 0);
        let res = rope.get_prefix_word_mut(&mut cursor, &WordStyle::Little);
        assert_eq!(res, None);
        assert_eq!(cursor, Cursor::new(0, 0));

        // Character under cursor doesn't count towards prefix.
        let mut cursor = Cursor::new(0, 8);
        let res = rope.get_prefix_word_mut(&mut cursor, &WordStyle::Little);
        assert_eq!(res, None);
        assert_eq!(cursor, Cursor::new(0, 8));

        // Characters before the space get used.
        let mut cursor = Cursor::new(0, 7);
        let res = rope.get_prefix_word_mut(&mut cursor, &WordStyle::Little).unwrap();
        assert_eq!(res.to_string(), "bar");
        assert_eq!(cursor, Cursor::new(0, 4));

        // Characters before the "r" get used.
        let mut cursor = Cursor::new(0, 6);
        let res = rope.get_prefix_word_mut(&mut cursor, &WordStyle::Little).unwrap();
        assert_eq!(res.to_string(), "ba");
        assert_eq!(cursor, Cursor::new(0, 4));
    }

    #[test]
    fn test_get_cursor_word_little() {
        let rope = EditRope::from("hello-world a b c\n");

        // Little word stops at hyphen.
        let cursor = Cursor::new(0, 0);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Little).unwrap();
        assert_eq!(res.to_string(), "hello");

        // Little word doesn't go backwards past hyphen.
        let cursor = Cursor::new(0, 6);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Little).unwrap();
        assert_eq!(res.to_string(), "world");

        // Start on space, and find next word.
        let cursor = Cursor::new(0, 11);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Little).unwrap();
        assert_eq!(res.to_string(), "a");

        // Start on single-character word.
        let cursor = Cursor::new(0, 14);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Little).unwrap();
        assert_eq!(res.to_string(), "b");
    }

    #[test]
    fn test_get_cursor_word_big() {
        let rope = EditRope::from("hello-world a b c\n");

        // Little word includes hyphen.
        let cursor = Cursor::new(0, 0);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Big).unwrap();
        assert_eq!(res.to_string(), "hello-world");

        // Big word goes backwards over hyphen.
        let cursor = Cursor::new(0, 6);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Big).unwrap();
        assert_eq!(res.to_string(), "hello-world");

        // Start on space, and find next word.
        let cursor = Cursor::new(0, 11);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Big).unwrap();
        assert_eq!(res.to_string(), "a");

        // Start on single-character word.
        let cursor = Cursor::new(0, 14);
        let res = rope.get_cursor_word(&cursor, &WordStyle::Big).unwrap();
        assert_eq!(res.to_string(), "b");
    }

    #[test]
    fn test_get_cursor_word_mut() {
        let text = EditRope::from("hello world\n");
        let mut cursor = Cursor::new(0, 8);

        let res = text.get_cursor_word_mut(&mut cursor, &WordStyle::Little).unwrap();
        assert_eq!(cursor, Cursor::new(0, 6));
        assert_eq!(res.to_string(), "world");
    }

    #[test]
    fn test_find_char() {
        let rope = EditRope::from("a b c a b c 1 2 3 a b c 1 2 3\na b a b\n");
        let mut cursor = rope.first();

        let next = MoveDir1D::Next;
        let prev = MoveDir1D::Previous;

        // "fc"
        let inclusive = true;
        let needle = 'c';
        let count = 1;

        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 4));

        // ";"
        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        // ";"
        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 22));

        assert_eq!(rope.find_char(&cursor, inclusive, next, true, needle, count), None);

        // ","
        cursor = rope.find_char(&cursor, inclusive, prev, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        // ","
        cursor = rope.find_char(&cursor, inclusive, prev, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 4));

        assert_eq!(rope.find_char(&cursor, inclusive, prev, true, needle, count), None);

        // ";"
        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        // ";"
        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 22));

        // "tb"
        let inclusive = false;
        let needle = 'b';

        // "," moves backwards successfully.
        cursor = rope.find_char(&cursor, inclusive, prev, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 21));

        // "," moves backwards successfully.
        cursor = rope.find_char(&cursor, inclusive, prev, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));

        // "," moves backwards successfully.
        cursor = rope.find_char(&cursor, inclusive, prev, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 3));

        // "," cannot succeed, and returns None.
        assert_eq!(rope.find_char(&cursor, inclusive, prev, true, needle, count), None);

        // ";" moves forward successfully.
        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        // ";" moves forward successfully.
        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 19));

        // ";" fails to move forward when wrap is false.
        assert_eq!(rope.find_char(&cursor, inclusive, next, false, needle, count), None);

        // "," moves backwards successfully.
        cursor = rope.find_char(&cursor, inclusive, prev, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));

        // If the count is higher than the number of results available, don't move.
        let count = 100;
        assert_eq!(rope.find_char(&cursor, inclusive, prev, true, needle, count), None);

        // When wrap is false, don't cross line endings for results ("3;").
        let count = 3;
        assert_eq!(rope.find_char(&cursor, inclusive, next, false, needle, count), None);

        // Making wrap true does allow crossing the line ending (wrapping "3;").
        let count = 3;
        cursor = rope.find_char(&cursor, inclusive, next, true, needle, count).unwrap();
        assert_eq!(cursor, Cursor::new(1, 5));
    }

    #[test]
    fn test_find_regex_next() {
        let zero = Cursor::new(0, 0);
        let needle = Regex::new("he").unwrap();
        let cw = TargetShape::CharWise;

        // Empty string.
        let character = EditRope::from("");
        let res = character.find_regex(&zero, MoveDir1D::Next, &needle, 1);
        assert_eq!(res, None);

        // Negative result.
        let character = EditRope::from("character");
        let res = character.find_regex(&zero, MoveDir1D::Next, &needle, 1);
        assert_eq!(res, None);

        // Positive result.
        let writhe = EditRope::from("writhe");
        let res = writhe.find_regex(&zero, MoveDir1D::Next, &needle, 1).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 5), cw));

        // Multiple count.
        let multi = EditRope::from("writhe helium\nworld help\n");

        let res = multi.find_regex(&zero, MoveDir1D::Next, &needle, 3).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(1, 6), Cursor::new(1, 7), cw));

        // Start at non-zero cursor.
        let cursor = Cursor::new(1, 6);
        let res = multi.find_regex(&cursor, MoveDir1D::Next, &needle, 2).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 8), cw));

        // Wrap around multiple times.
        let res = multi.find_regex(&zero, MoveDir1D::Next, &needle, 8).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 8), cw));
    }

    #[test]
    fn test_find_regex_previous() {
        let zero = Cursor::new(0, 0);
        let needle = Regex::new("he").unwrap();
        let cw = TargetShape::CharWise;

        // Empty string.
        let character = EditRope::from("");
        let res = character.find_regex(&zero, MoveDir1D::Previous, &needle, 1);
        assert_eq!(res, None);

        // Negative result.
        let character = EditRope::from("character");
        let res = character.find_regex(&zero, MoveDir1D::Previous, &needle, 1);
        assert_eq!(res, None);

        // Positive result.
        let writhe = EditRope::from("writhe");
        let res = writhe.find_regex(&zero, MoveDir1D::Previous, &needle, 1).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 5), cw));

        // Multiple count.
        let multi = EditRope::from("writhe helium\nworld help\n");

        let res = multi.find_regex(&zero, MoveDir1D::Previous, &needle, 2).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 8), cw));

        // Start at non-zero cursor.
        let cursor = Cursor::new(0, 4);
        let res = multi.find_regex(&cursor, MoveDir1D::Previous, &needle, 2).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 8), cw));

        // Wrap around multiple times.
        let res = multi.find_regex(&zero, MoveDir1D::Previous, &needle, 8).unwrap();
        assert_eq!(res, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 8), cw));
    }

    #[test]
    fn test_find_item() {
        let rope = EditRope::from("hello world ( a b c { d ( e () f ) g } h )");

        // Forward search for the outermost "(".
        assert_eq!(rope.find_item(&Cursor::new(0, 0)), Some(Cursor::new(0, 41)));
        assert_eq!(rope.find_item(&Cursor::new(0, 5)), Some(Cursor::new(0, 41)));
        assert_eq!(rope.find_item(&Cursor::new(0, 12)), Some(Cursor::new(0, 41)));

        // Forward search for the outermost "{".
        assert_eq!(rope.find_item(&Cursor::new(0, 13)), Some(Cursor::new(0, 37)));
        assert_eq!(rope.find_item(&Cursor::new(0, 20)), Some(Cursor::new(0, 37)));

        // Backwards searches for the outermost "}".
        assert_eq!(rope.find_item(&Cursor::new(0, 35)), Some(Cursor::new(0, 20)));
        assert_eq!(rope.find_item(&Cursor::new(0, 37)), Some(Cursor::new(0, 20)));

        // Backwards searches for the outermost ")".
        assert_eq!(rope.find_item(&Cursor::new(0, 39)), Some(Cursor::new(0, 12)));
        assert_eq!(rope.find_item(&Cursor::new(0, 40)), Some(Cursor::new(0, 12)));
        assert_eq!(rope.find_item(&Cursor::new(0, 41)), Some(Cursor::new(0, 12)));
    }

    #[test]
    fn test_find_item_single_quote() {
        let rope = EditRope::from("foo ( ')' ) bar");

        // Search fowards through the string.
        assert_eq!(rope.find_item(&Cursor::new(0, 0)), Some(Cursor::new(0, 10)));

        // Search backwards through the string.
        assert_eq!(rope.find_item(&Cursor::new(0, 9)), Some(Cursor::new(0, 4)));

        // Hitting the single quote while looking for an item to match returns None.
        assert_eq!(rope.find_item(&Cursor::new(0, 5)), None);
        assert_eq!(rope.find_item(&Cursor::new(0, 6)), None);
        assert_eq!(rope.find_item(&Cursor::new(0, 8)), None);

        // Starting on the paren in the quotes hits the start of the string and returns None.
        assert_eq!(rope.find_item(&Cursor::new(0, 7)), None);
    }
    #[test]
    fn test_find_item_double_quote() {
        let rope = EditRope::from("foo ( \")\" ) bar");

        // Search fowards through the string.
        assert_eq!(rope.find_item(&Cursor::new(0, 0)), Some(Cursor::new(0, 10)));

        // Search backwards through the string.
        assert_eq!(rope.find_item(&Cursor::new(0, 9)), Some(Cursor::new(0, 4)));

        // Hitting the double quote while looking for an item to match returns None.
        assert_eq!(rope.find_item(&Cursor::new(0, 5)), None);
        assert_eq!(rope.find_item(&Cursor::new(0, 6)), None);
        assert_eq!(rope.find_item(&Cursor::new(0, 8)), None);

        // Starting on the paren in the quotes hits the start of the string and returns None.
        assert_eq!(rope.find_item(&Cursor::new(0, 7)), None);
    }

    #[test]
    fn test_motion_char_line() {
        let rope = EditRope::from("hello\nworld\na b c d e\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        // Test moving down lines.
        let mov = MoveType::Line(MoveDir1D::Next);

        // Start out at (0, 0).
        assert_eq!(cursor, Cursor::new(0, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 0));

        // Test moving up lines.
        let mov = MoveType::Line(MoveDir1D::Previous);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Test moving columns/characters forward.
        let mov = MoveType::Column(MoveDir1D::Next, false);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 1));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 2));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 3));

        // Test that moving a line down preserves column position.
        let mov = MoveType::Line(MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 3));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 3));

        // Test that we can't move the cursor past the bottom line of the buffer.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 3));

        // Test moving columns/characters backwards.
        let mov = MoveType::Column(MoveDir1D::Previous, false);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 2));

        // Test moving columns forward with a count.
        vctx.action.count = Some(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 0));

        // Test moving columns backwards with a count.
        let mov = MoveType::Column(MoveDir1D::Next, false);
        vctx.action.count = Some(4);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 4));

        vctx.action.count = Some(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));

        // Can't move past last column when wrap is false.
        vctx.action.count = Some(10);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 8));

        // Moving up clamps cursor to a valid column.
        let mov = MoveType::Line(MoveDir1D::Previous);
        vctx.action.count = None;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 4).goal(8));

        // Moving down returns to xgoal column.
        let mov = MoveType::Line(MoveDir1D::Next);
        vctx.action.count = None;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 8));
    }

    #[test]
    fn test_motion_column_wrap() {
        let rope = EditRope::from("hello\nworld\na b c d e\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        assert_eq!(cursor, Cursor::new(0, 0));

        // Can move past last column when wrap is true.
        let mov = MoveType::Column(MoveDir1D::Next, true);
        vctx.action.count = Some(5);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        // Can move past first column when wrap is true.
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        vctx.action.count = Some(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 3));

        // When inserting text, we can access the last, empty column.
        vctx.persist.insert = Some(InsertStyle::Insert);

        // Move to last column.
        let mov = MoveType::Column(MoveDir1D::Next, true);
        vctx.action.count = Some(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));

        // Moving forward one from the last column moves onto next line.
        let mov = MoveType::Column(MoveDir1D::Next, true);
        vctx.action.count = Some(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        // Move back one from the first column moves into the last, empty column.
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        vctx.action.count = Some(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));
    }

    #[test]
    fn test_motion_word_accents() {
        let rope = EditRope::from("árvíztűrő tükörfúrógép");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        // "w"
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        // "b"
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));
    }

    #[test]
    fn test_motion_word() {
        let rope = EditRope::from("hello world\na,b,c,d e,f,g,h\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        // "w"
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 6));

        for x in 0..=6 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, x));
        }

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 8));

        for x in 9..14 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, x));
        }

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        // "B"
        let mov = MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 8));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 6));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Cannot move back any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // "W"
        let mov = MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 6));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 8));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        // "b"
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        cursor = Cursor::new(1, 8);

        for x in 0..=6 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, 6 - x));
        }

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 6));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // "e"
        let mov = MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 4));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        for x in 0..=6 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, x));
        }

        for x in 8..14 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, x));
        }

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        // "gE"
        let mov = MoveType::WordEnd(WordStyle::Big, MoveDir1D::Previous);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 6));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 4));

        // Previous word end of the first word is first character.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Cannot move back any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // "E"
        let mov = MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 4));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 6));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        // "ge"
        let mov = MoveType::WordEnd(WordStyle::Little, MoveDir1D::Previous);

        for x in 0..=5 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, 13 - x));
        }

        for x in 0..=6 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, 6 - x));
        }

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 4));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Cannot move back any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));
    }

    #[test]
    fn test_motion_word_begin_nonalphanum() {
        let rope = EditRope::from("hello   world  \nhow,are ,, you,doing\n today\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_last_col();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        // Emacs' <M-f> movement.
        let mov = MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 13));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 3));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 20));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));

        // Now move backwards!
        let mov = MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Previous);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 20));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 3));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 13));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Move forward to word end.
        let mov = MoveType::WordEnd(WordStyle::NonAlphaNum, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 15));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 3));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 10));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 14));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));
    }

    #[test]
    fn test_motion_word_alphanum() {
        let rope = EditRope::from("hello   world  \nhow,are ,, you,doing\n today\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_last_col();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        // Move forwards with WordStyle::AlphaNum.
        let mov = MoveType::WordBegin(WordStyle::AlphaNum, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 8));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 4));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 11));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 15));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 1));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));

        // Emacs' <M-b> movement.
        let mov = MoveType::WordBegin(WordStyle::AlphaNum, MoveDir1D::Previous);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 1));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 15));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 11));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 4));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 8));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Move forward to the word end.
        let mov = MoveType::WordEnd(WordStyle::AlphaNum, MoveDir1D::Next);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 4));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 12));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 2));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 6));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 13));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 19));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 5));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));

        // Cannot move any further.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 6));
    }

    #[test]
    fn test_final_non_blank() {
        let rope = EditRope::from("hello world       \na b c d e  \n12345\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();

        assert_eq!(cursor, Cursor::new(0, 0));

        // Move to final non-blank on current line ("g_").
        let mov = MoveType::FinalNonBlank(MoveDir1D::Next);
        let count = Count::Exact(0);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        // Move to final non-blank on next line ("2g_"), which has trailing spaces.
        let mov = MoveType::FinalNonBlank(MoveDir1D::Next);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 8));

        // Move to final non-blank on next line ("2g_"), which doesn't have trailing spaces.
        let mov = MoveType::FinalNonBlank(MoveDir1D::Next);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 4));

        // Cannot move forwards any further.
        let mov = MoveType::FinalNonBlank(MoveDir1D::Next);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 4));

        // Move back two lines.
        let mov = MoveType::FinalNonBlank(MoveDir1D::Previous);
        let count = Count::Exact(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));

        // Cannot move back any further.
        let mov = MoveType::FinalNonBlank(MoveDir1D::Previous);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 10));
    }

    #[test]
    fn test_first_word() {
        let rope = EditRope::from("       hello world\n  a b c d e\n    first\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();

        assert_eq!(cursor, Cursor::new(0, 0));

        // Move to first word on current line ("^").
        let mov = MoveType::FirstWord(MoveDir1D::Previous);
        let count = Count::Exact(0);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        // Move to first word on next line ("<Enter>" / "+").
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        let count = Count::Contextual;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 2));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 4));

        // Move backwards to first word of previous lines ("-").
        let mov = MoveType::FirstWord(MoveDir1D::Previous);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 2));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));
    }

    #[test]
    fn test_motion_line_pos() {
        let rope = EditRope::from("1234567890\nabcde\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let mut cursor = rope.first();

        assert_eq!(cursor, Cursor::new(0, 0));

        let mov = MoveType::LinePos(MovePosition::End);
        let count = Count::MinusOne;

        // Move to end of line.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9).goal(usize::MAX));

        // Repeating stays at end of line because of Count::MinusOne.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9).goal(usize::MAX));

        // Using a higher context count allows us to move to the next line ending.
        vctx.action.count = Some(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 4).goal(usize::MAX));

        // Trying to move past the end of the buffer fails.
        assert_eq!(rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)), None);

        // Move to middle of line.
        vctx.action.count = None;
        let mov = MoveType::LinePos(MovePosition::Middle);
        let count = Count::MinusOne;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 2));

        // Move to beginning of line.
        let mov = MoveType::LinePos(MovePosition::Beginning);
        let count = Count::MinusOne;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));
    }

    #[test]
    fn test_motion_line_column() {
        let rope = EditRope::from(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n",
        );
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();

        let mov = MoveType::LineColumnOffset;

        // Move to column 1 ("1|")
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Move to column 10 ("10|")
        let count = Count::Exact(10);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));

        // Move to column 8 ("8|")
        let count = Count::Exact(8);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        // Move to column 4 ("4|")
        let count = Count::Exact(4);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 3));

        // Attempting to move past the last column goes to last column ("1000|")
        let count = Count::Exact(1000);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));
    }

    #[test]
    fn test_motion_line_percent() {
        let rope = EditRope::from("abcdefghijklmnopqrstuvwxyz\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();

        let mov = MoveType::LinePercent;

        assert_eq!(cursor, Cursor::new(0, 0));

        // Move halfway into the line ("50gM")
        let count = Count::Exact(50);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 12));

        // Move to the end of the line ("100gM").
        let count = Count::Exact(100);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 25));

        // Move to the beginning of the line ("1gM").
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Can't move pass the end of the line ("101gM").
        let count = Count::Exact(101);
        assert_eq!(rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)), None);
    }

    #[test]
    fn test_motion_buffer_byte_offset() {
        let rope = EditRope::from(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n",
        );
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();

        let mov = MoveType::BufferByteOffset;

        // Move to byte 1 ("1go")
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Move to byte 6 ("6go")
        let count = Count::Exact(6);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));

        // Moving to byte 11, which is a newline, goes to byte 10 ("11go")
        let count = Count::Exact(11);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));

        // Move to byte 12, the beginning of the second line ("12go")
        let count = Count::Exact(12);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        // Move to byte 27, towards the middle of the third line ("12go")
        let count = Count::Exact(27);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 4));

        // Attempting to move past last byte goes to last character ("1000go")
        let count = Count::Exact(1000);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 9));
    }

    #[test]
    fn test_motion_buffer_line_offset() {
        let rope = EditRope::from(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n\
            uvwxyz,.<>\n\
            -_=+[{]}\\|\n\
            !@#$%^&*()\n\
            1234567890\n",
        );
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();

        // Move to end of buffer ("8G")
        let mov = MoveType::BufferLineOffset;
        let count = Count::Exact(8);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(6, 0));

        // Move to middle of buffer ("4G")
        let mov = MoveType::BufferLineOffset;
        let count = Count::Exact(4);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(3, 0));

        // Move to somewhere near beginning of buffer ("1G")
        let mov = MoveType::BufferLineOffset;
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Attempting to move past end goes to last line ("1000G")
        let mov = MoveType::BufferLineOffset;
        let count = Count::Exact(1000);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(6, 0));
    }

    #[test]
    fn test_motion_buffer_line_percent() {
        let rope = EditRope::from(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n\
            uvwxyz,.<>\n\
            -_=+[{]}\\|\n\
            !@#$%^&*()\n\
            1234567890\n\
            abcdefghij\n",
        );
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();

        // Move to end of buffer ("100%")
        let mov = MoveType::BufferLinePercent;
        let count = Count::Exact(100);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(7, 0));

        // Move to middle of buffer ("50%")
        let mov = MoveType::BufferLinePercent;
        let count = Count::Exact(50);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(3, 0));

        // Move to somewhere near beginning of buffer ("1%")
        let mov = MoveType::BufferLinePercent;
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));

        // Attempting to move past end fails ("101%")
        let mov = MoveType::BufferLinePercent;
        let count = Count::Exact(101);
        assert_eq!(rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)), None);

        // Attempting to move past end fails ("1000%")
        let mov = MoveType::BufferLinePercent;
        let count = Count::Exact(1000);
        assert_eq!(rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)), None);
    }

    #[test]
    fn test_motion_buffer_pos() {
        let rope = EditRope::from(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n\
            uvwxyz,.<>\n\
            -_=+[{]}\\|\n\
            !@#$%^&*()\n\
            1234567890\n",
        );
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        // Move to end of buffer ("G")
        let mov = MoveType::BufferPos(MovePosition::End);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(6, 0));

        // Move to middle of buffer
        let mov = MoveType::BufferPos(MovePosition::Middle);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(3, 0));

        // Move to beginning of buffer ("gg")
        let mov = MoveType::BufferPos(MovePosition::Beginning);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 0));
    }

    #[test]
    fn test_motion_viewport_pos() {
        let rope = EditRope::from(
            "12345\n\
            67890\n\
            ABCDE\n\
            1234567890\n\
            abcdefghijklmno\n\
            pqrst\n\
            uvwxyzzzzz\n\
            !!!!!!!\n\
            @@@@@@@\n\
            FGHIJ\n\
            KLMNO\n",
        );
        let mut vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let count = Count::Contextual;

        /*
         * Set up a wrapped, 5x7 screen. This will look like:
         *
         * +-----+
         * |ABCDE| <- line offset 2
         * |12345| <- line offset 3
         * |67890|
         * |abcde| <- line offset 4
         * |fghij|
         * |klmno|
         * |pqrst| <- line offset 5
         * +-----+
         */
        vwctx.corner = Cursor::new(2, 0);
        vwctx.set_wrap(true);
        vwctx.dimensions = (5, 7);

        // Start out at (3, 0).
        let mut cursor = Cursor::new(3, 0);

        // Navigate to the top of screen (H).
        let mov = MoveType::ViewportPos(MovePosition::Beginning);
        vctx.action.count = Some(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 0));

        // Navigate to the third line from the top of screen ("3H").
        let mov = MoveType::ViewportPos(MovePosition::Beginning);
        vctx.action.count = Some(3);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(4, 0));

        // Attempting to navigate to the seventh line from the top of screen just stops at the
        // bottommost visible line ("7H").
        let mov = MoveType::ViewportPos(MovePosition::Beginning);
        vctx.action.count = Some(7);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(5, 0));

        // Navigate to the middle of the screen ("M").
        let mov = MoveType::ViewportPos(MovePosition::Middle);
        vctx.action.count = None;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(4, 0));

        // Navigate to the bottom of the screen ("L").
        let mov = MoveType::ViewportPos(MovePosition::End);
        vctx.action.count = None;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(5, 0));

        // Navigate to the third line from the bottom of screen ("3L").
        let mov = MoveType::ViewportPos(MovePosition::End);
        vctx.action.count = Some(3);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(3, 0));

        // Attempting to navigate to the sixth line from the bottom of the screen just stops at the
        // topmost visible line ("6L").
        let mov = MoveType::ViewportPos(MovePosition::End);
        vctx.action.count = Some(6);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(2, 0));
    }

    #[test]
    fn test_motion_screen_wrap() {
        let rope = EditRope::from("abcdefghij\nklmnopqrstuvwxyz\n");
        let mut vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();

        vwctx.corner = Cursor::new(0, 0);
        vwctx.set_wrap(true);
        vwctx.dimensions = (5, 5);

        // Start out at (0, 0).
        let mut cursor = Cursor::new(0, 0);

        // Navigate up and down using MoveType::ScreenLine.
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        let count = Count::Exact(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        let mov = MoveType::ScreenLine(MoveDir1D::Previous);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));

        // Larger count than number of lines stops at last screen line.
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        let count = Count::Exact(10);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 15));

        // Cannot move any further than the last screen line.
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 15));

        // Move back up to the second screen line.
        let mov = MoveType::ScreenLine(MoveDir1D::Previous);
        let count = Count::Exact(4);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));

        // Navigate around the screen line using MoveType::ScreenLinePos.
        let mov = MoveType::ScreenLinePos(MovePosition::End);
        let count = Count::MinusOne;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));

        let mov = MoveType::ScreenLinePos(MovePosition::Middle);
        let count = Count::Exact(0);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        let mov = MoveType::ScreenLinePos(MovePosition::Beginning);
        let count = Count::Exact(0);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 5));
    }

    #[test]
    fn test_motion_screen_nowrap() {
        let rope = EditRope::from("abcdefghij\nklmnopqrstuvwxyz\n");
        let mut vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();

        vwctx.corner = Cursor::new(0, 5);
        vwctx.set_wrap(false);
        vwctx.dimensions = (5, 5);

        // Start out at (0, 0).
        let mut cursor = Cursor::new(0, 7);

        // Navigate up and down using MoveType::ScreenLine.
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));

        let mov = MoveType::ScreenLine(MoveDir1D::Previous);
        let count = Count::Exact(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        // Larger count than number of lines stops at last line.
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        let count = Count::Exact(10);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));

        // Cannot move any further than the last line.
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        let count = Count::Exact(1);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));

        // Navigate around the visible portion of the line using MoveType::ScreenLinePos.
        let mov = MoveType::ScreenLinePos(MovePosition::End);
        let count = Count::MinusOne;
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 9));

        let mov = MoveType::ScreenLinePos(MovePosition::Middle);
        let count = Count::Exact(0);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));

        let mov = MoveType::ScreenLinePos(MovePosition::Beginning);
        let count = Count::Exact(0);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 5));
    }

    #[test]
    fn test_motion_screen_first_word_wrap() {
        let rope = EditRope::from("abcde  f g  hij\n  klm  nop\n");
        let mut vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();

        vwctx.set_wrap(true);
        vwctx.corner = Cursor::new(0, 5);
        vwctx.dimensions = (5, 5);

        // Stay on current line.
        let mov = MoveType::ScreenFirstWord(MoveDir1D::Next);
        let count = Count::Exact(0);
        let cursor = Cursor::new(0, 8);
        let cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        // Move to first word three screen lines down.
        let mov = MoveType::ScreenFirstWord(MoveDir1D::Next);
        let count = Count::Exact(3);
        let cursor = Cursor::new(0, 8);
        let cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));

        // Move to first word two screen lines up.
        let mov = MoveType::ScreenFirstWord(MoveDir1D::Previous);
        let count = Count::Exact(2);
        let cursor = Cursor::new(1, 7);
        let cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 12));
    }

    #[test]
    fn test_motion_screen_first_word_nowrap() {
        let rope = EditRope::from("abcde  f g  hij\n  klm  nop\n");
        let mut vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();

        vwctx.set_wrap(false);
        vwctx.corner = Cursor::new(0, 5);
        vwctx.dimensions = (5, 5);

        // Stay on current line.
        let mov = MoveType::ScreenFirstWord(MoveDir1D::Next);
        let count = Count::Exact(0);
        let cursor = Cursor::new(0, 8);
        let cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 7));

        // Move to first word on the visible poriton of the line below.
        let mov = MoveType::ScreenFirstWord(MoveDir1D::Next);
        let count = Count::Exact(1);
        let cursor = Cursor::new(0, 8);
        let cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 7));
    }

    #[test]
    fn test_range_buffer() {
        let rope = EditRope::from("abcdef\nghijklmn\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let cw = TargetShape::LineWise;
        let count = Count::Contextual;
        let rt = RangeType::Buffer;

        // Test multiple starting points, to show it doesn't matter.
        let cursor = Cursor::new(0, 0);
        let er = rope.range(&cursor, &rt, true, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 0), Cursor::new(1, 8), cw));

        let cursor = Cursor::new(0, 3);
        let er = rope.range(&cursor, &rt, true, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 0), Cursor::new(1, 8), cw));

        let cursor = Cursor::new(1, 0);
        let er = rope.range(&cursor, &rt, true, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 0), Cursor::new(1, 8), cw));

        let cursor = Cursor::new(1, 5);
        let er = rope.range(&cursor, &rt, true, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 0), Cursor::new(1, 8), cw));
    }

    #[test]
    fn test_range_number_base2() {
        let rope = EditRope::from("abc103g-458\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;

        let rt = RangeType::Word(WordStyle::Number(Radix::Binary));
        let inc = false;

        for x in 0..=2 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
            assert_eq!(er, None);
        }

        for x in 3..=4 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(er, EditRange::inclusive(Cursor::new(0, 3), Cursor::new(0, 4), cw));
        }

        for x in 5..=10 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
            assert_eq!(er, None);
        }
    }

    #[test]
    fn test_range_number_base8() {
        let rope = EditRope::from("abc103g-458\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;

        let rt = RangeType::Word(WordStyle::Number(Radix::Octal));
        let inc = false;

        for x in 0..=2 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
            assert_eq!(er, None);
        }

        for x in 3..=5 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(er, EditRange::inclusive(Cursor::new(0, 3), Cursor::new(0, 5), cw));
        }

        let cursor = Cursor::new(0, 6);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);

        for x in 7..=9 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(er, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 9), cw));
        }

        let cursor = Cursor::new(0, 10);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_number_base10() {
        let rope = EditRope::from("abc103g-458\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;

        let rt = RangeType::Word(WordStyle::Number(Radix::Decimal));
        let inc = false;

        let cursor = Cursor::new(0, 1);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);

        for x in 3..=5 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(er, EditRange::inclusive(Cursor::new(0, 3), Cursor::new(0, 5), cw));
        }

        let cursor = Cursor::new(0, 6);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);

        for x in 7..=10 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(er, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 10), cw));
        }
    }

    #[test]
    fn test_range_number_base16() {
        let rope = EditRope::from("abc103g-458\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;

        let rt = RangeType::Word(WordStyle::Number(Radix::Hexadecimal));
        let inc = false;

        for x in 0..=5 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(er, EditRange::inclusive(Cursor::new(0, 0), Cursor::new(0, 5), cw));
        }

        let cursor = Cursor::new(0, 6);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);

        for x in 7..=10 {
            let cursor = Cursor::new(0, x);
            let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(er, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 10), cw));
        }
    }

    #[test]
    fn test_range_whitespace() {
        let rope = EditRope::from("a   \t   b\nc  \t\n\n    d");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let inc = true;

        // Test ranges without crossing newlines.
        let rt = RangeType::Word(WordStyle::Whitespace(false));

        let cursor = Cursor::new(0, 1);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 1), Cursor::new(0, 7), cw));

        let cursor = Cursor::new(0, 7);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 1), Cursor::new(0, 7), cw));

        let cursor = Cursor::new(1, 3);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(1, 1), Cursor::new(1, 3), cw));

        let cursor = Cursor::new(2, 0);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);

        let cursor = Cursor::new(3, 1);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(3, 0), Cursor::new(3, 3), cw));

        // Test ranges with crossing newlines.
        let rt = RangeType::Word(WordStyle::Whitespace(true));

        let cursor = Cursor::new(0, 1);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 1), Cursor::new(0, 7), cw));

        let cursor = Cursor::new(0, 7);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 1), Cursor::new(0, 7), cw));

        let cursor = Cursor::new(1, 3);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(1, 1), Cursor::new(3, 3), cw));

        let cursor = Cursor::new(2, 0);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(1, 1), Cursor::new(3, 3), cw));

        let cursor = Cursor::new(3, 1);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(1, 1), Cursor::new(3, 3), cw));
    }

    #[test]
    fn test_range_line() {
        let rope = EditRope::from("1 2 3\nhello world\n    foo bar\na b c d e f\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let cw = TargetShape::LineWise;
        let count = Count::Contextual;
        let rt = RangeType::Line;
        let cursor = Cursor::new(1, 6);
        let inc = true;

        // Select a single line.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 0), Cursor::new(1, 6), cw));

        // End cursor is placed on the first word (important for forced-motion compatibility).
        vctx.action.count = Some(2);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 6), Cursor::new(2, 4), cw));

        // Select up to the last line.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 6), Cursor::new(3, 0), cw));

        // Providing a count higher than number of lines stops at the last line.
        vctx.action.count = Some(4);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 6), Cursor::new(3, 0), cw));
    }

    #[test]
    fn test_range_bracketed_start_at_paren() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')');
        let inc = true;

        let cursor_al = Cursor::new(0, 9);
        let cursor_ar = Cursor::new(0, 11);
        let cursor_cl = Cursor::new(0, 25);
        let cursor_cr = Cursor::new(0, 27);

        // Select parentheses surrounding "a" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_al, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 9), Cursor::new(0, 11), cw));

        vctx.action.count = Some(1);
        let er = rope.range(&cursor_ar, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 9), Cursor::new(0, 11), cw));

        // Select parentheses surrounding "a" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_al, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        vctx.action.count = Some(3);
        let er = rope.range(&cursor_ar, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Select the parentheses surrounding "c" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_cl, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 25), Cursor::new(0, 27), cw));

        vctx.action.count = Some(1);
        let er = rope.range(&cursor_cr, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 25), Cursor::new(0, 27), cw));

        // Look for the parentheses surrounding "c" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_cl, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        vctx.action.count = Some(3);
        let er = rope.range(&cursor_cr, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));
    }

    #[test]
    fn test_range_bracketed_forward() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')');
        let inc = true;

        // These starting positions are before the quotes.
        let cursor_1 = Cursor::new(0, 5);
        let cursor_a = Cursor::new(0, 10);

        // Look for the parentheses surrounding "1".
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_1, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "a" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 9), Cursor::new(0, 11), cw));

        // Look for the parentheses surrounding "a" w/ count = 2.
        vctx.action.count = Some(2);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 29), cw));

        // Look for the parentheses surrounding "a" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "a" w/ count = 4.
        vctx.action.count = Some(4);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_bracketed_backward() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')');
        let inc = true;

        // These starting positions are after the quotes.
        let cursor_2 = Cursor::new(0, 31);
        let cursor_c = Cursor::new(0, 26);

        // Look for the parentheses surrounding "2".
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_2, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "c" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_c, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 25), Cursor::new(0, 27), cw));

        // Look for the parentheses surrounding "c" w/ count = 2.
        vctx.action.count = Some(2);
        let er = rope.range(&cursor_c, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 29), cw));

        // Look for the parentheses surrounding "c" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_c, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "c" w/ count = 4.
        vctx.action.count = Some(4);
        let er = rope.range(&cursor_c, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_bracketed_no_surrounding_parens() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')');
        let inc = true;

        // Left side of paren group.
        let cursor_l = Cursor::new(0, 3);
        let er = rope.range(&cursor_l, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);

        // Right side of paren group.
        let cursor_r = Cursor::new(0, 35);
        let er = rope.range(&cursor_r, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_bracketed_exclusive() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = mkctx_vim();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')');
        let inc = false;

        // These starting positions are before the quotes.
        let cursor_1 = Cursor::new(0, 5);
        let cursor_a = Cursor::new(0, 10);

        // Look for the parentheses surrounding "1".
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_1, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 5), Cursor::new(0, 33), cw));

        // Look for the parentheses surrounding "a" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 10), Cursor::new(0, 10), cw));

        // Look for the parentheses surrounding "a" w/ count = 2.
        vctx.action.count = Some(2);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 8), Cursor::new(0, 28), cw));

        // Look for the parentheses surrounding "a" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 5), Cursor::new(0, 33), cw));

        // Look for the parentheses surrounding "a" w/ count = 4.
        vctx.action.count = Some(4);
        let er = rope.range(&cursor_a, &rt, inc, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_quoted() {
        let rope = EditRope::from("a b c 'd e f \\'g h i\\' j k' l m n 'o p' q");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = mkctx_vim();
        let count = Count::Contextual;
        let rt = RangeType::Quote('\'');
        let cw = TargetShape::CharWise;
        let inc = true;

        // Start before escaped single quotes.
        let cursor = Cursor::new(0, 7);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 6), Cursor::new(0, 26), cw));

        // Start inside escaped single quotes.
        let cursor = Cursor::new(0, 17);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 6), Cursor::new(0, 26), cw));

        // Starting from "m" we get (0, 26) and (0, 34), even if we might not consider those
        // a pair when scanning from the start of the line.
        let cursor = Cursor::new(0, 30);
        let er = rope.range(&cursor, &rt, inc, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 26), Cursor::new(0, 34), cw));
    }

    #[test]
    fn test_len() {
        let rope = EditRope::from("\u{00AB}a\u{00BB}\n");

        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 0)).0, 0);
        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 1)).0, 1);
        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 2)).0, 2);
    }

    #[test]
    fn test_diff_add_start() {
        let rope1 = EditRope::from("1234 5678\nghijkl\n");
        let rope2 = EditRope::from("def\n1234 5678\nghijkl\n");
        let rope3 = EditRope::from("abcdef\n1234 5678\nghijkl\n");

        assert_eq!(rope1.diff(&rope1), vec![]);

        assert_eq!(rope1.diff(&rope2), vec![CursorAdjustment::Line {
            line_start: 0,
            line_end: usize::MAX,
            amount: 1,
            amount_after: 0,
        }]);

        assert_eq!(rope1.diff(&rope3), vec![CursorAdjustment::Line {
            line_start: 0,
            line_end: usize::MAX,
            amount: 1,
            amount_after: 0,
        }]);

        assert_eq!(rope2.diff(&rope3), vec![CursorAdjustment::Column {
            line: 0,
            column_start: 0,
            amt_line: 0,
            amt_col: 3,
        }]);
    }

    #[test]
    fn test_diff_del_start() {
        let rope1 = EditRope::from("abcdef\n1234 5678\nghijkl\n");
        let rope2 = EditRope::from("def\n1234 5678\nghijkl\n");
        let rope3 = EditRope::from("1234 5678\nghijkl\n");

        assert_eq!(rope1.diff(&rope1), vec![]);

        assert_eq!(rope1.diff(&rope2), vec![CursorAdjustment::Column {
            line: 0,
            column_start: 0,
            amt_line: 0,
            amt_col: -3,
        }]);

        assert_eq!(rope1.diff(&rope3), vec![
            CursorAdjustment::Column {
                line: 0,
                column_start: 0,
                amt_line: 0,
                amt_col: isize::MIN,
            },
            CursorAdjustment::Line {
                line_start: 0,
                line_end: 0,
                amount: isize::MIN,
                amount_after: -1,
            }
        ]);

        assert_eq!(rope2.diff(&rope3), vec![
            CursorAdjustment::Column {
                line: 0,
                column_start: 0,
                amt_line: 0,
                amt_col: isize::MIN,
            },
            CursorAdjustment::Line {
                line_start: 0,
                line_end: 0,
                amount: isize::MIN,
                amount_after: -1,
            }
        ]);
    }

    #[test]
    fn test_diff_add_middle() {
        let rope1 = EditRope::from("abcdef\nghijkl\n");
        let rope2 = EditRope::from("abcdef\n1234\nghijkl\n");
        let rope3 = EditRope::from("abcdef\n1234 5678\nghijkl\n");

        assert_eq!(rope1.diff(&rope1), vec![]);

        assert_eq!(rope1.diff(&rope2), vec![CursorAdjustment::Line {
            line_start: 1,
            line_end: usize::MAX,
            amount: 1,
            amount_after: 0,
        }]);

        assert_eq!(rope1.diff(&rope3), vec![CursorAdjustment::Line {
            line_start: 1,
            line_end: usize::MAX,
            amount: 1,
            amount_after: 0,
        }]);

        assert_eq!(rope2.diff(&rope3), vec![CursorAdjustment::Column {
            line: 1,
            column_start: 4,
            amt_line: 0,
            amt_col: 5,
        }]);
    }

    #[test]
    fn test_diff_del_middle() {
        let rope1 = EditRope::from("abcdef\n1234 5678\nghijkl\n");
        let rope2 = EditRope::from("abcdef\n1234\nghijkl\n");
        let rope3 = EditRope::from("abcdef\nghijkl\n");

        assert_eq!(rope1.diff(&rope1), vec![]);

        assert_eq!(rope1.diff(&rope2), vec![CursorAdjustment::Column {
            line: 1,
            column_start: 4,
            amt_line: 0,
            amt_col: -5,
        }]);

        assert_eq!(rope1.diff(&rope3), vec![
            CursorAdjustment::Column {
                line: 1,
                column_start: 0,
                amt_line: 0,
                amt_col: isize::MIN,
            },
            CursorAdjustment::Line {
                line_start: 1,
                line_end: 1,
                amount: isize::MIN,
                amount_after: -1,
            },
        ]);

        assert_eq!(rope2.diff(&rope3), vec![
            CursorAdjustment::Column {
                line: 1,
                column_start: 0,
                amt_line: 0,
                amt_col: isize::MIN,
            },
            CursorAdjustment::Line {
                line_start: 1,
                line_end: 1,
                amount: isize::MIN,
                amount_after: -1,
            },
        ]);
    }

    #[test]
    fn test_diff_add_end() {
        let rope1 = EditRope::from("hello world\nhello world\n");
        let rope2 = EditRope::from("hello world\nhello world\nhello\n");
        let rope3 = EditRope::from("hello world\nhello world\nhello world\n");

        assert_eq!(rope1.diff(&rope1), vec![]);

        assert_eq!(rope1.diff(&rope2), vec![CursorAdjustment::Line {
            line_start: 2,
            line_end: usize::MAX,
            amount: 1,
            amount_after: 0,
        }]);

        assert_eq!(rope1.diff(&rope3), vec![CursorAdjustment::Line {
            line_start: 2,
            line_end: usize::MAX,
            amount: 1,
            amount_after: 0,
        }]);

        assert_eq!(rope2.diff(&rope3), vec![CursorAdjustment::Column {
            line: 2,
            column_start: 5,
            amt_line: 0,
            amt_col: 6,
        }]);
    }

    #[test]
    fn test_diff_del_end() {
        let rope1 = EditRope::from("hello world\nhello world\nhello world\n");
        let rope2 = EditRope::from("hello world\nhello world\nhello\n");
        let rope3 = EditRope::from("hello world\nhello world\n");

        assert_eq!(rope1.diff(&rope1), vec![]);

        assert_eq!(rope1.diff(&rope2), vec![CursorAdjustment::Column {
            line: 2,
            column_start: 5,
            amt_line: 0,
            amt_col: -6,
        }]);

        assert_eq!(rope1.diff(&rope3), vec![CursorAdjustment::Line {
            line_start: 2,
            line_end: 2,
            amount: -1,
            amount_after: isize::MIN,
        }]);

        assert_eq!(rope2.diff(&rope3), vec![CursorAdjustment::Line {
            line_start: 2,
            line_end: 2,
            amount: -1,
            amount_after: isize::MIN,
        }]);
    }
}
