//! # High-level rope manipulation
//!
//! ## Overview
//!
//! This module provides a wrapper around a rope implementation that works with the types from
//! [editing::base], and provides a number of convenience functions and trait implementations to support
//! [EditBuffer](crate::editing::buffer::EditBuffer).
//!
//! [editing::base]: crate::editing::base
use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::Debug;
use std::ops::Add;
use std::ops::AddAssign;

use regex::{Match, Regex};

use xi_rope::delta::DeltaElement;
use xi_rope::diff::{Diff, LineHashDiff};
use xi_rope::rope::{BaseMetric, LinesMetric, Utf16CodeUnitsMetric};
use xi_rope::rope::{Rope, RopeInfo};
use xi_rope::tree::Cursor as RopeCursor;

use crate::editing::action::EditAction;
use crate::editing::cursor::{Cursor, CursorAdjustment, CursorChoice};

use crate::editing::base::{
    BoundaryTest,
    BoundaryTestContext,
    Case,
    Count,
    CursorMovements,
    CursorMovementsContext,
    CursorSearch,
    EditContext,
    EditRange,
    InsertStyle,
    MoveDir1D,
    MovePosition,
    MoveTerminus,
    MoveType,
    RangeType,
    TargetShape,
    ViewportContext,
    WordStyle,
};

/// Byte offset into an [EditRope].
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
pub struct ByteOff(usize);

impl PartialOrd for ByteOff {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Ord for ByteOff {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

struct U16Off(usize);

pub(super) type CursorContext<'a> = (&'a EditRope, usize, bool);

pub(super) trait PrivateCursorOps {
    fn set_column<'a>(&mut self, x: usize, ctx: &CursorContext<'a>);
    fn set_line<'a>(&mut self, y: usize, ctx: &CursorContext<'a>);
    fn set<'a>(&mut self, y: usize, x: usize, ctx: &CursorContext<'a>);
    fn clamp<'a>(&mut self, ctx: &CursorContext<'a>);

    fn line<'a>(&mut self, dir: MoveDir1D, count: usize, ctx: &CursorContext<'a>);
    fn column<'a>(&mut self, dir: MoveDir1D, wrap: bool, count: usize, ctx: &CursorContext<'a>);
    fn textpos<'a>(
        &mut self,
        pos: MovePosition,
        start: usize,
        width: usize,
        ctx: &CursorContext<'a>,
    );

    fn screen_line<'a>(&mut self, dir: MoveDir1D, count: usize, ctx: &CursorContext<'a>);
    fn screen_linepos<'a>(&mut self, pos: MovePosition, ctx: &CursorContext<'a>);

    fn bufpos<'a>(&mut self, pos: MovePosition, ctx: &CursorContext<'a>);
    fn skip_space<'a>(&mut self, ctx: &CursorContext<'a>);
    fn skip_space_rev<'a>(&mut self, ctx: &CursorContext<'a>);
    fn first_word<'a>(&mut self, ctx: &CursorContext<'a>);
}

impl PrivateCursorOps for Cursor {
    fn clamp<'a>(&mut self, ctx: &CursorContext<'a>) {
        self.set(self.y, self.x, ctx);
    }

    fn set<'a>(&mut self, y: usize, x: usize, ctx: &CursorContext<'a>) {
        let ymax = ctx.0.max_line_idx();

        self.y = y.min(ymax);

        let xmax = ctx.0.max_column_idx(self.y, ctx.2);

        self.x = x.min(xmax);
        self.xgoal = self.x;
    }

    fn set_column<'a>(&mut self, x: usize, ctx: &CursorContext<'a>) {
        self.x = x.min(ctx.0.max_column_idx(self.y, ctx.2));
        self.xgoal = self.x;
    }

    fn set_line<'a>(&mut self, y: usize, ctx: &CursorContext<'a>) {
        let nlines = ctx.0.max_line_idx();

        self.y = y.min(nlines);
        self.x = self.xgoal.min(ctx.0.max_column_idx(self.y, ctx.2));
    }

    fn bufpos<'a>(&mut self, pos: MovePosition, ctx: &CursorContext<'a>) {
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

    fn screen_line<'a>(&mut self, dir: MoveDir1D, mut count: usize, ctx: &CursorContext<'a>) {
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

    fn screen_linepos<'a>(&mut self, pos: MovePosition, ctx: &CursorContext<'a>) {
        let width = ctx.1;

        if width == 0 {
            return;
        }

        let start = self.x - (self.x % width);
        self.textpos(pos, start, width, ctx);
    }

    fn textpos<'a>(
        &mut self,
        pos: MovePosition,
        start: usize,
        width: usize,
        ctx: &CursorContext<'a>,
    ) {
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

    fn skip_space<'a>(&mut self, ctx: &CursorContext<'a>) {
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

    fn skip_space_rev<'a>(&mut self, ctx: &CursorContext<'a>) {
        let off = ctx.0.cursor_to_offset(self);
        let mut rc = RopeCursor::new(&ctx.0.rope, off.0);
        let mut x = self.x;

        while x > 0 {
            match rc.peek_next_codepoint() {
                None => break,
                Some(c) => {
                    if !c.is_ascii_whitespace() {
                        break;
                    }

                    x -= 1;
                },
            }

            rc.prev::<BaseMetric>();
        }

        self.set_column(x, ctx);
    }

    fn first_word<'a>(&mut self, ctx: &CursorContext<'a>) {
        self.set_column(0, ctx);
        self.skip_space(ctx);
    }

    fn line<'a>(&mut self, dir: MoveDir1D, count: usize, ctx: &CursorContext<'a>) {
        match dir {
            MoveDir1D::Previous => {
                self.set_line(self.y.saturating_sub(count), ctx);
            },
            MoveDir1D::Next => {
                self.set_line(self.y.saturating_add(count), ctx);
            },
        }
    }

    fn column<'a>(
        &mut self,
        dir: MoveDir1D,
        wrap: bool,
        mut count: usize,
        ctx: &CursorContext<'a>,
    ) {
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
    } else if b <= 0b110_11111 {
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
    rc_pos: RopeCursor<'a, RopeInfo>,
    rc_end: RopeCursor<'a, RopeInfo>,

    pos: Option<usize>,
    end: Option<usize>,

    first: usize,
    last: usize,
}

/// Iterator over a rope's newlines.
pub struct NewlineIterator<'a> {
    rc: RopeCursor<'a, RopeInfo>,
}

impl<'a> CharacterIterator<'a> {
    fn new(rope: &'a Rope, pos: usize, end: usize) -> Self {
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
            (Some(pos), None) => pos > self.last,
            (None, Some(end)) => self.first > end,
            (None, None) => false,
        }
    }

    /// Byte offset into the underlying rope of the last character returned from [Iterator::next].
    pub fn pos(&self) -> ByteOff {
        ByteOff(self.pos.expect("next() hasn't been called yet"))
    }

    /// Byte offset into the underlying rope of the last character returned from
    /// [DoubleEndedIterator::next_back].
    pub fn pos_back(&self) -> ByteOff {
        ByteOff(self.end.expect("next_back() hasn't been called yet"))
    }
}

impl<'a> Iterator for CharacterIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.rc_pos.peek_next_codepoint();

        self.pos = self.rc_pos.pos().into();
        self.rc_pos.next::<BaseMetric>();

        if self.done() {
            return None;
        } else {
            return res;
        }
    }
}

impl<'a> DoubleEndedIterator for CharacterIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let res = self.rc_end.peek_next_codepoint();

        self.end = self.rc_end.pos().into();
        self.rc_end.prev::<BaseMetric>();

        if self.done() {
            return None;
        } else {
            return res;
        }
    }
}

impl<'a> Iterator for NewlineIterator<'a> {
    type Item = ByteOff;

    fn next(&mut self) -> Option<Self::Item> {
        let nl = self.rc.next::<LinesMetric>()?;
        let nl = nl.saturating_sub(1);

        Some(ByteOff(nl))
    }
}

struct BoundaryTestIterator<'a> {
    chars: CharacterIterator<'a>,
    ctx: BoundaryTestContext,

    coff: ByteOff,
    aoff: ByteOff,
    boff: ByteOff,
}

impl<'a> BoundaryTestIterator<'a> {
    fn new(chars: CharacterIterator<'a>, ctx: BoundaryTestContext) -> Self {
        Self {
            chars,
            ctx,

            aoff: ByteOff(0),
            boff: ByteOff(0),
            coff: ByteOff(0),
        }
    }

    fn pos(&self) -> ByteOff {
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

fn cursor_from_rc(rc: &RopeCursor<'_, RopeInfo>) -> Cursor {
    let rope = rc.root();
    let tlen = rc.total_len();
    let maxi = tlen.saturating_sub(1);

    let off = rc.pos().min(maxi);
    let line = rope.line_of_offset(off);
    let col = off - rope.offset_of_line(line);

    Cursor::new(line, col)
}

#[inline]
fn move_cursor(rc: &mut RopeCursor<'_, RopeInfo>, dir: &MoveDir1D) -> Option<char> {
    let _ = match dir {
        MoveDir1D::Previous => rc.prev::<BaseMetric>(),
        MoveDir1D::Next => rc.next::<BaseMetric>(),
    };

    rc.peek_next_codepoint()
}

fn roperepeat(rope: &Rope, shape: TargetShape, mut times: usize) -> Rope {
    match shape {
        TargetShape::CharWise | TargetShape::LineWise => {
            let mut result = rope.slice(..);

            while times > 1 {
                result.edit(result.len().., rope.slice(..));

                times -= 1;
            }

            return result;
        },
        TargetShape::BlockWise => {
            let mut first = true;
            let mut result = Rope::from("");

            for line in rope.lines(0..) {
                if first {
                    first = false;
                } else {
                    result = result + Rope::from("\n");
                }

                result = result + Rope::from(line.repeat(times));
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

fn get_last_column<'a, 'b, 'c, C: EditContext>(
    ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
) -> bool {
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
    fn empty() -> EditRope {
        EditRope::from("")
    }

    /// Calculate the max indexable column in a given line given the current context.
    ///
    /// This function expects to be given a valid line number as input.
    pub(crate) fn max_column_idx<'a>(&self, y: usize, lastcol: bool) -> usize {
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
    fn max_line_idx<'a>(&self) -> usize {
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

    fn _middle_line_idx<'a, 'b, 'c>(&self, view: &ViewportContext<Cursor>) -> usize {
        if view.wrap {
            let (width, height) = view.dimensions;
            let mut lines = Vec::new();
            let mut l = 0;

            for line in self.lines_at(view.corner.y, view.corner.x) {
                let w = 1 + line.len().saturating_sub(1) / width;

                for _ in 0..w {
                    lines.push(l);
                }

                l += 1;

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
        ByteOff(start): ByteOff,
        ByteOff(end): ByteOff,
        inclusive: bool,
    ) -> (EditRope, EditRope, EditRope) {
        let size = self.rope.len();
        let beg = start.min(size);
        let end = end.min(size);

        if inclusive {
            let prefix = EditRope { rope: self.rope.slice(..beg) };
            let middle = EditRope { rope: self.rope.slice(beg..=end) };
            let suffix = EditRope { rope: self.rope.slice(end.saturating_add(1)..) };

            return (prefix, middle, suffix);
        } else {
            let prefix = EditRope { rope: self.rope.slice(..beg) };
            let middle = EditRope { rope: self.rope.slice(beg..end) };
            let suffix = EditRope { rope: self.rope.slice(end..) };

            return (prefix, middle, suffix);
        }
    }

    fn _trim_start_idx(&self) -> Option<ByteOff> {
        let mut rc = RopeCursor::new(&self.rope, 0);

        while let Some(c) = rc.peek_next_codepoint() {
            if c.is_ascii_whitespace() {
                rc.next::<BaseMetric>();
                continue;
            }

            return Some(ByteOff(rc.pos()));
        }

        return None;
    }

    fn _trim_end_idx(&self) -> Option<ByteOff> {
        let last = self.last_offset();
        let mut rc = RopeCursor::new(&self.rope, last.0);

        while let Some(c) = rc.peek_next_codepoint() {
            if c.is_ascii_whitespace() {
                rc.prev::<BaseMetric>();
                continue;
            }

            return Some(ByteOff(rc.pos()));
        }

        return None;
    }

    /// Remove leading whitespace from the rope.
    pub fn trim_start(&self) -> EditRope {
        if let Some(start) = self._trim_start_idx() {
            let end = self.last_offset();

            self.slice(start, end, true)
        } else {
            EditRope::from("")
        }
    }

    /// Remove trailing whitespace from the rope.
    pub fn trim_end(&self) -> EditRope {
        if let Some(end) = self._trim_end_idx() {
            self.slice(ByteOff(0), end, true)
        } else {
            EditRope::from("")
        }
    }

    /// Remove matching leading characters from the rope.
    pub fn trim_start_matches<F>(&self, matches: F) -> EditRope
    where
        F: Fn(char) -> bool,
    {
        let mut rc = RopeCursor::new(&self.rope, 0);

        while let Some(c) = rc.peek_next_codepoint() {
            if matches(c) {
                rc.next::<BaseMetric>();
                continue;
            }

            let rope = self.rope.slice(rc.pos()..);

            return EditRope { rope };
        }

        return EditRope::empty();
    }

    /// Remove matching trailing characters from the rope.
    pub fn trim_end_matches<F>(&self, matches: F) -> EditRope
    where
        F: Fn(char) -> bool,
    {
        let last = self.last_offset();
        let mut rc = RopeCursor::new(&self.rope, last.0);

        while let Some(c) = rc.peek_next_codepoint() {
            if matches(c) {
                rc.prev::<BaseMetric>();
                continue;
            }

            let rope = self.rope.slice(..=rc.pos());

            return EditRope { rope };
        }

        return EditRope::empty();
    }

    /// Remove whitespace from the start and end of the rope.
    pub fn trim(&self) -> EditRope {
        let start = self._trim_start_idx();
        let end = self._trim_end_idx();

        if let (Some(start), Some(end)) = (start, end) {
            self.slice(start, end, true)
        } else {
            EditRope::empty()
        }
    }

    /// Returns a slice of a range within the rope.
    pub fn slice(
        &self,
        ByteOff(start): ByteOff,
        ByteOff(end): ByteOff,
        inclusive: bool,
    ) -> EditRope {
        let size = self.rope.len();
        let beg = start.min(size);
        let end = end.min(size);

        let rope = if inclusive {
            self.rope.slice(beg..=end)
        } else {
            self.rope.slice(beg..end)
        };

        EditRope { rope }
    }

    /// Replace a range within the rope with some new text.
    pub fn replace(
        &mut self,
        ByteOff(start): ByteOff,
        ByteOff(end): ByteOff,
        inclusive: bool,
        s: &str,
    ) {
        if inclusive {
            self.rope.edit(start..=end, s);
        } else {
            self.rope.edit(start..end, s);
        }
    }

    /// Return the rope repeated *n* times.
    pub fn repeat(&self, shape: TargetShape, times: usize) -> EditRope {
        EditRope { rope: roperepeat(&self.rope, shape, times) }
    }

    /// Transform a range within the rope with function *f*, and return the updated version.
    pub fn transform(
        &self,
        start: ByteOff,
        end: ByteOff,
        inclusive: bool,
        f: impl Fn(EditRope) -> EditRope,
    ) -> EditRope {
        let (prefix, middle, suffix) = self.split(start, end, inclusive);

        return prefix + f(middle) + suffix;
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
        let off = self.offset_of_line(cursor.y).0;
        let tlines = text.get_lines() as isize;
        let tlen = text.len();

        self.rope.edit(off..off, text.rope);

        let adj = CursorAdjustment::Line {
            line_start: cursor.y,
            line_end: usize::MAX,
            amount: tlines,
            amount_after: 0,
        };

        let start = self.offset_to_cursor(ByteOff(off));
        let end = self.offset_to_cursor(ByteOff(off + tlen));

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
        let mut rc = self.offset_to_rc(coff);
        let tlines = text.get_lines() as isize;
        let tlen = text.len();

        let soff = match rc.next::<LinesMetric>() {
            Some(end) => {
                self.rope.edit(end..end, text.rope);
                end
            },
            None => {
                let end = self.rope.len();
                self.rope.edit(end..end, text.rope);
                self.rope.edit(end..end, Rope::from("\n"));
                end
            },
        };

        let lstart = self.line_of_offset(coff) + 1;
        let adj = CursorAdjustment::Line {
            line_start: lstart,
            line_end: usize::MAX,
            amount: tlines,
            amount_after: 0,
        };

        let start = self.offset_to_cursor(ByteOff(soff));
        let end = self.offset_to_cursor(ByteOff(soff + tlen));

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
        let alines = self.rope.slice(sline..).measure::<LinesMetric>();

        let loff = text.offset_of_line(ilines.min(alines));

        for line in text.rope.lines(0..loff.0) {
            let colmax = self.max_column_idx(c.y, true);
            let cstart = c.x.saturating_add(off).min(colmax);
            let ioff = self.lincol_to_offset(c.y, cstart).0;
            let tlen = line.len();

            adjs.push(CursorAdjustment::Column {
                line: c.y,
                column_start: cstart,
                amt_line: 0,
                amt_col: tlen as isize,
            });

            self.rope.edit(ioff..ioff, line);

            eoff = ByteOff(ioff + tlen);

            c.down(1);
        }

        if ilines > alines {
            let start = text.offset_of_line(alines).0;
            let append = text.rope.slice(start..) + Rope::from("\n");
            let alen = append.len();
            let len = self.rope.len();
            self.rope.edit(len..len, append);
            eoff = ByteOff(len + alen);
        }

        let start = self.offset_to_cursor(coff);
        let end = self.offset_to_cursor(eoff);
        let default = start.clone();

        return (CursorChoice::Range(start, end, default), adjs);
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

        let coff = self.cursor_to_offset(&cursor);
        let ioff = self.lincol_to_offset(cursor.y, cstart);

        let tlen = text.len();
        let tlines = text.get_lines() as isize;

        match style {
            InsertStyle::Replace => {
                self.rope.edit(ioff.0..ioff.0 + tlen, text.rope);
            },
            InsertStyle::Insert => {
                self.rope.edit(ioff.0..ioff.0, text.rope);
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
        let end = self.offset_to_cursor(ByteOff(ioff.0 + tlen));

        let noff = self.offset_to_u16(insend);
        let noff = noff.0.saturating_sub(1).saturating_add(co);
        let noff = self.u16_to_offset(U16Off(noff));
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
        if text.len() == 0 {
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
        if text.len() == 0 {
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
        let len = self.rope.len();
        let end = len.saturating_sub(1);
        let s = self.rope.slice(end..).to_string();
        match s.as_str() {
            "\n" => return,
            _ => {
                self.rope.edit(len..len, "\n");
            },
        }
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

    /// Returns true if a line only contains only whitespace characters.
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
    pub fn is_blank_range(&self, start: ByteOff, end: ByteOff) -> bool {
        for c in self.chars_until(start, end) {
            if !c.is_ascii_whitespace() {
                return false;
            }
        }

        return true;
    }

    /// Return the length in bytes as a [ByteOff].
    pub fn len_offset(&self) -> ByteOff {
        ByteOff(self.len())
    }

    /// Return the length in bytes.
    pub fn len(&self) -> usize {
        self.rope.len()
    }

    /// Return the number of lines in this rope.
    pub fn get_lines(&self) -> usize {
        self.rope.measure::<LinesMetric>()
    }

    /// Return the number of columns on the given line.
    pub fn get_columns(&self, line: usize) -> usize {
        let lbeg_off = self.offset_of_line(line);
        let lbeg_u16 = self.offset_to_u16(lbeg_off);

        let mut rc = self.offset_to_rc(lbeg_off);

        match rc.next::<LinesMetric>() {
            Some(lend_off) => {
                let lend_off = ByteOff(lend_off);
                let lend_u16 = self.offset_to_u16(lend_off);

                return lend_u16.0.saturating_sub(lbeg_u16.0).saturating_sub(1);
            },
            None => {
                let lend_off = ByteOff(self.rope.len());
                let lend_u16 = self.offset_to_u16(lend_off);

                return lend_u16.0.saturating_sub(lbeg_u16.0);
            },
        }
    }

    pub(crate) fn lines(&self, line: usize) -> xi_rope::rope::Lines {
        let off = self.offset_of_line(line).0;

        self.rope.lines(off..)
    }

    pub(crate) fn lines_at(&self, line: usize, column: usize) -> xi_rope::rope::Lines {
        let off = self.lincol_to_offset(line, column).0;

        self.rope.lines(off..)
    }

    /// Return the line number of the given byte offset.
    pub fn line_of_offset(&self, off: ByteOff) -> usize {
        self.rope.line_of_offset(off.0)
    }

    /// Return the byte offset of the start of a given line.
    pub fn offset_of_line(&self, line: usize) -> ByteOff {
        ByteOff(self.rope.offset_of_line(line))
    }

    /// Convert a byte offset to a [Cursor].
    pub fn offset_to_cursor<'a>(&self, off: ByteOff) -> Cursor {
        let off = off.min(self.last_offset());

        let line = self.line_of_offset(off);
        let loff = self.offset_of_line(line);
        let lu16 = self.offset_to_u16(loff);
        let off_u16 = self.offset_to_u16(off);

        Cursor::new(line, off_u16.0 - lu16.0)
    }

    fn offset_to_rc<'a>(&self, off: ByteOff) -> RopeCursor<'_, RopeInfo> {
        RopeCursor::new(&self.rope, off.0)
    }

    fn offset_to_u16(&self, off: ByteOff) -> U16Off {
        U16Off(self.rope.count::<Utf16CodeUnitsMetric>(off.0))
    }

    fn lincol_to_u16(&self, line: usize, col: usize) -> U16Off {
        let linoff = self.offset_of_line(line);

        U16Off(self.offset_to_u16(linoff).0 + col)
    }

    fn u16_to_offset(&self, U16Off(u16off): U16Off) -> ByteOff {
        ByteOff(self.rope.count_base_units::<Utf16CodeUnitsMetric>(u16off))
    }

    fn lincol_to_offset(&self, line: usize, col: usize) -> ByteOff {
        self.u16_to_offset(self.lincol_to_u16(line, col))
    }

    /// Convert a cursor to a byte offset.
    pub fn cursor_to_offset(&self, cursor: &Cursor) -> ByteOff {
        self.lincol_to_offset(cursor.y, cursor.x)
    }

    /// Return a cursor located at the first character in the rope.
    pub fn first(&self) -> Cursor {
        Cursor::new(0, 0)
    }

    /// Return the last byte offset in the rope.
    pub fn last_offset(&self) -> ByteOff {
        ByteOff(self.rope.len().saturating_sub(1))
    }

    /// Return a cursor located at the last character in the rope.
    pub fn last(&self) -> Cursor {
        self.offset_to_cursor(self.last_offset())
    }

    /// Compare this rope with a new version, and return a vector of adjustments needed to fix
    /// cursors and marks when moving to the new version.
    pub fn diff(&self, other: &EditRope) -> Vec<CursorAdjustment> {
        let delta = LineHashDiff::compute_delta(&self.rope, &other.rope);
        let mut adjs = Vec::new();
        let mut last = ByteOff(0);
        let mut inserted = 0;

        for el in delta.els.into_iter() {
            match el {
                DeltaElement::Copy(start, end) => {
                    /*
                     * DeltaElement::Copy represents what bytes are copied from the base to create
                     * the other rope. Gaps between the offsets are therefore deletions, and we
                     * need to adjust later lines and anything following removed columns.
                     */
                    let start = ByteOff(start);
                    let end = ByteOff(end);

                    if start == last {
                        last = end;
                        continue;
                    }

                    let clast = self.offset_to_cursor(last);
                    let sline = self.line_of_offset(start);

                    if clast.y == sline {
                        let lu16 = self.offset_to_u16(last);
                        let su16 = self.offset_to_u16(start);

                        adjs.push(CursorAdjustment::Column {
                            line: clast.y + inserted,
                            column_start: clast.x,
                            amt_line: 0,
                            amt_col: -(su16.0 as isize - lu16.0 as isize),
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
                    let nlines = node.measure::<LinesMetric>();

                    if nlines == 0 {
                        let clast = self.offset_to_cursor(last);
                        let added = node.measure::<Utf16CodeUnitsMetric>();

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
        let text = self.to_string();
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

        return None;
    }

    fn _find_regex_next(
        &self,
        start: usize,
        needle: &Regex,
        mut count: usize,
    ) -> Option<EditRange<Cursor>> {
        let text = self.to_string();

        // Start search right after the cursor position.
        let mut res: Option<Match> = None;
        let mut pos = next_utf8(text.as_ref(), start);

        macro_rules! advance {
            () => {
                if let Some(m) = res {
                    let e = m.end();
                    pos = e;

                    if m.start() == e {
                        pos = next_utf8(text.as_ref(), pos);
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
        let off = self.cursor_to_offset(&nc);
        let off_u16 = self.offset_to_u16(off);

        let boff = self.u16_to_offset(U16Off(off_u16.0.saturating_sub(1)));
        let aoff = self.u16_to_offset(U16Off(off_u16.0.saturating_add(1)));

        let (chars, skip_first) = match dir {
            MoveDir1D::Next => {
                if off_u16.0 > 0 {
                    (self.chars(boff), true)
                } else {
                    (self.chars(off), false)
                }
            },
            MoveDir1D::Previous => {
                let last = self.last_offset();

                if aoff > last {
                    (self.chars_until(ByteOff(0), last), false)
                } else {
                    (self.chars_until(ByteOff(0), aoff), true)
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

    fn find_quote_start<'a>(&self, rc: &mut RopeCursor<'_, RopeInfo>, quote: char) -> Option<()> {
        loop {
            rc.prev::<BaseMetric>();

            match rc.peek_next_codepoint()? {
                c if c == quote => {
                    // Make sure the quote isn't preceded by an escape.
                    let off = rc.pos();

                    rc.prev::<BaseMetric>();

                    match rc.peek_next_codepoint() {
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

    fn find_quote_end<'a>(&self, rc: &mut RopeCursor<'_, RopeInfo>, quote: char) -> Option<()> {
        loop {
            rc.next::<BaseMetric>();

            match rc.peek_next_codepoint()? {
                c if c == quote => {
                    return Some(());
                },
                '\\' => {
                    // Skip next character.
                    rc.next::<BaseMetric>();
                },
                _ => {
                    continue;
                },
            }
        }
    }

    fn find_match<'a>(
        &self,
        rc: RopeCursor<'_, RopeInfo>,
        close: char,
        open: char,
        dir: MoveDir1D,
    ) -> Option<Cursor> {
        self.find_bracket(rc, close, open, dir, 1)
    }

    fn find_bracket<'a>(
        &self,
        mut rc: RopeCursor<'_, RopeInfo>,
        close: char,
        open: char,
        dir: MoveDir1D,
        mut count: usize,
    ) -> Option<Cursor> {
        while count != 0 {
            match move_cursor(&mut rc, &dir)? {
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

        return cursor_from_rc(&rc).into();
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
        let c = rcl.peek_next_codepoint()?;

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

        while let Some(c) = rc.peek_next_codepoint() {
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
                    rc.next::<BaseMetric>();
                    continue;
                },
            }
        }

        return None;
    }

    fn find_item_range(&self, nc: &Cursor) -> Option<EditRange<Cursor>> {
        let off = self.cursor_to_offset(nc);
        let mut rc = self.offset_to_rc(off);

        while let Some(c) = rc.peek_next_codepoint() {
            let (start, end) = match c {
                '(' => {
                    let start = cursor_from_rc(&rc);
                    let end = self.find_match(rc, ')', '(', MoveDir1D::Next)?;

                    (start, end)
                },
                ')' => {
                    let end = cursor_from_rc(&rc);
                    let start = self.find_match(rc, '(', ')', MoveDir1D::Previous)?;

                    (start, end)
                },
                '[' => {
                    let start = cursor_from_rc(&rc);
                    let end = self.find_match(rc, ']', '[', MoveDir1D::Next)?;

                    (start, end)
                },
                ']' => {
                    let end = cursor_from_rc(&rc);
                    let start = self.find_match(rc, '[', ']', MoveDir1D::Previous)?;

                    (start, end)
                },
                '{' => {
                    let start = cursor_from_rc(&rc);
                    let end = self.find_match(rc, '}', '{', MoveDir1D::Next)?;

                    (start, end)
                },
                '}' => {
                    let end = cursor_from_rc(&rc);
                    let start = self.find_match(rc, '{', '}', MoveDir1D::Previous)?;

                    (start, end)
                },
                '"' | '\'' => {
                    return None;
                },
                _ => {
                    rc.next::<BaseMetric>();
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
        if rcl.peek_next_codepoint()? == quote {
            // XXX: implement
            return None;
        } else {
            self.find_quote_start(&mut rcl, quote)?;
            self.find_quote_end(&mut rcr, quote)?;
        }

        let mut start = cursor_from_rc(&rcl);
        let mut end = cursor_from_rc(&rcr);

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
    ) -> Option<ByteOff> {
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

    fn seek<O: BoundaryTest>(
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
    pub fn newlines(&self, offset: ByteOff) -> NewlineIterator {
        let rc = self.offset_to_rc(offset);

        NewlineIterator { rc }
    }

    /// Returns an iterator over the characters within this rope following `position`.
    pub fn chars(&self, ByteOff(pos): ByteOff) -> CharacterIterator {
        let end = self.last_offset().0;

        CharacterIterator::new(&self.rope, pos, end)
    }

    /// Returns an iterator over the characters within this rope following `position`.
    pub fn chars_until(&self, ByteOff(pos): ByteOff, ByteOff(end): ByteOff) -> CharacterIterator {
        CharacterIterator::new(&self.rope, pos, end)
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

        self.slice(so, eo, true).into()
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

impl Add for EditRope {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        let rope = self.rope + rhs.rope;

        EditRope { rope }
    }
}

impl AddAssign for EditRope {
    fn add_assign(&mut self, rhs: EditRope) {
        let rope = self.rope.slice(..) + rhs.rope;
        self.rope = rope;
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

impl PartialEq for EditRope {
    fn eq(&self, other: &Self) -> bool {
        let len = self.len();

        if len != other.len() {
            return false;
        }

        let mut scanner = xi_rope::compare::RopeScanner::new(&self.rope, &other.rope);
        return scanner.find_ne_char(0, 0, None) == len;
    }
}

impl Eq for EditRope {}

impl ToString for EditRope {
    fn to_string(&self) -> String {
        self.rope.to_string()
    }
}

impl<C: EditContext> CursorMovements<Cursor, C> for EditRope {
    fn first_word<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Cursor {
        let mut nc = cursor.clone();
        let cctx = &(&*self, 0usize, false);
        nc.first_word(cctx);

        return nc;
    }

    fn movement<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        let lastcol = get_last_column(ctx);
        let cctx = &(self, ctx.view.get_width(), lastcol);
        let mut nc = cursor.clone();

        match (movement, ctx.context.resolve(count)) {
            // buffer position movements
            (MoveType::BufferByteOffset, off) => {
                let off = ByteOff(off.saturating_sub(1));

                nc = self.offset_to_cursor(off);

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
        }

        return Some(nc);
    }

    fn range<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        range: &RangeType,
        inclusive: bool,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
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

                self.find_bracketed(&cursor, *left, *right, inclusive, count)
            },
            (RangeType::Quote(quote), _) => self.find_quoted(&cursor, *quote, inclusive),
            (RangeType::XmlTag, _) => {
                // XXX: implement
                None
            },
        }
    }

    /**
     * Given an action's movement, calculate the affected range base on the cursor's current
     * position.
     */
    fn range_of_movement<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
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
            let _ = move_cursor(&mut haystack, &dir);
        }

        // Count occurrences of the character until we reach our goal.
        while count > 0 {
            match move_cursor(&mut haystack, &dir) {
                None => return None,
                Some('\n') if !multiline => return None,
                Some(c) => {
                    if needle == c {
                        count -= 1;
                        nc = cursor_from_rc(&haystack);
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

    fn find_regex(
        &self,
        cursor: &Cursor,
        dir: MoveDir1D,
        needle: &Regex,
        count: usize,
    ) -> Option<EditRange<Cursor>> {
        let start = self.cursor_to_offset(cursor);

        if start > self.last_offset() || self.len() == 0 {
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
    use crate::editing::base::{CursorEnd, Radix, Wrappable};
    use crate::editing::cursor::CursorState;
    use crate::env::vim::VimContext;

    macro_rules! cmctx {
        ($vwctx: expr, $vctx: expr) => {
            &CursorMovementsContext {
                action: &EditAction::Motion,
                view: &$vwctx,
                context: &$vctx,
            }
        };
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
    fn test_offset_to_cursor() {
        let rope = EditRope::from("a b c\nd e f\ng h i\n");

        assert_eq!(rope.offset_to_cursor(ByteOff(0)), Cursor::new(0, 0));
        assert_eq!(rope.offset_to_cursor(ByteOff(2)), Cursor::new(0, 2));
        assert_eq!(rope.offset_to_cursor(ByteOff(5)), Cursor::new(0, 5));

        assert_eq!(rope.offset_to_cursor(ByteOff(6)), Cursor::new(1, 0));
        assert_eq!(rope.offset_to_cursor(ByteOff(8)), Cursor::new(1, 2));
        assert_eq!(rope.offset_to_cursor(ByteOff(11)), Cursor::new(1, 5));

        assert_eq!(rope.offset_to_cursor(ByteOff(12)), Cursor::new(2, 0));
        assert_eq!(rope.offset_to_cursor(ByteOff(14)), Cursor::new(2, 2));
        assert_eq!(rope.offset_to_cursor(ByteOff(17)), Cursor::new(2, 5));
    }

    #[test]
    fn test_line_of_offset() {
        let rope = EditRope::from("a b c\nd e f\ng h i\n");

        // All characters in first line are on line 0.
        assert_eq!(rope.line_of_offset(ByteOff(0)), 0);
        assert_eq!(rope.line_of_offset(ByteOff(1)), 0);
        assert_eq!(rope.line_of_offset(ByteOff(2)), 0);
        assert_eq!(rope.line_of_offset(ByteOff(3)), 0);
        assert_eq!(rope.line_of_offset(ByteOff(4)), 0);

        // Newline character is counted as the line that it ends.
        assert_eq!(rope.line_of_offset(ByteOff(5)), 0);

        // First character after newline is on the next line (line 1).
        assert_eq!(rope.line_of_offset(ByteOff(6)), 1);
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
        let rope = EditRope::from("a b c");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(0, 4));

        let rope = EditRope::from("a\nb\nc");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(2, 0));

        let rope = EditRope::from("a\nb\nc\n");
        assert_eq!(rope.first(), Cursor::new(0, 0));
        assert_eq!(rope.last(), Cursor::new(2, 1));
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
        let mut vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
    fn test_motion_word() {
        let rope = EditRope::from("hello world\na,b,c,d e,f,g,h\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        vctx.persist.insert = Some(InsertStyle::Insert);

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
        let mut vctx: VimContext = VimContext::default();
        let mut cursor = rope.first();
        let count = Count::Contextual;

        vctx.persist.insert = Some(InsertStyle::Insert);

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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();

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
        let vctx: VimContext = VimContext::default();

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
        let vctx: VimContext = VimContext::default();

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
        let vctx: VimContext = VimContext::default();

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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        let mut vctx: VimContext = VimContext::default();
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
        let vctx: VimContext = VimContext::default();
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
        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 1)).0, 2);
        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 2)).0, 3);
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
