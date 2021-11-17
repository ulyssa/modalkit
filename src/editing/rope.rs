use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::Debug;
use std::ops::Add;
use std::ops::AddAssign;

use regex::Regex;
use xi_rope::delta::DeltaElement;
use xi_rope::diff::{Diff, LineHashDiff};
use xi_rope::rope::{BaseMetric, LinesMetric, Utf16CodeUnitsMetric};
use xi_rope::rope::{Rope, RopeInfo};
use xi_rope::tree::Cursor as RopeCursor;

use crate::editing::cursor::{Cursor, CursorAdjustment};

use crate::editing::base::{
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
    MoveType,
    RangeType,
    TargetShape,
    ViewportContext,
    WordStyle,
};

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
    fn goal(self, goal: usize) -> Cursor;
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
    fn first_word<'a>(&mut self, ctx: &CursorContext<'a>);
}

impl PrivateCursorOps for Cursor {
    fn goal(mut self, goal: usize) -> Cursor {
        self.xgoal = goal;
        self
    }

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

fn is_word_char(c: char) -> bool {
    return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_';
}

fn is_keyword(c: char) -> bool {
    return c >= '!' && c <= '/' || c >= '[' && c <= '^' || c >= '{' && c <= '~' || c == '`';
}

fn is_space_char(c: char) -> bool {
    return c.is_ascii_whitespace();
}

fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

fn is_big_word_begin(rc: &RopeCursor<'_, RopeInfo>, dir: &MoveDir1D, last: bool) -> bool {
    let off = rc.pos();
    let tlen = rc.total_len();

    if off == 0 {
        /*
         * The first character always counts as a word beginning.
         */
        return true;
    }

    if off >= tlen {
        /*
         * If we're moving towards the end of the document, the last
         * character counts as a word beginning.
         */
        return dir == &MoveDir1D::Next;
    }

    let s = rc.root().slice(off - 1..off + 1).to_string();

    let mut chars = s.chars();
    let oa = chars.next();
    let ob = chars.next();

    match (oa, ob) {
        (Some(_), None) => true,
        (Some(a), Some(b)) => {
            let aws = is_space_char(a);
            let bws = is_space_char(b);
            let bnl = is_newline(b);

            return (last && bnl) || (aws && !bws);
        },
        _ => false,
    }
}

fn is_big_word_end(rc: &RopeCursor<'_, RopeInfo>, _: &MoveDir1D, _: bool) -> bool {
    let off = rc.pos();
    let tlen = rc.total_len();

    if off == 0 || off >= tlen - 1 {
        return true;
    }

    let s = rc.root().slice(off..off + 2).to_string();

    let mut chars = s.chars();
    let oa = chars.next();
    let ob = chars.next();

    match (oa, ob) {
        (Some(a), Some(b)) => {
            let aws = is_space_char(a);
            let bws = is_space_char(b);

            return !aws && bws;
        },
        _ => false,
    }
}

fn is_word_begin(rc: &RopeCursor<'_, RopeInfo>, dir: &MoveDir1D, last: bool) -> bool {
    let off = rc.pos();
    let tlen = rc.total_len();

    if off == 0 {
        /*
         * The first character always counts as a word beginning.
         */
        return true;
    }

    if off >= tlen {
        /*
         * If we're moving towards the end of the document, the index after
         * the last character counts as a word beginning.
         */
        return dir == &MoveDir1D::Next;
    }

    let s = rc.root().slice(off - 1..off + 1).to_string();

    let mut chars = s.chars();
    let oa = chars.next();
    let ob = chars.next();

    match (oa, ob) {
        (Some(_), None) => true,
        (Some(a), Some(b)) => {
            let awc = is_word_char(a);
            let akw = is_keyword(a);
            let bwc = is_word_char(b);
            let bkw = is_keyword(b);
            let bnl = is_newline(b);

            return (last && bnl) || (awc && bkw) || (akw && bwc) || (!awc && bwc) || (!akw && bkw);
        },
        _ => false,
    }
}

fn is_word_end(rc: &RopeCursor<'_, RopeInfo>, _: &MoveDir1D, _: bool) -> bool {
    let off = rc.pos();
    let tlen = rc.total_len();

    if off == 0 || off >= tlen - 1 {
        return true;
    }

    let s = rc.root().slice(off..off + 2).to_string();

    let mut chars = s.chars();
    let oa = chars.next();
    let ob = chars.next();

    match (oa, ob) {
        (Some(a), Some(b)) => {
            let awc = is_word_char(a);
            let akw = is_keyword(a);
            let bwc = is_word_char(b);
            let bkw = is_keyword(b);

            return (awc && bkw) || (akw && bwc) || (awc && !bwc) || (akw && !bkw);
        },
        _ => false,
    }
}

pub struct CharacterIterator<'a> {
    rc: RopeCursor<'a, RopeInfo>,
    position: usize,
}

pub struct NewlineIterator<'a> {
    rc: RopeCursor<'a, RopeInfo>,
}

impl<'a> CharacterIterator<'a> {
    pub fn pos(&self) -> ByteOff {
        ByteOff(self.position)
    }
}

impl<'a> Iterator for CharacterIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.rc.peek_next_codepoint();

        self.position = self.rc.pos();
        self.rc.next::<BaseMetric>();

        return res;
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

type MatchFn = fn(&RopeCursor<'_, RopeInfo>, &MoveDir1D, bool) -> bool;

fn cursor_from_rc(rc: &RopeCursor<'_, RopeInfo>) -> Cursor {
    let off = rc.pos();
    let rope = rc.root();
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

#[derive(Clone, Debug)]
pub struct EditRope {
    rope: Rope,
}

impl EditRope {
    /// Calculate the max indexable column in a given line given the current context.
    ///
    /// This function expects to be given a valid line number as input.
    fn max_column_idx<'a>(&self, y: usize, lastcol: bool) -> usize {
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

    pub fn repeat(&self, shape: TargetShape, times: usize) -> EditRope {
        EditRope { rope: roperepeat(&self.rope, shape, times) }
    }

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

    pub fn changecase(&self, case: &Case) -> EditRope {
        let s = self.rope.to_string();
        let s = match case {
            Case::Upper => s.to_uppercase(),
            Case::Lower => s.to_lowercase(),
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
    ) -> (Cursor, Vec<CursorAdjustment>) {
        let off = self.offset_of_line(cursor.y).0;
        let tlines = text.get_lines() as isize;

        self.rope.edit(off..off, text.rope);

        let adj = CursorAdjustment::Line {
            line_start: cursor.y,
            line_end: usize::MAX,
            amount: tlines,
            amount_after: 0,
        };

        let cctx = &(&*self, 0, true);
        let mut nc = cursor.clone();
        nc.first_word(cctx);

        (nc, vec![adj])
    }

    /// Do a linewise insertion of some text below the cursor's current line. The text should
    /// already contain trailing newline.
    fn _insert_below(
        &mut self,
        cursor: &Cursor,
        text: EditRope,
    ) -> (Cursor, Vec<CursorAdjustment>) {
        let coff = self.cursor_to_offset(cursor);
        let mut rc = self.offset_to_rc(coff);
        let tlines = text.get_lines() as isize;

        match rc.next::<LinesMetric>() {
            Some(end) => {
                self.rope.edit(end..end, text.rope);
            },
            None => {
                let end = self.rope.len();
                self.rope.edit(end..end, text.rope);
                self.rope.edit(end..end, Rope::from("\n"));
            },
        }

        let lstart = self.line_of_offset(coff);
        let adj = CursorAdjustment::Line {
            line_start: lstart,
            line_end: usize::MAX,
            amount: tlines,
            amount_after: 0,
        };

        let cctx = &(&*self, 0usize, true);
        let mut nc = cursor.clone();
        nc.down(1);
        nc.first_word(cctx);

        return (nc, vec![adj]);
    }

    /// Do a blockwise insertion of some text. Lines within the block should be separated by a
    /// newline, and there should be no trailing newline.
    fn _insert_block(
        &mut self,
        cursor: &Cursor,
        off: usize,
        text: EditRope,
    ) -> (Cursor, Vec<CursorAdjustment>) {
        let colmax = self.max_column_idx(cursor.y, true);
        let cstart = cursor.x.saturating_add(off).min(colmax);
        let coff = self.lincol_to_offset(cursor.y, cstart);
        let nc = self.offset_to_cursor(coff);

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

            adjs.push(CursorAdjustment::Column {
                line: c.y,
                column_start: cstart,
                amt_line: 0,
                amt_col: line.len() as isize,
            });

            self.rope.edit(ioff..ioff, line);

            let cctx = &(&*self, 0, false);
            c.line(MoveDir1D::Next, 1, cctx);
        }

        if ilines > alines {
            let start = text.offset_of_line(alines).0;
            let append = text.rope.slice(start..) + Rope::from("\n");
            let len = self.rope.len();
            self.rope.edit(len..len, append);
        }

        return (nc, adjs);
    }

    fn _insert(
        &mut self,
        cursor: &Cursor,
        off: usize,
        co: usize,
        text: EditRope,
        style: InsertStyle,
    ) -> (Cursor, Vec<CursorAdjustment>) {
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

        let noff = self.offset_to_u16(insend);
        let noff = noff.0.saturating_sub(1).saturating_add(co);
        let noff = self.u16_to_offset(U16Off(noff));

        return (self.offset_to_cursor(noff), adjs);
    }

    pub fn insert(
        &mut self,
        cursor: &Cursor,
        dir: MoveDir1D,
        text: EditRope,
        style: InsertStyle,
    ) -> (Cursor, Vec<CursorAdjustment>) {
        match dir {
            MoveDir1D::Previous => {
                return self._insert(cursor, 0, 1, text, style);
            },
            MoveDir1D::Next => {
                return self._insert(cursor, 1, 1, text, style);
            },
        }
    }

    pub fn paste(
        &mut self,
        cursor: &Cursor,
        dir: MoveDir1D,
        text: EditRope,
        shape: TargetShape,
    ) -> (Cursor, Vec<CursorAdjustment>) {
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

    pub fn get_char_at(&self, cursor: &Cursor) -> Option<char> {
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

    pub fn len_offset(&self) -> ByteOff {
        ByteOff(self.len())
    }

    pub fn len(&self) -> usize {
        self.rope.len()
    }

    pub fn get_lines(&self) -> usize {
        self.rope.measure::<LinesMetric>()
    }

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

    pub fn lines(&self, line: usize) -> xi_rope::rope::Lines {
        let off = self.offset_of_line(line).0;

        self.rope.lines(off..)
    }

    pub fn lines_at(&self, line: usize, column: usize) -> xi_rope::rope::Lines {
        let off = self.lincol_to_offset(line, column).0;

        self.rope.lines(off..)
    }

    pub fn line_of_offset(&self, off: ByteOff) -> usize {
        self.rope.line_of_offset(off.0)
    }

    pub fn offset_of_line(&self, line: usize) -> ByteOff {
        ByteOff(self.rope.offset_of_line(line))
    }

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

    pub fn cursor_to_offset(&self, cursor: &Cursor) -> ByteOff {
        self.lincol_to_offset(cursor.y, cursor.x)
    }

    pub fn first(&self) -> Cursor {
        Cursor::new(0, 0)
    }

    pub fn last_offset(&self) -> ByteOff {
        ByteOff(self.rope.len().saturating_sub(1))
    }

    pub fn last(&self) -> Cursor {
        self.offset_to_cursor(self.last_offset())
    }

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

                    let lline = self.line_of_offset(last);
                    let sline = self.line_of_offset(start);

                    if lline == sline {
                        let lu16 = self.offset_to_u16(last);
                        let su16 = self.offset_to_u16(start);

                        adjs.push(CursorAdjustment::Column {
                            line: lline + inserted,
                            column_start: lu16.0,
                            amt_line: 0,
                            amt_col: -(su16.0 as isize - lu16.0 as isize),
                        });
                    } else {
                        let clast = self.offset_to_cursor(last);
                        adjs.push(CursorAdjustment::Column {
                            line: clast.y + inserted,
                            column_start: clast.x,
                            amt_line: 0,
                            amt_col: isize::MIN,
                        });

                        let dlines = sline - lline;
                        adjs.push(CursorAdjustment::Line {
                            line_start: lline + inserted,
                            line_end: sline + inserted,
                            amount: isize::MAX,
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

        return adjs;
    }

    fn find_word(
        &self,
        nc: &Cursor,
        dir: MoveDir1D,
        matches: MatchFn,
        mut count: usize,
        lastcount: bool,
    ) -> Option<Cursor> {
        let off = self.cursor_to_offset(&nc);
        let mut rc = self.offset_to_rc(off);

        while count > 0 {
            /*
             * Some movements, like WordBegin and BigWordBegin, end in a different location when
             * performing an operation. We pass a flag to the matches function when we've reached
             * the final count, and are not doing a Motion.
             */
            let lastcount = count == 1 && lastcount;

            match move_cursor(&mut rc, &dir) {
                None => {
                    break;
                },
                Some(_) => {
                    if matches(&rc, &dir, lastcount) {
                        count -= 1;
                    }
                },
            }
        }

        /*
         * Word movements always move, even if they can't do a full count.
         */
        Some(cursor_from_rc(&rc))
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

    pub fn newlines(&self, offset: ByteOff) -> NewlineIterator {
        let rc = self.offset_to_rc(offset);

        NewlineIterator { rc }
    }

    pub fn chars(&self, ByteOff(position): ByteOff) -> CharacterIterator {
        let rc = RopeCursor::new(&self.rope, position);

        CharacterIterator { rc, position }
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
        let lastcol = ctx.context.get_insert_style().is_some();
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
            (MoveType::FirstWord(dir), count) => {
                nc.line(*dir, count, cctx);
                nc.first_word(cctx);
            },
            (MoveType::WordBegin(WordStyle::Little, dir), count) => {
                return self.find_word(&nc, *dir, is_word_begin, count, !ctx.action.is_motion());
            },
            (MoveType::WordEnd(WordStyle::Little, dir), count) => {
                return self.find_word(&nc, *dir, is_word_end, count, !ctx.action.is_motion());
            },
            (MoveType::WordBegin(WordStyle::Big, dir), count) => {
                return self.find_word(
                    &nc,
                    *dir,
                    is_big_word_begin,
                    count,
                    !ctx.action.is_motion(),
                );
            },
            (MoveType::WordEnd(WordStyle::Big, dir), count) => {
                return self.find_word(&nc, *dir, is_big_word_end, count, !ctx.action.is_motion());
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
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<EditRange<Cursor>> {
        match (range, count) {
            (RangeType::Buffer, _) => {
                let start = self.first();
                let end = self.last();
                let range = EditRange::inclusive(start, end, TargetShape::LineWise);

                Some(range)
            },
            (RangeType::Line, count) => {
                let style = ctx.context.get_insert_style();
                let cctx = &(self, ctx.view.get_width(), style.is_some());
                let count = ctx.context.resolve(count).saturating_sub(1);

                let start = cursor.clone();
                let mut end = cursor.clone();

                // Place end cursor on the first word count lines away
                end.line(MoveDir1D::Next, count, cctx);
                end.first_word(cctx);

                EditRange::new(start, end, TargetShape::LineWise, false).into()
            },
            (RangeType::Word(_), _) => {
                return None;
            },
            (RangeType::Paragraph, _) => {
                return None;
            },
            (RangeType::Sentence, _) => {
                return None;
            },
            (RangeType::Bracketed(left, right, inclusive), count) => {
                let count = ctx.context.resolve(count);

                self.find_bracketed(&cursor, *left, *right, *inclusive, count)
            },
            (RangeType::Quote(quote, inclusive), _) => {
                self.find_quoted(&cursor, *quote, *inclusive)
            },
            (RangeType::XmlTag(_), _) => {
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
        _cursor: &Cursor,
        _dir: MoveDir1D,
        _needle: Regex,
        _count: usize,
    ) -> Option<Cursor> {
        unimplemented!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::base::{EditAction, Wrappable};
    use crate::vim::VimContext;

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
    fn test_chars_iter() {
        let r = EditRope::from("hello\nworld\n");
        let mut iter = r.chars(0.into());

        assert_eq!(iter.next(), Some('h'));
        assert_eq!(iter.next(), Some('e'));
        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.next(), Some('o'));
        assert_eq!(iter.next(), Some('\n'));
        assert_eq!(iter.next(), Some('w'));
        assert_eq!(iter.next(), Some('o'));
        assert_eq!(iter.next(), Some('r'));
        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.next(), Some('d'));
        assert_eq!(iter.next(), Some('\n'));
        assert_eq!(iter.next(), None);

        let r = EditRope::from("foo bar baz\n");
        let mut iter = r.chars(4.into());

        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), Some('r'));
        assert_eq!(iter.next(), Some(' '));
        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), Some('z'));
        assert_eq!(iter.next(), Some('\n'));
        assert_eq!(iter.next(), None);
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
        let mut vctx = VimContext::default();
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
        let mut vctx = VimContext::default();
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
        let vctx = VimContext::default();
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

        // "B"
        let mov = MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous);

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 0));

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 6));

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

        // "b"
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        for x in 0..=6 {
            cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
            assert_eq!(cursor, Cursor::new(1, 6 - x));
        }

        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 6));

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
    }

    #[test]
    fn test_first_word() {
        let rope = EditRope::from("       hello world\n  a b c d e\n    first\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = VimContext::default();
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
        let mut vctx = VimContext::default();
        let mut cursor = rope.first();

        assert_eq!(cursor, Cursor::new(0, 0));

        let mov = MoveType::LinePos(MovePosition::End);
        let count = Count::MinusOne;

        // Move to end of line.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));

        // Repeating stays at end of line because of Count::MinusOne.
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(0, 9));

        // Using a higher context count allows us to move to the next line ending.
        vctx.action.count = Some(2);
        cursor = rope.movement(&cursor, &mov, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(cursor, Cursor::new(1, 4));

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
        let vctx = VimContext::default();
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
        let vctx = VimContext::default();
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
        let vctx = VimContext::default();
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
        let vctx = VimContext::default();
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
        let vctx = VimContext::default();
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
        let vctx = VimContext::default();
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
        let mut vctx = VimContext::default();
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
        let vctx = VimContext::default();

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
        let vctx = VimContext::default();

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
        let vctx = VimContext::default();

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
        let vctx = VimContext::default();

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
    fn test_range_line() {
        let rope = EditRope::from("1 2 3\nhello world\n    foo bar\na b c d e f\n");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = VimContext::default();
        let cw = TargetShape::LineWise;
        let count = Count::Contextual;
        let rt = RangeType::Line;
        let cursor = Cursor::new(1, 6);

        // Select a single line.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 0), Cursor::new(1, 6), cw));

        // End cursor is placed on the first word (important for forced-motion compatibility).
        vctx.action.count = Some(2);
        let er = rope.range(&cursor, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 6), Cursor::new(2, 4), cw));

        // Select up to the last line.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 6), Cursor::new(3, 0), cw));

        // Providing a count higher than number of lines stops at the last line.
        vctx.action.count = Some(4);
        let er = rope.range(&cursor, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::exclusive(Cursor::new(1, 6), Cursor::new(3, 0), cw));
    }

    #[test]
    fn test_range_bracketed_start_at_paren() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = VimContext::default();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')', true);

        let cursor_al = Cursor::new(0, 9);
        let cursor_ar = Cursor::new(0, 11);
        let cursor_cl = Cursor::new(0, 25);
        let cursor_cr = Cursor::new(0, 27);

        // Select parentheses surrounding "a" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_al, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 9), Cursor::new(0, 11), cw));

        vctx.action.count = Some(1);
        let er = rope.range(&cursor_ar, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 9), Cursor::new(0, 11), cw));

        // Select parentheses surrounding "a" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_al, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        vctx.action.count = Some(3);
        let er = rope.range(&cursor_ar, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Select the parentheses surrounding "c" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_cl, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 25), Cursor::new(0, 27), cw));

        vctx.action.count = Some(1);
        let er = rope.range(&cursor_cr, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 25), Cursor::new(0, 27), cw));

        // Look for the parentheses surrounding "c" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_cl, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        vctx.action.count = Some(3);
        let er = rope.range(&cursor_cr, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));
    }

    #[test]
    fn test_range_bracketed_forward() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = VimContext::default();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')', true);

        // These starting positions are before the quotes.
        let cursor_1 = Cursor::new(0, 5);
        let cursor_a = Cursor::new(0, 10);

        // Look for the parentheses surrounding "1".
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_1, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "a" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_a, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 9), Cursor::new(0, 11), cw));

        // Look for the parentheses surrounding "a" w/ count = 2.
        vctx.action.count = Some(2);
        let er = rope.range(&cursor_a, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 29), cw));

        // Look for the parentheses surrounding "a" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_a, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "a" w/ count = 4.
        vctx.action.count = Some(4);
        let er = rope.range(&cursor_a, &rt, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_bracketed_backward() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let mut vctx = VimContext::default();
        let cw = TargetShape::CharWise;
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')', true);

        // These starting positions are after the quotes.
        let cursor_2 = Cursor::new(0, 31);
        let cursor_c = Cursor::new(0, 26);

        // Look for the parentheses surrounding "2".
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_2, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "c" w/ count = 1.
        vctx.action.count = Some(1);
        let er = rope.range(&cursor_c, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 25), Cursor::new(0, 27), cw));

        // Look for the parentheses surrounding "c" w/ count = 2.
        vctx.action.count = Some(2);
        let er = rope.range(&cursor_c, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 7), Cursor::new(0, 29), cw));

        // Look for the parentheses surrounding "c" w/ count = 3.
        vctx.action.count = Some(3);
        let er = rope.range(&cursor_c, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 4), Cursor::new(0, 34), cw));

        // Look for the parentheses surrounding "c" w/ count = 4.
        vctx.action.count = Some(4);
        let er = rope.range(&cursor_c, &rt, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_bracketed_no_surrounding_parens() {
        let rope = EditRope::from("foo (1 ( (a) \")\" (b) ')' (c) ) 2 3) bar");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = VimContext::default();
        let count = Count::Contextual;
        let rt = RangeType::Bracketed('(', ')', true);

        // Left side of paren group.
        let cursor_l = Cursor::new(0, 3);
        let er = rope.range(&cursor_l, &rt, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);

        // Right side of paren group.
        let cursor_r = Cursor::new(0, 35);
        let er = rope.range(&cursor_r, &rt, &count, cmctx!(vwctx, vctx));
        assert_eq!(er, None);
    }

    #[test]
    fn test_range_quoted() {
        let rope = EditRope::from("a b c 'd e f \\'g h i\\' j k' l m n 'o p' q");
        let vwctx = ViewportContext::<Cursor>::default();
        let vctx = VimContext::default();
        let count = Count::Contextual;
        let rt = RangeType::Quote('\'', true);
        let cw = TargetShape::CharWise;

        // Start before escaped single quotes.
        let cursor = Cursor::new(0, 7);
        let er = rope.range(&cursor, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 6), Cursor::new(0, 26), cw));

        // Start inside escaped single quotes.
        let cursor = Cursor::new(0, 17);
        let er = rope.range(&cursor, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 6), Cursor::new(0, 26), cw));

        // Starting from "m" we get (0, 26) and (0, 34), even if we might not consider those
        // a pair when scanning from the start of the line.
        let cursor = Cursor::new(0, 30);
        let er = rope.range(&cursor, &rt, &count, cmctx!(vwctx, vctx)).unwrap();
        assert_eq!(er, EditRange::inclusive(Cursor::new(0, 26), Cursor::new(0, 34), cw));
    }

    #[test]
    fn test_len() {
        let rope = EditRope::from("\u{00AB}a\u{00BB}\n");

        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 0)).0, 0);
        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 1)).0, 2);
        assert_eq!(rope.cursor_to_offset(&Cursor::new(0, 2)).0, 3);
    }
}
