//! # Buffer cursors
//!
//! ## Overview
//!
//! This module contains the types and logic for representing cursors, selections, cursor groups
//! and manipulating them within a buffer.
//!
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::VecDeque;

use crate::util::sort2;

use super::base::Wrappable;

mod choice;
mod group;
mod state;

pub use choice::CursorChoice;
pub use group::{CursorGroup, CursorGroupCombineError, CursorGroupIter, CursorGroupIterMut};
pub use state::{CursorState, Selection, Selections};

/// Represents a movable point within a document.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Cursor {
    pub(crate) xgoal: usize,
    pub(crate) x: usize,
    pub(crate) y: usize,
}

/// Represents changes to make to cursors after a document modification.
#[derive(Debug, Eq, PartialEq)]
pub enum CursorAdjustment {
    /// Adjust cursors located on a specific line.
    Column {
        /// The line whose cursors are being modified.
        line: usize,

        /// The starting column within the line.
        column_start: usize,

        /// The amount to adjust the line of cursors by.
        amt_line: isize,

        /// The amount to adjust the column of cursors by.
        amt_col: isize,
    },

    /// Adjust cursors starting at a given line.
    Line {
        /// The line at which to begin adjusting cursors.
        line_start: usize,

        /// The line at which to stop adjusting cursors (inclusive).
        line_end: usize,

        /// The amount by which to adjust the line of cursors within the range.
        amount: isize,

        /// The amount by which to adjust the line of cursors after `line_end`.
        amount_after: isize,
    },
}

impl Cursor {
    /// Create a new cursor.
    pub fn new(line: usize, column: usize) -> Self {
        Cursor { xgoal: column, x: column, y: line }
    }

    /// Get the line that this cursor is on.
    pub fn get_y(&self) -> usize {
        self.y
    }

    /// Get the column that this is on.
    pub fn get_x(&self) -> usize {
        self.x
    }

    pub(crate) fn goal(mut self, goal: usize) -> Cursor {
        self.xgoal = goal;
        self
    }

    /// Set the column for this cursor.
    pub fn set_x(&mut self, x: usize) {
        self.x = x;
        self.xgoal = x;
    }

    /// Set the line for this cursor.
    pub fn set_y(&mut self, y: usize) {
        self.y = y;
    }

    /// Move this cursor to the left by offset columns.
    pub fn left(&mut self, off: usize) {
        self.x = self.x.saturating_sub(off);
        self.xgoal = self.x;
    }

    /// Move this cursor to the right by offset columns.
    pub fn right(&mut self, off: usize) {
        self.x = self.x.saturating_add(off);
        self.xgoal = self.x;
    }

    /// Move this cursor down by offset lines.
    pub fn down(&mut self, off: usize) {
        self.y = self.y.saturating_add(off);
    }

    /// Move this cursor up by offset lines.
    pub fn up(&mut self, off: usize) {
        self.y = self.y.saturating_sub(off);
    }

    fn adjust_x(&mut self, off: isize) {
        let abs = off.unsigned_abs();

        if off < 0 {
            self.left(abs);
        } else {
            self.right(abs);
        }
    }

    fn adjust_y(&mut self, off: isize) {
        let abs = off.unsigned_abs();

        if off < 0 {
            self.up(abs);
        } else {
            self.down(abs);
        }
    }

    fn adjust1(&mut self, adj: &CursorAdjustment) {
        match adj {
            CursorAdjustment::Line { line_start, line_end, amount, amount_after } => {
                if self.y >= *line_start && self.y <= *line_end {
                    if *amount == isize::MAX {
                        self.zero();
                    } else {
                        self.adjust_y(*amount);
                    }
                } else if *amount_after != 0 && self.y > *line_end {
                    self.adjust_y(*amount_after);
                }
            },
            CursorAdjustment::Column { line, column_start, amt_line, amt_col } => {
                if self.y == *line && self.x >= *column_start {
                    self.adjust_y(*amt_line);
                    self.adjust_x(*amt_col);
                }
            },
        }
    }

    fn compare(&self, other: &Cursor) -> Ordering {
        let ycmp = self.y.cmp(&other.y);

        if ycmp != Ordering::Equal {
            return ycmp;
        }

        let xcmp = self.x.cmp(&other.x);

        if xcmp != Ordering::Equal {
            return xcmp;
        }

        self.xgoal.cmp(&other.xgoal)
    }
}

/// Trait for adjusting cursors and objects that contain cursors.
pub trait Adjustable {
    /// Zero out the line and column of any contained cursors.
    fn zero(&mut self);

    /// Apply a [CursorAdjustment] to any applicable cursors.
    fn adjust(&mut self, adj: &[CursorAdjustment]);
}

impl<T> Adjustable for Vec<T>
where
    T: Adjustable,
{
    fn zero(&mut self) {
        for item in self.iter_mut() {
            item.zero();
        }
    }

    fn adjust(&mut self, adj: &[CursorAdjustment]) {
        for item in self.iter_mut() {
            item.adjust(adj);
        }
    }
}

impl<T> Adjustable for VecDeque<T>
where
    T: Adjustable,
{
    fn zero(&mut self) {
        for item in self.iter_mut() {
            item.zero();
        }
    }

    fn adjust(&mut self, adj: &[CursorAdjustment]) {
        for item in self.iter_mut() {
            item.adjust(adj);
        }
    }
}

impl Adjustable for Cursor {
    /// Zero out this cursor's line and column.
    fn zero(&mut self) {
        self.xgoal = 0;
        self.x = 0;
        self.y = 0;
    }

    fn adjust(&mut self, adjs: &[CursorAdjustment]) {
        for adj in adjs {
            self.adjust1(adj);
        }
    }
}

impl Wrappable for Cursor {
    fn set_wrap(&mut self, wrap: bool) {
        if wrap {
            self.set_x(0);
        }
    }
}

impl PartialOrd for Cursor {
    fn partial_cmp(&self, other: &Cursor) -> Option<Ordering> {
        Some(self.compare(other))
    }
}

impl Ord for Cursor {
    fn cmp(&self, other: &Cursor) -> Ordering {
        self.compare(other)
    }
}

/// Treat two cursors as describing a block of text, and return the upper left cursor, and bottom
/// cursor for the block.
pub(crate) fn block_cursors(a: &Cursor, b: &Cursor) -> (Cursor, Cursor) {
    let (lstart, lend) = sort2(a.y, b.y);

    let lcol = a.x.min(b.x);
    let rcol = a.x.max(b.x);
    let rgoal = a.xgoal.max(b.xgoal);

    (Cursor::new(lstart, lcol), Cursor::new(lend, rcol).goal(rgoal))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cursor_cmp() {
        let c1 = Cursor::new(7, 6);
        let c2 = Cursor::new(7, 10);
        let c3 = Cursor::new(10, 0);

        assert_eq!(c1.cmp(&c1), Ordering::Equal);
        assert_eq!(c2.cmp(&c2), Ordering::Equal);
        assert_eq!(c3.cmp(&c3), Ordering::Equal);

        assert_eq!(c1.cmp(&c2), Ordering::Less);
        assert_eq!(c1.cmp(&c3), Ordering::Less);

        assert_eq!(c2.cmp(&c1), Ordering::Greater);
        assert_eq!(c2.cmp(&c3), Ordering::Less);

        assert_eq!(c3.cmp(&c1), Ordering::Greater);
        assert_eq!(c3.cmp(&c2), Ordering::Greater);
    }

    #[test]
    fn test_cursor_getters() {
        let c1 = Cursor::new(5, 6);
        let c2 = Cursor::new(0, 1000);

        assert_eq!(c1.get_y(), 5);
        assert_eq!(c1.get_x(), 6);
        assert_eq!(c2.get_y(), 0);
        assert_eq!(c2.get_x(), 1000);
    }
}
