use std::cmp::Ordering;

use crate::{
    editing::base::{CursorMergeStyle, MoveDir1D, TargetShape},
    editing::cursor::{Adjustable, Cursor, CursorAdjustment},
    editing::rope::EditRope,
    util::sort2,
};

/// A selection is an extendable range of text within a buffer.
pub type Selection = (Cursor, Cursor, TargetShape);

/// Multiple extendable ranges within a buffer.
pub type Selections = Vec<Selection>;

/// Current position and selection state for a member of a cursor group.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CursorState {
    /// Current position of a cursor.
    Location(Cursor),

    /// The current position of a cursor, and the anchor and shape of its selection.
    Selection(Cursor, Cursor, TargetShape),
}

impl CursorState {
    /// Get a reference to the anchor of a selection.
    pub fn anchor(&self) -> &Cursor {
        match self {
            CursorState::Location(cursor) => cursor,
            CursorState::Selection(_, anchor, _) => anchor,
        }
    }

    /// Get a reference to the cursor.
    pub fn cursor(&self) -> &Cursor {
        match self {
            CursorState::Location(cursor) => cursor,
            CursorState::Selection(cursor, _, _) => cursor,
        }
    }

    /// Get the shape of the selection.
    pub fn shape(&self) -> TargetShape {
        match self {
            CursorState::Location(_) => TargetShape::CharWise,
            CursorState::Selection(_, _, shape) => *shape,
        }
    }

    /// Convert this to a [CursorState::Location] if it isn't already.
    pub fn unselect(&mut self) {
        match self {
            CursorState::Location(_) => return,
            CursorState::Selection(cursor, _, _) => {
                *self = CursorState::Location(cursor.clone());
            },
        }
    }

    /// Indicates if this is a [CursorState::Selection].
    pub fn is_selection(&self) -> bool {
        matches!(self, CursorState::Selection(..))
    }

    /// Set fields in this [CursorState] using those in another.
    pub fn set(&mut self, update: CursorState) {
        match self {
            CursorState::Location(_) => *self = update,
            CursorState::Selection(ref mut cursor, ref mut anchor, ref mut shape) => {
                match update {
                    CursorState::Location(c) => *cursor = c,
                    CursorState::Selection(c, a, s) => {
                        *cursor = c;
                        *anchor = a;
                        *shape = s;
                    },
                }
            },
        }
    }

    /// Set the cursor.
    pub fn set_cursor(&mut self, new: Cursor) {
        match self {
            CursorState::Location(ref mut cursor) => *cursor = new,
            CursorState::Selection(ref mut cursor, _, _) => *cursor = new,
        }
    }

    /// Set the anchor for the selection.
    ///
    /// The value will be converted to a [CursorState::Selection] if needed.
    pub fn set_anchor(&mut self, anchor: Cursor) {
        match self {
            CursorState::Location(cursor) => {
                *self = CursorState::Selection(cursor.clone(), anchor, TargetShape::CharWise);
            },
            CursorState::Selection(_, ref mut a, _) => {
                *a = anchor;
            },
        }
    }

    /// Set the shape of the selection.
    ///
    /// The value will be converted to a [CursorState::Selection] if needed.
    pub fn set_shape(&mut self, shape: TargetShape) {
        match self {
            CursorState::Location(cursor) => {
                *self = CursorState::Selection(cursor.clone(), cursor.clone(), shape);
            },
            CursorState::Selection(_, _, ref mut s) => {
                *s = shape;
            },
        }
    }

    /// Return a sorted pair of the cursor and anchor.
    pub fn sorted(&self) -> (Cursor, Cursor) {
        match self {
            CursorState::Location(cursor) => (cursor.clone(), cursor.clone()),
            CursorState::Selection(cursor, anchor, _) => sort2(cursor.clone(), anchor.clone()),
        }
    }

    /// Swap the cursor and anchor.
    pub fn swap(&mut self) {
        if let CursorState::Selection(ref mut cursor, ref mut anchor, _) = self {
            std::mem::swap(cursor, anchor);
        }
    }

    /// If this is a [CursorState::Selection], return it as a [Selection].
    pub fn to_selection(&self) -> Option<Selection> {
        match self {
            CursorState::Location(_) => None,
            CursorState::Selection(cursor, anchor, shape) => {
                let (sels, sele) = sort2(cursor.clone(), anchor.clone());

                Some((sels, sele, *shape))
            },
        }
    }

    /// Return a triple of cursor, anchor, and shape.
    pub fn to_triple(&self) -> (Cursor, Cursor, TargetShape) {
        match self {
            CursorState::Location(cursor) => {
                (cursor.clone(), cursor.clone(), TargetShape::CharWise)
            },
            CursorState::Selection(cursor, anchor, shape) => {
                (cursor.clone(), anchor.clone(), *shape)
            },
        }
    }

    /// The number of characters contained by the selection.
    pub fn len(&self, rope: &EditRope) -> usize {
        match self {
            CursorState::Location(_) => 1,
            CursorState::Selection(ref cursor, ref anchor, _) => {
                let (lc, rc) = sort2(cursor, anchor);
                let loff = rope.cursor_to_offset(lc);
                let roff = rope.cursor_to_offset(rc);

                rope.chars_until(loff, roff).count()
            },
        }
    }

    /// Indicates the direction of the cursor relatie to the anchor.
    pub fn direction(&self) -> MoveDir1D {
        match self {
            CursorState::Location(_) => MoveDir1D::Next,
            CursorState::Selection(cursor, anchor, _) => {
                if cursor < anchor {
                    MoveDir1D::Previous
                } else {
                    MoveDir1D::Next
                }
            },
        }
    }

    /// Returns the cursor or the anchor, depending on which comes earlier in the text.
    pub fn start(&self) -> &Cursor {
        match self {
            CursorState::Location(cursor) => cursor,
            CursorState::Selection(cursor, anchor, _) => sort2(&cursor, &anchor).0,
        }
    }

    /// Returns the cursor or the anchor, depending on which comes later in the text.
    pub fn end(&self) -> &Cursor {
        match self {
            CursorState::Location(cursor) => cursor,
            CursorState::Selection(cursor, anchor, _) => sort2(&cursor, &anchor).1,
        }
    }

    /// Returns the intersection of this selection with another.
    pub fn overlaps(&self, other: &Self) -> bool {
        let (amin, amax) = (self.start(), self.end());
        let (bmin, bmax) = (other.start(), other.end());

        (amin <= bmin && bmin <= amax) || (bmin <= amin && amin <= bmax)
    }

    /// Returns the intersection of this selection with another.
    pub fn intersect(&self, other: &Self) -> Self {
        let direction = other.direction();
        let shape = other.shape();

        let lc = sort2(self.start(), other.start()).1;
        let rc = sort2(self.end(), other.end()).0;

        let (lc, rc) = if lc <= rc {
            (lc.clone(), rc.clone())
        } else {
            (lc.clone(), lc.clone())
        };

        match direction {
            MoveDir1D::Previous => CursorState::Selection(lc, rc, shape),
            MoveDir1D::Next => CursorState::Selection(rc, lc, shape),
        }
    }

    /// Returns the union of this selection with another.
    pub fn union(&self, other: &Self) -> Self {
        let direction = other.direction();
        let shape = other.shape();

        let lc = sort2(self.start(), other.start()).0.clone();
        let rc = sort2(self.end(), other.end()).1.clone();

        match direction {
            MoveDir1D::Previous => CursorState::Selection(lc, rc, shape),
            MoveDir1D::Next => CursorState::Selection(rc, lc, shape),
        }
    }

    /// Merge this selection with another.
    pub fn merge(&self, other: &Self, style: &CursorMergeStyle, rope: &EditRope) -> Self {
        match style {
            CursorMergeStyle::Union => {
                return self.union(other);
            },
            CursorMergeStyle::Intersect => {
                return self.intersect(other);
            },
            CursorMergeStyle::SelectLong => {
                let slen = self.len(rope);
                let olen = other.len(rope);

                if slen > olen {
                    return self.clone();
                } else {
                    return other.clone();
                }
            },
            CursorMergeStyle::SelectShort => {
                let slen = self.len(rope);
                let olen = other.len(rope);

                if slen < olen {
                    return self.clone();
                } else {
                    return other.clone();
                }
            },
            CursorMergeStyle::SelectCursor(MoveDir1D::Previous) => {
                if self.cursor() < other.cursor() {
                    return self.clone();
                } else {
                    return other.clone();
                }
            },
            CursorMergeStyle::SelectCursor(MoveDir1D::Next) => {
                if self.cursor() > other.cursor() {
                    return self.clone();
                } else {
                    return other.clone();
                }
            },
        }
    }
}

impl From<Cursor> for CursorState {
    fn from(cursor: Cursor) -> Self {
        CursorState::Location(cursor)
    }
}

impl Adjustable for CursorState {
    fn zero(&mut self) {
        match self {
            CursorState::Location(cursor) => cursor.zero(),
            CursorState::Selection(cursor, anchor, _) => {
                cursor.zero();
                anchor.zero();
            },
        }
    }

    fn adjust(&mut self, adj: &[CursorAdjustment]) {
        match self {
            CursorState::Location(cursor) => cursor.adjust(adj),
            CursorState::Selection(cursor, anchor, _) => {
                cursor.adjust(adj);
                anchor.adjust(adj);
            },
        }
    }
}

impl Default for CursorState {
    fn default() -> Self {
        CursorState::Location(Cursor::new(0, 0))
    }
}

impl PartialOrd for CursorState {
    fn partial_cmp(&self, other: &CursorState) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CursorState {
    fn cmp(&self, other: &CursorState) -> Ordering {
        match (self, other) {
            (CursorState::Location(c1), CursorState::Location(c2)) => c1.cmp(c2),
            (CursorState::Location(c1), CursorState::Selection(c2, ..)) => c1.cmp(c2),
            (CursorState::Selection(c1, ..), CursorState::Location(c2)) => c1.cmp(c2),
            (CursorState::Selection(c1, a1, _), CursorState::Selection(c2, a2, _)) => {
                c1.cmp(c2).then(a1.cmp(a2))
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_overlaps() {
        let cw = TargetShape::CharWise;
        let s1 = CursorState::Selection(Cursor::new(0, 2), Cursor::new(0, 5), cw);
        let s2 = CursorState::Selection(Cursor::new(2, 4), Cursor::new(2, 7), cw);
        let s3 = CursorState::Selection(Cursor::new(0, 0), Cursor::new(0, 3), cw);
        let s4 = CursorState::Selection(Cursor::new(0, 6), Cursor::new(2, 3), cw);
        let s5 = CursorState::Selection(Cursor::new(3, 0), Cursor::new(3, 5), cw);
        let s6 = CursorState::Selection(Cursor::new(5, 0), Cursor::new(2, 7), cw);

        // s1 only overlaps w/ s3.
        assert_eq!(s1.overlaps(&s2), false);
        assert_eq!(s1.overlaps(&s3), true);
        assert_eq!(s1.overlaps(&s4), false);
        assert_eq!(s1.overlaps(&s5), false);
        assert_eq!(s1.overlaps(&s6), false);

        // s2 only overlaps w/ s6.
        assert_eq!(s2.overlaps(&s3), false);
        assert_eq!(s2.overlaps(&s4), false);
        assert_eq!(s2.overlaps(&s5), false);
        assert_eq!(s2.overlaps(&s6), true);

        // s3 overlaps w/ nothing else.
        assert_eq!(s3.overlaps(&s4), false);
        assert_eq!(s3.overlaps(&s5), false);
        assert_eq!(s3.overlaps(&s6), false);

        // s4 overlaps w/ nothing else.
        assert_eq!(s4.overlaps(&s5), false);
        assert_eq!(s4.overlaps(&s6), false);

        // s5 overlaps w/ s6.
        assert_eq!(s5.overlaps(&s6), true);
    }

    #[test]
    fn test_merge_union() {
        let cw = TargetShape::CharWise;
        let a = CursorState::Location(Cursor::new(2, 4));
        let b = CursorState::Selection(Cursor::new(1, 75), Cursor::new(3, 0), cw);
        let c = CursorState::Selection(Cursor::new(2, 30), Cursor::new(4, 6), cw);
        let u = CursorMergeStyle::Union;
        let r = EditRope::from("");

        // Merges with self.
        assert_eq!(
            a.merge(&a, &u, &r),
            CursorState::Selection(Cursor::new(2, 4), Cursor::new(2, 4), cw)
        );
        assert_eq!(b.merge(&b, &u, &r), b);
        assert_eq!(c.merge(&c, &u, &r), c);

        // a is completely surrounded by b.
        assert_eq!(a.merge(&b, &u, &r), b);

        // a is outside of c, so the start of the selection moves left.
        assert_eq!(
            a.merge(&c, &u, &r),
            CursorState::Selection(Cursor::new(2, 4), Cursor::new(4, 6), cw)
        );

        // b and c overlap.
        assert_eq!(
            b.merge(&c, &u, &r),
            CursorState::Selection(Cursor::new(1, 75), Cursor::new(4, 6), cw)
        );
    }

    #[test]
    fn test_merge_intersect() {
        let cw = TargetShape::CharWise;
        let a = CursorState::Location(Cursor::new(2, 4));
        let b = CursorState::Selection(Cursor::new(1, 75), Cursor::new(3, 0), cw);
        let c = CursorState::Selection(Cursor::new(2, 30), Cursor::new(4, 6), cw);
        let i = CursorMergeStyle::Intersect;
        let r = EditRope::from("");

        // Merges with self.
        assert_eq!(
            a.merge(&a, &i, &r),
            CursorState::Selection(Cursor::new(2, 4), Cursor::new(2, 4), cw)
        );
        assert_eq!(b.merge(&b, &i, &r), b);
        assert_eq!(c.merge(&c, &i, &r), c);

        // a intersects b at only one point, itself.
        assert_eq!(
            a.merge(&b, &i, &r),
            CursorState::Selection(Cursor::new(2, 4), Cursor::new(2, 4), cw)
        );

        // a doesn't truly intersect with c, so we use the leftmost position of the rightmost
        // selection.
        assert_eq!(
            a.merge(&c, &i, &r),
            CursorState::Selection(Cursor::new(2, 30), Cursor::new(2, 30), cw)
        );

        // b and c overlap, so we return the intersecting range.
        assert_eq!(
            b.merge(&c, &i, &r),
            CursorState::Selection(Cursor::new(2, 30), Cursor::new(3, 0), cw)
        );
    }

    #[test]
    fn test_merge_select_long() {
        let cw = TargetShape::CharWise;
        let a = CursorState::Selection(Cursor::new(1, 0), Cursor::new(0, 0), cw);
        let b = CursorState::Selection(Cursor::new(2, 0), Cursor::new(1, 0), cw);
        let c = CursorState::Selection(Cursor::new(3, 0), Cursor::new(2, 0), cw);
        let l = CursorMergeStyle::SelectLong;
        let r = EditRope::from("12345\n\n678\n\n");

        // Merges with self.
        assert_eq!(a.merge(&a, &l, &r), a);
        assert_eq!(b.merge(&b, &l, &r), b);
        assert_eq!(c.merge(&c, &l, &r), c);

        // Selection a is longer than b and c.
        assert_eq!(a.merge(&b, &l, &r), a);
        assert_eq!(a.merge(&c, &l, &r), a);

        // Selection c is longer than b.
        assert_eq!(b.merge(&c, &l, &r), c);
    }

    #[test]
    fn test_merge_select_short() {
        let cw = TargetShape::CharWise;
        let a = CursorState::Selection(Cursor::new(1, 0), Cursor::new(0, 0), cw);
        let b = CursorState::Selection(Cursor::new(2, 0), Cursor::new(1, 0), cw);
        let c = CursorState::Selection(Cursor::new(3, 0), Cursor::new(2, 0), cw);
        let s = CursorMergeStyle::SelectShort;
        let r = EditRope::from("12345\n\n678\n\n");

        // Merges with self.
        assert_eq!(a.merge(&a, &s, &r), a);
        assert_eq!(b.merge(&b, &s, &r), b);
        assert_eq!(c.merge(&c, &s, &r), c);

        // Selection c is shorter than a.
        assert_eq!(a.merge(&c, &s, &r), c);

        // Selection b is shorter than a and c.
        assert_eq!(a.merge(&b, &s, &r), b);
        assert_eq!(b.merge(&c, &s, &r), b);
    }

    #[test]
    fn test_merge_select_cursor_prev() {
        let cw = TargetShape::CharWise;
        let a = CursorState::Location(Cursor::new(2, 4));
        let b = CursorState::Selection(Cursor::new(1, 75), Cursor::new(3, 0), cw);
        let c = CursorState::Selection(Cursor::new(2, 30), Cursor::new(4, 6), cw);
        let d = CursorState::Selection(Cursor::new(5, 0), Cursor::new(0, 0), cw);
        let p = CursorMergeStyle::SelectCursor(MoveDir1D::Previous);
        let r = EditRope::from("");

        // Merges with self.
        assert_eq!(a.merge(&a, &p, &r), a);
        assert_eq!(b.merge(&b, &p, &r), b);
        assert_eq!(c.merge(&c, &p, &r), c);
        assert_eq!(d.merge(&d, &p, &r), d);

        // a, b, and c's cursors come before d's; anchor doesn't matter.
        assert_eq!(a.merge(&d, &p, &r), a);
        assert_eq!(b.merge(&d, &p, &r), b);
        assert_eq!(c.merge(&d, &p, &r), c);

        // b's cursor comes before a's.
        assert_eq!(a.merge(&b, &p, &r), b);

        // a's cursor comes before c's.
        assert_eq!(a.merge(&c, &p, &r), a);

        // b's cursor comes before c's.
        assert_eq!(b.merge(&c, &p, &r), b);
    }

    #[test]
    fn test_merge_select_cursor_next() {
        let cw = TargetShape::CharWise;
        let a = CursorState::Location(Cursor::new(2, 4));
        let b = CursorState::Selection(Cursor::new(1, 75), Cursor::new(3, 0), cw);
        let c = CursorState::Selection(Cursor::new(2, 30), Cursor::new(4, 6), cw);
        let d = CursorState::Selection(Cursor::new(0, 0), Cursor::new(5, 0), cw);
        let n = CursorMergeStyle::SelectCursor(MoveDir1D::Next);
        let r = EditRope::from("");

        // Merges with self.
        assert_eq!(a.merge(&a, &n, &r), a);
        assert_eq!(b.merge(&b, &n, &r), b);
        assert_eq!(c.merge(&c, &n, &r), c);
        assert_eq!(d.merge(&d, &n, &r), d);

        // a, b, and c's cursors come after d's; anchor doesn't matter.
        assert_eq!(a.merge(&d, &n, &r), a);
        assert_eq!(b.merge(&d, &n, &r), b);
        assert_eq!(c.merge(&d, &n, &r), c);

        // a's cursor comes after b's.
        assert_eq!(a.merge(&b, &n, &r), a);

        // c's cursor comes after a's.
        assert_eq!(a.merge(&c, &n, &r), c);

        // c's cursor comes after b's.
        assert_eq!(b.merge(&c, &n, &r), c);
    }
}
