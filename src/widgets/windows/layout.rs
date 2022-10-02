//! # Window Layouts
//!
//! ## Overview
//!
//! Windows are placed in subdivided rows and columns, and by default equally sized according to
//! how many windows are in the widest portion of the layout, and how many windows are in the
//! tallest portion of the layout.
use std::cmp::Ordering;
use std::convert::TryInto;
use std::marker::PhantomData;
use std::ops::Not;

use tui::{buffer::Buffer, layout::Rect, widgets::StatefulWidget};

use crate::widgets::util::{rect_down, rect_right, rect_zero_height, rect_zero_width};
use crate::widgets::{TermOffset, Window, WindowContainer};

use super::size::{ResizeInfo, ResizeInfoTrail, MIN_WIN_LEN};
use super::tree::{SubtreeOps, TreeOps};
use super::{
    winnr_cmp,
    AxisT,
    AxisTree,
    AxisTreeNode,
    HorizontalT,
    TreeInfo,
    Value,
    VerticalT,
    WindowActions,
};

use crate::util::idx_offset;

use crate::editing::action::{EditResult, UIResult, WindowAction};

use crate::editing::base::{
    Axis,
    Axis::{Horizontal, Vertical},
    CloseFlags,
    CloseTarget,
    Count,
    EditContext,
    FocusChange,
    MoveDir1D,
    MoveDir2D,
    MoveDir2D::{Down, Left, Right, Up},
    MovePosition,
    SizeChange,
};

fn windex<C: EditContext>(count: &Count, ctx: &C) -> usize {
    ctx.resolve(count).saturating_sub(1)
}

fn slopped_length(base: u16, count: u16, slop: &mut u16) -> u16 {
    let slopped = count.min(*slop);
    let length = base * count + slopped;

    *slop -= slopped;

    return length;
}

fn set_area_lens<W, X, Y>(node: &mut AxisTreeNode<W, X, Y>, area: Rect, info: &ResizeInfo)
where
    W: Window,
    X: AxisT,
    Y: AxisT,
{
    if let Some(ref lengths) = info.lengths {
        assert_eq!(lengths.len(), node.weight());

        match X::axis() {
            Axis::Horizontal => {
                let mut carea = rect_zero_height(area);
                let mut iter = lengths.iter();
                let mut rem = area.height;

                let mut f = |value: &mut Value<W, X, Y>| {
                    let size = iter.next().unwrap();
                    let height = size.length.min(rem);

                    carea = rect_down(carea, height);
                    rem -= height;

                    value.set_area(carea, info);
                };

                node.for_each_value(&mut f);
            },
            Axis::Vertical => {
                let mut carea = rect_zero_width(area);
                let mut iter = lengths.iter();
                let mut rem = area.width;

                let mut f = |value: &mut Value<W, X, Y>| {
                    let size = iter.next().unwrap();
                    let width = size.length.min(rem);

                    carea = rect_right(carea, width);
                    rem -= width;

                    value.set_area(carea, info);
                };

                node.for_each_value(&mut f);
            },
        }
    }
}

fn set_area_equal<W, X, Y>(node: &mut AxisTreeNode<W, X, Y>, area: Rect, info: &ResizeInfo)
where
    W: Window,
    X: AxisT,
    Y: AxisT,
{
    let (lw, lh) = node.dimensions();
    let (lw, lh) = (lw as u16, lh as u16);

    match X::axis() {
        Axis::Horizontal => {
            let height = area.height / lh;

            let mut slop = area.height % lh;
            let mut carea = rect_zero_height(area);

            let mut f = |value: &mut Value<W, X, Y>| {
                let (_, h) = value.dimensions();
                let height = slopped_length(height, h as u16, &mut slop);

                carea = rect_down(carea, height);

                value.set_area(carea, info);
            };

            node.for_each_value(&mut f);
        },
        Axis::Vertical => {
            let width = area.width / lw;

            let mut slop = area.width % lw;
            let mut carea = rect_zero_width(area);

            let mut f = |value: &mut Value<W, X, Y>| {
                let (w, _) = value.dimensions();
                let width = slopped_length(width, w as u16, &mut slop);

                carea = rect_right(carea, width);

                value.set_area(carea, info);
            };

            node.for_each_value(&mut f);
        },
    }
}

/// These operations are for manipulating the [Windows](Window) contained within the current tree
/// and all of the trees it contains along the rotated axis. The indices here are for all of the
/// reachable windows within this view of the layout, and not for the [Values](Value) within the
/// tree.
pub(super) trait LayoutOps<W, X, Y>
where
    W: Window,
    X: AxisT,
    Y: AxisT,
{
    /// Return how many [Windows](Window) this tree contains.
    fn size(&self) -> usize;

    /// Return the widest and tallest [Window] count in this tree.
    fn dimensions(&self) -> (usize, usize);

    /// Return the widest [Window] count in this tree.
    fn width(&self) -> usize {
        self.dimensions().0
    }

    /// Return the tallest [Window] count in this tree.
    fn height(&self) -> usize {
        self.dimensions().1
    }

    /// Remove the [Window] at the given index.
    fn close(&mut self, at: usize, trail: Box<ResizeInfoTrail<'_, X, Y>>) -> Option<W>;

    /// Collapse this tree to just the [Window] located at the given index.
    fn collapse(self, at: usize) -> Option<W>;

    /// Get a reference to the [Window] at the given index and the area it occupies.
    fn get_area(&self, at: usize) -> Option<(&W, Rect)>;

    /// Get a reference to the [Window] at the given index.
    fn get(&self, at: usize) -> Option<&W> {
        self.get_area(at).map(|(w, _)| w)
    }

    /// Get a mutable reference to the [Window] at the given index.
    fn get_mut(&mut self, at: usize) -> Option<&mut W>;

    /// Swap the [Windows](Window) at the given indices.
    fn swap(&mut self, a: usize, b: usize);

    /// Split the screen space occupied by the [Window] at the given index along the given axis,
    /// and insert `W` before or after it.
    fn open(
        &mut self,
        at: usize,
        open: W,
        length: Option<u16>,
        rel: MoveDir1D,
        split_axis: Axis,
        trail: Box<ResizeInfoTrail<'_, X, Y>>,
    ) -> usize;

    /// Clear all explicitly specified window sizes.
    fn clear_sizes(&mut self);

    /// Save all of the current lengths in the tree along a given [Axis].
    fn freeze(&mut self, axis: Axis);

    /// Set the area that this portion of the layout covers.
    fn resize(
        &mut self,
        at: usize,
        axis: Axis,
        change: SizeChange<u16>,
        trail: Box<ResizeInfoTrail<'_, X, Y>>,
    );

    /// Set the area that this portion of the layout covers.
    fn set_area(&mut self, area: Rect, info: &ResizeInfo);

    /// Draw the [Windows](Window) that this tree contains.
    fn draw(&mut self, buf: &mut Buffer, focus: Option<usize>);

    fn _neighbor_walk(
        &self,
        base: usize,
        current: (usize, usize),
        c: TermOffset,
        dir: MoveDir2D,
    ) -> (usize, usize);
    fn _neighbor_of(
        &self,
        base: usize,
        at: usize,
        c: TermOffset,
        dir: MoveDir2D,
        count: usize,
    ) -> Option<(usize, usize)>;

    /// Find the neighbor of a given [Window] that is [*n*](Count) windows away in [MoveDir2D]
    /// direction.
    fn neighbor(&self, at: usize, dir: MoveDir2D, count: usize) -> Option<usize>;
}

impl<W: Window, X: AxisT, Y: AxisT> LayoutOps<W, X, Y> for Value<W, X, Y> {
    fn close(&mut self, idx: usize, trail: Box<ResizeInfoTrail<'_, X, Y>>) -> Option<W> {
        match self {
            Value::Window(_, _) => panic!("cannot remove element from non-tree"),
            Value::Tree(tree, ref mut info) => {
                let trail = ResizeInfoTrail::new(idx, &mut info.resized, Some(trail));

                tree.close(idx, trail)
            },
        }
    }

    fn collapse(self, at: usize) -> Option<W> {
        match self {
            Value::Window(w, _) => Some(w),
            Value::Tree(tree, _) => tree.collapse(at),
        }
    }

    fn get_area(&self, at: usize) -> Option<(&W, Rect)> {
        match self {
            Value::Window(ref window, info) => {
                if at == 0 {
                    Some((window, info.area))
                } else {
                    None
                }
            },
            Value::Tree(ref tree, _) => {
                return tree.get_area(at);
            },
        }
    }

    fn get_mut(&mut self, at: usize) -> Option<&mut W> {
        match self {
            Value::Window(ref mut window, _) => {
                if at == 0 {
                    Some(window)
                } else {
                    None
                }
            },
            Value::Tree(ref mut tree, _) => {
                return tree.get_mut(at);
            },
        }
    }

    fn dimensions(&self) -> (usize, usize) {
        match self {
            Value::Window(_, _) => (1, 1),
            Value::Tree(tree, _) => tree.dimensions(),
        }
    }

    fn size(&self) -> usize {
        match self {
            Value::Window(_, _) => 1,
            Value::Tree(tree, _) => tree.size(),
        }
    }

    fn swap(&mut self, a: usize, b: usize) {
        match self {
            Value::Window(_, _) => (),
            Value::Tree(tree, _) => tree.swap(a, b),
        }
    }

    fn clear_sizes(&mut self) {
        match self {
            Value::Window(_, _) => {
                return;
            },
            Value::Tree(tree, ref mut info) => {
                info.resized.lengths = None;

                tree.clear_sizes();
            },
        }
    }

    fn freeze(&mut self, axis: Axis) {
        match self {
            Value::Window(_, _) => {
                return;
            },
            Value::Tree(tree, info) => {
                if axis == Y::axis() {
                    info.resized.lengths = Some(tree.get_lengths());
                }

                tree.freeze(axis);
            },
        }
    }

    fn resize(
        &mut self,
        at: usize,
        axis: Axis,
        change: SizeChange<u16>,
        mut trail: Box<ResizeInfoTrail<'_, X, Y>>,
    ) {
        match self {
            Value::Window(_, info) => {
                let len = match change {
                    SizeChange::Equal => {
                        trail.clear(axis);
                        return;
                    },
                    SizeChange::Exact(amt) => amt.max(MIN_WIN_LEN),
                    SizeChange::Decrease(amt) => info.get_length(axis).saturating_sub(amt),
                    SizeChange::Increase(amt) => info.get_length(axis).saturating_add(amt),
                };

                trail.set_size(axis, len);
            },
            Value::Tree(tree, ref mut info) => {
                info.resized.lengths = Some(tree.get_lengths());

                let trail = ResizeInfoTrail::new(at, &mut info.resized, Some(trail));

                tree.resize(at, axis, change, trail);
            },
        }
    }

    fn set_area(&mut self, area: Rect, _: &ResizeInfo) {
        match self {
            Value::Window(_, ref mut info) => {
                info.area = area;
            },
            Value::Tree(tree, ref mut info) => {
                info.area = area;
                tree.set_area(area, &info.resized);
            },
        }
    }

    fn draw(&mut self, buf: &mut Buffer, focus: Option<usize>) {
        match self {
            Value::Window(window, ref mut info) => {
                window.draw(info.area, buf, matches!(focus, Some(0)));
            },
            Value::Tree(tree, _) => {
                tree.draw(buf, focus);
            },
        }
    }

    fn open(
        &mut self,
        at: usize,
        open: W,
        length: Option<u16>,
        rel: MoveDir1D,
        split_axis: Axis,
        trail: Box<ResizeInfoTrail<'_, X, Y>>,
    ) -> usize {
        match self {
            Value::Window(_, info) => {
                // Change info for the rotated tree.
                let mut info = info.to_tree(length.map(|_| split_axis));
                let mut trail = ResizeInfoTrail::new(at, &mut info.resized, Some(trail));
                trail.split_or_clear(rel, split_axis, length);

                // Make new tree with the converted info.
                let tree = AxisTreeNode::singleton(open);
                let mut node = Value::Tree(Box::new(tree), info);
                std::mem::swap(self, &mut node);

                match (self, node, rel) {
                    (Value::Tree(t, _), Value::Window(w, _), MoveDir1D::Previous) => {
                        t.insert_max(w);
                        0
                    },
                    (Value::Tree(t, _), Value::Window(w, _), MoveDir1D::Next) => {
                        t.insert_min(w);
                        1
                    },
                    (_, _, _) => {
                        panic!("split: invalid state");
                    },
                }
            },
            Value::Tree(tree, ref mut info) => {
                let trail = ResizeInfoTrail::new(at, &mut info.resized, Some(trail));

                tree.open(at, open, length, rel, split_axis, trail)
            },
        }
    }

    fn _neighbor_walk(
        &self,
        base: usize,
        current: (usize, usize),
        c: TermOffset,
        dir: MoveDir2D,
    ) -> (usize, usize) {
        if current.1 == 0 {
            return current;
        }

        match self {
            Value::Window(_, _) => (base, current.1 - 1),
            Value::Tree(tree, _) => tree._neighbor_walk(base, current, c, dir),
        }
    }

    fn _neighbor_of(
        &self,
        base: usize,
        at: usize,
        c: TermOffset,
        dir: MoveDir2D,
        count: usize,
    ) -> Option<(usize, usize)> {
        match self {
            Value::Window(_, _) => Some((base, count)),
            Value::Tree(tree, _) => tree._neighbor_of(base, at, c, dir, count),
        }
    }

    fn neighbor(&self, _: usize, _: MoveDir2D, _: usize) -> Option<usize> {
        unreachable!();
    }
}

type HorizontalTree<W> = AxisTree<W, HorizontalT, VerticalT>;

impl<W: Window, X: AxisT, Y: AxisT> LayoutOps<W, X, Y> for AxisTreeNode<W, X, Y> {
    fn size(&self) -> usize {
        self.info.size
    }

    fn dimensions(&self) -> (usize, usize) {
        self.info.dimensions
    }

    fn close(&mut self, at: usize, mut trail: Box<ResizeInfoTrail<'_, X, Y>>) -> Option<W> {
        let lsize = self.left.size();
        let vsize = self.value.size();

        match winnr_cmp(at, lsize, vsize) {
            (Ordering::Less, idx) => {
                let ret = self.left.close(idx, trail);
                self.balance_left();
                return ret;
            },
            (Ordering::Equal, idx) => {
                if vsize == 1 {
                    trail.clear(X::axis());

                    return self.remove_root().collapse(0);
                } else {
                    let ret = self.value.close(idx, trail);
                    self._update_info();
                    return ret;
                }
            },
            (Ordering::Greater, idx) => {
                let ret = self.right.close(idx, trail);
                self.balance_right();
                return ret;
            },
        }
    }

    fn collapse(self, at: usize) -> Option<W> {
        let lsize = self.left.size();
        let vsize = self.value.size();

        match winnr_cmp(at, lsize, vsize) {
            (Ordering::Less, idx) => self.left.collapse(idx),
            (Ordering::Equal, idx) => self.value.collapse(idx),
            (Ordering::Greater, idx) => self.right.collapse(idx),
        }
    }

    fn get_area(&self, at: usize) -> Option<(&W, Rect)> {
        let lsize = self.left.size();
        let vsize = self.value.size();

        match winnr_cmp(at, lsize, vsize) {
            (Ordering::Less, idx) => self.left.get_area(idx),
            (Ordering::Equal, idx) => self.value.get_area(idx),
            (Ordering::Greater, idx) => self.right.get_area(idx),
        }
    }

    fn get_mut(&mut self, at: usize) -> Option<&mut W> {
        let lsize = self.left.size();
        let vsize = self.value.size();

        match winnr_cmp(at, lsize, vsize) {
            (Ordering::Less, idx) => self.left.get_mut(idx),
            (Ordering::Equal, idx) => self.value.get_mut(idx),
            (Ordering::Greater, idx) => self.right.get_mut(idx),
        }
    }

    fn open(
        &mut self,
        at: usize,
        open: W,
        length: Option<u16>,
        rel: MoveDir1D,
        split_axis: Axis,
        mut trail: Box<ResizeInfoTrail<'_, X, Y>>,
    ) -> usize {
        let lsize = self.left.size();
        let vsize = self.value.size();

        match winnr_cmp(at, lsize, vsize) {
            (Ordering::Less, idx) => {
                let winnr = self.left.open(idx, open, length, rel, split_axis, trail);
                self.balance_right();
                winnr
            },
            (Ordering::Equal, idx) => {
                let split_here = vsize == 1 && split_axis == X::axis();

                if split_here {
                    trail.split_or_clear(rel, split_axis, length);

                    match rel {
                        MoveDir1D::Previous => {
                            let winnr = self.left.insert_max(open);
                            self.balance_right();
                            winnr
                        },
                        MoveDir1D::Next => {
                            let winnr = self.right.insert_min(open);
                            let winnr = lsize + vsize + winnr;
                            self.balance_left();
                            winnr
                        },
                    }
                } else {
                    let winnr = self.value.open(idx, open, length, rel, split_axis, trail);
                    let winnr = lsize + winnr;
                    self._update_info();
                    winnr
                }
            },
            (Ordering::Greater, idx) => {
                let winnr = self.right.open(idx, open, length, rel, split_axis, trail);
                let winnr = lsize + vsize + winnr;
                self.balance_left();
                winnr
            },
        }
    }

    /*
     * Ideally this would just be two self.get_mut() calls, but that would
     * mutably borrow self twice, so we need to descend the tree until we get
     * to the divergent point.
     */
    fn swap(&mut self, a: usize, b: usize) {
        if a == b {
            /*
             * No-op, do nothing.
             */
            return;
        }

        let lsize = self.left.size();
        let vsize = self.value.size();

        let ap = winnr_cmp(a, lsize, vsize);
        let bp = winnr_cmp(b, lsize, vsize);

        let (amut, bmut) = match (ap, bp) {
            ((Ordering::Less, aidx), (Ordering::Less, bidx)) => {
                self.left.swap(aidx, bidx);
                return;
            },
            ((Ordering::Less, aidx), (Ordering::Equal, bidx)) => {
                (self.left.get_mut(aidx), self.value.get_mut(bidx))
            },
            ((Ordering::Less, aidx), (Ordering::Greater, bidx)) => {
                (self.left.get_mut(aidx), self.right.get_mut(bidx))
            },
            ((Ordering::Equal, aidx), (Ordering::Less, bidx)) => {
                (self.value.get_mut(aidx), self.left.get_mut(bidx))
            },
            ((Ordering::Equal, aidx), (Ordering::Equal, bidx)) => {
                self.value.swap(aidx, bidx);
                return;
            },
            ((Ordering::Equal, aidx), (Ordering::Greater, bidx)) => {
                (self.value.get_mut(aidx), self.right.get_mut(bidx))
            },
            ((Ordering::Greater, aidx), (Ordering::Less, bidx)) => {
                (self.right.get_mut(aidx), self.left.get_mut(bidx))
            },
            ((Ordering::Greater, aidx), (Ordering::Equal, bidx)) => {
                (self.right.get_mut(aidx), self.value.get_mut(bidx))
            },
            ((Ordering::Greater, aidx), (Ordering::Greater, bidx)) => {
                self.right.swap(aidx, bidx);
                return;
            },
        };

        match (amut, bmut) {
            (Some(awin), Some(bwin)) => {
                std::mem::swap(awin, bwin);
            },
            (_, _) => {
                /*
                 * If an index doesn't exist, do nothing.
                 */
            },
        }
    }

    fn clear_sizes(&mut self) {
        self.left.clear_sizes();
        self.value.clear_sizes();
        self.right.clear_sizes();
    }

    fn freeze(&mut self, axis: Axis) {
        self.left.freeze(axis);
        self.value.freeze(axis);
        self.right.freeze(axis);
    }

    fn resize(
        &mut self,
        at: usize,
        axis: Axis,
        change: SizeChange<u16>,
        trail: Box<ResizeInfoTrail<'_, X, Y>>,
    ) {
        let lsize = self.left.size();
        let vsize = self.value.size();

        match winnr_cmp(at, lsize, vsize) {
            (Ordering::Less, idx) => {
                self.left.resize(idx, axis, change, trail);
            },
            (Ordering::Equal, idx) => {
                self.value.resize(idx, axis, change, trail);
            },
            (Ordering::Greater, idx) => {
                self.right.resize(idx, axis, change, trail);
            },
        }
    }

    fn set_area(&mut self, area: Rect, info: &ResizeInfo) {
        if info.lengths.is_some() {
            set_area_lens(self, area, info);
        } else {
            set_area_equal(self, area, info);
        }
    }

    fn draw(&mut self, buf: &mut Buffer, focus: Option<usize>) {
        let mut base = 0;
        let mut f = |value: &mut Value<W, X, Y>| {
            value.draw(buf, focus.and_then(|n| n.checked_sub(base)));

            base += value.size();
        };

        self.for_each_value(&mut f);
    }

    fn _neighbor_walk(
        &self,
        base: usize,
        current: (usize, usize),
        c: TermOffset,
        dir: MoveDir2D,
    ) -> (usize, usize) {
        if current.1 == 0 {
            /*
             * We've already found the target index.
             */
            return current;
        }

        let lsize = self.left.size();
        let vsize = self.value.size();

        let lbase = base;
        let vbase = lbase + lsize;
        let rbase = vbase + vsize;

        match (X::axis(), dir) {
            (Vertical, Left) | (Horizontal, Up) => {
                let current = self.right._neighbor_walk(rbase, current, c, dir);
                let current = self.value._neighbor_walk(vbase, current, c, dir);
                return self.left._neighbor_walk(lbase, current, c, dir);
            },
            (Vertical, Right) | (Horizontal, Down) => {
                let current = self.left._neighbor_walk(lbase, current, c, dir);
                let current = self.value._neighbor_walk(vbase, current, c, dir);
                return self.right._neighbor_walk(rbase, current, c, dir);
            },
            (Vertical, Up | Down) => {
                /*
                 * Find the node with the same screen column offset as the starting window's
                 * cursor, and continue from there.
                 */
                let mut off = base;
                let col = c.0;

                for value in self.iter() {
                    let area = value.area();
                    let lc = area.left();
                    let rc = area.right();

                    if col >= lc && col < rc {
                        return value._neighbor_walk(off, current, c, dir);
                    }

                    off += value.size();
                }

                // We should always find a valid window, but fall back here if we don't.
                self.value._neighbor_walk(base, current, c, dir)
            },
            (Horizontal, Left | Right) => {
                /*
                 * Find the node with the same screen row offset as the starting window's cursor,
                 * and continue from there.
                 */

                let mut off = base;
                let row = c.1;

                for value in self.iter() {
                    let area = value.area();
                    let tr = area.top();
                    let br = area.bottom();

                    if row >= tr && row < br {
                        return value._neighbor_walk(off, current, c, dir);
                    }

                    off += value.size();
                }

                // We should always find a valid window, but fall back here if we don't.
                self.value._neighbor_walk(base, current, c, dir)
            },
        }
    }

    fn _neighbor_of(
        &self,
        base: usize,
        at: usize,
        c: TermOffset,
        dir: MoveDir2D,
        count: usize,
    ) -> Option<(usize, usize)> {
        let lsize = self.left.size();
        let vsize = self.value.size();

        let branch = winnr_cmp(at, lsize, vsize);

        let lbase = base;
        let vbase = lbase + lsize;
        let rbase = vbase + vsize;

        match (X::axis(), dir) {
            (Vertical, Left) | (Horizontal, Up) => {
                match branch {
                    (Ordering::Less, idx) => {
                        return self.left._neighbor_of(lbase, idx, c, dir, count);
                    },
                    (Ordering::Equal, idx) => {
                        let current = self.value._neighbor_of(vbase, idx, c, dir, count)?;
                        let current = self.left._neighbor_walk(lbase, current, c, dir);

                        return Some(current);
                    },
                    (Ordering::Greater, idx) => {
                        let current = self.right._neighbor_of(rbase, idx, c, dir, count)?;
                        let current = self.value._neighbor_walk(vbase, current, c, dir);
                        let current = self.left._neighbor_walk(lbase, current, c, dir);

                        return Some(current);
                    },
                }
            },
            (Vertical, Right) | (Horizontal, Down) => {
                match branch {
                    (Ordering::Less, idx) => {
                        let current = self.left._neighbor_of(lbase, idx, c, dir, count)?;
                        let current = self.value._neighbor_walk(vbase, current, c, dir);
                        let current = self.right._neighbor_walk(rbase, current, c, dir);

                        return Some(current);
                    },
                    (Ordering::Equal, idx) => {
                        let current = self.value._neighbor_of(vbase, idx, c, dir, count)?;
                        let current = self.right._neighbor_walk(rbase, current, c, dir);

                        return Some(current);
                    },
                    (Ordering::Greater, idx) => {
                        return self.right._neighbor_of(rbase, idx, c, dir, count);
                    },
                }
            },
            (Vertical, Up | Down) | (Horizontal, Left | Right) => {
                match branch {
                    (Ordering::Less, idx) => {
                        return self.left._neighbor_of(lbase, idx, c, dir, count);
                    },
                    (Ordering::Equal, idx) => {
                        return self.value._neighbor_of(vbase, idx, c, dir, count);
                    },
                    (Ordering::Greater, idx) => {
                        return self.right._neighbor_of(rbase, idx, c, dir, count);
                    },
                }
            },
        }
    }

    fn neighbor(&self, at: usize, dir: MoveDir2D, count: usize) -> Option<usize> {
        let (w, r) = self.get_area(at)?;
        let c = w.get_term_cursor().unwrap_or((r.x, r.y));
        self._neighbor_of(0, at, c, dir, count).map(|current| current.0)
    }
}

impl<W: Window, X: AxisT, Y: AxisT> LayoutOps<W, X, Y> for AxisTree<W, X, Y> {
    fn size(&self) -> usize {
        match self {
            None => 0,
            Some(node) => node.size(),
        }
    }

    fn dimensions(&self) -> (usize, usize) {
        match self {
            None => (0, 0),
            Some(node) => node.dimensions(),
        }
    }

    fn close(&mut self, at: usize, mut trail: Box<ResizeInfoTrail<'_, X, Y>>) -> Option<W> {
        if let Some(node) = self {
            let lsize = node.left.size();
            let vsize = node.value.size();

            match winnr_cmp(at, lsize, vsize) {
                (Ordering::Less, idx) => {
                    let ret = node.left.close(idx, trail);
                    node.balance_left();
                    return ret;
                },
                (Ordering::Equal, idx) => {
                    if vsize == 1 {
                        trail.clear(X::axis());

                        return self.remove_root().collapse(0);
                    } else {
                        let ret = node.value.close(idx, trail);
                        node._update_info();
                        return ret;
                    }
                },
                (Ordering::Greater, idx) => {
                    let ret = node.right.close(idx, trail);
                    node.balance_right();
                    return ret;
                },
            }
        } else {
            return None;
        }
    }

    fn collapse(self, at: usize) -> Option<W> {
        self.and_then(|node| node.collapse(at))
    }

    fn get_area(&self, at: usize) -> Option<(&W, Rect)> {
        self.as_ref().and_then(|node| node.get_area(at))
    }

    fn get_mut(&mut self, at: usize) -> Option<&mut W> {
        self.as_mut().and_then(|node| node.get_mut(at))
    }

    fn swap(&mut self, a: usize, b: usize) {
        if let Some(node) = self {
            node.swap(a, b);
        }
    }

    fn open(
        &mut self,
        at: usize,
        open: W,
        length: Option<u16>,
        rel: MoveDir1D,
        split_axis: Axis,
        trail: Box<ResizeInfoTrail<'_, X, Y>>,
    ) -> usize {
        match self {
            None => {
                *self = AxisTree::from(if split_axis == X::axis() {
                    Value::from(open)
                } else {
                    Value::from(AxisTreeNode::singleton(open))
                });

                return 0;
            },
            Some(node) => {
                return node.open(at, open, length, rel, split_axis, trail);
            },
        }
    }

    fn _neighbor_walk(
        &self,
        base: usize,
        current: (usize, usize),
        c: TermOffset,
        dir: MoveDir2D,
    ) -> (usize, usize) {
        match self {
            None => current,
            Some(node) => node._neighbor_walk(base, current, c, dir),
        }
    }

    fn _neighbor_of(
        &self,
        base: usize,
        at: usize,
        c: TermOffset,
        dir: MoveDir2D,
        count: usize,
    ) -> Option<(usize, usize)> {
        self.as_ref().and_then(|node| node._neighbor_of(base, at, c, dir, count))
    }

    fn clear_sizes(&mut self) {
        if let Some(node) = self {
            node.clear_sizes();
        }
    }

    fn freeze(&mut self, axis: Axis) {
        if let Some(node) = self {
            node.freeze(axis);
        }
    }

    fn resize(
        &mut self,
        at: usize,
        axis: Axis,
        change: SizeChange<u16>,
        trail: Box<ResizeInfoTrail<'_, X, Y>>,
    ) {
        if let Some(node) = self {
            node.resize(at, axis, change, trail);
        }
    }

    fn set_area(&mut self, area: Rect, info: &ResizeInfo) {
        if let Some(node) = self {
            node.set_area(area, info);
        }
    }

    fn draw(&mut self, buf: &mut Buffer, focus: Option<usize>) {
        if let Some(node) = self {
            node.draw(buf, focus);
        }
    }

    fn neighbor(&self, at: usize, dir: MoveDir2D, count: usize) -> Option<usize> {
        self.as_ref().and_then(|tree| tree.neighbor(at, dir, count))
    }
}

/// Manages the current layout and focus of [Windows](Window) on the screen.
pub struct WindowLayoutState<W: Window> {
    root: HorizontalTree<W>,
    info: TreeInfo,
    zoom: bool,
    focused: usize,
    focused_last: usize,
}

impl<W: Window> WindowLayoutState<W> {
    /// Create a new instance containing a single [Window].
    pub fn new(window: W) -> Self {
        WindowLayoutState {
            root: AxisTree::singleton(window),
            info: TreeInfo::default(),
            zoom: false,
            focused: 0,
            focused_last: 0,
        }
    }

    /// Create a new instance without any windows.
    pub fn empty() -> Self {
        WindowLayoutState {
            root: None,
            info: TreeInfo::default(),
            zoom: false,
            focused: 0,
            focused_last: 0,
        }
    }

    /// Fetch a reference to the currently focused [Window].
    pub fn get(&self) -> Option<&W> {
        self.root.get(self.focused)
    }

    /// Fetch a mutable reference the currently focused [Window].
    pub fn get_mut(&mut self) -> Option<&mut W> {
        self.root.get_mut(self.focused)
    }

    /// Extract the currently focused [Window] into its own window layout.
    pub fn extract(&mut self) -> Self {
        let at = self.focused;
        let trail = ResizeInfoTrail::new(at, &mut self.info.resized, None);

        if let Some(w) = self.root.close(at, trail) {
            self._clamp_focus();

            return WindowLayoutState::new(w);
        } else {
            return WindowLayoutState::empty();
        }
    }

    fn move_side(&mut self, at: usize, dir: MoveDir2D) {
        let trail = ResizeInfoTrail::new(at, &mut self.info.resized, None);

        if let Some(window) = self.root.close(at, trail) {
            let nr = self.root.insert_side(window, dir);
            self._focus(nr);
        }
    }

    fn close(&mut self, at: usize, _flags: CloseFlags) -> bool {
        let trail = ResizeInfoTrail::new(at, &mut self.info.resized, None);

        self.root.close(at, trail);
        self._clamp_focus();

        return self.root.size() == 0;
    }

    fn only(&mut self, at: usize, _flags: CloseFlags) -> bool {
        let target = if at < self.root.size() {
            at
        } else {
            self.focused
        };

        self.root = std::mem::take(&mut self.root)
            .collapse(target)
            .and_then(AxisTree::singleton);
        self._clamp_focus();

        return self.root.size() == 0;
    }

    fn open(&mut self, w: W, length: Option<u16>, axis: Axis, rel: MoveDir1D) {
        let windex = self.focused;

        if length.is_some() {
            self.freeze(axis);
        }

        let trail = ResizeInfoTrail::new(windex, &mut self.info.resized, None);

        let nr = self.root.open(windex, w, length, rel, axis, trail);
        self.root.set_area(self.info.area, &self.info.resized);

        self._focus(nr);
    }

    fn clear_sizes(&mut self) {
        self.info.resized.clear_sizes();
        self.root.clear_sizes();
        self.root.set_area(self.info.area, &self.info.resized);
    }

    fn freeze(&mut self, axis: Axis) {
        if axis == Axis::Horizontal {
            self.info.resized.lengths = Some(self.root.get_lengths());
        }

        self.root.freeze(axis);
    }

    fn resize(&mut self, axis: Axis, change: SizeChange<u16>) {
        self.freeze(axis);

        let windex = self.focused;
        let trail = ResizeInfoTrail::new(windex, &mut self.info.resized, None);

        self.root.resize(windex, axis, change, trail);
        self.root.set_area(self.info.area, &self.info.resized);
    }

    fn split(&mut self, axis: Axis, rel: MoveDir1D) {
        if let Some(w) = self.root.get(self.focused) {
            let wd = w.dup();
            self.open(wd, None, axis, rel);
        }
    }

    fn _focus(&mut self, focus: usize) {
        let max = self.root.size().saturating_sub(1);

        self.focused_last = self.focused;
        self.focused = focus.min(max);
    }

    fn _clamp_focus(&mut self) {
        let nwins = self.root.size();
        let lastw = nwins.saturating_sub(1);

        self.focused_last = self.focused_last.min(lastw);
        self.focused = self.focused.min(lastw);
    }

    fn _max_idx(&self) -> usize {
        self.root.size().saturating_sub(1)
    }

    fn _target<C: EditContext>(&self, change: &FocusChange, ctx: &C) -> Option<usize> {
        match change {
            FocusChange::Current => {
                return Some(self.focused);
            },
            FocusChange::Offset(count, false) => {
                let winnr = windex(count, ctx);

                if winnr >= self.root.size() {
                    // Invalid window index; do nothing.
                    return None;
                }

                return Some(winnr);
            },
            FocusChange::Offset(count, true) => {
                let target = windex(count, ctx).min(self._max_idx());

                return Some(target);
            },
            FocusChange::Direction1D(dir, count, wrap) => {
                let nwins = self.root.size();
                let count = ctx.resolve(count);

                return idx_offset(self.focused, count, dir, nwins, *wrap);
            },
            FocusChange::Direction2D(dir, count) => {
                let count = ctx.resolve(count);

                return self.root.neighbor(self.focused, *dir, count);
            },
            FocusChange::Position(MovePosition::Beginning) => {
                return Some(0);
            },
            FocusChange::Position(MovePosition::Middle) => {
                return Some(self.root.size() / 2);
            },
            FocusChange::Position(MovePosition::End) => {
                return Some(self.root.size().saturating_sub(1));
            },
            FocusChange::PreviouslyFocused => {
                return Some(self.focused_last);
            },
        }
    }
}

impl<W: Window, C: EditContext> WindowActions<W, C> for WindowLayoutState<W> {
    fn window_focus(&mut self, change: &FocusChange, ctx: &C) -> EditResult {
        if let Some(target) = self._target(change, ctx) {
            self.zoom = false;
            self._focus(target);
        }

        return Ok(None);
    }

    fn window_exchange(&mut self, change: &FocusChange, ctx: &C) -> EditResult {
        if let Some(target) = self._target(change, ctx) {
            self.zoom = false;
            self.root.swap(self.focused, target);
        }

        return Ok(None);
    }

    fn window_move_side(&mut self, dir: MoveDir2D, _: &C) -> EditResult {
        self.zoom = false;
        self.move_side(self.focused, dir);

        return Ok(None);
    }

    fn window_rotate(&mut self, _dir: MoveDir1D, _ctx: &C) -> EditResult {
        // XXX: implement

        return Ok(None);
    }

    fn window_split(&mut self, axis: Axis, rel: MoveDir1D, count: Count, ctx: &C) -> EditResult {
        let count = ctx.resolve(&count);

        self.zoom = false;

        for _ in 0..count {
            self.split(axis, rel);
        }

        return Ok(None);
    }

    fn window_clear_sizes(&mut self, _: &C) -> EditResult {
        self.clear_sizes();

        return Ok(None);
    }

    fn window_resize(&mut self, axis: Axis, size: SizeChange<Count>, ctx: &C) -> EditResult {
        let change: SizeChange<u16> = match &size {
            SizeChange::Equal => SizeChange::Equal,
            SizeChange::Exact(count) => SizeChange::Exact(ctx.resolve(count).try_into()?),
            SizeChange::Increase(count) => SizeChange::Increase(ctx.resolve(count).try_into()?),
            SizeChange::Decrease(count) => SizeChange::Decrease(ctx.resolve(count).try_into()?),
        };

        self.zoom = false;
        self.resize(axis, change);

        return Ok(None);
    }

    fn window_close(&mut self, target: CloseTarget, flags: CloseFlags, ctx: &C) -> EditResult {
        let nwins = self.root.size();

        if nwins == 1 && !flags.contains(CloseFlags::QUIT) {
            /*
             * If the QUIT flag is not present, disallow closing the last window.
             */
            return Ok(None);
        }

        self.zoom = false;

        match target {
            CloseTarget::Single(focus) => {
                if let Some(target) = self._target(&focus, ctx) {
                    self.close(target, flags);
                }

                return Ok(None);
            },
            CloseTarget::AllBut(focus) => {
                if let Some(target) = self._target(&focus, ctx) {
                    self.only(target, flags);
                }

                return Ok(None);
            },
            CloseTarget::All => {
                self.root = None;

                return Ok(None);
            },
        }
    }

    fn window_zoom_toggle(&mut self, _: &C) -> EditResult {
        self.zoom = self.zoom.not();

        Ok(None)
    }
}

impl<W: Window, C: EditContext> WindowContainer<W, C> for WindowLayoutState<W> {
    fn windows(&self) -> usize {
        self.root.size()
    }

    fn window_open(
        &mut self,
        window: W,
        axis: Axis,
        rel: MoveDir1D,
        count: Option<Count>,
        ctx: &C,
    ) -> UIResult {
        let count = count.map(|count| ctx.resolve(&count) as u16);

        self.open(window, count, axis, rel);

        return Ok(None);
    }

    fn window_command(&mut self, action: WindowAction, ctx: &C) -> UIResult {
        let info = match action {
            WindowAction::Focus(target) => self.window_focus(&target, ctx)?,
            WindowAction::MoveSide(dir) => self.window_move_side(dir, ctx)?,
            WindowAction::Exchange(target) => self.window_exchange(&target, ctx)?,
            WindowAction::Rotate(dir) => self.window_rotate(dir, ctx)?,
            WindowAction::Split(axis, rel, count) => self.window_split(axis, rel, count, ctx)?,
            WindowAction::ClearSizes => self.window_clear_sizes(ctx)?,
            WindowAction::Resize(axis, size) => self.window_resize(axis, size, ctx)?,
            WindowAction::Close(target, flags) => self.window_close(target, flags, ctx)?,
            WindowAction::ZoomToggle => self.window_zoom_toggle(ctx)?,
        };

        return Ok(info);
    }
}

/// Handles rendering the current window layout state to the terminal.
pub struct WindowLayout<W: Window> {
    focused: bool,
    _pw: PhantomData<W>,
}

impl<W: Window> WindowLayout<W> {
    /// Create a new widget for displaying window layouts.
    pub fn new() -> Self {
        WindowLayout { focused: false, _pw: PhantomData }
    }

    /// Indicate whether the window layout tree is currently focused.
    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl<W: Window> Default for WindowLayout<W> {
    fn default() -> Self {
        Self::new()
    }
}

impl<W: Window> StatefulWidget for WindowLayout<W> {
    type State = WindowLayoutState<W>;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if state.zoom {
            if let Some(window) = state.get_mut() {
                window.draw(area, buf, true);
            }

            return;
        }

        state.info.area = area;
        state.root.set_area(area, &state.info.resized);
        state.root.draw(buf, state.focused.into());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::vim::VimContext;
    use crate::widgets::TerminalCursor;
    use rand::Rng;

    macro_rules! fc {
        ($n: expr) => {
            FocusChange::Offset(Count::Exact($n), false)
        };
        ($n: expr, $b: expr) => {
            FocusChange::Offset(Count::Exact($n), $b)
        };
    }

    macro_rules! window_open {
        ($tree: expr, $w: expr, $axis: expr, $dir: expr, $c: expr, $ctx: expr) => {
            $tree.window_open($w, $axis, $dir, $c.into(), $ctx).unwrap()
        };
    }

    macro_rules! window_close {
        ($tree: expr, $ct: expr, $flags: expr, $ctx: expr) => {
            $tree.window_close($ct, $flags, $ctx).unwrap()
        };
    }

    macro_rules! window_exchange {
        ($tree: expr, $fc: expr, $ctx: expr) => {
            $tree.window_exchange(&$fc, $ctx).unwrap()
        };
    }

    macro_rules! window_focus {
        ($tree: expr, $fc: expr, $ctx: expr) => {
            $tree.window_focus(&$fc, $ctx).unwrap()
        };
    }

    macro_rules! window_focus_off {
        ($tree: expr, $c: expr, $ctx: expr) => {
            window_focus!($tree, fc!($c), $ctx)
        };
    }

    macro_rules! window_focus_1d {
        ($tree: expr, $dir: expr, $c: expr, $ctx: expr) => {
            window_focus!($tree, FocusChange::Direction1D($dir, $c, true), $ctx)
        };
    }

    macro_rules! window_focus_2d {
        ($tree: expr, $dir: expr, $c: expr, $ctx: expr) => {
            window_focus!($tree, FocusChange::Direction2D($dir, $c), $ctx)
        };
    }

    macro_rules! window_move_side {
        ($tree: expr, $dir: expr, $ctx: expr) => {
            $tree.window_move_side($dir, $ctx).unwrap()
        };
    }

    macro_rules! window_resize {
        ($tree: expr, $axis: expr, $szch: expr, $ctx: expr) => {
            $tree.window_resize($axis, $szch, $ctx).unwrap()
        };
    }

    macro_rules! window_split {
        ($tree: expr, $axis: expr, $dir: expr, $count: expr, $ctx: expr) => {
            $tree.window_split($axis, $dir, $count, $ctx).unwrap()
        };
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct TestWindow {
        term_area: Rect,
        id: Option<usize>,
    }

    impl TestWindow {
        fn new() -> Self {
            TestWindow { term_area: Rect::default(), id: None }
        }
    }

    impl TerminalCursor for TestWindow {
        fn get_term_cursor(&self) -> Option<(u16, u16)> {
            (self.term_area.left(), self.term_area.top()).into()
        }
    }

    impl Window for TestWindow {
        fn draw(&mut self, area: Rect, _: &mut Buffer, _: bool) {
            self.term_area = area;
        }

        fn dup(&self) -> Self {
            self.clone()
        }

        fn close(&mut self, _: CloseFlags) -> bool {
            true
        }
    }

    fn three_by_three() -> (WindowLayoutState<TestWindow>, VimContext) {
        /*
         * Set up a 3x3 grid, and start in bottom right. The splitting order is important here, to
         * create a more interesting tree. The final result (and their window indexes) looks like:
         *
         * +---+---+---+
         * | 0 | 1 | 6 |
         * +---+---+---+
         * | 2 | 3 | 7 |
         * +---+---+---+
         * | 4 | 5 | 8 |
         * +---+---+---+
         */
        let mut tree = WindowLayoutState::new(TestWindow::new());
        let ctx: VimContext = VimContext::default();

        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx);
        window_split!(tree, Axis::Horizontal, MoveDir1D::Previous, Count::Exact(2), &ctx);
        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx);
        window_focus_off!(tree, 3, &ctx);
        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx);
        window_focus_off!(tree, 5, &ctx);
        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx);
        window_focus_off!(tree, 7, &ctx);
        window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(2), &ctx);

        let mut idx = 0;

        while let Some(w) = tree.root.get_mut(idx) {
            w.id = Some(idx);
            idx += 1;
        }

        return (tree, ctx);
    }

    #[test]
    fn test_tree_get() {
        let (mut tree, _) = three_by_three();

        assert_eq!(tree.root.get(10), None);
        assert_eq!(tree.root.get(100), None);

        assert_eq!(tree.root.get_mut(10), None);
        assert_eq!(tree.root.get_mut(100), None);

        assert_eq!(tree.root.get(0).unwrap().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().id, Some(8));

        assert_eq!(tree.root.get_mut(0).unwrap().id, Some(0));
        assert_eq!(tree.root.get_mut(8).unwrap().id, Some(8));
    }

    #[test]
    fn test_tree_rotations() {
        let ctx: VimContext = VimContext::default();
        let flags = CloseFlags::QUIT;

        // Repeatedly delete the last window.
        let mut tree = WindowLayoutState::new(TestWindow::new());
        window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(99), &ctx);
        assert_eq!(tree.root.size(), 100);

        for n in 0..100 {
            window_close!(tree, CloseTarget::Single(fc!(100 - n)), flags, &ctx);
            assert_eq!(tree.root.size(), 99 - n);
        }

        // Repeatedly delete the first window.
        let mut tree = WindowLayoutState::new(TestWindow::new());
        window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(99), &ctx);
        assert_eq!(tree.root.size(), 100);

        for n in 0..100 {
            window_close!(tree, CloseTarget::Single(fc!(1)), flags, &ctx);
            assert_eq!(tree.root.size(), 99 - n);
        }

        // Generate random insertions and then deletions.
        let mut tree = WindowLayoutState::new(TestWindow::new());
        let mut rng = rand::thread_rng();

        assert_eq!(tree.root.size(), 1);

        for n in 0..999 {
            let size = tree.root.size();
            let target = rng.gen_range(1..=size);
            window_focus_off!(tree, target, &ctx);
            window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(1), &ctx);
            assert_eq!(tree.root.size(), 2 + n);
        }

        for n in 0..1000 {
            let size = tree.root.size();
            let target = rng.gen_range(1..=size);
            window_close!(tree, CloseTarget::Single(fc!(target)), flags, &ctx);
            assert_eq!(tree.root.size(), 999 - n);
        }
    }

    #[test]
    fn test_window_split() {
        let mut tree = WindowLayoutState::new(TestWindow::new());
        let ctx: VimContext = VimContext::default();

        assert_eq!(tree.root.size(), 1);
        assert_eq!(tree.focused, 0);

        window_split!(tree, Axis::Horizontal, MoveDir1D::Previous, Count::Contextual, &ctx);
        assert_eq!(tree.root.size(), 2);
        assert_eq!(tree.focused, 0);

        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Contextual, &ctx);
        assert_eq!(tree.root.size(), 3);
        assert_eq!(tree.focused, 0);

        window_split!(tree, Axis::Vertical, MoveDir1D::Next, Count::Contextual, &ctx);
        assert_eq!(tree.root.size(), 4);
        assert_eq!(tree.focused, 1);
    }

    #[test]
    fn test_window_nav_1d() {
        let mut tree = WindowLayoutState::new(TestWindow::new());
        let ctx: VimContext = VimContext::default();

        window_split!(tree, Axis::Horizontal, MoveDir1D::Previous, Count::Exact(5), &ctx);
        assert_eq!(tree.root.size(), 6);
        assert_eq!(tree.focused, 0);

        window_focus_1d!(tree, MoveDir1D::Next, Count::Exact(2), &ctx);
        assert_eq!(tree.focused, 2);

        window_focus_1d!(tree, MoveDir1D::Previous, Count::Exact(1), &ctx);
        assert_eq!(tree.focused, 1);

        window_focus_1d!(tree, MoveDir1D::Previous, Count::Exact(2), &ctx);
        assert_eq!(tree.focused, 5);

        window_focus_1d!(tree, MoveDir1D::Next, Count::Exact(1), &ctx);
        assert_eq!(tree.focused, 0);
    }

    #[test]
    fn test_window_nav_2d() {
        let (mut tree, ctx) = three_by_three();

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);

        // Draw so we know where the windows are inside the terminal.
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);
        let widget = WindowLayout::default();
        widget.render(area, &mut buffer, &mut tree);

        // Move left 2 windows.
        window_focus_2d!(tree, MoveDir2D::Left, Count::Exact(2), &ctx);
        assert_eq!(tree.focused, 4);

        // Move right 1 window.
        window_focus_2d!(tree, MoveDir2D::Right, Count::Exact(1), &ctx);
        assert_eq!(tree.focused, 5);

        // Move up 2 windows.
        window_focus_2d!(tree, MoveDir2D::Up, Count::Exact(2), &ctx);
        assert_eq!(tree.focused, 1);

        // We're already at the top, so we can't move up anymore.
        window_focus_2d!(tree, MoveDir2D::Up, Count::Exact(1), &ctx);
        assert_eq!(tree.focused, 1);

        // Move down 1 window.
        window_focus_2d!(tree, MoveDir2D::Down, Count::Exact(1), &ctx);
        assert_eq!(tree.focused, 3);

        // Try to move left 3 windows, stopping at the left side.
        window_focus_2d!(tree, MoveDir2D::Left, Count::Exact(3), &ctx);
        assert_eq!(tree.focused, 2);
    }

    #[test]
    fn test_window_close() {
        let (mut tree, ctx) = three_by_three();
        let flags = CloseFlags::NONE;

        let target = CloseTarget::Single(fc!(8));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 8);

        let target = CloseTarget::Single(fc!(6));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 7);

        let target = CloseTarget::Single(fc!(3));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 6);

        let target = CloseTarget::Single(fc!(2));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 5);

        /*
         * We should now have a tree that visually looks like:
         *
         * +---+---+
         * | 0 | 6 |
         * +---|   |
         * | 3 +---+
         * +---|   |
         * | 4 | 8 |
         * +---+---+
         */
        assert_eq!(tree.root.get(0).unwrap().id, Some(0));
        assert_eq!(tree.root.get(1).unwrap().id, Some(3));
        assert_eq!(tree.root.get(2).unwrap().id, Some(4));
        assert_eq!(tree.root.get(3).unwrap().id, Some(6));
        assert_eq!(tree.root.get(4).unwrap().id, Some(8));

        let target = CloseTarget::Single(fc!(2));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 4);

        let target = CloseTarget::Single(fc!(3));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 3);

        let target = CloseTarget::Single(fc!(3));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 2);

        let target = CloseTarget::Single(fc!(1));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 1);

        // Can't close last window because flags are NONE.
        let target = CloseTarget::Single(fc!(1));
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 1);

        // Passing QUIT does the job.
        let target = CloseTarget::Single(fc!(1));
        let flags = CloseFlags::QUIT;
        assert!(window_close!(tree, target, flags, &ctx).is_none());
        assert_eq!(tree.root.size(), 0);
    }

    #[test]
    fn test_window_close_allbut() {
        for idx in 1usize..=9usize {
            let (mut tree, ctx) = three_by_three();
            let target = CloseTarget::AllBut(fc!(idx));
            assert!(window_close!(tree, target, CloseFlags::NONE, &ctx).is_none());
            assert_eq!(tree.root.size(), 1);
            assert_eq!(tree.get().unwrap().id, Some(idx.saturating_sub(1)));
        }
    }

    #[test]
    fn test_window_close_all() {
        let (mut tree, ctx) = three_by_three();
        let target = CloseTarget::All;
        assert!(window_close!(tree, target, CloseFlags::NONE, &ctx).is_none());
        assert_eq!(tree.root.size(), 0);
    }

    #[test]
    fn test_window_exchange() {
        let (mut tree, ctx) = three_by_three();

        // Verify starting conditions.
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.get(0).unwrap().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().id, Some(8));

        // Swap window 9 with window 1.
        window_exchange!(tree, &fc!(1), &ctx);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.get(0).unwrap().id, Some(8));
        assert_eq!(tree.root.get(8).unwrap().id, Some(0));

        // Swap the new window 9 with window 8.
        window_exchange!(tree, &fc!(8), &ctx);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.get(0).unwrap().id, Some(8));
        assert_eq!(tree.root.get(7).unwrap().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().id, Some(7));

        // Focus on window 1, and swap with window 6.
        window_focus_off!(tree, 1, &ctx);
        assert_eq!(tree.focused, 0);

        window_exchange!(tree, &fc!(6), &ctx);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 0);
        assert_eq!(tree.root.get(0).unwrap().id, Some(5));
        assert_eq!(tree.root.get(5).unwrap().id, Some(8));
        assert_eq!(tree.root.get(7).unwrap().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().id, Some(7));
    }

    #[test]
    fn test_window_move_side() {
        let (mut tree, ctx) = three_by_three();

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        window_move_side!(tree, MoveDir2D::Left, &ctx);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (4, 3));
        assert_eq!(tree.focused, 0);

        window_move_side!(tree, MoveDir2D::Right, &ctx);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (4, 3));
        assert_eq!(tree.focused, 8);

        window_move_side!(tree, MoveDir2D::Up, &ctx);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 4));
        assert_eq!(tree.focused, 0);

        window_move_side!(tree, MoveDir2D::Down, &ctx);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.dimensions(), (3, 4));
    }

    #[test]
    fn test_window_resize_vertical_increase() {
        let (mut tree, ctx) = three_by_three();
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);

        // Draw so that everything gets an initial area.
        WindowLayout::default().render(area, &mut buffer, &mut tree);

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        // Top row before resizing.
        assert_eq!(tree.root.get(0).unwrap().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().term_area, Rect::new(67, 0, 33, 34));

        // Middle row before resizing.
        assert_eq!(tree.root.get(2).unwrap().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row before resizing.
        assert_eq!(tree.root.get(4).unwrap().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().term_area, Rect::new(67, 67, 33, 33));

        // Resize window 8.
        window_resize!(tree, Vertical, SizeChange::Increase(5.into()), &ctx);

        // Draw again so that we update the saved term_area.
        WindowLayout::default().render(area, &mut buffer, &mut tree);

        // Top row after resizing.
        assert_eq!(tree.root.get(0).unwrap().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().term_area, Rect::new(34, 0, 28, 34));
        assert_eq!(tree.root.get(6).unwrap().term_area, Rect::new(62, 0, 38, 34));

        // Middle row after resizing.
        assert_eq!(tree.root.get(2).unwrap().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().term_area, Rect::new(34, 34, 28, 33));
        assert_eq!(tree.root.get(7).unwrap().term_area, Rect::new(62, 34, 38, 33));

        // Bottom row after resizing has changed 5 and 8.
        assert_eq!(tree.root.get(4).unwrap().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().term_area, Rect::new(34, 67, 28, 33));
        assert_eq!(tree.root.get(8).unwrap().term_area, Rect::new(62, 67, 38, 33));
    }

    #[test]
    fn test_window_open_vertical_size() {
        let (mut tree, ctx) = three_by_three();
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);

        // Draw so that everything gets an initial area.
        WindowLayout::default().render(area, &mut buffer, &mut tree);

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        // Top row before opening window.
        assert_eq!(tree.root.get(0).unwrap().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().term_area, Rect::new(67, 0, 33, 34));

        // Middle row before opening window.
        assert_eq!(tree.root.get(2).unwrap().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row before opening window.
        assert_eq!(tree.root.get(4).unwrap().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().term_area, Rect::new(67, 67, 33, 33));

        // Open a window that is 5 columns wide.
        let w = TestWindow::new();
        window_open!(tree, w, Vertical, MoveDir1D::Previous, Count::Exact(5), &ctx);

        // We should now have one more window.
        assert_eq!(tree.root.size(), 10);

        // Draw again so that we update the saved term_area.
        WindowLayout::default().render(area, &mut buffer, &mut tree);

        // Top row after opening the window remains the same.
        assert_eq!(tree.root.get(0).unwrap().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().term_area, Rect::new(67, 0, 33, 34));

        // Middle row after opening the window remains the same.
        assert_eq!(tree.root.get(2).unwrap().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row after opening window has changed widths.
        assert_eq!(tree.root.get(4).unwrap().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().term_area, Rect::new(67, 67, 5, 33));
        assert_eq!(tree.root.get(9).unwrap().term_area, Rect::new(72, 67, 28, 33));
    }

    #[test]
    fn test_window_open_vertical_no_size() {
        let (mut tree, ctx) = three_by_three();
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);

        // Draw so that everything gets an initial area.
        WindowLayout::default().render(area, &mut buffer, &mut tree);

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        // Top row before opening window.
        assert_eq!(tree.root.get(0).unwrap().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().term_area, Rect::new(67, 0, 33, 34));

        // Middle row before opening window.
        assert_eq!(tree.root.get(2).unwrap().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row before opening window.
        assert_eq!(tree.root.get(4).unwrap().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().term_area, Rect::new(67, 67, 33, 33));

        // Open a new window without specifying a height.
        let w = TestWindow::new();
        window_open!(tree, w, Vertical, MoveDir1D::Previous, None, &ctx);

        // We should now have one more window.
        assert_eq!(tree.root.size(), 10);

        // Draw again so that we update the saved term_area.
        WindowLayout::default().render(area, &mut buffer, &mut tree);

        // Top row after opening the window remains the same.
        assert_eq!(tree.root.get(0).unwrap().term_area, Rect::new(0, 0, 25, 34));
        assert_eq!(tree.root.get(1).unwrap().term_area, Rect::new(25, 0, 25, 34));
        assert_eq!(tree.root.get(6).unwrap().term_area, Rect::new(50, 0, 50, 34));

        // Middle row after opening the window remains the same.
        assert_eq!(tree.root.get(2).unwrap().term_area, Rect::new(0, 34, 25, 33));
        assert_eq!(tree.root.get(3).unwrap().term_area, Rect::new(25, 34, 25, 33));
        assert_eq!(tree.root.get(7).unwrap().term_area, Rect::new(50, 34, 50, 33));

        // Bottom row after opening window has changed widths.
        assert_eq!(tree.root.get(4).unwrap().term_area, Rect::new(0, 67, 25, 33));
        assert_eq!(tree.root.get(5).unwrap().term_area, Rect::new(25, 67, 25, 33));
        assert_eq!(tree.root.get(8).unwrap().term_area, Rect::new(50, 67, 25, 33));
        assert_eq!(tree.root.get(9).unwrap().term_area, Rect::new(75, 67, 25, 33));
    }
}
