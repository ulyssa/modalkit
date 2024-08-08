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

use serde::{Deserialize, Serialize};

use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::Style,
    widgets::{Block, BorderType, Borders, StatefulWidget, Widget},
};

use crate::util::{rect_down, rect_right, rect_zero_height, rect_zero_width};
use crate::{TermOffset, TerminalCursor, Window, WindowOps};

use super::size::{ResizeInfo, ResizeInfoTrail, MIN_WIN_LEN};
use super::slot::WindowSlot;
use super::tree::{AxisTreeIterMut, SubtreeOps, TreeOps};
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
    WindowInfo,
};

use modalkit::actions::{Jumpable, WindowAction, WindowContainer, WindowCount};
use modalkit::errors::{EditError, EditResult, UIError, UIResult};
use modalkit::prelude::Axis::{Horizontal, Vertical};
use modalkit::prelude::MoveDir2D::{Down, Left, Right, Up};
use modalkit::prelude::*;
use modalkit::ui::idx_offset;

use modalkit::editing::{
    application::ApplicationInfo,
    context::{EditContext, Resolve},
    store::Store,
};

fn windex(count: &Count, ctx: &EditContext) -> usize {
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
    X: AxisT,
    Y: AxisT,
{
    let Some(ref lengths) = info.lengths else {
        return;
    };
    assert_eq!(lengths.len(), node.weight());

    fn winlen(sd: &super::size::SizeDescription, winrem: u16, lenrem: u16) -> u16 {
        if winrem == 0 {
            // Ignore sd.length for the last window.
            return lenrem;
        }

        // Make sure large window lengths don't steal too much.
        lenrem.saturating_sub(winrem * MIN_WIN_LEN).min(sd.length)
    }

    match X::axis() {
        Axis::Horizontal => {
            let mut carea = rect_zero_height(area);
            let mut iter = lengths.iter();
            let mut rem = area.height;

            let mut f = |value: &mut Value<W, X, Y>| {
                let size = iter.next().unwrap();
                let height = winlen(size, iter.as_slice().len() as u16, rem);

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
                let width = winlen(size, iter.as_slice().len() as u16, rem);

                carea = rect_right(carea, width);
                rem -= width;

                value.set_area(carea, info);
            };

            node.for_each_value(&mut f);
        },
    }
}

fn set_area_equal<W, X, Y>(node: &mut AxisTreeNode<W, X, Y>, area: Rect, info: &ResizeInfo)
where
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

enum WrappedIterMut<'a, W> {
    Horizontal(AxisTreeIterMut<'a, W, HorizontalT, VerticalT>),
    Vertical(AxisTreeIterMut<'a, W, VerticalT, HorizontalT>),
}

struct SlotIter<'a, W> {
    stack: Vec<WrappedIterMut<'a, WindowSlot<W>>>,
}

impl<'a, W> Iterator for SlotIter<'a, W> {
    type Item = &'a mut WindowSlot<W>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let last = self.stack.last_mut()?;

            match last {
                WrappedIterMut::Horizontal(node) => {
                    match node.next() {
                        None => {
                            let _ = self.stack.pop();
                        },
                        Some(Value::Window(w, _)) => {
                            return Some(w);
                        },
                        Some(Value::Tree(t, _)) => {
                            let iter = WrappedIterMut::Vertical(t.iter_mut());
                            self.stack.push(iter);
                            continue;
                        },
                    }
                },
                WrappedIterMut::Vertical(node) => {
                    match node.next() {
                        None => {
                            let _ = self.stack.pop();
                        },
                        Some(Value::Window(w, _)) => {
                            return Some(w);
                        },
                        Some(Value::Tree(t, _)) => {
                            let iter = WrappedIterMut::Horizontal(t.iter_mut());
                            self.stack.push(iter);
                            continue;
                        },
                    }
                },
            }
        }
    }
}

/// These operations are for manipulating the [Windows](Window) contained within the current tree
/// and all of the trees it contains along the rotated axis. The indices here are for all of the
/// reachable windows within this view of the layout, and not for the [Values](Value) within the
/// tree.
pub(super) trait LayoutOps<W, X, Y>
where
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
    fn neighbor(&self, at: usize, dir: MoveDir2D, count: usize) -> Option<usize>
    where
        W: TerminalCursor;
}

impl<W, X, Y> LayoutOps<W, X, Y> for Value<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
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

    fn neighbor(&self, _: usize, _: MoveDir2D, _: usize) -> Option<usize>
    where
        W: TerminalCursor,
    {
        unreachable!();
    }
}

type HorizontalTree<W> = AxisTree<W, HorizontalT, VerticalT>;

impl<W, X, Y> LayoutOps<W, X, Y> for AxisTreeNode<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
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

        if let (Some(awin), Some(bwin)) = (amut, bmut) {
            /*
             * If both indexes were valid, swap their windows.
             */
            std::mem::swap(awin, bwin);
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

    fn neighbor(&self, at: usize, dir: MoveDir2D, count: usize) -> Option<usize>
    where
        W: TerminalCursor,
    {
        let (w, r) = self.get_area(at)?;
        let c = w.get_term_cursor().unwrap_or((r.x, r.y));
        self._neighbor_of(0, at, c, dir, count).map(|current| current.0)
    }
}

impl<W, X, Y> LayoutOps<W, X, Y> for AxisTree<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
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

    fn neighbor(&self, at: usize, dir: MoveDir2D, count: usize) -> Option<usize>
    where
        W: TerminalCursor,
    {
        self.as_ref().and_then(|tree| tree.neighbor(at, dir, count))
    }
}

/// Data structure holding layout description and state
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(bound(deserialize = "I::WindowId: Deserialize<'de>"))]
#[serde(bound(serialize = "I::WindowId: Serialize"))]
#[serde(rename_all = "lowercase")]
pub struct WindowLayoutRoot<I: ApplicationInfo> {
    layout: WindowLayoutDescription<I>,
    focused: usize,
    zoomed: bool,
}

impl<I> WindowLayoutRoot<I>
where
    I: ApplicationInfo,
{
    /// Restore a layout from a description of windows and splits.
    pub fn to_layout<W: Window<I>>(
        self,
        area: Option<Rect>,
        store: &mut Store<I>,
    ) -> UIResult<WindowLayoutState<W, I>, I> {
        let mut layout = self.layout.to_layout(area, store)?;
        layout._focus(self.focused);
        layout.zoom = self.zoomed;
        Ok(layout)
    }
}

/// A description of a window layout.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(bound(deserialize = "I::WindowId: Deserialize<'de>"))]
#[serde(bound(serialize = "I::WindowId: Serialize"))]
#[serde(rename_all = "lowercase", tag = "type")]
pub enum WindowLayoutDescription<I: ApplicationInfo> {
    /// A single window.
    Window {
        /// The identifier for this window.
        window: I::WindowId,

        /// The length of this window within the parent split.
        length: Option<u16>,
    },

    /// A collection of adjacent windows.
    Split {
        /// The windows in this container.
        children: Vec<Self>,

        /// The length of this container within the parent split.
        length: Option<u16>,
    },
}

impl<I, W, X, Y> From<&Value<W, X, Y>> for WindowLayoutDescription<I>
where
    I: ApplicationInfo,
    W: Window<I>,
    X: AxisT,
    Y: AxisT,
{
    fn from(node: &Value<W, X, Y>) -> Self {
        match node {
            Value::Window(window, info) => {
                let length = if X::axis() == Axis::Horizontal {
                    info.area.height.into()
                } else {
                    info.area.width.into()
                };

                WindowLayoutDescription::Window { window: window.id(), length }
            },
            Value::Tree(tree, info) => {
                let length = if X::axis() == Axis::Horizontal {
                    info.area.height.into()
                } else {
                    info.area.width.into()
                };

                WindowLayoutDescription::Split {
                    children: tree.iter().map(Self::from).collect(),
                    length,
                }
            },
        }
    }
}

impl<I> WindowLayoutDescription<I>
where
    I: ApplicationInfo,
{
    fn into_value<W, X, Y>(self, store: &mut Store<I>) -> UIResult<Value<W, X, Y>, I>
    where
        W: Window<I>,
        X: AxisT,
        Y: AxisT,
    {
        match self {
            WindowLayoutDescription::Window { window, .. } => {
                let w = W::open(window, store)?;
                let v = Value::Window(w, WindowInfo::default());

                Ok(v)
            },
            WindowLayoutDescription::Split { children, .. } => {
                let mut layout: AxisTree<W, Y, X> = None;
                let sizes = children
                    .iter()
                    .map(|v| {
                        match v {
                            Self::Window { length, .. } | Self::Split { length, .. } => *length,
                        }
                    })
                    .collect::<Vec<_>>();

                for child in children {
                    let value = child.into_value(store)?;
                    layout.insert_max_value(value);
                }

                let Some(layout) = layout else {
                    let msg = "Cannot open empty split";
                    let err = UIError::Failure(msg.to_string());

                    return Err(err);
                };

                let lengths = if sizes.iter().any(Option::is_some) {
                    let mut l = layout.get_lengths();

                    for (sd, length) in l.iter_mut().zip(sizes.into_iter()) {
                        if let Some(length) = length {
                            sd.length = length;
                        }
                    }

                    Some(l)
                } else {
                    None
                };

                let resized = ResizeInfo { lengths };
                let info = TreeInfo { area: Rect::default(), resized };
                Ok(Value::Tree(layout, info))
            },
        }
    }

    /// Restore a layout from a description of windows and splits.
    pub fn to_layout<W: Window<I>>(
        self,
        area: Option<Rect>,
        store: &mut Store<I>,
    ) -> UIResult<WindowLayoutState<W, I>, I> {
        let mut layout = WindowLayoutState::empty();

        match self.into_value(store)? {
            Value::Window(w, _) => {
                layout.root.insert_min(w);
            },
            Value::Tree(tree, info) => {
                layout.root = Some(tree);
                layout.info = info;
            },
        }

        if let Some(area) = area {
            layout.root.set_area(area, &layout.info.resized);
            layout.info.area = area;
        }

        return Ok(layout);
    }
}

/// Manages the current layout and focus of [Windows](Window) on the screen.
pub struct WindowLayoutState<W: Window<I>, I: ApplicationInfo> {
    root: HorizontalTree<WindowSlot<W>>,
    info: TreeInfo,
    zoom: bool,
    focused: usize,
    focused_last: usize,
    _p: PhantomData<I>,
}

impl<W, I> WindowLayoutState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    /// Create a new instance containing a single [Window].
    pub fn new(window: W) -> Self {
        WindowLayoutState::from_slot(window.into())
    }

    /// Create a new instance without any windows.
    pub fn empty() -> Self {
        WindowLayoutState {
            root: None,
            info: TreeInfo::default(),
            zoom: false,
            focused: 0,
            focused_last: 0,
            _p: PhantomData,
        }
    }

    /// Convert this layout to a serializable summary of its windows and splits.
    pub fn as_description(&self) -> WindowLayoutRoot<I> {
        let mut children = vec![];
        let focused = self.focused;
        let zoomed = self.zoom;

        let Some(root) = &self.root else {
            return WindowLayoutRoot {
                layout: WindowLayoutDescription::Split { children, length: None },
                focused,
                zoomed,
            };
        };

        for w in root.iter() {
            children.push(w.into());
        }

        return WindowLayoutRoot {
            layout: WindowLayoutDescription::Split { children, length: None },
            focused,
            zoomed,
        };
    }

    /// Create a new instance containing a single [Window] displaying some content.
    pub fn from_target(
        &self,
        target: &OpenTarget<I::WindowId>,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<Self, I> {
        let w = self._open(target, ctx, store)?;
        let layout = WindowLayoutState::new(w);

        Ok(layout)
    }

    fn from_slot(slot: WindowSlot<W>) -> Self {
        WindowLayoutState {
            root: AxisTree::singleton(slot),
            info: TreeInfo::default(),
            zoom: false,
            focused: 0,
            focused_last: 0,
            _p: PhantomData,
        }
    }

    /// Iterate over the [WindowSlot] values in the window layout.
    fn slots(&mut self) -> SlotIter<'_, W> {
        let iter = SubtreeOps::iter_mut(&mut self.root);
        let stack = vec![WrappedIterMut::Horizontal(iter)];

        SlotIter { stack }
    }

    /// Fetch a reference to the currently focused [Window].
    pub fn get(&self) -> Option<&W> {
        self.get_slot().map(WindowSlot::get)
    }

    /// Fetch a mutable reference the currently focused [Window].
    pub fn get_mut(&mut self) -> Option<&mut W> {
        self.get_slot_mut().map(WindowSlot::get_mut)
    }

    /// Fetch a mutable reference the currently focused [WindowSlot].
    fn get_slot(&self) -> Option<&WindowSlot<W>> {
        self.root.get(self.focused)
    }

    /// Fetch a mutable reference the currently focused [WindowSlot].
    fn get_slot_mut(&mut self) -> Option<&mut WindowSlot<W>> {
        self.root.get_mut(self.focused)
    }

    /// Extract the currently focused [Window] into its own window layout.
    pub fn extract(&mut self) -> Self {
        let at = self.focused;
        let trail = ResizeInfoTrail::new(at, &mut self.info.resized, None);

        if let Some(slot) = self.root.close(at, trail) {
            self._clamp_focus();

            return WindowLayoutState::from_slot(slot);
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

        let nr = self.root.open(windex, w.into(), length, rel, axis, trail);
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

    fn resize(&mut self, windex: usize, axis: Axis, change: SizeChange<u16>) {
        self.freeze(axis);

        let trail = ResizeInfoTrail::new(windex, &mut self.info.resized, None);

        self.root.resize(windex, axis, change, trail);
        self.root.set_area(self.info.area, &self.info.resized);
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

    fn _open(
        &self,
        target: &OpenTarget<I::WindowId>,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<W, I> {
        match target {
            OpenTarget::Alternate => {
                let slot = self.get_slot().ok_or(UIError::NoWindow)?;

                match slot.get_alt() {
                    Some(alt) => Ok(alt.dup(store)),
                    None => Err(UIError::Failure("No alternate window".into())),
                }
            },
            OpenTarget::Application(id) => W::open(id.clone(), store),
            OpenTarget::Current => {
                let slot = self.get_slot().ok_or(UIError::NoWindow)?;

                Ok(slot.get().dup(store))
            },
            OpenTarget::Cursor(style) => {
                let slot = self.get_slot().ok_or(UIError::NoWindow)?;

                if let Some(text) = slot.get().get_cursor_word(style) {
                    W::find(text, store)
                } else {
                    let msg = "No word under cursor".to_string();
                    let err = UIError::Failure(msg);

                    Err(err)
                }
            },
            OpenTarget::List(count) => W::posn(ctx.resolve(count), store),
            OpenTarget::Name(name) => W::find(name.clone(), store),
            OpenTarget::Offset(dir, count) => {
                let slot = self.get_slot().ok_or(UIError::NoWindow)?;
                let count = ctx.resolve(count);

                match slot.get_off(*dir, count) {
                    Some(w) => Ok(w.dup(store)),
                    None => Err(UIError::Failure("Not a valid window offset".into())),
                }
            },
            OpenTarget::Selection => {
                let slot = self.get_slot().ok_or(UIError::NoWindow)?;

                if let Some(text) = slot.get().get_selected_word() {
                    W::find(text, store)
                } else {
                    let msg = "No text currently selected".to_string();
                    let err = UIError::Failure(msg);

                    Err(err)
                }
            },
            OpenTarget::Unnamed => W::unnamed(store),
        }
    }

    fn _target(&self, change: &FocusChange, ctx: &EditContext) -> Option<usize> {
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

impl<W, I> WindowActions<EditContext, I> for WindowLayoutState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn window_focus(
        &mut self,
        change: &FocusChange,
        ctx: &EditContext,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        if let Some(target) = self._target(change, ctx) {
            self.zoom = false;
            self._focus(target);
        }

        return Ok(None);
    }

    fn window_exchange(
        &mut self,
        change: &FocusChange,
        ctx: &EditContext,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        if let Some(target) = self._target(change, ctx) {
            self.zoom = false;
            self.root.swap(self.focused, target);
        }

        return Ok(None);
    }

    fn window_move_side(
        &mut self,
        dir: MoveDir2D,
        _: &EditContext,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        self.zoom = false;
        self.move_side(self.focused, dir);

        return Ok(None);
    }

    fn window_rotate(
        &mut self,
        _dir: MoveDir1D,
        _ctx: &EditContext,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        // XXX: implement
        let msg = "Window rotation is not currently implemented";
        let err = EditError::Unimplemented(msg.into());

        return Err(err);
    }

    fn window_split(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        axis: Axis,
        rel: MoveDir1D,
        count: &Count,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let count = ctx.resolve(count);
        let w = self._open(target, ctx, store)?;

        self.zoom = false;

        for _ in 0..count {
            self.open(w.dup(store), None, axis, rel);
        }

        return Ok(None);
    }

    fn window_clear_sizes(&mut self, _: &EditContext, _: &mut Store<I>) -> EditResult<EditInfo, I> {
        self.clear_sizes();

        return Ok(None);
    }

    fn window_resize(
        &mut self,
        change: &FocusChange,
        axis: Axis,
        size: &SizeChange<Count>,
        ctx: &EditContext,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let Some(target) = self._target(change, ctx) else {
            return Ok(None);
        };

        let change: SizeChange<u16> = match &size {
            SizeChange::Equal => SizeChange::Equal,
            SizeChange::Exact(count) => SizeChange::Exact(ctx.resolve(count).try_into()?),
            SizeChange::Increase(count) => SizeChange::Increase(ctx.resolve(count).try_into()?),
            SizeChange::Decrease(count) => SizeChange::Decrease(ctx.resolve(count).try_into()?),
        };

        self.zoom = false;
        self.resize(target, axis, change);

        return Ok(None);
    }

    fn window_close(
        &mut self,
        target: &WindowTarget,
        flags: CloseFlags,
        ctx: &EditContext,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let nwins = self.root.size();

        if nwins == 1 && !flags.contains(CloseFlags::QUIT) {
            /*
             * If the QUIT flag is not present, disallow closing the last window.
             */
            return Ok(None);
        }

        self.zoom = false;

        match target {
            WindowTarget::Single(focus) => {
                if let Some(target) = self._target(focus, ctx) {
                    self.close(target, flags);
                }

                return Ok(None);
            },
            WindowTarget::AllBut(focus) => {
                if let Some(target) = self._target(focus, ctx) {
                    self.only(target, flags);
                }

                return Ok(None);
            },
            WindowTarget::All => {
                self.root = None;

                return Ok(None);
            },
        }
    }

    fn window_open(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        axis: Axis,
        rel: MoveDir1D,
        count: &Count,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let count: u16 = ctx.resolve(count).try_into().map_err(EditError::from)?;
        let w = self._open(target, ctx, store)?;

        self.open(w, Some(count), axis, rel);

        Ok(None)
    }

    fn window_switch(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let slot = self.get_slot_mut().ok_or(UIError::NoWindow)?;

        let w = match target {
            OpenTarget::Alternate => {
                slot.alternate();

                return Ok(None);
            },
            OpenTarget::Application(id) => W::open(id.clone(), store)?,
            OpenTarget::Current => slot.get().dup(store),
            OpenTarget::Cursor(style) => {
                if let Some(text) = slot.get().get_cursor_word(style) {
                    W::find(text, store)?
                } else {
                    let msg = "No word under cursor".to_string();
                    let err = UIError::Failure(msg);

                    return Err(err);
                }
            },
            OpenTarget::List(count) => W::posn(ctx.resolve(count), store)?,
            OpenTarget::Name(name) => W::find(name.clone(), store)?,
            OpenTarget::Offset(dir, count) => {
                slot.offset(*dir, ctx.resolve(count));

                return Ok(None);
            },
            OpenTarget::Selection => {
                if let Some(text) = slot.get().get_selected_word() {
                    W::find(text, store)?
                } else {
                    let msg = "No text currently selected".to_string();
                    let err = UIError::Failure(msg);

                    return Err(err);
                }
            },
            OpenTarget::Unnamed => W::unnamed(store)?,
        };

        slot.open(w);

        Ok(None)
    }

    fn window_write(
        &mut self,
        target: &WindowTarget,
        path: Option<&str>,
        flags: WriteFlags,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        match target {
            WindowTarget::Single(focus) => {
                if let Some(target) = self._target(focus, ctx) {
                    if let Some(w) = self.root.get_mut(target) {
                        return w.write(path, flags, store);
                    }
                }

                return Ok(None);
            },
            WindowTarget::AllBut(focus) => {
                let target = self._target(focus, ctx);

                for (i, slot) in self.slots().enumerate() {
                    if matches!(target, Some(idx) if idx == i) {
                        continue;
                    }

                    let _ = slot.write(path, flags, store)?;
                }

                return Ok(None);
            },
            WindowTarget::All => {
                for slot in self.slots() {
                    let _ = slot.write(path, flags, store)?;
                }

                return Ok(None);
            },
        }
    }

    fn window_zoom_toggle(&mut self, _: &EditContext, _: &mut Store<I>) -> EditResult<EditInfo, I> {
        self.zoom = self.zoom.not();

        Ok(None)
    }
}

impl<W, I> WindowCount for WindowLayoutState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn windows(&self) -> usize {
        self.root.size()
    }
}

impl<W, C, I> Jumpable<C, I> for WindowLayoutState<W, I>
where
    W: Window<I> + Jumpable<C, I>,
    I: ApplicationInfo,
{
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &C,
    ) -> UIResult<usize, I> {
        self.get_slot_mut().ok_or(UIError::NoWindow)?.jump(list, dir, count, ctx)
    }
}

impl<W, I> WindowContainer<EditContext, Store<I>, I> for WindowLayoutState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn window_command(
        &mut self,
        action: &WindowAction<I>,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let info = match action {
            WindowAction::ClearSizes => self.window_clear_sizes(ctx, store)?,
            WindowAction::Close(target, flags) => self.window_close(target, *flags, ctx, store)?,
            WindowAction::Exchange(target) => self.window_exchange(target, ctx, store)?,
            WindowAction::Focus(target) => self.window_focus(target, ctx, store)?,
            WindowAction::MoveSide(dir) => self.window_move_side(*dir, ctx, store)?,
            WindowAction::Open(target, axis, rel, count) => {
                self.window_open(target, *axis, *rel, count, ctx, store)?
            },
            WindowAction::Resize(target, axis, size) => {
                self.window_resize(target, *axis, size, ctx, store)?
            },
            WindowAction::Rotate(dir) => self.window_rotate(*dir, ctx, store)?,
            WindowAction::Split(target, axis, rel, count) => {
                self.window_split(target, *axis, *rel, count, ctx, store)?
            },
            WindowAction::Switch(target) => self.window_switch(target, ctx, store)?,
            WindowAction::Write(target, path, flags) => {
                let path = path.as_ref().map(String::as_str);

                self.window_write(target, path, *flags, ctx, store)?
            },
            WindowAction::ZoomToggle => self.window_zoom_toggle(ctx, store)?,
            act => {
                let msg = format!("unknown window action: {act:?}");
                return Err(UIError::Unimplemented(msg));
            },
        };

        return Ok(info);
    }
}

/// Handles rendering the current window layout state to the terminal.
pub struct WindowLayout<'a, W: Window<I>, I: ApplicationInfo> {
    store: &'a mut Store<I>,
    focused: bool,

    borders: bool,
    border_style: Style,
    border_style_focused: Style,
    border_type: BorderType,

    _pw: PhantomData<(W, I)>,
}

impl<'a, W, I> WindowLayout<'a, W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    /// Create a new widget for displaying window layouts.
    pub fn new(store: &'a mut Store<I>) -> Self {
        WindowLayout {
            store,
            focused: false,
            borders: false,
            border_style: Style::default(),
            border_style_focused: Style::default(),
            border_type: BorderType::Plain,
            _pw: PhantomData,
        }
    }

    /// What [Style] should be used when drawing borders.
    pub fn border_style(mut self, style: Style) -> Self {
        self.border_style = style;
        self
    }

    /// What [Style] should be used when drawing the border of the selected window.
    pub fn border_style_focused(mut self, style: Style) -> Self {
        self.border_style_focused = style;
        self
    }

    /// What characters should be used when drawing borders.
    pub fn border_type(mut self, border_type: BorderType) -> Self {
        self.border_type = border_type;
        self
    }

    /// Indicate whether to draw borders around windows.
    pub fn borders(mut self, borders: bool) -> Self {
        self.borders = borders;
        self
    }

    /// Indicate whether the window layout tree is currently focused.
    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }

    fn _draw<X, Y>(
        &mut self,
        node: &mut AxisTreeNode<WindowSlot<W>, X, Y>,
        focus: Option<usize>,
        _outer: Rect,
        buf: &mut Buffer,
    ) where
        X: AxisT,
        Y: AxisT,
    {
        let mut base = 0;
        let mut f = |value: &mut Value<WindowSlot<W>, X, Y>| {
            let focus = focus.and_then(|n| n.checked_sub(base));

            match value {
                Value::Window(w, info) => {
                    let focused = matches!(focus, Some(0));

                    if self.borders {
                        let title = w.get().get_win_title(self.store);
                        let block = Block::default()
                            .title(title)
                            .borders(Borders::ALL)
                            .border_style(if focused {
                                self.border_style_focused
                            } else {
                                self.border_style
                            })
                            .border_type(self.border_type);
                        let inner = block.inner(info.area);

                        block.render(info.area, buf);
                        w.draw(inner, buf, focused, self.store);
                    } else {
                        w.draw(info.area, buf, focused, self.store);
                    }
                },
                Value::Tree(t, _) => {
                    self._draw(t, focus, _outer, buf);
                },
            }

            base += value.size();
        };

        node.for_each_value(&mut f);
    }
}

impl<'a, W, I> StatefulWidget for WindowLayout<'a, W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    type State = WindowLayoutState<W, I>;

    fn render(mut self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if state.zoom {
            if let Some(window) = state.get_mut() {
                window.draw(area, buf, self.focused, self.store);
            }

            return;
        }

        let focused = self.focused.then_some(state.focused);

        state.info.area = area;
        state.root.set_area(area, &state.info.resized);

        if let Some(root) = &mut state.root {
            self._draw(root.as_mut(), focused, area, buf);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerminalCursor;
    use modalkit::editing::{completion::CompletionList, store::Store};
    use modalkit::errors::EditError;
    use rand::Rng;
    use ratatui::text::Line;

    macro_rules! fc {
        ($n: expr) => {
            FocusChange::Offset(Count::Exact($n), false)
        };
        ($n: expr, $b: expr) => {
            FocusChange::Offset(Count::Exact($n), $b)
        };
    }

    macro_rules! window_close {
        ($tree: expr, $ct: expr, $flags: expr, $ctx: expr, $store: expr) => {
            $tree.window_close(&$ct, $flags, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! window_exchange {
        ($tree: expr, $fc: expr, $ctx: expr, $store: expr) => {
            $tree.window_exchange(&$fc, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! window_focus {
        ($tree: expr, $fc: expr, $ctx: expr, $store: expr) => {
            $tree.window_focus(&$fc, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! window_focus_off {
        ($tree: expr, $c: expr, $ctx: expr, $store: expr) => {
            window_focus!($tree, fc!($c), $ctx, $store)
        };
    }

    macro_rules! window_focus_1d {
        ($tree: expr, $dir: expr, $c: expr, $ctx: expr, $store: expr) => {
            window_focus!($tree, FocusChange::Direction1D($dir, $c, true), $ctx, $store)
        };
    }

    macro_rules! window_focus_2d {
        ($tree: expr, $dir: expr, $c: expr, $ctx: expr, $store: expr) => {
            window_focus!($tree, FocusChange::Direction2D($dir, $c), $ctx, $store)
        };
    }

    macro_rules! window_move_side {
        ($tree: expr, $dir: expr, $ctx: expr, $store: expr) => {
            $tree.window_move_side($dir, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! window_resize {
        ($tree: expr, $change: expr, $axis: expr, $szch: expr, $ctx: expr, $store: expr) => {
            $tree.window_resize(&$change, $axis, &$szch, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! window_switch {
        ($tree: expr, $target: expr, $ctx: expr, $store: expr) => {
            $tree.window_switch(&$target, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! window_split {
        ($tree: expr, $axis: expr, $dir: expr, $count: expr, $ctx: expr, $store: expr) => {
            $tree
                .window_split(&OpenTarget::Current, $axis, $dir, &$count, $ctx, &mut $store)
                .unwrap()
        };
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum TestApp {}

    impl ApplicationInfo for TestApp {
        type Error = String;
        type Action = ();
        type Store = ();
        type WindowId = Option<usize>;
        type ContentId = Option<usize>;

        fn content_of_command(_: CommandType) -> Option<usize> {
            None
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct TestWindow {
        dirty: bool,
        term_area: Rect,
        id: Option<usize>,
    }

    impl TestWindow {
        fn new() -> Self {
            TestWindow { dirty: true, term_area: Rect::default(), id: None }
        }
    }

    impl From<Option<usize>> for TestWindow {
        fn from(id: Option<usize>) -> Self {
            TestWindow { dirty: true, term_area: Rect::default(), id }
        }
    }

    impl TerminalCursor for TestWindow {
        fn get_term_cursor(&self) -> Option<(u16, u16)> {
            (self.term_area.left(), self.term_area.top()).into()
        }
    }

    impl WindowOps<TestApp> for TestWindow {
        fn dup(&self, _: &mut Store<TestApp>) -> Self {
            self.clone()
        }

        fn close(&mut self, flags: CloseFlags, store: &mut Store<TestApp>) -> bool {
            if !flags.contains(CloseFlags::WRITE) {
                return true;
            }

            let flags = if flags.contains(CloseFlags::FORCE) {
                WriteFlags::FORCE
            } else {
                WriteFlags::NONE
            };

            self.write(None, flags, store).is_ok()
        }

        fn write(
            &mut self,
            _: Option<&str>,
            flags: WriteFlags,
            _: &mut Store<TestApp>,
        ) -> UIResult<EditInfo, TestApp> {
            if flags.contains(WriteFlags::FORCE) {
                self.dirty = false;
                Ok(None)
            } else {
                Err(UIError::Failure("Cannot write".into()))
            }
        }

        fn draw(&mut self, area: Rect, _: &mut Buffer, _: bool, _: &mut Store<TestApp>) {
            self.term_area = area;
        }

        fn get_completions(&self) -> Option<CompletionList> {
            None
        }

        fn get_cursor_word(&self, _: &WordStyle) -> Option<String> {
            None
        }

        fn get_selected_word(&self) -> Option<String> {
            None
        }
    }

    impl Window<TestApp> for TestWindow {
        fn id(&self) -> Option<usize> {
            self.id
        }

        fn get_win_title(&self, _: &mut Store<TestApp>) -> Line {
            Line::from("Window Title")
        }

        fn open(id: Option<usize>, _: &mut Store<TestApp>) -> UIResult<Self, TestApp> {
            Ok(TestWindow::from(id))
        }

        fn find(name: String, _: &mut Store<TestApp>) -> UIResult<Self, TestApp> {
            match name.parse::<usize>() {
                Ok(n) => Ok(TestWindow::from(Some(n))),
                Err(e) => Err(EditError::from(e).into()),
            }
        }

        fn posn(index: usize, _: &mut Store<TestApp>) -> UIResult<Self, TestApp> {
            Ok(TestWindow::from(Some(index)))
        }

        fn unnamed(_: &mut Store<TestApp>) -> UIResult<Self, TestApp> {
            Ok(TestWindow::from(None))
        }
    }

    impl<C> Jumpable<C, TestApp> for TestWindow {
        fn jump(
            &mut self,
            _: PositionList,
            _: MoveDir1D,
            count: usize,
            _: &C,
        ) -> UIResult<usize, TestApp> {
            return Ok(count);
        }
    }

    fn mkstorectx() -> (Store<TestApp>, EditContext) {
        (Store::default(), EditContext::default())
    }

    fn mktree() -> (WindowLayoutState<TestWindow, TestApp>, Store<TestApp>, EditContext) {
        let (store, ctx) = mkstorectx();
        let tree = WindowLayoutState::new(TestWindow::new());

        return (tree, store, ctx);
    }

    fn three_by_three() -> (WindowLayoutState<TestWindow, TestApp>, Store<TestApp>, EditContext) {
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
        let (mut tree, mut store, ctx) = mktree();

        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx, store);
        window_split!(tree, Axis::Horizontal, MoveDir1D::Previous, Count::Exact(2), &ctx, store);
        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx, store);
        window_focus_off!(tree, 3, &ctx, store);
        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx, store);
        window_focus_off!(tree, 5, &ctx, store);
        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Exact(1), &ctx, store);
        window_focus_off!(tree, 7, &ctx, store);
        window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(2), &ctx, store);

        let mut idx = 0;

        while let Some(slot) = tree.root.get_mut(idx) {
            slot.get_mut().id = Some(idx);
            idx += 1;
        }

        return (tree, store, ctx);
    }

    #[test]
    fn test_tree_get() {
        let (mut tree, _, _) = three_by_three();

        assert_eq!(tree.root.get(10), None);
        assert_eq!(tree.root.get(100), None);

        assert_eq!(tree.root.get_mut(10), None);
        assert_eq!(tree.root.get_mut(100), None);

        assert_eq!(tree.root.get(0).unwrap().get().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().get().id, Some(8));

        assert_eq!(tree.root.get_mut(0).unwrap().get().id, Some(0));
        assert_eq!(tree.root.get_mut(8).unwrap().get().id, Some(8));
    }

    #[test]
    fn test_tree_rotations() {
        let (mut store, ctx) = mkstorectx();
        let flags = CloseFlags::QUIT;

        // Repeatedly delete the last window.
        let mut tree = WindowLayoutState::new(TestWindow::new());
        window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(99), &ctx, store);
        assert_eq!(tree.root.size(), 100);

        for n in 0..100 {
            window_close!(tree, WindowTarget::Single(fc!(100 - n)), flags, &ctx, store);
            assert_eq!(tree.root.size(), 99 - n);
        }

        // Repeatedly delete the first window.
        let mut tree = WindowLayoutState::new(TestWindow::new());
        window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(99), &ctx, store);
        assert_eq!(tree.root.size(), 100);

        for n in 0..100 {
            window_close!(tree, WindowTarget::Single(fc!(1)), flags, &ctx, store);
            assert_eq!(tree.root.size(), 99 - n);
        }

        // Generate random insertions and then deletions.
        let mut tree = WindowLayoutState::new(TestWindow::new());
        let mut rng = rand::thread_rng();

        assert_eq!(tree.root.size(), 1);

        for n in 0..999 {
            let size = tree.root.size();
            let target = rng.gen_range(1..=size);
            window_focus_off!(tree, target, &ctx, store);
            window_split!(tree, Axis::Horizontal, MoveDir1D::Next, Count::Exact(1), &ctx, store);
            assert_eq!(tree.root.size(), 2 + n);
        }

        for n in 0..1000 {
            let size = tree.root.size();
            let target = rng.gen_range(1..=size);
            window_close!(tree, WindowTarget::Single(fc!(target)), flags, &ctx, store);
            assert_eq!(tree.root.size(), 999 - n);
        }
    }

    #[test]
    fn test_window_split() {
        let (mut store, ctx) = mkstorectx();
        let mut tree = WindowLayoutState::new(TestWindow::new());

        assert_eq!(tree.root.size(), 1);
        assert_eq!(tree.focused, 0);

        window_split!(tree, Axis::Horizontal, MoveDir1D::Previous, Count::Contextual, &ctx, store);
        assert_eq!(tree.root.size(), 2);
        assert_eq!(tree.focused, 0);

        window_split!(tree, Axis::Vertical, MoveDir1D::Previous, Count::Contextual, &ctx, store);
        assert_eq!(tree.root.size(), 3);
        assert_eq!(tree.focused, 0);

        window_split!(tree, Axis::Vertical, MoveDir1D::Next, Count::Contextual, &ctx, store);
        assert_eq!(tree.root.size(), 4);
        assert_eq!(tree.focused, 1);
    }

    #[test]
    fn test_window_nav_1d() {
        let (mut store, ctx) = mkstorectx();
        let mut tree = WindowLayoutState::new(TestWindow::new());

        window_split!(tree, Axis::Horizontal, MoveDir1D::Previous, Count::Exact(5), &ctx, store);
        assert_eq!(tree.root.size(), 6);
        assert_eq!(tree.focused, 0);

        window_focus_1d!(tree, MoveDir1D::Next, Count::Exact(2), &ctx, store);
        assert_eq!(tree.focused, 2);

        window_focus_1d!(tree, MoveDir1D::Previous, Count::Exact(1), &ctx, store);
        assert_eq!(tree.focused, 1);

        window_focus_1d!(tree, MoveDir1D::Previous, Count::Exact(2), &ctx, store);
        assert_eq!(tree.focused, 5);

        window_focus_1d!(tree, MoveDir1D::Next, Count::Exact(1), &ctx, store);
        assert_eq!(tree.focused, 0);
    }

    #[test]
    fn test_window_nav_2d() {
        let (mut tree, mut store, ctx) = three_by_three();

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);

        // Draw so we know where the windows are inside the terminal.
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);
        let widget = WindowLayout::new(&mut store);
        widget.render(area, &mut buffer, &mut tree);

        // Move left 2 windows.
        window_focus_2d!(tree, MoveDir2D::Left, Count::Exact(2), &ctx, store);
        assert_eq!(tree.focused, 4);

        // Move right 1 window.
        window_focus_2d!(tree, MoveDir2D::Right, Count::Exact(1), &ctx, store);
        assert_eq!(tree.focused, 5);

        // Move up 2 windows.
        window_focus_2d!(tree, MoveDir2D::Up, Count::Exact(2), &ctx, store);
        assert_eq!(tree.focused, 1);

        // We're already at the top, so we can't move up anymore.
        window_focus_2d!(tree, MoveDir2D::Up, Count::Exact(1), &ctx, store);
        assert_eq!(tree.focused, 1);

        // Move down 1 window.
        window_focus_2d!(tree, MoveDir2D::Down, Count::Exact(1), &ctx, store);
        assert_eq!(tree.focused, 3);

        // Try to move left 3 windows, stopping at the left side.
        window_focus_2d!(tree, MoveDir2D::Left, Count::Exact(3), &ctx, store);
        assert_eq!(tree.focused, 2);
    }

    #[test]
    fn test_window_close() {
        let (mut tree, mut store, ctx) = three_by_three();
        let flags = CloseFlags::NONE;

        let target = WindowTarget::Single(fc!(8));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 8);

        let target = WindowTarget::Single(fc!(6));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 7);

        let target = WindowTarget::Single(fc!(3));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 6);

        let target = WindowTarget::Single(fc!(2));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
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
        assert_eq!(tree.root.get(0).unwrap().get().id, Some(0));
        assert_eq!(tree.root.get(1).unwrap().get().id, Some(3));
        assert_eq!(tree.root.get(2).unwrap().get().id, Some(4));
        assert_eq!(tree.root.get(3).unwrap().get().id, Some(6));
        assert_eq!(tree.root.get(4).unwrap().get().id, Some(8));

        let target = WindowTarget::Single(fc!(2));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 4);

        let target = WindowTarget::Single(fc!(3));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 3);

        let target = WindowTarget::Single(fc!(3));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 2);

        let target = WindowTarget::Single(fc!(1));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 1);

        // Can't close last window because flags are NONE.
        let target = WindowTarget::Single(fc!(1));
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 1);

        // Passing QUIT does the job.
        let target = WindowTarget::Single(fc!(1));
        let flags = CloseFlags::QUIT;
        assert!(window_close!(tree, target, flags, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 0);
    }

    #[test]
    fn test_window_close_allbut() {
        for idx in 1usize..=9usize {
            let (mut tree, mut store, ctx) = three_by_three();
            let target = WindowTarget::AllBut(fc!(idx));
            assert!(window_close!(tree, target, CloseFlags::NONE, &ctx, store).is_none());
            assert_eq!(tree.root.size(), 1);
            assert_eq!(tree.get().unwrap().id, Some(idx.saturating_sub(1)));
        }
    }

    #[test]
    fn test_window_close_all() {
        let (mut tree, mut store, ctx) = three_by_three();
        let target = WindowTarget::All;
        assert!(window_close!(tree, target, CloseFlags::NONE, &ctx, store).is_none());
        assert_eq!(tree.root.size(), 0);
    }

    #[test]
    fn test_window_write() {
        let (mut tree, mut store, ctx) = mktree();
        let target = WindowTarget::All;

        // Window starts out with a dirty state.
        assert_eq!(tree.get().unwrap().dirty, true);

        // Write without FORCE fails.
        let res = tree.window_write(&target, None, WriteFlags::NONE, &ctx, &mut store);
        assert!(res.is_err());

        // Write with FORCE succeeds.
        tree.window_write(&target, None, WriteFlags::FORCE, &ctx, &mut store)
            .unwrap();
        assert_eq!(tree.get().unwrap().dirty, false);
    }

    #[test]
    fn test_window_exchange() {
        let (mut tree, mut store, ctx) = three_by_three();

        // Verify starting conditions.
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.get(0).unwrap().get().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().get().id, Some(8));

        // Swap window 9 with window 1.
        window_exchange!(tree, &fc!(1), &ctx, store);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.get(0).unwrap().get().id, Some(8));
        assert_eq!(tree.root.get(8).unwrap().get().id, Some(0));

        // Swap the new window 9 with window 8.
        window_exchange!(tree, &fc!(8), &ctx, store);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.get(0).unwrap().get().id, Some(8));
        assert_eq!(tree.root.get(7).unwrap().get().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().get().id, Some(7));

        // Focus on window 1, and swap with window 6.
        window_focus_off!(tree, 1, &ctx, store);
        assert_eq!(tree.focused, 0);

        window_exchange!(tree, &fc!(6), &ctx, store);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 0);
        assert_eq!(tree.root.get(0).unwrap().get().id, Some(5));
        assert_eq!(tree.root.get(5).unwrap().get().id, Some(8));
        assert_eq!(tree.root.get(7).unwrap().get().id, Some(0));
        assert_eq!(tree.root.get(8).unwrap().get().id, Some(7));
    }

    #[test]
    fn test_window_move_side() {
        let (mut tree, mut store, ctx) = three_by_three();

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        window_move_side!(tree, MoveDir2D::Left, &ctx, store);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (4, 3));
        assert_eq!(tree.focused, 0);

        window_move_side!(tree, MoveDir2D::Right, &ctx, store);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (4, 3));
        assert_eq!(tree.focused, 8);

        window_move_side!(tree, MoveDir2D::Up, &ctx, store);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 4));
        assert_eq!(tree.focused, 0);

        window_move_side!(tree, MoveDir2D::Down, &ctx, store);
        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.focused, 8);
        assert_eq!(tree.root.dimensions(), (3, 4));
    }

    #[test]
    fn test_window_resize_vertical_increase() {
        let (mut tree, mut store, ctx) = three_by_three();
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);

        // Draw so that everything gets an initial area.
        WindowLayout::new(&mut store).render(area, &mut buffer, &mut tree);

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        // Top row before resizing.
        assert_eq!(tree.root.get(0).unwrap().get().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().get().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().get().term_area, Rect::new(67, 0, 33, 34));

        // Middle row before resizing.
        assert_eq!(tree.root.get(2).unwrap().get().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().get().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().get().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row before resizing.
        assert_eq!(tree.root.get(4).unwrap().get().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().get().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().get().term_area, Rect::new(67, 67, 33, 33));

        // Resize window 8.
        window_resize!(
            tree,
            FocusChange::Offset(8.into(), false),
            Vertical,
            SizeChange::Increase(5.into()),
            &ctx,
            store
        );

        // Draw again so that we update the saved term_area.
        WindowLayout::new(&mut store).render(area, &mut buffer, &mut tree);

        // Top row after resizing.
        assert_eq!(tree.root.get(0).unwrap().get().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().get().term_area, Rect::new(34, 0, 28, 34));
        assert_eq!(tree.root.get(6).unwrap().get().term_area, Rect::new(62, 0, 38, 34));

        // Middle row after resizing.
        assert_eq!(tree.root.get(2).unwrap().get().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().get().term_area, Rect::new(34, 34, 28, 33));
        assert_eq!(tree.root.get(7).unwrap().get().term_area, Rect::new(62, 34, 38, 33));

        // Bottom row after resizing has changed 5 and 8.
        assert_eq!(tree.root.get(4).unwrap().get().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().get().term_area, Rect::new(34, 67, 28, 33));
        assert_eq!(tree.root.get(8).unwrap().get().term_area, Rect::new(62, 67, 38, 33));
    }

    #[test]
    fn test_window_open_vertical_size() {
        let (mut tree, mut store, _) = three_by_three();
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);

        // Draw so that everything gets an initial area.
        WindowLayout::new(&mut store).render(area, &mut buffer, &mut tree);

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        // Top row before opening window.
        assert_eq!(tree.root.get(0).unwrap().get().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().get().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().get().term_area, Rect::new(67, 0, 33, 34));

        // Middle row before opening window.
        assert_eq!(tree.root.get(2).unwrap().get().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().get().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().get().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row before opening window.
        assert_eq!(tree.root.get(4).unwrap().get().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().get().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().get().term_area, Rect::new(67, 67, 33, 33));

        // Open a window that is 5 columns wide.
        let w = TestWindow::new();
        tree.open(w, Some(5), Vertical, MoveDir1D::Previous);

        // We should now have one more window.
        assert_eq!(tree.root.size(), 10);

        // Draw again so that we update the saved term_area.
        WindowLayout::new(&mut store).render(area, &mut buffer, &mut tree);

        // Top row after opening the window remains the same.
        assert_eq!(tree.root.get(0).unwrap().get().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().get().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().get().term_area, Rect::new(67, 0, 33, 34));

        // Middle row after opening the window remains the same.
        assert_eq!(tree.root.get(2).unwrap().get().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().get().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().get().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row after opening window has changed widths.
        assert_eq!(tree.root.get(4).unwrap().get().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().get().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().get().term_area, Rect::new(67, 67, 5, 33));
        assert_eq!(tree.root.get(9).unwrap().get().term_area, Rect::new(72, 67, 28, 33));
    }

    #[test]
    fn test_window_open_vertical_no_size() {
        let (mut tree, mut store, _) = three_by_three();
        let mut buffer = Buffer::empty(Rect::new(0, 0, 100, 100));
        let area = Rect::new(0, 0, 100, 100);

        // Draw so that everything gets an initial area.
        WindowLayout::new(&mut store).render(area, &mut buffer, &mut tree);

        assert_eq!(tree.root.size(), 9);
        assert_eq!(tree.root.dimensions(), (3, 3));
        assert_eq!(tree.focused, 8);

        // Top row before opening window.
        assert_eq!(tree.root.get(0).unwrap().get().term_area, Rect::new(0, 0, 34, 34));
        assert_eq!(tree.root.get(1).unwrap().get().term_area, Rect::new(34, 0, 33, 34));
        assert_eq!(tree.root.get(6).unwrap().get().term_area, Rect::new(67, 0, 33, 34));

        // Middle row before opening window.
        assert_eq!(tree.root.get(2).unwrap().get().term_area, Rect::new(0, 34, 34, 33));
        assert_eq!(tree.root.get(3).unwrap().get().term_area, Rect::new(34, 34, 33, 33));
        assert_eq!(tree.root.get(7).unwrap().get().term_area, Rect::new(67, 34, 33, 33));

        // Bottom row before opening window.
        assert_eq!(tree.root.get(4).unwrap().get().term_area, Rect::new(0, 67, 34, 33));
        assert_eq!(tree.root.get(5).unwrap().get().term_area, Rect::new(34, 67, 33, 33));
        assert_eq!(tree.root.get(8).unwrap().get().term_area, Rect::new(67, 67, 33, 33));

        // Open a new window without specifying a height.
        let w = TestWindow::new();
        tree.open(w, None, Vertical, MoveDir1D::Previous);

        // We should now have one more window.
        assert_eq!(tree.root.size(), 10);

        // Draw again so that we update the saved term_area.
        WindowLayout::new(&mut store).render(area, &mut buffer, &mut tree);

        // Top row after opening the window remains the same.
        assert_eq!(tree.root.get(0).unwrap().get().term_area, Rect::new(0, 0, 25, 34));
        assert_eq!(tree.root.get(1).unwrap().get().term_area, Rect::new(25, 0, 25, 34));
        assert_eq!(tree.root.get(6).unwrap().get().term_area, Rect::new(50, 0, 50, 34));

        // Middle row after opening the window remains the same.
        assert_eq!(tree.root.get(2).unwrap().get().term_area, Rect::new(0, 34, 25, 33));
        assert_eq!(tree.root.get(3).unwrap().get().term_area, Rect::new(25, 34, 25, 33));
        assert_eq!(tree.root.get(7).unwrap().get().term_area, Rect::new(50, 34, 50, 33));

        // Bottom row after opening window has changed widths.
        assert_eq!(tree.root.get(4).unwrap().get().term_area, Rect::new(0, 67, 25, 33));
        assert_eq!(tree.root.get(5).unwrap().get().term_area, Rect::new(25, 67, 25, 33));
        assert_eq!(tree.root.get(8).unwrap().get().term_area, Rect::new(50, 67, 25, 33));
        assert_eq!(tree.root.get(9).unwrap().get().term_area, Rect::new(75, 67, 25, 33));
    }

    #[test]
    fn test_window_switch_and_jump() {
        let (mut store, ctx) = mkstorectx();
        let mut tree = WindowLayoutState::new(TestWindow::new());
        let next = MoveDir1D::Next;
        let prev = MoveDir1D::Previous;
        let jl = PositionList::JumpList;

        window_switch!(tree, OpenTarget::Name("1".into()), &ctx, store);
        assert_eq!(tree.get().unwrap().id, Some(1));

        window_switch!(tree, OpenTarget::Name("2".into()), &ctx, store);
        assert_eq!(tree.get().unwrap().id, Some(2));

        window_switch!(tree, OpenTarget::Application(3.into()), &ctx, store);
        assert_eq!(tree.get().unwrap().id, Some(3));

        window_switch!(tree, OpenTarget::Alternate, &ctx, store);
        assert_eq!(tree.get().unwrap().id, Some(2));

        let count = tree.jump(jl, prev, 1, &ctx).unwrap();
        assert_eq!(count, 0);
        assert_eq!(tree.get().unwrap().id, Some(3));

        let count = tree.jump(jl, prev, 1, &ctx).unwrap();
        assert_eq!(count, 0);
        assert_eq!(tree.get().unwrap().id, Some(1));

        let count = tree.jump(jl, prev, 1, &ctx).unwrap();
        assert_eq!(count, 0);
        assert_eq!(tree.get().unwrap().id, None);

        let count = tree.jump(jl, next, 2, &ctx).unwrap();
        assert_eq!(count, 0);
        assert_eq!(tree.get().unwrap().id, Some(3));

        let count = tree.jump(jl, next, 2, &ctx).unwrap();
        assert_eq!(count, 1);
        assert_eq!(tree.get().unwrap().id, Some(2));
    }

    #[test]
    fn test_layout_as_description() {
        use WindowLayoutDescription::{Split, Window};

        let (mut tree, mut store, _) = three_by_three();
        let mut buffer = Buffer::empty(Rect::new(0, 0, 60, 60));
        let area = Rect::new(0, 0, 60, 60);
        tree._focus(3);

        // Draw so that everything gets an initial area.
        WindowLayout::new(&mut store).render(area, &mut buffer, &mut tree);

        let desc1 = tree.as_description();
        let exp = WindowLayoutDescription::<TestApp>::Split {
            children: vec![Split {
                children: vec![
                    Split {
                        children: vec![
                            Split {
                                children: vec![
                                    Window { window: Some(0), length: Some(20) },
                                    Window { window: Some(1), length: Some(20) },
                                ],
                                length: Some(20),
                            },
                            Split {
                                children: vec![
                                    Window { window: Some(2), length: Some(20) },
                                    Window { window: Some(3), length: Some(20) },
                                ],
                                length: Some(20),
                            },
                            Split {
                                children: vec![
                                    Window { window: Some(4), length: Some(20) },
                                    Window { window: Some(5), length: Some(20) },
                                ],
                                length: Some(20),
                            },
                        ],
                        length: Some(40),
                    },
                    Split {
                        children: vec![
                            Window { window: Some(6), length: Some(20) },
                            Window { window: Some(7), length: Some(20) },
                            Window { window: Some(8), length: Some(20) },
                        ],
                        length: Some(20),
                    },
                ],
                length: Some(60),
            }],
            length: None,
        };
        assert_eq!(desc1.layout, exp);
        assert_eq!(desc1.focused, 3);
        assert_eq!(desc1.zoomed, false);

        // Turn back into a layout, and then generate a new description to show it's the same.
        let tree = desc1
            .clone()
            .to_layout::<TestWindow>(tree.info.area.into(), &mut store)
            .unwrap();
        assert_eq!(tree.as_description(), desc1);

        // Test against an example JSON serialization to test naming.
        let serialized = serde_json::to_string_pretty(&desc1).unwrap();
        let exp = include_str!("../../tests/window-layout.json");
        assert_eq!(serialized, exp.trim_end());
    }
}
