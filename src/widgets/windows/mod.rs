//! # Tiled Window Layouts
//!
//! ## Overview
//!
//! This widget handles the logic for creating multi-window layouts. Some of its features are:
//!
//! * Horizontally and vertically split windows into subregions.
//! * Resize windows along either axis
//! * Move windows around
//! * Move focus between windows in 2-dimensional terms (up, down, left, right)
//!
use std::cmp::Ordering;
use std::marker::PhantomData;

use tui::layout::Rect;

use crate::editing::base::Axis;

use super::Window;

use self::layout::LayoutOps;
use self::size::{ResizeInfo, SizeDescription, MIN_WIN_LEN};
use self::tree::SubtreeOps;

mod layout;
mod size;
mod tree;

pub use self::layout::{WindowLayout, WindowLayoutState};

pub(self) struct AxisTreeNode<W: Window, X: AxisT, Y: AxisT> {
    pub(self) value: Value<W, X, Y>,
    pub(self) info: Info,
    pub(self) left: Option<Box<Self>>,
    pub(self) right: Option<Box<Self>>,
    _p: PhantomData<(X, Y)>,
}

pub(self) type AxisTree<W, X, Y> = Option<Box<AxisTreeNode<W, X, Y>>>;

struct Info {
    dimensions: (usize, usize),
    size: usize,
    weight: usize,
}

impl Info {
    fn from<W: Window, X: AxisT, Y: AxisT>(
        value: &Value<W, X, Y>,
        left: &AxisTree<W, X, Y>,
        right: &AxisTree<W, X, Y>,
    ) -> Info {
        let size = left.size() + value.size() + right.size();
        let weight = left.weight() + 1 + right.weight();

        let (lw, lh) = left.dimensions();
        let (vw, vh) = value.dimensions();
        let (rw, rh) = right.dimensions();

        let dimensions = match X::axis() {
            Axis::Horizontal => (lw.max(rw).max(vw), lh + rh + vh),
            Axis::Vertical => (lw + rw + vw, lh.max(rh).max(vh)),
        };

        Info { size, weight, dimensions }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct TreeInfo {
    area: Rect,
    resized: ResizeInfo,
}

impl Default for TreeInfo {
    fn default() -> Self {
        TreeInfo {
            area: Rect::default(),
            resized: ResizeInfo::default(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct WindowInfo {
    area: Rect,
}

impl WindowInfo {
    fn to_tree(&self, dimension: Option<Axis>) -> TreeInfo {
        let mut ri = ResizeInfo::default();

        if let Some(axis) = dimension {
            let len = self.get_length(axis);
            let sd = SizeDescription::new(0..1, len, MIN_WIN_LEN);

            ri.lengths = Some(vec![sd]);
        }

        TreeInfo { area: self.area, resized: ri }
    }

    fn get_length(&self, axis: Axis) -> u16 {
        match axis {
            Axis::Horizontal => self.area.height,
            Axis::Vertical => self.area.width,
        }
    }
}

impl Default for WindowInfo {
    fn default() -> Self {
        WindowInfo { area: Rect::default() }
    }
}

pub(self) enum Value<W: Window, X: AxisT, Y: AxisT> {
    Window(W, WindowInfo),
    Tree(Box<AxisTreeNode<W, Y, X>>, TreeInfo),
}

impl<W: Window, X: AxisT, Y: AxisT> Value<W, X, Y> {
    fn area(&self) -> Rect {
        match self {
            Value::Window(_, info) => info.area,
            Value::Tree(_, info) => info.area,
        }
    }
}

impl<W: Window, X: AxisT, Y: AxisT> From<W> for Value<W, X, Y> {
    fn from(w: W) -> Self {
        Value::Window(w, WindowInfo::default())
    }
}

impl<W: Window, X: AxisT, Y: AxisT> From<AxisTreeNode<W, Y, X>> for Value<W, X, Y> {
    fn from(tree: AxisTreeNode<W, Y, X>) -> Self {
        Value::Tree(Box::new(tree), TreeInfo::default())
    }
}

impl<W: Window, X: AxisT, Y: AxisT> From<AxisTreeNode<W, X, Y>> for AxisTree<W, X, Y> {
    fn from(node: AxisTreeNode<W, X, Y>) -> AxisTree<W, X, Y> {
        Some(Box::new(node))
    }
}

impl<W: Window, X: AxisT, Y: AxisT> From<Value<W, X, Y>> for AxisTree<W, X, Y> {
    fn from(value: Value<W, X, Y>) -> AxisTree<W, X, Y> {
        Some(Box::new(AxisTreeNode::from(value)))
    }
}

impl<W: Window, X: AxisT, Y: AxisT> From<Value<W, X, Y>> for AxisTreeNode<W, X, Y> {
    fn from(value: Value<W, X, Y>) -> AxisTreeNode<W, X, Y> {
        AxisTreeNode::new(value, None, None)
    }
}

impl<W: Window, X: AxisT, Y: AxisT> From<W> for AxisTreeNode<W, X, Y> {
    fn from(window: W) -> AxisTreeNode<W, X, Y> {
        AxisTreeNode::from(Value::from(window))
    }
}

pub(self) trait AxisT: std::fmt::Debug {
    fn axis() -> Axis;
}

#[derive(Debug)]
pub(self) enum HorizontalT {}

impl AxisT for HorizontalT {
    fn axis() -> Axis {
        Axis::Horizontal
    }
}

#[derive(Debug)]
pub(self) enum VerticalT {}

impl AxisT for VerticalT {
    fn axis() -> Axis {
        Axis::Vertical
    }
}

pub(self) fn winnr_cmp(at: usize, lsize: usize, vsize: usize) -> (Ordering, usize) {
    if at < lsize {
        (Ordering::Less, at)
    } else if at - lsize < vsize {
        (Ordering::Equal, at - lsize)
    } else {
        (Ordering::Greater, at - lsize - vsize)
    }
}
