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

use crate::editing::{
    action::{EditInfo, EditResult, UIResult},
    application::ApplicationInfo,
    base::{
        Axis,
        CloseFlags,
        Count,
        FocusChange,
        MoveDir1D,
        MoveDir2D,
        OpenTarget,
        SizeChange,
        WindowTarget,
        WriteFlags,
    },
    context::EditContext,
    store::Store,
};

use self::layout::LayoutOps;
use self::size::{ResizeInfo, SizeDescription, MIN_WIN_LEN};
use self::tree::SubtreeOps;

mod layout;
mod size;
mod slot;
mod tree;

pub use self::layout::{WindowLayout, WindowLayoutDescription, WindowLayoutState};

pub(self) struct AxisTreeNode<W, X: AxisT, Y: AxisT> {
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
    fn from<W, X: AxisT, Y: AxisT>(
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

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct TreeInfo {
    area: Rect,
    resized: ResizeInfo,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
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

pub(self) enum Value<W, X: AxisT, Y: AxisT> {
    Window(W, WindowInfo),
    Tree(Box<AxisTreeNode<W, Y, X>>, TreeInfo),
}

impl<W, X, Y> Value<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn area(&self) -> Rect {
        match self {
            Value::Window(_, info) => info.area,
            Value::Tree(_, info) => info.area,
        }
    }
}

impl<W, X, Y> From<W> for Value<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn from(w: W) -> Self {
        Value::Window(w, WindowInfo::default())
    }
}

impl<W, X, Y> From<AxisTreeNode<W, Y, X>> for Value<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn from(tree: AxisTreeNode<W, Y, X>) -> Self {
        Value::Tree(Box::new(tree), TreeInfo::default())
    }
}

impl<W, X, Y> From<AxisTreeNode<W, X, Y>> for AxisTree<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn from(node: AxisTreeNode<W, X, Y>) -> AxisTree<W, X, Y> {
        Some(Box::new(node))
    }
}

impl<W, X, Y> From<Value<W, X, Y>> for AxisTree<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn from(value: Value<W, X, Y>) -> AxisTree<W, X, Y> {
        Some(Box::new(AxisTreeNode::from(value)))
    }
}

impl<W, X, Y> From<Value<W, X, Y>> for AxisTreeNode<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn from(value: Value<W, X, Y>) -> AxisTreeNode<W, X, Y> {
        AxisTreeNode::new(value, None, None)
    }
}

impl<W, X, Y> From<W> for AxisTreeNode<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
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

pub(super) trait WindowActions<C, I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    /// Close one or more [Windows](Window).
    fn window_close(
        &mut self,
        target: &WindowTarget,
        flags: CloseFlags,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Swap the placement of the currently focused [Window] with another.
    fn window_exchange(
        &mut self,
        change: &FocusChange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Switch focus to a different [Window].
    fn window_focus(
        &mut self,
        change: &FocusChange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Move the currently focused [Window] to occupy a whole side of the screen.
    fn window_move_side(
        &mut self,
        dir: MoveDir2D,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Change the size of the currently focused [Window].
    fn window_clear_sizes(&mut self, ctx: &C, store: &mut Store<I>) -> EditResult<EditInfo, I>;

    /// Change the size of the currently focused [Window].
    fn window_resize(
        &mut self,
        target: &FocusChange,
        axis: Axis,
        size: &SizeChange<Count>,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Rotate the placement of the windows in either the row or column of the currently focused
    /// [Window].
    fn window_rotate(
        &mut self,
        dir: MoveDir1D,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Split the currently focused [Window] [*n* times](Count), with each new [Window] showing the
    /// same content.
    fn window_split(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        axis: Axis,
        rel: MoveDir1D,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I>;

    fn window_open(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        axis: Axis,
        rel: MoveDir1D,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I>;

    fn window_switch(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        ctx: &C,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I>;

    /// Write one or more [Windows](Window).
    fn window_write(
        &mut self,
        target: &WindowTarget,
        path: Option<&str>,
        flags: WriteFlags,
        ctx: &C,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I>;

    /// Zoom in on the currently focused window. If we're already zoomed in on a window, return to
    /// showing all windows.
    fn window_zoom_toggle(&mut self, ctx: &C, store: &mut Store<I>) -> EditResult<EditInfo, I>;
}
