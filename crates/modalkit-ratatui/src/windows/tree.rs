use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ops::DerefMut;

use super::layout::LayoutOps;
use super::size::{SizeDescription, MIN_WIN_LEN};
use super::{AxisT, AxisTree, AxisTreeNode, Info, TreeInfo, Value};

use modalkit::prelude::{
    Axis,
    Axis::{Horizontal, Vertical},
    MoveDir2D,
    MoveDir2D::{Down, Left, Right, Up},
};

const DELTA: usize = 3;
const GAMMA: usize = 2;

fn is_balanced<W, X, Y>(a: &AxisTree<W, X, Y>, b: &AxisTree<W, X, Y>) -> bool
where
    X: AxisT,
    Y: AxisT,
{
    DELTA * (a.weight() + 1) >= (b.weight() + 1)
}

fn is_single<W, X, Y>(a: &AxisTree<W, X, Y>, b: &AxisTree<W, X, Y>) -> bool
where
    X: AxisT,
    Y: AxisT,
{
    (a.weight() + 1) < GAMMA * (b.weight() + 1)
}

fn glue<W, X, Y>(mut left: AxisTree<W, X, Y>, mut right: AxisTree<W, X, Y>) -> AxisTree<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    match (left.is_some(), right.is_some()) {
        (false, false) => {
            return None;
        },
        (true, false) => {
            return left;
        },
        (false, true) => {
            return right;
        },
        (true, true) => {
            if left.weight() > right.weight() {
                let pred = left.remove_max();
                let mut node = AxisTreeNode::new(pred, left, right);
                node.balance_left();
                return AxisTree::from(node);
            } else {
                let succ = right.remove_min();
                let mut node = AxisTreeNode::new(succ, left, right);
                node.balance_right();
                return AxisTree::from(node);
            }
        },
    }
}

/// The window layout is structured as a weight-balanced binary tree, where each node represents
/// either a window, or a child layout along the rotated axis.
///
/// We perform the tree operations using the method and parameters described in "Balancing
/// weight-balanced trees" by Yoichi Hirai and Kazuihiko Yamamoto
/// ([https://yoichihirai.com/bst.pdf]).
pub(super) trait SubtreeOps<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    /// Create an instance of the tree containing one [Window].
    fn singleton(window: W) -> Self;

    /// Return how many [Values](Value) are in this tree.
    fn weight(&self) -> usize;

    /// Fetch a mutable reference to the n<sup>th</sup> `Value` in the tree.
    #[allow(unused)]
    fn val_mut(&mut self, at: usize) -> Option<&mut Value<W, X, Y>>;

    /// Iterate over immutable references to each [Value] in this tree.
    fn iter(&self) -> AxisTreeIter<'_, W, X, Y>;

    /// Iterate over mutable references to each [Value] in this tree.
    fn iter_mut(&mut self) -> AxisTreeIterMut<'_, W, X, Y>;

    /// Call a function for each [Value] in this tree.
    ///
    /// This does an in-order traversal of the tree.
    fn for_each_value<F: FnMut(&mut Value<W, X, Y>)>(&mut self, f: &mut F);

    /// Rebalance the tree by performing a left rotation.
    ///
    /// This should be done after inserting in the right child, or removing from the left child.
    fn balance_left(&mut self);

    /// Rebalance the tree by performing a right rotation.
    ///
    /// This should be done after inserting in the left child, or removing from the right child.
    fn balance_right(&mut self);

    /// Insert a [Value] at the leftmost position in the tree.
    fn insert_min_value(&mut self, open: Value<W, X, Y>) -> usize;

    /// Insert a [Value] at the rightmost position in the tree.
    fn insert_max_value(&mut self, open: Value<W, X, Y>) -> usize;

    /// Insert a [Window] at the leftmost position in the tree.
    fn insert_min(&mut self, open: W) -> usize {
        self.insert_min_value(Value::from(open))
    }

    /// Insert a [Window] at the rightmost position in the tree.
    fn insert_max(&mut self, open: W) -> usize {
        self.insert_max_value(Value::from(open))
    }

    /// Remove and return the root node.
    fn remove_root(&mut self) -> Self;
}

pub(super) trait TreeOps<W, X, Y>: SubtreeOps<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    /// Get the length of each [Value] along the axis X, and the range of indices covered by the
    /// contents of the [Value].
    fn get_lengths(&self) -> Vec<SizeDescription>;

    /// Remove and return the leftmost value.
    fn remove_min(&mut self) -> Value<W, X, Y>;

    /// Remove and return the rightmost value.
    fn remove_max(&mut self) -> Value<W, X, Y>;

    /// Insert a [Window] at one of the sides of the window layout.
    fn insert_side(&mut self, open: W, side: MoveDir2D) -> usize;
}

struct AxisTreeIterTrail<'a, W, X: AxisT, Y: AxisT> {
    value: &'a mut Value<W, X, Y>,
    right: &'a mut AxisTree<W, X, Y>,
    parent: AxisTreeIterTrailParent<'a, W, X, Y>,
}

impl<'a, W, X, Y> AxisTreeIterTrail<'a, W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn new(
        tree: &'a mut AxisTreeNode<W, X, Y>,
        parent: AxisTreeIterTrailParent<'a, W, X, Y>,
    ) -> Self {
        let mut trail = AxisTreeIterTrail {
            value: &mut tree.value,
            right: &mut tree.right,
            parent,
        };
        let mut left = &mut tree.left;

        while let Some(node) = left {
            let parent = Some(Box::new(trail));
            trail = AxisTreeIterTrail {
                value: &mut node.value,
                right: &mut node.right,
                parent,
            };
            left = &mut node.left;
        }

        trail
    }

    fn next(self) -> (&'a mut Value<W, X, Y>, AxisTreeIterTrailParent<'a, W, X, Y>) {
        if let Some(node) = self.right {
            let trail = AxisTreeIterTrail::new(node, self.parent);
            let trail = Some(Box::new(trail));

            (self.value, trail)
        } else {
            (self.value, self.parent)
        }
    }
}

type AxisTreeIterTrailParent<'a, W, X, Y> = Option<Box<AxisTreeIterTrail<'a, W, X, Y>>>;

pub(super) struct AxisTreeIterMut<'a, W, X: AxisT, Y: AxisT> {
    current: AxisTreeIterTrailParent<'a, W, X, Y>,
}

impl<'a, W, X, Y> From<&'a mut AxisTreeNode<W, X, Y>> for AxisTreeIterMut<'a, W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn from(node: &'a mut AxisTreeNode<W, X, Y>) -> Self {
        let current = AxisTreeIterTrail::new(node, None);
        let current = Some(Box::new(current));

        AxisTreeIterMut { current }
    }
}

impl<'a, W, X, Y> From<&'a mut AxisTree<W, X, Y>> for AxisTreeIterMut<'a, W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn from(node: &'a mut AxisTree<W, X, Y>) -> Self {
        match node {
            Some(root) => AxisTreeIterMut::from(root.deref_mut()),
            None => AxisTreeIterMut { current: None },
        }
    }
}

impl<'a, W, X, Y> Iterator for AxisTreeIterMut<'a, W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    type Item = &'a mut Value<W, X, Y>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current.take()?;
        let (value, current) = current.next();
        self.current = current;

        return Some(value);
    }
}

pub(super) struct AxisTreeIter<'a, W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    previous: Vec<(&'a Value<W, X, Y>, &'a AxisTree<W, X, Y>)>,
    current: &'a AxisTree<W, X, Y>,
}

impl<'a, W, X, Y> Iterator for AxisTreeIter<'a, W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    type Item = &'a Value<W, X, Y>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.current {
                None => {
                    match self.previous.pop() {
                        None => {
                            return None;
                        },
                        Some((v, r)) => {
                            self.current = r;

                            return Some(v);
                        },
                    }
                },
                Some(node) => {
                    self.previous.push((&node.value, &node.right));
                    self.current = &node.left;
                },
            }
        }
    }
}

impl<W, X, Y> AxisTreeNode<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    pub fn new(
        value: Value<W, X, Y>,
        left: AxisTree<W, X, Y>,
        right: AxisTree<W, X, Y>,
    ) -> AxisTreeNode<W, X, Y> {
        let info = Info::from(&value, &left, &right);

        AxisTreeNode { value, info, left, right, _p: PhantomData }
    }

    pub fn get_lengths(&self) -> Vec<SizeDescription> {
        let mut off = 0;
        let f = |v: &Value<W, X, Y>| {
            let (w, h) = v.dimensions();
            let (w, h) = (w as u16, h as u16);
            let (w, h) = (w * MIN_WIN_LEN, h * MIN_WIN_LEN);

            match (X::axis(), v) {
                (Axis::Horizontal, Value::Window(_, info)) => {
                    let start = off;
                    off = start + 1;

                    SizeDescription::new(start..off, info.area.height, h)
                },
                (Axis::Horizontal, Value::Tree(t, info)) => {
                    let start = off;
                    off = start + t.size();

                    SizeDescription::new(start..off, info.area.height, h)
                },
                (Axis::Vertical, Value::Window(_, info)) => {
                    let start = off;
                    off = start + 1;

                    SizeDescription::new(start..off, info.area.width, w)
                },
                (Axis::Vertical, Value::Tree(t, info)) => {
                    let start = off;
                    off = start + t.size();

                    SizeDescription::new(start..off, info.area.width, w)
                },
            }
        };

        self.iter().map(f).collect()
    }

    pub fn _update_info(&mut self) {
        self.info = Info::from(&self.value, &self.left, &self.right);
    }

    /*
     *     v1                 v2
     *    /  \               /  \
     *  t1    v2     =>    v1    t3
     *       /  \         /  \
     *     t2    t3     t1    t2
     */
    fn _rotate_left_single(&mut self) {
        let mut r = std::mem::take(&mut self.right).unwrap();

        // swap v1 and v2; v2 is now in its final place.
        std::mem::swap(&mut self.value, &mut r.value);

        let v1 = r.value;

        let t1 = std::mem::take(&mut self.left);
        let t2 = r.left;
        let t3 = r.right;

        self.left = Some(Box::new(AxisTreeNode::new(v1, t1, t2)));
        self.right = t3;
        self._update_info();
    }

    /*
     *      v1                    v3
     *     /  \                  /  \
     *    /    \                /    \
     *  t1      v2     =>     v1      v2
     *         /  \          /  \    /  \
     *       v3    t4      t1   t2  t3   t4
     *      /  \
     *    t2    t3
     */
    fn _rotate_left_double(&mut self) {
        let mut r1 = std::mem::take(&mut self.right).unwrap();
        let mut r2 = std::mem::take(&mut r1.left).unwrap();

        // swap v1 and v3; v3 is now in its final place.
        std::mem::swap(&mut self.value, &mut r2.value);

        let v1 = r2.value;
        let v2 = r1.value;

        let t1 = std::mem::take(&mut self.left);
        let t2 = r2.left;
        let t3 = r2.right;
        let t4 = r1.right;

        self.left = Some(Box::new(AxisTreeNode::new(v1, t1, t2)));
        self.right = Some(Box::new(AxisTreeNode::new(v2, t3, t4)));
        self._update_info();
    }

    fn _rotate_left(&mut self) {
        match &self.right {
            None => {
                panic!("AxisTreeNode::_rotate_left() should only be called after right node insertions!");
            },
            Some(r) => {
                if is_single(&r.left, &r.right) {
                    self._rotate_left_single();
                } else {
                    self._rotate_left_double();
                }
            },
        }
    }

    /*
     *       v1            v2
     *      /  \          /  \
     *    v2    t3  =>  t1    v1
     *   /  \                /  \
     * t1    t2            t2    t3
     */
    fn _rotate_right_single(&mut self) {
        let mut l = std::mem::take(&mut self.left).unwrap();

        // swap v1 and v2; v2 is now in its final place.
        std::mem::swap(&mut self.value, &mut l.value);

        let v1 = l.value;

        let t1 = l.left;
        let t2 = l.right;
        let t3 = std::mem::take(&mut self.right);

        self.left = t1;
        self.right = Some(Box::new(AxisTreeNode::new(v1, t2, t3)));
        self._update_info();
    }

    /*
     *        v1                    v3
     *       /  \                  /  \
     *      /    \                /    \
     *    v2      t4     =>     v2      v1
     *   /  \                  /  \    /  \
     * t1    v3              t1   t2  t3   t4
     *      /  \
     *    t2    t3
     */
    fn _rotate_right_double(&mut self) {
        let mut l1 = std::mem::take(&mut self.left).unwrap();
        let mut l2 = std::mem::take(&mut l1.right).unwrap();

        // swap v1 and v3; v3 is now in its final place.
        std::mem::swap(&mut self.value, &mut l2.value);

        let v1 = l2.value;
        let v2 = l1.value;

        let t1 = l1.left;
        let t2 = l2.left;
        let t3 = l2.right;
        let t4 = std::mem::take(&mut self.right);

        self.left = Some(Box::new(AxisTreeNode::new(v2, t1, t2)));
        self.right = Some(Box::new(AxisTreeNode::new(v1, t3, t4)));
        self._update_info();
    }

    fn _rotate_right(&mut self) {
        match &self.left {
            None => {
                panic!("AxisTreeNode::_rotate_right() should only be called after left node insertions!");
            },
            Some(l) => {
                if is_single(&l.right, &l.left) {
                    self._rotate_right_single();
                } else {
                    self._rotate_right_double();
                }
            },
        }
    }
}

impl<W, X, Y> SubtreeOps<W, X, Y> for AxisTreeNode<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn singleton(window: W) -> AxisTreeNode<W, X, Y> {
        AxisTreeNode::from(window)
    }

    fn weight(&self) -> usize {
        self.info.weight
    }

    fn val_mut(&mut self, at: usize) -> Option<&mut Value<W, X, Y>> {
        let lweight = self.left.weight();

        match at.cmp(&lweight) {
            Ordering::Less => self.left.val_mut(at),
            Ordering::Equal => Some(&mut self.value),
            Ordering::Greater => self.right.val_mut(at - lweight - 1),
        }
    }

    fn balance_left(&mut self) {
        self._update_info();

        if is_balanced(&self.left, &self.right) {
            return;
        }

        self._rotate_left();
    }

    fn balance_right(&mut self) {
        self._update_info();

        if is_balanced(&self.right, &self.left) {
            return;
        }

        self._rotate_right();
    }

    fn insert_min_value(&mut self, open: Value<W, X, Y>) -> usize {
        let idx = self.left.insert_min_value(open);
        self.balance_right();

        idx
    }

    fn insert_max_value(&mut self, open: Value<W, X, Y>) -> usize {
        let idx = self.right.insert_max_value(open);
        let idx = idx + self.left.size() + self.value.size();
        self.balance_left();

        idx
    }

    fn iter(&self) -> AxisTreeIter<'_, W, X, Y> {
        AxisTreeIter {
            previous: vec![(&self.value, &self.right)],
            current: &self.left,
        }
    }

    fn iter_mut(&mut self) -> AxisTreeIterMut<'_, W, X, Y> {
        AxisTreeIterMut::from(self)
    }

    fn for_each_value<F: FnMut(&mut Value<W, X, Y>)>(&mut self, f: &mut F) {
        self.left.for_each_value(f);
        f(&mut self.value);
        self.right.for_each_value(f);
    }

    fn remove_root(&mut self) -> Self {
        let left = std::mem::take(&mut self.left);
        let right = std::mem::take(&mut self.right);
        let mut node = glue(left, right)
            .expect("cannot remove root of an AxisTreeNode containing a single value");

        std::mem::swap(self, &mut node);

        return *node;
    }
}

impl<W, X, Y> SubtreeOps<W, X, Y> for AxisTree<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn singleton(window: W) -> AxisTree<W, X, Y> {
        AxisTree::from(Value::from(window))
    }

    fn weight(&self) -> usize {
        match self {
            None => 0,
            Some(node) => node.weight(),
        }
    }

    fn val_mut(&mut self, at: usize) -> Option<&mut Value<W, X, Y>> {
        match self {
            None => None,
            Some(node) => node.val_mut(at),
        }
    }

    fn iter(&self) -> AxisTreeIter<'_, W, X, Y> {
        AxisTreeIter { previous: Vec::new(), current: self }
    }

    fn iter_mut(&mut self) -> AxisTreeIterMut<'_, W, X, Y> {
        AxisTreeIterMut::from(self)
    }

    fn for_each_value<F: FnMut(&mut Value<W, X, Y>)>(&mut self, f: &mut F) {
        match self {
            None => (),
            Some(node) => node.for_each_value(f),
        }
    }

    fn balance_left(&mut self) {
        if let Some(node) = self {
            node.balance_left();
        }
    }

    fn balance_right(&mut self) {
        if let Some(node) = self {
            node.balance_right();
        }
    }

    fn insert_min_value(&mut self, open: Value<W, X, Y>) -> usize {
        match self {
            None => {
                *self = AxisTree::from(open);

                return 0;
            },
            Some(node) => {
                return node.insert_min_value(open);
            },
        }
    }

    fn insert_max_value(&mut self, open: Value<W, X, Y>) -> usize {
        match self {
            None => {
                *self = AxisTree::from(open);

                return 0;
            },
            Some(node) => {
                return node.insert_max_value(open);
            },
        }
    }

    fn remove_root(&mut self) -> Self {
        match self {
            None => {
                return None;
            },
            Some(node) => {
                let left = std::mem::take(&mut node.left);
                let right = std::mem::take(&mut node.right);
                let mut node = glue(left, right);

                std::mem::swap(self, &mut node);

                return node;
            },
        }
    }
}

impl<W, X, Y> TreeOps<W, X, Y> for AxisTree<W, X, Y>
where
    X: AxisT,
    Y: AxisT,
{
    fn get_lengths(&self) -> Vec<SizeDescription> {
        if let Some(tree) = self {
            tree.get_lengths()
        } else {
            vec![]
        }
    }

    fn remove_min(&mut self) -> Value<W, X, Y> {
        match self {
            None => {
                panic!("cannot remove min node from an empty subtree");
            },
            Some(node) => {
                if node.left.is_some() {
                    let value = node.left.remove_min();
                    node.balance_left();
                    return value;
                } else {
                    let node = std::mem::take(self).unwrap();
                    *self = node.right;
                    return node.value;
                }
            },
        }
    }

    fn remove_max(&mut self) -> Value<W, X, Y> {
        match self {
            None => {
                panic!("cannot remove max node from an empty subtree");
            },
            Some(node) => {
                if node.right.is_some() {
                    let value = node.right.remove_max();
                    node.balance_right();
                    return value;
                } else {
                    let node = std::mem::take(self).unwrap();
                    *self = node.left;
                    return node.value;
                }
            },
        }
    }

    fn insert_side(&mut self, open: W, side: MoveDir2D) -> usize {
        match (X::axis(), side) {
            (Vertical, Left) | (Horizontal, Up) => {
                return self.insert_min(open);
            },
            (Vertical, Right) | (Horizontal, Down) => {
                return self.insert_max(open);
            },
            (Vertical, Up) | (Horizontal, Left) => {
                let window: Value<W, Y, X> = Value::from(open);
                let xtree: AxisTree<W, X, Y> = std::mem::take(self);
                let ytree: AxisTree<W, Y, X> =
                    xtree.and_then(|xtree| AxisTree::from(Value::Tree(xtree, TreeInfo::default())));
                let ynode: AxisTreeNode<W, Y, X> = AxisTreeNode::new(window, None, ytree);
                let xtree: AxisTree<W, X, Y> = AxisTree::from(Value::from(ynode));

                *self = xtree;

                return 0;
            },
            (Vertical, Down) | (Horizontal, Right) => {
                let window: Value<W, Y, X> = Value::from(open);
                let xtree: AxisTree<W, X, Y> = std::mem::take(self);
                let ytree: AxisTree<W, Y, X> =
                    xtree.and_then(|xtree| AxisTree::from(Value::Tree(xtree, TreeInfo::default())));
                let ynode: AxisTreeNode<W, Y, X> = AxisTreeNode::new(window, ytree, None);
                let xtree: AxisTree<W, X, Y> = AxisTree::from(Value::from(ynode));

                *self = xtree;

                return self.size().saturating_sub(1);
            },
        }
    }
}
