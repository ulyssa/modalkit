use std::cmp::Ordering;
use std::ops::Index;

use crate::editing::context::{EditContext, Resolve};
use crate::prelude::*;
use crate::util::{idx_move, idx_offset};

/// List of items that tracks a current and previous focus.
pub struct FocusList<T> {
    /// The list of items.
    items: Vec<T>,

    /// Previously focused position.
    idx_last: usize,

    /// Currently focused position.
    idx_curr: usize,
}

impl<T> FocusList<T> {
    /// Create a new [FocusList] given an original set of items.
    pub fn new(items: Vec<T>) -> Self {
        FocusList { items, idx_last: 0, idx_curr: 0 }
    }

    /// Returns how many items are in this list.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Indicates whether this list is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Get a reference to the currently focused item.
    pub fn get(&self) -> Option<&T> {
        self.items.get(self.idx_curr)
    }

    /// Get a mutable reference to the currently focused item.
    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.items.get_mut(self.idx_curr)
    }

    /// Get the currently focused position in this list.
    pub fn pos(&self) -> usize {
        self.idx_curr
    }

    /// Iterate over mutable references to the items in this list.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.items.iter_mut()
    }

    /// Iterate over references to the items in this list.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items.iter()
    }

    /// Try closing targeted items using `f` until one fails or all targets are closed.
    ///
    /// If the target cannot be closed, focus will be moved to it.
    pub fn try_close<E, F>(
        &mut self,
        target: &TabTarget,
        mut f: F,
        ctx: &EditContext,
    ) -> Result<(), E>
    where
        F: FnMut(&mut T) -> Result<(), E>,
    {
        let mut result = Ok(());

        match target {
            TabTarget::All => {
                let old = std::mem::take(&mut self.items);

                // The first item we fail to close will be at the front.
                self.idx_curr = 0;
                self.idx_last = 0;

                for mut item in old.into_iter() {
                    if result.is_ok() {
                        if let e @ Err(_) = f(&mut item) {
                            result = e;
                        } else {
                            continue;
                        }
                    }

                    self.items.push(item);
                }
            },
            TabTarget::AllBut(fc) => {
                if let Some((idx, _)) = self.target(fc, ctx) {
                    let old = std::mem::take(&mut self.items);

                    // Focus will end on the remaining item or the error.
                    self.idx_curr = 0;
                    self.idx_last = 0;

                    for (i, mut tab) in old.into_iter().enumerate() {
                        if i == idx {
                            self.items.push(tab);
                            continue;
                        }

                        if result.is_ok() {
                            if let e @ Err(_) = f(&mut tab) {
                                result = e;

                                // Focus on the error.
                                self.set_focus(self.len());
                            } else {
                                continue;
                            }
                        }

                        self.items.push(tab);
                    }
                }
            },
            TabTarget::Single(fc) => {
                if let Some((idx, _)) = self.target(fc, ctx) {
                    if let e @ Err(_) = f(&mut self.items[idx]) {
                        result = e;

                        // Focus on the error.
                        self.set_focus(idx);
                    } else {
                        self.remove(idx);
                    }
                }
            },
        }

        return result;
    }

    fn _max_idx(&self) -> usize {
        self.len().saturating_sub(1)
    }

    fn _idx(&self, count: &Count, ctx: &EditContext) -> Option<usize> {
        ctx.resolve(count).checked_sub(1)
    }

    /// Determine the index of the item targeted by a given [FocusChange].
    ///
    /// The [MoveDir1D] value returned indicates which side of the index to insert at.
    pub fn target(&self, change: &FocusChange, ctx: &EditContext) -> Option<(usize, MoveDir1D)> {
        let target = match change {
            FocusChange::Current => self.idx_curr,
            FocusChange::Offset(count, false) => {
                let idx = if let Some(n) = self._idx(count, ctx) {
                    n
                } else {
                    return Some((0, MoveDir1D::Previous));
                };

                if idx >= self.len() {
                    // Invalid tab index; do nothing.
                    return None;
                }

                idx
            },
            FocusChange::Offset(count, true) => {
                if let Some(n) = self._idx(count, ctx) {
                    n.min(self._max_idx())
                } else {
                    return Some((0, MoveDir1D::Previous));
                }
            },
            FocusChange::Direction1D(dir, count, wrap) => {
                let ntabs = self.len();
                let count = ctx.resolve(count);

                return idx_offset(self.idx_curr, count, dir, ntabs, *wrap).map(|i| (i, *dir));
            },
            FocusChange::Direction2D(MoveDir2D::Left, count) => {
                let ntabs = self.len();
                let count = ctx.resolve(count) % ntabs;

                (ntabs + self.idx_curr - count) % ntabs
            },
            FocusChange::Direction2D(MoveDir2D::Right, count) => {
                let ntabs = self.len();
                let count = ctx.resolve(count) % ntabs;

                (self.idx_curr + count) % ntabs
            },
            FocusChange::Direction2D(MoveDir2D::Up | MoveDir2D::Down, _) => {
                return None;
            },
            FocusChange::Position(MovePosition::Beginning) => {
                return Some((0, MoveDir1D::Previous));
            },
            FocusChange::Position(MovePosition::Middle) => self.len() / 2,
            FocusChange::Position(MovePosition::End) => self.len().saturating_sub(1),
            FocusChange::PreviouslyFocused => self.idx_last,
        };

        Some((target, MoveDir1D::Next))
    }

    /// Insert an item before or after a given index.
    pub fn insert(&mut self, idx: usize, side: MoveDir1D, item: T) {
        let idx = match side {
            MoveDir1D::Previous => idx,
            MoveDir1D::Next => idx + 1,
        };

        self.items.insert(idx, item);

        if self.idx_curr >= idx {
            self.idx_last = self.idx_curr + 1;
        } else {
            self.idx_last = self.idx_curr;
        }

        self.idx_curr = idx;
    }

    /// Remove the item located at a given index.
    pub fn remove(&mut self, idx: usize) {
        if idx >= self.len() {
            return;
        }

        let _ = self.items.remove(idx);
        let max = self._max_idx();

        self.idx_last = match idx.cmp(&self.idx_last) {
            Ordering::Less => self.idx_last.saturating_sub(1).min(max),
            Ordering::Equal => 0,
            Ordering::Greater => self.idx_last.min(max),
        };

        self.idx_curr = match idx.cmp(&self.idx_curr) {
            Ordering::Less => self.idx_curr.saturating_sub(1).min(max),
            Ordering::Equal => self.idx_last,
            Ordering::Greater => self.idx_curr.min(max),
        };
    }

    /// Remove the currently focused item.
    pub fn remove_current(&mut self) {
        self.remove(self.idx_curr);
    }

    fn set_focus(&mut self, idx: usize) {
        if idx == self.idx_curr {
            return;
        }

        self.idx_last = self.idx_curr;
        self.idx_curr = idx;
    }

    /// Switch focus to a new item.
    pub fn focus(&mut self, change: &FocusChange, ctx: &EditContext) {
        if let Some((idx, _)) = self.target(change, ctx) {
            self.set_focus(idx);
        }
    }

    /// Move the currently focused item to a new position.
    pub fn transfer(&mut self, change: &FocusChange, ctx: &EditContext) {
        if let Some((idx, side)) = self.target(change, ctx) {
            idx_move(&mut self.items, &mut self.idx_curr, idx, &mut self.idx_last, side);
        }
    }

    /// Push a new item onto the end of this list.
    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }
}

impl<T> Default for FocusList<T> {
    fn default() -> Self {
        FocusList::new(Vec::new())
    }
}

impl<T> Index<usize> for FocusList<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        self.items.index(index)
    }
}

impl<T> AsRef<[T]> for FocusList<T> {
    fn as_ref(&self) -> &[T] {
        self.items.as_ref()
    }
}

impl<T> From<T> for FocusList<T> {
    fn from(item: T) -> Self {
        FocusList::new(vec![item])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::MoveDir1D::{Next, Previous as Prev};

    fn close1(v: &mut char) -> Result<(), char> {
        if *v == 'c' {
            Err(*v)
        } else {
            Ok(())
        }
    }

    fn close2(_: &mut char) -> Result<(), bool> {
        Ok(())
    }

    #[test]
    fn test_insert() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        list.insert(2, Next, 'q');
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'q', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 0);

        list.insert(2, Prev, 'r');
        assert_eq!(list.as_ref(), &['a', 'b', 'r', 'c', 'q', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 4);

        // Insert at the end of the list.
        list.insert(8, Next, 's');
        assert_eq!(list.as_ref(), &['a', 'b', 'r', 'c', 'q', 'd', 'e', 'f', 'g', 's']);
        assert_eq!(list.idx_curr, 9);
        assert_eq!(list.idx_last, 2);

        // Insert at the beginning of the list.
        list.insert(0, Prev, 't');
        assert_eq!(list.as_ref(), &['t', 'a', 'b', 'r', 'c', 'q', 'd', 'e', 'f', 'g', 's']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 10);
    }

    #[test]
    fn test_remove() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);

        list.idx_curr = 1;
        list.idx_last = 4;

        // Remove an index after idx_curr and idx_last.
        list.remove(6);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'd', 'e', 'f']);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 4);

        // Remove an index in between idx_curr and idx_last.
        list.remove(2);
        assert_eq!(list.as_ref(), &['a', 'b', 'd', 'e', 'f']);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 3);

        // Removing idx_curr goes to idx_last.
        list.remove(1);
        assert_eq!(list.as_ref(), &['a', 'd', 'e', 'f']);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 2);

        // Removing idx_curr goes to 0 when it's also equal to idx_last.
        list.remove(2);
        assert_eq!(list.as_ref(), &['a', 'd', 'f']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);
    }

    #[test]
    fn test_focus_dir() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Move two forward.
        list.focus(&FocusChange::Direction1D(Next, 2.into(), true), &ctx);
        assert_eq!(list.idx_curr, 2);

        // Move three forward.
        list.focus(&FocusChange::Direction1D(Next, 3.into(), true), &ctx);
        assert_eq!(list.idx_curr, 5);
        assert_eq!(list.idx_last, 2);

        // Move six forward and wrap around.
        list.focus(&FocusChange::Direction1D(Next, 6.into(), true), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 5);

        // Move six backwards, wrapping around to where we had been.
        list.focus(&FocusChange::Direction1D(Prev, 6.into(), true), &ctx);
        assert_eq!(list.idx_curr, 5);
        assert_eq!(list.idx_last, 4);

        // Move two backwards.
        list.focus(&FocusChange::Direction1D(Prev, 2.into(), true), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 5);

        // Check that we can prevent wrapping around at the end.
        list.focus(&FocusChange::Direction1D(Next, 5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 5);

        // Check that we can prevent wrapping around at the beginning.
        list.focus(&FocusChange::Direction1D(Prev, 5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 5);

        // We should be able to move backwards within the boundaries.
        list.focus(&FocusChange::Direction1D(Prev, 1.into(), false), &ctx);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 3);

        // We should be able to move forwards within the boundaries.
        list.focus(&FocusChange::Direction1D(Next, 2.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 2);
    }

    #[test]
    fn test_focus_offset() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Move to the 2nd item.
        list.focus(&FocusChange::Offset(2.into(), false), &ctx);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 0);

        // Move to the 5th item.
        list.focus(&FocusChange::Offset(5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 1);

        // Trying to move past the end does nothing.
        list.focus(&FocusChange::Offset(9.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 1);

        // Trying to move past the end now succeeds, and goes to the last item.
        list.focus(&FocusChange::Offset(9.into(), true), &ctx);
        assert_eq!(list.idx_curr, 6);
        assert_eq!(list.idx_last, 4);

        // Move to the 1st item.
        list.focus(&FocusChange::Offset(1.into(), true), &ctx);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 6);
    }

    #[test]
    fn test_focus_position() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Move to the end.
        list.focus(&FocusChange::Position(MovePosition::End), &ctx);
        assert_eq!(list.idx_curr, 6);
        assert_eq!(list.idx_last, 0);

        // Move to the middle.
        list.focus(&FocusChange::Position(MovePosition::Middle), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 6);

        // Move to the beginning.
        list.focus(&FocusChange::Position(MovePosition::Beginning), &ctx);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 3);
    }

    #[test]
    fn test_focus_previously_focused() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // First, move to the 5th item.
        list.focus(&FocusChange::Offset(5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 0);

        // Going to the 5th item again doesn't touch idx_last.
        list.focus(&FocusChange::Offset(5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 0);

        // Jump to the previously focused item.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 4);

        // Jump back again.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 0);

        // First, move to the 2nd item.
        list.focus(&FocusChange::Offset(2.into(), false), &ctx);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 4);

        // Jump to the previously focused item.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 1);

        // Jump once more.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 4);
    }

    #[test]
    fn test_move_item() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        list.idx_curr = 1;
        list.idx_last = 3;

        // Move 'b' to the end.
        list.transfer(&FocusChange::Position(MovePosition::End), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'd', 'e', 'f', 'g', 'b']);
        assert_eq!(list.idx_curr, 6);
        assert_eq!(list.idx_last, 2);

        // Move 'b' to the beginning.
        list.transfer(&FocusChange::Position(MovePosition::Beginning), &ctx);
        assert_eq!(list.as_ref(), &['b', 'a', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 3);

        // Move 'b' to after the fourth item, 'd'.
        list.transfer(&FocusChange::Offset(4.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'd', 'b', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 2);

        // Move 'b' to after the first item, 'a'.
        list.transfer(&FocusChange::Offset(1.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 3);

        // Move 'b' to after the zeroth item.
        list.transfer(&FocusChange::Offset(0.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['b', 'a', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 3);

        // Move 'b' forwards three times.
        list.transfer(&FocusChange::Direction1D(Next, 3.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'd', 'b', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 2);

        // Move 'b' backwards once.
        list.transfer(&FocusChange::Direction1D(Prev, 1.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'b', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 3);
    }

    #[test]
    fn test_try_close_all() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        list.idx_curr = 4;
        list.idx_last = 5;

        // Close the first several items before failing.
        let res = list.try_close(&TabTarget::All, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Trying again makes no progress.
        let res = list.try_close(&TabTarget::All, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Using close2() we make it all the way through.
        let res = list.try_close(&TabTarget::All, close2, &ctx);
        assert_eq!(list.is_empty(), true);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);
    }

    #[test]
    fn test_try_close_all_but() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        list.idx_curr = 4;
        list.idx_last = 4;

        // Try to close everything but 'e', but stop at 'c'.
        let target = TabTarget::AllBut(FocusChange::Current);
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Trying again makes no progress.
        let target = TabTarget::AllBut(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Using close2() we make it all the way through.
        let target = TabTarget::AllBut(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close2, &ctx);
        assert_eq!(list.as_ref(), &['e']);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);
    }

    #[test]
    fn test_try_close_single() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = EditContext::default();

        list.idx_curr = 5;
        list.idx_last = 0;

        // Cannot close 'c' using close1(); focus moves to 'c'.
        let target = TabTarget::Single(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 5);

        // But we can close 'd' just fine.
        let target = TabTarget::Single(FocusChange::Offset(4.into(), true));
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'e', 'f', 'g']);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 4);

        // We can close 'c' using close2().
        let target = TabTarget::Single(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close2, &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'e', 'f', 'g']);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 3);
    }
}
