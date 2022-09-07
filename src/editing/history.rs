//! # History management
use std::collections::vec_deque::{Iter, VecDeque};

use regex::Regex;

use super::{
    base::{CursorSearch, MoveDir1D},
    cursor::Cursor,
};

/// Iterator over historical values.
pub struct HistoryIterator<'a, T> {
    piter: Iter<'a, T>,
    fiter: Iter<'a, T>,
    curro: Option<&'a T>,
}

impl<'a, T> Iterator for HistoryIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let p = self.piter.next();

        if p.is_some() {
            return p;
        }

        if self.curro.is_some() {
            return self.curro.take();
        }

        return self.fiter.next();
    }
}

/// A navigable collection of historical values.
#[derive(Clone)]
pub struct HistoryList<T> {
    maxlen: usize,
    past: VecDeque<T>,
    current: T,
    future: VecDeque<T>,
}

impl<T> HistoryList<T> {
    /// Create a new history list.
    ///
    /// `maxlen` controls how many items the history list holds besides the current value.
    pub fn new(init: T, maxlen: usize) -> Self {
        let past = VecDeque::with_capacity(maxlen);
        let future = VecDeque::with_capacity(maxlen);

        HistoryList { maxlen, past, current: init, future }
    }

    /// Move backwards through the history to the oldest item.
    pub fn start(&mut self) -> &T {
        while let Some(item) = self.past.pop_back() {
            let old = std::mem::replace(&mut self.current, item);
            self.future.push_front(old)
        }

        &self.current
    }

    /// Move forwards through the history to the most recent item.
    pub fn end(&mut self) -> &T {
        while let Some(item) = self.future.pop_front() {
            let old = std::mem::replace(&mut self.current, item);
            self.past.push_back(old);
        }

        &self.current
    }

    /// Push a new history item into the list at the current position, and remove any future
    /// values.
    pub fn push(&mut self, item: T) {
        let old = std::mem::replace(&mut self.current, item);
        self.past.push_back(old);
        self.future.clear();

        while self.past.len() > self.maxlen {
            let _ = self.past.pop_front();
        }
    }

    /// Fast-forward the history list so that all values are now in the past, and set a new
    /// current value.
    pub fn append(&mut self, item: T) {
        let old = std::mem::replace(&mut self.current, item);
        self.past.push_back(old);
        self.past.append(&mut self.future);

        while self.past.len() > self.maxlen {
            let _ = self.past.pop_front();
        }
    }

    /// If `item` is equivalent to the current value, then the current value is moved to the end of
    /// the history list, and all previously future values are now in the past.
    ///
    /// Otherwise, if `item` is different from the current value, this behaves exactly like
    /// [HistoryList::append].
    ///
    /// This is useful for allowing users to navigate through historical values, select one,
    /// and make it the most recent value.
    pub fn select(&mut self, item: T)
    where
        T: PartialEq,
    {
        if self.current == item {
            self.past.append(&mut self.future);
        } else {
            self.append(item);
        }
    }

    /// Get a reference to the value at the current position.
    pub fn current(&self) -> &T {
        &self.current
    }

    /// Returns the number of historical items preceding the current value.
    pub fn past_len(&self) -> usize {
        self.past.len()
    }

    /// Returns the number of historical items following the current value.
    pub fn future_len(&self) -> usize {
        self.future.len()
    }

    /// Move backwards through the history list.
    pub fn prev(&mut self, count: usize) -> &T {
        for _ in 0..count {
            match self.past.pop_back() {
                Some(item) => {
                    let old = std::mem::replace(&mut self.current, item);
                    self.future.push_front(old)
                },
                None => break,
            }
        }

        &self.current
    }

    /// Move forwards through the history list.
    pub fn next(&mut self, count: usize) -> &T {
        for _ in 0..count {
            match self.future.pop_front() {
                Some(item) => {
                    let old = std::mem::replace(&mut self.current, item);
                    self.past.push_back(old);
                },
                None => break,
            }
        }

        &self.current
    }

    /// Move forwards or backwards through the history list.
    pub fn navigate(&mut self, dir: MoveDir1D, count: usize) -> &T {
        match dir {
            MoveDir1D::Next => self.next(count),
            MoveDir1D::Previous => self.prev(count),
        }
    }

    /// Iterate over the values within the history list.
    pub fn iter<'a>(&'a self) -> HistoryIterator<'a, T> {
        HistoryIterator {
            piter: self.past.iter(),
            fiter: self.future.iter(),
            curro: Some(&self.current),
        }
    }
}

impl<T> Default for HistoryList<T>
where
    T: Default,
{
    fn default() -> HistoryList<T> {
        HistoryList::new(T::default(), 100)
    }
}

impl<T> HistoryList<T>
where
    T: CursorSearch<Cursor>,
{
    /// Find a matching element in this history list.
    pub fn find(&mut self, needle: &Regex, dir: MoveDir1D, incremental: bool) -> Option<&T> {
        let zero = Cursor::new(0, 0);
        let matches = |t: &T| t.find_regex(&zero, MoveDir1D::Next, needle, 1).is_some();
        let matches_idx = |(_, t): &(usize, &T)| matches(t);

        if incremental && matches(&self.current) {
            return Some(&self.current);
        }

        match dir {
            MoveDir1D::Previous => {
                let res = self.past.iter().enumerate().rfind(matches_idx);

                if let Some((i, _)) = res {
                    let mut shifted = self.past.split_off(i);

                    if let Some(mut current) = shifted.remove(0) {
                        std::mem::swap(&mut self.current, &mut current);
                        self.future.push_front(current);
                    }

                    std::mem::swap(&mut self.future, &mut shifted);
                    self.future.append(&mut shifted);

                    return Some(&self.current);
                } else {
                    return None;
                }
            },
            MoveDir1D::Next => {
                let res = self.future.iter().enumerate().find(matches_idx);

                if let Some((i, _)) = res {
                    let mut shifted = self.future.split_off(i);

                    if let Some(mut current) = shifted.remove(0) {
                        std::mem::swap(&mut self.current, &mut current);
                        self.past.push_back(current);
                    }

                    std::mem::swap(&mut self.future, &mut shifted);
                    self.past.append(&mut shifted);

                    return Some(&self.current);
                } else {
                    return None;
                }
            },
        }
    }
}

#[cfg(test)]
impl<T: ToString> HistoryList<T> {
    pub(crate) fn strs(&self) -> Vec<String> {
        self.iter().map(ToString::to_string).collect()
    }
}

#[cfg(test)]
impl HistoryList<char> {
    pub(crate) fn chars(&self) -> Vec<char> {
        self.iter().map(Clone::clone).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::rope::EditRope;

    #[test]
    fn test_history_nav() {
        let mut hlist = HistoryList::new('a', 100);
        let v = vec!['a', 'b', 'c', 'd', 'e', 'f', 'g'];

        hlist.append('b');
        hlist.append('c');
        hlist.append('d');
        hlist.append('e');
        hlist.append('f');
        hlist.append('g');

        assert_eq!(hlist.current, 'g');
        assert_eq!(hlist.past_len(), 6);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), v);

        hlist.prev(3);
        assert_eq!(hlist.current, 'd');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.future_len(), 3);
        assert_eq!(hlist.chars(), v);

        hlist.next(2);
        assert_eq!(hlist.current, 'f');
        assert_eq!(hlist.past_len(), 5);
        assert_eq!(hlist.future_len(), 1);
        assert_eq!(hlist.chars(), v);

        hlist.prev(15);
        assert_eq!(hlist.current, 'a');
        assert_eq!(hlist.past_len(), 0);
        assert_eq!(hlist.future_len(), 6);
        assert_eq!(hlist.chars(), v);

        hlist.next(10);
        assert_eq!(hlist.current, 'g');
        assert_eq!(hlist.past_len(), 6);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), v);
    }

    #[test]
    fn test_history_limit_append() {
        let mut hlist = HistoryList::new('a', 3);

        assert_eq!(hlist.current, 'a');
        assert_eq!(hlist.past_len(), 0);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a']);

        hlist.append('b');
        assert_eq!(hlist.current, 'b');
        assert_eq!(hlist.past_len(), 1);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a', 'b']);

        hlist.append('c');
        assert_eq!(hlist.current, 'c');
        assert_eq!(hlist.past_len(), 2);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c']);

        hlist.append('d');
        assert_eq!(hlist.current, 'd');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'd']);

        hlist.append('e');
        assert_eq!(hlist.current, 'e');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['b', 'c', 'd', 'e']);

        hlist.append('f');
        assert_eq!(hlist.current, 'f');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['c', 'd', 'e', 'f']);
    }

    #[test]
    fn test_history_limit_push() {
        let mut hlist = HistoryList::new('a', 3);

        assert_eq!(hlist.current, 'a');
        assert_eq!(hlist.past_len(), 0);
        assert_eq!(hlist.chars(), vec!['a']);

        hlist.push('b');
        assert_eq!(hlist.current, 'b');
        assert_eq!(hlist.past_len(), 1);
        assert_eq!(hlist.chars(), vec!['a', 'b']);

        hlist.push('c');
        assert_eq!(hlist.current, 'c');
        assert_eq!(hlist.past_len(), 2);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c']);

        hlist.push('d');
        assert_eq!(hlist.current, 'd');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'd']);

        hlist.push('e');
        assert_eq!(hlist.current, 'e');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.chars(), vec!['b', 'c', 'd', 'e']);

        hlist.push('f');
        assert_eq!(hlist.current, 'f');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.chars(), vec!['c', 'd', 'e', 'f']);
    }

    #[test]
    fn test_history_push() {
        let mut hlist = HistoryList::new('a', 100);

        hlist.push('b');
        hlist.push('c');
        hlist.push('d');
        hlist.push('e');

        assert_eq!(hlist.current, 'e');
        assert_eq!(hlist.past_len(), 4);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'd', 'e']);

        hlist.prev(2);
        assert_eq!(hlist.current, 'c');
        assert_eq!(hlist.past_len(), 2);
        assert_eq!(hlist.future_len(), 2);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'd', 'e']);

        hlist.push('f');
        assert_eq!(hlist.current, 'f');
        assert_eq!(hlist.past_len(), 3);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'f']);
    }

    #[test]
    fn test_history_append() {
        let mut hlist = HistoryList::new('a', 100);

        hlist.append('b');
        hlist.append('c');
        hlist.append('d');
        hlist.append('e');

        assert_eq!(hlist.current, 'e');
        assert_eq!(hlist.past_len(), 4);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'd', 'e']);

        hlist.prev(2);
        assert_eq!(hlist.current, 'c');
        assert_eq!(hlist.past_len(), 2);
        assert_eq!(hlist.future_len(), 2);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'd', 'e']);

        hlist.append('f');
        assert_eq!(hlist.current, 'f');
        assert_eq!(hlist.past_len(), 5);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), vec!['a', 'b', 'c', 'd', 'e', 'f']);
    }

    #[test]
    fn test_history_select() {
        let mut hlist = HistoryList::new('a', 100);

        hlist.select('b');
        hlist.select('c');
        hlist.select('d');
        hlist.select('e');

        let v = vec!['a', 'b', 'c', 'd', 'e'];

        assert_eq!(hlist.current, 'e');
        assert_eq!(hlist.past_len(), 4);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), v);

        hlist.prev(2);

        assert_eq!(hlist.current, 'c');
        assert_eq!(hlist.past_len(), 2);
        assert_eq!(hlist.future_len(), 2);
        assert_eq!(hlist.chars(), v);

        hlist.select('c');

        let v = vec!['a', 'b', 'd', 'e', 'c'];

        assert_eq!(hlist.current, 'c');
        assert_eq!(hlist.past_len(), 4);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), v);

        hlist.prev(2);

        assert_eq!(hlist.current, 'd');
        assert_eq!(hlist.past_len(), 2);
        assert_eq!(hlist.future_len(), 2);
        assert_eq!(hlist.chars(), v);

        hlist.select('a');

        let v = vec!['a', 'b', 'd', 'e', 'c', 'a'];

        assert_eq!(hlist.current, 'a');
        assert_eq!(hlist.past_len(), 5);
        assert_eq!(hlist.future_len(), 0);
        assert_eq!(hlist.chars(), v);
    }

    #[test]
    fn test_history_find() {
        let mut hlist = HistoryList::new(EditRope::from("goodbye"), 100);

        hlist.append(EditRope::from("hello"));
        hlist.append(EditRope::from("world"));
        hlist.append(EditRope::from("help"));
        hlist.append(EditRope::from("whisk"));
        hlist.append(EditRope::from("helm"));
        hlist.append(EditRope::from("aluminum"));
        hlist.append(EditRope::from("writhe"));
        hlist.append(EditRope::from("character"));
        hlist.append(EditRope::from("helium"));
        hlist.append(EditRope::from("product"));

        assert_eq!(hlist.current.to_string(), "product");

        // Search for /he/ going backwards.
        let needle = Regex::new("he").unwrap();

        hlist.find(&needle, MoveDir1D::Previous, false);
        assert_eq!(hlist.current.to_string(), "helium");
        assert_eq!(hlist.past_len(), 9);
        assert_eq!(hlist.future_len(), 1);

        hlist.find(&needle, MoveDir1D::Previous, false);
        assert_eq!(hlist.current.to_string(), "writhe");

        hlist.find(&needle, MoveDir1D::Previous, false);
        assert_eq!(hlist.current.to_string(), "helm");

        hlist.find(&needle, MoveDir1D::Previous, false);
        assert_eq!(hlist.current.to_string(), "help");

        hlist.find(&needle, MoveDir1D::Previous, false);
        assert_eq!(hlist.current.to_string(), "hello");

        // Doesn't move any further back.
        hlist.find(&needle, MoveDir1D::Previous, false);
        assert_eq!(hlist.current.to_string(), "hello");

        // Search for /he.*m/ going forwards.
        let needle = Regex::new("he.*m").unwrap();

        hlist.find(&needle, MoveDir1D::Next, false);
        assert_eq!(hlist.current.to_string(), "helm");

        hlist.find(&needle, MoveDir1D::Next, false);
        assert_eq!(hlist.current.to_string(), "helium");

        // Doesn't move any further forwards.
        hlist.find(&needle, MoveDir1D::Next, false);
        assert_eq!(hlist.current.to_string(), "helium");

        // Incremental search doesn't move if .current() continues to match.
        hlist.find(&needle, MoveDir1D::Previous, true);
        assert_eq!(hlist.current.to_string(), "helium");

        hlist.find(&needle, MoveDir1D::Previous, true);
        assert_eq!(hlist.current.to_string(), "helium");
    }
}
