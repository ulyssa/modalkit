use std::collections::VecDeque;

#[derive(Clone)]
pub struct HistoryList<T> {
    maxlen: usize,
    past: VecDeque<T>,
    pub current: T,
    future: VecDeque<T>,
}

impl<T> HistoryList<T> {
    pub fn new(init: T, maxlen: usize) -> HistoryList<T> {
        let past = VecDeque::with_capacity(maxlen);
        let future = VecDeque::with_capacity(maxlen);

        HistoryList { maxlen, past, current: init, future }
    }

    pub fn append(&mut self, item: T) {
        let old = std::mem::replace(&mut self.current, item);
        self.past.push_back(old);
        self.future.clear();

        while self.past.len() > self.maxlen {
            let _ = self.past.pop_front();
        }
    }

    pub fn current(&self) -> &T {
        &self.current
    }

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
}

impl<T> Default for HistoryList<T>
where
    T: Default,
{
    fn default() -> HistoryList<T> {
        HistoryList::new(T::default(), 100)
    }
}
