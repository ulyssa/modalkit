use std::collections::HashMap;
use std::hash::Hash;

use crate::editing::base::Mark;
use crate::editing::cursor::{Cursor, CursorAdjustment};

use super::BufferId;

#[derive(Clone, Debug)]
pub struct CursorStore<M: Copy + Eq + Hash> {
    map: HashMap<M, Cursor>,
}

impl<M: Copy + Eq + Hash> Default for CursorStore<M> {
    fn default() -> CursorStore<M> {
        CursorStore::new()
    }
}

impl<M: Copy + Eq + Hash> CursorStore<M> {
    pub fn new() -> Self {
        CursorStore { map: HashMap::new() }
    }

    pub fn contains(&mut self, mark: M) -> bool {
        self.map.contains_key(&mark)
    }

    pub fn del(&mut self, mark: M) {
        let _ = self.map.remove(&mark);
    }

    pub fn get(&self, mark: M) -> Option<Cursor> {
        self.map.get(&mark).map(Cursor::clone)
    }

    pub fn put(&mut self, mark: M, cursor: Cursor) {
        self.map.insert(mark, cursor);
    }

    pub fn zero_all(&mut self) {
        for cursor in self.map.values_mut() {
            cursor.zero();
        }
    }

    pub fn adjust(&mut self, adj: &CursorAdjustment) {
        for cursor in self.map.values_mut() {
            cursor.adjust(adj);
        }
    }
}

pub struct MarkStore {
    global: HashMap<Mark, (Cursor, BufferId)>,
    buffer: HashMap<BufferId, CursorStore<Mark>>,
}

impl MarkStore {
    pub fn new() -> Self {
        MarkStore { global: HashMap::new(), buffer: HashMap::new() }
    }

    pub fn get(&self, id: BufferId, mark: Mark) -> Option<Cursor> {
        if mark.is_global() {
            let (cursor, owner) = self.global.get(&mark)?;

            if id == *owner {
                Some(cursor.clone())
            } else {
                None
            }
        } else if let Some(bstore) = self.buffer.get(&id) {
            bstore.get(mark)
        } else {
            None
        }
    }

    pub fn put(&mut self, id: BufferId, mark: Mark, cursor: Cursor) {
        if mark.is_global() {
            self.global.insert(mark, (cursor, id));
        } else if let Some(bstore) = self.buffer.get_mut(&id) {
            bstore.put(mark, cursor);
        } else {
            let mut bstore = CursorStore::new();
            bstore.put(mark, cursor);
            self.buffer.insert(id, bstore);
        }
    }

    pub fn adjust(&mut self, id: BufferId, adj: &CursorAdjustment) {
        for (cursor, owner) in self.global.values_mut() {
            if id == *owner {
                cursor.adjust(adj);
            }
        }

        if let Some(bmarks) = self.buffer.get_mut(&id) {
            bmarks.adjust(adj);
        }
    }

    pub fn zero_all(&mut self, id: BufferId) {
        for (cursor, owner) in self.global.values_mut() {
            if id == *owner {
                cursor.zero();
            }
        }

        if let Some(bmarks) = self.buffer.get_mut(&id) {
            bmarks.zero_all();
        }
    }
}

impl Default for MarkStore {
    fn default() -> MarkStore {
        MarkStore::new()
    }
}
