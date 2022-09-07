use std::collections::HashMap;
use std::hash::Hash;

use crate::editing::base::Mark;
use crate::editing::cursor::{Cursor, CursorAdjustment};

use super::BufferId;

/// Map a given identifier onto a [Cursor].
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
    /// Create a new cursor store.
    pub fn new() -> Self {
        CursorStore { map: HashMap::new() }
    }

    /// Check whether an identifier is mapped to a [Cursor].
    pub fn contains(&mut self, mark: M) -> bool {
        self.map.contains_key(&mark)
    }

    /// Delete a mapping.
    pub fn del(&mut self, mark: M) {
        let _ = self.map.remove(&mark);
    }

    /// Get the [Cursor] referenced by the given identifier, if it exists.
    pub fn get(&self, mark: M) -> Option<Cursor> {
        self.map.get(&mark).map(Cursor::clone)
    }

    /// Update the [Cursor] mapped to by an identifier.
    pub fn put(&mut self, mark: M, cursor: Cursor) {
        self.map.insert(mark, cursor);
    }

    /// Update all stored cursors to point to the first column of the first line.
    pub fn zero_all(&mut self) {
        for cursor in self.map.values_mut() {
            cursor.zero();
        }
    }

    /// Adjust any matching cursors as described.
    pub fn adjust(&mut self, adj: &CursorAdjustment) {
        for cursor in self.map.values_mut() {
            cursor.adjust(adj);
        }
    }
}

/// Map marks onto cursors.
pub struct MarkStore {
    global: HashMap<Mark, (Cursor, BufferId)>,
    buffer: HashMap<BufferId, CursorStore<Mark>>,
}

impl MarkStore {
    /// Create a new mark store.
    pub fn new() -> Self {
        MarkStore { global: HashMap::new(), buffer: HashMap::new() }
    }

    /// Get the [Cursor] mapped to by [Mark] for the specified buffer.
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

    /// Update the [Cursor] mapped to by [Mark] for the specified buffer.
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

    /// Adjust all marks associated with a buffer as described.
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

    /// Update all marks associated with a buffer to point to the first column of the first line.
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
