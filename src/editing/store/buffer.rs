use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use crate::editing::application::ApplicationInfo;
use crate::editing::buffer::EditBuffer;

/// A shared reference to an [EditBuffer].
pub type SharedBuffer<I> = Arc<RwLock<EditBuffer<I>>>;

/// Globally track allocated buffers.
pub struct BufferStore<I: ApplicationInfo> {
    buffers: HashMap<I::ContentId, SharedBuffer<I>>,
}

impl<I> BufferStore<I>
where
    I: ApplicationInfo,
{
    /// Create a new buffer store.
    pub fn new() -> Self {
        BufferStore { buffers: HashMap::new() }
    }

    fn mkbuf(&mut self, id: I::ContentId) -> SharedBuffer<I> {
        let buffer = EditBuffer::new(id);

        return Arc::new(RwLock::new(buffer));
    }

    fn mkbufstr(&mut self, s: &str, id: I::ContentId) -> SharedBuffer<I> {
        let buffer = EditBuffer::from_str(id, s);

        return Arc::new(RwLock::new(buffer));
    }

    /// Get a buffer for storing the sub-content of a window.
    pub fn entry(&mut self, id: I::ContentId) -> Entry<I::ContentId, SharedBuffer<I>> {
        self.buffers.entry(id)
    }

    /// Get a buffer for storing the sub-content of a window.
    pub fn load(&mut self, id: I::ContentId) -> SharedBuffer<I> {
        if let Some(buffer) = self.buffers.get(&id) {
            return buffer.clone();
        } else {
            let buffer = self.mkbuf(id.clone());

            self.buffers.insert(id, buffer.clone());

            return buffer;
        }
    }

    /// Get a buffer for storing the sub-content of a window; if it doesn't exist yet, create one
    /// whose initial contents are `s`.
    pub fn load_str(&mut self, id: I::ContentId, s: &str) -> SharedBuffer<I> {
        if let Some(buffer) = self.buffers.get(&id) {
            return buffer.clone();
        } else {
            let buffer = self.mkbufstr(s, id.clone());

            self.buffers.insert(id, buffer.clone());

            return buffer;
        }
    }
}
