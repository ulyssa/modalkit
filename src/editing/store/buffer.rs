use std::collections::HashMap;
use std::hash::Hash;
use std::sync::{Arc, RwLock};

use crate::editing::application::ApplicationInfo;
use crate::editing::buffer::EditBuffer;
use crate::util::IdGenerator;

/// Identifier for an [EditBuffer].
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct BufferId(pub(crate) u64);

/// A shared reference to an [EditBuffer].
pub type SharedBuffer<I> = Arc<RwLock<EditBuffer<I>>>;

/// Globally track allocated buffers.
pub struct BufferStore<I: ApplicationInfo> {
    buffers: HashMap<BufferId, SharedBuffer<I>>,

    idgen: IdGenerator,
}

impl<I> BufferStore<I>
where
    I: ApplicationInfo,
{
    /// Create a new buffer store.
    pub fn new() -> Self {
        BufferStore {
            buffers: HashMap::new(),

            idgen: IdGenerator::default(),
        }
    }

    /// Allocate a new buffer.
    pub fn new_buffer(&mut self) -> SharedBuffer<I> {
        let id = BufferId(self.idgen.next());
        let buffer = EditBuffer::new(id);
        let buffer = Arc::new(RwLock::new(buffer));

        self.buffers.insert(id, buffer.clone());

        return buffer;
    }

    /// Create a new buffer whose contents are `s`.
    pub fn from_str(&mut self, s: &str) -> SharedBuffer<I> {
        let id = BufferId(self.idgen.next());
        let buffer = EditBuffer::from_str(id, s);
        let buffer = Arc::new(RwLock::new(buffer));

        self.buffers.insert(id, buffer.clone());

        return buffer;
    }

    /// Get a shared reference to a buffer given its identifier.
    pub fn get_buffer(&self, id: &BufferId) -> SharedBuffer<I> {
        self.buffers.get(id).unwrap().clone()
    }
}
