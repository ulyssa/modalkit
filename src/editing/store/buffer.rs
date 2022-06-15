use std::collections::HashMap;
use std::hash::Hash;
use std::sync::{Arc, RwLock};

use crate::editing::base::{Application, EditContext};
use crate::editing::buffer::EditBuffer;
use crate::util::IdGenerator;

use super::SharedStore;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct BufferId(pub(crate) u64);

pub type SharedBuffer<C, P> = Arc<RwLock<EditBuffer<C, P>>>;

pub struct BufferStore<C: EditContext, P: Application> {
    buffers: HashMap<BufferId, SharedBuffer<C, P>>,

    idgen: IdGenerator,
}

impl<C, P> BufferStore<C, P>
where
    C: EditContext,
    P: Application,
{
    pub fn new() -> Self {
        BufferStore {
            buffers: HashMap::new(),

            idgen: IdGenerator::default(),
        }
    }

    pub fn new_buffer(&mut self, store: SharedStore<C, P>) -> SharedBuffer<C, P> {
        let id = BufferId(self.idgen.next());
        let buffer = EditBuffer::new(id, store);
        let buffer = Arc::new(RwLock::new(buffer));

        self.buffers.insert(id, buffer.clone());

        return buffer;
    }

    pub fn get_buffer(&self, id: &BufferId) -> SharedBuffer<C, P> {
        self.buffers.get(id).unwrap().clone()
    }
}
