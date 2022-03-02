use std::sync::{Arc, RwLock};

use crate::editing::base::EditContext;

mod buffer;
mod cursor;
mod digraph;
mod register;

pub use self::buffer::{BufferId, BufferStore, SharedBuffer};
pub use self::cursor::{CursorStore, MarkStore};
pub use self::digraph::DigraphStore;
pub use self::register::{RegisterCell, RegisterStore};

pub struct Store<C: EditContext> {
    pub buffers: BufferStore<C>,
    pub digraphs: DigraphStore,
    pub registers: RegisterStore,
    pub marks: MarkStore,
}

pub type SharedStore<C> = Arc<RwLock<Store<C>>>;

impl<C: EditContext> Store<C> {
    pub fn new() -> SharedStore<C> {
        let store = Store {
            buffers: BufferStore::new(),
            digraphs: DigraphStore::default(),
            registers: RegisterStore::default(),
            marks: MarkStore::default(),
        };

        return Arc::new(RwLock::new(store));
    }

    pub fn new_buffer(store: &SharedStore<C>) -> SharedBuffer<C> {
        let clone = store.clone();

        store.try_write().unwrap().buffers.new_buffer(clone)
    }

    pub fn get_buffer(id: BufferId, store: &SharedStore<C>) -> SharedBuffer<C> {
        store.try_read().unwrap().buffers.get_buffer(&id)
    }
}
