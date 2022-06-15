use std::sync::{Arc, RwLock};

use crate::editing::base::{Application, EditContext};

mod buffer;
mod cursor;
mod digraph;
mod register;

pub use self::buffer::{BufferId, BufferStore, SharedBuffer};
pub use self::cursor::{CursorStore, MarkStore};
pub use self::digraph::DigraphStore;
pub use self::register::{RegisterCell, RegisterStore};

pub struct Store<C: EditContext, P: Application> {
    pub buffers: BufferStore<C, P>,
    pub digraphs: DigraphStore,
    pub registers: RegisterStore,
    pub marks: MarkStore,

    pub application: P::Store,
}

pub type SharedStore<C, P> = Arc<RwLock<Store<C, P>>>;

impl<C, P> Store<C, P>
where
    C: EditContext,
    P: Application,
{
    pub fn new() -> SharedStore<C, P> {
        let store = Store {
            buffers: BufferStore::new(),
            digraphs: DigraphStore::default(),
            registers: RegisterStore::default(),
            marks: MarkStore::default(),

            application: P::Store::default(),
        };

        return Arc::new(RwLock::new(store));
    }

    pub fn new_buffer(store: &SharedStore<C, P>) -> SharedBuffer<C, P> {
        let clone = store.clone();

        store.try_write().unwrap().buffers.new_buffer(clone)
    }

    pub fn get_buffer(id: BufferId, store: &SharedStore<C, P>) -> SharedBuffer<C, P> {
        store.try_read().unwrap().buffers.get_buffer(&id)
    }
}
