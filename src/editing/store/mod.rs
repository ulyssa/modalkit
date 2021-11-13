use std::hash::Hash;
use std::sync::{Arc, RwLock};

use crate::util::IdGenerator;

mod cursor;
mod digraph;
mod register;

pub use self::cursor::{CursorStore, MarkStore};
pub use self::digraph::DigraphStore;
pub use self::register::{RegisterCell, RegisterStore};

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct BufferId(u64);

pub struct Store {
    pub digraphs: DigraphStore,
    pub registers: RegisterStore,
    pub marks: MarkStore,

    idgen: IdGenerator,
}

pub type SharedStore = Arc<RwLock<Store>>;

impl Store {
    pub fn new() -> SharedStore {
        let store = Store {
            digraphs: DigraphStore::default(),
            registers: RegisterStore::default(),
            marks: MarkStore::default(),

            idgen: IdGenerator::default(),
        };

        return Arc::new(RwLock::new(store));
    }

    pub fn next_buffer_id(&mut self) -> BufferId {
        BufferId(self.idgen.next())
    }
}
