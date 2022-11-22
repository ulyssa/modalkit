//! # Intra-buffer communication
//!
//! ## Overview
//!
//! These components support sharing information between multiple buffers, such as
//! [registers](RegisterStore). Most of the time, you will only need to create a [SharedStore]
//! which contains all of the common components.
//!
//! ## Example
//!
//! ```
//! use modalkit::{
//!     editing::application::EmptyInfo,
//!     editing::store::{SharedStore, Store},
//!     env::vim::VimContext,
//! };
//!
//! fn main() {
//!     let store: SharedStore<EmptyInfo> = Store::default().shared();
//!     let locked = store.try_read().unwrap();
//!
//!     assert_eq!(locked.digraphs.get(('>', '>')), Some('\u{00BB}'));
//! }
//! ```
use std::sync::{Arc, RwLock};

use crate::editing::application::ApplicationInfo;
use crate::editing::history::HistoryList;
use crate::editing::rope::EditRope;

mod buffer;
mod cursor;
mod digraph;
mod register;

pub use self::buffer::{BufferId, BufferStore, SharedBuffer};
pub use self::cursor::{AdjustStore, CursorStore, GlobalAdjustable};
pub use self::digraph::DigraphStore;
pub use self::register::{RegisterCell, RegisterPutFlags, RegisterStore};

const COMMAND_HISTORY_LEN: usize = 50;
const SEARCH_HISTORY_LEN: usize = 50;

/// Global editing context
pub struct Store<I: ApplicationInfo> {
    /// Tracks what [buffers](crate::editing::buffer::EditBuffer) have been created.
    pub buffers: BufferStore<I>,

    /// Tracks mapped digraphs.
    pub digraphs: DigraphStore,

    /// Tracks the current value of each [Register](crate::editing::base::Register).
    pub registers: RegisterStore,

    /// Tracks globally-relevant cursors and cursor groups.
    pub cursors: CursorStore,

    /// Tracks previous commands.
    pub commands: HistoryList<EditRope>,

    /// Tracks previous search expressions.
    pub searches: HistoryList<EditRope>,

    /// Application-specific storage.
    pub application: I::Store,
}

/// Shared reference to the global context.
pub type SharedStore<I> = Arc<RwLock<Store<I>>>;

impl<I> Store<I>
where
    I: ApplicationInfo,
{
    /// Create a new Store using an already initialized application store.
    pub fn new(application: I::Store) -> Self {
        Store {
            buffers: BufferStore::new(),
            digraphs: DigraphStore::default(),
            registers: RegisterStore::default(),
            cursors: CursorStore::default(),

            commands: HistoryList::new("".into(), COMMAND_HISTORY_LEN),
            searches: HistoryList::new("".into(), SEARCH_HISTORY_LEN),

            application,
        }
    }

    /// Wrap this store so that it can be shared between threads.
    pub fn shared(self) -> SharedStore<I> {
        return Arc::new(RwLock::new(self));
    }

    /// Create a new shared buffer.
    pub fn new_buffer(&mut self) -> SharedBuffer<I> {
        self.buffers.new_buffer()
    }

    /// Get a buffer via its identifier.
    pub fn get_buffer(&mut self, id: BufferId) -> SharedBuffer<I> {
        self.buffers.get_buffer(&id)
    }

    /// Add a command to the command history after the prompt has been aborted.
    ///
    /// This will not update [Register::LastCommand].
    ///
    /// [Register::LastCommand]: super::base::Register::LastCommand
    pub fn set_aborted_cmd<T: Into<EditRope>>(&mut self, text: T) {
        let rope = text.into();

        if rope.len() > 0 {
            self.commands.select(rope);
        } else {
            let _ = self.commands.end();
        }
    }

    /// Add a search query to the search history after the prompt has been aborted.
    ///
    /// This will not update [Register::LastSearch].
    ///
    /// [Register::LastSearch]: super::base::Register::LastSearch
    pub fn set_aborted_search<T: Into<EditRope>>(&mut self, text: T) {
        let rope = text.into();

        if rope.len() > 0 {
            self.searches.select(rope);
        } else {
            let _ = self.searches.end();
        }
    }

    /// Add a command to the command history, and set [Register::LastCommand].
    ///
    /// [Register::LastCommand]: super::base::Register::LastCommand
    pub fn set_last_cmd<T: Into<EditRope>>(&mut self, text: T) {
        let rope = text.into();

        if rope.len() == 0 {
            // Disallow empty commands.
            return;
        }

        self.commands.select(rope.clone());
        self.registers.set_last_cmd(rope);
    }

    /// Add a search query to the search history, and set [Register::LastSearch].
    ///
    /// [Register::LastSearch]: super::base::Register::LastSearch
    pub fn set_last_search<T: Into<EditRope>>(&mut self, text: T) {
        let rope = text.into();

        if rope.len() == 0 {
            // Disallow empty searches.
            return;
        }

        self.searches.select(rope.clone());
        self.registers.set_last_search(rope);
    }
}

impl<I> Default for Store<I>
where
    I: ApplicationInfo,
    I::Store: Default,
{
    fn default() -> Self {
        Store::new(I::Store::default())
    }
}
