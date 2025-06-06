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
//! };
//!
//! let store: SharedStore<EmptyInfo> = Store::default().shared();
//! let locked = store.try_read().unwrap();
//!
//! assert_eq!(locked.digraphs.get(('>', '>')), Some('\u{00BB}'));
//! ```
use std::sync::{Arc, RwLock};

use crate::editing::application::ApplicationInfo;
use crate::editing::completion::{Completer, EmptyCompleter};

mod buffer;
mod complete;
mod cursor;
mod digraph;
mod register;

pub use self::buffer::{BufferStore, SharedBuffer};
pub use self::complete::CompletionStore;
pub use self::cursor::{AdjustStore, CursorStore, GlobalAdjustable};
pub use self::digraph::DigraphStore;
pub use self::register::{RegisterCell, RegisterError, RegisterPutFlags, RegisterStore};

/// Global editing context
pub struct Store<I: ApplicationInfo> {
    /// Tracks what [buffers](crate::editing::buffer::EditBuffer) have been created.
    pub buffers: BufferStore<I>,

    /// Tracks information used for text completions.
    pub completions: CompletionStore,

    /// Tracks mapped digraphs.
    pub digraphs: DigraphStore,

    /// Tracks the current value of each [Register](crate::prelude::Register).
    pub registers: RegisterStore,

    /// An application-specific completer.
    pub completer: Box<dyn Completer<I>>,

    /// Tracks globally-relevant cursors and cursor groups.
    pub cursors: CursorStore<I>,

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
            completions: CompletionStore::default(),
            digraphs: DigraphStore::default(),
            registers: RegisterStore::default(),
            cursors: CursorStore::default(),
            completer: Box::new(EmptyCompleter),

            application,
        }
    }

    /// Wrap this store so that it can be shared between threads.
    pub fn shared(self) -> SharedStore<I> {
        return Arc::new(RwLock::new(self));
    }

    /// Get a buffer via its identifier.
    pub fn load_buffer(&mut self, id: I::ContentId) -> SharedBuffer<I> {
        self.buffers.load(id)
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
