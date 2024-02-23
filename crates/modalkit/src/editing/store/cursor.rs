use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

use crate::editing::{
    application::{ApplicationContentId, ApplicationInfo},
    cursor::{Adjustable, Cursor, CursorAdjustment, CursorGroup, CursorGroupCombineError},
};
use crate::errors::{EditError, EditResult};
use crate::prelude::{Mark, Register};

/// Trait for objects that store cursors from multiple buffers.
pub trait GlobalAdjustable<ID: ApplicationContentId> {
    /// Zero out cursors associated with a given buffer.
    fn zero_id(&mut self, id: &ID);

    /// Adjust cursors associated with a given buffer.
    fn adjust_id(&mut self, id: &ID, adjs: &[CursorAdjustment]);
}

impl<C, ID> GlobalAdjustable<ID> for (ID, C)
where
    C: Adjustable,
    ID: ApplicationContentId,
{
    fn zero_id(&mut self, id: &ID) {
        if id == &self.0 {
            self.1.zero();
        }
    }

    fn adjust_id(&mut self, id: &ID, adjs: &[CursorAdjustment]) {
        if id == &self.0 {
            self.1.adjust(adjs);
        }
    }
}

/// Map an identifier onto an [Adjustable] or a [GlobalAdjustable].
#[derive(Clone, Debug)]
pub struct AdjustStore<M, C = Cursor>
where
    M: Copy + Eq + Hash,
{
    map: HashMap<M, C>,
}

impl<M, C> Default for AdjustStore<M, C>
where
    M: Copy + Eq + Hash,
{
    fn default() -> Self {
        AdjustStore::new()
    }
}

impl<M, C> AdjustStore<M, C>
where
    M: Copy + Eq + Hash,
{
    /// Create a new cursor store.
    pub fn new() -> Self {
        AdjustStore { map: HashMap::new() }
    }

    /// Delete a mapping.
    pub fn del(&mut self, id: M) {
        let _ = self.map.remove(&id);
    }

    /// Get a reference to the identified value, if it exists.
    pub fn get(&self, id: M) -> Option<&C> {
        self.map.get(&id)
    }

    /// Get a mutable reference to the identified value, if it exists.
    pub fn get_mut(&mut self, id: M) -> Option<&mut C> {
        self.map.get_mut(&id)
    }

    /// Use the [Entry] interface to manipulate the value.
    pub fn entry(&mut self, id: M) -> Entry<M, C> {
        self.map.entry(id)
    }

    /// Update the value mapped to by an identifier.
    pub fn put(&mut self, id: M, cursor: C) {
        self.map.insert(id, cursor);
    }
}

impl<M, C> Adjustable for AdjustStore<M, C>
where
    M: Copy + Eq + Hash,
    C: Adjustable,
{
    /// Update all stored cursors to point to the first column of the first line.
    fn zero(&mut self) {
        for cursor in self.map.values_mut() {
            cursor.zero();
        }
    }

    /// Adjust any matching cursors as described.
    fn adjust(&mut self, adj: &[CursorAdjustment]) {
        for cursor in self.map.values_mut() {
            cursor.adjust(adj);
        }
    }
}

impl<M, C, ID> GlobalAdjustable<ID> for AdjustStore<M, C>
where
    M: Copy + Eq + Hash,
    C: GlobalAdjustable<ID>,
    ID: ApplicationContentId,
{
    fn zero_id(&mut self, id: &ID) {
        for vals in self.map.values_mut() {
            vals.zero_id(id);
        }
    }

    fn adjust_id(&mut self, id: &ID, adjs: &[CursorAdjustment]) {
        for val in self.map.values_mut() {
            val.adjust_id(id, adjs);
        }
    }
}

/// Tracks important cursors and cursor groups:
///
/// - Saved cursors (see [EditorAction::Mark])
/// - Saved cursor groups (see [CursorAction::Save])
///
/// [EditorAction::Mark]: crate::actions::EditorAction::Mark
/// [CursorAction::Save]: crate::actions::CursorAction::Save
pub struct CursorStore<I>
where
    I: ApplicationInfo,
{
    /// Tracks global marks.
    global: HashMap<Mark, (I::ContentId, Cursor)>,

    /// Tracks buffer-local marks.
    buffer: HashMap<I::ContentId, AdjustStore<Mark>>,

    /// Tracks saved cursor groups.
    groups: HashMap<Register, (I::ContentId, CursorGroup)>,
}

impl<I> CursorStore<I>
where
    I: ApplicationInfo,
{
    /// Create a new mark store.
    pub fn new() -> Self {
        CursorStore {
            global: HashMap::new(),
            buffer: HashMap::new(),
            groups: HashMap::new(),
        }
    }

    /// Get the [Cursor] mapped to by [Mark] for the specified buffer.
    pub fn get_mark(&self, id: I::ContentId, mark: Mark) -> EditResult<Cursor, I> {
        let unset = EditError::MarkNotSet(mark);

        if mark.is_global() {
            let (owner, cursor) = self.global.get(&mark).ok_or(unset)?.to_owned();

            if id == owner {
                Ok(cursor)
            } else {
                Err(EditError::WrongBuffer(owner))
            }
        } else if let Some(bstore) = self.buffer.get(&id) {
            bstore.get(mark).map(Cursor::clone).ok_or(unset)
        } else {
            Err(unset)
        }
    }

    /// Update the [Cursor] mapped to by [Mark] for the specified buffer.
    pub fn set_mark(&mut self, id: I::ContentId, mark: Mark, cursor: Cursor) {
        if mark.is_global() {
            self.global.insert(mark, (id, cursor));
        } else {
            self.buffer.entry(id).or_default().put(mark, cursor);
        }
    }

    /// Restore a cursor group from a given [Register].
    pub fn get_group(&self, id: I::ContentId, reg: &Register) -> EditResult<CursorGroup, I> {
        let (owner, group) = self
            .groups
            .get(reg)
            .ok_or_else(|| EditError::from(CursorGroupCombineError::Empty(reg.clone())))?
            .to_owned();

        if id == owner {
            Ok(group)
        } else {
            Err(EditError::WrongBuffer(owner))
        }
    }

    /// Save a cursor group to a given [Register].
    pub fn set_group(
        &mut self,
        id: I::ContentId,
        reg: Register,
        group: CursorGroup,
    ) -> EditResult<(), I> {
        if reg.is_cursor_storage() {
            self.groups.insert(reg, (id, group));

            return Ok(());
        } else {
            let msg = "Invalid register for cursor groups";
            let err = EditError::Failure(msg.into());

            return Err(err);
        }
    }
}

impl<I> GlobalAdjustable<I::ContentId> for CursorStore<I>
where
    I: ApplicationInfo,
{
    /// Update all marks associated with a buffer to point to the first column of the first line.
    fn zero_id(&mut self, id: &I::ContentId) {
        for val in self.global.values_mut() {
            val.zero_id(id);
        }

        for val in self.groups.values_mut() {
            val.zero_id(id);
        }

        if let Some(bmarks) = self.buffer.get_mut(id) {
            bmarks.zero();
        }
    }

    /// Adjust all marks associated with a buffer as described.
    fn adjust_id(&mut self, id: &I::ContentId, adjs: &[CursorAdjustment]) {
        for val in self.global.values_mut() {
            val.adjust_id(id, adjs);
        }

        for val in self.groups.values_mut() {
            val.adjust_id(id, adjs);
        }

        if let Some(bmarks) = self.buffer.get_mut(id) {
            bmarks.adjust(adjs);
        }
    }
}

impl<I> Default for CursorStore<I>
where
    I: ApplicationInfo,
{
    fn default() -> CursorStore<I> {
        CursorStore::new()
    }
}
