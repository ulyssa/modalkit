//! # Line annotations
//!
//! ## Overview
//!
//! This module contains logic for tracking line annotations.
//!
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;

use anymap2::SendSyncAnyMap;

/// Tracks different types of annotations for each line within a given buffer.
pub struct LineInfoStore<Line: Eq + Hash> {
    map: HashMap<Line, SendSyncAnyMap>,
}

impl<L> LineInfoStore<L>
where
    L: Eq + Hash,
{
    /// Create a new store.
    pub fn new() -> Self {
        let map = HashMap::new();

        LineInfoStore { map }
    }

    /// Get a reference to the annotation of type `T` on line `L`.
    pub fn get<T: Send + Sync + 'static>(&self, line: &L) -> Option<&T> {
        let info = self.map.get(line)?;

        return info.get();
    }

    /// Get a mutable reference to the annotation of type `T` on line `L`.
    pub fn get_mut<T: Send + Sync + 'static>(&mut self, line: &L) -> Option<&mut T> {
        let info = self.map.get_mut(line)?;

        return info.get_mut();
    }

    /// Create or overwrite an annotation of type `T` on line `L`.
    pub fn set<T: Send + Sync + 'static>(&mut self, line: L, info: T) {
        if let Some(ref mut any) = self.map.get_mut(&line) {
            any.insert(info);
        } else {
            let mut any = SendSyncAnyMap::new();
            any.insert(info);
            self.map.insert(line, any);
        }
    }
}

impl<L> Default for LineInfoStore<L>
where
    L: Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}
