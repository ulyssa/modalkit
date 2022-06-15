use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;

use anymap2::SendSyncAnyMap;

pub struct LineInfoStore<Line: Eq + Hash> {
    map: HashMap<Line, SendSyncAnyMap>,
}

impl<L> LineInfoStore<L>
where
    L: Eq + Hash,
{
    pub fn new() -> Self {
        let map = HashMap::new();

        LineInfoStore { map }
    }

    pub fn get<T: Send + Sync + 'static>(&self, line: &L) -> Option<&T> {
        let info = self.map.get(line)?;

        return info.get();
    }

    pub fn get_mut<T: Send + Sync + 'static>(&mut self, line: &L) -> Option<&mut T> {
        let info = self.map.get_mut(line)?;

        return info.get_mut();
    }

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
