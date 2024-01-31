use std::cmp::{Ord, Ordering};
use std::slice::IterMut;

use crate::prelude::*;
use crate::{
    editing::cursor::{Adjustable, CursorAdjustment, CursorState},
    editing::rope::EditRope,
    util::idx_offset,
};

/// Errors returned when combining cursor groups.
#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum CursorGroupCombineError {
    /// Empty cursor group.
    #[error("Specified register does not contain a cursor group")]
    Empty(Register),

    /// The cursors groups need to be the same size.
    #[error("The two groups are not the same size ({0} vs {1})")]
    SizeMismatch(usize, usize),
}

/// Iterate over references to [CursorState] values within a [CursorGroup].
pub struct CursorGroupIter<'a> {
    members: std::vec::IntoIter<&'a CursorState>,
    leader: Option<&'a CursorState>,
}

impl<'a> Iterator for CursorGroupIter<'a> {
    type Item = &'a CursorState;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(m) = self.members.next() {
            if let Some(ref mut l) = self.leader {
                if let Ordering::Less = Ord::cmp(l, &m) {
                    return std::mem::replace(l, m).into();
                }
            }

            return Some(m);
        }

        return self.leader.take();
    }
}

impl<'a> DoubleEndedIterator for CursorGroupIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(m) = self.members.next_back() {
            if let Some(ref mut l) = self.leader {
                if let Ordering::Greater = Ord::cmp(l, &m) {
                    return std::mem::replace(l, m).into();
                }
            }

            return Some(m);
        }

        return self.leader.take();
    }
}

/// Iterate over mutable references to [CursorState] values within a [CursorGroup].
pub struct CursorGroupIterMut<'a> {
    members: IterMut<'a, CursorState>,
    leader: Option<&'a mut CursorState>,
}

impl<'a> Iterator for CursorGroupIterMut<'a> {
    type Item = &'a mut CursorState;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(m) = self.members.next() {
            if let Some(ref mut l) = self.leader {
                if let Ordering::Less = Ord::cmp(l, &m) {
                    return std::mem::replace(l, m).into();
                }
            }

            return Some(m);
        }

        return self.leader.take();
    }
}

impl<'a> DoubleEndedIterator for CursorGroupIterMut<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(m) = self.members.next_back() {
            if let Some(ref mut l) = self.leader {
                if let Ordering::Greater = Ord::cmp(l, &m) {
                    return std::mem::replace(l, m).into();
                }
            }

            return Some(m);
        }

        return self.leader.take();
    }
}

/// A group of cursors, where one of them has on-screen focus.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CursorGroup {
    /// The members of the cursor group which do not have have on-screen focus.
    pub members: Vec<CursorState>,

    /// The main cursor, which has on-screen focus.
    pub leader: CursorState,
}

impl CursorGroup {
    /// Create a new [CursorGroup] with the given leader and followers.
    pub fn new(leader: CursorState, members: Vec<CursorState>) -> Self {
        let mut group = CursorGroup { leader, members };

        group.merge();

        return group;
    }

    fn len(&self) -> usize {
        self.members.len() + 1
    }

    /// Ensure that the member cursor states are sorted by their cursors.
    pub fn sort(&mut self) {
        self.members.sort()
    }

    /// Iterate over mutable references to the [CursorState] values in this group, in cursor order.
    pub fn iter_mut(&mut self) -> CursorGroupIterMut<'_> {
        self.sort();

        CursorGroupIterMut {
            members: self.members.iter_mut(),
            leader: Some(&mut self.leader),
        }
    }

    /// Iterate over references to the [CursorState] values in this group, in cursor order.
    pub fn iter(&self) -> CursorGroupIter<'_> {
        let mut members = self.members.iter().collect::<Vec<_>>();
        members.sort();

        CursorGroupIter {
            members: members.into_iter(),
            leader: Some(&self.leader),
        }
    }

    /// Clone each cursor in the group *n* times.
    pub fn split(&mut self, count: usize) {
        let mut created = vec![];

        for state in self.iter_mut() {
            for _ in 0..count {
                created.push(state.clone());
            }
        }

        self.members.append(&mut created);
        self.members.sort();
    }

    /// Close some of the cursors in this cursor group.
    pub fn close(&mut self, target: &CursorCloseTarget) {
        match target {
            CursorCloseTarget::Leader => {
                self.members.sort();

                let midx =
                    self.members.iter().position(|state| self.leader.cursor() < state.cursor());

                if let Some(idx) = midx {
                    self.leader = self.members.remove(idx);
                } else if let Some(leader) = self.members.pop() {
                    self.leader = leader;
                }
            },
            CursorCloseTarget::Followers => {
                self.members = vec![];
            },
        }
    }

    /// Rotate the cursor leader forwards or backwards *n* times.
    pub fn rotate(&mut self, dir: MoveDir1D, offset: usize) {
        if self.members.is_empty() {
            return;
        }

        // Make the leader one of the members.
        let mut members = vec![(true, std::mem::take(&mut self.leader))];

        for member in std::mem::take(&mut self.members).into_iter() {
            members.push((false, member));
        }

        // Sort all of the members by their cursor positions.
        members.sort_by(|a, b| {
            return a.1.cursor().cmp(b.1.cursor());
        });

        // Calculate the relative offset of the new leader from the old leader.
        let idx = members
            .iter()
            .position(|(leader, _)| *leader)
            .map(|idx| idx_offset(idx, offset, &dir, members.len(), true).unwrap_or(idx))
            .unwrap_or(0);

        // Promote the new leader.
        self.leader = members.remove(idx).1;
        self.members = members.into_iter().map(|p| p.1).collect();
    }

    /// Combine this cursor group with another.
    pub fn combine(
        &self,
        other: &Self,
        style: &CursorGroupCombineStyle,
        rope: &EditRope,
    ) -> Result<Self, CursorGroupCombineError> {
        let group = match style {
            CursorGroupCombineStyle::Append => {
                let leader = other.leader.clone();
                let members = other.members.iter().chain(self.iter()).cloned().collect::<Vec<_>>();

                CursorGroup::new(leader, members)
            },
            CursorGroupCombineStyle::Merge(style) => {
                let alen = self.len();
                let blen = other.len();

                if alen != blen {
                    let err = CursorGroupCombineError::SizeMismatch(alen, blen);

                    return Err(err);
                }

                // Preserve relative leader position.
                let mut a = vec![(&self.leader, true)];
                let mut b = vec![&other.leader];

                for member in self.members.iter() {
                    a.push((member, false));
                }

                for member in other.members.iter() {
                    b.push(member);
                }

                a.sort();
                b.sort();

                let mut leader = CursorState::default();
                let mut members = vec![];

                for ((a, leads), b) in a.iter().zip(b.iter()) {
                    let c = a.merge(b, style, rope);

                    if *leads {
                        leader = c;
                    } else {
                        members.push(c);
                    }
                }

                CursorGroup::new(leader, members)
            },
            CursorGroupCombineStyle::Replace => other.clone(),
        };

        return Ok(group);
    }

    /// Merge any overlapping selections.
    pub fn merge(&mut self) {
        if self.members.is_empty() {
            // There's nothing to merge.
            return;
        }

        // Begin by sorting and merging member selections.
        let mut unmerged = std::mem::take(&mut self.members);
        unmerged.sort_by(|a, b| a.start().cmp(b.start()));

        let mut iter = unmerged.into_iter();
        let mut merged = Vec::new();

        let mut a = iter.next().unwrap();

        for b in iter {
            if a.overlaps(&b) {
                // Merge w/ previous selection.
                a = a.union(&b);
            } else {
                // Save the previous selection, and begin merges into this one.
                merged.push(a);
                a = b;
            }
        }

        // Commit last member.
        merged.push(a);

        // Now we can merge them into the current leader.
        for m in merged.into_iter() {
            if self.leader.overlaps(&m) {
                // Merge this selection into the leader.
                self.leader = self.leader.union(&m);
            } else {
                // Keep this selection.
                self.members.push(m);
            }
        }
    }
}

impl Adjustable for CursorGroup {
    fn zero(&mut self) {
        for member in self.members.iter_mut() {
            member.zero();
        }

        self.leader.zero();
    }

    fn adjust(&mut self, adjs: &[CursorAdjustment]) {
        for member in self.members.iter_mut() {
            member.adjust(adjs);
        }

        self.leader.adjust(adjs);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::cursor::Cursor;

    #[test]
    fn test_combine_append() {
        // Define some unique cursors.
        let c0 = CursorState::Location(Cursor::new(0, 0));
        let c1 = CursorState::Location(Cursor::new(0, 1));
        let c2 = CursorState::Location(Cursor::new(0, 2));
        let c3 = CursorState::Location(Cursor::new(0, 3));
        let c4 = CursorState::Location(Cursor::new(0, 4));

        // Create two cursor groups.
        let a = CursorGroup::new(c1.clone(), vec![c0.clone(), c2.clone()]);
        let b = CursorGroup::new(c4.clone(), vec![c3.clone()]);
        let r = EditRope::from("");

        let res = a.combine(&b, &CursorGroupCombineStyle::Append, &r).unwrap();
        let exp =
            CursorGroup::new(c4.clone(), vec![c0.clone(), c1.clone(), c2.clone(), c3.clone()]);
        assert_eq!(res, exp);
    }

    #[test]
    fn test_combine_merge_error() {
        // Define some unique cursors.
        let c0 = CursorState::Location(Cursor::new(0, 0));
        let c1 = CursorState::Location(Cursor::new(0, 1));
        let c2 = CursorState::Location(Cursor::new(0, 2));
        let c3 = CursorState::Location(Cursor::new(0, 3));
        let c4 = CursorState::Location(Cursor::new(0, 4));

        // Create two cursor groups.
        let a = CursorGroup::new(c1.clone(), vec![c0.clone(), c2.clone()]);
        let b = CursorGroup::new(c4.clone(), vec![c3.clone()]);
        let r = EditRope::from("");

        let res = a.combine(&b, &CursorMergeStyle::Union.into(), &r);
        assert_eq!(res, Err(CursorGroupCombineError::SizeMismatch(3, 2)));
    }

    #[test]
    fn test_combine_replace() {
        // Define some unique cursors.
        let c0 = CursorState::Location(Cursor::new(0, 0));
        let c1 = CursorState::Location(Cursor::new(0, 1));
        let c2 = CursorState::Location(Cursor::new(0, 2));
        let c3 = CursorState::Location(Cursor::new(0, 3));
        let c4 = CursorState::Location(Cursor::new(0, 4));

        // Create two cursor groups.
        let a = CursorGroup::new(c1.clone(), vec![c0.clone(), c2.clone()]);
        let b = CursorGroup::new(c4.clone(), vec![c3.clone()]);
        let r = EditRope::from("");

        // a replaced with b is b.
        let res = a.combine(&b, &CursorGroupCombineStyle::Replace, &r).unwrap();
        assert_eq!(res, b);

        // b replaced with a is a.
        let res = b.combine(&a, &CursorGroupCombineStyle::Replace, &r).unwrap();
        assert_eq!(res, a);
    }

    #[test]
    fn test_group_iter() {
        let cw = TargetShape::CharWise;
        let c1 = CursorState::Location(Cursor::new(0, 1));
        let c2 = CursorState::Location(Cursor::new(0, 2));
        let s3 = CursorState::Selection(Cursor::new(0, 3), Cursor::new(0, 4), cw);
        let c5 = CursorState::Location(Cursor::new(0, 5));
        let c7 = CursorState::Location(Cursor::new(0, 7));

        // Skip the merge() call in new() so we can verify that members gets sorted.
        let group = CursorGroup {
            leader: c5.clone(),
            members: vec![c7.clone(), c2.clone(), c1.clone(), s3.clone()],
        };

        let mut iter = group.iter();

        assert_eq!(iter.next(), Some(&c1));
        assert_eq!(iter.next(), Some(&c2));
        assert_eq!(iter.next(), Some(&s3));
        assert_eq!(iter.next(), Some(&c5));
        assert_eq!(iter.next(), Some(&c7));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_group_iter_mut() {
        let cw = TargetShape::CharWise;
        let mut c1 = CursorState::Location(Cursor::new(0, 1));
        let mut c2 = CursorState::Location(Cursor::new(0, 2));
        let mut s3 = CursorState::Selection(Cursor::new(0, 3), Cursor::new(0, 4), cw);
        let mut c5 = CursorState::Location(Cursor::new(0, 5));
        let mut c7 = CursorState::Location(Cursor::new(0, 7));

        // Skip the merge() call in new() so we can verify that members gets sorted.
        let mut group = CursorGroup {
            leader: c5.clone(),
            members: vec![c7.clone(), c2.clone(), c1.clone(), s3.clone()],
        };

        let mut iter = group.iter_mut();

        assert_eq!(iter.next(), Some(&mut c1));
        assert_eq!(iter.next(), Some(&mut c2));
        assert_eq!(iter.next(), Some(&mut s3));
        assert_eq!(iter.next(), Some(&mut c5));
        assert_eq!(iter.next(), Some(&mut c7));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_group_merge() {
        let cw = TargetShape::CharWise;
        let s1 = CursorState::Selection(Cursor::new(0, 2), Cursor::new(0, 5), cw);
        let s2 = CursorState::Selection(Cursor::new(2, 4), Cursor::new(2, 7), cw);
        let s3 = CursorState::Selection(Cursor::new(0, 0), Cursor::new(0, 3), cw);
        let s4 = CursorState::Selection(Cursor::new(0, 6), Cursor::new(2, 3), cw);
        let s5 = CursorState::Selection(Cursor::new(3, 0), Cursor::new(3, 5), cw);
        let s6 = CursorState::Selection(Cursor::new(5, 0), Cursor::new(2, 7), cw);

        // Skip the merge() call in new() so we do the call ourselves.
        let mut group = CursorGroup {
            leader: s1.clone(),
            members: vec![s2.clone(), s3.clone(), s4.clone(), s5.clone(), s6.clone()],
        };

        group.merge();

        assert_eq!(group.leader, CursorState::Selection(Cursor::new(0, 0), Cursor::new(0, 5), cw));
        assert_eq!(group.members, vec![
            CursorState::Selection(Cursor::new(0, 6), Cursor::new(2, 3), cw),
            CursorState::Selection(Cursor::new(2, 4), Cursor::new(5, 0), cw),
        ]);
    }

    #[test]
    fn test_group_rotate() {
        let c0 = CursorState::Location(Cursor::new(0, 0));
        let c1 = CursorState::Location(Cursor::new(0, 1));
        let c2 = CursorState::Location(Cursor::new(0, 2));
        let c3 = CursorState::Location(Cursor::new(0, 3));
        let c4 = CursorState::Location(Cursor::new(0, 4));
        let c5 = CursorState::Location(Cursor::new(0, 5));
        let mut group = CursorGroup::new(c1.clone(), vec![
            c0.clone(),
            c2.clone(),
            c3.clone(),
            c4.clone(),
            c5.clone(),
        ]);

        group.rotate(MoveDir1D::Next, 2);
        assert_eq!(group.leader, c3.clone());
        assert_eq!(group.members, vec![c0.clone(), c1.clone(), c2.clone(), c4.clone(), c5.clone()]);

        group.rotate(MoveDir1D::Next, 3);
        assert_eq!(group.leader, c0.clone());
        assert_eq!(group.members, vec![c1.clone(), c2.clone(), c3.clone(), c4.clone(), c5.clone()]);

        group.rotate(MoveDir1D::Previous, 1);
        assert_eq!(group.leader, c5.clone());
        assert_eq!(group.members, vec![c0.clone(), c1.clone(), c2.clone(), c3.clone(), c4.clone()]);
    }
}
