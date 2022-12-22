use std::cmp::Ordering;
use std::iter::Iterator;
use std::marker::PhantomData;
use std::ops::Range;

use crate::editing::base::{Axis, MoveDir1D};

use super::AxisT;

pub const MIN_WIN_LEN: u16 = 2;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct SizeDescription {
    pub(super) range: Range<usize>,
    pub(super) length: u16,
    pub(super) min: u16,
}

impl SizeDescription {
    pub fn new(range: Range<usize>, length: u16, min: u16) -> Self {
        SizeDescription { range, length, min }
    }

    pub fn allow(&self) -> u16 {
        self.length.saturating_sub(self.min)
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(super) struct ResizeInfo {
    pub lengths: Option<Vec<SizeDescription>>,
}

impl ResizeInfo {
    #[allow(unused)]
    pub fn new(lengths: Vec<SizeDescription>) -> Self {
        ResizeInfo { lengths: Some(lengths) }
    }

    pub fn clear_sizes(&mut self) {
        self.lengths = None;
    }
}

pub(super) type ResizeInfoTrailParent<'a, X, Y> = Option<Box<ResizeInfoTrail<'a, Y, X>>>;

#[derive(Debug, Eq, PartialEq)]
pub(super) struct ResizeInfoTrail<'a, X: AxisT, Y: AxisT> {
    windex: usize,
    current: &'a mut ResizeInfo,
    parents: ResizeInfoTrailParent<'a, X, Y>,
    _p: PhantomData<(X, Y)>,
}

impl<'a, X: AxisT, Y: AxisT> ResizeInfoTrail<'a, X, Y> {
    pub fn new(
        windex: usize,
        current: &'a mut ResizeInfo,
        parents: ResizeInfoTrailParent<'a, X, Y>,
    ) -> Box<Self> {
        Box::new(ResizeInfoTrail { windex, current, parents, _p: PhantomData })
    }

    pub fn clear(&mut self, axis: Axis) {
        if axis == X::axis() {
            self.current.lengths = None;
        }

        if let Some(ref mut parents) = self.parents {
            parents.clear(axis);
        }
    }

    fn find_index(&mut self) -> Option<usize> {
        self.current.lengths.as_ref().and_then(|lengths| {
            let mut lidx = None;

            for (i, size) in lengths.iter().enumerate() {
                if size.range.contains(&self.windex) {
                    lidx = Some(i);
                    break;
                }
            }

            return lidx;
        })
    }

    fn steal_dir(&mut self, dir: MoveDir1D, axis: Axis, need: u16) -> u16 {
        if need == 0 {
            return 0;
        }

        if axis != X::axis() {
            return self
                .parents
                .as_mut()
                .map(|parent: &mut Box<ResizeInfoTrail<'_, Y, X>>| -> u16 {
                    parent.steal_dir(dir, axis, need)
                })
                .unwrap_or(0);
        }

        let lidx = match self.find_index() {
            None => return 0,
            Some(i) => i,
        };

        match self.current.lengths {
            None => return 0,
            Some(ref mut lengths) => {
                let mut take = 0;

                let f = |size: &mut SizeDescription| {
                    let steal = size.allow().min(need - take);
                    size.length -= steal;
                    take += steal;
                };

                match dir {
                    MoveDir1D::Next => {
                        lengths[lidx + 1..].iter_mut().for_each(f);
                    },
                    MoveDir1D::Previous => {
                        lengths[..lidx].iter_mut().rev().for_each(f);
                    },
                };

                if let (Some(ref mut parent), true) = (self.parents.as_mut(), need > take) {
                    take += parent.steal_dir(dir, axis, need - take);
                }

                if let Some(ref mut size) = lengths.get_mut(lidx) {
                    size.length += take;
                }

                return take;
            },
        }
    }

    fn steal(&mut self, axis: Axis, need: u16) -> u16 {
        let amtn = self.steal_dir(MoveDir1D::Next, axis, need);
        let amtp = self.steal_dir(MoveDir1D::Previous, axis, need - amtn);

        amtn + amtp
    }

    fn presplit(&mut self, axis: Axis, need: u16) -> u16 {
        match self.current.lengths {
            None => return 0,
            Some(ref mut lengths) => {
                for ref mut size in lengths.iter_mut() {
                    if !size.range.contains(&self.windex) {
                        continue;
                    }

                    let allow = size.allow();

                    if allow > need {
                        return need;
                    } else {
                        let taken = self.steal(axis, need - allow);
                        let total = allow + taken;

                        return total;
                    }
                }

                return 0;
            },
        }
    }

    fn split(&mut self, rel: MoveDir1D, axis: Axis, need: u16) {
        if axis != X::axis() {
            if let Some(ref mut parent) = self.parents {
                parent.split(rel, axis, need)
            }
            return;
        }

        if let Some(lidx) = self.find_index() {
            let l = self.presplit(axis, need);

            if let Some(ref mut lengths) = self.current.lengths {
                if let Some(ref mut size) = lengths.get_mut(lidx) {
                    let (a, b) = match rel {
                        MoveDir1D::Previous => (l, size.length - l),
                        MoveDir1D::Next => (size.length - l, l),
                    };

                    size.range = lidx + 1..lidx + 2;
                    size.length = b;

                    let range = lidx..lidx + 1;
                    let descr = SizeDescription::new(range, a, MIN_WIN_LEN);

                    lengths.insert(lidx, descr);
                }
            }
        }
    }

    pub fn split_or_clear(&mut self, rel: MoveDir1D, axis: Axis, need: Option<u16>) {
        if let Some(length) = need {
            self.split(rel, axis, length);
        } else {
            self.clear(axis);
        }
    }

    fn spill_dir(&mut self, dir: MoveDir1D, axis: Axis, len: u16) -> u16 {
        if len == 0 {
            return 0;
        }

        if axis != X::axis() {
            return self
                .parents
                .as_mut()
                .map(|parent: &mut Box<ResizeInfoTrail<'_, Y, X>>| -> u16 {
                    parent.spill_dir(dir, axis, len)
                })
                .unwrap_or(0);
        }

        let lidx = match self.find_index() {
            None => return 0,
            Some(i) => i,
        };

        match self.current.lengths {
            None => return 0,
            Some(ref mut lengths) => {
                let mut spill = 0;

                let mut neighbor = match dir {
                    MoveDir1D::Next => lengths.get_mut(lidx + 1),
                    MoveDir1D::Previous => {
                        if lidx > 0 {
                            lengths.get_mut(lidx - 1)
                        } else {
                            None
                        }
                    },
                };

                if let Some(ref mut size) = neighbor {
                    let amt = len - spill;
                    size.length += amt;
                    spill += amt;
                }

                if let (Some(ref mut parent), true) = (self.parents.as_mut(), len > spill) {
                    spill += parent.spill_dir(dir, axis, len - spill);
                }

                if let Some(ref mut size) = lengths.get_mut(lidx) {
                    size.length -= spill;
                }

                return spill;
            },
        }
    }

    fn spill(&mut self, axis: Axis, len: u16) -> u16 {
        let amtn = self.spill_dir(MoveDir1D::Next, axis, len);
        let amtp = self.spill_dir(MoveDir1D::Previous, axis, len - amtn);

        amtn + amtp
    }

    pub fn set_size(&mut self, axis: Axis, len: u16) {
        if axis != X::axis() {
            if let Some(ref mut parent) = self.parents {
                parent.set_size(axis, len);
            }

            return;
        }

        if let Some(lidx) = self.find_index() {
            let len = if let Some(ref size) = self.get_size(lidx) {
                let length = size.length;

                match len.cmp(&length) {
                    Ordering::Greater => length + self.steal(axis, len - length),
                    Ordering::Equal => {
                        return;
                    },
                    Ordering::Less => {
                        let diff = length.saturating_sub(size.min.max(len));

                        length - self.spill(axis, diff)
                    },
                }
            } else {
                return;
            };

            if let Some(ref mut size) = self.get_size(lidx) {
                size.length = len;
            }
        }
    }

    fn get_size(&mut self, lidx: usize) -> Option<&mut SizeDescription> {
        let lengths = self.current.lengths.as_mut()?;

        lengths.get_mut(lidx)
    }
}

#[cfg(test)]
mod tests {
    use super::super::{HorizontalT, VerticalT};
    use super::*;

    type ResizeInfoTrailV<'a> = ResizeInfoTrail<'a, VerticalT, HorizontalT>;

    /*
     *              166
     * <------------------------->
     * +-------------------------+ -
     * | info2 (windows 0, 1, 2) | | 10
     * |-------------------------| -
     * | info3 (windows 3, 4, 5) | | 30
     * |-------------------------| -
     * | info4 (windows 6, 7)    | | 2
     * +-------------------------+ -
     */
    fn mk_info_outer() -> ResizeInfo {
        let sda = SizeDescription::new(0..3, 10, MIN_WIN_LEN);
        let sdb = SizeDescription::new(3..6, 30, MIN_WIN_LEN);
        let sdc = SizeDescription::new(6..8, 2, MIN_WIN_LEN);

        ResizeInfo::new(vec![sda, sdb, sdc])
    }

    /*
     *    50      66      50
     * <-------|-------|------->
     * +-----------------------+ -
     * | win 0 | win 1 | win 2 | | 10
     * +-----------------------+ -
     *
     */
    fn mk_info_top() -> ResizeInfo {
        let sda = SizeDescription::new(0..1, 50, MIN_WIN_LEN);
        let sdb = SizeDescription::new(1..2, 66, MIN_WIN_LEN);
        let sdc = SizeDescription::new(2..3, 50, MIN_WIN_LEN);

        ResizeInfo::new(vec![sda, sdb, sdc])
    }

    /*
     *    50      114     2
     * <-------|-------|------->
     * +-----------------------+ -
     * | win 3 | win 4 | win 5 | | 10
     * +-----------------------+ -
     *
     */
    fn mk_info_middle() -> ResizeInfo {
        let sda = SizeDescription::new(0..1, 50, MIN_WIN_LEN);
        let sdb = SizeDescription::new(1..2, 114, MIN_WIN_LEN);
        let sdc = SizeDescription::new(2..3, 2, MIN_WIN_LEN);

        ResizeInfo::new(vec![sda, sdb, sdc])
    }

    /*
     *     62      104
     * <--------|-------->
     * +-----------------+ -
     * |  win 6 |  win 7 | | 10
     * +-----------------+ -
     *
     */
    fn mk_info_bottom() -> ResizeInfo {
        let sda = SizeDescription::new(0..1, 62, MIN_WIN_LEN);
        let sdb = SizeDescription::new(1..2, 104, MIN_WIN_LEN);

        ResizeInfo::new(vec![sda, sdb])
    }

    struct TestResizeContext {
        outer: ResizeInfo,
        idxo: usize,

        inner: ResizeInfo,
        idxi: usize,
    }

    impl TestResizeContext {
        fn trail(&mut self) -> Box<ResizeInfoTrailV<'_>> {
            let outer = ResizeInfoTrail::new(self.idxo, &mut self.outer, None);
            let inner = ResizeInfoTrail::new(self.idxi, &mut self.inner, Some(outer));

            return inner;
        }

        fn inner_lengths(&self) -> Vec<u16> {
            match self.inner.lengths {
                None => {
                    return vec![];
                },
                Some(ref lengths) => {
                    return lengths.iter().map(|size| size.length).collect();
                },
            }
        }

        fn outer_lengths(&self) -> Vec<u16> {
            match self.outer.lengths {
                None => {
                    return vec![];
                },
                Some(ref lengths) => {
                    return lengths.iter().map(|size| size.length).collect();
                },
            }
        }
    }

    fn mk_info(idx: usize) -> TestResizeContext {
        let o = mk_info_outer();

        match idx {
            0..=2 => {
                TestResizeContext {
                    outer: o,
                    idxo: idx,
                    inner: mk_info_top(),
                    idxi: idx,
                }
            },
            3..=5 => {
                TestResizeContext {
                    outer: o,
                    idxo: idx,
                    inner: mk_info_middle(),
                    idxi: idx - 3,
                }
            },
            6..=7 => {
                TestResizeContext {
                    outer: o,
                    idxo: idx,
                    inner: mk_info_bottom(),
                    idxi: idx - 6,
                }
            },
            _ => panic!(),
        }
    }

    #[test]
    fn test_split_vertical() {
        let mut ctx = mk_info(4);
        let mut trail = ctx.trail();

        trail.split(MoveDir1D::Next, Axis::Vertical, 20);

        assert_eq!(ctx.outer, mk_info_outer());
        assert_eq!(ctx.inner_lengths(), vec![50, 94, 20, 2]);
    }

    #[test]
    fn test_set_size_1_bigger_vertical() {
        let mut ctx = mk_info(1);
        let mut trail = ctx.trail();

        // This is bigger than the available number of columns, so we shrink both neighbors to
        // their minimum width.
        trail.set_size(Axis::Vertical, 1000);

        assert_eq!(ctx.outer, mk_info_outer());
        assert_eq!(ctx.inner_lengths(), vec![2, 162, 2]);
    }

    #[test]
    fn test_set_size_1_bigger_horizontal() {
        let mut ctx = mk_info(1);
        let mut trail = ctx.trail();

        // This is bigger than the available number of rows, so we shrink the two other neighbors
        // to their minimum height.
        trail.set_size(Axis::Horizontal, 1000);

        assert_eq!(ctx.outer_lengths(), vec![38, 2, 2]);
        assert_eq!(ctx.inner, mk_info_top());
    }

    #[test]
    fn test_set_size_5_bigger_vertical() {
        let mut ctx = mk_info(5);
        let mut trail = ctx.trail();

        trail.set_size(Axis::Vertical, 10);

        assert_eq!(ctx.outer, mk_info_outer());
        assert_eq!(ctx.inner_lengths(), vec![50, 106, 10]);
    }

    #[test]
    fn test_set_size_5_bigger_horizontal() {
        let mut ctx = mk_info(5);
        let mut trail = ctx.trail();

        trail.set_size(Axis::Horizontal, 35);

        assert_eq!(ctx.outer_lengths(), vec![5, 35, 2]);
        assert_eq!(ctx.inner, mk_info_middle());
    }

    #[test]
    fn test_set_size_5_smaller_vertical() {
        let mut ctx = mk_info(5);
        let mut trail = ctx.trail();

        // This is smaller than the minimum size, so nothing should change.
        trail.set_size(Axis::Vertical, 1);

        assert_eq!(ctx.outer, mk_info_outer());
        assert_eq!(ctx.inner, mk_info_middle());
    }

    #[test]
    fn test_set_size_5_smaller_horizontal() {
        let mut ctx = mk_info(5);
        let mut trail = ctx.trail();

        trail.set_size(Axis::Horizontal, 5);

        assert_eq!(ctx.outer_lengths(), vec![10, 5, 27]);
        assert_eq!(ctx.inner, mk_info_middle());
    }

    #[test]
    fn test_clear_axis_vertical() {
        let mut ctx = mk_info(5);
        let mut trail = ctx.trail();

        trail.clear(Axis::Vertical);

        assert_eq!(ctx.outer, mk_info_outer());
        assert_eq!(ctx.inner.lengths, None);
    }

    #[test]
    fn test_clear_axis_horizontal() {
        let mut ctx = mk_info(5);
        let mut trail = ctx.trail();

        trail.clear(Axis::Horizontal);

        assert_eq!(ctx.inner, mk_info_middle());
        assert_eq!(ctx.outer.lengths, None);
    }
}
