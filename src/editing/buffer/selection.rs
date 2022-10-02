use std::cmp::Ordering;

use crate::editing::cursor::{block_cursors, Cursor};
use crate::editing::rope::PrivateCursorOps;
use crate::util::sort2;

use crate::editing::action::{EditAction, EditError, EditResult};

use crate::editing::base::{
    Application,
    Count,
    CursorMovements,
    EditContext,
    EditTarget,
    MoveDir1D,
    MoveTerminus,
    SelectionCursorChange,
    SelectionResizeStyle,
    SelectionSplitStyle,
    TargetShape,
    TargetShapeFilter,
};

use super::{CursorGroupId, CursorGroupIdContext, EditBuffer};

pub trait SelectionActions<C> {
    /// Move where the cursor is located in a selection.
    fn selection_cursor_set(&mut self, side: &SelectionCursorChange, ctx: &C) -> EditResult;

    /// Duplicate existing selections onto the lines below them.
    fn selection_duplicate(&mut self, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult;

    /// Change the boundaries of the selection to be exactly those of the range.
    fn selection_resize(
        &mut self,
        style: SelectionResizeStyle,
        target: &EditTarget,
        ctx: &C,
    ) -> EditResult;

    /// Split a multiline selection into multiple single-line selections.
    fn selection_split(
        &mut self,
        style: SelectionSplitStyle,
        filter: TargetShapeFilter,
        ctx: &C,
    ) -> EditResult;

    /// Remove whitespace from the ends of matching selections.
    fn selection_trim(&mut self, filter: TargetShapeFilter, ctx: &C) -> EditResult;
}

impl<'a, 'b, C, P> EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    pub(super) fn _merge_selections(&mut self, gid: CursorGroupId) -> EditResult {
        let EditBuffer { anchors, cursors, leaders, members, vshapes, .. } = self;

        let leader = *leaders.get(&gid).expect("no leader in cursor group");
        let cursor = cursors.get(leader).ok_or(EditError::InvalidCursor)?;
        let anchor = anchors.get(leader).unwrap_or_else(|| cursor.clone());
        let (mut lsels, mut lsele, before) = if cursor < anchor {
            (cursor, anchor, true)
        } else {
            (anchor, cursor, false)
        };

        if let Some(members) = members.get_mut(&gid) {
            if members.len() == 0 {
                // There's no need to touch anything.
                return Ok(None);
            }

            // Begin by sorting and merging member selections.
            let mut unmerged = std::mem::take(members)
                .into_iter()
                .map(|id| {
                    let cursor = cursors.get(id).expect("bad cursor");
                    let anchor = anchors.get(id).unwrap_or_else(|| cursor.clone());

                    if cursor < anchor {
                        (id, cursor, anchor, true)
                    } else {
                        (id, anchor, cursor, false)
                    }
                })
                .collect::<Vec<_>>();

            unmerged.sort_by(|a, b| a.1.cmp(&b.1));

            let mut iter = unmerged.into_iter();
            let mut merged = Vec::new();

            let (mut aid, mut asels, mut asele, mut abef) = iter.next().unwrap();

            for (bid, bsels, bsele, bbef) in iter {
                if bsels <= asele {
                    // Merge into the previous selection.
                    asels = asels.min(bsels);
                    asele = asele.max(bsele);

                    // Remove this cursor.
                    vshapes.remove(&bid);
                    anchors.del(bid);
                    cursors.del(bid);
                } else {
                    // Save the previous selection, and begin merging into this one.
                    merged.push((aid, asels, asele, abef));

                    aid = bid;
                    asels = bsels;
                    asele = bsele;
                    abef = bbef;
                }
            }

            // Commit last member.
            merged.push((aid, asels, asele, abef));

            // Now we can merge them into the current leader.
            for (id, msels, msele, mbef) in merged.into_iter() {
                let lcontains = lsels <= msels && msels <= lsele;
                let mcontains = msels <= lsels && lsels <= msele;

                if lcontains || mcontains {
                    // Merge this selection into the leader.
                    lsels = lsels.min(msels);
                    lsele = lsele.max(msele);

                    // Remove the cursor.
                    vshapes.remove(&id);
                    anchors.del(id);
                    cursors.del(id);
                } else {
                    // Keep this selection.
                    members.push(id);

                    if mbef {
                        cursors.put(id, msels);
                        anchors.put(id, msele);
                    } else {
                        cursors.put(id, msele);
                        anchors.put(id, msels);
                    }
                }
            }

            if before {
                cursors.put(leader, lsels);
                anchors.put(leader, lsele);
            } else {
                cursors.put(leader, lsele);
                anchors.put(leader, lsels);
            }
        }

        Ok(None)
    }
}

impl<'a, 'b, C, P> SelectionActions<CursorGroupIdContext<'a, 'b, C>> for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn selection_cursor_set(
        &mut self,
        side: &SelectionCursorChange,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        for id in self.get_group(ctx.0) {
            let mut anchor = self.anchors.get(id).ok_or(EditError::NoSelection)?;
            let mut cursor = self.cursors.get(id).ok_or(EditError::InvalidCursor)?;
            let shape = self.vshapes.get(&id).ok_or(EditError::NoSelection)?;

            match (shape, side) {
                (
                    TargetShape::CharWise | TargetShape::LineWise,
                    SelectionCursorChange::SwapAnchor(_),
                ) => {
                    self.anchors.put(id, cursor);
                    self.cursors.put(id, anchor);
                },
                (TargetShape::BlockWise, SelectionCursorChange::SwapAnchor(false)) => {
                    self.anchors.put(id, cursor);
                    self.cursors.put(id, anchor);
                },
                (TargetShape::BlockWise, SelectionCursorChange::SwapAnchor(true)) => {
                    let cctx = (&self.text, ctx.1.get_width(), true);
                    let cx = cursor.x;
                    let ax = anchor.x;

                    anchor.set_column(cx, &cctx);
                    cursor.set_column(ax, &cctx);

                    self.anchors.put(id, anchor);
                    self.cursors.put(id, cursor);
                },
                (TargetShape::CharWise, SelectionCursorChange::Beginning) => {
                    let (begin, end) = sort2(anchor, cursor);

                    self.anchors.put(id, end);
                    self.cursors.put(id, begin);
                },
                (TargetShape::CharWise, SelectionCursorChange::End) => {
                    let (begin, end) = sort2(anchor, cursor);

                    self.anchors.put(id, begin);
                    self.cursors.put(id, end);
                },
                (TargetShape::LineWise, SelectionCursorChange::Beginning) => {
                    let (mut begin, mut end) = sort2(anchor, cursor);
                    let maxcol = self.text.max_column_idx(end.y, false);

                    begin.set_x(0);
                    end.set_x(maxcol);

                    self.anchors.put(id, end);
                    self.cursors.put(id, begin);
                },
                (TargetShape::LineWise, SelectionCursorChange::End) => {
                    let (mut begin, mut end) = sort2(anchor, cursor);
                    let maxcol = self.text.max_column_idx(end.y, false);

                    begin.set_x(0);
                    end.set_x(maxcol);

                    self.anchors.put(id, begin);
                    self.cursors.put(id, end);
                },
                (TargetShape::BlockWise, SelectionCursorChange::Beginning) => {
                    let (bx, ex) = sort2(anchor.x, cursor.x);
                    let (by, ey) = sort2(anchor.y, cursor.y);

                    let begin = Cursor::new(by, bx);
                    let end = Cursor::new(ey, ex);

                    self.anchors.put(id, end);
                    self.cursors.put(id, begin);
                },
                (TargetShape::BlockWise, SelectionCursorChange::End) => {
                    let (bx, ex) = sort2(anchor.x, cursor.x);
                    let (by, ey) = sort2(anchor.y, cursor.y);

                    let begin = Cursor::new(by, bx);
                    let end = Cursor::new(ey, ex);

                    self.anchors.put(id, begin);
                    self.cursors.put(id, end);
                },
            }
        }

        Ok(None)
    }

    fn selection_duplicate(
        &mut self,
        dir: MoveDir1D,
        count: Count,
        ictx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let gid = ictx.0;
        let count = ictx.2.resolve(&count);
        let lines = self.text.get_lines();
        let lastcol = ictx.2.get_last_column();

        for (start, end, shape) in self.get_group_selections(gid).unwrap_or_else(Vec::new) {
            let check = |ebuf: &EditBuffer<C, P>, lstart, lend| {
                let smax = ebuf.text.max_column_idx(lstart, lastcol);
                let emax = ebuf.text.max_column_idx(lend, lastcol);

                start.x <= smax && end.x <= emax
            };

            let copy = |ebuf: &mut EditBuffer<C, P>, lstart, lend| {
                let sels = Cursor::new(lstart, start.x);
                let sele = Cursor::new(lend, end.x);

                let cursor_id = ebuf.create_cursor_from(gid, &sele);
                ebuf.anchors.put(cursor_id, sels);
                ebuf.vshapes.insert(cursor_id, shape);
            };

            match dir {
                MoveDir1D::Next => {
                    let mut lstart = end.y + 1;
                    let mut create = count;
                    let ldiff = end.y - start.y;

                    while lstart < lines && create > 0 {
                        let lend = lstart + ldiff;

                        if lend >= lines {
                            break;
                        }

                        if check(self, lstart, lend) {
                            copy(self, lstart, lend);
                            lstart = lend + 1;
                            create = create - 1;
                            continue;
                        }

                        lstart += 1;
                    }
                },
                MoveDir1D::Previous => {
                    let ldiff = end.y - start.y;
                    let mut mstart = start.y.checked_sub(ldiff + 1);
                    let mut create = count;

                    while let Some(lstart) = mstart {
                        let lend = lstart + ldiff;

                        if create == 0 {
                            break;
                        }

                        if check(self, lstart, lend) {
                            copy(self, lstart, lend);
                            mstart = lstart.checked_sub(ldiff + 1);
                            create = create - 1;
                            continue;
                        }

                        mstart = lstart.checked_sub(1);
                    }
                },
            }
        }

        self._merge_selections(gid)
    }

    fn selection_resize(
        &mut self,
        style: SelectionResizeStyle,
        target: &EditTarget,
        ictx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let ctx = &self._ctx_cgi2es(&EditAction::Motion, ictx);
        let gid = ictx.0;
        let shape = ctx.context.get_target_shape();

        let (reset, obj) = match style {
            SelectionResizeStyle::Extend => (false, false),
            SelectionResizeStyle::Object => (false, true),
            SelectionResizeStyle::Restart => (true, false),
        };

        for (id, cursor) in self.get_group_cursors(gid) {
            if reset || !self.anchors.contains(id) {
                self.anchors.put(id, cursor.clone());
            }

            let tshape = match target {
                EditTarget::Boundary(range, inclusive, term, count) => {
                    if let Some(r) = self.text.range(&cursor, range, *inclusive, count, ctx) {
                        let nc = match term {
                            MoveTerminus::Beginning => r.start,
                            MoveTerminus::End => r.end,
                        };

                        self.set_cursor(id, nc);

                        r.shape
                    } else {
                        TargetShape::CharWise
                    }
                },
                EditTarget::CurrentPosition | EditTarget::Selection => {
                    // Do nothing to the cursor.
                    TargetShape::CharWise
                },
                EditTarget::CharJump(mark) => {
                    let nc = self._charjump(mark, &ctx)?;
                    self.set_cursor(id, nc);

                    TargetShape::CharWise
                },
                EditTarget::LineJump(mark) => {
                    let nc = self._linejump(mark, &ctx)?;
                    self.set_cursor(id, nc);

                    TargetShape::LineWise
                },
                EditTarget::Motion(mv, count) => {
                    if let Some(nc) = self.text.movement(&cursor, mv, count, &ctx) {
                        self.set_cursor(id, nc);
                    }

                    mv.shape()
                },
                EditTarget::Range(range, inclusive, count) => {
                    if let Some(r) = self.text.range(&cursor, range, *inclusive, count, &ctx) {
                        if obj {
                            self.anchors.put(id, r.start);
                            self.cursors.put(id, r.end);
                        } else {
                            let anchor = self.anchors.get(id).unwrap_or(cursor.clone());
                            let (start, end) = sort2(cursor, anchor);
                            let start = r.start.min(start);
                            let end = r.end.max(end);

                            self.anchors.put(id, start);
                            self.cursors.put(id, end);
                        }

                        r.shape
                    } else {
                        TargetShape::CharWise
                    }
                },
                EditTarget::Search(search, flip, count) => {
                    if let Some(r) = self._search(&cursor, search, flip, count, ctx.context)? {
                        if obj {
                            self.anchors.put(id, r.start);
                            self.cursors.put(id, r.end);
                        } else {
                            self.set_cursor(id, r.start);
                        }

                        r.shape
                    } else {
                        TargetShape::CharWise
                    }
                },
            };

            self.vshapes.insert(id, shape.unwrap_or(tshape));
        }

        self._merge_selections(gid)
    }

    fn selection_split(
        &mut self,
        style: SelectionSplitStyle,
        filter: TargetShapeFilter,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let gid = ctx.0;

        for id in self.get_group(gid) {
            let anchor = self.anchors.get(id).ok_or(EditError::NoSelection)?;
            let cursor = self.cursors.get(id).ok_or(EditError::InvalidCursor)?;
            let shape = *self.vshapes.get(&id).ok_or(EditError::NoSelection)?;

            if !filter.matches(&shape) {
                continue;
            }

            match (style, shape) {
                (SelectionSplitStyle::Anchor, _) => {
                    if anchor.y == cursor.y && anchor.x == cursor.x {
                        // Anchor and cursor are already the same.
                        continue;
                    }

                    // Update this selection's anchor to be at the cursor position.
                    self.anchors.put(id, cursor.clone());

                    // Create new selection from old anchor.
                    let cursor_id = self.create_cursor_from(gid, &anchor);
                    self.anchors.put(cursor_id, anchor.clone());
                    self.vshapes.insert(cursor_id, shape);
                },
                (SelectionSplitStyle::Lines, TargetShape::CharWise) => {
                    let (start, end) = sort2(anchor.clone(), cursor.clone());

                    for line in start.y..=end.y {
                        let lc = if line == start.y {
                            Cursor::new(line, start.x)
                        } else {
                            Cursor::new(line, 0)
                        };

                        let rc = if line == end.y {
                            Cursor::new(line, end.x)
                        } else {
                            Cursor::new(line, self.text.get_columns(line).saturating_sub(1))
                        };

                        if line == start.y {
                            self.cursors.put(id, lc.clone());
                            self.anchors.put(id, rc.clone());
                        } else {
                            let cursor_id = self.create_cursor_from(gid, &lc);
                            self.anchors.put(cursor_id, rc.clone());
                            self.vshapes.insert(cursor_id, shape);
                        }
                    }
                },
                (SelectionSplitStyle::Lines, TargetShape::LineWise) => {
                    let (start, end) = sort2(anchor.clone(), cursor.clone());

                    for line in start.y..=end.y {
                        let maxidx = self.text.get_columns(line).saturating_sub(1);
                        let lc = Cursor::new(line, 0);
                        let rc = Cursor::new(line, maxidx);

                        if line == start.y {
                            self.cursors.put(id, lc.clone());
                            self.anchors.put(id, rc.clone());
                        } else {
                            let cursor_id = self.create_cursor_from(gid, &lc);
                            self.anchors.put(cursor_id, rc.clone());
                            self.vshapes.insert(cursor_id, shape);
                        }
                    }
                },
                (SelectionSplitStyle::Lines, TargetShape::BlockWise) => {
                    // Determine the left and right borders of the block.
                    let (mut lc, mut rc) = block_cursors(&anchor, &cursor);

                    // Sort the cursors.
                    let (start, end) = sort2(anchor, cursor);

                    for line in start.y..=end.y {
                        let lctx = &(&self.text, 0, true);
                        let rctx = &(&self.text, 0, false);

                        lc.set_line(line, lctx);
                        rc.set_line(line, rctx);

                        if line == start.y {
                            self.cursors.put(id, lc.clone());
                            self.anchors.put(id, rc.clone());
                        } else {
                            let cursor_id = self.create_cursor_from(gid, &lc);
                            self.anchors.put(cursor_id, rc.clone());
                            self.vshapes.insert(cursor_id, shape);
                        }
                    }
                },
            }
        }

        Ok(None)
    }

    fn selection_trim(
        &mut self,
        filter: TargetShapeFilter,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let gid = ctx.0;
        let lastcol = ctx.2.get_last_column();
        let mut del = Vec::new();

        for (id, mut cursor) in self.get_group_cursors(gid) {
            let mut anchor = self.anchors.get(id).unwrap_or_else(|| cursor.clone());
            let shape = *self.vshapes.get(&id).unwrap_or(&TargetShape::CharWise);

            if !filter.matches(&shape) {
                continue;
            }

            match shape {
                TargetShape::CharWise => {
                    let (mut sels, mut sele, before) = if cursor < anchor {
                        (cursor, anchor, true)
                    } else {
                        (anchor, cursor, false)
                    };

                    let mut offs = self.text.cursor_to_offset(&sels);
                    let mut offe = self.text.cursor_to_offset(&sele);

                    let mut iter = self.text.chars(offs);

                    while let Some(c) = iter.next() {
                        let pos = iter.pos();

                        if pos > offe {
                            offs = pos;
                            break;
                        }

                        if c.is_ascii_whitespace() {
                            continue;
                        }

                        offs = pos;
                        break;
                    }

                    if offs > offe {
                        del.push(id);
                        continue;
                    }

                    let mut iter = self.text.chars_until(offs, offe);

                    while let Some(c) = iter.next_back() {
                        if c.is_ascii_whitespace() {
                            continue;
                        }

                        offe = iter.pos_back();
                        break;
                    }

                    sels = self.text.offset_to_cursor(offs);
                    sele = self.text.offset_to_cursor(offe);

                    if before {
                        self.cursors.put(id, sels);
                        self.anchors.put(id, sele);
                    } else {
                        self.cursors.put(id, sele);
                        self.anchors.put(id, sels);
                    }
                },
                TargetShape::LineWise => {
                    let (mut lstart, mut lend) = sort2(cursor.y, anchor.y);

                    while lstart <= lend && self.text.is_blank_line(lstart) {
                        lstart += 1;
                    }

                    if lstart > lend {
                        del.push(id);
                        continue;
                    }

                    while lstart <= lend && self.text.is_blank_line(lend) {
                        lend -= 1;
                    }

                    let cctx = &(&self.text, 0, lastcol);

                    if cursor < anchor {
                        cursor.set_line(lstart, cctx);
                        anchor.set_line(lend, cctx);
                    } else {
                        anchor.set_line(lstart, cctx);
                        cursor.set_line(lend, cctx);
                    };

                    self.cursors.put(id, cursor);
                    self.anchors.put(id, anchor);
                },
                TargetShape::BlockWise => {
                    // Determine the left and right borders of the block.
                    let (mut lc, mut rc) = block_cursors(&anchor, &cursor);

                    let lctx = &(&self.text, 0, true);
                    let rctx = &(&self.text, 0, false);

                    let (mut lstart, mut lend) = sort2(lc.y, rc.y);

                    // Remove top rows containing whitespace.
                    while lstart <= lend {
                        lc.set_line(lstart, lctx);
                        rc.set_line(lstart, rctx);

                        let loff = self.text.cursor_to_offset(&lc);
                        let roff = self.text.cursor_to_offset(&rc);

                        if self.text.is_blank_range(loff, roff) {
                            lstart += 1;
                        } else {
                            break;
                        }
                    }

                    if lstart > lend {
                        del.push(id);
                        continue;
                    }

                    // Remove bottom rows containing whitespace.
                    while lstart <= lend {
                        lc.set_line(lend, lctx);
                        rc.set_line(lend, rctx);

                        let loff = self.text.cursor_to_offset(&lc);
                        let roff = self.text.cursor_to_offset(&rc);

                        if self.text.is_blank_range(loff, roff) {
                            lend -= 1;
                        } else {
                            break;
                        }
                    }

                    // Clamp x and xgoal of the right cursor based on new line range.
                    rc.x = 0;

                    for line in lstart..=lend {
                        let max = self.text.max_column_idx(line, false);
                        let col = rc.xgoal.min(max);

                        rc.x = rc.x.max(col);
                    }

                    rc.xgoal = rc.x;

                    // Remove blank left columns.
                    while lc.x < rc.x {
                        let mut blank = true;

                        for line in lstart..=lend {
                            let cursor = Cursor::new(line, lc.x);

                            if let Some(c) = self.text.get_char_at_cursor(&cursor) {
                                if !c.is_ascii_whitespace() {
                                    blank = false;
                                    break;
                                }
                            }
                        }

                        if blank {
                            lc.x += 1;
                        } else {
                            break;
                        }
                    }

                    lc.xgoal = lc.x;

                    // Remove blank right columns.
                    while lc.x < rc.x {
                        let mut blank = true;

                        for line in lstart..=lend {
                            let cursor = Cursor::new(line, rc.x);

                            if let Some(c) = self.text.get_char_at_cursor(&cursor) {
                                if !c.is_ascii_whitespace() {
                                    blank = false;
                                    break;
                                }
                            }
                        }

                        if blank {
                            rc.x -= 1;
                        } else {
                            break;
                        }
                    }

                    rc.xgoal = rc.x;

                    // Place the right cursor on the line with the most columns, so that we
                    // preserve the largest block possible.
                    let scols = self.get_columns(lstart);
                    let ecols = self.get_columns(lend);

                    match scols.cmp(&ecols) {
                        Ordering::Less => {
                            lc.set_line(lstart, lctx);
                            rc.set_line(lend, rctx);

                            self.anchors.put(id, lc);
                            self.cursors.put(id, rc);
                        },
                        Ordering::Equal => {
                            lc.set_line(lstart, lctx);
                            rc.set_line(lend, rctx);

                            self.anchors.put(id, lc);
                            self.cursors.put(id, rc);
                        },
                        Ordering::Greater => {
                            lc.set_line(lend, lctx);
                            rc.set_line(lstart, rctx);

                            self.anchors.put(id, lc);
                            self.cursors.put(id, rc);
                        },
                    }
                },
            }
        }

        self.delete_cursors(gid, del);

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    use crate::editing::base::CursorCloseTarget;

    macro_rules! selection_cursor_set {
        ($ebuf: expr, $change: expr, $ctx: expr) => {
            $ebuf.selection_cursor_set($change, $ctx).unwrap()
        };
    }

    macro_rules! selection_split_lines {
        ($ebuf: expr, $filter: expr, $ctx: expr) => {
            selection_split!($ebuf, SelectionSplitStyle::Lines, $filter, $ctx)
        };
    }

    macro_rules! selection_extend {
        ($ebuf: expr, $et: expr, $ctx: expr) => {
            $ebuf.selection_resize(SelectionResizeStyle::Extend, &$et, $ctx).unwrap()
        };
    }

    macro_rules! selection_restart {
        ($ebuf: expr, $et: expr, $ctx: expr) => {
            $ebuf.selection_resize(SelectionResizeStyle::Restart, &$et, $ctx).unwrap()
        };
    }

    macro_rules! selection_trim {
        ($ebuf: expr, $filter: expr, $ctx: expr) => {
            $ebuf.selection_trim($filter, $ctx).unwrap()
        };
    }

    macro_rules! selection_duplicate {
        ($ebuf: expr, $dir: expr, $count: expr, $ctx: expr) => {
            $ebuf.selection_duplicate($dir, $count, $ctx).unwrap()
        };
    }

    macro_rules! selection_close {
        ($ebuf: expr, $target: expr, $ctx: expr) => {
            $ebuf.cursor_close($target, $ctx).unwrap()
        };
    }

    macro_rules! selection_rotate {
        ($ebuf: expr, $dir: expr, $count: expr, $ctx: expr) => {
            $ebuf.cursor_rotate($dir, $count, $ctx).unwrap()
        };
    }

    macro_rules! selection_split {
        ($ebuf: expr, $style: expr, $filter: expr, $ctx: expr) => {
            $ebuf.selection_split($style, $filter, $ctx).unwrap()
        };
    }

    #[test]
    fn test_selection_split_lines_blockwise() {
        let mut ebuf = mkbufstr("a b c d\ne f g\nh i j k l\nm n o p\nq r\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (2, 6).
        ebuf.set_leader(curid, Cursor::new(2, 6));

        vctx.persist.shape = Some(TargetShape::BlockWise);

        // Create a charwise selection across the three lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(2, 6), Cursor::new(4, 0), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));

        // Filter doesn't match, nothing happens.
        selection_split_lines!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));

        // Filter matches, splits into multiple CharWise selections.
        selection_split_lines!(ebuf, TargetShapeFilter::BLOCK, ctx!(curid, vwctx, vctx));

        let selection1 = (Cursor::new(2, 0), Cursor::new(2, 6), TargetShape::BlockWise);
        let selection2 = (Cursor::new(3, 0), Cursor::new(3, 6), TargetShape::BlockWise);
        let selection3 = (Cursor::new(4, 0), Cursor::new(4, 2).goal(6), TargetShape::BlockWise);
        let selections = vec![selection2, selection3];

        assert_eq!(ebuf.get_leader_selection(curid), Some(selection1.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));
        assert_eq!(ebuf.get_follower_selections(curid), Some(selections));
        assert_eq!(ebuf.get_followers(curid), vec![Cursor::new(3, 0), Cursor::new(4, 0)]);
    }

    #[test]
    fn test_selection_split_lines_charwise() {
        let mut ebuf = mkbufstr("a b c d\ne f g\nh i j k l\nm n o p\nq r s t\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (1, 2).
        ebuf.set_leader(curid, Cursor::new(1, 2));

        vctx.persist.shape = Some(TargetShape::CharWise);

        // Create a charwise selection across the three lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(1, 2), Cursor::new(3, 0), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter doesn't match, nothing happens.
        selection_split_lines!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter matches, splits into multiple CharWise selections.
        selection_split_lines!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx));

        let selection1 = (Cursor::new(1, 2), Cursor::new(1, 4), TargetShape::CharWise);
        let selection2 = (Cursor::new(2, 0), Cursor::new(2, 8), TargetShape::CharWise);
        let selection3 = (Cursor::new(3, 0), Cursor::new(3, 0), TargetShape::CharWise);
        let selections = vec![selection2, selection3];

        assert_eq!(ebuf.get_leader_selection(curid), Some(selection1.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));
        assert_eq!(ebuf.get_follower_selections(curid), Some(selections));
        assert_eq!(ebuf.get_followers(curid), vec![Cursor::new(2, 0), Cursor::new(3, 0)]);
    }

    #[test]
    fn test_selection_split_lines_linewise() {
        let mut ebuf = mkbufstr("a b c d\ne f g\nh i j k l\nm n o p\nq r s t\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (1, 0).
        ebuf.set_leader(curid, Cursor::new(1, 0));

        vctx.persist.shape = Some(TargetShape::LineWise);

        // Create a linewise selection across three lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(1, 0), Cursor::new(3, 0), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter doesn't match, nothing happens.
        selection_split_lines!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter matches, splits into multiple LineWise selections.
        selection_split_lines!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx));

        let selection1 = (Cursor::new(1, 0), Cursor::new(1, 4), TargetShape::LineWise);
        let selection2 = (Cursor::new(2, 0), Cursor::new(2, 8), TargetShape::LineWise);
        let selection3 = (Cursor::new(3, 0), Cursor::new(3, 6), TargetShape::LineWise);
        let selections = vec![selection2, selection3];

        assert_eq!(ebuf.get_leader_selection(curid), Some(selection1.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
        assert_eq!(ebuf.get_follower_selections(curid), Some(selections));
        assert_eq!(ebuf.get_followers(curid), vec![Cursor::new(2, 0), Cursor::new(3, 0)]);
    }

    #[test]
    fn test_selection_cursor_set_charwise() {
        let mut ebuf = mkbufstr("hello world\na b c d\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 6).
        ebuf.set_leader(curid, Cursor::new(0, 6));

        vctx.persist.shape = Some(TargetShape::CharWise);

        // Create a selection to resize from here to next word beginning.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(0, 6), Cursor::new(1, 0), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // We're already at the end so this shouldn't move.
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // Now swap which side is the anchor, and which side is mobile.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(false),
            ctx!(curid, vwctx, vctx)
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // We're already at the beginning so this shouldn't move.
        selection_cursor_set!(ebuf, &SelectionCursorChange::Beginning, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Move to end.
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_selection_cursor_set_linewise() {
        let mut ebuf = mkbufstr("foo\nhello world\na b c d\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (1, 6).
        ebuf.set_leader(curid, Cursor::new(1, 6));

        vctx.persist.shape = Some(TargetShape::LineWise);

        // Create a linewise selection across the two lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(1, 6), Cursor::new(2, 0), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));

        // Now swap which side is the anchor, and which side is mobile.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(false),
            ctx!(curid, vwctx, vctx)
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));

        // Moving to the end of the selection places cursor in last column of last line.
        let selection = (Cursor::new(1, 0), Cursor::new(2, 6), TargetShape::LineWise);
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 6));

        // Moving to the beginning of the selection places cursor in first column of first line.
        selection_cursor_set!(ebuf, &SelectionCursorChange::Beginning, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_selection_cursor_set_blockwise() {
        let mut ebuf = mkbufstr("foo\nhello world\na b c d\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (1, 2).
        ebuf.set_leader(curid, Cursor::new(1, 2));

        vctx.persist.shape = Some(TargetShape::BlockWise);

        // Create a block selection across two lines.
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 21), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(1, 2), Cursor::new(2, 4), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 4));

        // Now swap which side is the anchor, and which side is mobile.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(false),
            ctx!(curid, vwctx, vctx)
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));

        // Now swap over to the upper-right corner.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(true),
            ctx!(curid, vwctx, vctx)
        );

        let selection = (Cursor::new(1, 4), Cursor::new(2, 2), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 4));

        // Now swap to the first column of the selection.
        selection_cursor_set!(ebuf, &SelectionCursorChange::Beginning, ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(1, 2), Cursor::new(2, 4), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));

        // Now swap to the last column of the selection.
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx));

        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 4));
    }

    #[test]
    fn test_selection_restart() {
        let mut ebuf = mkbufstr("hello world a b c d\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.persist.shape = Some(TargetShape::CharWise);

        // Create a selection to resize from here to next word beginning.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(0, 0), Cursor::new(0, 6), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Now resize the selection to just be the second word.
        let mov = MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next);
        selection_restart!(ebuf, mv!(mov), ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(0, 6), Cursor::new(0, 10), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));

        // Now resize the selection to just be the cursor position.
        selection_restart!(ebuf, EditTarget::CurrentPosition, ctx!(curid, vwctx, vctx));

        let selection = (Cursor::new(0, 10), Cursor::new(0, 10), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));
    }

    #[test]
    fn test_selection_resize_range_grows() {
        let mut ebuf = mkbufstr("hello world\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();
        let cw = TargetShape::CharWise;

        // Start out at (0, 2), over "l".
        ebuf.set_leader(curid, Cursor::new(0, 2));

        vctx.persist.shape = Some(cw);

        // Create selection over "ll".
        let right = MoveType::Column(MoveDir1D::Next, false).into();
        let lsel = (Cursor::new(0, 2), Cursor::new(0, 3), cw);
        selection_extend!(ebuf, right, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));

        // Doing a word range should also move anchor.
        let range = RangeType::Word(WordStyle::Little).into();
        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), cw);
        selection_extend!(ebuf, range, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
    }

    #[test]
    fn test_selection_duplicate_and_rotate() {
        let mut ebuf = mkbufstr(
            "copy lines\nabc\ncopy lines\n\nstart line\n\nabc\ncopy lines\nabc\ncopy lines\n",
        );
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (4, 2), on the "a" in "start line".
        ebuf.set_leader(curid, Cursor::new(4, 2));

        vctx.persist.shape = Some(TargetShape::CharWise);

        // Create a selection from "a" to "i".
        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, EditAction::Motion, mv!(mov, 5), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(4, 2), Cursor::new(4, 7), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 7));

        // Now duplicate the selection twice, onto the next available lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 2.into(), ctx!(curid, vwctx, vctx));

        let fsels = vec![
            (Cursor::new(7, 2), Cursor::new(7, 7), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 7), TargetShape::CharWise),
        ];

        // Leader stays the same, and we have new selections on the lines containing "copy line".
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 7));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Duplicating again doesn't result in overlapping selections.
        selection_duplicate!(ebuf, MoveDir1D::Next, 2.into(), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 7));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Move back to word end so we have different end columns.
        let mov = MoveType::WordEnd(WordStyle::Little, MoveDir1D::Previous);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(4, 2), Cursor::new(4, 4), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(7, 2), Cursor::new(7, 3), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 3), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Duplicate backwards once.
        selection_duplicate!(ebuf, MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx));

        let fsels = vec![
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 3), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 3), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Duplicate 4 times backwards, getting one more selection.
        selection_duplicate!(ebuf, MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx));

        let fsels = vec![
            (Cursor::new(0, 2), Cursor::new(0, 4), TargetShape::CharWise),
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 3), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 3), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Duplicate once forward, extending the second to last selection.
        selection_duplicate!(ebuf, MoveDir1D::Next, 1.into(), ctx!(curid, vwctx, vctx));
        let fsels = vec![
            (Cursor::new(0, 2), Cursor::new(0, 4), TargetShape::CharWise),
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 4), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 3), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Again.
        selection_duplicate!(ebuf, MoveDir1D::Next, 1.into(), ctx!(curid, vwctx, vctx));
        let fsels = vec![
            (Cursor::new(0, 2), Cursor::new(0, 4), TargetShape::CharWise),
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 4), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Rotate the leader three times forwards, wrapping around.
        selection_rotate!(ebuf, MoveDir1D::Next, 3.into(), ctx!(curid, vwctx, vctx));
        let lsel = (Cursor::new(0, 2), Cursor::new(0, 4), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(4, 2), Cursor::new(4, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 4), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Rotate the leader once backwards, wrapping around again.
        selection_rotate!(ebuf, MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx));
        let lsel = (Cursor::new(9, 2), Cursor::new(9, 4), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(0, 2), Cursor::new(0, 4), TargetShape::CharWise),
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(4, 2), Cursor::new(4, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(9, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Rotate the leader twice backwards, no wrapping.
        selection_rotate!(ebuf, MoveDir1D::Previous, 2.into(), ctx!(curid, vwctx, vctx));
        let lsel = (Cursor::new(4, 2), Cursor::new(4, 4), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(0, 2), Cursor::new(0, 4), TargetShape::CharWise),
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 4), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));
    }

    #[test]
    fn test_selection_duplicate_and_close() {
        let mut ebuf = mkbufstr("  a  \n  b  \n  c  \n  d  \n  e  \n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.persist.shape = Some(TargetShape::CharWise);

        // Create a selection to the end of the line.
        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, EditAction::Motion, mv!(mov, 4), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Now duplicate the selection four times.
        selection_duplicate!(ebuf, MoveDir1D::Next, 4.into(), ctx!(curid, vwctx, vctx));

        let fsels = vec![
            (Cursor::new(1, 0), Cursor::new(1, 4), TargetShape::CharWise),
            (Cursor::new(2, 0), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Rotate the leader forward.
        selection_rotate!(ebuf, MoveDir1D::Next, 1.into(), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(1, 0), Cursor::new(1, 4), TargetShape::CharWise);

        let fsels = vec![
            (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise),
            (Cursor::new(2, 0), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Close leader.
        selection_close!(ebuf, &CursorCloseTarget::Leader, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise);

        let fsels = vec![
            (Cursor::new(2, 0), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Close followers.
        selection_close!(ebuf, &CursorCloseTarget::Followers, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise);

        let fsels = vec![];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));
    }

    #[test]
    fn test_selection_trim_charwise() {
        let mut ebuf = mkbufstr("a   \t   b   \n   c   \n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 1), on the space after the "a".
        ebuf.set_leader(curid, Cursor::new(0, 1));

        vctx.persist.shape = Some(TargetShape::CharWise);

        // Create a selection from the space after "a" to the final column.
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 20), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 1), Cursor::new(1, 6), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));

        // Trim with TargetShapeFilter::LINE does nothing.
        selection_trim!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));

        // Now trim down to the "b" and the "c" with TargetShapeFilter::CHAR.
        selection_trim!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 8), Cursor::new(1, 3), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Move back 1 from the "c".
        let mov = MoveType::Column(MoveDir1D::Previous, false);
        edit!(ebuf, EditAction::Motion, mv!(mov, 1), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 8), Cursor::new(1, 2), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));

        // Trim down to just the "b".
        selection_trim!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 8), Cursor::new(0, 8), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 8));
    }

    #[test]
    fn test_selection_trim_linewise() {
        let mut ebuf = mkbufstr("a\n   \n\t\nb   \n   c   \n\n\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (1, 0), a line of all spaces.
        ebuf.set_leader(curid, Cursor::new(1, 0));

        vctx.persist.shape = Some(TargetShape::LineWise);

        // Create a selection down to line 6.
        let mov = MoveType::Line(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 5), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(1, 0), Cursor::new(6, 0), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(6, 0));

        // Trim with TargetShapeFilter::CHAR does nothing.
        selection_trim!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(6, 0));

        // Now trim down to the "b" and "c" lines with TargetShapeFilter::LINE.
        selection_trim!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(3, 0), Cursor::new(4, 0), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));
    }

    #[test]
    fn test_selection_trim_blockwise() {
        let mut ebuf = mkbufstr("     \n     \n   a \n b   \n     \n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.persist.shape = Some(TargetShape::BlockWise);

        // Create a selection from the space after "a" to the final column.
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 29), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 0), Cursor::new(4, 4), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));

        // Trim with TargetShapeFilter::LINE does nothing.
        selection_trim!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));

        // Now trim down with TargetShapeFilter::BLOCK so that we have a block containing both "a"
        // and "b".
        selection_trim!(ebuf, TargetShapeFilter::BLOCK, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(2, 1), Cursor::new(3, 3), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 3));
    }

    #[test]
    fn test_selection_trim_blockwise_xgoal() {
        let mut ebuf = mkbufstr("     \n  a  \n        b   \n  c\n\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.persist.shape = Some(TargetShape::BlockWise);

        // Create a selection going down the first column.
        let mov = MoveType::Line(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 4), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 0), Cursor::new(4, 0), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));

        // Now move the xgoal.
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Motion, mv!(mov, 0), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 0), Cursor::new(4, 0).goal(usize::MAX), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0).goal(usize::MAX));

        // Trimming should:
        // - Not drop "b" itself.
        // - Remove space after "b"
        // - Place cursor on the longest line, for convenient manual resizing.
        selection_trim!(ebuf, TargetShapeFilter::BLOCK, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(1, 4).goal(8), Cursor::new(3, 2), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 4).goal(8));
    }

    #[test]
    fn test_selection_trim_drop_empty() {
        let mut ebuf = mkbufstr("  a  \n     \n  b  \n     \n  c  \n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 1), on the space after the "a".
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.persist.shape = Some(TargetShape::CharWise);

        // Select the whole first line.
        let mov = MoveType::Column(MoveDir1D::Next, true);
        edit!(ebuf, EditAction::Motion, mv!(mov, 4), ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Duplicate selection across the following lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 4.into(), ctx!(curid, vwctx, vctx));

        let fsels = vec![
            (Cursor::new(1, 0), Cursor::new(1, 4), TargetShape::CharWise),
            (Cursor::new(2, 0), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Now trim, dropping two selections, and shrinking 3.
        selection_trim!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx));

        let lsel = (Cursor::new(0, 2), Cursor::new(0, 2), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(2, 2), Cursor::new(2, 2), TargetShape::CharWise),
            (Cursor::new(4, 2), Cursor::new(4, 2), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));
    }
}
