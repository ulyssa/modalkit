use crate::{
    actions::EditAction,
    editing::{
        application::ApplicationInfo,
        context::Resolve,
        cursor::{block_cursors, Cursor},
        rope::PrivateCursorOps,
        store::Store,
    },
    errors::{EditError, EditResult},
    prelude::*,
    util::sort2,
};

use super::{CursorGroupIdContext, CursorState, EditBuffer};

pub trait SelectionActions<C, I>
where
    I: ApplicationInfo,
{
    /// Move where the cursor is located in a selection.
    fn selection_cursor_set(
        &mut self,
        side: &SelectionCursorChange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Duplicate existing selections onto the lines below them.
    fn selection_duplicate(
        &mut self,
        dir: MoveDir1D,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Expand selection to the specified boundaries if they're not already there.
    fn selection_expand(
        &mut self,
        boundary: &SelectionBoundary,
        filter: TargetShapeFilter,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Filter selections that match the regular expression in [Register::LastSearch].
    fn selection_filter(
        &mut self,
        drop: bool,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Join adjacent selections.
    fn selection_join(&mut self, ctx: &C, store: &mut Store<I>) -> EditResult<EditInfo, I>;

    /// Change the boundaries of the selection to be exactly those of the range.
    fn selection_resize(
        &mut self,
        style: &SelectionResizeStyle,
        target: &EditTarget,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Split a multiline selection into multiple single-line selections.
    fn selection_split(
        &mut self,
        style: &SelectionSplitStyle,
        filter: TargetShapeFilter,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Remove whitespace from the ends of matching selections.
    fn selection_trim(
        &mut self,
        boundary: &SelectionBoundary,
        filter: TargetShapeFilter,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;
}

impl<'a, I> SelectionActions<CursorGroupIdContext<'a>, I> for EditBuffer<I>
where
    I: ApplicationInfo,
{
    fn selection_cursor_set(
        &mut self,
        side: &SelectionCursorChange,
        ctx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ctx.0;
        let mut group = self.get_group(gid);

        for state in group.iter_mut() {
            let mut anchor = state.anchor().clone();
            let mut cursor = state.cursor().clone();
            let shape = state.shape();

            match (shape, side) {
                (
                    TargetShape::CharWise | TargetShape::LineWise,
                    SelectionCursorChange::SwapAnchor(_),
                ) => {
                    state.swap();
                },
                (TargetShape::BlockWise, SelectionCursorChange::SwapAnchor(false)) => {
                    state.swap();
                },
                (TargetShape::BlockWise, SelectionCursorChange::SwapAnchor(true)) => {
                    let cctx = (&self.text, ctx.1.get_width(), true);
                    let cx = cursor.x;
                    let ax = anchor.x;

                    anchor.set_column(cx, &cctx);
                    cursor.set_column(ax, &cctx);

                    *state = CursorState::Selection(cursor, anchor, shape);
                },
                (TargetShape::CharWise, SelectionCursorChange::Beginning) => {
                    let (begin, end) = sort2(anchor, cursor);

                    *state = CursorState::Selection(begin, end, shape);
                },
                (TargetShape::CharWise, SelectionCursorChange::End) => {
                    let (begin, end) = sort2(anchor, cursor);

                    *state = CursorState::Selection(end, begin, shape);
                },
                (TargetShape::LineWise, SelectionCursorChange::Beginning) => {
                    let (mut begin, mut end) = sort2(anchor, cursor);
                    let maxcol = self.text.max_column_idx(end.y, false);

                    begin.set_x(0);
                    end.set_x(maxcol);

                    *state = CursorState::Selection(begin, end, shape);
                },
                (TargetShape::LineWise, SelectionCursorChange::End) => {
                    let (mut begin, mut end) = sort2(anchor, cursor);
                    let maxcol = self.text.max_column_idx(end.y, false);

                    begin.set_x(0);
                    end.set_x(maxcol);

                    *state = CursorState::Selection(end, begin, shape);
                },
                (TargetShape::BlockWise, SelectionCursorChange::Beginning) => {
                    let (bx, ex) = sort2(anchor.x, cursor.x);
                    let (by, ey) = sort2(anchor.y, cursor.y);

                    let begin = Cursor::new(by, bx);
                    let end = Cursor::new(ey, ex);

                    *state = CursorState::Selection(begin, end, shape);
                },
                (TargetShape::BlockWise, SelectionCursorChange::End) => {
                    let (bx, ex) = sort2(anchor.x, cursor.x);
                    let (by, ey) = sort2(anchor.y, cursor.y);

                    let begin = Cursor::new(by, bx);
                    let end = Cursor::new(ey, ex);

                    *state = CursorState::Selection(end, begin, shape);
                },
            }
        }

        self.set_group(gid, group);

        Ok(None)
    }

    fn selection_duplicate(
        &mut self,
        dir: MoveDir1D,
        count: &Count,
        ictx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ictx.0;
        let count = ictx.2.resolve(count);
        let lines = self.text.get_lines();

        let mut group = self.get_group(gid);
        let mut created = vec![];

        for state in group.iter_mut() {
            let (start, end) = state.sorted();
            let shape = state.shape();

            let check = |ebuf: &EditBuffer<I>, lstart, lend| {
                let smax = ebuf.text.max_column_idx(lstart, true);
                let emax = ebuf.text.max_column_idx(lend, true);

                start.x <= smax && end.x <= emax
            };

            let mut copy = |lstart, lend| {
                let sels = Cursor::new(lstart, start.x);
                let sele = Cursor::new(lend, end.x);

                created.push(CursorState::Selection(sele, sels, shape));
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
                            copy(lstart, lend);
                            lstart = lend + 1;
                            create -= 1;
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
                            copy(lstart, lend);
                            mstart = lstart.checked_sub(ldiff + 1);
                            create -= 1;
                            continue;
                        }

                        mstart = lstart.checked_sub(1);
                    }
                },
            }
        }

        group.members.append(&mut created);
        group.merge();
        self.set_group(gid, group);

        Ok(None)
    }

    fn selection_expand(
        &mut self,
        boundary: &SelectionBoundary,
        filter: TargetShapeFilter,
        ictx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        use MoveDir1D::{Next, Previous};
        use MoveTerminus::{Beginning, End};

        let ctx = &self._ctx_cgi2es(&EditAction::Motion, ictx);
        let gid = ictx.0;
        let shape = ctx.context.get_target_shape();
        let mut group = self.get_group(gid);

        // Save current positions before we expand.
        self.push_jump(gid, &group);

        for state in group.iter_mut() {
            if !filter.matches(&state.shape()) {
                continue;
            }

            if let Some(shape) = shape {
                state.set_shape(shape);
            }

            let sels = state.start();
            let sele = state.end();

            let sels = self.text.seek(sels, boundary, Beginning, Previous, 1, true, true);
            let sele = self.text.seek(sele, boundary, End, Next, 1, true, true);

            if let (Some(start), Some(end)) = (sels, sele) {
                state.set_anchor(start);
                state.set_cursor(end);
            }
        }

        group.merge();
        self.set_group(gid, group);

        Ok(None)
    }

    fn selection_filter(
        &mut self,
        drop: bool,
        ictx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ictx.0;
        let mut group = self.get_group(gid);

        let needle = self._get_regex(store)?;
        let members = std::mem::take(&mut group.members);

        let keep = |state: &CursorState| {
            let ms = self.text.find_matches(state.start(), state.end(), &needle);

            return ms.is_empty() == drop;
        };

        for member in members.into_iter() {
            if keep(&member) {
                group.members.push(member);
            }
        }

        if !keep(&group.leader) {
            if let Some(leader) = group.members.pop() {
                group.leader = leader;
            } else {
                let msg = "No selections remaining".to_string();
                let err = EditError::Failure(msg);

                return Err(err);
            }
        }

        self.set_group(gid, group);

        return Ok(None);
    }

    fn selection_join(
        &mut self,
        ictx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ictx.0;
        let mut group = self.get_group(gid);

        group.merge();

        if group.members.is_empty() {
            // There's nothing to join.
            return Ok(None);
        }

        // Save current positions before we join them together.
        self.push_jump(gid, &group);

        let unjoined = std::mem::take(&mut group.members);
        let mut iter = unjoined.into_iter();
        let mut joined = Vec::new();
        let mut a = iter.next().unwrap();

        for b in iter {
            let ao: usize = self.text.cursor_to_offset(a.end()).into();
            let bo: usize = self.text.cursor_to_offset(b.start()).into();

            if bo.saturating_sub(ao) == 1 {
                a = a.union(&b);
            } else {
                joined.push(a);
                a = b;
            }
        }

        joined.push(a);

        for member in joined.into_iter() {
            let lo: usize = self.text.cursor_to_offset(group.leader.end()).into();
            let mo: usize = self.text.cursor_to_offset(member.start()).into();

            if mo.saturating_sub(lo) == 1 {
                group.leader = group.leader.union(&member);
            } else {
                group.members.push(member);
            }
        }

        self.set_group(gid, group);

        Ok(None)
    }

    fn selection_resize(
        &mut self,
        style: &SelectionResizeStyle,
        target: &EditTarget,
        ictx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let ctx = &self._ctx_cgi2es(&EditAction::Motion, ictx);
        let gid = ictx.0;
        let shape = ctx.context.get_target_shape();

        let (reset, obj) = match style {
            SelectionResizeStyle::Extend => (false, false),
            SelectionResizeStyle::Object => (false, true),
            SelectionResizeStyle::Restart => (true, false),
        };

        let mut group = self.get_group(gid);

        if target.is_jumping() {
            // Save current positions before we jump.
            self.push_jump(gid, &group);
        }

        for state in group.iter_mut() {
            let cursor = state.cursor().clone();

            if reset || !state.is_selection() {
                state.set_anchor(cursor.clone());
            }

            let tshape = match target {
                EditTarget::Boundary(range, inclusive, term, count) => {
                    if let Some(r) = self.text.range(&cursor, range, *inclusive, count, ctx) {
                        let nc = match term {
                            MoveTerminus::Beginning => r.start,
                            MoveTerminus::End => r.end,
                        };

                        state.set_cursor(nc);

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
                    let nc = self._charjump(mark, ctx, store)?;
                    state.set_cursor(nc);

                    TargetShape::CharWise
                },
                EditTarget::LineJump(mark) => {
                    let nc = self._linejump(mark, ctx, store)?;
                    state.set_cursor(nc);

                    TargetShape::LineWise
                },
                EditTarget::Motion(mv, count) => {
                    if let Some(nc) = self.text.movement(&cursor, mv, count, ctx) {
                        state.set_cursor(nc);
                    }

                    mv.shape()
                },
                EditTarget::Range(range, inclusive, count) => {
                    if let Some(r) = self.text.range(&cursor, range, *inclusive, count, ctx) {
                        if obj {
                            state.set_anchor(r.start);
                            state.set_cursor(r.end);
                        } else {
                            let (start, end) = state.sorted();
                            let start = r.start.min(start);
                            let end = r.end.max(end);

                            state.set_anchor(start);
                            state.set_cursor(end);
                        }

                        r.shape
                    } else {
                        TargetShape::CharWise
                    }
                },
                EditTarget::Search(search, flip, count) => {
                    if let Some(r) =
                        self._search(&cursor, search, flip, count, ctx.context, store)?
                    {
                        if obj {
                            state.set_anchor(r.start);
                            state.set_cursor(r.end);
                        } else {
                            state.set_cursor(r.start);
                        }

                        r.shape
                    } else {
                        TargetShape::CharWise
                    }
                },
            };

            state.set_shape(shape.unwrap_or(tshape));
        }

        group.merge();
        self.set_group(gid, group);

        Ok(None)
    }

    fn selection_split(
        &mut self,
        style: &SelectionSplitStyle,
        filter: TargetShapeFilter,
        ctx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ctx.0;
        let mut group = self.get_group(gid);
        let mut created = vec![];

        for state in group.iter_mut() {
            let (cursor, anchor, shape) = state.to_triple();

            if !filter.matches(&shape) {
                continue;
            }

            match (style, shape) {
                (SelectionSplitStyle::Anchor, _) => {
                    if anchor.y == cursor.y && anchor.x == cursor.x {
                        // Anchor and cursor are already the same.
                        continue;
                    }

                    // Create new selection from old anchor.
                    created.push(CursorState::Selection(anchor.clone(), anchor.clone(), shape));

                    // Update this selection's anchor to be at the cursor position.
                    state.set_anchor(cursor.clone());
                },
                (SelectionSplitStyle::Lines, TargetShape::CharWise) => {
                    let (start, end) = state.sorted();

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
                            state.set_cursor(lc.clone());
                            state.set_anchor(rc.clone());
                        } else {
                            created.push(CursorState::Selection(lc.clone(), rc.clone(), shape));
                        }
                    }
                },
                (SelectionSplitStyle::Lines, TargetShape::LineWise) => {
                    let (start, end) = state.sorted();

                    for line in start.y..=end.y {
                        let maxidx = self.text.get_columns(line).saturating_sub(1);
                        let lc = Cursor::new(line, 0);
                        let rc = Cursor::new(line, maxidx);

                        if line == start.y {
                            state.set_cursor(lc.clone());
                            state.set_anchor(rc.clone());
                        } else {
                            created.push(CursorState::Selection(lc.clone(), rc.clone(), shape));
                        }
                    }
                },
                (SelectionSplitStyle::Lines, TargetShape::BlockWise) => {
                    // Determine the left and right borders of the block.
                    let (mut lc, mut rc) = block_cursors(&anchor, &cursor);

                    // Sort the cursors.
                    let (start, end) = state.sorted();

                    for line in start.y..=end.y {
                        let lctx = &(&self.text, 0, true);
                        let rctx = &(&self.text, 0, false);

                        lc.set_line(line, lctx);
                        rc.set_line(line, rctx);

                        if line == start.y {
                            state.set_cursor(lc.clone());
                            state.set_anchor(rc.clone());
                        } else {
                            created.push(CursorState::Selection(lc.clone(), rc.clone(), shape));
                        }
                    }
                },
            }
        }

        group.members.append(&mut created);
        group.merge();
        self.set_group(gid, group);

        Ok(None)
    }

    fn selection_trim(
        &mut self,
        boundary: &SelectionBoundary,
        filter: TargetShapeFilter,
        ctx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        use MoveDir1D::{Next, Previous};
        use MoveTerminus::{Beginning, End};

        let gid = ctx.0;
        let mut group = self.get_group(gid);

        let trim = |state: &CursorState| -> Option<CursorState> {
            if !filter.matches(&state.shape()) {
                return Some(state.clone());
            }

            let (cursor, anchor, shape) = state.to_triple();
            let shape = ctx.2.get_target_shape().unwrap_or(shape);

            let (sels, sele, before) = if cursor < anchor {
                (cursor, anchor, true)
            } else {
                (anchor, cursor, false)
            };

            let sels = self.text.seek(&sels, boundary, Beginning, Next, 1, true, true)?;
            let sele = self.text.seek(&sele, boundary, End, Previous, 1, true, true)?;

            if sels > sele {
                None
            } else if before {
                Some(CursorState::Selection(sels, sele, shape))
            } else {
                Some(CursorState::Selection(sele, sels, shape))
            }
        };

        group.members = group.members.iter().filter_map(trim).collect();

        if let Some(leader) = trim(&group.leader) {
            group.leader = leader;
        } else if let Some(leader) = group.members.pop() {
            group.leader = leader;
        } else {
            let msg = "No selections remaining".to_string();
            let err = EditError::Failure(msg);

            return Err(err);
        }

        self.set_group(gid, group);

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    macro_rules! selection_cursor_set {
        ($ebuf: expr, $change: expr, $ctx: expr, $store: expr) => {
            $ebuf.selection_cursor_set($change, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! selection_split_lines {
        ($ebuf: expr, $filter: expr, $ctx: expr, $store: expr) => {
            selection_split!($ebuf, SelectionSplitStyle::Lines, $filter, $ctx, $store)
        };
    }

    macro_rules! selection_extend {
        ($ebuf: expr, $et: expr, $ctx: expr, $store: expr) => {
            $ebuf
                .selection_resize(&SelectionResizeStyle::Extend, &$et, $ctx, &mut $store)
                .unwrap()
        };
    }

    macro_rules! selection_restart {
        ($ebuf: expr, $et: expr, $ctx: expr, $store: expr) => {
            $ebuf
                .selection_resize(&SelectionResizeStyle::Restart, &$et, $ctx, &mut $store)
                .unwrap()
        };
    }

    macro_rules! selection_expand {
        ($ebuf: expr, $boundary: expr, $filter: expr, $ctx: expr, $store: expr) => {
            $ebuf.selection_expand(&$boundary, $filter, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! selection_trim {
        ($ebuf: expr, $boundary: expr, $filter: expr, $ctx: expr, $store: expr) => {
            $ebuf.selection_trim(&$boundary, $filter, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! selection_duplicate {
        ($ebuf: expr, $dir: expr, $count: expr, $ctx: expr, $store: expr) => {
            $ebuf.selection_duplicate($dir, &$count, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! selection_close {
        ($ebuf: expr, $target: expr, $ctx: expr, $store: expr) => {
            $ebuf.cursor_close($target, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! selection_rotate {
        ($ebuf: expr, $dir: expr, $count: expr, $ctx: expr, $store: expr) => {
            $ebuf.cursor_rotate($dir, &$count, $ctx, &mut $store).unwrap()
        };
    }

    macro_rules! selection_join {
        ($ebuf: expr, $ctx: expr, $store: expr) => {
            $ebuf.selection_join($ctx, &mut $store).unwrap()
        };
    }

    macro_rules! selection_split {
        ($ebuf: expr, $style: expr, $filter: expr, $ctx: expr, $store: expr) => {
            $ebuf.selection_split(&$style, $filter, $ctx, &mut $store).unwrap()
        };
    }

    #[test]
    fn test_selection_join() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello\nhello\nhello\nhello\nhello\n");

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Extend to the newline.
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 6), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 5), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        // Duplicate selection across the following lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 4.into(), ctx!(curid, vwctx, vctx), store);

        let fsels = vec![
            (Cursor::new(1, 0), Cursor::new(1, 5), TargetShape::CharWise),
            (Cursor::new(2, 0), Cursor::new(2, 5), TargetShape::CharWise),
            (Cursor::new(3, 0), Cursor::new(3, 5), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 5), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        selection_join!(ebuf, ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 0), Cursor::new(4, 5), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 5));
        assert_eq!(ebuf.get_follower_selections(curid), None);
    }

    #[test]
    fn test_selection_split_lines_blockwise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("a b c d\ne f g\nh i j k l\nm n o p\nq r\n");

        // Start out at (2, 6).
        ebuf.set_leader(curid, Cursor::new(2, 6));

        vctx.target_shape = Some(TargetShape::BlockWise);

        // Create a charwise selection across the three lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(2, 6), Cursor::new(4, 0), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));

        // Filter doesn't match, nothing happens.
        selection_split_lines!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));

        // Filter matches, splits into multiple CharWise selections.
        selection_split_lines!(ebuf, TargetShapeFilter::BLOCK, ctx!(curid, vwctx, vctx), store);

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
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("a b c d\ne f g\nh i j k l\nm n o p\nq r s t\n");

        // Start out at (1, 2).
        ebuf.set_leader(curid, Cursor::new(1, 2));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a charwise selection across the three lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(1, 2), Cursor::new(3, 0), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter doesn't match, nothing happens.
        selection_split_lines!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter matches, splits into multiple CharWise selections.
        selection_split_lines!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx), store);

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
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("a b c d\ne f g\nh i j k l\nm n o p\nq r s t\n");

        // Start out at (1, 0).
        ebuf.set_leader(curid, Cursor::new(1, 0));

        vctx.target_shape = Some(TargetShape::LineWise);

        // Create a linewise selection across three lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(1, 0), Cursor::new(3, 0), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter doesn't match, nothing happens.
        selection_split_lines!(ebuf, TargetShapeFilter::CHAR, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), None);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));

        // Filter matches, splits into multiple LineWise selections.
        selection_split_lines!(ebuf, TargetShapeFilter::LINE, ctx!(curid, vwctx, vctx), store);

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
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("hello world\na b c d\n");

        // Start out at (0, 6).
        ebuf.set_leader(curid, Cursor::new(0, 6));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a selection to resize from here to next word beginning.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(0, 6), Cursor::new(1, 0), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // We're already at the end so this shouldn't move.
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // Now swap which side is the anchor, and which side is mobile.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(false),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // We're already at the beginning so this shouldn't move.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::Beginning,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Move to end.
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_selection_cursor_set_linewise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("foo\nhello world\na b c d\n");

        // Start out at (1, 6).
        ebuf.set_leader(curid, Cursor::new(1, 6));

        vctx.target_shape = Some(TargetShape::LineWise);

        // Create a linewise selection across the two lines.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(1, 6), Cursor::new(2, 0), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));

        // Now swap which side is the anchor, and which side is mobile.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(false),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));

        // Moving to the end of the selection places cursor in last column of last line.
        let selection = (Cursor::new(1, 0), Cursor::new(2, 6), TargetShape::LineWise);
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 6));

        // Moving to the beginning of the selection places cursor in first column of first line.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::Beginning,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_selection_cursor_set_blockwise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("foo\nhello world\na b c d\n");

        // Start out at (1, 2).
        ebuf.set_leader(curid, Cursor::new(1, 2));

        vctx.target_shape = Some(TargetShape::BlockWise);

        // Create a block selection across two lines.
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 21), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(1, 2), Cursor::new(2, 4), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 4));

        // Now swap which side is the anchor, and which side is mobile.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(false),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));

        // Now swap over to the upper-right corner.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::SwapAnchor(true),
            ctx!(curid, vwctx, vctx),
            store
        );

        let selection = (Cursor::new(1, 4), Cursor::new(2, 2), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 4));

        // Now swap to the first column of the selection.
        selection_cursor_set!(
            ebuf,
            &SelectionCursorChange::Beginning,
            ctx!(curid, vwctx, vctx),
            store
        );

        let selection = (Cursor::new(1, 2), Cursor::new(2, 4), TargetShape::BlockWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));

        // Now swap to the last column of the selection.
        selection_cursor_set!(ebuf, &SelectionCursorChange::End, ctx!(curid, vwctx, vctx), store);

        assert_eq!(ebuf.get_leader_selection(curid), Some(selection.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 4));
    }

    #[test]
    fn test_selection_restart() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("hello world a b c d\n");

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a selection to resize from here to next word beginning.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(0, 0), Cursor::new(0, 6), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Now resize the selection to just be the second word.
        let mov = MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next);
        selection_restart!(ebuf, mv!(mov), ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(0, 6), Cursor::new(0, 10), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));

        // Now resize the selection to just be the cursor position.
        selection_restart!(ebuf, EditTarget::CurrentPosition, ctx!(curid, vwctx, vctx), store);

        let selection = (Cursor::new(0, 10), Cursor::new(0, 10), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(selection));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));
    }

    #[test]
    fn test_selection_resize_range_grows() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("hello world\n");
        let cw = TargetShape::CharWise;

        // Start out at (0, 2), over "l".
        ebuf.set_leader(curid, Cursor::new(0, 2));

        vctx.target_shape = Some(cw);

        // Create selection over "ll".
        let right = MoveType::Column(MoveDir1D::Next, false).into();
        let lsel = (Cursor::new(0, 2), Cursor::new(0, 3), cw);
        selection_extend!(ebuf, right, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));

        // Doing a word range should also move anchor.
        let range = RangeType::Word(WordStyle::Little).into();
        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), cw);
        selection_extend!(ebuf, range, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
    }

    #[test]
    fn test_selection_duplicate_and_rotate() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr(
            "copy lines\na1\ncopy lines\n\nstart line\n\nb2\ncopy lines\nc3\ncopy lines\n",
        );

        // Start out at (4, 2), on the " " in "copy lines".
        ebuf.set_leader(curid, Cursor::new(4, 2));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a selection from " " to "n".
        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, EditAction::Motion, mv!(mov, 5), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(4, 2), Cursor::new(4, 7), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 7));

        // Now duplicate the selection twice, onto the next available lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 2.into(), ctx!(curid, vwctx, vctx), store);

        let fsels = vec![
            (Cursor::new(7, 2), Cursor::new(7, 7), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 7), TargetShape::CharWise),
        ];

        // Leader stays the same, and we have new selections on the lines containing "copy line".
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 7));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Duplicating again doesn't result in overlapping selections.
        selection_duplicate!(ebuf, MoveDir1D::Next, 2.into(), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 7));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Move back to word end so we have different end columns.
        let mov = MoveType::WordEnd(WordStyle::Little, MoveDir1D::Previous);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(4, 2), Cursor::new(4, 4), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(7, 2), Cursor::new(7, 3), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 3), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Duplicate backwards once.
        selection_duplicate!(ebuf, MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx), store);

        let fsels = vec![
            (Cursor::new(2, 2), Cursor::new(2, 4), TargetShape::CharWise),
            (Cursor::new(7, 2), Cursor::new(7, 3), TargetShape::CharWise),
            (Cursor::new(9, 2), Cursor::new(9, 3), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Duplicate 4 times backwards, getting one more selection.
        selection_duplicate!(ebuf, MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx), store);

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
        selection_duplicate!(ebuf, MoveDir1D::Next, 1.into(), ctx!(curid, vwctx, vctx), store);
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
        selection_duplicate!(ebuf, MoveDir1D::Next, 1.into(), ctx!(curid, vwctx, vctx), store);
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
        selection_rotate!(ebuf, MoveDir1D::Next, 3.into(), ctx!(curid, vwctx, vctx), store);
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
        selection_rotate!(ebuf, MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx), store);
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
        selection_rotate!(ebuf, MoveDir1D::Previous, 2.into(), ctx!(curid, vwctx, vctx), store);
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
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("  a  \n  b  \n  c  \n  d  \n  e  \n");

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a selection to the end of the line.
        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, EditAction::Motion, mv!(mov, 4), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Now duplicate the selection four times.
        selection_duplicate!(ebuf, MoveDir1D::Next, 4.into(), ctx!(curid, vwctx, vctx), store);

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
        selection_rotate!(ebuf, MoveDir1D::Next, 1.into(), ctx!(curid, vwctx, vctx), store);

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
        selection_close!(ebuf, &CursorCloseTarget::Leader, ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(2, 0), Cursor::new(2, 4), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise),
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 4), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 4));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Close followers.
        selection_close!(ebuf, &CursorCloseTarget::Followers, ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(2, 0), Cursor::new(2, 4), TargetShape::CharWise);

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 4));
        assert_eq!(ebuf.get_follower_selections(curid), None);
    }

    #[test]
    fn test_selection_expand_line_charwise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("a   \t   b   \n   c   \n");

        // Start out at (0, 1), on the space after the "a".
        ebuf.set_leader(curid, Cursor::new(0, 1));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a selection from the space after "a" to "c".
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 17), ctx!(curid, vwctx, vctx), store);
        let lsel = (Cursor::new(0, 1), Cursor::new(1, 3), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Expand with TargetShapeFilter::LINE does nothing.
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::LINE,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Now expand to include both lines.
        let lsel = (Cursor::new(0, 0), Cursor::new(1, 7), TargetShape::CharWise);
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 7));

        // Shrink selection to go from start to "b".
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 9), ctx!(curid, vwctx, vctx), store);
        let lsel = (Cursor::new(0, 0), Cursor::new(0, 8), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 8));

        // Now expand to include just the first line.
        let lsel = (Cursor::new(0, 0), Cursor::new(0, 12), TargetShape::CharWise);
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 12));

        // Repeating changes nothing.
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 12));
    }

    #[test]
    fn test_selection_expand_line_linewise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("a   \t   b   \n   c   \n");

        // Start out at (0, 1), on the space after the "a".
        ebuf.set_leader(curid, Cursor::new(0, 1));

        vctx.target_shape = Some(TargetShape::LineWise);

        // Create a selection from the space after "a" to "c".
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 17), ctx!(curid, vwctx, vctx), store);
        let lsel = (Cursor::new(0, 1), Cursor::new(1, 3), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Expand with TargetShapeFilter::CHAR does nothing.
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Since this was a linewise selection, expansion should touch same content, but move
        // anchor and cursor.
        let lsel = (Cursor::new(0, 0), Cursor::new(1, 7), TargetShape::LineWise);
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::LINE,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 7));

        // Shrink selection to first line, with cursor at "b".
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 9), ctx!(curid, vwctx, vctx), store);
        let lsel = (Cursor::new(0, 0), Cursor::new(0, 8), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 8));

        // Expansion still only selects first line, but cursor is moved.
        let lsel = (Cursor::new(0, 0), Cursor::new(0, 12), TargetShape::LineWise);
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::LINE,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 12));

        // Repeating changes nothing.
        selection_expand!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::LINE,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 12));
    }

    #[test]
    fn test_selection_trim_line_charwise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("a   \t   b   \n   c   \n");

        // Start out at (0, 1), on the space after the "a".
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a selection from the first character to "c".
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 17), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 0), Cursor::new(1, 3), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Trim with TargetShapeFilter::LINE does nothing.
        selection_trim!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::LINE,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Now trim down to the first line.
        selection_trim!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 12), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 12));

        // Repeating changes nothing.
        selection_trim!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 12), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 12));
    }

    #[test]
    fn test_selection_trim_nws_charwise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("a   \t   b   \n   c   \n");

        // Start out at (0, 1), on the space after the "a".
        ebuf.set_leader(curid, Cursor::new(0, 1));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Create a selection from the space after "a" to the final column.
        let mov = MoveType::BufferByteOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 20), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 1), Cursor::new(1, 6), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));

        // Trim with TargetShapeFilter::LINE does nothing.
        selection_trim!(
            ebuf,
            SelectionBoundary::NonWhitespace,
            TargetShapeFilter::LINE,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));

        // Now trim down to the "b" and the "c" with TargetShapeFilter::CHAR.
        selection_trim!(
            ebuf,
            SelectionBoundary::NonWhitespace,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );

        let lsel = (Cursor::new(0, 8), Cursor::new(1, 3), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));

        // Move back 1 from the "c".
        let mov = MoveType::Column(MoveDir1D::Previous, false);
        edit!(ebuf, EditAction::Motion, mv!(mov, 1), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 8), Cursor::new(1, 2), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));

        // Trim down to just the "b".
        selection_trim!(
            ebuf,
            SelectionBoundary::NonWhitespace,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );

        let lsel = (Cursor::new(0, 8), Cursor::new(0, 8), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 8));
    }

    #[test]
    fn test_selection_trim_nws_linewise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("a\n   \n\t\nb   \n   c   \n\n\n");

        // Start out at (1, 0), a line of all spaces.
        ebuf.set_leader(curid, Cursor::new(1, 0));

        vctx.target_shape = Some(TargetShape::LineWise);

        // Create a selection down to line 6.
        let mov = MoveType::Line(MoveDir1D::Next);
        edit!(ebuf, EditAction::Motion, mv!(mov, 5), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(1, 0), Cursor::new(6, 0), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(6, 0));

        // Trim with TargetShapeFilter::CHAR does nothing.
        selection_trim!(
            ebuf,
            SelectionBoundary::NonWhitespace,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(6, 0));

        // Now trim down to the "b" and "c" lines with TargetShapeFilter::LINE.
        selection_trim!(
            ebuf,
            SelectionBoundary::NonWhitespace,
            TargetShapeFilter::LINE,
            ctx!(curid, vwctx, vctx),
            store
        );

        let lsel = (Cursor::new(3, 0), Cursor::new(4, 3), TargetShape::LineWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 3));
    }

    #[test]
    fn test_selection_trim_line_drop_empty() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("  a  \n      \n  b  \n      \n  c  \n");

        // Start out at (0, 1), on the space after the "a".
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Select the whole first line.
        let mov = MoveType::Column(MoveDir1D::Next, true);
        edit!(ebuf, EditAction::Motion, mv!(mov, 5), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 5), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        // Duplicate selection across the following lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 4.into(), ctx!(curid, vwctx, vctx), store);

        let fsels = vec![
            (Cursor::new(1, 0), Cursor::new(1, 5), TargetShape::CharWise),
            (Cursor::new(2, 0), Cursor::new(2, 5), TargetShape::CharWise),
            (Cursor::new(3, 0), Cursor::new(3, 5), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 5), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Now trim, dropping two selections, and shrinking 3.
        selection_trim!(
            ebuf,
            SelectionBoundary::Line,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 5), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(2, 0), Cursor::new(2, 5), TargetShape::CharWise),
            (Cursor::new(4, 0), Cursor::new(4, 5), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));
    }

    #[test]
    fn test_selection_trim_nws_drop_empty() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("  a  \n     \n  b  \n     \n  c  \n");

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        vctx.target_shape = Some(TargetShape::CharWise);

        // Select the whole first line.
        let mov = MoveType::Column(MoveDir1D::Next, true);
        edit!(ebuf, EditAction::Motion, mv!(mov, 4), ctx!(curid, vwctx, vctx), store);

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise);
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Duplicate selection across the following lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 4.into(), ctx!(curid, vwctx, vctx), store);

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
        selection_trim!(
            ebuf,
            SelectionBoundary::NonWhitespace,
            TargetShapeFilter::CHAR,
            ctx!(curid, vwctx, vctx),
            store
        );

        let lsel = (Cursor::new(0, 2), Cursor::new(0, 2), TargetShape::CharWise);
        let fsels = vec![
            (Cursor::new(2, 2), Cursor::new(2, 2), TargetShape::CharWise),
            (Cursor::new(4, 2), Cursor::new(4, 2), TargetShape::CharWise),
        ];

        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));
    }

    #[test]
    fn test_selection_filter_keep_matches() {
        let (mut ebuf, curid, vwctx, vctx, mut store) = mkfivestr(
            "hello\nworld\nhelp\nwhisk\nhelm\naluminum\nwrithe\ncharacter\nhelium\nproduct\n",
        );

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        // Duplicate selection across the following lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 9.into(), ctx!(curid, vwctx, vctx), store);

        // Extend selection to the end of the line.
        let mov = MoveType::Column(MoveDir1D::Next, false);
        selection_extend!(ebuf, mv!(mov, 10), ctx!(curid, vwctx, vctx), store);

        let fsels = vec![
            (Cursor::new(1, 0), Cursor::new(1, 4), TargetShape::CharWise), // "world"
            (Cursor::new(2, 0), Cursor::new(2, 3), TargetShape::CharWise), // "help"
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise), // "whisk"
            (Cursor::new(4, 0), Cursor::new(4, 3), TargetShape::CharWise), // "helm"
            (Cursor::new(5, 0), Cursor::new(5, 7), TargetShape::CharWise), // "aluminum"
            (Cursor::new(6, 0), Cursor::new(6, 5), TargetShape::CharWise), // "writhe"
            (Cursor::new(7, 0), Cursor::new(7, 8), TargetShape::CharWise), // "character"
            (Cursor::new(8, 0), Cursor::new(8, 5), TargetShape::CharWise), // "helium"
            (Cursor::new(9, 0), Cursor::new(9, 6), TargetShape::CharWise), // "product"
        ];

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise); // "hello"
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Set regex to /he/.
        store.registers.set_last_search("he");

        // Keep selections matching /he/.
        ebuf.selection_filter(false, ctx!(curid, vwctx, vctx), &mut store).unwrap();

        let fsels = vec![
            (Cursor::new(2, 0), Cursor::new(2, 3), TargetShape::CharWise), // "help"
            (Cursor::new(4, 0), Cursor::new(4, 3), TargetShape::CharWise), // "helm"
            (Cursor::new(6, 0), Cursor::new(6, 5), TargetShape::CharWise), // "writhe"
            (Cursor::new(8, 0), Cursor::new(8, 5), TargetShape::CharWise), // "helium"
        ];

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise); // "hello"
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));
    }

    #[test]
    fn test_selection_filter_drop_matches() {
        let (mut ebuf, curid, vwctx, vctx, mut store) = mkfivestr(
            "hello\nworld\nhelp\nwhisk\nhelm\naluminum\nwrithe\ncharacter\nhelium\nproduct\n",
        );

        // Start out at (0, 0).
        ebuf.set_leader(curid, Cursor::new(0, 0));

        // Duplicate selection across the following lines.
        selection_duplicate!(ebuf, MoveDir1D::Next, 9.into(), ctx!(curid, vwctx, vctx), store);

        // Extend selection to the end of the line.
        let mov = MoveType::Column(MoveDir1D::Next, false);
        selection_extend!(ebuf, mv!(mov, 10), ctx!(curid, vwctx, vctx), store);

        let fsels = vec![
            (Cursor::new(1, 0), Cursor::new(1, 4), TargetShape::CharWise), // "world"
            (Cursor::new(2, 0), Cursor::new(2, 3), TargetShape::CharWise), // "help"
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise), // "whisk"
            (Cursor::new(4, 0), Cursor::new(4, 3), TargetShape::CharWise), // "helm"
            (Cursor::new(5, 0), Cursor::new(5, 7), TargetShape::CharWise), // "aluminum"
            (Cursor::new(6, 0), Cursor::new(6, 5), TargetShape::CharWise), // "writhe"
            (Cursor::new(7, 0), Cursor::new(7, 8), TargetShape::CharWise), // "character"
            (Cursor::new(8, 0), Cursor::new(8, 5), TargetShape::CharWise), // "helium"
            (Cursor::new(9, 0), Cursor::new(9, 6), TargetShape::CharWise), // "product"
        ];

        let lsel = (Cursor::new(0, 0), Cursor::new(0, 4), TargetShape::CharWise); // "hello"
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));

        // Set regex to /he/.
        store.registers.set_last_search("he");

        // Drop selections matching /he/.
        ebuf.selection_filter(true, ctx!(curid, vwctx, vctx), &mut store).unwrap();

        let fsels = vec![
            (Cursor::new(1, 0), Cursor::new(1, 4), TargetShape::CharWise), // "world"
            (Cursor::new(3, 0), Cursor::new(3, 4), TargetShape::CharWise), // "whisk"
            (Cursor::new(5, 0), Cursor::new(5, 7), TargetShape::CharWise), // "aluminum"
            (Cursor::new(7, 0), Cursor::new(7, 8), TargetShape::CharWise), // "character"
        ];

        let lsel = (Cursor::new(9, 0), Cursor::new(9, 6), TargetShape::CharWise); // "product"
        assert_eq!(ebuf.get_leader(curid), Cursor::new(9, 6));
        assert_eq!(ebuf.get_leader_selection(curid), Some(lsel.clone()));
        assert_eq!(ebuf.get_follower_selections(curid), Some(fsels.clone()));
    }
}
