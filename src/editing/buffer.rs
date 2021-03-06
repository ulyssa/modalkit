use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::Range;

use crate::{
    editing::cursor::{Cursor, CursorAdjustment},
    editing::histlist::HistoryList,
    editing::lineinfo::LineInfoStore,
    editing::rope::{ByteOff, CursorContext, EditRope, PrivateCursorOps},
    editing::store::{BufferId, CursorStore, RegisterCell, SharedBuffer, SharedStore},
    util::{sort2, IdGenerator},
};

use super::base::{
    Application,
    Case,
    Char,
    Count,
    CursorAction,
    CursorCloseTarget,
    CursorMovements,
    CursorMovementsContext,
    CursorSearch,
    EditAction,
    EditContext,
    EditError,
    EditRange,
    EditResult,
    EditTarget,
    HistoryAction,
    IndentChange,
    InsertStyle,
    Mark,
    MoveDir1D,
    MoveDirMod,
    NumberChange,
    Register,
    SearchType,
    SelectionCursorChange,
    Specifier,
    TargetShape,
    TargetShapeFilter,
    ViewportContext,
};

trait EditString<C> {
    fn delete(&mut self, range: &CursorRange, ctx: C) -> Option<Cursor>;
    fn yank(&mut self, range: &CursorRange, ctx: C) -> Option<Cursor>;
    fn replace(&mut self, c: char, virt: bool, range: &CursorRange, ctx: C) -> Option<Cursor>;
    fn changecase(&mut self, case: &Case, range: &CursorRange, ctx: C) -> Option<Cursor>;
    fn format(&mut self, range: &CursorRange, ctx: C) -> Option<Cursor>;
    fn changenum(&mut self, change: &NumberChange, range: &CursorRange, ctx: C) -> Option<Cursor>;
    fn join(&mut self, spaces: bool, range: &CursorRange, ctx: C) -> Option<Cursor>;
    fn indent(&mut self, change: &IndentChange, range: &CursorRange, ctx: C) -> Option<Cursor>;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct CursorId(u64);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct CursorGroupId(u64);

pub type CursorRange = EditRange<Cursor>;

pub struct EditBuffer<C: EditContext, P: Application> {
    /// A unique identifier for this buffer.
    id: BufferId,

    /// The current contents of the buffer.
    text: EditRope,

    /// Allocates new CursorIds.
    idgen: IdGenerator,

    /// Tracks the shape of the selection associated with CursorId.
    vshapes: HashMap<CursorId, TargetShape>,

    /// Tracks the anchors for all current selections.
    anchors: CursorStore<CursorId>,

    /// Tracks all of the current cursors.
    cursors: CursorStore<CursorId>,

    /// Allocates new CursorGroupIds.
    cgidgen: IdGenerator,

    /// Tracks the members of a cursor group.
    leaders: HashMap<CursorGroupId, CursorId>,

    /// Tracks the members of a cursor group.
    members: HashMap<CursorGroupId, Vec<CursorId>>,

    history: HistoryList<EditRope>,
    lineinfo: LineInfoStore<usize>,
    store: SharedStore<C, P>,

    _pc: PhantomData<C>,
}

trait HistoryActions<C> {
    fn redo(&mut self, count: Count, ctx: &C) -> EditResult;
    fn undo(&mut self, count: Count, ctx: &C) -> EditResult;
    fn checkpoint(&mut self) -> EditResult;
}

trait CursorActions<C> {
    fn cursor_split(&mut self, count: Count, ctx: &C) -> EditResult;
    fn cursor_close(&mut self, target: &CursorCloseTarget, ctx: &C) -> EditResult;
}

pub trait Editable<C> {
    fn edit(&mut self, action: &EditAction, target: &EditTarget, ctx: &C) -> EditResult;
    fn type_char(&mut self, ch: Char, ctx: &C) -> EditResult;
    fn selcursor_set(&mut self, side: &SelectionCursorChange, ctx: &C) -> EditResult;
    fn selection_split_lines(&mut self, filter: TargetShapeFilter, ctx: &C) -> EditResult;
    fn paste(&mut self, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult;
    fn open_line(&mut self, dir: MoveDir1D, ctx: &C) -> EditResult;
    fn mark(&mut self, name: Mark, ctx: &C) -> EditResult;

    fn cursor_command(&mut self, act: CursorAction, ctx: &C) -> EditResult;
    fn history_command(&mut self, act: HistoryAction, ctx: &C) -> EditResult;
}

type Selection = (Cursor, Cursor, TargetShape);
type Selections = Vec<Selection>;
type CursorGroupIdContext<'a, 'b, T> = (CursorGroupId, &'a ViewportContext<Cursor>, &'b T);

impl<C, P> EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    pub fn new(id: BufferId, store: SharedStore<C, P>) -> Self {
        let text = EditRope::from("\n");
        let history = HistoryList::new(text.clone(), 100);
        let lineinfo = LineInfoStore::new();

        EditBuffer {
            id,
            text,

            idgen: IdGenerator::default(),
            cursors: CursorStore::default(),
            anchors: CursorStore::default(),
            vshapes: HashMap::new(),

            cgidgen: IdGenerator::default(),
            leaders: HashMap::new(),
            members: HashMap::new(),

            history,
            lineinfo,
            store,

            _pc: PhantomData,
        }
    }

    pub fn buffer_id(&self) -> BufferId {
        self.id
    }

    fn _char<'a, 'b, 'c>(&self, c: Char, cursor: &Cursor) -> EditResult<char> {
        match c {
            Char::Single(c) => {
                return Ok(c);
            },
            Char::Digraph(d1, d2) => {
                if let Some(c) = self.store.read().unwrap().digraphs.get((d1, d2)) {
                    Ok(c)
                } else {
                    Err(EditError::InvalidDigraph(d1, d2))
                }
            },
            Char::CtrlSeq(_) => {
                let msg = "Cannot represent control sequence as a character".to_string();
                let err = EditError::Failure(msg);

                return Err(err);
            },
            Char::CopyLine(MoveDir1D::Previous) => {
                fn err() -> EditError {
                    let msg = "No character above cursor".to_string();

                    return EditError::Failure(msg);
                }

                if cursor.y == 0 {
                    return Err(err());
                }

                let above = Cursor::new(cursor.y - 1, cursor.x);

                self.text.get_char_at(&above).ok_or_else(err)
            },
            Char::CopyLine(MoveDir1D::Next) => {
                fn err() -> EditError {
                    let msg = "No character below cursor".to_string();

                    return EditError::Failure(msg);
                }

                let below = Cursor::new(cursor.y + 1, cursor.x);

                self.text.get_char_at(&below).ok_or_else(err)
            },
        }
    }

    fn _str<'a, 'b, 'c>(&self, c: Char, cursor: &Cursor) -> EditResult<String> {
        match c {
            Char::Single(c) => {
                return Ok(c.to_string());
            },
            Char::Digraph(d1, d2) => {
                if let Some(c) = self.store.read().unwrap().digraphs.get((d1, d2)) {
                    return Ok(c.to_string());
                } else {
                    return Err(EditError::InvalidDigraph(d1, d2));
                }
            },
            Char::CtrlSeq(s) => {
                return Ok(s);
            },
            Char::CopyLine(MoveDir1D::Previous) => {
                let msg = "No character above cursor".to_string();
                let err = EditError::Failure(msg);

                if cursor.y == 0 {
                    return Err(err);
                }

                let above = Cursor::new(cursor.y - 1, cursor.x);

                if let Some(c) = self.text.get_char_at(&above) {
                    return Ok(c.to_string());
                } else {
                    return Err(err);
                }
            },
            Char::CopyLine(MoveDir1D::Next) => {
                let below = Cursor::new(cursor.y + 1, cursor.x);

                if let Some(c) = self.text.get_char_at(&below) {
                    return Ok(c.to_string());
                } else {
                    let msg = "No character below cursor".to_string();
                    let err = EditError::Failure(msg);

                    return Err(err);
                }
            },
        }
    }

    fn _charjump<'a, 'b, 'c>(
        &self,
        mark: &Specifier<Mark>,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> EditResult<Cursor> {
        self.get_mark(ctx.context.resolve(mark))
    }

    fn _linejump<'a, 'b, 'c>(
        &self,
        mark: &Specifier<Mark>,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> EditResult<Cursor> {
        let cursor = self.get_mark(ctx.context.resolve(mark))?;
        let cursor = self.text.first_word(&cursor, ctx);

        Ok(cursor)
    }

    fn _charsearch<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        flip: &MoveDirMod,
        multiline: bool,
        count: &Count,
        ctx: &C,
    ) -> EditResult<Option<Cursor>> {
        let res = match ctx.get_search_char() {
            Some((dir, inclusive, needle)) => {
                let needle = self._char(needle, cursor)?;
                let count = ctx.resolve(count);
                let dir = flip.resolve(&dir);

                self.text.find_char(cursor, inclusive, dir, multiline, needle, count)
            },
            None => None,
        };

        Ok(res)
    }

    fn _regexsearch<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        flip: &MoveDirMod,
        count: &Count,
        ctx: &C,
    ) -> EditResult<Option<Cursor>> {
        let res = match ctx.get_search_regex() {
            Some((dir, needle)) => {
                let count = ctx.resolve(count);
                let dir = flip.resolve(&dir);

                self.text.find_regex(cursor, dir, needle, count)
            },
            None => None,
        };

        Ok(res)
    }

    fn _target<'a, 'b, 'c>(
        &self,
        id: CursorId,
        target: &EditTarget,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> EditResult<Option<CursorRange>> {
        let cursor = self.get_cursor(id);

        match target {
            EditTarget::CurrentPosition => {
                let end = cursor.clone();
                let range = CursorRange::inclusive(cursor, end, TargetShape::CharWise);

                return Ok(Some(range));
            },
            EditTarget::CharJump(mark) => {
                let nc = self._charjump(mark, ctx)?;
                let range = CursorRange::exclusive(cursor, nc, TargetShape::CharWise);

                return Ok(Some(range));
            },
            EditTarget::LineJump(mark) => {
                let nc = self._linejump(mark, ctx)?;
                let range = CursorRange::exclusive(cursor, nc, TargetShape::LineWise);

                return Ok(Some(range));
            },
            EditTarget::Search(SearchType::Char(multi), flip, count) => {
                let nco = self._charsearch(&cursor, flip, *multi, count, ctx.context)?;
                let range = nco.map(|nc| {
                    let shape = TargetShape::CharWise;
                    let inclusive = nc > cursor;

                    CursorRange::new(cursor, nc, shape, inclusive)
                });

                return Ok(range);
            },
            EditTarget::Search(SearchType::Regex, flip, count) => {
                let nco = self._regexsearch(&cursor, flip, count, ctx.context)?;
                let range = nco.map(|nc| {
                    let shape = TargetShape::CharWise;
                    let inclusive = nc > cursor;

                    CursorRange::new(cursor, nc, shape, inclusive)
                });

                return Ok(range);
            },
            EditTarget::Selection => {
                if let Some(selnc) = self.anchors.get(id) {
                    let shape = ctx.context.get_target_shape().unwrap_or(TargetShape::CharWise);
                    let range = CursorRange::inclusive(selnc, cursor, shape);

                    return Ok(Some(range));
                } else {
                    return Ok(None);
                }
            },
            EditTarget::Motion(motion, count) => {
                return Ok(self.text.range_of_movement(&cursor, motion, count, ctx));
            },
            EditTarget::Range(range, count) => {
                return Ok(self.text.range(&cursor, range, count, ctx));
            },
        }
    }

    fn _effective<'a, 'b, 'c>(
        &self,
        range: &CursorRange,
        forced: Option<TargetShape>,
    ) -> (TargetShape, Vec<(ByteOff, ByteOff, bool)>) {
        match forced.unwrap_or(range.shape) {
            TargetShape::CharWise => {
                let start = self.text.cursor_to_offset(&range.start);
                let end = self.text.cursor_to_offset(&range.end);
                let ranges = vec![(start, end, range.inclusive)];

                (TargetShape::CharWise, ranges)
            },
            TargetShape::LineWise => {
                let start = self.text.offset_of_line(range.start.y);
                let end = self.text.offset_of_line(range.end.y);
                let mut iter = self.text.newlines(end);
                let ranges = match iter.next() {
                    Some(end) => {
                        vec![(start, end, true)]
                    },
                    None => {
                        vec![(start, self.text.len_offset(), false)]
                    },
                };

                (TargetShape::LineWise, ranges)
            },
            TargetShape::BlockWise => {
                // Determine the left and right borders of the block.
                let (mut lc, mut rc) = if range.start.x < range.end.x {
                    (range.start.clone(), range.end.clone())
                } else {
                    (range.end.clone(), range.start.clone())
                };

                let mut ranges = vec![];
                let lctx = &(&self.text, 0, true);
                let rctx = &(&self.text, 0, false);

                for line in range.start.y..=range.end.y {
                    lc.set_line(line, lctx);
                    rc.set_line(line, rctx);

                    let left = self.text.cursor_to_offset(&lc);
                    let right = self.text.cursor_to_offset(&rc);

                    ranges.push((left, right, true));
                }

                (TargetShape::BlockWise, ranges)
            },
        }
    }

    fn _adjust(&mut self, adj: &CursorAdjustment) {
        let mut store = self.store.write().unwrap();

        self.cursors.adjust(adj);
        self.anchors.adjust(adj);

        store.marks.adjust(self.id, adj);
    }

    fn _adjust_all(&mut self, adjs: Vec<CursorAdjustment>) {
        for adj in adjs {
            self._adjust(&adj);
        }
    }

    fn _adjust_columns(
        &mut self,
        line: usize,
        column_start: usize,
        amt_line: isize,
        amt_col: isize,
    ) {
        let adj = CursorAdjustment::Column { line, column_start, amt_line, amt_col };

        self._adjust(&adj);
    }

    fn _adjust_lines(
        &mut self,
        line_start: usize,
        line_end: usize,
        amount: isize,
        amount_after: isize,
    ) {
        let adj = CursorAdjustment::Line { line_start, line_end, amount, amount_after };

        self._adjust(&adj);
    }

    pub(crate) fn motion<'a, 'b, 'c>(
        &mut self,
        target: &EditTarget,
        ictx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        for id in self.get_group(ictx.0) {
            let shape = ictx.2.get_target_shape();
            let mut cursor = self.get_cursor(id);

            match shape {
                None => {
                    self.clear_selection(id);
                },
                Some(shape) => {
                    if !self.anchors.contains(id) {
                        self.clamp(&mut cursor, ictx);
                        self.anchors.put(id, cursor.clone());
                    }

                    self.vshapes.insert(id, shape);
                },
            }

            let ctx = self._ctx_cgi2es(&EditAction::Motion, ictx);

            match target {
                EditTarget::CurrentPosition => {
                    self.set_cursor(id, cursor);
                },
                EditTarget::CharJump(mark) => {
                    let nc = self._charjump(mark, &ctx)?;
                    self.set_cursor(id, nc);
                },
                EditTarget::LineJump(mark) => {
                    let nc = self._linejump(mark, &ctx)?;
                    self.set_cursor(id, nc);
                },
                EditTarget::Motion(mv, count) => {
                    if let Some(nc) = self.text.movement(&cursor, mv, count, &ctx) {
                        self.set_cursor(id, nc);
                    }
                },
                EditTarget::Range(range, count) => {
                    if let Some(r) = self.text.range(&cursor, range, count, &ctx) {
                        if shape.is_some() {
                            // XXX: not right: only ever grows, and picks start or end based on that.
                            self.anchors.put(id, r.start);
                        }

                        self.set_cursor(id, r.end);
                    }
                },
                EditTarget::Search(SearchType::Char(wrap), flip, count) => {
                    if let Some(end) = self._charsearch(&cursor, flip, *wrap, count, ctx.context)? {
                        self.set_cursor(id, end);
                    }
                },
                EditTarget::Search(SearchType::Regex, flip, count) => {
                    if let Some(end) = self._regexsearch(&cursor, flip, count, ctx.context)? {
                        self.set_cursor(id, end);
                    }
                },
                EditTarget::Selection => {
                    // Do nothing.
                },
            }
        }

        Ok(None)
    }

    pub fn get_mark(&self, mark: Mark) -> EditResult<Cursor> {
        self.store
            .read()
            .unwrap()
            .marks
            .get(self.id, mark)
            .ok_or(EditError::MarkNotSet(mark))
    }

    pub fn set_mark(&mut self, mark: Mark, cursor: Cursor) {
        self.store.write().unwrap().marks.put(self.id, mark, cursor);
    }

    pub fn get_register(&self, register: &Option<Register>) -> RegisterCell {
        self.store.read().unwrap().registers.get(register)
    }

    pub fn set_register(
        &mut self,
        register: &Option<Register>,
        cell: RegisterCell,
        append: bool,
        del: bool,
    ) {
        let mut store = self.store.write().unwrap();

        store.registers.put(register, cell, append, del)
    }

    pub fn get_text(&self) -> String {
        self.text.to_string()
    }

    pub fn set_text<T: Into<String>>(&mut self, t: T) {
        let s: String = t.into();

        self.text = EditRope::from(s);
        self.text.trailing_newline();

        self.cursors.zero_all();
        self.anchors.zero_all();

        self.store.write().unwrap().marks.zero_all(self.id);
    }

    pub fn append_text<T: Into<String>>(&mut self, t: T) -> Range<usize> {
        let s: String = t.into();

        let start = self.get_lines();
        self.text += EditRope::from(s);
        self.text.trailing_newline();
        let end = self.get_lines();

        Range { start, end }
    }

    pub fn reset_text(&mut self) -> String {
        let text = self.text.to_string();

        self.set_text("\n");

        return text;
    }

    fn add_follower(&mut self, group: CursorGroupId, follower: CursorId) {
        if let Some(followers) = self.members.get_mut(&group) {
            followers.push(follower);
        } else {
            let followers = vec![follower];
            self.members.insert(group, followers);
        }
    }

    pub fn get_group(&self, id: CursorGroupId) -> Vec<CursorId> {
        let leader = self.leaders.get(&id).unwrap();
        let mut group = vec![*leader];

        if let Some(members) = self.members.get(&id) {
            group.extend_from_slice(members.as_slice());
        }

        group
    }

    pub fn get_group_cursors(&self, id: CursorGroupId) -> Vec<(CursorId, Cursor)> {
        self.get_group(id)
            .into_iter()
            .map(|cid| (cid, self.get_cursor(cid)))
            .collect()
    }

    pub fn set_leader(&mut self, id: CursorGroupId, cursor: Cursor) {
        let leader = *self.leaders.get(&id).unwrap();

        self.set_cursor(leader, cursor);
    }

    pub fn get_leader(&self, id: CursorGroupId) -> Cursor {
        let leader = self.leaders.get(&id).unwrap();

        self.get_cursor(*leader)
    }

    pub fn get_followers(&self, id: CursorGroupId) -> Vec<Cursor> {
        if let Some(followers) = self.members.get(&id) {
            followers.into_iter().map(|cid| self.get_cursor(*cid)).collect()
        } else {
            Vec::new()
        }
    }

    pub fn get_follower_selections(&self, id: CursorGroupId) -> Option<Selections> {
        let followers = self.members.get(&id)?;
        let selections = followers
            .into_iter()
            .filter_map(|follower| self.get_selection(*follower))
            .collect::<Selections>();

        Some(selections)
    }

    pub fn get_leader_selection(&self, id: CursorGroupId) -> Option<Selection> {
        let leader = self.leaders.get(&id)?;

        self.get_selection(*leader)
    }

    pub fn get_group_selections(&self, id: CursorGroupId) -> Option<Selections> {
        let lsel = self.get_leader_selection(id)?;

        if let Some(mut fsels) = self.get_follower_selections(id) {
            fsels.push(lsel);

            Some(fsels)
        } else {
            Some(vec![lsel])
        }
    }

    pub fn create_group(&mut self) -> CursorGroupId {
        let id = CursorGroupId(self.cgidgen.next());
        let cursor = self.create_cursor();

        self.leaders.insert(id, cursor);

        id
    }

    pub fn create_cursor(&mut self) -> CursorId {
        self.create_cursor_at(0, 0)
    }

    fn create_cursor_from(&mut self, cursor: &Cursor) -> CursorId {
        let id = CursorId(self.idgen.next());

        self.cursors.put(id, cursor.clone());

        return id;
    }

    fn create_cursor_at(&mut self, line: usize, column: usize) -> CursorId {
        let id = CursorId(self.idgen.next());
        let cursor = Cursor::new(line, column);

        self.cursors.put(id, cursor);

        return id;
    }

    fn delete_cursor(&mut self, id: CursorId) {
        let _ = self.vshapes.remove(&id);
        self.cursors.del(id);
        self.anchors.del(id);
    }

    pub fn clear_selection(&mut self, id: CursorId) {
        let _ = self.vshapes.remove(&id);
        self.anchors.del(id);
    }

    pub fn get_selection(&self, id: CursorId) -> Option<Selection> {
        let anchor = self.anchors.get(id)?;
        let cursor = self.cursors.get(id)?;
        let shape = self.vshapes.get(&id)?;

        if anchor < cursor {
            return Some((anchor, cursor, *shape));
        } else {
            return Some((cursor, anchor, *shape));
        }
    }

    pub fn get_cursor(&self, id: CursorId) -> Cursor {
        self.cursors.get(id).unwrap()
    }

    pub fn set_cursor(&mut self, id: CursorId, cursor: Cursor) {
        self.cursors.put(id, cursor)
    }

    pub fn lines(&self, line: usize) -> xi_rope::rope::Lines {
        self.text.lines(line)
    }

    pub fn lines_at(&self, line: usize, column: usize) -> xi_rope::rope::Lines {
        self.text.lines_at(line, column)
    }

    pub fn get_lines(&self) -> usize {
        self.text.get_lines()
    }

    pub fn get_columns(&self, y: usize) -> usize {
        self.text.get_columns(y)
    }

    pub fn get_line_info<T: Send + Sync + 'static>(&self, line: usize) -> Option<&T> {
        self.lineinfo.get(&line)
    }

    pub fn get_line_info_mut<T: Send + Sync + 'static>(&mut self, line: usize) -> Option<&mut T> {
        self.lineinfo.get_mut(&line)
    }

    pub fn set_line_info<T: Send + Sync + 'static>(&mut self, line: usize, info: T) {
        self.lineinfo.set(line, info);
    }

    pub fn clamp<'a, 'b>(&self, cursor: &mut Cursor, ctx: &CursorGroupIdContext<'a, 'b, C>) {
        PrivateCursorOps::clamp(cursor, &self._ctx_cgi2c(ctx));
    }

    fn _ctx_cgi2es<'a, 'b, 'c>(
        &self,
        action: &'a EditAction,
        ctx: &CursorGroupIdContext<'b, 'c, C>,
    ) -> CursorMovementsContext<'a, 'b, 'c, Cursor, C> {
        CursorMovementsContext { action, view: ctx.1, context: ctx.2 }
    }

    fn _ctx_cgi2c<'a, 'b, 'c>(
        &'a self,
        ctx: &CursorGroupIdContext<'b, 'c, C>,
    ) -> CursorContext<'a> {
        let lastcol = ctx.2.get_insert_style().is_some();
        let width = ctx.1.get_width();

        (&self.text, width, lastcol)
    }

    fn _ctx_es2c<'a, 'b, 'c, 'd>(
        &'d self,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> CursorContext<'d> {
        let lastcol = ctx.context.get_insert_style().is_some();
        let width = ctx.view.get_width();

        (&self.text, width, lastcol)
    }
}

impl<'a, 'b, 'c, C, P> EditString<&CursorMovementsContext<'a, 'b, 'c, Cursor, C>>
    for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn delete(
        &mut self,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        let style = ctx.context.get_insert_style().unwrap_or(InsertStyle::Insert);
        let (shape, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut deleted = EditRope::from("");
        let mut first = true;
        let mut coff = self.text.cursor_to_offset(&range.start);

        for (start, end, inclusive) in ranges.into_iter().rev() {
            if first {
                first = false;
            } else {
                deleted = EditRope::from("\n") + deleted;
            }

            let (prefix, text, suffix) = self.text.split(start, end, inclusive);

            let tlines = text.get_lines();
            let tlen = text.len() as isize;
            let lstart = self.text.line_of_offset(start);

            deleted = text + deleted;

            match style {
                InsertStyle::Insert => {
                    self.text = prefix + suffix;
                },
                InsertStyle::Replace => {
                    let current = self.history.current();
                    let restore = current.slice(start, end, inclusive);

                    self.text = prefix + restore + suffix;
                },
            }

            if tlines == 0 {
                let cstart = self.text.offset_to_cursor(start);
                self._adjust_columns(cstart.y, cstart.x, 0, -tlen);
            } else {
                let lend = lstart.saturating_add(tlines - 1);
                self._adjust_lines(lstart, lend, isize::MAX, -(tlines as isize));
            }

            coff = start;
        }

        self.text.trailing_newline();

        let cell = RegisterCell::new(shape, deleted);
        let register = ctx.context.get_register();
        let append = ctx.context.get_register_append();
        self.set_register(&register, cell, append, true);

        let cursor = self.text.offset_to_cursor(coff);

        return Some(cursor);
    }

    fn yank(
        &mut self,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        let (shape, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut yanked = EditRope::from("");
        let mut first = true;

        for (start, end, inclusive) in ranges.into_iter() {
            if first {
                first = false;
            } else {
                yanked += EditRope::from('\n');
            }

            yanked += self.text.slice(start, end, inclusive);
        }

        let cell = RegisterCell::new(shape, yanked);
        let register = ctx.context.get_register();
        let append = ctx.context.get_register_append();
        self.set_register(&register, cell, append, false);

        match shape {
            TargetShape::LineWise => {
                // LineWise yanks leave the cursor in place.
                return None;
            },
            TargetShape::CharWise => {
                // CharWise yanks place the cursor at the beginning.
                return range.start.clone().into();
            },
            TargetShape::BlockWise => {
                // BlockWise yanks place the cursor at the upper left.
                let y = range.start.y.min(range.end.y);
                let x = range.start.x.min(range.end.x);

                return Cursor::new(y, x).into();
            },
        }
    }

    fn replace(
        &mut self,
        c: char,
        _virt: bool,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        let (_, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut cursor = None;

        // XXX: if this is a blockwise replace, then whitespace needs to be split into individual
        // spaces first, and then replaced.

        for (start, end, inclusive) in ranges.into_iter().rev() {
            self.text = self.text.transform(start, end, inclusive, |r| {
                let s: String = r.to_string();
                let n: String =
                    s.chars().map(|i| if i == '\n' || i == '\r' { i } else { c }).collect();
                return EditRope::from(n);
            });

            /*
             * Unlike most operations, character replacement puts the cursor on the final character
             * in the affected range, and not immediately after it. This allows the cursor to stay
             * in place when doing a single character replacement (e.g. "ra").
             */
            let _ = if inclusive || end == 0.into() {
                cursor.get_or_insert(end)
            } else {
                cursor.get_or_insert(end - 1.into())
            };
        }

        return cursor.map(|off| self.text.offset_to_cursor(off));
    }

    fn changecase(
        &mut self,
        case: &Case,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        let (shape, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut cursor = None;

        for (start, end, inclusive) in ranges.into_iter().rev() {
            self.text = self.text.transform(start, end, inclusive, |r| r.changecase(case));

            cursor = Some(start);
        }

        match shape {
            TargetShape::CharWise => {
                return cursor.map(|off| self.text.offset_to_cursor(off));
            },
            TargetShape::LineWise => {
                if range.start.y == range.end.y {
                    return self.text.first_word(&range.start, ctx).into();
                } else {
                    return range.start.clone().into();
                }
            },
            TargetShape::BlockWise => {
                return cursor.map(|off| self.text.offset_to_cursor(off));
            },
        }
    }

    fn indent(
        &mut self,
        _: &IndentChange,
        _: &CursorRange,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        // XXX: implement (:help <, :help >, :help v_b_<, :help v_b_>)

        return None;
    }

    fn format(
        &mut self,
        _: &CursorRange,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        /*
         * Automatically formatting lines requires a whole lot of logic that just doesn't exist
         * in this codebase yet. At some point, if some kind of filetype detection is added, then
         * this function can be made to do something useful.
         */
        return None;
    }

    fn changenum(
        &mut self,
        _: &NumberChange,
        _: &CursorRange,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        // XXX: implement (:help nrformats)

        return None;
    }

    fn join(
        &mut self,
        spaces: bool,
        range: &CursorRange,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
    ) -> Option<Cursor> {
        // Joining is always forced into a LineWise movement.
        let (_, ranges) = self._effective(range, Some(TargetShape::LineWise));
        let mut cursor = None;

        for (start, end, inclusive) in ranges.into_iter().rev() {
            let mut nls = Vec::new();

            for nl in self.text.newlines(start) {
                if nl > end || (!inclusive && nl == end) {
                    break;
                }

                nls.push(nl);
            }

            if nls.len() > 1 {
                /*
                 * Normally we ignore the final newline in the range since it's the lines *inside*
                 * the range that are being joined together, but since the minimum number of lines
                 * joined is 2, we don't remove the last offset if it's the only offset.
                 */
                let _ = nls.pop();
            }

            for nl in nls.into_iter().rev() {
                // Leave the buffer's final newline alone.
                if nl == self.text.last_offset() {
                    continue;
                }

                let y0 = self.text.line_of_offset(nl);
                let y1 = y0 + 1;
                let y0c = self.text.get_columns(y0);

                let diff = if spaces {
                    let mut iter = self.text.chars(nl + 1.into());
                    let mut blank = false;

                    while let Some(c) = iter.next() {
                        if c == '\n' {
                            blank = true;
                            break;
                        } else if c.is_ascii_whitespace() {
                            continue;
                        } else {
                            break;
                        }
                    }

                    let jtxt = if blank { "" } else { " " };

                    let stop = iter.pos();
                    let space = stop - nl - jtxt.len().into();
                    let camt = y0c as isize + jtxt.len() as isize - usize::from(space) as isize;

                    self._adjust_columns(y1, 0, -1, camt);
                    self._adjust_lines(y1, usize::MAX, -1, 0);
                    self.text.replace(nl, stop, false, jtxt);

                    space
                } else {
                    let camt = y0c as isize;

                    self._adjust_columns(y1, 0, -1, camt);
                    self._adjust_lines(y1, usize::MAX, -1, 0);
                    self.text.replace(nl, nl, true, "");

                    1.into()
                };

                if cursor.is_none() {
                    cursor = Some(nl);
                } else {
                    cursor = cursor.map(|off| off - diff);
                }
            }
        }

        return cursor.map(|off| self.text.offset_to_cursor(off));
    }
}

impl<'a, 'b, C, P> CursorActions<CursorGroupIdContext<'a, 'b, C>> for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn cursor_close(
        &mut self,
        target: &CursorCloseTarget,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let gid = ctx.0;

        match target {
            CursorCloseTarget::Leader => {
                if let Some(members) = self.members.get_mut(&gid) {
                    if members.len() == 0 {
                        return Ok(None);
                    }

                    let members_new = members.split_off(1);
                    let leader_new = members.pop().unwrap();
                    let leader_old = *self.leaders.get(&gid).expect("no current group leader");

                    self.delete_cursor(leader_old);
                    self.leaders.insert(gid, leader_new);
                    self.members.insert(gid, members_new);
                }
            },
            CursorCloseTarget::Followers => {
                if let Some(members) = self.members.get_mut(&ctx.0) {
                    let closed = members.split_off(0);

                    for member in closed.into_iter() {
                        self.delete_cursor(member);
                    }
                }
            },
        }

        Ok(None)
    }

    fn cursor_split(&mut self, count: Count, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let count = ctx.2.resolve(&count);
        let group = ctx.0;

        if count == 1 {
            return Ok(None);
        }

        for (_, cursor) in self.get_group_cursors(group) {
            for _ in 1..count {
                let id = self.create_cursor_from(&cursor);

                self.add_follower(group, id);
            }
        }

        Ok(None)
    }
}

impl<'a, 'b, C, P> HistoryActions<CursorGroupIdContext<'a, 'b, C>> for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn undo(&mut self, count: Count, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let count = ctx.2.resolve(&count);
        let older = self.history.prev(count);

        let adjs = self.text.diff(older);
        self.text = older.clone();
        self._adjust_all(adjs);

        Ok(None)
    }

    fn redo(&mut self, count: Count, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let count = ctx.2.resolve(&count);
        let newer = self.history.next(count);

        let adjs = self.text.diff(newer);
        self.text = newer.clone();
        self._adjust_all(adjs);

        Ok(None)
    }

    fn checkpoint(&mut self) -> EditResult {
        if self.text != self.history.current {
            self.history.append(self.text.clone());
        }

        Ok(None)
    }
}

impl<'a, 'b, C, P> Editable<CursorGroupIdContext<'a, 'b, C>> for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn edit(
        &mut self,
        action: &EditAction,
        target: &EditTarget,
        ictx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        if let EditAction::Motion = action {
            return self.motion(target, ictx);
        }

        let ctx = &self._ctx_cgi2es(action, ictx);

        for member in self.get_group(ictx.0) {
            let nc = match (self._target(member, target, ctx)?, action) {
                (Some(range), EditAction::Delete) => self.delete(&range, ctx),
                (Some(range), EditAction::Yank) => self.yank(&range, ctx),
                (Some(range), EditAction::Replace(v)) => {
                    match ctx.context.get_replace_char() {
                        Some(c) => {
                            let cursor = self.get_cursor(member);
                            let c = self._char(c, &cursor)?;

                            self.replace(c, *v, &range, ctx)
                        },
                        None => {
                            let msg = "No replacement character".to_string();
                            let err = EditError::Failure(msg);

                            return Err(err);
                        },
                    }
                },
                (Some(range), EditAction::Format) => self.format(&range, ctx),
                (Some(range), EditAction::ChangeCase(case)) => self.changecase(case, &range, ctx),
                (Some(range), EditAction::ChangeNumber(change)) => {
                    self.changenum(change, &range, ctx)
                },
                (Some(range), EditAction::Join(spaces)) => self.join(*spaces, &range, ctx),
                (Some(range), EditAction::Indent(change)) => self.indent(change, &range, ctx),
                (Some(_), EditAction::Motion) => panic!("Unexpected EditAction::Motion!"),
                (None, _) => None,
            };

            if let Some(mut nc) = nc {
                self.clamp(&mut nc, ictx);
                self.set_cursor(member, nc);
            }
        }

        Ok(None)
    }

    fn type_char(&mut self, ch: Char, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let style = ctx.2.get_insert_style().unwrap_or(InsertStyle::Insert);

        for (member, cursor) in self.get_group_cursors(ctx.0).into_iter().rev() {
            let s = self._str(ch.clone(), &cursor)?;
            let (cursor, adjs) =
                self.text
                    .insert(&cursor, MoveDir1D::Previous, EditRope::from(s.as_str()), style);

            self._adjust_all(adjs);
            self.set_cursor(member, cursor);
        }

        Ok(None)
    }

    fn selcursor_set(
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
                    if cursor < anchor {
                        self.anchors.put(id, cursor);
                        self.cursors.put(id, anchor);
                    }
                },
                (TargetShape::CharWise, SelectionCursorChange::End) => {
                    if anchor < cursor {
                        self.anchors.put(id, cursor);
                        self.cursors.put(id, anchor);
                    }
                },
                (TargetShape::LineWise, SelectionCursorChange::Beginning) => {
                    if cursor < anchor {
                        self.anchors.put(id, cursor);
                        self.cursors.put(id, anchor);
                    }
                },
                (TargetShape::LineWise, SelectionCursorChange::End) => {
                    if anchor < cursor {
                        self.anchors.put(id, cursor);
                        self.cursors.put(id, anchor);
                    }
                },
                (TargetShape::BlockWise, SelectionCursorChange::Beginning) => {
                    if cursor.x < anchor.x {
                        self.anchors.put(id, cursor);
                        self.cursors.put(id, anchor);
                    }
                },
                (TargetShape::BlockWise, SelectionCursorChange::End) => {
                    if anchor.x < cursor.x {
                        self.anchors.put(id, cursor);
                        self.cursors.put(id, anchor);
                    }
                },
            }
        }

        Ok(None)
    }

    fn selection_split_lines(
        &mut self,
        filter: TargetShapeFilter,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let gid = ctx.0;

        for id in self.get_group(gid) {
            let anchor = self.anchors.get(id).ok_or(EditError::NoSelection)?;
            let cursor = self.cursors.get(id).ok_or(EditError::InvalidCursor)?;
            let shape = *self.vshapes.get(&id).ok_or(EditError::NoSelection)?;

            match shape {
                TargetShape::CharWise => {
                    if !filter.contains(TargetShapeFilter::CHAR) {
                        continue;
                    }

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
                            let cursor_id = self.create_cursor_from(&lc);
                            self.anchors.put(cursor_id, rc.clone());
                            self.vshapes.insert(cursor_id, shape);
                            self.add_follower(gid, cursor_id);
                        }
                    }
                },
                TargetShape::LineWise => {
                    if !filter.contains(TargetShapeFilter::LINE) {
                        continue;
                    }

                    let (start, end) = sort2(anchor.clone(), cursor.clone());

                    for line in start.y..=end.y {
                        let maxidx = self.text.get_columns(line).saturating_sub(1);
                        let lc = Cursor::new(line, 0);
                        let rc = Cursor::new(line, maxidx);

                        if line == start.y {
                            self.cursors.put(id, lc.clone());
                            self.anchors.put(id, rc.clone());
                        } else {
                            let cursor_id = self.create_cursor_from(&lc);
                            self.anchors.put(cursor_id, rc.clone());
                            self.vshapes.insert(cursor_id, shape);
                            self.add_follower(gid, cursor_id);
                        }
                    }
                },
                TargetShape::BlockWise => {
                    if !filter.contains(TargetShapeFilter::BLOCK) {
                        continue;
                    }

                    // Determine the left and right borders of the block.
                    let (mut lc, mut rc) = if anchor.x < cursor.x {
                        (anchor.clone(), cursor.clone())
                    } else {
                        (cursor.clone(), anchor.clone())
                    };

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
                            let cursor_id = self.create_cursor_from(&lc);
                            self.anchors.put(cursor_id, rc.clone());
                            self.vshapes.insert(cursor_id, shape);
                            self.add_follower(gid, cursor_id);
                        }
                    }
                },
            }
        }

        Ok(None)
    }

    fn paste(
        &mut self,
        dir: MoveDir1D,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let count = ctx.2.resolve(&count);
        let style = ctx.2.get_insert_style();
        let cell = self.get_register(&ctx.2.get_register());
        let text = cell.value.repeat(cell.shape, count);

        for member in self.get_group(ctx.0) {
            let cursor = self.get_cursor(member);
            let (mut cursor, adjs) = if let Some(style) = style {
                self.text.insert(&cursor, dir, text.clone(), style)
            } else {
                self.text.paste(&cursor, dir, text.clone(), cell.shape)
            };

            // XXX: remove this and do it right in .paste()
            self.clamp(&mut cursor, ctx);

            self._adjust_all(adjs);
            self.set_cursor(member, cursor);
        }

        Ok(None)
    }

    fn open_line(&mut self, dir: MoveDir1D, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let text = EditRope::from("\n");

        for member in self.get_group(ctx.0) {
            let cursor = self.get_cursor(member);
            let (cursor, adjs) = self.text.paste(&cursor, dir, text.clone(), TargetShape::LineWise);

            self._adjust_all(adjs);
            self.set_cursor(member, cursor);
        }

        Ok(None)
    }

    fn mark(&mut self, name: Mark, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let leader = self.get_leader(ctx.0);

        self.set_mark(name, leader);

        Ok(None)
    }

    fn cursor_command(
        &mut self,
        act: CursorAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        match act {
            CursorAction::Close(target) => self.cursor_close(&target, ctx),
            CursorAction::Split(count) => self.cursor_split(count, ctx),
        }
    }

    fn history_command(
        &mut self,
        act: HistoryAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        match act {
            HistoryAction::Checkpoint => self.checkpoint(),
            HistoryAction::Undo(count) => self.undo(count, ctx),
            HistoryAction::Redo(count) => self.redo(count, ctx),
        }
    }
}

impl<'a, 'b, C, P> Editable<CursorGroupIdContext<'a, 'b, C>> for SharedBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn edit(
        &mut self,
        operation: &EditAction,
        motion: &EditTarget,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        self.try_write().unwrap().edit(operation, motion, ctx)
    }

    fn type_char(&mut self, ch: Char, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        self.try_write().unwrap().type_char(ch, ctx)
    }

    fn selcursor_set(
        &mut self,
        change: &SelectionCursorChange,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        self.try_write().unwrap().selcursor_set(change, ctx)
    }

    fn selection_split_lines(
        &mut self,
        filter: TargetShapeFilter,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        self.try_write().unwrap().selection_split_lines(filter, ctx)
    }

    fn paste(
        &mut self,
        dir: MoveDir1D,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        self.try_write().unwrap().paste(dir, count, ctx)
    }

    fn open_line(&mut self, dir: MoveDir1D, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        self.try_write().unwrap().open_line(dir, ctx)
    }

    fn mark(&mut self, name: Mark, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        self.try_write().unwrap().mark(name, ctx)
    }

    fn cursor_command(
        &mut self,
        act: CursorAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        self.try_write().unwrap().cursor_command(act, ctx)
    }

    fn history_command(
        &mut self,
        act: HistoryAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        self.try_write().unwrap().history_command(act, ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::base::TargetShape::{BlockWise, CharWise, LineWise};
    use crate::editing::base::{MovePosition, MoveType, RangeType, Specifier, WordStyle};
    use crate::editing::store::Store;
    use crate::vim::VimContext;

    macro_rules! mv {
        ($mt: expr) => {
            EditTarget::Motion($mt.clone(), Count::Contextual)
        };
        ($mt: expr, $c: expr) => {
            EditTarget::Motion($mt.clone(), Count::Exact($c))
        };
    }

    macro_rules! range {
        ($rt: expr) => {
            EditTarget::Range($rt, Count::Contextual)
        };
        ($rt: expr, $c: expr) => {
            EditTarget::Range($rt, Count::Exact($c))
        };
    }

    macro_rules! ctx {
        ($curid: expr, $vwctx: expr, $vctx: expr) => {
            &($curid, &$vwctx, &$vctx)
        };
    }

    macro_rules! cell {
        ($shape: expr, $str: expr) => {
            RegisterCell::new($shape, EditRope::from($str))
        };
    }

    macro_rules! edit {
        ($ebuf: expr, $act: expr, $target: expr, $ctx: expr) => {
            $ebuf.edit(&$act, &$target, $ctx).unwrap()
        };
    }

    macro_rules! paste {
        ($ebuf: expr, $dir: expr, $c: expr, $ctx: expr) => {
            $ebuf.paste($dir, $c, $ctx).unwrap()
        };
    }

    macro_rules! mark {
        ($c: expr) => {
            Mark::BufferNamed($c)
        };
    }

    macro_rules! get_mark {
        ($ebuf: expr, $c: expr) => {
            $ebuf.get_mark(mark!($c))
        };
    }

    macro_rules! assert_mark {
        ($ebuf: expr, $c: expr, $cursor: expr) => {
            assert_eq!(get_mark!($ebuf, $c).unwrap(), $cursor)
        };
    }

    macro_rules! edit_char_mark {
        ($ebuf: expr, $act: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr) => {
            edit!(
                $ebuf,
                $act,
                EditTarget::CharJump(Specifier::Exact(mark!($c))),
                ctx!($curid, $vwctx, $vctx)
            )
        };
    }
    macro_rules! edit_line_mark {
        ($ebuf: expr, $act: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr) => {
            edit!(
                $ebuf,
                $act,
                EditTarget::LineJump(Specifier::Exact(mark!($c))),
                ctx!($curid, $vwctx, $vctx)
            )
        };
    }

    macro_rules! type_char {
        ($ebuf: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr) => {
            $ebuf
                .type_char(Char::Single($c).into(), ctx!($curid, $vwctx, $vctx))
                .unwrap()
        };
    }

    macro_rules! type_digraph {
        ($ebuf: expr, $d1: expr, $d2: expr, $curid: expr, $vwctx: expr, $vctx: expr) => {
            $ebuf
                .type_char(Char::Digraph($d1, $d2).into(), ctx!($curid, $vwctx, $vctx))
                .unwrap()
        };
    }

    macro_rules! type_copy_line {
        ($ebuf: expr, $dir: expr, $curid: expr, $vwctx: expr, $vctx: expr) => {
            $ebuf
                .type_char(Char::CopyLine($dir).into(), ctx!($curid, $vwctx, $vctx))
                .unwrap()
        };
    }

    macro_rules! get_reg {
        ($ebuf: expr, $reg: expr) => {
            $ebuf.get_register(&Some($reg))
        };
    }

    macro_rules! get_named_reg {
        ($ebuf: expr, $reg: expr) => {
            get_reg!($ebuf, Register::Named($reg))
        };
    }

    macro_rules! get_recent_del_reg {
        ($ebuf: expr, $n: expr) => {
            get_reg!($ebuf, Register::RecentlyDeleted($n))
        };
    }

    macro_rules! set_reg {
        ($ebuf: expr, $reg: expr, $shape: expr, $txt: expr) => {
            $ebuf.set_register(&Some($reg), cell!($shape, $txt), false, false);
        };
    }

    macro_rules! set_named_reg {
        ($ebuf: expr, $reg: expr, $shape: expr, $txt: expr) => {
            set_reg!($ebuf, Register::Named($reg), $shape, $txt);
        };
    }

    fn mkbuf() -> EditBuffer<VimContext, ()> {
        EditBuffer::new(BufferId(0), Store::new())
    }

    fn mkbufstr(s: &str) -> EditBuffer<VimContext, ()> {
        let mut buf = mkbuf();
        buf.set_text(s);
        buf.checkpoint().unwrap();
        return buf;
    }

    #[test]
    fn test_marks() {
        let mut ebuf = mkbufstr(
            "12345\n\
            67890\n\
            abcde\n\
            fghij\n\
            klmno\n\
            pqrst\n\
            uvwxy\n",
        );
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Set up a bunch of marks to check.
        ebuf.set_leader(curid, Cursor::new(0, 4));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(1, 3));
        ebuf.mark(mark!('b'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(2, 1));
        ebuf.mark(mark!('c'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(2, 4));
        ebuf.mark(mark!('d'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(4, 1));
        ebuf.mark(mark!('e'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(4, 3));
        ebuf.mark(mark!('f'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(5, 3));
        ebuf.mark(mark!('g'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(5, 2));
        ebuf.mark(mark!('h'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(6, 0));
        ebuf.mark(mark!('i'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(6, 4));
        ebuf.mark(mark!('j'), ctx!(curid, vwctx, vctx)).unwrap();

        // Move to the third line, so we can verify that earlier marks go untouched.
        ebuf.set_leader(curid, Cursor::new(2, 3));

        // Test that typing 'q' moves 'd right.
        type_char!(ebuf, 'q', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcqde\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 4));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(2, 5));
        assert_mark!(ebuf, 'e', Cursor::new(4, 1));
        assert_mark!(ebuf, 'f', Cursor::new(4, 3));
        assert_mark!(ebuf, 'g', Cursor::new(5, 3));
        assert_mark!(ebuf, 'h', Cursor::new(5, 2));
        assert_mark!(ebuf, 'i', Cursor::new(6, 0));
        assert_mark!(ebuf, 'j', Cursor::new(6, 4));

        // Test that typing '\n' moves the marks down, and adjusts the column for 'd.
        type_char!(ebuf, '\n', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nde\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(3, 1));
        assert_mark!(ebuf, 'e', Cursor::new(5, 1));
        assert_mark!(ebuf, 'f', Cursor::new(5, 3));
        assert_mark!(ebuf, 'g', Cursor::new(6, 3));
        assert_mark!(ebuf, 'h', Cursor::new(6, 2));
        assert_mark!(ebuf, 'i', Cursor::new(7, 0));
        assert_mark!(ebuf, 'j', Cursor::new(7, 4));

        // Test that pasting a word adjusts the column for 'd.
        set_named_reg!(ebuf, 's', CharWise, "hello ");
        vctx.action.register = Some(Register::Named('s'));
        paste!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(curid, vwctx, vctx));
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nhello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 11));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(3, 13));
        assert_mark!(ebuf, 'e', Cursor::new(5, 1));
        assert_mark!(ebuf, 'f', Cursor::new(5, 3));
        assert_mark!(ebuf, 'g', Cursor::new(6, 3));
        assert_mark!(ebuf, 'h', Cursor::new(6, 2));
        assert_mark!(ebuf, 'i', Cursor::new(7, 0));
        assert_mark!(ebuf, 'j', Cursor::new(7, 4));

        // Test that pasting a line adjusts columns.
        set_named_reg!(ebuf, 's', LineWise, "foo\nbar\n");
        vctx.action.register = Some(Register::Named('s'));
        paste!(ebuf, MoveDir1D::Previous, Count::Exact(3), ctx!(curid, vwctx, vctx));
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nfoo\nbar\nfoo\nbar\nfoo\nbar\n\
            hello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(9, 13));
        assert_mark!(ebuf, 'e', Cursor::new(11, 1));
        assert_mark!(ebuf, 'f', Cursor::new(11, 3));
        assert_mark!(ebuf, 'g', Cursor::new(12, 3));
        assert_mark!(ebuf, 'h', Cursor::new(12, 2));
        assert_mark!(ebuf, 'i', Cursor::new(13, 0));
        assert_mark!(ebuf, 'j', Cursor::new(13, 4));

        set_named_reg!(ebuf, 's', LineWise, "baz\n");
        vctx.action.register = Some(Register::Named('s'));
        paste!(ebuf, MoveDir1D::Next, Count::Exact(1), ctx!(curid, vwctx, vctx));
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nfoo\nbaz\nbar\nfoo\nbar\nfoo\nbar\n\
            hello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(10, 13));
        assert_mark!(ebuf, 'e', Cursor::new(12, 1));
        assert_mark!(ebuf, 'f', Cursor::new(12, 3));
        assert_mark!(ebuf, 'g', Cursor::new(13, 3));
        assert_mark!(ebuf, 'h', Cursor::new(13, 2));
        assert_mark!(ebuf, 'i', Cursor::new(14, 0));
        assert_mark!(ebuf, 'j', Cursor::new(14, 4));

        // Delete the pasted lines.
        edit!(ebuf, EditAction::Delete, range!(RangeType::Line, 6), ctx!(curid, vwctx, vctx));
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nfoo\nhello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(4, 13));
        assert_mark!(ebuf, 'e', Cursor::new(6, 1));
        assert_mark!(ebuf, 'f', Cursor::new(6, 3));
        assert_mark!(ebuf, 'g', Cursor::new(7, 3));
        assert_mark!(ebuf, 'h', Cursor::new(7, 2));
        assert_mark!(ebuf, 'i', Cursor::new(8, 0));
        assert_mark!(ebuf, 'j', Cursor::new(8, 4));

        // Delete the pasted words.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Delete, mv!(mov, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\nde\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(4, 1));
        assert_mark!(ebuf, 'e', Cursor::new(6, 1));
        assert_mark!(ebuf, 'f', Cursor::new(6, 3));
        assert_mark!(ebuf, 'g', Cursor::new(7, 3));
        assert_mark!(ebuf, 'h', Cursor::new(7, 2));
        assert_mark!(ebuf, 'i', Cursor::new(8, 0));
        assert_mark!(ebuf, 'j', Cursor::new(8, 4));

        // Delete the word containing 'd, sending it to column 0.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\n\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(4, 0));
        assert_mark!(ebuf, 'e', Cursor::new(6, 1));
        assert_mark!(ebuf, 'f', Cursor::new(6, 3));
        assert_mark!(ebuf, 'g', Cursor::new(7, 3));
        assert_mark!(ebuf, 'h', Cursor::new(7, 2));
        assert_mark!(ebuf, 'i', Cursor::new(8, 0));
        assert_mark!(ebuf, 'j', Cursor::new(8, 4));

        // Delete the lines containing marks 'd, 'e and 'f, sending them to (0, 0).
        edit!(ebuf, EditAction::Delete, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 0));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(0, 0));
        assert_mark!(ebuf, 'e', Cursor::new(0, 0));
        assert_mark!(ebuf, 'f', Cursor::new(0, 0));
        assert_mark!(ebuf, 'g', Cursor::new(4, 3));
        assert_mark!(ebuf, 'h', Cursor::new(4, 2));
        assert_mark!(ebuf, 'i', Cursor::new(5, 0));
        assert_mark!(ebuf, 'j', Cursor::new(5, 4));

        // Do a blockwise paste and check that columns get adjusted.
        set_named_reg!(ebuf, 's', BlockWise, "foo\nbar");
        vctx.action.register = Some(Register::Named('s'));
        paste!(ebuf, MoveDir1D::Next, Count::Exact(1), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\npfooqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 1));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 3));
        assert_mark!(ebuf, 'c', Cursor::new(2, 1));
        assert_mark!(ebuf, 'd', Cursor::new(0, 0));
        assert_mark!(ebuf, 'e', Cursor::new(0, 0));
        assert_mark!(ebuf, 'f', Cursor::new(0, 0));
        assert_mark!(ebuf, 'g', Cursor::new(4, 6));
        assert_mark!(ebuf, 'h', Cursor::new(4, 5));
        assert_mark!(ebuf, 'i', Cursor::new(5, 0));
        assert_mark!(ebuf, 'j', Cursor::new(5, 7));

        // Test that marks get adjusted after blockwise deletes.
        let target = EditTarget::CharJump(Specifier::Exact(mark!('b')));
        vctx.persist.shape = Some(TargetShape::BlockWise);
        edit!(ebuf, EditAction::Delete, target, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "12345\n60\na\nf\npqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 1));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(1, 0));
        assert_mark!(ebuf, 'c', Cursor::new(2, 0));
        assert_mark!(ebuf, 'd', Cursor::new(0, 0));
        assert_mark!(ebuf, 'e', Cursor::new(0, 0));
        assert_mark!(ebuf, 'f', Cursor::new(0, 0));
        assert_mark!(ebuf, 'g', Cursor::new(4, 3));
        assert_mark!(ebuf, 'h', Cursor::new(4, 2));
        assert_mark!(ebuf, 'i', Cursor::new(5, 0));
        assert_mark!(ebuf, 'j', Cursor::new(5, 7));

        // Move to first line and test joining lines.
        let mov = MoveType::BufferLineOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 1), ctx!(curid, vwctx, vctx));

        let operation = EditAction::Join(true);
        edit!(ebuf, operation, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "12345 60 a\nf\npqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 8));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(0, 6));
        assert_mark!(ebuf, 'c', Cursor::new(0, 9));
        assert_mark!(ebuf, 'd', Cursor::new(0, 0));
        assert_mark!(ebuf, 'e', Cursor::new(0, 0));
        assert_mark!(ebuf, 'f', Cursor::new(0, 0));
        assert_mark!(ebuf, 'g', Cursor::new(2, 3));
        assert_mark!(ebuf, 'h', Cursor::new(2, 2));
        assert_mark!(ebuf, 'i', Cursor::new(3, 0));
        assert_mark!(ebuf, 'j', Cursor::new(3, 7));

        let operation = EditAction::Join(false);
        edit!(ebuf, operation, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "12345 60 afpqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));
        assert_mark!(ebuf, 'a', Cursor::new(0, 4));
        assert_mark!(ebuf, 'b', Cursor::new(0, 6));
        assert_mark!(ebuf, 'c', Cursor::new(0, 9));
        assert_mark!(ebuf, 'd', Cursor::new(0, 0));
        assert_mark!(ebuf, 'e', Cursor::new(0, 0));
        assert_mark!(ebuf, 'f', Cursor::new(0, 0));
        assert_mark!(ebuf, 'g', Cursor::new(0, 14));
        assert_mark!(ebuf, 'h', Cursor::new(0, 13));
        assert_mark!(ebuf, 'i', Cursor::new(1, 0));
        assert_mark!(ebuf, 'j', Cursor::new(1, 7));
    }

    #[test]
    fn test_motion_mark_jump() {
        let mut ebuf = mkbufstr("12345\n   67890\nabcde\nfghij\n klmno\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        // Set up a bunch of marks to check.
        ebuf.set_leader(curid, Cursor::new(0, 4));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(1, 6));
        ebuf.mark(mark!('b'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(2, 1));
        ebuf.mark(mark!('c'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(2, 5));
        ebuf.mark(mark!('d'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(3, 2));
        ebuf.mark(mark!('e'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(4, 4));
        ebuf.mark(mark!('f'), ctx!(curid, vwctx, vctx)).unwrap();

        let op = EditAction::Motion;

        // Move to the top left to begin.
        ebuf.set_leader(curid, Cursor::new(0, 0));

        // Using LineJump always goes to the first word.
        edit_line_mark!(ebuf, op, 'a', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        edit_line_mark!(ebuf, op, 'b', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));
        edit_line_mark!(ebuf, op, 'c', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));
        edit_line_mark!(ebuf, op, 'd', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));
        edit_line_mark!(ebuf, op, 'e', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 0));
        edit_line_mark!(ebuf, op, 'f', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 1));

        // Using CharJump goes to the marked column.
        edit_char_mark!(ebuf, op, 'a', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        edit_char_mark!(ebuf, op, 'b', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));
        edit_char_mark!(ebuf, op, 'c', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 1));
        edit_char_mark!(ebuf, op, 'd', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 5));
        edit_char_mark!(ebuf, op, 'e', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 2));
        edit_char_mark!(ebuf, op, 'f', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(4, 4));
    }

    #[test]
    fn test_typing_insert_char() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        type_char!(ebuf, 'h', curid, vwctx, vctx);
        type_char!(ebuf, 'e', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        type_char!(ebuf, ' ', curid, vwctx, vctx);
        type_char!(ebuf, 'w', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);
        type_char!(ebuf, 'r', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'd', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));

        type_char!(ebuf, '\n', curid, vwctx, vctx);
        type_char!(ebuf, '1', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello world\n1\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 1));
    }

    #[test]
    fn test_typing_insert_digraph() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        type_digraph!(ebuf, '>', '>', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "\u{00BB}\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 1));

        type_digraph!(ebuf, '<', '<', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "\u{00BB}\u{00AB}\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
    }

    #[test]
    fn test_typing_insert_copy_line() {
        let mut ebuf = mkbufstr("abc\n_\n1234\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        let above = MoveDir1D::Previous;
        let below = MoveDir1D::Next;

        // Set cursor to (1, 1).
        ebuf.set_leader(curid, Cursor::new(1, 1));

        // Copy character above cursor ("^Y").
        type_copy_line!(ebuf, above, curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));
        assert_eq!(ebuf.get_text(), "abc\n_b\n1234\n");

        // Copy character below cursor ("^E").
        type_copy_line!(ebuf, below, curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));
        assert_eq!(ebuf.get_text(), "abc\n_b3\n1234\n");

        // There are no more characters above the cursor to copy ("^Y").
        let res = ebuf.type_char(Char::CopyLine(above), ctx!(curid, vwctx, vctx));
        assert!(res.is_err());
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));
        assert_eq!(ebuf.get_text(), "abc\n_b3\n1234\n");

        // There is still a character below though ("^E").
        type_copy_line!(ebuf, below, curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 4));
        assert_eq!(ebuf.get_text(), "abc\n_b34\n1234\n");

        // And now there's nothing below to copy ("^E").
        let res = ebuf.type_char(Char::CopyLine(below), ctx!(curid, vwctx, vctx));
        assert!(res.is_err());
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 4));
        assert_eq!(ebuf.get_text(), "abc\n_b34\n1234\n");
    }

    #[test]
    fn test_typing_replace() {
        let mut ebuf = mkbufstr("hello");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        vctx.persist.insert = Some(InsertStyle::Replace);

        type_char!(ebuf, 'c', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "cello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 1));

        type_char!(ebuf, 'a', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "callo\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));

        type_char!(ebuf, 'l', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "callo\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        type_char!(ebuf, 'y', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "calyo\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        type_char!(ebuf, 'x', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "calyx\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
    }

    #[test]
    fn test_replace() {
        let mut ebuf = mkbufstr("hello world\na b c d e\nfoo bar baz");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // 3r!
        let mov = MoveType::Column(MoveDir1D::Next, false);
        vctx.action.replace = Some('!'.into());
        edit!(ebuf, EditAction::Replace(false), mv!(mov, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "!!!lo world\na b c d e\nfoo bar baz\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));

        // replace three words ("!", "lo", "world") w/ "Q"
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        vctx.action.replace = Some('Q'.into());
        edit!(ebuf, EditAction::Replace(false), mv!(mov, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "!!QQQQQQQQQ\na b c d e\nfoo bar baz\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));

        // replace two lines w/ ":", leaving newlines intact.
        let mov = RangeType::Line;
        vctx.action.replace = Some(':'.into());
        edit!(ebuf, EditAction::Replace(false), range!(mov, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), ":::::::::::\n:::::::::\nfoo bar baz\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 8));
    }

    #[test]
    fn test_yank() {
        let mut ebuf = mkbufstr("hello world\na b c d e\nfoo bar baz");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);

        // Move forward to "world"
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        vctx.action.operation = EditAction::Yank;

        // Test that we use the unnamed register ("") by default.
        edit!(ebuf, EditAction::Yank, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Both "" and "0 should now be updated.
        assert_eq!(get_reg!(ebuf, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(CharWise, "world"));

        // Test using the named 'a' register ("a).
        vctx.action.count = Some(3);
        vctx.action.register = Some(Register::Named('a'));
        edit!(ebuf, EditAction::Yank, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Both "" and "a should now be updated, and "0 untouched.
        assert_eq!(get_reg!(ebuf, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(CharWise, "world\na b "));
        assert_eq!(get_named_reg!(ebuf, 'a'), cell!(CharWise, "world\na b "));

        // Append a line to the 'a' register ("A).
        vctx.action.count = None;
        vctx.action.register = Some(Register::Named('a'));
        vctx.action.register_append = true;
        edit!(ebuf, EditAction::Yank, range!(RangeType::Line), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Both "" and "a should contain appended text, and "0 be untouched.
        assert_eq!(get_reg!(ebuf, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(
            get_reg!(ebuf, Register::Unnamed),
            cell!(LineWise, "world\na b \nhello world\n")
        );
        assert_eq!(get_named_reg!(ebuf, 'a'), cell!(LineWise, "world\na b \nhello world\n"));

        // The blackhole register ("_) discards the yanked text.
        vctx.action.count = None;
        vctx.action.register = Some(Register::Blackhole);
        vctx.action.register_append = false;
        edit!(ebuf, EditAction::Yank, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // All registers should be untouched, and "_ should not return the word "world".
        assert_eq!(get_reg!(ebuf, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(
            get_reg!(ebuf, Register::Unnamed),
            cell!(LineWise, "world\na b \nhello world\n")
        );
        assert_eq!(get_named_reg!(ebuf, 'a'), cell!(LineWise, "world\na b \nhello world\n"));
        assert_eq!(get_reg!(ebuf, Register::Blackhole), cell!(CharWise, ""));
    }

    #[test]
    fn test_paste() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::CharWise, "hello");
        set_named_reg!(ebuf, 'b', TargetShape::CharWise, " world");
        set_named_reg!(ebuf, 'c', TargetShape::LineWise, "foo bar\n");
        set_named_reg!(ebuf, 'd', TargetShape::LineWise, "three\nregister\nlines\n");
        set_named_reg!(ebuf, 'e', TargetShape::BlockWise, "abcde\n12345");
        set_named_reg!(ebuf, 'f', TargetShape::BlockWise, "1\n2\n3\n4\n5\n6\n7");

        // Start with an empty buffer
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // place "a ("hello") into the buffer
        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // place "b (" world") into the buffer
        vctx.action.register = Some(Register::Named('b'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));

        // place "c ("foo bar\n") on the line below
        vctx.action.register = Some(Register::Named('c'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nfoo bar\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // place "d ("three\nregister\nlines\n") on the line above
        vctx.action.register = Some(Register::Named('d'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // place "c ("foo bar\n") on the line below, breaking up the "d text.
        vctx.action.register = Some(Register::Named('c'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\nfoo bar\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));

        // place "e ("abcde\n12345") twice before the next several lines.
        vctx.action.register = Some(Register::Named('e'));
        paste!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(curid, vwctx, vctx));
        assert_eq!(
            ebuf.get_text(),
            "hello world\nthree\nabcdeabcdefoo bar\n1234512345register\nlines\nfoo bar\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));

        // place "f ("1\n2\n3\n4\n5\n6\n7") on the next several lines, adding new lines as needed.
        vctx.action.register = Some(Register::Named('f'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar\n12234512345register\nl3ines\nf4oo bar\n5\n6\n7\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 1));

        // Move to the end of the line, and repeat pasting "f.
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Motion, mv!(mov, 0), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 17));

        vctx.action.register = Some(Register::Named('f'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar1\n12234512345registe2r\nl3ines3\nf4oo bar4\n55\n66\n77\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 18));
    }

    #[test]
    fn test_paste_empty_charwise_next() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::CharWise, "hello");

        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
    }

    #[test]
    fn test_paste_empty_linewise_next() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::LineWise, "hello\n");

        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "\nhello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_paste_empty_blockwise_next() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::BlockWise, "hello\nworld");

        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\nworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
    }

    #[test]
    fn test_paste_insert() {
        let mut ebuf = mkbufstr("hello world\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::LineWise, "foo\n");
        set_named_reg!(ebuf, 'b', TargetShape::BlockWise, "a\nb\nc");

        vctx.persist.insert = Some(InsertStyle::Insert);

        // Start out at (0, 6).
        ebuf.set_leader(curid, Cursor::new(0, 6));

        // place "a ("foo\n") into the buffer as if it were CharWise.
        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello foo\nworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // place "b ("a\nb\nc") into the buffer as if it were CharWise.
        vctx.action.register = Some(Register::Named('b'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello foo\na\nb\ncworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 1));
    }

    #[test]
    fn test_paste_repeat() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::CharWise, "hello");
        set_named_reg!(ebuf, 'b', TargetShape::LineWise, "1 2 3\n");
        set_named_reg!(ebuf, 'c', TargetShape::BlockWise, "a\nb\nc");

        // Paste "hello" from "a 5 times.
        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Exact(5), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 24));

        // Paste "1 2 3\n" from "b 2 times.
        vctx.action.register = Some(Register::Named('b'));
        paste!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "1 2 3\n1 2 3\nhellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Paste "a\nb\nc" from "c 4 times.
        vctx.action.register = Some(Register::Named('c'));
        paste!(ebuf, MoveDir1D::Next, Count::Exact(4), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "1aaaa 2 3\n1bbbb 2 3\nhccccellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 1));
    }

    #[test]
    fn test_delete() {
        let mut ebuf = mkbufstr("hello world\na b c d e f\n\n\n1 2 3 4 5 6\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        // Test deleting a word.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "world\na b c d e f\n\n\n1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(get_reg!(ebuf, Register::LastYanked), RegisterCell::default());

        // Less than a line was deleted, so this goes into "-, not "1.
        assert_eq!(get_reg!(ebuf, Register::SmallDelete), cell!(CharWise, "hello "));
        assert_eq!(get_recent_del_reg!(ebuf, 0), RegisterCell::default());

        // Test that deleting multiple words crosses lines.
        edit!(ebuf, EditAction::Delete, mv!(mov, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "c d e f\n\n\n1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // More than a line was deleted, so this goes into "1 and "- is untouched.
        assert_eq!(get_reg!(ebuf, Register::SmallDelete), cell!(CharWise, "hello "));
        assert_eq!(get_recent_del_reg!(ebuf, 0), cell!(CharWise, "world\na b "));

        // Test that the behaviour changes if the last word is at the end of a line.
        edit!(ebuf, EditAction::Delete, mv!(mov, 4), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "\n\n\n1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Less than a line was deleted, so this goes into "- and "1 is untouched.
        assert_eq!(get_reg!(ebuf, Register::SmallDelete), cell!(CharWise, "c d e f"));
        assert_eq!(get_recent_del_reg!(ebuf, 0), cell!(CharWise, "world\na b "));

        // Test deleting blank lines.
        edit!(ebuf, EditAction::Delete, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // More than a line was deleted, so "1 shifts to "2, this goes into "1 and "- is untouched.
        assert_eq!(get_reg!(ebuf, Register::SmallDelete), cell!(CharWise, "c d e f"));
        assert_eq!(get_recent_del_reg!(ebuf, 0), cell!(LineWise, "\n\n\n"));
        assert_eq!(get_recent_del_reg!(ebuf, 1), cell!(CharWise, "world\na b "));

        // Move forward two words and delete text in middle of line.
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Test deleting in middle of string.
        edit!(ebuf, EditAction::Delete, mv!(mov, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "1 2 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Less than a line was deleted, so "- is updated, other registers remain the same.
        assert_eq!(get_reg!(ebuf, Register::SmallDelete), cell!(CharWise, "3 4 "));
        assert_eq!(get_recent_del_reg!(ebuf, 0), cell!(LineWise, "\n\n\n"));
        assert_eq!(get_recent_del_reg!(ebuf, 1), cell!(CharWise, "world\na b "));

        // Test that deleting more lines than exists deletes whole string.
        edit!(ebuf, EditAction::Delete, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // "1 and "2 get shifted, "0 set, "- untouched.
        assert_eq!(get_reg!(ebuf, Register::SmallDelete), cell!(CharWise, "3 4 "));
        assert_eq!(get_recent_del_reg!(ebuf, 0), cell!(LineWise, "1 2 5 6\n"));
        assert_eq!(get_recent_del_reg!(ebuf, 1), cell!(LineWise, "\n\n\n"));
        assert_eq!(get_recent_del_reg!(ebuf, 2), cell!(CharWise, "world\na b "));
    }

    #[test]
    fn test_delete_blockwise() {
        let mut ebuf = mkbufstr("hello world\n1 2 3 4 5 6\n  a b c d e f\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Set cursor to (0, 7).
        ebuf.set_leader(curid, Cursor::new(0, 7));

        // Do a blockwise delete from here to the first word of the third line.
        vctx.persist.shape = Some(TargetShape::BlockWise);
        edit!(ebuf, EditAction::Delete, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "herld\n1 5 6\n  d e f\n");

        // Check that the deleted text went into "" and "1. "0 and "- should be untouched.
        assert_eq!(get_recent_del_reg!(ebuf, 0), cell!(BlockWise, "llo wo\n2 3 4 \na b c "));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(BlockWise, "llo wo\n2 3 4 \na b c "));
        assert_eq!(get_reg!(ebuf, Register::LastYanked), cell!(CharWise, ""));
        assert_eq!(get_reg!(ebuf, Register::SmallDelete), cell!(CharWise, ""));
    }

    #[test]
    fn test_delete_eol() {
        let mut ebuf = mkbufstr("hello world\na b c d e f\n\n\n1 2 3 4 5 6\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        // Set cursor to (0, 3).
        ebuf.set_leader(curid, Cursor::new(0, 3));

        // Delete from cursor to the end of the line ("d$").
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Delete, mv!(mov, 0), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(ebuf.get_text(), "hel\na b c d e f\n\n\n1 2 3 4 5 6\n");
    }

    #[test]
    fn test_change() {
        let mut ebuf = mkbufstr("hello world\na b c d e f\n\n\n1 2 3 4 5 6\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        vctx.persist.insert = Some(InsertStyle::Insert);

        // Start out at (0, 3).
        ebuf.set_leader(curid, Cursor::new(0, 3));

        // Delete from cursor to the end of the line ("c$").
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Delete, mv!(mov, 0), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));
        assert_eq!(ebuf.get_text(), "hel\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Delete previous character ("<BS>").
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(ebuf.get_text(), "he\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Delete previous word ("^W").
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(ebuf.get_text(), "\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Delete next character ("<Del>").
        let mov = MoveType::Column(MoveDir1D::Next, true);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(ebuf.get_text(), "a b c d e f\n\n\n1 2 3 4 5 6\n");

        // Move to (0, 3).
        ebuf.set_leader(curid, Cursor::new(3, 0));

        // Delete previous newline character ("<BS>").
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));
        assert_eq!(ebuf.get_text(), "a b c d e f\n\n1 2 3 4 5 6\n");

        // Delete two previous newline characters ("<BS>").
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));
        assert_eq!(ebuf.get_text(), "a b c d e f1 2 3 4 5 6\n");
    }

    #[test]
    fn test_changecase() {
        let mut ebuf = mkbufstr("thiS iS An eXaMpLE of mIxed cASE\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);

        // Test Case::Toggle operations
        let operation = EditAction::ChangeCase(Case::Toggle);

        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "THIs iS An eXaMpLE of mIxed cASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Running a second time toggles again
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "thiS Is aN ExAmPle OF MiXED Case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Test Case::Upper operations
        let operation = EditAction::ChangeCase(Case::Upper);

        // Make first word uppercase
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "THIS Is aN ExAmPle OF MiXED Case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Uppercasing is idempotent
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "THIS Is aN ExAmPle OF MiXED Case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Make whole line uppercase
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "THIS IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Uppercasing is idempotent
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "THIS IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Test Case::Lower operations
        let operation = EditAction::ChangeCase(Case::Lower);

        // Make first word lowercase
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "this IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Operation is idempotent
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "this IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Make whole line lowercase
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "this is an example of mixed case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Operation is idempotent
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "this is an example of mixed case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // XXX: cursor should move to first word after g~~/gUU/guu
    }

    #[test]
    fn test_search_char_inclusive_forwards() {
        let mut ebuf = mkbufstr("a b c a b c 1 2 3 a b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Move in the same search direction as the last search (";").
        let same = EditTarget::Search(SearchType::Char(false), MoveDirMod::Same, Count::Contextual);

        // Move in the opposite search direction as the last search (",").
        let flip = EditTarget::Search(SearchType::Char(false), MoveDirMod::Flip, Count::Contextual);

        // Set cursor to (0, 4), after the first "a".
        ebuf.set_leader(curid, Cursor::new(0, 4));
        vctx.persist.charsearch_params = (MoveDir1D::Next, true);
        vctx.persist.charsearch = Some('a'.into());

        // Delete from cursor to the second "a" ("d2fa").
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, same, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_text(), "a b  b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");

        // Trying to delete to a third "a" should do nothing, since it hits the line ending ("d3;").
        vctx.action.count = Some(3);
        edit!(ebuf, EditAction::Delete, same, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_text(), "a b  b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");

        // Using SearchType::Char(true) allows searching onto the next line, like kakoune does.
        let target =
            EditTarget::Search(SearchType::Char(true), MoveDirMod::Same, Count::Contextual);
        vctx.action.count = Some(3);
        edit!(ebuf, EditAction::Delete, target, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_text(), "a b  b c 1 2 3\n");

        // Delete to the previous occurrence of "a" ("d,").
        vctx.action.count = Some(1);
        edit!(ebuf, EditAction::Delete, flip, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(ebuf.get_text(), " b c 1 2 3\n");
    }

    #[test]
    fn test_search_char_inclusive_backwards() {
        let mut ebuf = mkbufstr("a b c a b c 1 2 3 a b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Move in the same search direction as the last search (";").
        let same = EditTarget::Search(SearchType::Char(false), MoveDirMod::Same, Count::Contextual);

        // Move in the opposite search direction as the last search (",").
        let flip = EditTarget::Search(SearchType::Char(false), MoveDirMod::Flip, Count::Contextual);

        // Set cursor to (1, 4), after the first "b" on the line.
        ebuf.set_leader(curid, Cursor::new(1, 4));
        vctx.persist.charsearch_params = (MoveDir1D::Previous, true);
        vctx.persist.charsearch = Some('b'.into());

        // Trying to delete multiple b's with multiline = false fails ("2dFb").
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, same, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 4));
        assert_eq!(
            ebuf.get_text(),
            "a b c a b c 1 2 3 a b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n"
        );

        // Setting multiline = true allows us to delete across the line boundary.
        let target =
            EditTarget::Search(SearchType::Char(true), MoveDirMod::Same, Count::Contextual);
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, target, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 20));
        assert_eq!(ebuf.get_text(), "a b c a b c 1 2 3 a c a b c 1 2 3 a b c 1 2 3\n");

        // Delete backwards for one 'b' ("dFb").
        vctx.action.count = None;
        edit!(ebuf, EditAction::Delete, same, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 8));
        assert_eq!(ebuf.get_text(), "a b c a c a b c 1 2 3 a b c 1 2 3\n");

        // Delete twice in the flipped direction ("2d,").
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, flip, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 8));
        assert_eq!(ebuf.get_text(), "a b c a  c 1 2 3\n");
    }

    #[test]
    fn test_forced_motion_char() {
        let mut ebuf = mkbufstr("hello\nworld\na b c d e\n    word\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        let op = EditAction::Yank;
        vctx.persist.shape = Some(TargetShape::CharWise);

        // Move to (0, 2) to begin.
        ebuf.set_leader(curid, Cursor::new(0, 2));

        // Forced linewise into charwise motion (2yvj)
        let mov = MoveType::Line(MoveDir1D::Next);
        edit!(ebuf, op, mv!(mov, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(CharWise, "llo\nworld\na "));

        // Forced linewise into charwise motion (4yvG)
        let mov = MoveType::BufferLineOffset;
        edit!(ebuf, op, mv!(mov, 4), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(
            get_reg!(ebuf, Register::Unnamed),
            cell!(CharWise, "llo\nworld\na b c d e\n    ")
        );

        // Forced linewise into charwise motion (yv'a)
        ebuf.set_leader(curid, Cursor::new(3, 6));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 2));

        edit_line_mark!(ebuf, op, 'a', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(
            get_reg!(ebuf, Register::Unnamed),
            cell!(CharWise, "llo\nworld\na b c d e\n    ")
        );
    }

    #[test]
    fn test_forced_motion_line() {
        let mut ebuf = mkbufstr("hello\nworld\na b c d e\n1 2 3 4 5 6");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        let op = EditAction::Yank;
        vctx.persist.shape = Some(TargetShape::LineWise);

        // Move to (0, 2) to begin.
        ebuf.set_leader(curid, Cursor::new(0, 2));

        // Force charwise into linewise motion (100yVl)
        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, op, mv!(mov, 100), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(LineWise, "hello\n"));

        // Force charwise into linewise motion (2yVgj)
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        edit!(ebuf, op, mv!(mov, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(LineWise, "hello\nworld\na b c d e\n"));

        // Force charwise into linewise motion (yV`a)
        ebuf.set_leader(curid, Cursor::new(1, 2));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 2));

        edit_char_mark!(ebuf, op, 'a', curid, vwctx, vctx);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(LineWise, "hello\nworld\n"));
    }

    #[test]
    fn test_forced_motion_block() {
        let mut ebuf = mkbufstr("hello\nworld\na b c d e\n1 2 3 4 5 6");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        vctx.persist.shape = Some(TargetShape::BlockWise);

        let mov = MoveType::Line(MoveDir1D::Next);

        // Forced linewise into blockwise motion ("1y<C-V>j")
        edit!(ebuf, EditAction::Yank, mv!(mov, 1), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(BlockWise, "h\nw"));

        // Forced linewise into blockwise motion ("3y<C-V>j").
        edit!(ebuf, EditAction::Yank, mv!(mov, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(BlockWise, "h\nw\na\n1"));

        // Move down and test from a position that skips chars.
        ebuf.set_leader(curid, Cursor::new(3, 6));

        let mov = MoveType::Line(MoveDir1D::Previous);

        // Force linewise into blockwise motion ("3y<C-V>k").
        edit!(ebuf, EditAction::Yank, mv!(mov, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(BlockWise, "\n\nd\n4"));

        // Mark (3, 6), and move to (0, 2) so we can make a wider block.
        ebuf.set_leader(curid, Cursor::new(3, 6));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 2));

        // Force charwise into blockwise motion ("y<C-V>`a").
        let target = EditTarget::CharJump(Specifier::Exact(mark!('a')));
        edit!(ebuf, EditAction::Yank, target, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(ebuf, Register::Unnamed), cell!(BlockWise, "llo\nrld\nb c d\n2 3 4"));

        // Mark (3, 0), and move to (0, 4) so we can make a wider block.
        ebuf.set_leader(curid, Cursor::new(3, 0));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx)).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 4));

        // Test with a bottom cursor w/ a column that comes before the top cursor's column.
        let target = EditTarget::CharJump(Specifier::Exact(mark!('a')));
        edit!(ebuf, EditAction::Yank, target, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(
            get_reg!(ebuf, Register::Unnamed),
            cell!(BlockWise, "hello\nworld\na b c\n1 2 3")
        );
    }

    #[test]
    fn test_join_spaces() {
        let mut ebuf = mkbufstr("foo\n       hello world\n  a b c\n   d e\n    first\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let operation = EditAction::Join(true);

        // Joining with a count of 1 should still do something.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 1), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo hello world\n  a b c\n   d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        // Join with a count of 2 joins the current and next line.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo hello world a b c d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 21));

        // Try to join with four following lines, hitting the end of the buffer.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 5), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo hello world a b c d e first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 25));

        // Joining when there's only one line, should do nothing.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo hello world a b c d e first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 25));
    }

    #[test]
    fn test_join_nospaces() {
        let mut ebuf = mkbufstr("foo\n       hello world\n  a b c\n   d e\n    first\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let operation = EditAction::Join(false);

        // Join with a count of 1 still joins 2 lines together.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 1), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo       hello world\n  a b c\n   d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        // Join the current line with the following two lines.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo       hello world  a b c   d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 28));

        // Try to join the next four lines, hitting the end of the buffer.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 4), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo       hello world  a b c   d e    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 34));

        // Joining when there's only one line, should do nothing.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo       hello world  a b c   d e    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 34));
    }

    #[test]
    fn test_join_blanks() {
        let mut ebuf = mkbufstr("foo\n\n\n     \n    first\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let operation = EditAction::Join(true);

        // Join with the next empty line, adding no space.
        edit!(ebuf, operation, range!(RangeType::Line, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo\n\n     \n    first\n");

        // Join with the blank line, and the all spaces line.
        edit!(ebuf, operation, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo\n    first\n");

        // Join with final line.
        edit!(ebuf, operation, range!(RangeType::Line, 2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "foo first\n");
    }

    #[test]
    fn test_history() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        vctx.persist.insert = Some(InsertStyle::Insert);

        // Create several checkpoints.
        type_char!(ebuf, 'h', curid, vwctx, vctx);
        type_char!(ebuf, 'e', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);
        ebuf.checkpoint().unwrap();

        type_char!(ebuf, ' ', curid, vwctx, vctx);
        ebuf.checkpoint().unwrap();

        type_char!(ebuf, 'w', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);
        type_char!(ebuf, 'r', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'd', curid, vwctx, vctx);
        ebuf.checkpoint().unwrap();

        // Check that starting point is correct.
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));

        // Undo twice.
        ebuf.undo(Count::Exact(2), ctx!(curid, vwctx, vctx)).unwrap();
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        // Redo once.
        ebuf.redo(Count::Exact(1), ctx!(curid, vwctx, vctx)).unwrap();
        assert_eq!(ebuf.get_text(), "hello \n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Undo five times hits beginning.
        ebuf.undo(Count::Exact(5), ctx!(curid, vwctx, vctx)).unwrap();
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Redo thrice.
        ebuf.redo(Count::Exact(3), ctx!(curid, vwctx, vctx)).unwrap();
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));

        // XXX: need to test that marks also get adjusted.
    }

    #[test]
    fn test_visual_motion() {
        let mut ebuf = mkbufstr("foo\nbar\nbaz\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Perform CharWise selection.
        vctx.persist.shape = Some(TargetShape::CharWise);

        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 1));
        assert_eq!(
            ebuf.get_leader_selection(curid),
            Some((Cursor::new(0, 0), Cursor::new(0, 1), CharWise))
        );

        let mov = MoveType::Line(MoveDir1D::Next);

        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 1));
        assert_eq!(
            ebuf.get_leader_selection(curid),
            Some((Cursor::new(0, 0), Cursor::new(1, 1), CharWise))
        );

        // Changing shape to a LineWise selection keeps anchor and cursor in place.
        vctx.persist.shape = Some(TargetShape::LineWise);

        edit!(ebuf, EditAction::Motion, EditTarget::CurrentPosition, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 1));
        assert_eq!(
            ebuf.get_leader_selection(curid),
            Some((Cursor::new(0, 0), Cursor::new(1, 1), LineWise))
        );

        // Changing shape to a BlockWise selection keeps anchor and cursor in place.
        vctx.persist.shape = Some(TargetShape::BlockWise);

        edit!(ebuf, EditAction::Motion, EditTarget::CurrentPosition, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 1));
        assert_eq!(
            ebuf.get_leader_selection(curid),
            Some((Cursor::new(0, 0), Cursor::new(1, 1), BlockWise))
        );

        // XXX: test ranges that move the anchor; anchor should only ever grow range
    }

    #[test]
    fn test_cursor_group() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        // First split the cursor into five cursors.
        ebuf.cursor_split(Count::Exact(5), ctx!(curid, vwctx, vctx)).unwrap();

        // Now type with all of them.
        type_char!(ebuf, 'h', curid, vwctx, vctx);
        type_char!(ebuf, 'e', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        type_char!(ebuf, ' ', curid, vwctx, vctx);
        type_char!(ebuf, 'w', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);
        type_char!(ebuf, 'r', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'd', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello worldhello worldhello worldhello worldhello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));

        // And insert some newlines.
        type_char!(ebuf, '\n', curid, vwctx, vctx);
        assert_eq!(
            ebuf.get_text(),
            "hello world\nhello world\nhello world\nhello world\nhello world\n\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_get_lines() {
        let mut ebuf = mkbuf();

        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_lines(), 1);

        ebuf.set_text("foo\n");
        assert_eq!(ebuf.get_text(), "foo\n");
        assert_eq!(ebuf.get_lines(), 1);

        ebuf.set_text("foo\nbar baz\n");
        assert_eq!(ebuf.get_text(), "foo\nbar baz\n");
        assert_eq!(ebuf.get_lines(), 2);
    }

    #[test]
    fn test_ensure_nl() {
        let mut ebuf = mkbuf();

        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_lines(), 1);

        ebuf.set_text("foo bar baz");
        assert_eq!(ebuf.get_text(), "foo bar baz\n");
        assert_eq!(ebuf.get_lines(), 1);

        ebuf.set_text("foo\nbar\nbaz");
        assert_eq!(ebuf.get_text(), "foo\nbar\nbaz\n");
        assert_eq!(ebuf.get_lines(), 3);

        assert_eq!(ebuf.reset_text(), "foo\nbar\nbaz\n");
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_lines(), 1);
    }
}
