//! # Text buffer
//!
//! ## Overview
//!
//! The [EditBuffer] is capable of performing many different types of operations and rich
//! movements. For example:
//!
//! - Performing edit operations like text deletion or case changes using content-aware movements
//! (e.g., by word, up to a specific character or to the start of the line, etc.)
//! - Copying and pasting text between registers and the buffer
//! - Visual selections
//! - Cursor groups
//!
//! See [Editable], [EditAction], and [EditTarget] for more on what can be done.
use std::borrow::Cow;
use std::collections::hash_map::{Entry, HashMap};
use std::collections::vec_deque::VecDeque;
use std::marker::PhantomData;
use std::ops::Range;

use regex::Regex;

use crate::util::IdGenerator;

use crate::editing::{
    action::{
        CursorAction,
        EditAction,
        EditError,
        EditInfo,
        EditResult,
        Editable,
        EditorAction,
        EditorActions,
        HistoryAction,
        InsertTextAction,
        Jumpable,
        Searchable,
        SelectionAction,
        UIResult,
    },
    application::ApplicationInfo,
    base::{
        Char,
        CompletionDisplay,
        CompletionSelection,
        CompletionType,
        Count,
        CursorMovements,
        CursorMovementsContext,
        CursorSearch,
        EditRange,
        EditTarget,
        Mark,
        MoveDir1D,
        MoveDirMod,
        MoveTerminus,
        PositionList,
        Register,
        SearchType,
        SelectionResizeStyle,
        Specifier,
        TargetShape,
        ViewportContext,
        WordStyle,
    },
    completion::{CompletionList, LineCompleter},
    context::EditContext,
    cursor::{
        block_cursors,
        Adjustable,
        Cursor,
        CursorAdjustment,
        CursorChoice,
        CursorGroup,
        CursorState,
        Selection,
        Selections,
    },
    history::HistoryList,
    lineinfo::LineInfoStore,
    rope::{CharOff, CursorContext, EditRope, LineIterator, PrivateCursorOps},
    store::{AdjustStore, DigraphStore, GlobalAdjustable, SharedBuffer, Store},
};

#[cfg(test)]
#[macro_use]
mod macros_test;

mod complete;
mod cursor;
mod edit;
mod insert_text;
mod selection;

use self::complete::*;
use self::cursor::*;
use self::edit::*;
use self::insert_text::*;
use self::selection::*;

#[cfg(feature = "intervaltree")]
use intervaltree::IntervalTree;

const BUFFER_HISTORY_LEN: usize = 100;

/// Identifier for a specific cursor group.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct CursorGroupId(u64);

#[doc(hidden)]
pub type CursorRange = EditRange<Cursor>;

/// A text buffer.
pub struct EditBuffer<I: ApplicationInfo> {
    /// A unique identifier for this buffer.
    id: I::ContentId,

    /// The current contents of the buffer.
    text: EditRope,

    /// Tracks cursor groups.
    cursors: AdjustStore<CursorGroupId, CursorGroup>,

    /// Allocates new CursorGroupIds.
    cgidgen: IdGenerator,

    /// Tracks the changelist for cursor groups within given buffers.
    changed: VecDeque<CursorGroup>,
    changed_idx: HashMap<CursorGroupId, usize>,

    /// Tracks the jumplist for cursor groups within given buffers.
    jumped: AdjustStore<CursorGroupId, HistoryList<CursorGroup>>,

    completions: HashMap<CursorGroupId, CompletionList>,
    lines: LineCompleter,

    history: HistoryList<EditRope>,
    lineinfo: LineInfoStore<usize>,

    push_next_change: bool,

    _p: PhantomData<I>,
}

trait HistoryActions<C, I>
where
    I: ApplicationInfo,
{
    fn redo(&mut self, count: &Count, ctx: &C, store: &mut Store<I>) -> EditResult<EditInfo, I>;
    fn undo(&mut self, count: &Count, ctx: &C, store: &mut Store<I>) -> EditResult<EditInfo, I>;
    fn checkpoint(&mut self, ctx: &C, store: &mut Store<I>) -> EditResult<EditInfo, I>;
}

type CursorGroupIdContext<'a, 'b, T> = (CursorGroupId, &'a ViewportContext<Cursor>, &'b T);

#[cfg(feature = "intervaltree")]
pub(crate) type HighlightInfo = IntervalTree<usize, (Cursor, Cursor, TargetShape)>;

#[cfg(feature = "intervaltree")]
pub(crate) type FollowersInfo = IntervalTree<(usize, usize), Cursor>;

impl<I> EditBuffer<I>
where
    I: ApplicationInfo,
{
    /// Create a new buffer given its initial contents as an [EditRope].
    pub fn from_rope(id: I::ContentId, mut text: EditRope) -> Self {
        text.trailing_newline();

        let cursors = AdjustStore::default();
        let history = HistoryList::new(text.clone(), 100);
        let lineinfo = LineInfoStore::new();
        let jumped = AdjustStore::new();
        let cgidgen = IdGenerator::default();

        let changed = VecDeque::new();
        let changed_idx = HashMap::new();

        EditBuffer {
            id,
            text,
            cgidgen,
            cursors,
            changed,
            changed_idx,
            jumped,
            history,
            lineinfo,
            completions: HashMap::new(),
            lines: LineCompleter::default(),
            push_next_change: true,
            _p: PhantomData,
        }
    }

    /// Create a new buffer given its initial contents.
    pub fn from_str(id: I::ContentId, text: &str) -> Self {
        EditBuffer::from_rope(id, text.into())
    }

    /// Create a new empty buffer.
    pub fn new(id: I::ContentId) -> Self {
        EditBuffer::from_str(id, "\n")
    }

    /// Get this buffer's content identifier.
    pub fn id(&self) -> I::ContentId {
        self.id.clone()
    }

    fn _char(&self, c: Char, cursor: &Cursor, digraphs: &DigraphStore) -> EditResult<char, I> {
        match c {
            Char::Single(c) => {
                return Ok(c);
            },
            Char::Digraph(d1, d2) => {
                if let Some(c) = digraphs.get((d1, d2)) {
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
                fn err<I: ApplicationInfo>() -> EditError<I> {
                    let msg = "No character above cursor".to_string();

                    return EditError::Failure(msg);
                }

                if cursor.y == 0 {
                    return Err(err());
                }

                let above = Cursor::new(cursor.y - 1, cursor.x);

                self.text.get_char_at_cursor(&above).ok_or_else(err)
            },
            Char::CopyLine(MoveDir1D::Next) => {
                fn err<I: ApplicationInfo>() -> EditError<I> {
                    let msg = "No character below cursor".to_string();

                    return EditError::Failure(msg);
                }

                let below = Cursor::new(cursor.y + 1, cursor.x);

                self.text.get_char_at_cursor(&below).ok_or_else(err)
            },
        }
    }

    fn _str(&self, c: Char, cursor: &Cursor, digraphs: &DigraphStore) -> EditResult<String, I> {
        match c {
            Char::Single(c) => {
                return Ok(c.to_string());
            },
            Char::Digraph(d1, d2) => {
                if let Some(c) = digraphs.get((d1, d2)) {
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

                if let Some(c) = self.text.get_char_at_cursor(&above) {
                    return Ok(c.to_string());
                } else {
                    return Err(err);
                }
            },
            Char::CopyLine(MoveDir1D::Next) => {
                let below = Cursor::new(cursor.y + 1, cursor.x);

                if let Some(c) = self.text.get_char_at_cursor(&below) {
                    return Ok(c.to_string());
                } else {
                    let msg = "No character below cursor".to_string();
                    let err = EditError::Failure(msg);

                    return Err(err);
                }
            },
        }
    }

    fn _charjump<C: EditContext>(
        &self,
        mark: &Specifier<Mark>,
        ctx: &CursorMovementsContext<'_, '_, '_, Cursor, C>,
        store: &mut Store<I>,
    ) -> EditResult<Cursor, I> {
        store.cursors.get_mark(self.id.clone(), ctx.context.resolve(mark))
    }

    fn _linejump<C: EditContext>(
        &self,
        mark: &Specifier<Mark>,
        ctx: &CursorMovementsContext<'_, '_, '_, Cursor, C>,
        store: &mut Store<I>,
    ) -> EditResult<Cursor, I> {
        let cursor = store.cursors.get_mark(self.id.clone(), ctx.context.resolve(mark))?;
        let cursor = self.text.first_word(&cursor, ctx);

        Ok(cursor)
    }

    fn _charsearch<C: EditContext>(
        &self,
        cursor: &Cursor,
        flip: &MoveDirMod,
        multiline: bool,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<Option<CursorRange>, I> {
        let res = match ctx.get_search_char() {
            Some((dir, inclusive, needle)) => {
                let needle = self._char(needle, cursor, &store.digraphs)?;
                let count = ctx.resolve(count);
                let dir = flip.resolve(&dir);

                self.text
                    .find_char(cursor, inclusive, dir, multiline, needle, count)
                    .map(|c| EditRange::inclusive(c.clone(), c, TargetShape::CharWise))
            },
            None => None,
        };

        Ok(res)
    }

    fn _regexsearch<C: EditContext>(
        &self,
        cursor: &Cursor,
        flip: &MoveDirMod,
        count: &Count,
        ctx: &C,
        store: &Store<I>,
    ) -> EditResult<Option<CursorRange>, I> {
        let needle = self._get_regex(ctx, store)?;

        let count = ctx.resolve(count);
        let dir = ctx.get_search_regex_dir();
        let dir = flip.resolve(&dir);

        let res = self.text.find_regex(cursor, dir, &needle, count);

        Ok(res)
    }

    fn _wordsearch<C: EditContext>(
        &mut self,
        cursor: &Cursor,
        style: &WordStyle,
        boundary: bool,
        flip: &MoveDirMod,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<Option<CursorRange>, I> {
        let mut cursor = cursor.clone();
        let count = ctx.resolve(count);
        let dir = ctx.get_search_regex_dir();
        let dir = flip.resolve(&dir);

        let word = self
            .text
            .get_cursor_word_mut(&mut cursor, style)
            .ok_or(EditError::NoCursorWord)?;
        let word = regex::escape(word.to_string().as_str());

        let needle = if boundary {
            Regex::new(format!("\\b{word}\\b").as_str())
        } else {
            Regex::new(word.as_str())
        }?;

        store.set_last_search(needle.to_string());

        let res = self.text.find_regex(&cursor, dir, &needle, count);

        Ok(res)
    }

    fn _search<C: EditContext>(
        &mut self,
        cursor: &Cursor,
        search: &SearchType,
        flip: &MoveDirMod,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<Option<CursorRange>, I> {
        match search {
            SearchType::Char(multi) => {
                return self._charsearch(cursor, flip, *multi, count, ctx, store);
            },
            SearchType::Regex => {
                return self._regexsearch(cursor, flip, count, ctx, store);
            },
            SearchType::Word(style, boundary) => {
                return self._wordsearch(cursor, style, *boundary, flip, count, ctx, store);
            },
        }
    }

    fn _get_regex<C: EditContext>(&self, ctx: &C, store: &Store<I>) -> EditResult<Regex, I> {
        if let Some(regex) = ctx.get_search_regex() {
            return Ok(regex);
        }

        let lsearch = store.registers.get(&Register::LastSearch)?.value;
        let regex = Regex::new(lsearch.to_string().as_ref())?;

        return Ok(regex);
    }

    fn _target<C: EditContext>(
        &mut self,
        state: &CursorState,
        target: &EditTarget,
        ctx: &CursorMovementsContext<'_, '_, '_, Cursor, C>,
        store: &mut Store<I>,
    ) -> EditResult<Option<CursorRange>, I> {
        let cursor = state.cursor().clone();

        match target {
            EditTarget::Boundary(range, inclusive, term, count) => {
                if let Some(r) = self.text.range(&cursor, range, *inclusive, count, ctx) {
                    let side = match term {
                        MoveTerminus::Beginning => r.start,
                        MoveTerminus::End => r.end,
                    };

                    let r = CursorRange::new(cursor, side, r.shape, r.inclusive);

                    return Ok(Some(r));
                } else {
                    return Ok(None);
                }
            },
            EditTarget::CurrentPosition => {
                let end = cursor.clone();
                let range = CursorRange::inclusive(cursor, end, TargetShape::CharWise);

                return Ok(Some(range));
            },
            EditTarget::CharJump(mark) => {
                let nc = self._charjump(mark, ctx, store)?;
                let range = CursorRange::exclusive(cursor, nc, TargetShape::CharWise);

                return Ok(Some(range));
            },
            EditTarget::LineJump(mark) => {
                let nc = self._linejump(mark, ctx, store)?;
                let range = CursorRange::exclusive(cursor, nc, TargetShape::LineWise);

                return Ok(Some(range));
            },
            EditTarget::Search(search, flip, count) => {
                let range = self._search(&cursor, search, flip, count, ctx.context, store)?;

                let range = range.map(|r| {
                    let shape = TargetShape::CharWise;
                    let inclusive = r.start > cursor;

                    CursorRange::new(cursor, r.start, shape, inclusive)
                });

                return Ok(range);
            },
            EditTarget::Selection => {
                let shape = ctx.context.get_target_shape().unwrap_or_else(|| state.shape());
                let selnc = state.anchor().clone();
                let selnx = selnc.x;
                let range = CursorRange::inclusive(selnc.goal(selnx), cursor, shape);

                return Ok(Some(range));
            },
            EditTarget::Motion(motion, count) => {
                return Ok(self.text.range_of_movement(&cursor, motion, count, ctx));
            },
            EditTarget::Range(range, inclusive, count) => {
                return Ok(self.text.range(&cursor, range, *inclusive, count, ctx));
            },
        }
    }

    fn _effective(
        &self,
        range: &CursorRange,
        forced: Option<TargetShape>,
    ) -> (TargetShape, Vec<(CharOff, CharOff, bool)>) {
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
                let ranges = match self.text.newlines(end).next() {
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
                let (mut lc, mut rc) = block_cursors(&range.start, &range.end);

                let mut ranges = vec![];
                let min = lc.x;

                let lctx = &(&self.text, 0, true);
                let rctx = &(&self.text, 0, false);

                for line in range.start.y..=range.end.y {
                    lc.set_line(line, lctx);
                    rc.set_line(line, rctx);

                    if lc.x < min {
                        // Left column is right of the last column.
                        continue;
                    }

                    let left = self.text.cursor_to_offset(&lc);
                    let right = self.text.cursor_to_offset(&rc);

                    ranges.push((left, right, true));
                }

                (TargetShape::BlockWise, ranges)
            },
        }
    }

    fn _zero_local(&mut self) {
        self.cursors.zero();
        self.changed.zero();
        self.jumped.zero();
    }

    fn _zero(&mut self, store: &mut Store<I>) {
        self._zero_local();
        store.cursors.zero_id(&self.id);
    }

    fn _adjust(&mut self, adjs: &[CursorAdjustment], store: &mut Store<I>) {
        self.cursors.adjust(adjs);
        self.changed.adjust(adjs);
        self.jumped.adjust(adjs);

        for completion in self.completions.values_mut() {
            completion.adjust(adjs);
        }

        store.cursors.adjust_id(&self.id, adjs);
    }

    fn _adjust_all(&mut self, adjs: Vec<CursorAdjustment>, store: &mut Store<I>) {
        self._adjust(adjs.as_slice(), store);
    }

    fn _adjust_columns(
        &mut self,
        line: usize,
        column_start: usize,
        amt_line: isize,
        amt_col: isize,
        store: &mut Store<I>,
    ) {
        let adj = CursorAdjustment::Column { line, column_start, amt_line, amt_col };

        self._adjust(&[adj], store);
    }

    fn _adjust_lines(
        &mut self,
        line_start: usize,
        line_end: usize,
        amount: isize,
        amount_after: isize,
        store: &mut Store<I>,
    ) {
        let adj = CursorAdjustment::Line { line_start, line_end, amount, amount_after };

        self._adjust(&[adj], store);
    }

    pub(crate) fn line_leftover(
        &mut self,
        dir: MoveDir1D,
        count: usize,
        gid: CursorGroupId,
    ) -> usize {
        let leader = self.get_leader(gid);

        match dir {
            MoveDir1D::Next => {
                let avail = self.text.get_lines().saturating_sub(1).saturating_sub(leader.y);

                return count.saturating_sub(avail);
            },
            MoveDir1D::Previous => {
                return count.saturating_sub(leader.y);
            },
        }
    }

    pub(crate) fn motion<C: EditContext>(
        &mut self,
        target: &EditTarget,
        ictx: &CursorGroupIdContext<'_, '_, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let shape = ictx.2.get_target_shape();

        if shape.is_some() {
            return self.selection_resize(&SelectionResizeStyle::Extend, target, ictx, store);
        }

        let gid = ictx.0;
        let mut group = self.get_group(gid);
        self.completions.remove(&gid);

        if target.is_jumping() {
            // Save current positions before we jump.
            self.push_jump(gid, &group);
        }

        for state in group.iter_mut() {
            state.unselect();

            let ctx = self._ctx_cgi2es(&EditAction::Motion, ictx);
            let cursor = state.cursor();

            match target {
                EditTarget::Boundary(range, inclusive, term, count) => {
                    if let Some(r) = self.text.range(cursor, range, *inclusive, count, &ctx) {
                        let nc = match term {
                            MoveTerminus::Beginning => r.start,
                            MoveTerminus::End => r.end,
                        };

                        state.set_cursor(nc);
                    }
                },
                EditTarget::CurrentPosition | EditTarget::Selection => {
                    // Do nothing.
                },
                EditTarget::CharJump(mark) => {
                    let nc = self._charjump(mark, &ctx, store)?;
                    state.set_cursor(nc);
                },
                EditTarget::LineJump(mark) => {
                    let nc = self._linejump(mark, &ctx, store)?;
                    state.set_cursor(nc);
                },
                EditTarget::Motion(mv, count) => {
                    if let Some(nc) = self.text.movement(cursor, mv, count, &ctx) {
                        state.set_cursor(nc);
                    }
                },
                EditTarget::Range(range, inclusive, count) => {
                    if let Some(r) = self.text.range(cursor, range, *inclusive, count, &ctx) {
                        state.set_cursor(r.end);
                    }
                },
                EditTarget::Search(search, flip, count) => {
                    if let Some(r) =
                        self._search(cursor, search, flip, count, ctx.context, store)?
                    {
                        state.set_cursor(r.start);
                    }
                },
            }
        }

        self.set_group(gid, group);

        Ok(None)
    }

    /// Indicates whether this buffer contains only whitespace.
    pub fn is_blank(&self) -> bool {
        self.text.is_blank()
    }

    /// Return a reference to the contents of this buffer.
    pub fn get(&self) -> &EditRope {
        &self.text
    }

    /// Return the contents of this buffer as a [String].
    pub fn get_text(&self) -> String {
        self.text.to_string()
    }

    /// Swap out the contents of this buffer with `t` and return the old value.
    fn swap_rope<T: Into<EditRope>>(&mut self, t: T) -> EditRope {
        let mut rope = t.into();

        std::mem::swap(&mut self.text, &mut rope);
        self.text.trailing_newline();

        // Reinitialize history so that undo doesn't take us to old buffer state.
        self.history = HistoryList::new(self.text.clone(), BUFFER_HISTORY_LEN);

        // XXX: Need to zero out global marks on rope change.
        self._zero_local();

        // Any existing completions are now invalid.
        self.completions.clear();

        return rope;
    }

    /// Replace the contents of this buffer with `t`.
    ///
    /// This also resets buffer-associated state, like marks and history.
    pub fn set_text<T: Into<EditRope>>(&mut self, t: T) {
        let _ = self.swap_rope(t);
    }

    /// Append text to this buffer.
    pub fn append_text<T: Into<EditRope>>(&mut self, t: T) -> Range<usize> {
        let start = self.get_lines();
        self.text += t.into();
        self.text.trailing_newline();
        let end = self.get_lines();

        Range { start, end }
    }

    /// Clear the buffer of its current content, and return it.
    ///
    /// This also resets buffer-associated state, like marks and history.
    pub fn reset(&mut self) -> EditRope {
        self.swap_rope("\n")
    }

    /// Clear the buffer of its current content, and return it as a [String].
    ///
    /// This also resets buffer-associated state, like marks and history.
    pub fn reset_text(&mut self) -> String {
        self.reset().to_string()
    }

    /// Get completion candidates for the give cursor group to show the user.
    pub fn get_completions(&self, gid: CursorGroupId) -> Option<CompletionList> {
        self.completions.get(&gid).cloned()
    }

    /// Returns the text currently that the cursor group's leader is currently positioned over.
    pub fn get_cursor_word(&self, gid: CursorGroupId, style: &WordStyle) -> Option<String> {
        let group = self.cursors.get(gid)?;
        let cursor = group.leader.cursor();

        self.text.get_cursor_word(cursor, style).map(|r| r.to_string())
    }

    /// Returns the text currently selected by the cursor group's leader.
    pub fn get_selected_word(&self, gid: CursorGroupId) -> Option<String> {
        let group = self.cursors.get(gid)?;
        let start = self.text.cursor_to_offset(group.leader.start());
        let end = self.text.cursor_to_offset(group.leader.end());

        self.text.chars_until(start, end).collect::<String>().into()
    }

    fn push_change(&mut self, group: &CursorGroup) {
        if !self.push_next_change {
            return;
        }

        if let Some(c) = self.changed.back() {
            if c.leader.cursor().y == group.leader.cursor().y {
                // Don't push any more states until we've moved to a new line.
                return;
            }
        }

        self.changed.push_back(group.clone());
        self.push_next_change = false;

        while self.changed.len() > 100 {
            let _ = self.changed.pop_front();
        }
    }

    fn push_jump(&mut self, gid: CursorGroupId, group: &CursorGroup) {
        match self.jumped.entry(gid) {
            Entry::Occupied(mut o) => {
                let jumps = o.get_mut();

                if jumps.current() != group {
                    jumps.push(group.clone());
                }
            },
            Entry::Vacant(v) => {
                v.insert(HistoryList::new(group.clone(), 100));
            },
        }
    }

    /// Get a mutable reference to a cursor group.
    fn get_group_mut(&mut self, id: CursorGroupId) -> &mut CursorGroup {
        self.cursors.entry(id).or_default()
    }

    /// Get the identifiers of cursors within a cursor group.
    fn get_group(&mut self, id: CursorGroupId) -> CursorGroup {
        self.get_group_mut(id).clone()
    }

    /// Set the leader and member of a cursor group.
    pub fn set_group(&mut self, gid: CursorGroupId, group: CursorGroup) {
        self.cursors.put(gid, group);
    }

    /// Move the [Cursor] for the leader of a cursor group.
    pub fn set_leader(&mut self, id: CursorGroupId, cursor: Cursor) {
        self.get_group_mut(id).leader.set_cursor(cursor);
    }

    fn get_leader_state(&mut self, id: CursorGroupId) -> &CursorState {
        &self.get_group_mut(id).leader
    }

    /// Get the [Cursor] for the leader of a cursor group.
    pub fn get_leader(&mut self, id: CursorGroupId) -> Cursor {
        self.get_leader_state(id).cursor().clone()
    }

    /// Get the cursors of the followers within a cursor group.
    pub fn get_followers(&self, id: CursorGroupId) -> Vec<Cursor> {
        if let Some(group) = self.cursors.get(id) {
            group.members.iter().map(|state| state.cursor().clone()).collect()
        } else {
            Vec::new()
        }
    }

    /// Get the [Selections] for the followers within a cursor group.
    pub fn get_follower_selections(&self, id: CursorGroupId) -> Option<Selections> {
        let sels = self
            .cursors
            .get(id)?
            .members
            .iter()
            .filter_map(CursorState::to_selection)
            .collect::<Vec<_>>();

        if sels.is_empty() {
            None
        } else {
            Some(sels)
        }
    }

    /// Get the [Selection] for the leader of a cursor group.
    pub fn get_leader_selection(&mut self, id: CursorGroupId) -> Option<Selection> {
        self.get_leader_state(id).to_selection()
    }

    /// Get the [Selections] for everyone within a cursor group.
    pub fn get_group_selections(&self, id: CursorGroupId) -> Option<Selections> {
        let sels = self
            .cursors
            .get(id)?
            .iter()
            .filter_map(CursorState::to_selection)
            .collect::<Vec<_>>();

        if sels.is_empty() {
            None
        } else {
            Some(sels)
        }
    }

    /// Create a new cursor group.
    pub fn create_group(&mut self) -> CursorGroupId {
        CursorGroupId(self.cgidgen.next())
    }

    pub(crate) fn lines(&self, line: usize) -> LineIterator<'_> {
        self.get().lines(line)
    }

    pub(crate) fn lines_at(&self, line: usize, column: usize) -> LineIterator<'_> {
        self.get().lines_at(line, column)
    }

    /// Returns how many lines are within this buffer.
    pub fn get_lines(&self) -> usize {
        self.get().get_lines()
    }

    /// Returns how many columns are on a given line.
    pub fn get_columns(&self, y: usize) -> usize {
        self.get().get_columns(y)
    }

    /// Fetch a reference to the information of type `T` on a given line.
    pub fn get_line_info<T: Send + Sync + 'static>(&self, line: usize) -> Option<&T> {
        self.lineinfo.get(&line)
    }

    /// Fetch a mutable reference to the information of type `T` on a given line.
    pub fn get_line_info_mut<T: Send + Sync + 'static>(&mut self, line: usize) -> Option<&mut T> {
        self.lineinfo.get_mut(&line)
    }

    /// Update the information of type `T` on a given line.
    pub fn set_line_info<T: Send + Sync + 'static>(&mut self, line: usize, info: T) {
        self.lineinfo.set(line, info);
    }

    /// Clamp the line and column of the cursors in a [CursorState] so that they refer to a valid
    /// point within the buffer.
    pub fn clamp_state<C: EditContext>(
        &self,
        state: &mut CursorState,
        ctx: &CursorGroupIdContext<'_, '_, C>,
    ) {
        match state {
            CursorState::Location(ref mut cursor) => {
                PrivateCursorOps::clamp(cursor, &self._ctx_cgi2c(ctx));
            },
            CursorState::Selection(ref mut cursor, ref mut anchor, _) => {
                PrivateCursorOps::clamp(cursor, &self._ctx_cgi2c(ctx));
                PrivateCursorOps::clamp(anchor, &self._ctx_cgi2c(ctx));
            },
        }
    }

    /// Clamp the line and column of a cursor so that it refers to a valid point within the buffer.
    pub fn clamp<C: EditContext>(
        &self,
        cursor: &mut Cursor,
        ctx: &CursorGroupIdContext<'_, '_, C>,
    ) {
        PrivateCursorOps::clamp(cursor, &self._ctx_cgi2c(ctx));
    }

    fn _ctx_cgi2es<'a, 'b, 'c, C: EditContext>(
        &self,
        action: &'a EditAction,
        ctx: &CursorGroupIdContext<'b, 'c, C>,
    ) -> CursorMovementsContext<'a, 'b, 'c, Cursor, C> {
        CursorMovementsContext { action, view: ctx.1, context: ctx.2 }
    }

    fn _ctx_cgi2c<'a, C: EditContext>(
        &'a self,
        ctx: &CursorGroupIdContext<'_, '_, C>,
    ) -> CursorContext<'a> {
        let lastcol = ctx.2.get_last_column();
        let width = ctx.1.get_width();

        (&self.text, width, lastcol)
    }

    fn _ctx_es2c<'a, C: EditContext>(
        &'a self,
        ctx: &CursorMovementsContext<'_, '_, '_, Cursor, C>,
    ) -> CursorContext<'a> {
        let lastcol = ctx.context.get_last_column();
        let width = ctx.view.get_width();

        (&self.text, width, lastcol)
    }

    #[cfg(feature = "intervaltree")]
    pub(crate) fn _selection_intervals(&self, gid: CursorGroupId) -> HighlightInfo {
        self.get_group_selections(gid)
            .into_iter()
            .flatten()
            .map(|s| (s.0.y..s.1.y.saturating_add(1), s))
            .collect()
    }

    #[cfg(feature = "intervaltree")]
    pub(crate) fn _follower_intervals(&self, gid: CursorGroupId) -> FollowersInfo {
        self.get_followers(gid)
            .into_iter()
            .map(|c| ((c.y, c.x)..(c.y, c.x + 1), c))
            .collect()
    }
}

impl<'a, 'b, C, I> HistoryActions<CursorGroupIdContext<'a, 'b, C>, I> for EditBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn undo(
        &mut self,
        count: &Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let count = ctx.2.resolve(count);
        let older = self.history.prev(count);

        let adjs = self.text.diff(older);
        self.text = older.clone();
        self._adjust_all(adjs, store);

        Ok(None)
    }

    fn redo(
        &mut self,
        count: &Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let count = ctx.2.resolve(count);
        let newer = self.history.next(count);

        let adjs = self.text.diff(newer);
        self.text = newer.clone();
        self._adjust_all(adjs, store);

        Ok(None)
    }

    fn checkpoint(
        &mut self,
        _: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        if &self.text != self.history.current() {
            // First, increment lines that are in the latest rope.
            for line in self.text.lines(0).map(Cow::from) {
                self.lines.line_incr(line.as_ref());
                store.completions.lines.line_incr(line.as_ref());
            }

            // Then decrement lines from the previous checkpoint.
            for line in self.history.current().lines(0).map(Cow::from) {
                self.lines.line_decr(line.as_ref());
                store.completions.lines.line_decr(line.as_ref());
            }

            self.history.push(self.text.clone());
            self.push_next_change = true;
        } else if self.lines.is_empty() {
            // Generate completions on first checkpoint.
            for line in self.text.lines(0).map(Cow::from) {
                self.lines.line_incr(line.as_ref());
                store.completions.lines.line_incr(line.as_ref());
            }
        }

        Ok(None)
    }
}

impl<'a, 'b, C, I> EditorActions<CursorGroupIdContext<'a, 'b, C>, Store<I>, I> for EditBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn edit(
        &mut self,
        action: &EditAction,
        target: &EditTarget,
        ictx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        self.completions.remove(&ictx.0);

        if let EditAction::Motion = action {
            return self.motion(target, ictx, store);
        }

        let ctx = &self._ctx_cgi2es(action, ictx);
        let gid = ictx.0;
        let end = ctx.context.get_cursor_end();
        let mut group = self.get_group(gid);

        for state in group.iter_mut() {
            let choice = match (self._target(state, target, ctx, store)?, action) {
                (Some(range), EditAction::Delete) => self.delete(&range, ctx, store)?,
                (Some(range), EditAction::Yank) => self.yank(&range, ctx, store)?,
                (Some(range), EditAction::Replace(v)) => {
                    match ctx.context.get_replace_char() {
                        Some(c) => {
                            let c = self._char(c, state.cursor(), &store.digraphs)?;

                            self.replace(c, *v, &range, ctx, store)?
                        },
                        None => {
                            let msg = "No replacement character".to_string();
                            let err = EditError::Failure(msg);

                            return Err(err);
                        },
                    }
                },
                (Some(range), EditAction::Format) => self.format(&range, ctx, store)?,
                (Some(range), EditAction::ChangeCase(case)) => {
                    self.changecase(case, &range, ctx, store)?
                },
                (Some(range), EditAction::ChangeNumber(change, mul)) => {
                    self.changenum(change, *mul, &range, ctx, store)?
                },
                (Some(range), EditAction::Join(spaces)) => {
                    self.join(*spaces, &range, ctx, store)?
                },
                (Some(range), EditAction::Indent(change)) => {
                    self.indent(change, &range, ctx, store)?
                },
                (Some(_), EditAction::Motion) => panic!("Unexpected EditAction::Motion!"),
                (None, _) => CursorChoice::Empty,
            };

            if let Some(cursor) = choice.resolve(end) {
                state.set(cursor);
                self.clamp_state(state, ictx);
            }
        }

        self.push_change(&group);
        self.set_group(gid, group);

        Ok(None)
    }

    fn mark(
        &mut self,
        name: Mark,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let leader = self.get_leader(ctx.0);
        store.cursors.set_mark(self.id.clone(), name, leader);
        self.completions.remove(&ctx.0);

        Ok(None)
    }

    fn complete(
        &mut self,
        comptype: &CompletionType,
        selection: &CompletionSelection,
        display: &CompletionDisplay,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match comptype {
            CompletionType::Auto => self.complete_auto(selection, display, ctx, store),
            CompletionType::File => self.complete_file(selection, display, ctx, store),
            CompletionType::Line(scope) => {
                self.complete_line(scope, selection, display, ctx, store)
            },
            CompletionType::Word(scope) => {
                self.complete_word(scope, selection, display, ctx, store)
            },
        }
    }

    fn insert_text(
        &mut self,
        act: &InsertTextAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        self.completions.remove(&ctx.0);

        match act {
            InsertTextAction::OpenLine(shape, dir, count) => {
                self.open_line(*shape, *dir, count, ctx, store)
            },
            InsertTextAction::Paste(style, count) => self.paste(style, count, ctx, store),
            InsertTextAction::Transcribe(s, dir, count) => {
                self.transcribe(s.as_str(), *dir, count, ctx, store)
            },
            InsertTextAction::Type(c, dir, count) => {
                if let Some(c) = ctx.2.resolve(c) {
                    self.type_char(c, *dir, count, ctx, store)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn selection_command(
        &mut self,
        act: &SelectionAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        self.completions.remove(&ctx.0);

        match act {
            SelectionAction::CursorSet(change) => self.selection_cursor_set(change, ctx, store),
            SelectionAction::Duplicate(dir, count) => {
                self.selection_duplicate(*dir, count, ctx, store)
            },
            SelectionAction::Expand(boundary, filter) => {
                self.selection_expand(boundary, *filter, ctx, store)
            },
            SelectionAction::Resize(style, target) => {
                self.selection_resize(style, target, ctx, store)
            },
            SelectionAction::Split(style, filter) => {
                self.selection_split(style, *filter, ctx, store)
            },
            SelectionAction::Trim(boundary, filter) => {
                self.selection_trim(boundary, *filter, ctx, store)
            },
        }
    }

    fn cursor_command(
        &mut self,
        act: &CursorAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        self.completions.remove(&ctx.0);

        match act {
            CursorAction::Close(target) => self.cursor_close(target, ctx, store),
            CursorAction::Split(count) => self.cursor_split(count, ctx, store),
            CursorAction::Restore(style) => self.cursor_restore(style, ctx, store),
            CursorAction::Rotate(dir, count) => self.cursor_rotate(*dir, count, ctx, store),
            CursorAction::Save(style) => self.cursor_save(style, ctx, store),
        }
    }

    fn history_command(
        &mut self,
        act: &HistoryAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        self.completions.remove(&ctx.0);

        match act {
            HistoryAction::Checkpoint => self.checkpoint(ctx, store),
            HistoryAction::Undo(count) => self.undo(count, ctx, store),
            HistoryAction::Redo(count) => self.redo(count, ctx, store),
        }
    }
}

impl<'a, 'b, C, I> Editable<CursorGroupIdContext<'a, 'b, C>, Store<I>, I> for EditBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match act {
            EditorAction::Edit(ea, et) => {
                let ea = ctx.2.resolve(ea);

                self.edit(&ea, et, ctx, store)
            },

            EditorAction::Cursor(act) => self.cursor_command(act, ctx, store),
            EditorAction::History(act) => self.history_command(act, ctx, store),
            EditorAction::InsertText(act) => self.insert_text(act, ctx, store),
            EditorAction::Mark(name) => self.mark(ctx.2.resolve(name), ctx, store),
            EditorAction::Selection(act) => self.selection_command(act, ctx, store),

            EditorAction::Complete(ct, sel, disp) => self.complete(ct, sel, disp, ctx, store),
        }
    }
}

impl<'a, 'b, C, I> Jumpable<CursorGroupIdContext<'a, 'b, C>, I> for EditBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> UIResult<usize, I> {
        let gid = ctx.0;

        match list {
            PositionList::ChangeList => {
                let clen = self.changed.len();

                let off = match (dir, self.changed_idx.get(&gid)) {
                    (MoveDir1D::Previous, i) => {
                        let idx = i.unwrap_or(&0).to_owned().saturating_add(count);

                        if idx <= clen {
                            idx
                        } else {
                            clen
                        }
                    },
                    (MoveDir1D::Next, Some(i)) => {
                        let idx = i.to_owned();

                        if idx >= count {
                            idx - count
                        } else {
                            0
                        }
                    },
                    (MoveDir1D::Next, None) => {
                        return Ok(0);
                    },
                };

                self.changed_idx.insert(gid, off);

                if let Some(group) = self.changed.get(clen - off) {
                    let group = group.clone();

                    self.set_group(gid, group);
                }

                return Ok(0);
            },
            PositionList::JumpList => {
                let jumps = match self.jumped.get_mut(gid) {
                    Some(c) => c,
                    None => return Ok(count),
                };

                let (len, group) = match dir {
                    MoveDir1D::Previous => {
                        if jumps.future_len() == 0 {
                            // Push current position if this is the first jump backwards.
                            let current = self.cursors.entry(gid).or_default();

                            if jumps.current() != current {
                                jumps.push(current.clone());
                            }
                        }

                        let plen = jumps.past_len();
                        let group = jumps.prev(count);

                        (plen, group)
                    },
                    MoveDir1D::Next => {
                        let flen = jumps.future_len();
                        let group = jumps.next(count);

                        (flen, group)
                    },
                };

                if len > 0 {
                    let group = group.to_owned();

                    self.set_group(gid, group);
                }

                return Ok(count.saturating_sub(len));
            },
        };
    }
}

impl<'a, 'b, C, I> Editable<CursorGroupIdContext<'a, 'b, C>, Store<I>, I> for SharedBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        self.write().unwrap().editor_command(act, ctx, store)
    }
}

impl<'a, 'b, C, I> Searchable<CursorGroupIdContext<'a, 'b, C>, Store<I>, I> for EditBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let search = EditTarget::Search(SearchType::Regex, dir, count);

        Ok(self.motion(&search, ctx, store)?)
    }
}

impl<'a, 'b, C, I> Jumpable<CursorGroupIdContext<'a, 'b, C>, I> for SharedBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> UIResult<usize, I> {
        self.write().unwrap().jump(list, dir, count, ctx)
    }
}

impl<'a, 'b, C, I> Searchable<CursorGroupIdContext<'a, 'b, C>, Store<I>, I> for SharedBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        self.write().unwrap().search(dir, count, ctx, store)
    }
}

#[cfg(test)]
#[macro_use]
mod tests {
    pub use super::*;
    pub use crate::editing::application::EmptyInfo;
    pub use crate::editing::base::TargetShape::{BlockWise, CharWise, LineWise};
    pub use crate::editing::base::{
        InsertStyle,
        JoinStyle,
        MovePosition,
        MoveType,
        PasteStyle,
        RangeType,
        Specifier,
        WordStyle,
    };
    pub use crate::editing::store::{RegisterCell, RegisterPutFlags, Store};
    pub use crate::env::vim::VimContext;

    macro_rules! get_mark {
        ($store: expr, $c: expr) => {
            $store.cursors.get_mark("".to_string(), mark!($c))
        };
    }

    macro_rules! assert_mark {
        ($store: expr, $c: expr, $cursor: expr) => {
            assert_eq!(get_mark!($store, $c).unwrap(), $cursor)
        };
    }

    pub(super) fn mkctx() -> VimContext {
        VimContext::default()
    }

    pub(super) fn mkbuf() -> EditBuffer<EmptyInfo> {
        EditBuffer::new("".to_string())
    }

    pub(super) fn mkfive() -> (
        EditBuffer<EmptyInfo>,
        CursorGroupId,
        ViewportContext<Cursor>,
        VimContext,
        Store<EmptyInfo>,
    ) {
        let mut buf = mkbuf();
        let gid = buf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = mkctx();
        let store = Store::default();

        return (buf, gid, vwctx, vctx, store);
    }

    pub(super) fn mkfivestr(
        s: &str,
    ) -> (
        EditBuffer<EmptyInfo>,
        CursorGroupId,
        ViewportContext<Cursor>,
        VimContext,
        Store<EmptyInfo>,
    ) {
        let (mut buf, gid, vwctx, vctx, mut store) = mkfive();

        buf.set_text(s);
        buf.checkpoint(ctx!(gid, vwctx, vctx), &mut store).unwrap();

        return (buf, gid, vwctx, vctx, store);
    }

    #[test]
    fn test_marks() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr(
            "12345\n\
            67890\n\
            abcde\n\
            fghij\n\
            klmno\n\
            pqrst\n\
            uvwxy\n",
        );

        // Set up a bunch of marks to check.
        ebuf.set_leader(gid, Cursor::new(0, 4));
        ebuf.mark(mark!('a'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(1, 3));
        ebuf.mark(mark!('b'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(2, 1));
        ebuf.mark(mark!('c'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(2, 4));
        ebuf.mark(mark!('d'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(4, 1));
        ebuf.mark(mark!('e'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(4, 3));
        ebuf.mark(mark!('f'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(5, 3));
        ebuf.mark(mark!('g'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(5, 2));
        ebuf.mark(mark!('h'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(6, 0));
        ebuf.mark(mark!('i'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(6, 4));
        ebuf.mark(mark!('j'), ctx!(gid, vwctx, vctx), &mut store).unwrap();

        // Move to the third line, so we can verify that earlier marks go untouched.
        ebuf.set_leader(gid, Cursor::new(2, 3));

        // Test that typing 'q' moves 'd right.
        type_char!(ebuf, 'q', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcqde\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 4));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(2, 5));
        assert_mark!(store, 'e', Cursor::new(4, 1));
        assert_mark!(store, 'f', Cursor::new(4, 3));
        assert_mark!(store, 'g', Cursor::new(5, 3));
        assert_mark!(store, 'h', Cursor::new(5, 2));
        assert_mark!(store, 'i', Cursor::new(6, 0));
        assert_mark!(store, 'j', Cursor::new(6, 4));

        // Test that typing '\n' moves the marks down, and adjusts the column for 'd.
        type_char!(ebuf, '\n', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nde\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 0));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(3, 1));
        assert_mark!(store, 'e', Cursor::new(5, 1));
        assert_mark!(store, 'f', Cursor::new(5, 3));
        assert_mark!(store, 'g', Cursor::new(6, 3));
        assert_mark!(store, 'h', Cursor::new(6, 2));
        assert_mark!(store, 'i', Cursor::new(7, 0));
        assert_mark!(store, 'j', Cursor::new(7, 4));

        // Test that pasting a word adjusts the column for 'd.
        set_named_reg!(store, 's', CharWise, "hello ");
        vctx.action.register = Some(Register::Named('s'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(gid, vwctx, vctx), store);
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nhello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 11));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(3, 13));
        assert_mark!(store, 'e', Cursor::new(5, 1));
        assert_mark!(store, 'f', Cursor::new(5, 3));
        assert_mark!(store, 'g', Cursor::new(6, 3));
        assert_mark!(store, 'h', Cursor::new(6, 2));
        assert_mark!(store, 'i', Cursor::new(7, 0));
        assert_mark!(store, 'j', Cursor::new(7, 4));

        // Test that pasting a line adjusts columns.
        set_named_reg!(store, 's', LineWise, "foo\nbar\n");
        vctx.action.register = Some(Register::Named('s'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Exact(3), ctx!(gid, vwctx, vctx), store);
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nfoo\nbar\nfoo\nbar\nfoo\nbar\n\
            hello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 0));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(9, 13));
        assert_mark!(store, 'e', Cursor::new(11, 1));
        assert_mark!(store, 'f', Cursor::new(11, 3));
        assert_mark!(store, 'g', Cursor::new(12, 3));
        assert_mark!(store, 'h', Cursor::new(12, 2));
        assert_mark!(store, 'i', Cursor::new(13, 0));
        assert_mark!(store, 'j', Cursor::new(13, 4));

        set_named_reg!(store, 's', LineWise, "baz\n");
        vctx.action.register = Some(Register::Named('s'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Exact(1), ctx!(gid, vwctx, vctx), store);
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nfoo\nbaz\nbar\nfoo\nbar\nfoo\nbar\n\
            hello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 0));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(10, 13));
        assert_mark!(store, 'e', Cursor::new(12, 1));
        assert_mark!(store, 'f', Cursor::new(12, 3));
        assert_mark!(store, 'g', Cursor::new(13, 3));
        assert_mark!(store, 'h', Cursor::new(13, 2));
        assert_mark!(store, 'i', Cursor::new(14, 0));
        assert_mark!(store, 'j', Cursor::new(14, 4));

        // Delete the pasted lines.
        edit!(ebuf, EditAction::Delete, range!(RangeType::Line, 6), ctx!(gid, vwctx, vctx), store);
        assert_eq!(
            ebuf.get_text(),
            "12345\n67890\nabcq\nfoo\nhello hello de\nfghij\nklmno\npqrst\nuvwxy\n"
        );
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 0));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(4, 13));
        assert_mark!(store, 'e', Cursor::new(6, 1));
        assert_mark!(store, 'f', Cursor::new(6, 3));
        assert_mark!(store, 'g', Cursor::new(7, 3));
        assert_mark!(store, 'h', Cursor::new(7, 2));
        assert_mark!(store, 'i', Cursor::new(8, 0));
        assert_mark!(store, 'j', Cursor::new(8, 4));

        // Delete the pasted words.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Delete, mv!(mov, 2), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\nde\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 0));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(4, 1));
        assert_mark!(store, 'e', Cursor::new(6, 1));
        assert_mark!(store, 'f', Cursor::new(6, 3));
        assert_mark!(store, 'g', Cursor::new(7, 3));
        assert_mark!(store, 'h', Cursor::new(7, 2));
        assert_mark!(store, 'i', Cursor::new(8, 0));
        assert_mark!(store, 'j', Cursor::new(8, 4));

        // Delete the word containing 'd, sending it to column 0.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\n\nfghij\nklmno\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 0));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(4, 0));
        assert_mark!(store, 'e', Cursor::new(6, 1));
        assert_mark!(store, 'f', Cursor::new(6, 3));
        assert_mark!(store, 'g', Cursor::new(7, 3));
        assert_mark!(store, 'h', Cursor::new(7, 2));
        assert_mark!(store, 'i', Cursor::new(8, 0));
        assert_mark!(store, 'j', Cursor::new(8, 4));

        // Delete the lines containing marks 'd, 'e and 'f, sending them to (0, 0).
        edit!(ebuf, EditAction::Delete, range!(RangeType::Line, 3), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\npqrst\nuvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 0));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(0, 0));
        assert_mark!(store, 'e', Cursor::new(0, 0));
        assert_mark!(store, 'f', Cursor::new(0, 0));
        assert_mark!(store, 'g', Cursor::new(4, 3));
        assert_mark!(store, 'h', Cursor::new(4, 2));
        assert_mark!(store, 'i', Cursor::new(5, 0));
        assert_mark!(store, 'j', Cursor::new(5, 4));

        // Do a blockwise paste and check that columns get adjusted.
        set_named_reg!(store, 's', BlockWise, "foo\nbar");
        vctx.action.register = Some(Register::Named('s'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Exact(1), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "12345\n67890\nabcq\nfoo\npfooqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 1));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 3));
        assert_mark!(store, 'c', Cursor::new(2, 1));
        assert_mark!(store, 'd', Cursor::new(0, 0));
        assert_mark!(store, 'e', Cursor::new(0, 0));
        assert_mark!(store, 'f', Cursor::new(0, 0));
        assert_mark!(store, 'g', Cursor::new(4, 6));
        assert_mark!(store, 'h', Cursor::new(4, 5));
        assert_mark!(store, 'i', Cursor::new(5, 0));
        assert_mark!(store, 'j', Cursor::new(5, 7));

        // Test that marks get adjusted after blockwise deletes.
        let target = EditTarget::CharJump(Specifier::Exact(mark!('b')));
        vctx.persist.shape = Some(TargetShape::BlockWise);
        edit!(ebuf, EditAction::Delete, target, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "12345\n60\na\nf\npqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 1));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(1, 0));
        assert_mark!(store, 'c', Cursor::new(2, 0));
        assert_mark!(store, 'd', Cursor::new(0, 0));
        assert_mark!(store, 'e', Cursor::new(0, 0));
        assert_mark!(store, 'f', Cursor::new(0, 0));
        assert_mark!(store, 'g', Cursor::new(4, 3));
        assert_mark!(store, 'h', Cursor::new(4, 2));
        assert_mark!(store, 'i', Cursor::new(5, 0));
        assert_mark!(store, 'j', Cursor::new(5, 7));

        // Move to first line and test joining lines.
        let mov = MoveType::BufferLineOffset;
        edit!(ebuf, EditAction::Motion, mv!(mov, 1), ctx!(gid, vwctx, vctx), store);

        let operation = EditAction::Join(JoinStyle::OneSpace);
        edit!(ebuf, operation, range!(RangeType::Line, 3), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "12345 60 a\nf\npqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 8));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(0, 6));
        assert_mark!(store, 'c', Cursor::new(0, 9));
        assert_mark!(store, 'd', Cursor::new(0, 0));
        assert_mark!(store, 'e', Cursor::new(0, 0));
        assert_mark!(store, 'f', Cursor::new(0, 0));
        assert_mark!(store, 'g', Cursor::new(2, 3));
        assert_mark!(store, 'h', Cursor::new(2, 2));
        assert_mark!(store, 'i', Cursor::new(3, 0));
        assert_mark!(store, 'j', Cursor::new(3, 7));

        let operation = EditAction::Join(JoinStyle::NoChange);
        edit!(ebuf, operation, range!(RangeType::Line, 3), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "12345 60 afpqrst\nubarvwxy\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 11));
        assert_mark!(store, 'a', Cursor::new(0, 4));
        assert_mark!(store, 'b', Cursor::new(0, 6));
        assert_mark!(store, 'c', Cursor::new(0, 9));
        assert_mark!(store, 'd', Cursor::new(0, 0));
        assert_mark!(store, 'e', Cursor::new(0, 0));
        assert_mark!(store, 'f', Cursor::new(0, 0));
        assert_mark!(store, 'g', Cursor::new(0, 14));
        assert_mark!(store, 'h', Cursor::new(0, 13));
        assert_mark!(store, 'i', Cursor::new(1, 0));
        assert_mark!(store, 'j', Cursor::new(1, 7));
    }

    #[test]
    fn test_motion_mark_jump() {
        let (mut ebuf, gid, vwctx, vctx, mut store) =
            mkfivestr("12345\n   67890\nabcde\nfghij\n klmno\n");

        // Set up a bunch of marks to check.
        ebuf.set_leader(gid, Cursor::new(0, 4));
        ebuf.mark(mark!('a'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(1, 6));
        ebuf.mark(mark!('b'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(2, 1));
        ebuf.mark(mark!('c'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(2, 5));
        ebuf.mark(mark!('d'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(3, 2));
        ebuf.mark(mark!('e'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(4, 4));
        ebuf.mark(mark!('f'), ctx!(gid, vwctx, vctx), &mut store).unwrap();

        let op = EditAction::Motion;

        // Move to the top left to begin.
        ebuf.set_leader(gid, Cursor::new(0, 0));

        // Using LineJump always goes to the first word.
        edit_line_mark!(ebuf, op, 'a', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));
        edit_line_mark!(ebuf, op, 'b', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 3));
        edit_line_mark!(ebuf, op, 'c', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 0));
        edit_line_mark!(ebuf, op, 'd', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 0));
        edit_line_mark!(ebuf, op, 'e', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 0));
        edit_line_mark!(ebuf, op, 'f', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 1));

        // Using CharJump goes to the marked column.
        edit_char_mark!(ebuf, op, 'a', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
        edit_char_mark!(ebuf, op, 'b', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 6));
        edit_char_mark!(ebuf, op, 'c', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 1));
        edit_char_mark!(ebuf, op, 'd', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 5));
        edit_char_mark!(ebuf, op, 'e', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 2));
        edit_char_mark!(ebuf, op, 'f', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 4));
    }

    #[test]
    fn test_changelist() {
        let (mut ebuf, gid, vwctx, vctx, mut store) =
            mkfivestr("12345\n   67890\nabcde\nfghij\n klmno\n");
        let next = MoveDir1D::Next;
        let prev = MoveDir1D::Previous;
        let cl = PositionList::ChangeList;

        // Start out at (1, 3).
        ebuf.set_leader(gid, Cursor::new(1, 3));

        // Delete the "6", pushing current position into the changelist.
        edit!(ebuf, EditAction::Delete, EditTarget::CurrentPosition, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 3));
        assert_eq!(ebuf.get_text(), "12345\n   7890\nabcde\nfghij\n klmno\n");

        // Move down.
        edit!(ebuf, EditAction::Motion, mv!(MoveType::Line(next)), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 3));
        assert_eq!(ebuf.get_text(), "12345\n   7890\nabcde\nfghij\n klmno\n");

        // Delete the "d", no change made to changelist.
        edit!(ebuf, EditAction::Delete, EditTarget::CurrentPosition, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 3));
        assert_eq!(ebuf.get_text(), "12345\n   7890\nabce\nfghij\n klmno\n");

        // Checkpoint current state, so that we can push another position.
        ebuf.history_command(&HistoryAction::Checkpoint, ctx!(gid, vwctx, vctx), &mut store)
            .unwrap();

        // Move down.
        edit!(ebuf, EditAction::Motion, mv!(MoveType::Line(next)), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 3));
        assert_eq!(ebuf.get_text(), "12345\n   7890\nabce\nfghij\n klmno\n");

        // Type a character, pushing current position into the changelist.
        type_char!(ebuf, '1', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 4));
        assert_eq!(ebuf.get_text(), "12345\n   7890\nabce\nfgh1ij\n klmno\n");

        // Move down.
        edit!(ebuf, EditAction::Motion, mv!(MoveType::Line(next)), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(4, 4));
        assert_eq!(ebuf.get_text(), "12345\n   7890\nabce\nfgh1ij\n klmno\n");

        // Go back once to (3, 4).
        let count = ebuf.jump(cl, prev, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 4));

        // Go back once to (1, 3).
        let count = ebuf.jump(cl, prev, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 3));

        // Go forward once to (3, 4) again.
        let count = ebuf.jump(cl, next, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 4));

        // Can't go forward any more: original position is not saved in changelist, like it is w/
        // the jumplist.
        let count = ebuf.jump(cl, next, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 4));
    }

    #[test]
    fn test_jumplist() {
        let (mut ebuf, gid, vwctx, vctx, mut store) =
            mkfivestr("12345\n   67890\nabcde\nfghij\n klmno\n");
        let next = MoveDir1D::Next;
        let prev = MoveDir1D::Previous;
        let jl = PositionList::JumpList;

        // Set up a bunch of marks to jump to.
        ebuf.set_leader(gid, Cursor::new(0, 4));
        ebuf.mark(mark!('a'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(1, 6));
        ebuf.mark(mark!('b'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(2, 1));
        ebuf.mark(mark!('c'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(2, 5));
        ebuf.mark(mark!('d'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(3, 2));
        ebuf.mark(mark!('e'), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(gid, Cursor::new(4, 4));
        ebuf.mark(mark!('f'), ctx!(gid, vwctx, vctx), &mut store).unwrap();

        let op = EditAction::Motion;

        // Move to the top left to begin.
        ebuf.set_leader(gid, Cursor::new(0, 0));

        // CharJump should push (0, 0).
        edit_char_mark!(ebuf, op, 'a', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));

        // CharJump should push (0, 4).
        edit_char_mark!(ebuf, op, 'b', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 6));

        // CharJump should push (1, 6).
        edit_char_mark!(ebuf, op, 'c', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 1));

        // MoveType::Line is not a jump, so we don't push (2, 1).
        edit!(ebuf, op, MoveType::Line(next).into(), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 1));

        // Moving forward in the list when there's nothing should leave cursor where it is.
        let count = ebuf.jump(jl, next, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 1);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 1));

        // Move back twice in the jumplist.
        let count = ebuf.jump(jl, prev, 2, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
        assert_eq!(count, 0);

        // Move forward once to where we were before moving through the jumplist.
        let count = ebuf.jump(jl, next, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 6));

        // Move forward once, back to where we were before moving through the jumplist.
        let count = ebuf.jump(jl, next, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 1));

        // Move backward twice, back to (0, 4).
        let count = ebuf.jump(jl, prev, 2, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));

        edit!(ebuf, op, MoveType::Line(next).into(), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 4));

        // Now jump again, wiping future history with (1, 4).
        edit_char_mark!(ebuf, op, 'd', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 5));

        // Move forward does nothing since we wiped history.
        let count = ebuf.jump(jl, next, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 1);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 5));

        // Move backward to (1, 4).
        let count = ebuf.jump(jl, prev, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 4));

        // Move backward to (0, 4).
        let count = ebuf.jump(jl, prev, 1, ctx!(gid, vwctx, vctx)).unwrap();
        assert_eq!(count, 0);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
    }

    #[test]
    fn test_search_char_inclusive_forwards() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) =
            mkfivestr("a b c a b c 1 2 3 a b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");

        // Move in the same search direction as the last search (";").
        let same = EditTarget::Search(SearchType::Char(false), MoveDirMod::Same, Count::Contextual);

        // Move in the opposite search direction as the last search (",").
        let flip = EditTarget::Search(SearchType::Char(false), MoveDirMod::Flip, Count::Contextual);

        // Set cursor to (0, 4), after the first "a".
        ebuf.set_leader(gid, Cursor::new(0, 4));
        vctx.persist.charsearch_params = (MoveDir1D::Next, true);
        vctx.persist.charsearch = Some('a'.into());

        // Delete from cursor to the second "a" ("d2fa").
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, same, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_text(), "a b  b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");

        // Trying to delete to a third "a" should do nothing, since it hits the line ending ("d3;").
        vctx.action.count = Some(3);
        edit!(ebuf, EditAction::Delete, same, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_text(), "a b  b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");

        // Using SearchType::Char(true) allows searching onto the next line, like kakoune does.
        let target =
            EditTarget::Search(SearchType::Char(true), MoveDirMod::Same, Count::Contextual);
        vctx.action.count = Some(3);
        edit!(ebuf, EditAction::Delete, target, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
        assert_eq!(ebuf.get_text(), "a b  b c 1 2 3\n");

        // Delete to the previous occurrence of "a" ("d,").
        vctx.action.count = Some(1);
        edit!(ebuf, EditAction::Delete, flip, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));
        assert_eq!(ebuf.get_text(), " b c 1 2 3\n");
    }

    #[test]
    fn test_search_char_inclusive_backwards() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) =
            mkfivestr("a b c a b c 1 2 3 a b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n");

        // Move in the same search direction as the last search (";").
        let same = EditTarget::Search(SearchType::Char(false), MoveDirMod::Same, Count::Contextual);

        // Move in the opposite search direction as the last search (",").
        let flip = EditTarget::Search(SearchType::Char(false), MoveDirMod::Flip, Count::Contextual);

        // Set cursor to (1, 4), after the first "b" on the line.
        ebuf.set_leader(gid, Cursor::new(1, 4));
        vctx.persist.charsearch_params = (MoveDir1D::Previous, true);
        vctx.persist.charsearch = Some('b'.into());

        // Trying to delete multiple b's with multiline = false fails ("2dFb").
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, same, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 4));
        assert_eq!(
            ebuf.get_text(),
            "a b c a b c 1 2 3 a b c 1 2 3\na b c a b c 1 2 3 a b c 1 2 3\n"
        );

        // Setting multiline = true allows us to delete across the line boundary.
        let target =
            EditTarget::Search(SearchType::Char(true), MoveDirMod::Same, Count::Contextual);
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, target, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 20));
        assert_eq!(ebuf.get_text(), "a b c a b c 1 2 3 a c a b c 1 2 3 a b c 1 2 3\n");

        // Delete backwards for one 'b' ("dFb").
        vctx.action.count = None;
        edit!(ebuf, EditAction::Delete, same, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 8));
        assert_eq!(ebuf.get_text(), "a b c a c a b c 1 2 3 a b c 1 2 3\n");

        // Delete twice in the flipped direction ("2d,").
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, flip, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 8));
        assert_eq!(ebuf.get_text(), "a b c a  c 1 2 3\n");
    }

    #[test]
    fn test_search_regex() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) =
            mkfivestr("hello world\nhelp helm writhe\nwhisk helium\n");

        let op = EditAction::Motion;
        let mv = EditTarget::Search(SearchType::Regex, MoveDirMod::Same, Count::Contextual);

        store.set_last_search("he");

        // Move to (0, 6) to begin.
        ebuf.set_leader(gid, Cursor::new(0, 6));

        vctx.action.count = Some(1);
        edit!(ebuf, op, mv, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        vctx.action.count = Some(3);
        edit!(ebuf, op, mv, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 6));

        vctx.action.count = Some(4);
        edit!(ebuf, op, mv, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 14));

        vctx.persist.regexsearch_dir = MoveDir1D::Previous;

        vctx.action.count = Some(2);
        edit!(ebuf, op, mv, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        vctx.action.count = Some(1);
        edit!(ebuf, op, mv, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));
    }

    #[test]
    fn test_search_word_bound() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) =
            mkfivestr("hello world\nhellfire hello brimstone\nhello hell\n");

        let op = EditAction::Motion;
        let word = EditTarget::Search(
            SearchType::Word(WordStyle::Little, true),
            MoveDirMod::Same,
            Count::Contextual,
        );
        let next = EditTarget::Search(SearchType::Regex, MoveDirMod::Same, Count::Contextual);

        // Move to (0, 2) to begin, so that we're in the middle of "hello".
        ebuf.set_leader(gid, Cursor::new(0, 2));

        vctx.action.count = Some(1);
        edit!(ebuf, op, word, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 9));

        vctx.persist.regexsearch_dir = MoveDir1D::Previous;

        vctx.action.count = Some(1);
        edit!(ebuf, op, next, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        // Move to (2, 8) to begin, so that we're in the middle of "hell".
        ebuf.set_leader(gid, Cursor::new(2, 8));

        // Doesn't move.
        vctx.action.count = Some(1);
        edit!(ebuf, op, word, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 6));

        // Doesn't move.
        vctx.action.count = Some(4);
        edit!(ebuf, op, next, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 6));
    }

    #[test]
    fn test_search_word_no_bound() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) =
            mkfivestr("hello world\nhellfire hello brimstone\nhello hell\n");

        let op = EditAction::Motion;
        let word = EditTarget::Search(
            SearchType::Word(WordStyle::Little, false),
            MoveDirMod::Same,
            Count::Contextual,
        );
        let next = EditTarget::Search(SearchType::Regex, MoveDirMod::Same, Count::Contextual);

        // Move to (0, 2) to begin, so that we're in the middle of "hello".
        ebuf.set_leader(gid, Cursor::new(0, 2));

        vctx.action.count = Some(1);
        edit!(ebuf, op, word, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 9));

        vctx.persist.regexsearch_dir = MoveDir1D::Previous;

        vctx.action.count = Some(1);
        edit!(ebuf, op, next, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        // Move to (2, 8) to begin, so that we're in the middle of "hell".
        ebuf.set_leader(gid, Cursor::new(2, 8));

        vctx.action.count = Some(3);
        edit!(ebuf, op, word, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        vctx.action.count = Some(4);
        edit!(ebuf, op, next, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 9));
    }

    #[test]
    fn test_history() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        vctx.persist.insert = Some(InsertStyle::Insert);

        // Create several checkpoints.
        type_char!(ebuf, 'h', gid, vwctx, vctx, store);
        type_char!(ebuf, 'e', gid, vwctx, vctx, store);
        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        type_char!(ebuf, 'o', gid, vwctx, vctx, store);
        ebuf.checkpoint(ctx!(gid, vwctx, vctx), &mut store).unwrap();

        type_char!(ebuf, ' ', gid, vwctx, vctx, store);
        ebuf.checkpoint(ctx!(gid, vwctx, vctx), &mut store).unwrap();

        type_char!(ebuf, 'w', gid, vwctx, vctx, store);
        type_char!(ebuf, 'o', gid, vwctx, vctx, store);
        type_char!(ebuf, 'r', gid, vwctx, vctx, store);
        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        type_char!(ebuf, 'd', gid, vwctx, vctx, store);
        ebuf.checkpoint(ctx!(gid, vwctx, vctx), &mut store).unwrap();

        // Check that starting point is correct.
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 11));

        // Undo twice.
        ebuf.undo(&2.into(), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 5));

        // Redo once.
        ebuf.redo(&1.into(), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        assert_eq!(ebuf.get_text(), "hello \n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 6));

        // Undo five times hits beginning.
        ebuf.undo(&5.into(), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        // Redo thrice.
        ebuf.redo(&3.into(), ctx!(gid, vwctx, vctx), &mut store).unwrap();
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 11));

        // XXX: need to test that marks also get adjusted.
    }

    #[test]
    fn test_visual_motion() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr("foo\nbar\nbaz\n");

        // Perform CharWise selection.
        vctx.persist.shape = Some(TargetShape::CharWise);

        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));
        assert_eq!(
            ebuf.get_leader_selection(gid),
            Some((Cursor::new(0, 0), Cursor::new(0, 1), CharWise))
        );

        let mov = MoveType::Line(MoveDir1D::Next);

        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 1));
        assert_eq!(
            ebuf.get_leader_selection(gid),
            Some((Cursor::new(0, 0), Cursor::new(1, 1), CharWise))
        );

        // Changing shape to a LineWise selection keeps anchor and cursor in place.
        vctx.persist.shape = Some(TargetShape::LineWise);

        edit!(ebuf, EditAction::Motion, EditTarget::CurrentPosition, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 1));
        assert_eq!(
            ebuf.get_leader_selection(gid),
            Some((Cursor::new(0, 0), Cursor::new(1, 1), LineWise))
        );

        // Changing shape to a BlockWise selection keeps anchor and cursor in place.
        vctx.persist.shape = Some(TargetShape::BlockWise);

        edit!(ebuf, EditAction::Motion, EditTarget::CurrentPosition, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 1));
        assert_eq!(
            ebuf.get_leader_selection(gid),
            Some((Cursor::new(0, 0), Cursor::new(1, 1), BlockWise))
        );
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
