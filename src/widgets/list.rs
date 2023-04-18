//! # List
//!
//! ## Overview
//!
//! This renders a list of items that can be navigated using the [Editable] trait.
//!
//! ## Example
//!
//! ```
//! use modalkit::{
//!     editing::action::{Editable, EditAction, EditorActions},
//!     editing::application::EmptyInfo,
//!     editing::base::{MovePosition, MoveType},
//!     editing::store::Store,
//!     env::vim::VimContext,
//!     widgets::list::ListState,
//! };
//!
//! let mut store = Store::default();
//! let ctx = VimContext::<EmptyInfo>::default();
//!
//! // Create new list state.
//! let items = vec!["Alice".into(), "Bob".into(), "Eve".into()];
//! let mut list = ListState::<String, EmptyInfo>::new("People".into(), items);
//!
//! // Jump to end of the list.
//! let op = EditAction::Motion;
//! let mv = MoveType::BufferPos(MovePosition::End);
//! let _ = list.edit(&op, &mv.into(), &ctx, &mut store).unwrap();
//! ```
use std::cmp::{Ord, Ordering, PartialOrd};
use std::marker::PhantomData;

use regex::Regex;

use tui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Modifier as StyleModifier, Style},
    text::Text,
    widgets::{Paragraph, StatefulWidget, Widget},
};

use crate::editing::{
    action::{
        Action,
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
        PromptAction,
        Promptable,
        Scrollable,
        Searchable,
        SelectionAction,
        UIError,
        UIResult,
    },
    application::ApplicationInfo,
    base::{
        Axis,
        CloseFlags,
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
        MoveDir2D,
        MoveDirMod,
        MovePosition,
        MoveTerminus,
        MoveType,
        PositionList,
        RangeType,
        Register,
        ScrollSize,
        ScrollStyle,
        SearchType,
        TargetShape,
        ViewportContext,
        WordStyle,
        WriteFlags,
    },
    completion::CompletionList,
    context::EditContext,
    cursor::{Cursor, CursorGroup, CursorState},
    history::HistoryList,
    rope::EditRope,
    store::{RegisterCell, RegisterPutFlags, Store},
};

use crate::util::idx_offset;

use super::{ScrollActions, TerminalCursor, WindowOps};

fn _clamp_cursor(cursor: &mut ListCursor, len: usize) {
    let max = len.saturating_sub(1);

    if cursor.position <= max {
        return;
    }

    cursor.position = max;
    cursor.text_row = 0;
}

/// A position within a list.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ListCursor {
    /// The position of the selected item within the list.
    pub position: usize,

    /// A row within the [Text] representation of the selected [ListItem].
    pub text_row: usize,
}

impl ListCursor {
    /// Create a new cursor for a list.
    pub fn new(position: usize, text_row: usize) -> Self {
        ListCursor { position, text_row }
    }
}

impl Ord for ListCursor {
    fn cmp(&self, other: &Self) -> Ordering {
        let pcmp = self.position.cmp(&other.position);
        let tcmp = self.text_row.cmp(&other.text_row);

        pcmp.then(tcmp)
    }
}

impl PartialOrd for ListCursor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp(other).into()
    }
}

impl From<usize> for ListCursor {
    fn from(position: usize) -> Self {
        ListCursor { position, text_row: 0 }
    }
}

/// Trait for items kept in a [ListState].
pub trait ListItem<I>: Clone + ToString
where
    I: ApplicationInfo,
{
    /// Return a representation of this item to show in the terminal window.
    fn show(
        &self,
        selected: bool,
        viewport: &ViewportContext<ListCursor>,
        store: &mut Store<I>,
    ) -> Text;

    /// Return a word that represents this list item.
    ///
    /// By default this is just the [ToString] value, but you can provide a different
    /// implementation to get more useful [OpenTarget::Cursor] behaviour.
    ///
    /// [OpenTarget::Cursor]: crate::editing::base::OpenTarget::Cursor
    fn get_word(&self) -> Option<String> {
        self.to_string().into()
    }
}

impl<I> ListItem<I> for String
where
    I: ApplicationInfo,
{
    fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut Store<I>) -> Text {
        if selected {
            let hl = Style::default().add_modifier(StyleModifier::REVERSED);

            Text::styled(self.as_str(), hl)
        } else {
            Text::raw(self.as_str())
        }
    }
}

/// Persistent state for [List].
pub struct ListState<T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    id: I::ContentId,
    items: Vec<T>,
    cursor: ListCursor,
    viewctx: ViewportContext<ListCursor>,

    /// Tracks the jumplist for this window.
    jumped: HistoryList<ListCursor>,
}

/// Widget for rendering a list of text items.
pub struct List<'a, T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    focused: bool,
    empty_message: Option<Text<'a>>,
    empty_alignment: Alignment,
    store: &'a mut Store<I>,
    _p: PhantomData<T>,
}

impl<T, I> ListState<T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    /// Create state for a list.
    pub fn new(id: I::ContentId, items: Vec<T>) -> Self {
        let mut viewctx = ViewportContext::default();
        viewctx.wrap = true;

        ListState {
            id,
            items,
            cursor: 0.into(),
            viewctx,
            jumped: HistoryList::new(0.into(), 100),
        }
    }

    /// Get the content identifier for this list.
    pub fn id(&self) -> I::ContentId {
        self.id.clone()
    }

    /// Indicates whether or not this list contains any items.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    fn _clamp(&mut self) {
        _clamp_cursor(&mut self.cursor, self.items.len());
    }

    fn _range_to(&self, pos: ListCursor) -> EditRange<ListCursor> {
        EditRange::inclusive(self.cursor.clone(), pos, TargetShape::LineWise)
    }

    fn scrollview(&mut self, idx: usize, pos: MovePosition, store: &mut Store<I>) {
        match pos {
            MovePosition::Beginning => {
                self.viewctx.corner = idx.into();
            },
            MovePosition::Middle => {
                let mut lines = 0;
                let target = self.viewctx.get_height() / 2;
                let selidx = self.cursor.position;
                let posidx = idx;

                self.viewctx.corner.position = 0;
                self.viewctx.corner.text_row = 0;

                for (idx, item) in self.items.iter().enumerate().take(idx + 1).rev() {
                    let sel = selidx == idx;
                    let len = item.show(sel, &self.viewctx, store).lines.len();

                    if posidx == idx {
                        lines += len / 2;
                    } else {
                        lines += len;
                    }

                    if lines >= target {
                        // We've moved back far enough.
                        self.viewctx.corner.position = idx;
                        self.viewctx.corner.text_row = lines - target;
                        break;
                    }
                }
            },
            MovePosition::End => {
                let mut lines = 0;
                let target = self.viewctx.get_height();
                let pos = self.cursor.position;

                self.viewctx.corner.position = 0;
                self.viewctx.corner.text_row = 0;

                for (idx, item) in self.items.iter().enumerate().take(idx + 1).rev() {
                    let sel = idx == pos;
                    let len = item.show(sel, &self.viewctx, store).lines.len();

                    lines += len;

                    if lines >= target {
                        // We've moved back far enough.
                        self.viewctx.corner.position = idx;
                        self.viewctx.corner.text_row = lines - target;
                        break;
                    }
                }
            },
        }
    }

    /// Move cursor back inside viewport.
    fn shift_cursor(&mut self, store: &mut Store<I>) {
        if self.cursor < self.viewctx.corner {
            // Cursor is above the viewport; move it inside.
            self.cursor = self.viewctx.corner.position.into();
            return;
        }

        // Check whether the cursor is below the viewport.
        let mut lines = 0;

        for (idx, item) in self.items.iter().enumerate().skip(self.viewctx.corner.position) {
            if idx == self.cursor.position {
                // Cursor is already within the viewport.
                break;
            }

            lines += item.show(false, &self.viewctx, store).lines.len();

            if lines >= self.viewctx.get_height() {
                // We've reached the end of the viewport; move cursor into it.
                self.cursor = idx.into();
                break;
            }
        }
    }

    /// Replace the set of items with a new list.
    pub fn set(&mut self, items: Vec<T>) {
        self.items = items;
        self._clamp();
    }

    /// Get a reference to the currently selected value.
    pub fn get(&self) -> Option<&T> {
        self.items.get(self.cursor.position)
    }

    /// Get a mutable reference to the currently selected value.
    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.items.get_mut(self.cursor.position)
    }

    /// Set the dimensions and placement within the terminal window for this list.
    pub fn set_term_info(&mut self, area: Rect) {
        self.viewctx.dimensions = (area.width as usize, area.height as usize);
    }
}

impl<C, T, I> CursorMovements<ListCursor, C> for ListState<T, I>
where
    C: EditContext,
    T: ListItem<I>,
    I: ApplicationInfo,
{
    fn first_word(
        &self,
        pos: &ListCursor,
        _: &CursorMovementsContext<'_, '_, '_, ListCursor, C>,
    ) -> ListCursor {
        pos.clone()
    }

    fn movement(
        &self,
        pos: &ListCursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'_, '_, '_, ListCursor, C>,
    ) -> Option<ListCursor> {
        let len = self.items.len();
        let count = ctx.context.resolve(count);

        match movement {
            // These movements don't map meaningfully onto a list.
            MoveType::BufferByteOffset => None,
            MoveType::Column(_, _) => None,
            MoveType::ItemMatch => None,
            MoveType::LineColumnOffset => None,
            MoveType::LinePercent => None,
            MoveType::LinePos(_) => None,
            MoveType::SentenceBegin(_) => None,
            MoveType::ScreenFirstWord(_) => None,
            MoveType::ScreenLinePos(_) => None,
            MoveType::WordBegin(_, _) => None,
            MoveType::WordEnd(_, _) => None,

            MoveType::BufferLineOffset => {
                let max = len.saturating_sub(1);
                let off = count.saturating_sub(1).min(max);

                if off < len {
                    return Some(off.into());
                } else {
                    return None;
                }
            },
            MoveType::BufferLinePercent => {
                if count > 100 {
                    return None;
                }

                // Calculate the new index as described in :help N%
                let off = len.saturating_mul(count).saturating_add(99) / 100;
                let off = off.saturating_sub(1);

                if off < len {
                    return Some(off.into());
                } else {
                    return None;
                }
            },
            MoveType::BufferPos(MovePosition::Beginning) => {
                if len > 0 {
                    return Some(0.into());
                } else {
                    return None;
                }
            },
            MoveType::BufferPos(MovePosition::Middle) => {
                let off = len / 2;

                if off < len {
                    return Some(off.into());
                } else {
                    return None;
                }
            },
            MoveType::BufferPos(MovePosition::End) => {
                if len > 0 {
                    return Some(len.saturating_sub(1).into());
                } else {
                    return None;
                }
            },
            MoveType::FinalNonBlank(dir) |
            MoveType::FirstWord(dir) |
            MoveType::Line(dir) |
            MoveType::ScreenLine(dir) |
            MoveType::ParagraphBegin(dir) |
            MoveType::SectionBegin(dir) |
            MoveType::SectionEnd(dir) => {
                let pos = pos.position;

                match dir {
                    MoveDir1D::Previous => {
                        return Some(pos.saturating_sub(count).into());
                    },
                    MoveDir1D::Next => {
                        let max = len.saturating_sub(1);

                        return Some(pos.saturating_add(count).min(max).into());
                    },
                };
            },
            MoveType::ViewportPos(MovePosition::Beginning) => {
                return Some(self.viewctx.corner.position.into());
            },
            MoveType::ViewportPos(MovePosition::Middle) => {
                // Need store to calculate an accurate middle position.
                return None;
            },
            MoveType::ViewportPos(MovePosition::End) => {
                // Need store to calculate an accurate end position.
                return None;
            },
        }
    }

    fn range_of_movement(
        &self,
        pos: &ListCursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'_, '_, '_, ListCursor, C>,
    ) -> Option<EditRange<ListCursor>> {
        let other = self.movement(pos, movement, count, ctx)?;

        Some(EditRange::inclusive(pos.clone(), other, TargetShape::LineWise))
    }

    fn range(
        &self,
        pos: &ListCursor,
        range: &RangeType,
        _: bool,
        count: &Count,
        ctx: &CursorMovementsContext<'_, '_, '_, ListCursor, C>,
    ) -> Option<EditRange<ListCursor>> {
        let len = self.items.len();
        let max = len.saturating_sub(1);

        match range {
            RangeType::Bracketed(_, _) => None,
            RangeType::Item => None,
            RangeType::Quote(_) => None,
            RangeType::Word(_) => None,
            RangeType::XmlTag => None,

            RangeType::Buffer => {
                if len > 0 {
                    Some(EditRange::inclusive(0.into(), max.into(), TargetShape::LineWise))
                } else {
                    None
                }
            },
            RangeType::Line | RangeType::Paragraph | RangeType::Sentence => {
                let count = ctx.context.resolve(count);
                let end = count.saturating_sub(1).saturating_add(pos.position).min(max);

                if len > 0 {
                    Some(EditRange::inclusive(pos.clone(), end.into(), TargetShape::LineWise))
                } else {
                    None
                }
            },
        }
    }
}

impl<T, I> CursorSearch<ListCursor> for ListState<T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    fn find_char(
        &self,
        _: &ListCursor,
        _: bool,
        _: MoveDir1D,
        _: bool,
        _: char,
        _: usize,
    ) -> Option<ListCursor> {
        return None;
    }

    fn find_regex(
        &self,
        pos: &ListCursor,
        dir: MoveDir1D,
        needle: &Regex,
        count: usize,
    ) -> Option<EditRange<ListCursor>> {
        let mut matches = vec![];

        for (idx, item) in self.items.iter().enumerate() {
            let s = item.to_string();

            if needle.is_match(s.as_str()) {
                matches.push(idx);
            }
        }

        let modulus = matches.len();

        if modulus == 0 {
            return None;
        }

        let i = match dir {
            MoveDir1D::Previous => matches.iter().position(|&idx| idx >= pos.position).unwrap_or(0),
            MoveDir1D::Next => {
                let max = modulus.saturating_sub(1);

                matches.iter().rposition(|&idx| idx <= pos.position).unwrap_or(max)
            },
        };

        idx_offset(i, count, &dir, modulus, true).map(|i| {
            let cursor = ListCursor::from(matches[i]);
            let lw = TargetShape::LineWise;

            EditRange::inclusive(cursor.clone(), cursor, lw)
        })
    }
}

impl<C, T, I> EditorActions<C, Store<I>, I> for ListState<T, I>
where
    C: EditContext,
    T: ListItem<I>,
    I: ApplicationInfo,
{
    fn edit(
        &mut self,
        operation: &EditAction,
        motion: &EditTarget,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match operation {
            EditAction::Motion => {
                if motion.is_jumping() {
                    self.jumped.push(self.cursor.clone());
                }

                let pos = match motion {
                    EditTarget::CurrentPosition | EditTarget::Selection => {
                        return Ok(None);
                    },
                    EditTarget::Boundary(rt, inc, term, count) => {
                        let ctx = CursorMovementsContext {
                            action: operation,
                            view: &self.viewctx,
                            context: ctx,
                        };

                        self.range(&self.cursor, rt, *inc, count, &ctx).map(|r| {
                            match term {
                                MoveTerminus::Beginning => r.start,
                                MoveTerminus::End => r.end,
                            }
                        })
                    },
                    EditTarget::CharJump(mark) | EditTarget::LineJump(mark) => {
                        let mark = ctx.resolve(mark);
                        let cursor = store.cursors.get_mark(self.id.clone(), mark)?;

                        Some(cursor.y.into())
                    },
                    EditTarget::Motion(mt, count) => {
                        let ctx = CursorMovementsContext {
                            action: operation,
                            view: &self.viewctx,
                            context: ctx,
                        };

                        self.movement(&self.cursor, mt, count, &ctx)
                    },
                    EditTarget::Range(_, _, _) => {
                        return Err(EditError::Failure("Cannot use ranges in a list".to_string()));
                    },
                    EditTarget::Search(SearchType::Char(_), _, _) => {
                        let msg = "Cannot perform character search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },
                    EditTarget::Search(SearchType::Regex, flip, count) => {
                        let count = ctx.resolve(count);

                        let dir = ctx.get_search_regex_dir();
                        let dir = flip.resolve(&dir);

                        let needle = match ctx.get_search_regex() {
                            Some(re) => re,
                            None => {
                                let lsearch = store.registers.get(&Register::LastSearch)?;
                                let lsearch = lsearch.value.to_string();

                                Regex::new(lsearch.as_ref())?
                            },
                        };

                        self.find_regex(&self.cursor, dir, &needle, count).map(|r| r.start)
                    },
                    EditTarget::Search(SearchType::Word(_, _), _, _) => {
                        let msg = "Cannot perform word search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },
                };

                if let Some(pos) = pos {
                    self.cursor = pos;
                }

                return Ok(None);
            },
            EditAction::Yank => {
                let cmc = CursorMovementsContext {
                    action: operation,
                    view: &self.viewctx,
                    context: ctx,
                };

                let range = match motion {
                    EditTarget::CurrentPosition | EditTarget::Selection => {
                        Some(self._range_to(self.cursor.clone()))
                    },
                    EditTarget::Boundary(rt, inc, term, count) => {
                        self.range(&self.cursor, rt, *inc, count, &cmc).map(|r| {
                            self._range_to(match term {
                                MoveTerminus::Beginning => r.start,
                                MoveTerminus::End => r.end,
                            })
                        })
                    },
                    EditTarget::CharJump(mark) | EditTarget::LineJump(mark) => {
                        let mark = ctx.resolve(mark);
                        let cursor = store.cursors.get_mark(self.id.clone(), mark)?;

                        Some(self._range_to(cursor.y.into()))
                    },
                    EditTarget::Motion(mt, count) => {
                        self.range_of_movement(&self.cursor, mt, count, &cmc)
                    },
                    EditTarget::Range(rt, inc, count) => {
                        self.range(&self.cursor, rt, *inc, count, &cmc)
                    },
                    EditTarget::Search(SearchType::Char(_), _, _) => {
                        let msg = "Cannot perform character search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },
                    EditTarget::Search(SearchType::Regex, flip, count) => {
                        let count = ctx.resolve(count);

                        let dir = ctx.get_search_regex_dir();
                        let dir = flip.resolve(&dir);

                        let needle = match ctx.get_search_regex() {
                            Some(re) => re,
                            None => {
                                let lsearch = store.registers.get(&Register::LastSearch)?;
                                let lsearch = lsearch.value.to_string();

                                Regex::new(lsearch.as_ref())?
                            },
                        };

                        self.find_regex(&self.cursor, dir, &needle, count)
                    },
                    EditTarget::Search(SearchType::Word(_, _), _, _) => {
                        let msg = "Cannot perform word search in a list";
                        let err = EditError::Failure(msg.into());

                        return Err(err);
                    },
                };

                if let Some(range) = range {
                    let mut yanked = EditRope::from("");

                    for pos in range.start.position..=range.end.position {
                        if let Some(item) = self.items.get(pos) {
                            yanked += EditRope::from(item.to_string());
                            yanked += EditRope::from('\n');
                        } else {
                            break;
                        }
                    }

                    let cell = RegisterCell::new(TargetShape::LineWise, yanked);
                    let register = ctx.get_register().unwrap_or(Register::Unnamed);
                    let mut flags = RegisterPutFlags::NONE;

                    if ctx.get_register_append() {
                        flags |= RegisterPutFlags::APPEND;
                    }

                    store.registers.put(&register, cell, flags)?;
                }

                return Ok(None);
            },

            // Everything else is a modifying action.
            EditAction::ChangeCase(_) => Err(EditError::ReadOnly),
            EditAction::ChangeNumber(_, _) => Err(EditError::ReadOnly),
            EditAction::Delete => Err(EditError::ReadOnly),
            EditAction::Format => Err(EditError::ReadOnly),
            EditAction::Indent(_) => Err(EditError::ReadOnly),
            EditAction::Join(_) => Err(EditError::ReadOnly),
            EditAction::Replace(_) => Err(EditError::ReadOnly),
        }
    }

    fn mark(&mut self, name: Mark, _: &C, store: &mut Store<I>) -> EditResult<EditInfo, I> {
        let cursor = Cursor::new(self.cursor.position, 0);

        store.cursors.set_mark(self.id.clone(), name, cursor);

        Ok(None)
    }

    fn complete(
        &mut self,
        _: &CompletionType,
        _: &CompletionSelection,
        _: &CompletionDisplay,
        _: &C,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let msg = "Cannot complete any text inside a list";
        let err = EditError::Failure(msg.into());

        Err(err)
    }

    fn insert_text(
        &mut self,
        _: &InsertTextAction,
        _: &C,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        Err(EditError::ReadOnly)
    }

    fn selection_command(
        &mut self,
        _: &SelectionAction,
        _: &C,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        Err(EditError::Failure("Cannot perform selection actions in a list".into()))
    }

    fn history_command(
        &mut self,
        act: &HistoryAction,
        _: &C,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match act {
            HistoryAction::Checkpoint => Ok(None),
            HistoryAction::Undo(_) => Err(EditError::Failure("Nothing to undo".into())),
            HistoryAction::Redo(_) => Err(EditError::Failure("Nothing to redo".into())),
        }
    }

    fn cursor_command(
        &mut self,
        act: &CursorAction,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match act {
            CursorAction::Close(_) => Ok(None),
            CursorAction::Rotate(_, _) => Ok(None),
            CursorAction::Split(_) => Ok(None),

            CursorAction::Restore(_) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedCursorGroup);

                // Get saved group.
                let ngroup = store.cursors.get_group(self.id.clone(), &reg)?;

                // Lists don't have groups; override current position.
                if self.jumped.current() != &self.cursor {
                    self.jumped.push(self.cursor.clone());
                }

                self.cursor = ngroup.leader.cursor().y.into();

                Ok(None)
            },
            CursorAction::Save(_) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedCursorGroup);

                // Lists don't have groups; override any previously saved group.
                let cursor = Cursor::new(self.cursor.position, 0);
                let state = CursorState::Location(cursor);
                let group = CursorGroup::new(state, vec![]);

                store.cursors.set_group(self.id.clone(), reg, group)?;

                Ok(None)
            },
        }
    }
}

impl<C, T, I> Editable<C, Store<I>, I> for ListState<T, I>
where
    C: EditContext,
    T: ListItem<I>,
    I: ApplicationInfo,
{
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match act {
            EditorAction::Cursor(act) => self.cursor_command(act, ctx, store),
            EditorAction::Edit(ea, et) => self.edit(&ctx.resolve(ea), et, ctx, store),
            EditorAction::History(act) => self.history_command(act, ctx, store),
            EditorAction::InsertText(act) => self.insert_text(act, ctx, store),
            EditorAction::Mark(name) => self.mark(ctx.resolve(name), ctx, store),
            EditorAction::Selection(act) => self.selection_command(act, ctx, store),

            EditorAction::Complete(sel, ct, disp) => self.complete(sel, ct, disp, ctx, store),
        }
    }
}

impl<C, T, I> Jumpable<C, I> for ListState<T, I>
where
    C: EditContext,
    T: ListItem<I>,
    I: ApplicationInfo,
{
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        _: &C,
    ) -> UIResult<usize, I> {
        match list {
            PositionList::ChangeList => {
                let msg = "No changes to jump to within the list";
                let err = UIError::Failure(msg.into());

                return Err(err);
            },
            PositionList::JumpList => {
                let (len, pos) = match dir {
                    MoveDir1D::Previous => {
                        if self.jumped.future_len() == 0 && *self.jumped.current() != self.cursor {
                            // Push current position if this is the first jump backwards.
                            self.jumped.push(self.cursor.clone());
                        }

                        let plen = self.jumped.past_len();
                        let pos = self.jumped.prev(count);

                        (plen, pos)
                    },
                    MoveDir1D::Next => {
                        let flen = self.jumped.future_len();
                        let pos = self.jumped.next(count);

                        (flen, pos)
                    },
                };

                if len > 0 {
                    self.cursor = pos.clone();
                }

                return Ok(count.saturating_sub(len));
            },
        }
    }
}

impl<C, I, T> Promptable<C, Store<I>, I> for ListState<T, I>
where
    C: EditContext,
    I: ApplicationInfo,
    T: ListItem<I> + Promptable<C, Store<I>, I>,
{
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, C)>, I> {
        if let Some(item) = self.get_mut() {
            return item.prompt(act, ctx, store);
        } else {
            let msg = "No item currently selected";
            let err = EditError::Failure(msg.into());

            return Err(err);
        }
    }
}

impl<C, I, T> Searchable<C, Store<I>, I> for ListState<T, I>
where
    C: EditContext,
    I: ApplicationInfo,
    T: ListItem<I>,
{
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let search = EditTarget::Search(SearchType::Regex, dir, count);

        Ok(self.edit(&EditAction::Motion, &search, ctx, store)?)
    }
}

impl<C, I, T> ScrollActions<C, Store<I>, I> for ListState<T, I>
where
    C: EditContext,
    I: ApplicationInfo,
    T: ListItem<I>,
{
    fn dirscroll(
        &mut self,
        dir: MoveDir2D,
        size: ScrollSize,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        if self.items.is_empty() {
            return Ok(None);
        }

        let count = ctx.resolve(count);
        let height = self.viewctx.get_height();
        let mut corner = self.viewctx.corner.clone();

        let mut rows = match size {
            ScrollSize::Cell => count,
            ScrollSize::HalfPage => count.saturating_mul(height) / 2,
            ScrollSize::Page => count.saturating_mul(height),
        };

        _clamp_cursor(&mut corner, self.items.len());

        match dir {
            MoveDir2D::Up => {
                while rows > 0 {
                    if corner.text_row >= rows {
                        corner.text_row -= rows;
                        break;
                    } else if corner.position == 0 {
                        corner.text_row = 0;
                        break;
                    }

                    rows -= corner.text_row.saturating_add(1);

                    let pos = corner.position.saturating_sub(1);
                    let sel = pos == self.cursor.position;
                    let txt = self.items[pos].show(sel, &self.viewctx, store);

                    corner.position = pos;
                    corner.text_row = txt.height().saturating_sub(1);
                }
            },
            MoveDir2D::Down => {
                let last = self.items.len().saturating_sub(1);

                while rows > 0 {
                    let pos = corner.position;
                    let sel = pos == self.cursor.position;
                    let txt = self.items[pos].show(sel, &self.viewctx, store);
                    let len = txt.height();
                    let max = len.saturating_sub(1);

                    if pos == last {
                        corner.text_row = corner.text_row.saturating_add(rows).min(max);
                        break;
                    } else if corner.text_row >= max {
                        corner.position = pos.saturating_add(1);
                        corner.text_row = 0;

                        rows -= 1;
                    } else if corner.text_row + rows <= max {
                        corner.text_row += rows;
                        break;
                    } else {
                        corner.position = pos.saturating_add(1);
                        corner.text_row = 0;

                        rows -= len - corner.text_row;
                    }
                }
            },
            MoveDir2D::Left | MoveDir2D::Right => {
                let msg = "Cannot scroll horizontally in a list";
                let err = EditError::Failure(msg.into());

                return Err(err);
            },
        };

        self.viewctx.corner = corner;
        self.shift_cursor(store);

        Ok(None)
    }

    fn cursorpos(
        &mut self,
        pos: MovePosition,
        axis: Axis,
        _: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match axis {
            Axis::Horizontal => {
                let msg = "Cannot scroll horizontally in a list";
                let err = EditError::Failure(msg.into());

                return Err(err);
            },
            Axis::Vertical => {
                self.scrollview(self.cursor.position, pos, store);

                return Ok(None);
            },
        }
    }

    fn linepos(
        &mut self,
        pos: MovePosition,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let len = self.items.len();
        let index = ctx.resolve(count).min(len).saturating_sub(1);

        self.scrollview(index, pos, store);
        self.shift_cursor(store);

        Ok(None)
    }
}

impl<C, I, T> Scrollable<C, Store<I>, I> for ListState<T, I>
where
    C: EditContext,
    I: ApplicationInfo,
    T: ListItem<I>,
{
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match style {
            ScrollStyle::Direction2D(dir, size, count) => {
                return self.dirscroll(*dir, *size, count, ctx, store);
            },
            ScrollStyle::CursorPos(pos, axis) => {
                return self.cursorpos(*pos, *axis, ctx, store);
            },
            ScrollStyle::LinePos(pos, count) => {
                return self.linepos(*pos, count, ctx, store);
            },
        }
    }
}

impl<T, I> TerminalCursor for ListState<T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    fn get_term_cursor(&self) -> Option<(u16, u16)> {
        // We highlight the selected text, but don't show the cursor.
        return None;
    }
}

impl<I, T> WindowOps<I> for ListState<T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    fn dup(&self, _: &mut Store<I>) -> Self {
        ListState {
            id: self.id.clone(),
            items: self.items.clone(),
            cursor: self.cursor.clone(),
            viewctx: self.viewctx.clone(),
            jumped: self.jumped.clone(),
        }
    }

    fn close(&mut self, _: CloseFlags, _: &mut Store<I>) -> bool {
        true
    }

    fn write(&mut self, _: Option<&str>, _: WriteFlags, _: &mut Store<I>) -> UIResult<EditInfo, I> {
        Err(UIError::Failure("Cannot write list".into()))
    }

    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut Store<I>) {
        List::new(store).focus(focused).render(area, buf, self);
    }

    fn get_completions(&self) -> Option<CompletionList> {
        None
    }

    fn get_cursor_word(&self, _: &WordStyle) -> Option<String> {
        self.items.get(self.cursor.position).and_then(ListItem::get_word)
    }

    fn get_selected_word(&self) -> Option<String> {
        self.items.get(self.cursor.position).and_then(ListItem::get_word)
    }
}

impl<'a, T, I> List<'a, T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    /// Create a new widget.
    pub fn new(store: &'a mut Store<I>) -> Self {
        List {
            focused: false,
            empty_message: None,
            empty_alignment: Alignment::Left,
            store,
            _p: PhantomData,
        }
    }

    /// Set a message to display when the list is empty.
    pub fn empty_message<X: Into<Text<'a>>>(mut self, text: X) -> Self {
        self.empty_message = Some(text.into());
        self
    }

    /// Set the alignment of the displayed empty message.
    pub fn empty_alignment(mut self, alignment: Alignment) -> Self {
        self.empty_alignment = alignment;
        self
    }

    /// Indicate whether the widget is currently focused.
    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }
}

impl<'a, T, I> StatefulWidget for List<'a, T, I>
where
    T: ListItem<I>,
    I: ApplicationInfo,
{
    type State = ListState<T, I>;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        state.set_term_info(area);

        let height = state.viewctx.get_height();

        if height == 0 {
            return;
        }

        if state.is_empty() {
            if let Some(msg) = self.empty_message {
                Paragraph::new(msg).alignment(self.empty_alignment).render(area, buf);
                return;
            }
        }

        if state.cursor < state.viewctx.corner {
            state.viewctx.corner = state.cursor.clone();
        }

        state._clamp();

        let corner = &state.viewctx.corner;
        let mut lines = vec![];
        let mut sawit = false;

        for (idx, item) in state.items.iter().enumerate().skip(corner.position) {
            let sel = idx == state.cursor.position;
            let txt = item.show(self.focused && sel, &state.viewctx, self.store);

            if sel && txt.lines.len() >= height {
                lines = txt
                    .lines
                    .into_iter()
                    .take(height)
                    .enumerate()
                    .map(|(row, line)| (idx, row, line))
                    .collect();
                break;
            }

            for (row, line) in txt.lines.into_iter().enumerate() {
                if idx == corner.position && row < corner.text_row {
                    continue;
                }

                if sawit && lines.len() >= height {
                    break;
                }

                lines.push((idx, row, line));
            }

            if sel {
                sawit = true;
            }

            if sawit && lines.len() >= height {
                break;
            }
        }

        if lines.len() > height {
            let n = lines.len() - height;
            let _ = lines.drain(..n);
        }

        if let Some((idx, row, _)) = lines.first() {
            state.viewctx.corner.position = *idx;
            state.viewctx.corner.text_row = *row;
        }

        let mut y = area.top();
        let x = area.left();

        for (_, _, txt) in lines.into_iter() {
            let _ = buf.set_spans(x, y, &txt, area.width);

            y += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tui::text::{Span, Spans};

    use crate::{
        editing::action::WindowAction,
        editing::application::EmptyInfo,
        editing::base::OpenTarget,
        env::vim::VimContext,
    };

    #[derive(Clone)]
    struct TestItem {
        book: String,
        author: String,
    }

    impl TestItem {
        fn new(book: &str, author: &str) -> Self {
            let book = book.to_owned();
            let author = author.to_owned();

            TestItem { book, author }
        }
    }

    impl ToString for TestItem {
        fn to_string(&self) -> String {
            self.book.clone()
        }
    }

    impl<I> ListItem<I> for TestItem
    where
        I: ApplicationInfo,
    {
        fn show(&self, selected: bool, _: &ViewportContext<ListCursor>, _: &mut Store<I>) -> Text {
            let style = if selected {
                Style::default().add_modifier(StyleModifier::REVERSED)
            } else {
                Style::default()
            };

            let line1 = Spans::from(Span::styled(self.book.as_str(), style));
            let line2 = Spans::from(vec![Span::from("    by "), Span::from(self.author.as_str())]);

            Text { lines: vec![line1, line2] }
        }
    }

    impl<C, I> Promptable<C, Store<I>, I> for TestItem
    where
        C: EditContext,
        I: ApplicationInfo,
    {
        fn prompt(
            &mut self,
            act: &PromptAction,
            ctx: &C,
            _: &mut Store<I>,
        ) -> EditResult<Vec<(Action<I>, C)>, I> {
            match act {
                PromptAction::Submit => {
                    let target = OpenTarget::Name(self.author.clone());
                    let act = WindowAction::Switch(target);

                    return Ok(vec![(act.into(), ctx.clone())]);
                },
                PromptAction::Abort(_) | PromptAction::Recall(_, _, _) => {
                    let msg = "";
                    let err = EditError::Unimplemented(msg.into());

                    Err(err)
                },
            }
        }
    }

    fn mklist() -> (ListState<TestItem, EmptyInfo>, VimContext<EmptyInfo>, Store<EmptyInfo>) {
        /*
         * This will render as:
         *
         * +------------------------------+
         * |The Wind-Up Bird Chronicle    |
         * |    by Haruki Murakami        |
         * |The Master and Margarita      |
         * |    by Mikhail Bulgakov       |
         * |The Left Hand of Darkness     |
         * |    by Ursula K. Le Guin      |
         * |2666                          |
         * |    by Roberto Bolaño         |
         * |Nevada                        |
         * |    by Imogen Binnie          |
         * |Annihilation                  |
         * |    by Jeff Vandermeer        |
         * |Foucault's Pendulum           |
         * |    by Umberto Eco            |
         * |Monday Starts on Saturday     |
         * |    by Arkady Strugatsky      |
         * +------------------------------+
         */
        let mut list = ListState::new("".to_string(), vec![
            TestItem::new("The Wind-Up Bird Chronicle", "Haruki Murakami"),
            TestItem::new("The Master and Margarita", "Mikhail Bulgakov"),
            TestItem::new("The Left Hand of Darkness", "Ursula K. Le Guin"),
            TestItem::new("2666", "Roberto Bolaño"),
            TestItem::new("Nevada", "Imogen Binnie"),
            TestItem::new("Annihilation", "Jeff Vandermeer"),
            TestItem::new("Foucault's Pendulum", "Umberto Eco"),
            TestItem::new("Monday Starts on Saturday", "Arkady Strugatsky"),
        ]);

        list.viewctx.dimensions.0 = 30;
        list.viewctx.dimensions.1 = 5;

        (list, VimContext::default(), Store::default())
    }

    #[test]
    fn test_render_cursor() {
        let (mut list, _, mut store) = mklist();
        let area = Rect::new(0, 0, 30, 5);
        let mut buffer = Buffer::empty(area);

        // Start out w/ cursor at item 1 and corner at item 0.
        list.cursor = ListCursor::new(1, 0);
        list.viewctx.corner = ListCursor::new(0, 0);

        // Rendering list when cursor is in view doesn't move corner.
        list.draw(area, &mut buffer, true, &mut store);
        assert_eq!(list.cursor, ListCursor::new(1, 0));
        assert_eq!(list.viewctx.corner, ListCursor::new(0, 0));

        // Removing focus doesn't matter.
        list.draw(area, &mut buffer, false, &mut store);
        assert_eq!(list.cursor, ListCursor::new(1, 0));
        assert_eq!(list.viewctx.corner, ListCursor::new(0, 0));

        // Move cursor out of view.
        list.cursor = ListCursor::new(4, 0);

        // Rendering list when cursor is out of view moves corner.
        list.draw(area, &mut buffer, true, &mut store);
        assert_eq!(list.cursor, ListCursor::new(4, 0));
        assert_eq!(list.viewctx.corner, ListCursor::new(2, 1));

        // Reset corner.
        list.viewctx.corner = ListCursor::new(0, 0);

        // Corner still moves when unfocused.
        list.draw(area, &mut buffer, true, &mut store);
        assert_eq!(list.cursor, ListCursor::new(4, 0));
        assert_eq!(list.viewctx.corner, ListCursor::new(2, 1));
    }

    #[test]
    fn test_motion_line() {
        let (mut list, ctx, mut store) = mklist();
        let op = EditAction::Motion;
        let prev = MoveType::Line(MoveDir1D::Previous);
        let next = MoveType::Line(MoveDir1D::Next);

        assert_eq!(list.cursor.position, 0);

        list.edit(&op, &EditTarget::Motion(next.clone(), 1.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 1);

        list.edit(&op, &EditTarget::Motion(next.clone(), 3.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 4);

        list.edit(&op, &EditTarget::Motion(next.clone(), 5.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 7);

        list.edit(&op, &EditTarget::Motion(prev.clone(), 10.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 0);
    }

    #[test]
    fn test_motion_buffer_line_offset() {
        let (mut list, ctx, mut store) = mklist();
        let op = EditAction::Motion;
        let mv = MoveType::BufferLineOffset;

        list.edit(&op, &EditTarget::Motion(mv.clone(), 8.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 7);

        list.edit(&op, &EditTarget::Motion(mv.clone(), 5.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 4);

        list.edit(&op, &EditTarget::Motion(mv.clone(), 1.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 0);

        list.edit(&op, &EditTarget::Motion(mv.clone(), 10.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 7);
    }

    #[test]
    fn test_motion_buffer_line_percent() {
        let (mut list, ctx, mut store) = mklist();
        let op = EditAction::Motion;
        let mv = MoveType::BufferLinePercent;

        list.edit(&op, &EditTarget::Motion(mv.clone(), 100.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 7);

        list.edit(&op, &EditTarget::Motion(mv.clone(), 50.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 3);

        list.edit(&op, &EditTarget::Motion(mv.clone(), 25.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 1);

        list.edit(&op, &EditTarget::Motion(mv.clone(), 0.into()), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 0);
    }

    #[test]
    fn test_motion_buffer_pos() {
        let (mut list, ctx, mut store) = mklist();
        let op = EditAction::Motion;
        let beg = MoveType::BufferPos(MovePosition::Beginning);
        let mid = MoveType::BufferPos(MovePosition::Middle);
        let end = MoveType::BufferPos(MovePosition::End);

        assert_eq!(list.cursor.position, 0);

        list.edit(&op, &end.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 7);

        list.edit(&op, &beg.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 0);

        list.edit(&op, &mid.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 4);
    }

    #[test]
    fn test_motion_viewport() {
        let (mut list, ctx, mut store) = mklist();
        let op = EditAction::Motion;
        let beg = MoveType::ViewportPos(MovePosition::Beginning);

        assert_eq!(list.cursor.position, 0);

        list.viewctx.corner.position = 3;

        list.edit(&op, &beg.clone().into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 3);

        list.viewctx.corner.position = 6;

        list.edit(&op, &beg.clone().into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 6);
    }

    #[test]
    fn test_yank() {
        let (mut list, mut ctx, mut store) = mklist();
        let op = EditAction::Yank;
        let end = EditTarget::Motion(MoveType::BufferPos(MovePosition::End), 1.into());
        list.cursor.position = 4;
        ctx.action.register = Some(Register::Named('c'));

        list.edit(&op, &end, &ctx, &mut store).unwrap();

        let cell = store.registers.get(&Register::Named('c')).unwrap();
        let rope = cell.value;

        assert_eq!(
            rope.to_string(),
            "Nevada\nAnnihilation\nFoucault's Pendulum\nMonday Starts on Saturday\n"
        );
    }

    #[test]
    fn test_search() {
        let (mut list, ctx, mut store) = mklist();

        store.set_last_search("on");

        assert_eq!(list.cursor.position, 0);

        list.search(MoveDir1D::Next.into(), 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 5);

        list.search(MoveDir1D::Next.into(), 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 7);

        list.search(MoveDir1D::Next.into(), 1.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 0);

        list.search(MoveDir1D::Previous.into(), 3.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 0);

        list.search(MoveDir1D::Previous.into(), 2.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 5);

        list.search(MoveDir1D::Previous.into(), 2.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 7);
    }

    #[test]
    fn test_mark_and_jumps() {
        let (mut list, ctx, mut store) = mklist();

        let a = Mark::BufferNamed('a');
        let b = Mark::BufferNamed('b');
        let c = Mark::BufferNamed('c');
        let d = Mark::BufferNamed('d');

        assert_eq!(list.cursor.position, 0);
        list.mark(a, &ctx, &mut store).unwrap();

        list.cursor.position = 2;
        list.mark(b, &ctx, &mut store).unwrap();

        list.cursor.position = 4;
        list.mark(c, &ctx, &mut store).unwrap();

        list.cursor.position = 7;
        list.mark(d, &ctx, &mut store).unwrap();

        let op = EditAction::Motion;

        list.edit(&op, &EditTarget::LineJump(a.into()), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 0);

        list.edit(&op, &EditTarget::LineJump(b.into()), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 2);

        list.edit(&op, &EditTarget::LineJump(c.into()), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 4);

        list.edit(&op, &EditTarget::LineJump(d.into()), &ctx, &mut store).unwrap();
        assert_eq!(list.cursor.position, 7);

        let res = list.jump(PositionList::JumpList, MoveDir1D::Previous, 1, &ctx).unwrap();
        assert_eq!(res, 0);
        assert_eq!(list.cursor.position, 4);

        let res = list.jump(PositionList::JumpList, MoveDir1D::Previous, 1, &ctx).unwrap();
        assert_eq!(res, 0);
        assert_eq!(list.cursor.position, 2);

        let res = list.jump(PositionList::JumpList, MoveDir1D::Previous, 1, &ctx).unwrap();
        assert_eq!(res, 0);
        assert_eq!(list.cursor.position, 0);

        let res = list.jump(PositionList::JumpList, MoveDir1D::Next, 3, &ctx).unwrap();
        assert_eq!(res, 0);
        assert_eq!(list.cursor.position, 7);
    }

    #[test]
    fn test_scroll_dirscroll() {
        let (mut list, ctx, mut store) = mklist();

        assert_eq!(list.cursor.position, 0);
        assert_eq!(list.viewctx.corner.position, 0);

        list.dirscroll(MoveDir2D::Down, ScrollSize::Page, &2.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.viewctx.corner, ListCursor::new(5, 0));
        assert_eq!(list.cursor.position, 5);

        list.dirscroll(MoveDir2D::Up, ScrollSize::Page, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.viewctx.corner, ListCursor::new(2, 1));
        assert_eq!(list.cursor.position, 4);

        list.dirscroll(MoveDir2D::Down, ScrollSize::Cell, &1.into(), &ctx, &mut store)
            .unwrap();
        assert_eq!(list.viewctx.corner, ListCursor::new(3, 0));
        assert_eq!(list.cursor.position, 4);
    }

    #[test]
    fn test_scroll_cursorpos() {
        let (mut list, ctx, mut store) = mklist();

        list.cursor.position = 3;

        list.cursorpos(MovePosition::Beginning, Axis::Vertical, &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 3);
        assert_eq!(list.viewctx.corner, ListCursor::new(3, 0));

        list.cursorpos(MovePosition::Middle, Axis::Vertical, &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 3);
        assert_eq!(list.viewctx.corner, ListCursor::new(2, 1));

        list.cursorpos(MovePosition::End, Axis::Vertical, &ctx, &mut store)
            .unwrap();
        assert_eq!(list.cursor.position, 3);
        assert_eq!(list.viewctx.corner, ListCursor::new(1, 1));
    }

    #[test]
    fn test_scroll_linepos() {
        let (mut list, ctx, mut store) = mklist();

        assert_eq!(list.cursor.position, 0);
        assert_eq!(list.viewctx.corner.position, 0);

        // Cursor is above viewport after scrolling, and gets placed at top.
        list.linepos(MovePosition::Beginning, &5.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.viewctx.corner, ListCursor::new(4, 0));
        assert_eq!(list.cursor.position, 4);

        // Cursor is below viewport after scrolling, and gets placed at bottom.
        list.linepos(MovePosition::Middle, &1.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.viewctx.corner, ListCursor::new(0, 0));
        assert_eq!(list.cursor.position, 2);

        // Cursor is above viewport after scrolling, and gets placed at top.
        list.linepos(MovePosition::Middle, &6.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.viewctx.corner, ListCursor::new(4, 1));
        assert_eq!(list.cursor.position, 4);

        // Cursor stays inside viewport.
        list.linepos(MovePosition::End, &5.into(), &ctx, &mut store).unwrap();
        assert_eq!(list.viewctx.corner, ListCursor::new(2, 1));
        assert_eq!(list.cursor.position, 4);
    }

    #[test]
    fn test_submit() {
        let (mut list, ctx, mut store) = mklist();

        assert_eq!(list.cursor.position, 0);

        let acts = list.prompt(&PromptAction::Submit, &ctx, &mut store).unwrap();
        let target = OpenTarget::Name("Haruki Murakami".into());
        assert_eq!(acts.len(), 1);
        assert_eq!(acts[0].0, WindowAction::Switch(target).into());

        list.cursor.position = 4;

        let acts = list.prompt(&PromptAction::Submit, &ctx, &mut store).unwrap();
        let target = OpenTarget::Name("Imogen Binnie".into());
        assert_eq!(acts.len(), 1);
        assert_eq!(acts[0].0, WindowAction::Switch(target).into());
    }
}
