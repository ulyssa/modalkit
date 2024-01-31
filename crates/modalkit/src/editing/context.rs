//! # Editing Context
//!
//! ## Overview
//!
//! This module contains the contexts used by the editing buffer.
use super::action::EditAction;
use crate::prelude::*;

/// Trait for values that can be converted by the [EditContext].
pub trait Resolve<T, R> {
    /// Use contextual information to convert a `T` into an `R`.
    fn resolve(&self, t: &T) -> R;
}

/// Context for editing operations.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EditContext {
    pub(crate) operation: EditAction,
    pub(crate) mark: Option<Mark>,
    pub(crate) count: Option<usize>,
    pub(crate) typed: Option<Char>,

    pub(crate) cursor_end: CursorEnd,
    pub(crate) target_shape: Option<TargetShape>,
    pub(crate) insert_style: Option<InsertStyle>,
    pub(crate) last_column: bool,
    pub(crate) register: Option<Register>,
    pub(crate) register_append: bool,
    pub(crate) search_regex_dir: MoveDir1D,
    pub(crate) search_char: Option<(MoveDir1D, bool, Char)>,
    pub(crate) replace_char: Option<Char>,
    pub(crate) search_incremental: bool,
}

impl Default for EditContext {
    fn default() -> Self {
        Self {
            operation: EditAction::Motion,
            mark: None,
            count: None,
            typed: None,

            cursor_end: CursorEnd::Auto,
            target_shape: None,
            insert_style: None,
            last_column: false,
            register: None,
            register_append: false,
            search_regex_dir: MoveDir1D::Next,
            search_char: None,
            replace_char: None,
            search_incremental: false,
        }
    }
}

impl EditContext {
    /// Indicates where to leave the cursor after editing text.
    pub fn get_cursor_end(&self) -> CursorEnd {
        self.cursor_end
    }

    /// Indicates a shape to be applied to an [EditAction].
    pub fn get_target_shape(&self) -> Option<TargetShape> {
        self.target_shape
    }

    /// Indicates the style by which text should be inserted into the buffer.
    pub fn get_insert_style(&self) -> Option<InsertStyle> {
        self.insert_style
    }

    /// Indicates whether it is okay to move the cursor into the last column of a line.
    pub fn get_last_column(&self) -> bool {
        self.last_column
    }

    /// Indicates which register yanked and deleted text should go to.
    pub fn get_register(&self) -> Option<Register> {
        self.register.clone()
    }

    /// Indicates whether should be appended to the target register when yanking or deleting text.
    pub fn get_register_append(&self) -> bool {
        self.register_append
    }

    /// Get the direction in which to search.
    pub fn get_search_regex_dir(&self) -> MoveDir1D {
        self.search_regex_dir
    }

    /// Returns a character to search for on the current line, and the direction in
    /// which to search.
    pub fn get_search_char(&self) -> Option<(MoveDir1D, bool, Char)> {
        self.search_char.clone()
    }

    /// Returns a [character](Char) to use when performing an [EditAction::Replace] operation.
    pub fn get_replace_char(&self) -> Option<Char> {
        self.replace_char.clone()
    }

    /// Whether to perform incremental searches while typing in the search bar.
    pub fn is_search_incremental(&self) -> bool {
        self.search_incremental
    }
}

impl Resolve<Count, usize> for EditContext {
    fn resolve(&self, count: &Count) -> usize {
        match count {
            Count::Contextual => self.count.unwrap_or(1),
            Count::MinusOne => self.count.unwrap_or(0).saturating_sub(1),
            Count::Exact(n) => *n,
        }
    }
}

impl Resolve<Specifier<Char>, Option<Char>> for EditContext {
    fn resolve(&self, c: &Specifier<Char>) -> Option<Char> {
        match c {
            Specifier::Contextual => self.typed.clone(),
            Specifier::Exact(c) => Some(c.clone()),
        }
    }
}

impl Resolve<Specifier<Mark>, Mark> for EditContext {
    fn resolve(&self, mark: &Specifier<Mark>) -> Mark {
        match mark {
            Specifier::Contextual => self.mark.unwrap_or(Mark::LastJump),
            Specifier::Exact(m) => *m,
        }
    }
}

impl Resolve<Specifier<EditAction>, EditAction> for EditContext {
    fn resolve(&self, mark: &Specifier<EditAction>) -> EditAction {
        match mark {
            Specifier::Contextual => self.operation.clone(),
            Specifier::Exact(a) => a.clone(),
        }
    }
}

/// Build a new [EditContext].
#[derive(Default)]
pub struct EditContextBuilder(EditContext);

impl EditContextBuilder {
    /// Finish building the [EditContext].
    pub fn build(self) -> EditContext {
        self.0
    }

    /// Set the [CursorEnd].
    pub fn cursor_end(mut self, v: CursorEnd) -> Self {
        self.0.cursor_end = v;
        self
    }

    /// Set the [TargetShape].
    pub fn target_shape(mut self, v: Option<TargetShape>) -> Self {
        self.0.target_shape = v;
        self
    }

    /// Set the [InsertStyle].
    pub fn insert_style(mut self, v: Option<InsertStyle>) -> Self {
        self.0.insert_style = v;
        self
    }

    /// Set whether it's okay to move the cursor into the last column.
    pub fn last_column(mut self, v: bool) -> Self {
        self.0.last_column = v;
        self
    }

    /// Set the [Register].
    pub fn register(mut self, v: Option<Register>) -> Self {
        self.0.register = v;
        self
    }

    /// Set whether this operation should append contents to the register or replace the existing
    /// ones.
    pub fn register_append(mut self, v: bool) -> Self {
        self.0.register_append = v;
        self
    }

    /// Set the direction the regular expression should search in.
    pub fn search_regex_dir(mut self, v: MoveDir1D) -> Self {
        self.0.search_regex_dir = v;
        self
    }

    /// Set a character to search for.
    pub fn search_char(mut self, v: Option<(MoveDir1D, bool, Char)>) -> Self {
        self.0.search_char = v;
        self
    }

    /// Set a [Char] to replace existing characters with.
    pub fn replace_char(mut self, v: Option<Char>) -> Self {
        self.0.replace_char = v;
        self
    }

    /// Set whether we should do an incremental search.
    pub fn search_incremental(mut self, v: bool) -> Self {
        self.0.search_incremental = v;
        self
    }

    /// Set the contextual [Mark].
    pub fn mark(mut self, mark: Option<Mark>) -> Self {
        self.0.mark = mark;
        self
    }

    /// Set the contextual [Mark].
    pub fn typed_char(mut self, ch: Option<Char>) -> Self {
        self.0.typed = ch;
        self
    }

    /// Set the contextual [Mark].
    pub fn operation(mut self, op: EditAction) -> Self {
        self.0.operation = op;
        self
    }

    /// Set the contextual count.
    pub fn count(mut self, n: Option<usize>) -> Self {
        self.0.count = n;
        self
    }
}

impl From<EditContext> for EditContextBuilder {
    fn from(ctx: EditContext) -> Self {
        EditContextBuilder(ctx)
    }
}
