//! # Editing Context
//!
//! ## Overview
//!
//! This module contains the contexts used by the editing buffer.
use regex::Regex;

use crate::input::InputContext;

use super::action::EditAction;
use super::base::*;

/// Trait for context objects used during editing operations.
pub trait EditContext:
    InputContext
    + Resolve<Specifier<Char>, Option<Char>>
    + Resolve<Specifier<Mark>, Mark>
    + Resolve<Specifier<EditAction>, EditAction>
    + Resolve<Count, usize>
{
    /// Indicates where to leave the cursor after editing text.
    fn get_cursor_end(&self) -> CursorEnd {
        CursorEnd::Auto
    }

    /// Indicates a shape to be applied to an [EditAction].
    fn get_target_shape(&self) -> Option<TargetShape>;

    /// Indicates the style by which text should be inserted into the buffer.
    fn get_insert_style(&self) -> Option<InsertStyle>;

    /// Indicates whether it is okay to move the cursor into the last column of a line.
    fn get_last_column(&self) -> bool;

    /// Indicates which register yanked and deleted text should go to.
    fn get_register(&self) -> Option<Register>;

    /// Indicates whether should be appended to the target register when yanking or deleting text.
    fn get_register_append(&self) -> bool;

    /// Returns a regular expression to search for in the buffer.
    ///
    /// If the context doesn't specify a search regex, then consumers should fall back to using
    /// the contents of [Register::LastSearch].
    fn get_search_regex(&self) -> Option<Regex>;

    /// Get the direction in which to search.
    fn get_search_regex_dir(&self) -> MoveDir1D;

    /// Returns a character to search for on the current line, and the direction in
    /// which to search.
    fn get_search_char(&self) -> Option<(MoveDir1D, bool, Char)>;

    /// Returns a [character](Char) to use when performing an [EditAction::Replace] operation.
    fn get_replace_char(&self) -> Option<Char>;

    /// Whether to perform incremental searches while typing in the search bar.
    fn is_search_incremental(&self) -> bool;
}

/// Trait for values that can be converted by the [EditContext].
pub trait Resolve<T, R> {
    /// Use contextual information to convert a `T` into an `R`.
    fn resolve(&self, t: &T) -> R;
}
