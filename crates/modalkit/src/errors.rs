//! # Error Types
//!
//! ##
//!
//! This module contains error types that group together some of the more specific errors returned
//! by components and utilities in this crate.
use crate::actions::Action;
use crate::commands::CommandError;
use crate::editing::application::ApplicationInfo;
use crate::key::MacroError;
use crate::keybindings::dialog::Dialog;
use crate::prelude::*;

/// Errors returned from editing operation.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum EditError<I: ApplicationInfo> {
    /// Run an interactive dialog to determine how to complete this action.
    #[error("That action requires interactive confirmation")]
    NeedConfirm(Box<dyn Dialog<Action<I>>>),

    /// Failure to fetch a word at a cursor position.
    #[error("No word underneath cursor")]
    NoCursorWord,

    /// Failure to determine a search expression to use.
    #[error("No current search specified")]
    NoSearch,

    /// Failure due to lack of a current selection.
    #[error("No current selection")]
    NoSelection,

    /// Failure due to an umapped digraph.
    #[error("Invalid digraph: {0:?} {1:?}")]
    InvalidDigraph(char, char),

    /// Failure due to a bad regular expression.
    #[error("Invalid regular expression: {0}")]
    InvalidRegex(#[from] regex::Error),

    /// Failure due to an unset [Mark].
    #[error("Mark not set")]
    MarkNotSet(Mark),

    /// Failure due to referencing a cursor position in another buffer.
    #[error("Position is located in another buffer")]
    WrongBuffer(I::ContentId),

    /// Failure while combining cursor groups.
    #[error("Failed to combine cursor groups: {0}")]
    CursorGroupCombine(#[from] crate::editing::cursor::CursorGroupCombineError),

    /// Failure due to invalid input where an integer was expected.
    #[error("Integer conversion error: {0}")]
    IntConversionError(#[from] std::num::TryFromIntError),

    /// Failure due to invalid input where an integer was expected.
    #[error("Integer parsing error: {0}")]
    IntParseError(#[from] std::num::ParseIntError),

    /// Failure due to an unimplemented feature.
    #[error("Buffer is read-only")]
    ReadOnly,

    /// Failure due to an unimplemented feature.
    #[error("Unimplemented: {0}")]
    Unimplemented(String),

    /// Macro-related failure.
    #[error("Macro error: {0}")]
    MacroFailure(#[from] MacroError),

    /// Error while getting or putting a value into the register store.
    #[error("Register Error: {0}")]
    Register(#[from] crate::editing::store::RegisterError),

    /// Generic failure.
    #[error("Error: {0}")]
    Failure(String),
}

/// Wrapper for various Errors that consumers may want to combine.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum UIError<I>
where
    I: ApplicationInfo,
{
    /// Failure in application-specific code.
    #[error("{0}")]
    Application(I::Error),

    /// Failure during Input/Output.
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),

    /// Failure during editing.
    #[error("Editing error: {0}")]
    EditingFailure(#[from] EditError<I>),

    /// Failure while attempting to execute a command.
    #[error("Failed command: {0}")]
    CommandFailure(#[from] CommandError),

    /// Run an interactive dialog to determine how to complete this action.
    #[error("That action requires interactive confirmation")]
    NeedConfirm(Box<dyn Dialog<Action<I>>>),

    /// Failure while attempting to jump to previous positions.
    #[error("No previous positions in list")]
    NoList(PositionList),

    /// Failure when there's no currently selected tab.
    #[error("No tab currently selected")]
    NoTab,

    /// Failure when there's no currently selected window.
    #[error("No window currently selected")]
    NoWindow,

    /// Failure due to an unimplemented feature.
    #[error("Unimplemented: {0}")]
    Unimplemented(String),

    /// Generic failure.
    #[error("Error: {0}")]
    Failure(String),
}

/// Common result type for editing operations.
pub type EditResult<V, I> = Result<V, EditError<I>>;

/// Common result type for rendering and application functions.
pub type UIResult<V, I> = Result<V, UIError<I>>;
