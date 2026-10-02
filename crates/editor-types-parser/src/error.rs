use std::num::ParseIntError;

/// An error returned by [tokenize][crate::tokenize].
#[derive(Clone, Debug, thiserror::Error, PartialEq)]
pub enum Error {
    /// An error that occurred during the lexing phase.
    #[error("error lexing input: {0}")]
    Lexing(#[from] LexingError),

    /// An error that occurred during the AST-building phase.
    #[error("error building ActionToken tree: {0}")]
    TokenTree(#[from] TokenTreeError),
}

/// An error encountered while after lexing while the [ActionToken][crate::ActionToken] tree.
#[derive(Clone, Debug, thiserror::Error, PartialEq)]
pub enum TokenTreeError {
    /// The parser aborted before it could potentially recur too deeply.
    #[error("DSL input is too deeply nested to parse")]
    TooDeeplyNested,

    /// A syntax error occurred due to not finding one of the expected tokens.
    #[error("syntax error encountered during parsing; expected one of {0:?}")]
    SyntaxError(Vec<String>),

    /// A generic parser failure.
    #[error("failed to parse input")]
    ParseFailed,
}

/// An error occurred while trying to convert the input into a series of basic tokens.
#[derive(Clone, Debug, Default, thiserror::Error, PartialEq)]
pub enum LexingError {
    /// A generic lexer failure.
    #[default]
    #[error("bad input")]
    BadInput,

    /// Found an unexpected sequence of characters.
    #[error("unexpected or incomplete tokens starting near offset {0}: {1:?}")]
    UnexpectedSequence(usize, String),

    /// Found an invalid escape sequence.
    #[error("failed to parse escape sequence: {0}")]
    InvalidEscape(#[from] InvalidEscapeError),

    /// Failed to parse a number in the input.
    #[error("failed to parse integer: {0}")]
    InvalidNumber(#[from] ParseIntError),
}

/// An error encountered while trying to unescape a character or string.
#[derive(Clone, Debug, thiserror::Error, PartialEq)]
#[error(transparent)]
pub struct InvalidEscapeError(pub(crate) unescape_zero_copy::Error);
