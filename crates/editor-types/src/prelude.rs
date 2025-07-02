//! # Common set of types used for describing actions
//!
//! ## Overview
//!
//! These types are used to specify the details of how to execute [Action] and the
//! more specific actions that it encompasses.
//!
//! Usually you will just want to import everything in this module into your application via:
//!
//! ```
//! use editor_types::prelude::*;
//! ```
//!
//! [Action]: crate::Action
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::Hash;

use bitflags::bitflags;
use regex::Regex;

use crate::application::ApplicationWindowId;
use crate::context::EditContext;
use crate::util::{
    is_filename_char,
    is_filepath_char,
    is_horizontal_space,
    is_keyword,
    is_newline,
    is_space_char,
    is_word_char,
    sort2,
};
use crate::*;

/// Specify how to change the case of a string.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Case {
    /// Make the targeted text uppercase.
    Upper,

    /// Make the targeted text lowercase.
    Lower,

    /// Make the first character of the targeted text uppercase, and the rest lowercase.
    Title,

    /// Toggle the case of each character in the targeted text.
    Toggle,
}

/// Specify how to join lines together.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum JoinStyle {
    /// Leave whitespace around the join point as-is.
    NoChange,

    /// Replace whitespace around the join point with a single space.
    OneSpace,

    /// Always insert a new space at the join point, regardless of whether there's already
    /// whitespace there.
    NewSpace,
}

/// Specify how to insert register contents into a buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PasteStyle {
    /// Paste text before the cursor.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let style = PasteStyle::Cursor;
    /// let paste: Action = action!("insert paste -s cursor");
    /// assert_eq!(paste, InsertTextAction::Paste(style, Count::Contextual).into());
    /// ```
    Cursor,

    /// Paste text before the selection's start, or after its end.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let style = PasteStyle::Side(MoveDir1D::Next);
    /// let paste: Action = action!("insert paste -s (side -d next)");
    /// assert_eq!(paste, InsertTextAction::Paste(style, Count::Contextual).into());
    ///
    /// let style = PasteStyle::Side(MoveDir1D::Previous);
    /// let paste: Action = action!("insert paste -s (side -d prev)");
    /// assert_eq!(paste, InsertTextAction::Paste(style, Count::Contextual).into());
    /// ```
    Side(MoveDir1D),

    /// Replace selected text with register contents.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let style = PasteStyle::Replace;
    /// let paste: Action = action!("insert paste -s replace");
    /// assert_eq!(paste, InsertTextAction::Paste(style, Count::Contextual).into());
    /// ```
    Replace,
}

/// The source to search for completion candidates.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionScope {
    /// Only use completion candidates from the current buffer.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Line(CompletionScope::Buffer);
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T (line buffer) -D list"));
    /// ```
    Buffer,

    /// Use completion candidates available from all buffers.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Line(CompletionScope::Global);
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T (line global) -D list"));
    /// ```
    Global,
}

/// What type of phrase we are completing.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionStyle {
    /// Navigate through the list of completion candidates.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::List(MoveDir1D::Next);
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s (list next) -T auto -D list"));
    /// ```
    List(MoveDir1D),

    /// Generate completion candidates, but don't select any from the list.
    ///
    /// This is most helpful for keybindings that allow the user to get a
    /// list of potential candidates that they can read through without
    /// actually picking any, in case they don't know what the first
    /// character to type is.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::None;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s none -T auto -D list"));
    /// ```
    None,

    /// Complete only the longest common prefix from the completion candidates.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T auto -D list"));
    /// ```
    Prefix,

    /// If there is only a single completion candidate, select it.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::Single;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s single -T auto -D list"));
    /// ```
    Single,
}

/// What type of phrase we are completing.
///
/// Typically, most editors use the cursor's context to determine what to
/// complete. In those cases, [CompletionType::Auto] is sufficient, but other
/// variants are provided here to accomodate keybindings that specifically
/// complete context-independent values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionType {
    /// Determine what to complete by the buffer context.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T auto -D list"));
    /// ```
    Auto,

    /// Complete a filename.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::File;
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T file -D list"));
    /// ```
    File,

    /// Complete the rest of the line.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Line(CompletionScope::Global);
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T (line global) -D list"));
    /// ```
    Line(CompletionScope),

    /// Complete the current word.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Word(CompletionScope::Buffer);
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T (word buffer) -D list"));
    /// ```
    Word(CompletionScope),
}

/// How to display completion candidates.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionDisplay {
    /// Don't display candidates.
    ///
    /// This method of displaying completions is most useful for contexts where
    /// users don't expect to see possible completions and just want to cycle
    /// through what's available, such as completing filenames in a command bar.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::None;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T auto -D none"));
    /// ```
    None,

    /// Display candidates in a bar above the command bar.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::Bar;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T auto -D bar"));
    /// ```
    Bar,

    /// Display candidates in a pop-up list.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    /// assert_eq!(act, action!("complete -s prefix -T auto -D list"));
    /// ```
    List,
}

/// Specify what is targeted by an editing action.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum EditTarget {
    /// Move to one of the sides of a range.
    Boundary(RangeType, bool, MoveTerminus, Count),

    /// Target the current cursor position.
    CurrentPosition,

    /// Move to the line and column of a [Mark].
    CharJump(Specifier<Mark>),

    /// Move to the first word of the line that [Mark] is on.
    LineJump(Specifier<Mark>),

    /// Target the text between the current cursor position and the end of a motion.
    Motion(MoveType, Count),

    /// Target a range of text around the cursor.
    ///
    /// [bool] indicates if this is an inclusive range, when applicable to the [RangeType].
    Range(RangeType, bool, Count),

    /// Target the text between the current cursor position and the end of a search.
    ///
    /// The [MoveDirMod] parameter modifies the search direction.
    Search(SearchType, MoveDirMod, Count),

    /// Target the visually selected text.
    Selection,
}

impl EditTarget {
    /// Returns `true` if this is a target that causes cursor positions to be saved to
    /// [PositionList::JumpList].
    pub fn is_jumping(&self) -> bool {
        match self {
            EditTarget::Boundary(..) => true,
            EditTarget::CurrentPosition => false,
            EditTarget::CharJump(_) => true,
            EditTarget::LineJump(_) => true,
            EditTarget::Motion(mt, _) => mt.is_jumping(),
            EditTarget::Range(..) => true,
            EditTarget::Search(st, ..) => st.is_jumping(),
            EditTarget::Selection => false,
        }
    }
}

impl From<MoveType> for EditTarget {
    fn from(mt: MoveType) -> Self {
        EditTarget::Motion(mt, Count::Contextual)
    }
}

impl From<RangeType> for EditTarget {
    fn from(mt: RangeType) -> Self {
        EditTarget::Range(mt, true, Count::Contextual)
    }
}

/// Determines where to leave the cursor after editing text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CursorEnd {
    /// Keep the current cursor position as best as possible.
    Keep,

    /// Place the cursor at the start of the [EditTarget].
    Start,

    /// Place the cursor at the end of the [EditTarget].
    End,

    /// Select from the start to the end of the [EditTarget].
    Selection,

    /// Use the default cursor end position for the operation.
    Auto,
}

/// Description of a textual range within a buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EditRange<Cursor> {
    /// The start of the range.
    pub start: Cursor,

    /// The end of the range.
    pub end: Cursor,

    /// The default shape to interpret the range as. This might be overriden by
    /// [EditContext::get_target_shape].
    pub shape: TargetShape,

    /// Whether to include the character at the end Cursor when interpreted as a CharWise range.
    pub inclusive: bool,
}

impl<Cursor: Ord> EditRange<Cursor> {
    /// Create a new editing range.
    pub fn new(a: Cursor, b: Cursor, shape: TargetShape, inclusive: bool) -> Self {
        let (start, end) = sort2(a, b);

        EditRange { start, end, shape, inclusive }
    }

    /// Create a new inclusive editing range.
    pub fn inclusive(a: Cursor, b: Cursor, shape: TargetShape) -> Self {
        Self::new(a, b, shape, true)
    }

    /// Create a new exclusive editing range.
    pub fn exclusive(a: Cursor, b: Cursor, shape: TargetShape) -> Self {
        Self::new(a, b, shape, false)
    }
}

/// Different action sequences that can be repeated.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RepeatType {
    /// A sequence of changes made to a buffer.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let rep: Action = action!("repeat -s edit-sequence");
    /// assert_eq!(rep, Action::Repeat(RepeatType::EditSequence));
    /// ```
    EditSequence,

    /// The last [Action] done.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let rep: Action = action!("repeat -s last-action");
    /// assert_eq!(rep, Action::Repeat(RepeatType::LastAction));
    /// ```
    LastAction,

    /// The last selection resize made in a buffer.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let rep: Action = action!("repeat -s last-selection");
    /// assert_eq!(rep, Action::Repeat(RepeatType::LastSelection));
    /// ```
    LastSelection,
}

/// Specify a range within the text around the current cursor position.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SearchType {
    /// Search for the character indicated by [EditContext::get_search_char].
    ///
    /// [bool] controls whether the search should continue across line boundaries.
    Char(bool),

    /// Search for a regular expression.
    Regex,

    /// Search for the word currently under the cursor, and update the last [CommandType::Search]
    /// value in the application's register store.
    ///
    /// [bool] controls whether matches should be checked for using word boundaries.
    Word(WordStyle, bool),
}

impl SearchType {
    /// Returns `true` if this is an inclusive motion.
    pub fn is_inclusive_motion(&self) -> bool {
        match self {
            SearchType::Char(..) => true,
            SearchType::Regex => false,
            SearchType::Word(..) => false,
        }
    }

    /// Returns `true` if this is a search that causes cursor positions to be saved to
    /// [PositionList::JumpList].
    fn is_jumping(&self) -> bool {
        match self {
            SearchType::Char(..) => false,
            SearchType::Regex => true,
            SearchType::Word(..) => true,
        }
    }
}

/// The different ways of grouping a buffer's contents into words.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WordStyle {
    /// A run of alphanumeric characters.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::AlphaNum;
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word alphanum)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word alpha-num)"));
    /// ```
    AlphaNum,

    /// A sequence of non-blank characters.
    ///
    /// An empty line is also a Big word. Vim calls this a `WORD`.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Big;
    /// let kw: Action = Action::KeywordLookup(style.into());
    /// assert_eq!(kw, action!("keyword-lookup -t (word big)"));
    /// ```
    Big,

    /// A sequence of characters that match a test function.
    CharSet(fn(char) -> bool),

    /// A name of a directory or file.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::FileName;
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word filename)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word file-name)"));
    /// ```
    FileName,

    /// A path to a directory or file.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::FilePath;
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word filepath)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word file-path)"));
    /// ```
    FilePath,

    /// Either a sequence of alphanumeric characters and underscores, or a sequence of other
    /// non-blank characters.
    ///
    /// An empty line is also a Little word.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Little;
    /// let kw: Action = Action::KeywordLookup(style.into());
    /// assert_eq!(kw, action!("keyword-lookup -t (word little)"));
    /// ```
    Little,

    /// A run of non-alphanumeric characters.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::NonAlphaNum;
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word non-alphanum)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word non-alphanumeric)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word nonalphanum)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word nonalphanumeric)"));
    /// ```
    NonAlphaNum,

    /// A run of digits in the given base, with an optional leading hyphen.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Number(Radix::Decimal);
    /// let kw: Action = Action::KeywordLookup(style.into());
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix decimal)"));
    /// ```
    Number(Radix),

    /// A run of blank characters.
    ///
    /// [bool] controls whether this crosses line boundaries.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Whitespace(true);
    /// let kw: Action = Action::KeywordLookup(style.into());
    /// assert_eq!(kw, action!("keyword-lookup -t (word whitespace -w true)"));
    ///
    /// let style = WordStyle::Whitespace(false);
    /// let kw: Action = Action::KeywordLookup(style.into());
    /// assert_eq!(kw, action!("keyword-lookup -t (word whitespace -w false)"));
    /// ```
    Whitespace(bool),
}

impl WordStyle {
    /// Whether this [WordStyle] can ever contain the character `c`.
    pub fn contains(&self, c: char) -> bool {
        match self {
            WordStyle::AlphaNum => is_word_char(c),
            WordStyle::NonAlphaNum => !is_word_char(c),
            WordStyle::Big => !is_space_char(c),
            WordStyle::CharSet(f) => f(c),
            WordStyle::FileName => is_filename_char(c),
            WordStyle::FilePath => is_filepath_char(c),
            WordStyle::Little => is_word_char(c) || is_keyword(c),
            WordStyle::Number(radix) => radix.contains(c),
            WordStyle::Whitespace(true) => is_space_char(c),
            WordStyle::Whitespace(false) => is_horizontal_space(c),
        }
    }
}

impl BoundaryTest for WordStyle {
    fn is_boundary_begin(&self, ctx: &BoundaryTestContext) -> bool {
        match self {
            WordStyle::AlphaNum => {
                if ctx.after.is_none() && ctx.dir == MoveDir1D::Next {
                    // Last character is counted when moving forward.
                    return true;
                } else if let Some(before) = ctx.before {
                    let befwc = is_word_char(before);
                    let curwc = is_word_char(ctx.current);

                    return !befwc && curwc;
                } else {
                    // First character is always counted.
                    return true;
                }
            },
            WordStyle::NonAlphaNum => {
                if ctx.after.is_none() && ctx.dir == MoveDir1D::Next {
                    // Last character is counted when moving forward.
                    return true;
                } else if let Some(before) = ctx.before {
                    let befwc = is_word_char(before);
                    let curwc = is_word_char(ctx.current);

                    return befwc && !curwc;
                } else {
                    // First character is always counted.
                    return true;
                }
            },
            WordStyle::Big => {
                if ctx.after.is_none() && ctx.dir == MoveDir1D::Next {
                    // Last character is counted when moving forward.
                    return true;
                } else if let Some(before) = ctx.before {
                    let befws = is_space_char(before);
                    let curws = is_space_char(ctx.current);
                    let curnl = is_newline(ctx.current);

                    // The final word beginning is calculated differently during an operation.
                    let last = !ctx.motion && ctx.count == 1;

                    return (last && curnl) || (befws && !curws);
                } else {
                    // First character is always counted.
                    return true;
                }
            },
            WordStyle::CharSet(f) => {
                if let Some(before) = ctx.before {
                    f(ctx.current) && !f(before)
                } else {
                    f(ctx.current)
                }
            },
            WordStyle::FileName => {
                if let Some(before) = ctx.before {
                    is_filename_char(ctx.current) && !is_filename_char(before)
                } else {
                    is_filename_char(ctx.current)
                }
            },
            WordStyle::FilePath => {
                if let Some(before) = ctx.before {
                    is_filepath_char(ctx.current) && !is_filepath_char(before)
                } else {
                    is_filepath_char(ctx.current)
                }
            },
            WordStyle::Little => {
                if ctx.after.is_none() && ctx.dir == MoveDir1D::Next {
                    // Last character is counted when moving forward.
                    return true;
                } else if let Some(before) = ctx.before {
                    let befwc = is_word_char(before);
                    let befkw = is_keyword(before);
                    let curwc = is_word_char(ctx.current);
                    let curkw = is_keyword(ctx.current);
                    let curnl = is_newline(ctx.current);

                    // The final word beginning is calculated differently during an operation.
                    let last = !ctx.motion && ctx.count == 1;

                    return (last && curnl) ||
                        (befwc && curkw) ||
                        (befkw && curwc) ||
                        (!befwc && curwc) ||
                        (!befkw && curkw);
                } else {
                    // First character is always counted.
                    return true;
                }
            },
            WordStyle::Number(radix) => {
                let cn = radix.contains(ctx.current);

                if ctx.current == '-' {
                    // A hyphen is only the start of a number if a digit follows it.
                    matches!(ctx.after, Some(c) if radix.contains(c))
                } else if let Some(before) = ctx.before {
                    // Not preceded by a hyphen or digit.
                    cn && before != '-' && !radix.contains(before)
                } else {
                    // First character counts if it's a digit.
                    cn
                }
            },
            WordStyle::Whitespace(multiline) => {
                let f = if *multiline {
                    is_space_char
                } else {
                    is_horizontal_space
                };

                return f(ctx.current) && matches!(ctx.before, Some(c) if !f(c));
            },
        }
    }

    fn is_boundary_end(&self, ctx: &BoundaryTestContext) -> bool {
        match self {
            WordStyle::AlphaNum => {
                if ctx.before.is_none() && ctx.dir == MoveDir1D::Previous {
                    // First character is counted when moving back.
                    return true;
                } else if let Some(after) = ctx.after {
                    let curwc = is_word_char(ctx.current);
                    let aftwc = is_word_char(after);

                    return curwc && !aftwc;
                } else {
                    // Last character is always counted.
                    return true;
                }
            },
            WordStyle::NonAlphaNum => {
                if ctx.before.is_none() && ctx.dir == MoveDir1D::Previous {
                    // First character is counted when moving back.
                    return true;
                } else if let Some(after) = ctx.after {
                    let curwc = is_word_char(ctx.current);
                    let aftwc = is_word_char(after);

                    return !curwc && aftwc;
                } else {
                    // Last character is always counted.
                    return true;
                }
            },
            WordStyle::Big => {
                if ctx.before.is_none() && ctx.dir == MoveDir1D::Previous {
                    // First character is counted when moving back.
                    return true;
                } else if let Some(after) = ctx.after {
                    !is_space_char(ctx.current) && is_space_char(after)
                } else {
                    // Last character is always a word ending.
                    return true;
                }
            },
            WordStyle::CharSet(f) => {
                if let Some(after) = ctx.after {
                    f(ctx.current) && !f(after)
                } else {
                    f(ctx.current)
                }
            },
            WordStyle::FileName => {
                if let Some(after) = ctx.after {
                    is_filename_char(ctx.current) && !is_filename_char(after)
                } else {
                    is_filename_char(ctx.current)
                }
            },
            WordStyle::FilePath => {
                if let Some(after) = ctx.after {
                    is_filepath_char(ctx.current) && !is_filepath_char(after)
                } else {
                    is_filepath_char(ctx.current)
                }
            },
            WordStyle::Little => {
                if ctx.before.is_none() && ctx.dir == MoveDir1D::Previous {
                    // First character is counted when moving back.
                    return true;
                } else if let Some(after) = ctx.after {
                    let curwc = is_word_char(ctx.current);
                    let curkw = is_keyword(ctx.current);
                    let aftwc = is_word_char(after);
                    let aftkw = is_keyword(after);

                    return (curwc && aftkw) ||
                        (curkw && aftwc) ||
                        (curwc && !aftwc) ||
                        (curkw && !aftkw);
                } else {
                    // Last character is always counted.
                    return true;
                }
            },
            WordStyle::Number(radix) => {
                if let Some(after) = ctx.after {
                    return radix.contains(ctx.current) && !radix.contains(after);
                } else {
                    return radix.contains(ctx.current);
                }
            },
            WordStyle::Whitespace(multiline) => {
                let f = if *multiline {
                    is_space_char
                } else {
                    is_horizontal_space
                };

                return f(ctx.current) && matches!(ctx.after, Some(c) if !f(c));
            },
        }
    }
}

impl From<Radix> for WordStyle {
    fn from(radix: Radix) -> Self {
        WordStyle::Number(radix)
    }
}

/// Specify the base for a number.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Radix {
    /// A base 2 number.
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Number(Radix::Binary);
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix 2)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix bin)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix binary)"));
    /// ```
    Binary,

    /// A base 8 number.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Number(Radix::Octal);
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix 8)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix oct)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix octal)"));
    /// ```
    Octal,

    /// A base 10 number.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Number(Radix::Decimal);
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix 10)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix dec)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix decimal)"));
    /// ```
    Decimal,

    /// A base 16 number.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let style = WordStyle::Number(Radix::Hexadecimal);
    /// let kw: Action = Action::KeywordLookup(style.into());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix 16)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix hex)"));
    /// assert_eq!(kw, action!("keyword-lookup -t (word radix hexadecimal)"));
    /// ```
    Hexadecimal,
}

impl Radix {
    /// Test whether a character is used by this base.
    pub fn contains(&self, c: char) -> bool {
        match self {
            Radix::Binary => c == '0' || c == '1',
            Radix::Octal => c >= '0' && c <= '7',
            Radix::Decimal => c.is_ascii_digit(),
            Radix::Hexadecimal => c.is_ascii_hexdigit(),
        }
    }
}

/// Contextual information given while searching for the boundary of a range.
pub struct BoundaryTestContext {
    /// The current candidate character for the object boundary search.
    pub current: char,

    /// The character that comes before the candidate in the text.
    pub before: Option<char>,

    /// The character that comes after the candidate in the text.
    pub after: Option<char>,

    /// The direction the search is moving in.
    pub dir: MoveDir1D,

    /// Whether we are performing this search as part of a cursor movement.
    pub motion: bool,

    /// How many boundaries we have left to find.
    pub count: usize,
}

/// Trait for types which have simple start and end boundaries within a text document.
///
/// Boundaries are searched for a character at a time, with the previous and following character
/// context provided if available.
pub trait BoundaryTest {
    /// Check whether we are at the beginning of the range.
    fn is_boundary_begin(&self, ctx: &BoundaryTestContext) -> bool;

    /// Check whether we are at the end of the range.
    fn is_boundary_end(&self, ctx: &BoundaryTestContext) -> bool;

    /// Check whether we are at the given side of the range.
    fn is_boundary(&self, terminus: MoveTerminus, ctx: &BoundaryTestContext) -> bool {
        match terminus {
            MoveTerminus::Beginning => self.is_boundary_begin(ctx),
            MoveTerminus::End => self.is_boundary_end(ctx),
        }
    }
}

/// Specify a range within the text around the current cursor position.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RangeType {
    /// Select from the beginning to the end of a [word](WordStyle).
    Word(WordStyle),

    /// Select the whole buffer.
    Buffer,

    /// Select the current paragraph the cursor is in.
    Paragraph,

    /// Select the current sentence the cursor is in.
    Sentence,

    /// Select the current line the cursor is on.
    Line,

    /// Select the current block specified by the start and end characters.
    ///
    /// When done inclusively, the delimiters are included.
    Bracketed(char, char),

    /// Select the range enclosed by the next item character.
    ///
    /// This is the ranged version of [MoveType::ItemMatch].
    Item,

    /// Select text quoted by [char] around the cursor.
    ///
    /// When done inclusively, the quote characters are included.
    Quote(char),

    /// Select the XML block around the cursor.
    ///
    /// When done inclusively, the opening and closing tags are included.
    XmlTag,
}

/// Specify a movement away from the current cursor position.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum MoveType {
    /// Move to a line at a position relative to the buffer.
    BufferPos(MovePosition),

    /// Move to the column [*n* bytes](Count) into the buffer.
    BufferByteOffset,

    /// Move to the [*n*<sup>th</sup> line](Count) in the buffer.
    BufferLineOffset,

    /// Move to the line [*n*%](Count) of the way through the buffer.
    BufferLinePercent,

    /// Move to the previous or next column [*n* times](Count).
    ///
    /// The [bool] parameter indicates whether to cross line boundaries.
    Column(MoveDir1D, bool),

    /// Move to the final non-blank character [*n* lines](Count) away in [MoveDir1D] direction.
    FinalNonBlank(MoveDir1D),

    /// Move to the first word [*n* lines](Count) away in [MoveDir1D] direction.
    FirstWord(MoveDir1D),

    /// Move to the matching character of the next item.
    ///
    /// Items are characters like `(`/`)`, `[`/`]`, `{`/`}`, and so on.
    ItemMatch,

    /// Move [*n* lines](Count) in [MoveDir1D] direction.
    Line(MoveDir1D),

    /// Move to the [*n*<sup>th</sup>](Count) column in the current line.
    LineColumnOffset,

    /// Move to the column [*n*%](Count) of the way through the current line.
    LinePercent,

    /// Move to a column at a position relative to the current line.
    LinePos(MovePosition),

    /// Move to the beginning of a word [*n* times](Count) in [MoveDir1D] direction.
    WordBegin(WordStyle, MoveDir1D),

    /// Move to the end of a word [*n* times](Count) in [MoveDir1D] direction.
    WordEnd(WordStyle, MoveDir1D),

    /// Move to the beginning of a paragraph [*n* times](Count) in [MoveDir1D] direction.
    ParagraphBegin(MoveDir1D),

    /// Move to the beginning of a sentence [*n* times](Count) in [MoveDir1D] direction.
    SentenceBegin(MoveDir1D),

    /// Move to the beginning of a section [*n* times](Count) in [MoveDir1D] direction.
    SectionBegin(MoveDir1D),

    /// Move to the end of a section [*n* times](Count) in [MoveDir1D] direction.
    SectionEnd(MoveDir1D),

    /// Move to the first word of a screen line [*n* times](Count) away in [MoveDir1D] direction.
    ScreenFirstWord(MoveDir1D),

    /// Move [*n* screen lines](Count) in [MoveDir1D] direction.
    ScreenLine(MoveDir1D),

    /// Move to a column at a position relative to the current screen line.
    ScreenLinePos(MovePosition),

    /// Move to the first word of the line displayed at a position relative to the viewport.
    ViewportPos(MovePosition),
}

/// Represent movement along a 1-dimensional line.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveDir1D {
    /// Move backwards, or to a previous point.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::Rotate(MoveDir1D::Previous).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("window rotate -d previous"));
    /// assert_eq!(act, action!("window rotate -d prev"));
    /// ```
    Previous,

    /// Move forwards, or to a following point.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::Rotate(MoveDir1D::Next).into();
    /// assert_eq!(act, action!("window rotate -d next"));
    /// ```
    Next,
}

/// Represent movement along the horizontal or vertical axes.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveDir2D {
    /// Move leftwards.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::MoveSide(MoveDir2D::Left).into();
    /// assert_eq!(act, action!("window move-side -d left"));
    /// ```
    Left,

    /// Move rightwards.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::MoveSide(MoveDir2D::Right).into();
    /// assert_eq!(act, action!("window move-side -d right"));
    /// ```
    Right,

    /// Move upwards.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::MoveSide(MoveDir2D::Up).into();
    /// assert_eq!(act, action!("window move-side -d up"));
    /// ```
    Up,

    /// Move downwards.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::MoveSide(MoveDir2D::Down).into();
    /// assert_eq!(act, action!("window move-side -d down"));
    /// ```
    Down,
}

/// Represents the two sides of a range that has no meaningful middle.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveTerminus {
    /// The beginning of a range.
    Beginning,

    /// The end of a range.
    End,
}

/// Represent movement to a position along a 1-dimensional line.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MovePosition {
    /// Move to the beginning of some range.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// // All of these are equivalent:
    /// let scroll: Action = Action::Scroll(
    ///     ScrollStyle::LinePos(MovePosition::Beginning, 1.into()));
    /// assert_eq!(scroll, action!("scroll -s (line-pos -p b -c 1)"));
    /// assert_eq!(scroll, action!("scroll -s (line-pos -p beginning -c 1)"));
    /// ```
    Beginning,

    /// Move to the middle of some range.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// // All of these are equivalent:
    /// let scroll: Action = Action::Scroll(
    ///     ScrollStyle::LinePos(MovePosition::Middle, 1.into()));
    /// assert_eq!(scroll, action!("scroll -s (line-pos -p m -c 1)"));
    /// assert_eq!(scroll, action!("scroll -s (line-pos -p middle -c 1)"));
    /// ```
    Middle,

    /// Move to the end of some range.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// // All of these are equivalent:
    /// let scroll: Action = Action::Scroll(
    ///     ScrollStyle::LinePos(MovePosition::End, 1.into()));
    /// assert_eq!(scroll, action!("scroll -s (line-pos -p e -c 1)"));
    /// assert_eq!(scroll, action!("scroll -s (line-pos -p end -c 1)"));
    /// ```
    End,
}

/// Represents a modification of a previous [MoveDir1D] movement.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveDirMod {
    /// Use the same movement previously used.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let dir = MoveDirMod::Same;
    /// let search: Action = action!("search -d same");
    /// assert_eq!(search, Action::Search(dir, Count::Contextual));
    /// ```
    Same,

    /// Use the opposite of the movement previously used.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let dir = MoveDirMod::Flip;
    /// let search: Action = action!("search -d flip");
    /// assert_eq!(search, Action::Search(dir, Count::Contextual));
    /// ```
    Flip,

    /// Ignore whatever value was previously used.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let dir = MoveDirMod::Exact(MoveDir1D::Previous);
    /// let search: Action = action!("search -d (exact prev)");
    /// assert_eq!(search, Action::Search(dir, Count::Contextual));
    ///
    /// let dir = MoveDirMod::Exact(MoveDir1D::Next);
    /// let search: Action = action!("search -d (exact next)");
    /// assert_eq!(search, Action::Search(dir, Count::Contextual));
    /// ```
    Exact(MoveDir1D),
}

/// This represents a selection of a 2-dimensional axis.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Axis {
    /// The horizontal axis.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let axis = Axis::Horizontal;
    /// let scroll: Action = Action::Scroll(ScrollStyle::CursorPos(MovePosition::End, axis));
    ///
    /// // All of these are equivalent:
    /// assert_eq!(scroll, action!("scroll -s (cursor-pos -p end -x horizontal)"));
    /// assert_eq!(scroll, action!("scroll -s (cursor-pos -p end -x h)"));
    /// assert_eq!(scroll, action!("scroll -s (cursor-pos -p end -x {axis})"));
    /// ```
    Horizontal,

    /// The vertical axis.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let axis = Axis::Vertical;
    /// let scroll: Action = Action::Scroll(ScrollStyle::CursorPos(MovePosition::End, axis));
    ///
    /// // All of these are equivalent:
    /// assert_eq!(scroll, action!("scroll -s (cursor-pos -p end -x vertical)"));
    /// assert_eq!(scroll, action!("scroll -s (cursor-pos -p end -x v)"));
    /// assert_eq!(scroll, action!("scroll -s (cursor-pos -p end -x {axis})"));
    /// ```
    Vertical,
}

impl Axis {
    /// Rotate a 2-dimensional axis to its opposite.
    pub fn rotate(&self) -> Axis {
        match self {
            Axis::Horizontal => Axis::Vertical,
            Axis::Vertical => Axis::Horizontal,
        }
    }
}

/// This represents the units used when scrolling.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScrollSize {
    /// Scroll by number of character cells.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let scroll: Action = action!("scroll -s (dir2d -d up -z cell)");
    /// let style = ScrollStyle::Direction2D(MoveDir2D::Up, ScrollSize::Cell, Count::Contextual);
    /// assert_eq!(scroll, Action::Scroll(style));
    /// ```
    Cell,

    /// Scroll by [*n*](Count) times half the page size.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let scroll: Action = action!("scroll -s (dir2d -d up -z half-page)");
    /// let style = ScrollStyle::Direction2D(MoveDir2D::Up, ScrollSize::HalfPage, Count::Contextual);
    /// assert_eq!(scroll, Action::Scroll(style));
    /// ```
    HalfPage,

    /// Scroll by [*n*](Count) times the page size.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let scroll: Action = action!("scroll -s (dir2d -d up -z page)");
    /// let style = ScrollStyle::Direction2D(MoveDir2D::Up, ScrollSize::Page, Count::Contextual);
    /// assert_eq!(scroll, Action::Scroll(style));
    /// ```
    Page,
}

/// This represents the way in which the viewport should be scrolled.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScrollStyle {
    /// Scroll the viewport in [MoveDir2D] direction by [ScrollSize] units, [*n* times](Count).
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let scroll: Action = action!("scroll -s (dir2d -d up -z half-page)");
    /// let style = ScrollStyle::Direction2D(MoveDir2D::Up, ScrollSize::HalfPage, Count::Contextual);
    /// assert_eq!(scroll, Action::Scroll(style));
    /// ```
    ///
    /// See the documentation for [ScrollSize] for how to construct each of its variants with
    /// [action].
    Direction2D(MoveDir2D, ScrollSize, Count),

    /// Scroll the viewport so that the cursor is placed at [MovePosition] relative to [Axis].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let scroll: Action = action!("scroll -s (cursor-pos -p end -x vertical)");
    /// let style = ScrollStyle::CursorPos(MovePosition::End, Axis::Vertical);
    /// assert_eq!(scroll, Action::Scroll(style));
    /// ```
    ///
    /// See the documentation for [Axis] and [MovePosition] for how to construct each of their
    /// variants with [action].
    CursorPos(MovePosition, Axis),

    /// Scroll the viewport so that the [*n*<sup>th</sup> line](Count) is at [MovePosition] on the screen.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let scroll: Action = action!("scroll -s (line-pos -p end -c 1)");
    /// let style = ScrollStyle::LinePos(MovePosition::End, 1.into());
    /// assert_eq!(scroll, Action::Scroll(style));
    /// ```
    ///
    /// See the documentation for [MovePosition] for how to construct each of its variants with
    /// [action].
    LinePos(MovePosition, Count),
}

/// Place the cursor at a specified position in a visual selection, with the anchor now at the
/// opposite end.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SelectionCursorChange {
    /// Place the cursor in the first line of the selection, in the first column of the selection.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let change = SelectionCursorChange::Beginning;
    /// let act: Action = action!("selection cursor-set -f beginning");
    /// assert_eq!(act, SelectionAction::CursorSet(change).into());
    /// ```
    Beginning,

    /// Place the cursor in the last line of the selection, in the last column of the selection.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let change = SelectionCursorChange::End;
    /// let act: Action = action!("selection cursor-set -f end");
    /// assert_eq!(act, SelectionAction::CursorSet(change).into());
    /// ```
    End,

    /// Swap the cursor with the anchor of the selection.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let change = SelectionCursorChange::SwapAnchor;
    /// let act: Action = action!("selection cursor-set -f swap-anchor");
    /// assert_eq!(act, SelectionAction::CursorSet(change).into());
    /// ```
    SwapAnchor,

    /// Move the cursor to the other side of the selection.
    ///
    /// The "other side" of the selection depends on its [shape][TargetShape]:
    ///
    /// * When the selection is [BlockWise](TargetShape::BlockWise), the
    ///   cursor and anchor will stay on their current line, but change
    ///   columns to be placed on the opposite side of the selection's block.
    /// * When the selection is [LineWise](TargetShape::LineWise), the
    ///   other side of the selection is the anchor.
    /// * When the selection is [CharWise](TargetShape::CharWise), the
    ///   other side of the selection is the anchor.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let change = SelectionCursorChange::SwapSide;
    /// let act: Action = action!("selection cursor-set -f swap-side");
    /// assert_eq!(act, SelectionAction::CursorSet(change).into());
    /// ```
    SwapSide,
}

/// This represents what UI element is targeted during an Action.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FocusChange {
    /// Target the currently focused UI element.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let fc = FocusChange::Current;
    /// let act: Action = TabAction::Focus(fc).into();
    /// assert_eq!(act, action!("tab focus -f current"));
    /// ```
    Current,

    /// Target the [*n*<sup>th</sup> element](Count) from the beginning. The first element is numbered 1.
    ///
    /// If the specified *n* is greater than the number of elements, and [bool] is `true`, target
    /// the last element. Otherwise, do nothing.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let fc = FocusChange::Offset(2.into(), false);
    /// let act: Action = TabAction::Focus(fc).into();
    /// assert_eq!(act, action!("tab focus -f (offset -c 2 -l false)"));
    /// ```
    Offset(Count, bool),

    /// Target the element at [MovePosition] in the element list.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// // All of these are equivalent:
    /// let fc = FocusChange::Position(MovePosition::End);
    /// let act: Action = TabAction::Focus(fc).into();
    /// assert_eq!(act, action!("tab focus -f (pos -p end)"));
    /// assert_eq!(act, action!("tab focus -f (position -p end)"));
    /// ```
    ///
    /// See the documentation for [MovePosition] for how to construct each of its variants with
    /// [action].
    Position(MovePosition),

    /// Target the previously focused element.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// // All of these are equivalent:
    /// let fc = FocusChange::PreviouslyFocused;
    /// let act: Action = TabAction::Focus(fc).into();
    /// assert_eq!(act, action!("tab focus -f previously-focused"));
    /// assert_eq!(act, action!("tab focus -f previous"));
    /// assert_eq!(act, action!("tab focus -f prev"));
    /// ```
    PreviouslyFocused,

    /// Target the element [*n* times](Count) away in [MoveDir1D] direction.
    ///
    /// If moving [*n* times](Count) would go past the first or last element, and [bool] is `true`, wrap
    /// around to the other end of the element list and continue from there. Otherwise, do nothing.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// // All of these are equivalent:
    /// let fc = FocusChange::Direction1D(MoveDir1D::Next, 4.into(), true);
    /// let act: Action = TabAction::Focus(fc).into();
    /// assert_eq!(act, action!("tab focus -f (dir1d -d next -c 4 -w true)"));
    /// ```
    Direction1D(MoveDir1D, Count, bool),

    /// Target the element [*n* times](Count) away in [MoveDir2D] direction.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// // All of these are equivalent:
    /// let fc = FocusChange::Direction2D(MoveDir2D::Up, 3.into());
    /// let act: Action = TabAction::Focus(fc).into();
    /// assert_eq!(act, action!("tab focus -f (dir2d -d up -c 3)"));
    /// ```
    Direction2D(MoveDir2D, Count),
}

/// This represents how to change the size of a window.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SizeChange<I = Count> {
    /// Make the window and others along the specified axis the same size.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let size = SizeChange::Equal;
    /// let act: Action = WindowAction::Resize(FocusChange::Current, Axis::Vertical, size).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("window resize -f current -x vertical -z equal"));
    /// assert_eq!(act, action!("window resize -f current -x vertical -z eq"));
    /// ```
    Equal,

    /// Make the window exactly a specific size along the axis.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let size = SizeChange::Exact(5.into());
    /// let act: Action = WindowAction::Resize(FocusChange::Current, Axis::Vertical, size).into();
    /// assert_eq!(act, action!("window resize -f current -x vertical -z (exact 5)"));
    /// ```
    Exact(I),

    /// Decrease the size of the window by a specific amount.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// // All of these are equivalent:
    /// let size = SizeChange::Decrease(5.into());
    /// let act: Action = WindowAction::Resize(FocusChange::Current, Axis::Vertical, size).into();
    /// assert_eq!(act, action!("window resize -f current -x vertical -z (decrease 5)"));
    /// assert_eq!(act, action!("window resize -f current -x vertical -z (dec 5)"));
    /// ```
    Decrease(I),

    /// Increase the size of the window by a specific amount.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// // All of these are equivalent:
    /// let size = SizeChange::Increase(5.into());
    /// let act: Action = WindowAction::Resize(FocusChange::Current, Axis::Vertical, size).into();
    /// assert_eq!(act, action!("window resize -f current -x vertical -z (increase 5)"));
    /// assert_eq!(act, action!("window resize -f current -x vertical -z (inc 5)"));
    /// ```
    Increase(I),
}

/// This represents how to change the indentation of a range.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IndentChange<I = Count> {
    /// Automatically determine indentation level.
    Auto,

    /// Decrease the indentation level of indentation.
    Decrease(I),

    /// Increase the indentation level of indentation.
    Increase(I),
}

/// This represents how to change a number in text.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NumberChange {
    /// Decrease the first number in the targeted text by [*n*](Count).
    Decrease(Count),

    /// Increase the first number in the targeted text by [*n*](Count).
    Increase(Count),
}

/// Targets for [Action::KeywordLookup].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum KeywordTarget {
    /// Lookup the [word][WordStyle] surrounding the cursor.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let word = WordStyle::Little;
    /// let target = KeywordTarget::Word(word.clone());
    /// let act: Action = Action::KeywordLookup(target.clone());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("keyword-lookup -t {target}"));
    /// assert_eq!(act, action!("keyword-lookup -t (word little)"));
    /// assert_eq!(act, action!("keyword-lookup -t (word {})", word.clone()));
    /// assert_eq!(act, action!("keyword-lookup -t {word}", word.clone()));
    /// ```
    Word(WordStyle),

    /// Lookup the currently selected text.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let target = KeywordTarget::Selection;
    /// let act: Action = Action::KeywordLookup(target.clone());
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("keyword-lookup -t {target}"));
    /// assert_eq!(act, action!("keyword-lookup -t selection"));
    /// ```
    Selection,
}

impl From<WordStyle> for KeywordTarget {
    fn from(style: WordStyle) -> KeywordTarget {
        KeywordTarget::Word(style)
    }
}

/// Targets for [WindowAction::Open] and [WindowAction::Switch].
///
/// [WindowAction::Open]: crate::WindowAction::Open
/// [WindowAction::Switch]: crate::WindowAction::Switch
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OpenTarget<W: ApplicationWindowId> {
    /// An alternate window. This is usually the previous window.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Alternate;
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!("window switch -t alternate"));
    /// ```
    Alternate,

    /// An application-specific [identifier][ApplicationWindowId] to switch to.
    Application(W),

    /// Use the current window as the target.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Current;
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!("window switch -t current"));
    /// ```
    Current,

    /// Use the [word](WordStyle) under the cursor as a target name.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Cursor(WordStyle::FileName);
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!("window switch -t (cursor -s filename)"));
    /// ```
    ///
    /// See the documentation for [WordStyle] for how to construct each of its variants with
    /// [action].
    Cursor(WordStyle),

    /// An absolute position in a list of targets.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::List(2.into());
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!("window switch -t (list -c 2)"));
    /// ```
    List(Count),

    /// A named target (e.g., a filename to open).
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Name("foo bar".into());
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!(r#"window switch -t (name -i "foo bar")"#));
    /// ```
    Name(String),

    /// A window offset from the current one.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Offset(MoveDir1D::Next, 5.into());
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!("window switch -t (offset -d next -c 5)"));
    /// ```
    Offset(MoveDir1D, Count),

    /// Use the selected text as a target name.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Selection;
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!("window switch -t selection"));
    /// ```
    Selection,

    /// A default window to open when no target has been specified.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Unnamed;
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, action!("window switch -t unnamed"));
    /// ```
    Unnamed,
}

/// This represents what tabs are targeted by a tab command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TabTarget {
    /// Close the tab targeted by FocusChange.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let fc = TabTarget::Single(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let act: Action = TabAction::Close(fc, flags).into();
    /// assert_eq!(act, action!("tab close -t (single current) -F none"));
    /// ```
    Single(FocusChange),

    /// Close all tab *except* for the one targeted by FocusChange.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let fc = TabTarget::AllBut(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let act: Action = TabAction::Close(fc, flags).into();
    /// assert_eq!(act, action!("tab close -t (all-but current) -F none"));
    /// ```
    AllBut(FocusChange),

    /// Close all tabs.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let fc = TabTarget::All;
    /// let flags = CloseFlags::NONE;
    /// let act: Action = TabAction::Close(fc, flags).into();
    /// assert_eq!(act, action!("tab close -t all -F none"));
    /// ```
    All,
}

/// This represents what windows are targeted by a window command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WindowTarget {
    /// Close the window targeted by [FocusChange].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let fc = WindowTarget::Single(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let act: Action = WindowAction::Close(fc, flags).into();
    /// assert_eq!(act, action!("window close -t (single current) -F none"));
    /// ```
    Single(FocusChange),

    /// Close all windows *except* for the one targeted by [FocusChange].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let fc = WindowTarget::AllBut(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let act: Action = WindowAction::Close(fc, flags).into();
    /// assert_eq!(act, action!("window close -t (all-but current) -F none"));
    /// ```
    AllBut(FocusChange),

    /// Close all windows.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let fc = WindowTarget::All;
    /// let flags = CloseFlags::NONE;
    /// let act: Action = WindowAction::Close(fc, flags).into();
    /// assert_eq!(act, action!("window close -t all -F none"));
    /// ```
    All,
}

/// Target cursors in a cursor group.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CursorCloseTarget {
    /// Target the cursor group's leader.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let close: Action = action!("cursor close -t leader");
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Leader).into());
    /// ```
    Leader,

    /// Target the cursor group's followers.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let close: Action = action!("cursor close -t followers");
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Followers).into());
    /// ```
    Followers,
}

/// Ways to combine a newer cursor group with an already existing one.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CursorGroupCombineStyle {
    /// Use all of the selections from both groups.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let combine = CursorGroupCombineStyle::Append;
    /// let restore: Action = action!("cursor restore -s append");
    /// assert_eq!(restore, CursorAction::Restore(combine).into());
    /// ```
    Append,

    /// Merge each member with the matching member in the other group.
    ///
    /// This fails if the groups have a different number of members.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let combine = CursorGroupCombineStyle::Merge(CursorMergeStyle::Union);
    /// let restore: Action = action!("cursor restore -s (merge union)");
    /// assert_eq!(restore, CursorAction::Restore(combine).into());
    /// ```
    ///
    /// See the documentation for [CursorMergeStyle] for how to construct each of its
    /// variants with [action].
    Merge(CursorMergeStyle),

    /// Use only the selections in the newer group.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let combine = CursorGroupCombineStyle::Replace;
    /// let restore: Action = action!("cursor restore -s replace");
    /// assert_eq!(restore, CursorAction::Restore(combine).into());
    /// ```
    Replace,
}

impl From<CursorMergeStyle> for CursorGroupCombineStyle {
    fn from(style: CursorMergeStyle) -> Self {
        CursorGroupCombineStyle::Merge(style)
    }
}

/// Ways to combine two selections.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CursorMergeStyle {
    /// Merge the two selections to form one long selection.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let merge = CursorMergeStyle::Union;
    /// let save: Action = action!("cursor save -s (merge union)");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Merge(merge)).into());
    /// ```
    Union,

    /// Use the intersecting region of the two selections.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let merge = CursorMergeStyle::Intersect;
    /// let save: Action = action!("cursor save -s (merge intersect)");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Merge(merge)).into());
    /// ```
    Intersect,

    /// Select the one where the cursor is furthest in [MoveDir1D] direction.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let merge = CursorMergeStyle::SelectCursor(MoveDir1D::Previous);
    /// let save: Action = action!("cursor save -s (merge select-cursor -d prev)");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Merge(merge)).into());
    /// ```
    SelectCursor(MoveDir1D),

    /// Select the shortest selection.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let merge = CursorMergeStyle::SelectShort;
    /// let save: Action = action!("cursor save -s (merge select-short)");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Merge(merge)).into());
    /// ```
    SelectShort,

    /// Select the longest selection.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let merge = CursorMergeStyle::SelectLong;
    /// let save: Action = action!("cursor save -s (merge select-long)");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Merge(merge)).into());
    /// ```
    SelectLong,
}

/// This represents how to determine what count argument should be applied to an action.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Count {
    /// Use the count provided by the user, or 1 if one was not given.
    Contextual,
    /// Use the count provided by the user minus 1, or 0 if one was not given.
    MinusOne,
    /// Ignore the count provided by the user, and use the exact amount specified here.
    Exact(usize),
}

impl From<usize> for Count {
    fn from(n: usize) -> Self {
        Count::Exact(n)
    }
}

/// Saved cursor positions.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Mark {
    /// The position of the cursor in the current buffer when it last exited.
    ///
    /// For example, `'"` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact buffer-last-exited)");
    /// let exp: Action = EditorAction::Mark(Mark::BufferLastExited.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    BufferLastExited,

    /// A user-named position in the current buffer.
    ///
    /// For example, `'[a-z]` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact buffer-named 'c')");
    /// let exp: Action = EditorAction::Mark(Mark::BufferNamed('c').into()).into();
    /// assert_eq!(act, exp);
    /// ```
    BufferNamed(char),

    /// The position of the current when the application was previously exited.
    ///
    /// Index 0 is the cursor position the last time the application exited, 1 the position the
    /// second most recent exit, and so on.
    ///
    /// For example, `'[0-9]` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact global-last-exited 1)");
    /// let exp: Action = EditorAction::Mark(Mark::GlobalLastExited(1).into()).into();
    /// assert_eq!(act, exp);
    /// ```
    GlobalLastExited(usize),

    /// A global, user-named position in some buffer known to the application.
    ///
    /// For example, `'[A-Z]` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact global-named 'C')");
    /// let exp: Action = EditorAction::Mark(Mark::GlobalNamed('C').into()).into();
    /// assert_eq!(act, exp);
    /// ```
    GlobalNamed(char),

    /// The cursor position where the last change was made.
    ///
    /// For example, `'.` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact last-changed)");
    /// let exp: Action = EditorAction::Mark(Mark::LastChanged.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    LastChanged,

    /// The cursor position where the last text was inserted.
    ///
    /// For example, `'^` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact last-inserted)");
    /// let exp: Action = EditorAction::Mark(Mark::LastInserted.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    LastInserted,

    /// The cursor position before the latest jump.
    ///
    /// For example, `''` and `` '` `` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact last-jump)");
    /// let exp: Action = EditorAction::Mark(Mark::LastJump.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    LastJump,

    /// The position of the beginning of the last text selection.
    ///
    /// For example, `'<` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact visual-begin)");
    /// let exp: Action = EditorAction::Mark(Mark::VisualBegin.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    VisualBegin,

    /// The position of the end of the last text selection.
    ///
    /// For example, `'>` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact visual-end)");
    /// let exp: Action = EditorAction::Mark(Mark::VisualEnd.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    VisualEnd,

    /// The position of the beginning of the last yanked text.
    ///
    /// For example, `'[` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact last-yanked-begin)");
    /// let exp: Action = EditorAction::Mark(Mark::LastYankedBegin.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    LastYankedBegin,

    /// The position of the end of the last yanked text.
    ///
    /// For example, `']` in Vim.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let act: Action = action!("mark -m (exact last-yanked-end)");
    /// let exp: Action = EditorAction::Mark(Mark::LastYankedEnd.into()).into();
    /// assert_eq!(act, exp);
    /// ```
    LastYankedEnd,
}

impl Mark {
    /// Indicates whether this is a global mark.
    pub fn is_global(&self) -> bool {
        match self {
            Mark::GlobalNamed(_) => true,
            Mark::GlobalLastExited(_) => true,

            Mark::BufferLastExited => false,
            Mark::BufferNamed(_) => false,
            Mark::LastChanged => false,
            Mark::LastInserted => false,
            Mark::LastJump => false,
            Mark::VisualBegin => false,
            Mark::VisualEnd => false,
            Mark::LastYankedBegin => false,
            Mark::LastYankedEnd => false,
        }
    }
}

/// A value that may not be known now, but is present in the context.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum Specifier<T> {
    /// Look for a value of `T` in the [EditContext].
    #[default]
    Contextual,

    /// Use the value of `T` provided here.
    Exact(T),
}

impl<T> From<T> for Specifier<T> {
    fn from(v: T) -> Self {
        Specifier::Exact(v)
    }
}

bitflags! {
    /// These flags are used to specify the behaviour while writing a window.
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct WriteFlags: u32 {
        /// No flags set.
        ///
        /// ## Example: Using `action!`
        ///
        /// ```
        /// use editor_types::prelude::*;
        /// use editor_types::{action, Action, WindowAction};
        ///
        /// let target = WindowTarget::All;
        /// let flags = WriteFlags::NONE;
        /// let act: Action = WindowAction::Write(target, None, flags).into();
        /// assert_eq!(act, action!("window write -t all -F none"));
        /// ```
        const NONE = 0b00000000;

        /// Ignore any issues during closing.
        ///
        /// ## Example: Using `action!`
        ///
        /// ```
        /// use editor_types::prelude::*;
        /// use editor_types::{action, Action, WindowAction};
        ///
        /// let target = WindowTarget::All;
        /// let flags = WriteFlags::FORCE;
        /// let act: Action = WindowAction::Write(target, None, flags).into();
        /// assert_eq!(act, action!("window write -t all -F force"));
        /// ```
        const FORCE = 0b00000001;
    }
}

bitflags! {
    /// These flags are used to specify the behaviour while opening a window.
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct OpenFlags: u32 {
        /// No flags set.
        const NONE = 0b00000000;

        /// Try to ignore any issues during opening.
        const FORCE = 0b00000001;

        /// Attemp to create the target content if it doesn't already exist.
        const CREATE = 0b00000010;
    }
}

bitflags! {
    /// These flags are used to specify the behaviour while closing a window.
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct CloseFlags: u32 {
        /// No flags set.
        ///
        /// ## Example: Using `action!`
        ///
        /// ```
        /// use editor_types::prelude::*;
        /// use editor_types::{action, Action, TabAction};
        ///
        /// let fc = TabTarget::Single(FocusChange::Current);
        /// let flags = CloseFlags::NONE;
        /// let act: Action = TabAction::Close(fc, flags).into();
        /// assert_eq!(act, action!("tab close -t (single current) -F none"));
        /// ```
        const NONE = 0b00000000;

        /// Ignore any issues during closing.
        ///
        /// ## Example: Using `action!`
        ///
        /// ```
        /// use editor_types::prelude::*;
        /// use editor_types::{action, Action, TabAction};
        ///
        /// let fc = TabTarget::Single(FocusChange::Current);
        /// let flags = CloseFlags::FORCE;
        /// let act: Action = TabAction::Close(fc, flags).into();
        /// assert_eq!(act, action!("tab close -t (single current) -F force"));
        /// ```
        const FORCE = 0b00000001;

        /// Write while closing.
        ///
        /// ## Example: Using `action!`
        ///
        /// ```
        /// use editor_types::prelude::*;
        /// use editor_types::{action, Action, TabAction};
        ///
        /// let fc = TabTarget::Single(FocusChange::Current);
        /// let flags = CloseFlags::WRITE;
        /// let act: Action = TabAction::Close(fc, flags).into();
        /// assert_eq!(act, action!("tab close -t (single current) -F write"));
        /// ```
        const WRITE = 0b00000010;

        /// Quit if this is the last window.
        ///
        /// ## Example: Using `action!`
        ///
        /// ```
        /// use editor_types::prelude::*;
        /// use editor_types::{action, Action, TabAction};
        ///
        /// let fc = TabTarget::Single(FocusChange::Current);
        /// let flags = CloseFlags::QUIT;
        /// let act: Action = TabAction::Close(fc, flags).into();
        /// assert_eq!(act, action!("tab close -t (single current) -F quit"));
        /// ```
        const QUIT  = 0b00000100;

        /// Write out the window's contents and quit.
        const WQ = CloseFlags::WRITE.bits() | CloseFlags::QUIT.bits();

        /// Force quit the window.
        const FQ = CloseFlags::FORCE.bits() | CloseFlags::QUIT.bits();
    }
}

/// Different ways to expand or trim selections.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SelectionBoundary {
    /// A selection that starts at the beginning of a line and ends on a newline.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionBoundary::Line;
    /// let split: Action = action!("selection trim -b line");
    /// assert_eq!(split, SelectionAction::Trim(style, TargetShapeFilter::ALL).into());
    /// ```
    Line,

    /// A selection that starts on a non-whitespace character and ends on a non-whitespace
    /// character.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionBoundary::NonWhitespace;
    /// let act: Action = SelectionAction::Expand(style, TargetShapeFilter::ALL).into();
    ///
    /// assert_eq!(action!("selection expand -b non-whitespace"), act);
    /// assert_eq!(action!("selection expand -b non-ws"), act);
    /// ```
    NonWhitespace,
}

impl BoundaryTest for SelectionBoundary {
    fn is_boundary_begin(&self, ctx: &BoundaryTestContext) -> bool {
        match self {
            SelectionBoundary::Line => {
                if let Some(before) = ctx.before {
                    return before == '\n';
                } else {
                    return true;
                }
            },
            SelectionBoundary::NonWhitespace => {
                return !is_space_char(ctx.current);
            },
        }
    }

    fn is_boundary_end(&self, ctx: &BoundaryTestContext) -> bool {
        match self {
            SelectionBoundary::Line => ctx.current == '\n' || ctx.after.is_none(),
            SelectionBoundary::NonWhitespace => {
                return !is_space_char(ctx.current);
            },
        }
    }
}

/// Different ways to split existing selections into new ones.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SelectionSplitStyle {
    /// Split a selection into two [TargetShape::CharWise] selections, one at the current cursor
    /// position, and the other at the anchor.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionSplitStyle::Anchor;
    /// let split: Action = action!("selection split -s anchor");
    /// assert_eq!(split, SelectionAction::Split(style, TargetShapeFilter::ALL).into());
    /// ```
    Anchor,

    /// Split a selection at each line boundary it contains.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionSplitStyle::Lines;
    /// let split: Action = action!("selection split -s lines");
    /// assert_eq!(split, SelectionAction::Split(style, TargetShapeFilter::ALL).into());
    /// ```
    Lines,

    /// Split a selection into [TargetShape::CharWise] parts based on the regular expression
    /// stored in the register for [CommandType::Search].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionSplitStyle::Regex(MatchAction::Keep);
    /// let split: Action = action!("selection split -s (regex keep)");
    /// assert_eq!(split, SelectionAction::Split(style, TargetShapeFilter::ALL).into());
    ///
    /// let style = SelectionSplitStyle::Regex(MatchAction::Drop);
    /// let split: Action = action!("selection split -s (regex drop)");
    /// assert_eq!(split, SelectionAction::Split(style, TargetShapeFilter::ALL).into());
    /// ```
    Regex(MatchAction),
}

/// Different ways to change the boundaries of a visual selection.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SelectionResizeStyle {
    /// Extend (or possibly shrink) the selection by moving the cursor.
    ///
    /// When extending with [EditTarget::Range], this may also move the anchor to fully encompass
    /// the [RangeType].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionResizeStyle::Extend;
    /// let target = EditTarget::CurrentPosition;
    /// let act: Action = SelectionAction::Resize(style, target.clone()).into();
    /// assert_eq!(act, action!("selection resize -s extend -t {target}"));
    /// ```
    Extend,

    /// Interpret the [EditTarget] as the bounds of a text object, and select it.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionResizeStyle::Object;
    /// let target = EditTarget::CurrentPosition;
    /// let act: Action = SelectionAction::Resize(style, target.clone()).into();
    /// assert_eq!(act, action!("selection resize -s object -t {target}"));
    /// ```
    Object,

    /// Move the anchor to the current cursor position and create a new selection from there.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionResizeStyle::Restart;
    /// let target = EditTarget::CurrentPosition;
    /// let act: Action = SelectionAction::Resize(style, target.clone()).into();
    /// assert_eq!(act, action!("selection resize -s restart -t {target}"));
    /// ```
    Restart,
}

/// When focusing on the command bar, this is the type of command that should be submitted.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CommandType {
    /// Prompt the user for a command.
    Command,

    /// Prompt the user for a search query.
    Search,
}

/// What history items to recall during [PromptAction::Recall].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RecallFilter {
    /// Include all items in the prompt's history.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    ///
    /// let filter = RecallFilter::All;
    /// let act: Action = PromptAction::Recall(filter.clone(), MoveDir1D::Next, Count::Contextual).into();
    /// assert_eq!(act, action!("prompt recall -d next -c ctx -F all"));
    /// ```
    All,

    /// Only include items whose prefix matches the initially typed text.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    ///
    /// let filter = RecallFilter::PrefixMatch;
    /// let act: Action = PromptAction::Recall(filter.clone(), MoveDir1D::Next, Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("prompt recall -d next -c ctx -F prefix-match"));
    /// assert_eq!(act, action!("prompt recall -d next -c ctx -F prefix"));
    /// assert_eq!(act, action!("prompt recall -d next -c ctx -F {filter}"));
    /// ```
    PrefixMatch,
}

/// This specifies which list of cursors to use when jumping, the change list or the jump list.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PositionList {
    /// The change list contains positions where changes were previously made.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let list = PositionList::ChangeList;
    /// let count = Count::Contextual;
    /// let act: Action = Action::Jump(list, MoveDir1D::Next, count);
    /// assert_eq!(act, action!("jump -t change-list -d next"));
    /// ```
    ChangeList,

    /// The jump list contains positions where the cursor was placed before jumping to a new
    /// location in the document.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let list = PositionList::JumpList;
    /// let count = Count::Contextual;
    /// let act: Action = Action::Jump(list, MoveDir1D::Next, count);
    /// assert_eq!(act, action!("jump -t jump-list -d next"));
    /// ```
    JumpList,
}

/// This specifies the behaviour of entering and backspacing over characters.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InsertStyle {
    /// This specifies that typed characters should leave existing ones as is, and backspacing
    /// should remove characters.
    Insert,

    /// This specifies that typed characters should replace existing ones, and backspacing should
    /// restore any overwritten characters.
    Replace,
}

/// A character.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Char {
    /// An exact character.
    Single(char),
    /// A digraph sequence.
    Digraph(char, char),
    /// A terminal control sequence.
    CtrlSeq(String),
    /// Copy a character from the same column in the previous or next line.
    CopyLine(MoveDir1D),
}

impl From<char> for Char {
    fn from(c: char) -> Self {
        Char::Single(c)
    }
}

/// Locations for temporarily storing text shared between buffers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Register {
    /// The default register.
    ///
    /// For example, `""` in Vim.
    Unnamed,

    /// The default macro register.
    ///
    /// For example, `"@` in Kakoune.
    UnnamedMacro,

    /// The default cursor group register.
    ///
    ///
    /// For example, `"^` in Kakoune.
    UnnamedCursorGroup,

    /// Recently deleted text.
    ///
    /// For example, `"[1-9]` in Vim.
    RecentlyDeleted(usize),

    /// Most recently deleted text that was shorted than a line.
    ///
    /// For example, `"-` in Vim.
    SmallDelete,

    /// A register containing the last inserted text.
    ///
    /// For example, `".` in Vim.
    LastInserted,

    /// A register containing the last value entered for a [CommandType].
    ///
    /// For example, `":` and `"/` in Vim.
    LastCommand(CommandType),

    /// A register containing the last copied text.
    ///
    /// For eample, `"0` in Vim.
    LastYanked,

    /// A register named by `char`.
    ///
    /// The index of the most recent deletion is 0, the second most recent deletion is 1, and so
    /// on.
    ///
    /// For example, `"[a-zA-Z]` in Vim.
    Named(char),

    /// A read-only register containing the alternate buffer name.
    ///
    /// For example, `"#` in Vim.
    AltBufName,

    /// A read-only register containing the current buffer name.
    ///
    /// For example, `"%` in Vim.
    CurBufName,

    /// A register that discards all content written to it.
    ///
    /// For example, `"_` in Vim.
    Blackhole,

    /// A register representing the windowing environment's most recently selected text.
    ///
    /// For example, `"*` in Vim, or what clicking the mouse's middle button pastes in X and
    /// Wayland.
    SelectionPrimary,

    /// A register representing the windowing environment's most recently copied text.
    ///
    /// For example, `"+` in Vim, or what the keyboard shortcut pastes in X and Wayland.
    SelectionClipboard,
}

impl Register {
    /// Indicates whether a given register is allowed to store cursor groups.
    pub fn is_cursor_storage(&self) -> bool {
        match self {
            Register::Named(_) => true,
            Register::Blackhole => true,
            Register::UnnamedCursorGroup => true,

            Register::Unnamed => false,
            Register::UnnamedMacro => false,
            Register::RecentlyDeleted(_) => false,
            Register::SmallDelete => false,
            Register::LastCommand(_) => false,
            Register::LastInserted => false,
            Register::LastYanked => false,
            Register::AltBufName => false,
            Register::CurBufName => false,
            Register::SelectionPrimary => false,
            Register::SelectionClipboard => false,
        }
    }
}

/// This specifies either the shape of a visual selection, or a forced motion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TargetShape {
    /// A series of characters.
    ///
    /// During a selection, the two points indicate the start and end columns.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let shape = TargetShape::CharWise;
    /// let count = Count::Contextual;
    /// let act: Action = InsertTextAction::OpenLine(shape, MoveDir1D::Next, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("insert open-line -S charwise -d next"));
    /// assert_eq!(act, action!("insert open-line -S char -d next"));
    /// ```
    CharWise,

    /// A series of lines.
    ///
    /// During a selection, the two points indicate the start and end lines.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let shape = TargetShape::LineWise;
    /// let count = Count::Contextual;
    /// let act: Action = InsertTextAction::OpenLine(shape, MoveDir1D::Next, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("insert open-line -S linewise -d next"));
    /// assert_eq!(act, action!("insert open-line -S line -d next"));
    /// ```
    LineWise,

    /// A block of characters.
    ///
    /// During a selection, the two points indicate opposite corners.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let shape = TargetShape::BlockWise;
    /// let count = Count::Contextual;
    /// let act: Action = InsertTextAction::OpenLine(shape, MoveDir1D::Next, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("insert open-line -S blockwise -d next"));
    /// assert_eq!(act, action!("insert open-line -S block -d next"));
    /// ```
    BlockWise,
}

bitflags! {
    /// Bitmask that specifies what shapes are targeted by an action.
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct TargetShapeFilter: u32 {
        /// Match no shapes.
        const NONE = 0b00000000;

        /// Match all shapes.
        const ALL = 0b00000111;

        /// Match [TargetShape::CharWise].
        const CHAR = 0b00000001;

        /// Match [TargetShape::LineWise].
        const LINE = 0b00000010;

        /// Match [TargetShape::BlockWise].
        const BLOCK = 0b00000100;
    }
}

impl TargetShapeFilter {
    /// Check whether this filter applies to a given [TargetShape].
    pub fn matches(&self, shape: &TargetShape) -> bool {
        match shape {
            TargetShape::CharWise => self.contains(TargetShapeFilter::CHAR),
            TargetShape::LineWise => self.contains(TargetShapeFilter::LINE),
            TargetShape::BlockWise => self.contains(TargetShapeFilter::BLOCK),
        }
    }
}

impl From<TargetShape> for TargetShapeFilter {
    fn from(shape: TargetShape) -> Self {
        match shape {
            TargetShape::CharWise => TargetShapeFilter::CHAR,
            TargetShape::LineWise => TargetShapeFilter::LINE,
            TargetShape::BlockWise => TargetShapeFilter::BLOCK,
        }
    }
}

/// Action to take on targets when filtering with a regular expression.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum MatchAction {
    /// Keep targets of the regular expression.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let act = SelectionAction::Filter(MatchAction::Keep);
    /// let split: Action = action!("selection filter -F keep");
    /// assert_eq!(split, act.into());
    /// ```
    Keep,

    /// Remove targets of the regular expression.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let act = SelectionAction::Filter(MatchAction::Drop);
    /// let split: Action = action!("selection filter -F drop");
    /// assert_eq!(split, act.into());
    /// ```
    Drop,
}

impl MatchAction {
    /// Whether this action is [MatchAction::Keep].
    pub fn is_keep(&self) -> bool {
        matches!(self, MatchAction::Keep)
    }

    /// Whether this action is [MatchAction::Drop].
    pub fn is_drop(&self) -> bool {
        matches!(self, MatchAction::Drop)
    }
}

/// Methods for determining the start and end of a [RangeSpec].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeEndingType {
    /// A specific line number.
    Absolute(Count),

    /// All lines.
    All,

    /// The current line.
    Current,

    /// The last line.
    Last,

    /// The position of a given [Mark].
    Mark(Specifier<Mark>),

    /// The line matching a search using the last value of [CommandType::Search].
    Search(MoveDir1D),

    /// Perform a search using the last substitution pattern.
    SubPatSearch(MoveDir1D),

    /// No line was specified.
    Unspecified,
}

/// Modifier to a range ending.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeEndingModifier {
    /// Offset the end of a range by [*n*](Count) lines.
    Offset(MoveDir1D, Count),
}

/// One of the sides of a range.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RangeEnding(pub RangeEndingType, pub Vec<RangeEndingModifier>);

/// Position to begin a search in a range.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeSearchInit {
    /// Start from current cursor position.
    Cursor,

    /// Start from the beginning of the range.
    Start,
}

/// A range specification.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeSpec {
    /// A range specification where only one end of the range was given.
    Single(RangeEnding),

    /// A range specification where both ends of the range were given.
    Double(RangeEnding, RangeEnding, RangeSearchInit),
}

/// Trait for objects that allow toggling line wrapping.
pub trait Wrappable {
    /// Set whether or not displayed lines should be wrapped.
    fn set_wrap(&mut self, wrap: bool);
}

/// Information about what portion of a buffer is being displayed in a window.
pub struct ViewportContext<Cursor> {
    /// The line and column offset into the buffer shown at the upper-left hand corner of the
    /// window.
    pub corner: Cursor,

    /// Dimensions of the window.
    pub dimensions: (usize, usize),

    /// Whether or not displayed lines are being wrapped.
    pub wrap: bool,
}

impl<Cursor: Default> ViewportContext<Cursor> {
    /// Create a new context for describing a viewport.
    pub fn new() -> Self {
        ViewportContext {
            corner: Cursor::default(),
            dimensions: (0, 0),
            wrap: false,
        }
    }

    /// Get the viewport height.
    pub fn get_height(&self) -> usize {
        self.dimensions.1
    }

    /// Get the viewport width.
    pub fn get_width(&self) -> usize {
        self.dimensions.0
    }
}

impl<Cursor: Default> Default for ViewportContext<Cursor> {
    fn default() -> Self {
        ViewportContext::new()
    }
}

impl<Cursor: Clone> Clone for ViewportContext<Cursor> {
    fn clone(&self) -> Self {
        ViewportContext {
            corner: self.corner.clone(),
            dimensions: self.dimensions,
            wrap: self.wrap,
        }
    }
}

impl<Cursor: Wrappable> Wrappable for ViewportContext<Cursor> {
    fn set_wrap(&mut self, wrap: bool) {
        self.wrap = wrap;
        self.corner.set_wrap(wrap);
    }
}

/// This context object wraps information used when calculating what text covered by cursor
/// movements.
pub struct CursorMovementsContext<'a, Cursor> {
    /// What operation this movement is being done as part of.
    ///
    /// Certain movements, like [MoveType::WordBegin], behave different depending on the action.
    pub action: &'a EditAction,

    /// Information about the user's view of the text, since this impacts movements that rely on
    /// how the text is displayed, such as [MoveType::ScreenLine].
    pub view: &'a ViewportContext<Cursor>,

    /// The editing context contains information about the current [InsertStyle], as well as the
    /// user-supplied [Count].
    pub context: &'a EditContext,
}

/// Trait for objects capable of calculating contextual offsets from a cursor.
pub trait CursorMovements<Cursor> {
    /// Calculate the position of the first word on the line of the provided cursor.
    fn first_word(&self, cursor: &Cursor, ctx: &CursorMovementsContext<'_, Cursor>) -> Cursor;

    /// Calculate the position of the cursor after performing a movement.
    fn movement(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'_, Cursor>,
    ) -> Option<Cursor>;

    /// Calculate a cursor range from the given cursor to the location after performing the
    /// given movement.
    fn range_of_movement(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'_, Cursor>,
    ) -> Option<EditRange<Cursor>>;

    /// Calculate a cursor range based on a given cursor position and a [RangeType].
    fn range(
        &self,
        cursor: &Cursor,
        range: &RangeType,
        inclusive: bool,
        count: &Count,
        ctx: &CursorMovementsContext<'_, Cursor>,
    ) -> Option<EditRange<Cursor>>;
}

/// Trait for objects capable of searching text.
pub trait CursorSearch<Cursor> {
    /// Search for a specific character.
    fn find_char(
        &self,
        cursor: &Cursor,
        inclusive: bool,
        dir: MoveDir1D,
        multiline: bool,
        needle: char,
        count: usize,
    ) -> Option<Cursor>;

    /// Find matches for a regular expression within a range.
    fn find_matches(&self, start: &Cursor, end: &Cursor, needle: &Regex) -> Vec<EditRange<Cursor>>;

    /// Search for a regular expression.
    fn find_regex(
        &self,
        cursor: &Cursor,
        dir: MoveDir1D,
        needle: &Regex,
        count: usize,
    ) -> Option<EditRange<Cursor>>;
}

/// Trait for directions capable of being flipped.
pub trait Flip {
    /// Return the flipped representation of the value.
    fn flip(&self) -> Self;
}

impl Flip for MoveDir1D {
    fn flip(&self) -> MoveDir1D {
        match self {
            MoveDir1D::Previous => MoveDir1D::Next,
            MoveDir1D::Next => MoveDir1D::Previous,
        }
    }
}

impl Flip for MoveDir2D {
    fn flip(&self) -> MoveDir2D {
        match self {
            MoveDir2D::Left => MoveDir2D::Right,
            MoveDir2D::Right => MoveDir2D::Left,
            MoveDir2D::Up => MoveDir2D::Down,
            MoveDir2D::Down => MoveDir2D::Up,
        }
    }
}

impl MoveDir2D {
    /// Returns the [Axis] that the direction moves along.
    pub fn axis(&self) -> Axis {
        match self {
            MoveDir2D::Left => Axis::Horizontal,
            MoveDir2D::Right => Axis::Horizontal,
            MoveDir2D::Up => Axis::Vertical,
            MoveDir2D::Down => Axis::Vertical,
        }
    }
}

impl std::ops::Not for InsertStyle {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            InsertStyle::Insert => InsertStyle::Replace,
            InsertStyle::Replace => InsertStyle::Insert,
        }
    }
}

impl MoveType {
    /// Returns `true` if this is an inclusive motion.
    pub fn is_inclusive_motion(&self) -> bool {
        match self {
            MoveType::BufferPos(_) => true,
            MoveType::FinalNonBlank(_) => true,
            MoveType::ItemMatch => true,
            MoveType::LineColumnOffset => true,
            MoveType::WordEnd(_, _) => true,

            MoveType::BufferByteOffset => false,
            MoveType::BufferLineOffset => false,
            MoveType::BufferLinePercent => false,
            MoveType::Column(_, _) => false,
            MoveType::FirstWord(_) => false,
            MoveType::Line(_) => false,
            MoveType::LinePercent => false,
            MoveType::LinePos(_) => false,
            MoveType::ParagraphBegin(_) => false,
            MoveType::ScreenFirstWord(_) => false,
            MoveType::ScreenLine(_) => false,
            MoveType::ScreenLinePos(_) => false,
            MoveType::ViewportPos(_) => false,
            MoveType::SectionBegin(_) => false,
            MoveType::SectionEnd(_) => false,
            MoveType::SentenceBegin(_) => false,
            MoveType::WordBegin(_, _) => false,
        }
    }

    /// Returns `true` if this is a motion that causes cursor positions to be saved to
    /// [PositionList::JumpList].
    pub fn is_jumping(&self) -> bool {
        match self {
            MoveType::BufferByteOffset => true,
            MoveType::BufferLineOffset => true,
            MoveType::BufferLinePercent => true,
            MoveType::BufferPos(_) => true,
            MoveType::ItemMatch => true,
            MoveType::ParagraphBegin(_) => true,
            MoveType::ViewportPos(_) => true,
            MoveType::SectionBegin(_) => true,
            MoveType::SentenceBegin(_) => true,

            MoveType::Column(_, _) => false,
            MoveType::FinalNonBlank(_) => false,
            MoveType::FirstWord(_) => false,
            MoveType::LineColumnOffset => false,
            MoveType::Line(_) => false,
            MoveType::LinePercent => false,
            MoveType::LinePos(_) => false,
            MoveType::ScreenFirstWord(_) => false,
            MoveType::ScreenLine(_) => false,
            MoveType::ScreenLinePos(_) => false,
            MoveType::SectionEnd(_) => false,
            MoveType::WordBegin(_, _) => false,
            MoveType::WordEnd(_, _) => false,
        }
    }

    /// Returns the shape of the text selected by this movement when editing.
    pub fn shape(&self) -> TargetShape {
        match self {
            MoveType::BufferLineOffset => TargetShape::LineWise,
            MoveType::BufferLinePercent => TargetShape::LineWise,
            MoveType::BufferPos(_) => TargetShape::LineWise,
            MoveType::FirstWord(_) => TargetShape::LineWise,
            MoveType::Line(_) => TargetShape::LineWise,
            MoveType::ViewportPos(_) => TargetShape::LineWise,
            MoveType::SectionBegin(_) => TargetShape::LineWise,
            MoveType::SectionEnd(_) => TargetShape::LineWise,

            MoveType::BufferByteOffset => TargetShape::CharWise,
            MoveType::Column(_, _) => TargetShape::CharWise,
            MoveType::FinalNonBlank(_) => TargetShape::CharWise,
            MoveType::ItemMatch => TargetShape::CharWise,
            MoveType::LineColumnOffset => TargetShape::CharWise,
            MoveType::LinePercent => TargetShape::CharWise,
            MoveType::LinePos(_) => TargetShape::CharWise,
            MoveType::ParagraphBegin(_) => TargetShape::CharWise,
            MoveType::ScreenFirstWord(_) => TargetShape::CharWise,
            MoveType::ScreenLinePos(_) => TargetShape::CharWise,
            MoveType::ScreenLine(_) => TargetShape::CharWise,
            MoveType::SentenceBegin(_) => TargetShape::CharWise,
            MoveType::WordBegin(_, _) => TargetShape::CharWise,
            MoveType::WordEnd(_, _) => TargetShape::CharWise,
        }
    }
}

impl MoveDirMod {
    /// Modify a given direction.
    pub fn resolve(&self, dir: &MoveDir1D) -> MoveDir1D {
        match self {
            MoveDirMod::Same => *dir,
            MoveDirMod::Flip => dir.flip(),
            MoveDirMod::Exact(exact) => *exact,
        }
    }
}

impl From<MoveDir1D> for MoveDirMod {
    fn from(dir: MoveDir1D) -> Self {
        MoveDirMod::Exact(dir)
    }
}

/// Information to show the user at the bottom of the screen after an action.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InfoMessage {
    /// Print a simple, informational message on the status line.
    Message(String),

    /// Use an interactive pager to show the user some information.
    ///
    /// If you're using [keybindings], then you can handle this using [Pager] and
    /// [BindingMachine::run_dialog].
    ///
    /// [Pager]: keybindings::dialog::Pager
    /// [BindingMachine::run_dialog]: keybindings::BindingMachine::run_dialog
    Pager(String),
}

impl Display for InfoMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InfoMessage::Message(s) | InfoMessage::Pager(s) => write!(f, "{}", s),
        }
    }
}

impl From<&str> for InfoMessage {
    fn from(msg: &str) -> Self {
        InfoMessage::from(msg.to_string())
    }
}

impl From<String> for InfoMessage {
    fn from(msg: String) -> Self {
        InfoMessage::Message(msg)
    }
}

/// An optional, information message provided during editing.
pub type EditInfo = Option<InfoMessage>;
