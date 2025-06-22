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
    Cursor,

    /// Paste text before the selection's start, or after its end.
    Side(MoveDir1D),

    /// Replace selected text with register contents.
    Replace,
}

/// The source to search for completion candidates.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionScope {
    /// Only use completion candidates from the current buffer.
    Buffer,

    /// Use completion candidates available from all buffers.
    Global,
}

/// What type of phrase we are completing.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionSelection {
    /// Navigate through the list of completion candidates.
    List(MoveDir1D),

    /// Generate completion candidates, but don't select any from the list.
    None,

    /// Complete only the longest common prefix from the completion candidates.
    Prefix,

    /// If there is only a single completion candidate, select it.
    Single,
}

/// What type of phrase we are completing.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionType {
    /// Determine what to complete by the buffer context.
    Auto,

    /// Complete a filename.
    File,

    /// Complete the rest of the line.
    Line(CompletionScope),

    /// Complete the current word.
    Word(CompletionScope),
}

/// How to display completion candidates.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompletionDisplay {
    /// Don't display candidates.
    None,

    /// Display candidates in a bar above the command bar.
    Bar,

    /// Display candidates in a pop-up list.
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
    EditSequence,

    /// The last [Action] done.
    LastAction,

    /// The last selection resize made in a buffer.
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
    AlphaNum,

    /// A sequence of non-blank characters.
    ///
    /// An empty line is also a Big word. Vim calls this a `WORD`.
    Big,

    /// A sequence of characters that match a test function.
    CharSet(fn(char) -> bool),

    /// A name of a directory or file.
    FileName,

    /// A path to a directory or file.
    FilePath,

    /// Either a sequence of alphanumeric characters and underscores, or a sequence of other
    /// non-blank characters.
    ///
    /// An empty line is also a Little word.
    Little,

    /// A run of non-alphanumeric characters.
    NonAlphaNum,

    /// A run of digits in the given base, with an optional leading hyphen.
    Number(Radix),

    /// A run of blank characters.
    ///
    /// [bool] controls whether this crosses line boundaries.
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
    Binary,

    /// A base 8 number.
    Octal,

    /// A base 10 number.
    Decimal,

    /// A base 16 number.
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
    Previous,

    /// Move forwards, or to a following point.
    Next,
}

/// Represent movement along the horizontal or vertical axes.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveDir2D {
    /// Move leftwards.
    Left,

    /// Move rightwards.
    Right,

    /// Move upwards.
    Up,

    /// Move downwards.
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
    Beginning,

    /// Move to the middle of some range.
    Middle,

    /// Move to the end of some range.
    End,
}

/// Represents a modification of a previous [MoveDir1D] movement.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveDirMod {
    /// Use the same movement previously used.
    Same,

    /// Use the opposite of the movement previously used.
    Flip,

    /// Ignore whatever value was previously used.
    Exact(MoveDir1D),
}

/// This represents a selection of a 2-dimensional axis.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Axis {
    /// The horizontal axis.
    Horizontal,

    /// The vertical axis.
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
    Cell,

    /// Scroll by [*n*](Count) times half the page size.
    HalfPage,

    /// Scroll by [*n*](Count) times the page size.
    Page,
}

/// This represents the way in which the viewport should be scrolled.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScrollStyle {
    /// Scroll the viewport in [MoveDir2D] direction by [ScrollSize] units, [*n* times](Count).
    Direction2D(MoveDir2D, ScrollSize, Count),

    /// Scroll the viewport so that the cursor is placed at [MovePosition] relative to [Axis].
    CursorPos(MovePosition, Axis),

    /// Scroll the viewport so that the [*n*<sup>th</sup> line](Count) is at [MovePosition] on the screen.
    LinePos(MovePosition, Count),
}

/// Place the cursor at a specified position in a visual selection, with the anchor now at the
/// opposite end.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SelectionCursorChange {
    /// Place the cursor in the first line of the selection, in the first column of the selection.
    Beginning,

    /// Place the cursor in the last line of the selection, in the last column of the selection.
    End,

    /// Swap the cursor with the anchor of the selection.
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
    SwapSide,
}

/// This represents what UI element is targeted during an Action.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FocusChange {
    /// Target the currently focused UI element.
    Current,

    /// Target the [*n*<sup>th</sup> element](Count) from the beginning. The first element is numbered 1.
    ///
    /// If the specified *n* is greater than the number of elements, and [bool] is `true`, target
    /// the last element. Otherwise, do nothing.
    Offset(Count, bool),

    /// Target the element at [MovePosition] in the element list.
    Position(MovePosition),

    /// Target the previously focused element.
    PreviouslyFocused,

    /// Target the element [*n* times](Count) away in [MoveDir1D] direction.
    ///
    /// If moving [*n* times](Count) would go past the first or last element, and [bool] is `true`, wrap
    /// around to the other end of the element list and continue from there. Otherwise, do nothing.
    Direction1D(MoveDir1D, Count, bool),

    /// Target the element [*n* times](Count) away in [MoveDir2D] direction.
    Direction2D(MoveDir2D, Count),
}

/// This represents how to change the size of a window.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SizeChange<I = Count> {
    /// Make the window and others along the specified axis the same size.
    Equal,

    /// Make the window exactly a specific size along the axis.
    Exact(I),

    /// Decrease the size of the window by a specific amount.
    Decrease(I),

    /// Increase the size of the window by a specific amount.
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
    Word(WordStyle),

    /// Lookup the currently selected text.
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
    Alternate,

    /// An application-specific identifier to switch to.
    Application(W),

    /// Use the current window as the target.
    Current,

    /// Use the [word](WordStyle) under the cursor as a target name.
    Cursor(WordStyle),

    /// An absolute position in a list of targets.
    List(Count),

    /// A named target (e.g., a filename to open).
    Name(String),

    /// A window offset from the current one.
    Offset(MoveDir1D, Count),

    /// Use the selected text as a target name.
    Selection,

    /// A default window to open when no target has been specified.
    Unnamed,
}

/// This represents what tabs are targeted by a tab command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TabTarget {
    /// Close the tab targeted by FocusChange.
    Single(FocusChange),

    /// Close all tab *except* for the one targeted by FocusChange.
    AllBut(FocusChange),

    /// Close all tabs.
    All,
}

/// This represents what windows are targeted by a window command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WindowTarget {
    /// Close the window targeted by [FocusChange].
    Single(FocusChange),

    /// Close all windows *except* for the one targeted by [FocusChange].
    AllBut(FocusChange),

    /// Close all windows.
    All,
}

/// Target cursors in a cursor group.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CursorCloseTarget {
    /// Target the cursor group's leader.
    Leader,

    /// Target the cursor group's followers.
    Followers,
}

/// Ways to combine a newer cursor group with an already existing one.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CursorGroupCombineStyle {
    /// Use all of the selections from both groups.
    Append,

    /// Merge each member with the matching member in the other group.
    ///
    /// This fails if the groups have a different number of members.
    Merge(CursorMergeStyle),

    /// Use only the selections in the newer group.
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
    Union,

    /// Use the intersecting region of the two selections.
    Intersect,

    /// Select the one where the cursor is furthest in [MoveDir1D] direction.
    SelectCursor(MoveDir1D),

    /// Select the shorted selection.
    SelectShort,

    /// Select the longest selection.
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
    BufferLastExited,

    /// A user-named position in the current buffer.
    ///
    /// For example, `'[a-z]` in Vim.
    BufferNamed(char),

    /// The position of the current when the application was previously exited.
    ///
    /// Index 0 is the cursor position the last time the application exited, 1 the position the
    /// second most recent exit, and so on.
    ///
    /// For example, `'[0-9]` in Vim.
    GlobalLastExited(usize),

    /// A global, user-named position in some buffer known to the application.
    ///
    /// For example, `'[A-Z]` in Vim.
    GlobalNamed(char),

    /// The cursor position where the last change was made.
    ///
    /// For example, `'.` in Vim.
    LastChanged,

    /// The cursor position where the last text was inserted.
    ///
    /// For example, `'^` in Vim.
    LastInserted,

    /// The cursor position before the latest jump.
    ///
    /// For example, `''` and `` '` `` in Vim.
    LastJump,

    /// The position of the beginning of the last text selection.
    ///
    /// For example, `'<` in Vim.
    VisualBegin,

    /// The position of the end of the last text selection.
    ///
    /// For example, `'>` in Vim.
    VisualEnd,

    /// The position of the beginning of the last yanked text.
    ///
    /// For example, `'[` in Vim.
    LastYankedBegin,

    /// The position of the end of the last yanked text.
    ///
    /// For example, `']` in Vim.
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
        const NONE = 0b00000000;

        /// Ignore any issues during closing.
        const FORCE = 0b00000001;
    }
}

bitflags! {
    /// These flags are used to specify the behaviour while writing a window.
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
        const NONE = 0b00000000;

        /// Ignore any issues during closing.
        const FORCE = 0b00000001;

        /// Write while closing.
        const WRITE = 0b00000010;

        /// Quit if this is the last window.
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
    Line,

    /// A selection that starts on a non-whitespace character and ends on a non-whitespace
    /// character.
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
    Anchor,

    /// Split a selection at each line boundary it contains.
    Lines,

    /// Split a selection into [TargetShape::CharWise] parts based on the regular expression
    /// stored in the register for [CommandType::Search].
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
    Extend,

    /// Interpret the [EditTarget] as the bounds of a text object, and select it.
    Object,

    /// Move the anchor to the current cursor position and create a new selection from there.
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

/// This specifies which list of cursors to use when jumping, the change list or the jump list.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PositionList {
    /// The change list contains positions where changes were previously made.
    ChangeList,

    /// The jump list contains positions where the cursor was placed before jumping to a new
    /// location in the document.
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
    CharWise,

    /// A series of lines.
    ///
    /// During a selection, the two points indicate the start and end lines.
    LineWise,

    /// A block of characters.
    ///
    /// During a selection, the two points indicate opposite corners.
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
    Keep,
    /// Remove targets of the regular expression.
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
