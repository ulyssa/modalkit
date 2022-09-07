//! # Editor Types
//!
//! ## Overview
//!
//! The types in this module provides a defunctionalized view of a text editor. Consumers of these
//! types should map them into text manipulation or user interface actions.
//!
//! ## Examples
//!
//! ```
//! use modalkit::editing::base::{Action, EditAction, EditTarget};
//!
//! // Delete the current text selection.
//! let _: Action = Action::Edit(EditAction::Delete.into(), EditTarget::Selection);
//!
//! // Copy the next three lines.
//! use modalkit::editing::base::{RangeType};
//!
//! let _: Action = Action::Edit(EditAction::Yank.into(), EditTarget::Range(RangeType::Line, 3.into()));
//!
//! // Make some contextually specified number of words lowercase.
//! use modalkit::editing::base::{Case, Count, MoveDir1D, MoveType, WordStyle};
//!
//! let _: Action = Action::Edit(
//!     EditAction::ChangeCase(Case::Lower).into(),
//!     EditTarget::Motion(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next), Count::Contextual)
//! );
//!
//! // Scroll the viewport so that line 10 is at the top of the screen.
//! use modalkit::editing::base::{MovePosition, ScrollStyle};
//!
//! let _: Action = Action::Scroll(ScrollStyle::LinePos(MovePosition::Beginning, 10.into()));
//! ```
use std::fmt::Debug;

use bitflags::bitflags;
use regex::Regex;

use crate::{
    input::commands::{Command, CommandError},
    util::sort2,
};

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

/// The various actions that can be taken on text.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EditAction {
    /// Move the cursor.
    ///
    /// If a shape is [specified contextually](EditContext::get_target_shape), then visually select
    /// text while moving.
    Motion,
    /// Delete the targeted text.
    Delete,
    /// Yank the targeted text into a [Register].
    Yank,
    /// Replace characters within the targeted text with a new character.
    ///
    /// If [bool] is true, virtually replace characters by how many columns they occupy.
    Replace(bool),
    /// Automatically format the targeted text.
    Format,
    /// Change a number within the targeted text.
    ChangeNumber(NumberChange),
    /// Join the lines within the targeted text together.
    ///
    /// If [bool] is true, modify spacing when joining.
    Join(bool),
    /// Change the indent level of the targeted text.
    Indent(IndentChange),
    /// Change the case of the targeted text.
    ChangeCase(Case),
}

impl EditAction {
    /// Returns true if the value is [EditAction::Motion].
    pub fn is_motion(&self) -> bool {
        matches!(self, EditAction::Motion)
    }
}

impl Default for EditAction {
    fn default() -> Self {
        EditAction::Motion
    }
}

/// Specify what is targeted by an editing action.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum EditTarget {
    /// Target the current cursor position.
    CurrentPosition,

    /// Move to the line and column of a [Mark].
    CharJump(Specifier<Mark>),

    /// Move to the first word of the line that [Mark] is on.
    LineJump(Specifier<Mark>),

    /// Target the text between the current cursor position and the end of a motion.
    Motion(MoveType, Count),

    /// Target a range of text around the cursor.
    Range(RangeType, Count),

    /// Target the text between the current cursor position and the end of a search.
    ///
    /// The [MoveDirMod] parameter modifies the search direction.
    Search(SearchType, MoveDirMod, Count),

    /// Target the visually selected text.
    Selection,
}

impl From<MoveType> for EditTarget {
    fn from(mt: MoveType) -> Self {
        EditTarget::Motion(mt, Count::Contextual)
    }
}

impl From<RangeType> for EditTarget {
    fn from(mt: RangeType) -> Self {
        EditTarget::Range(mt, Count::Contextual)
    }
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

/// Specify a range within the text around the current cursor position.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SearchType {
    /// Search for the character indicated by [EditContext::get_search_char].
    ///
    /// [bool] controls whether the search should continue across line boundaries.
    Char(bool),

    /// Search for a regular expression.
    Regex,

    /// Search for the word currently under the cursor, and update [Register::LastSearch] via
    /// [Store::set_last_search].
    ///
    /// [bool] controls whether matches should be checked for using word boundaries.
    ///
    /// [Store::set_last_search]: crate::editing::store::Store::set_last_search
    Word(WordStyle, bool),
}

/// The different ways of grouping a buffer's contents into words.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WordStyle {
    /// Either a sequence of alphanumeric characters and underscores, or a sequence of other
    /// non-blank characters. An empty line is also a Little word.
    Little,

    /// A sequence of non-blank characters. An empty line also a Big word.
    ///
    /// Vim calls this a `WORD`.
    Big,
}

/// Specify a range within the text around the current cursor position.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RangeType {
    /// Select the whole buffer.
    Buffer,

    /// Select the current paragraph the cursor is in.
    Paragraph,

    /// Select the current sentence the cursor is in.
    Sentence,

    /// Select the current line the cursor is on.
    Line,

    /// Select a range of whitespace around the cursor.
    ///
    /// [bool] controls whether this crosses lines.
    Whitespace(bool),

    /// Select the current word the cursor is in.
    Word(WordStyle),

    /// Select the current block specified by the start and end characters.
    Bracketed(char, char, bool), // start, end, inclusive

    /// Select text quoted by [char] around the cursor.
    ///
    /// [bool] indicates whether the selection should include the quote characters.
    Quote(char, bool),

    /// Select the XML block around the cursor.
    ///
    /// [bool] indicates whether to include the XML tags for the block.
    XmlTag(bool),
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

    /// Move to the column just after the end of a word [*n* times](Count) in [MoveDir1D] direction.
    WordAfter(WordStyle, MoveDir1D),

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
    SwapAnchor(bool),
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
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NumberChange {
    /// Decrease the first number in the targeted text by [*n*](Count).
    DecreaseOne,

    /// Decrease the first number of each line in the targeted text by [*n*](Count) on the first
    /// number seen, [*n*](Count) times two for the second number seen, etc..
    DecreaseAll,

    /// Increase the first number in the targeted text by [*n*](Count).
    IncreaseOne,

    /// Increase the first number of each line in the targeted text by [*n*](Count) on the first
    /// number seen, [*n*](Count) times two for the second number seen, etc.
    IncreaseAll,
}

/// This represents what windows are targeted by a window command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CloseTarget {
    /// Close the element targeted by FocusChange.
    Single(FocusChange),
    /// Close all elements *except* for the one targeted by FocusChange.
    AllBut(FocusChange),
    /// Close all elements.
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Specifier<T> {
    /// Look for a value of `T` in the [EditContext].
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
    /// These flags are used to specify the behaviour surrounding closing a window.
    pub struct CloseFlags: u32 {
        /// No flags set.
        const NONE = 0b00000000;

        /// Write while closing.
        const WRITE = 0b00000001;

        /// Ignore any issues during closing.
        const FORCE = 0b00000010;

        /// Quit if this is the last window.
        const QUIT  = 0b00000100;

        /// Write out the window's contents and quit.
        const WQ = CloseFlags::WRITE.bits | CloseFlags::QUIT.bits;

        /// Force quit the window.
        const FQ = CloseFlags::FORCE.bits | CloseFlags::QUIT.bits;
    }
}

/// Text insertion actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum InsertTextAction {
    /// Insert a new line [shape-wise](TargetShape) before or after the current position.
    OpenLine(TargetShape, MoveDir1D),

    /// Paste before or after the current cursor position [*n*](Count) times.
    Paste(MoveDir1D, Count),

    /// Type a [character](Char) on [either side](MoveDir1D) of the cursor.
    Type(Specifier<Char>, MoveDir1D),
}

/// Editing history actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum HistoryAction {
    /// Create a new editing history checkpoint.
    Checkpoint,

    /// Redo [*n*](Count) edits.
    Redo(Count),

    /// Undo [*n*](Count) edits.
    Undo(Count),
}

/// Cursor group actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum CursorAction {
    /// Close the [targeted cursors](CursorCloseTarget) in the current cursor group.
    Close(CursorCloseTarget),

    /// Convert a cursor into [*n*](Count) cursors.
    Split(Count),
}

/// Command actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum CommandAction {
    /// Execute a command string.
    ///
    /// This should update [Register::LastCommand].
    Execute(String),

    /// Repeat the last executed command [*n* times](Count).
    Repeat(Count),
}

/// Command bar actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum CommandBarAction {
    /// Open the command bar so that the user can enter [CommandType] text.
    Focus(CommandType),

    /// Abort command entry.
    Abort,

    /// Submit the currently entered text.
    Submit,

    /// Move backwards and forwards through previous entries.
    Recall(MoveDir1D, Count),
}

/// Macro actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum MacroAction {
    /// Execute the contents of the contextually specified Register [*n* times](Count).
    Execute(Count),

    /// Execute the contents of the previously specified macro [*n* times](Count).
    Repeat(Count),

    /// Start or stop recording a macro.
    ToggleRecording,
}

/// Tab actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum TabAction {
    /// Close the [CloseTarget] tabs with [CloseFlags] options.
    Close(CloseTarget, CloseFlags),

    /// Change the currently focus to the tab targeted by [FocusChange].
    Focus(FocusChange),
}

/// Window actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum WindowAction {
    /// Close the [CloseTarget] windows with [CloseFlags] options.
    Close(CloseTarget, CloseFlags),

    /// Exchange the currently focused window with the window targeted by [FocusChange].
    Exchange(FocusChange),

    /// Change the current focus to the window targeted by [FocusChange].
    Focus(FocusChange),

    /// Move the currently focused window to the [MoveDir2D] side of the screen.
    MoveSide(MoveDir2D),

    /// Visually rotate the windows in [MoveDir2D] direction.
    Rotate(MoveDir1D),

    /// Split the currently focused window along [*n* times](Count) along [an axis](Axis), moving
    /// the focus in [MoveDir1D] direction after performing the split.
    Split(Axis, MoveDir1D, Count),

    /// Clear all of the explicitly set window sizes, and instead try to equally distribute
    /// available rows and columns.
    ClearSizes,

    /// Resize the currently focused window according to [SizeChange].
    Resize(Axis, SizeChange),

    /// Zoom in on the currently focused window so that it takes up the whole screen. If there is
    /// already a zoomed-in window, then return to showing all windows.
    ZoomToggle,
}

/// Trait for objects that describe application-specific actions.
///
/// Implementors of this trait can be used with [Action::Application]. This can then be used to
/// create additional keybindings and commands on top of the defaults provided by modules like
/// [modalkit::vim](crate::vim).
pub trait ApplicationAction: Clone + Debug + Eq + PartialEq {}

impl ApplicationAction for () {}

/// Trait for objects that hold application-specific information.
///
/// Implementors of this trait can be embedded in [Store](super::store::Store).
pub trait ApplicationStore: Default {}

impl ApplicationStore for () {}

/// Trait for objects that describe application-specific behaviour and types.
pub trait Application: Clone + Debug + Eq + PartialEq {
    /// The type for application-specific actions.
    type Action: ApplicationAction;

    /// The type for application-specific storage.
    type Store: ApplicationStore;
}

impl Application for () {
    type Action = ();
    type Store = ();
}

/// The result of either pressing a complete keybinding sequence, or parsing a command.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Action<P: Application = ()> {
    /// Do nothing.
    NoOp,

    /// Perform a history operation.
    History(HistoryAction),

    /// Perform a macro-related action.
    Macro(MacroAction),

    /// Complete the rest of the word typed thus far.
    Complete(MoveDir1D, bool),

    /// Perform the specified [action](EditAction) on [a target](EditTarget).
    Edit(Specifier<EditAction>, EditTarget),

    /// Insert text.
    InsertText(InsertTextAction),

    /// Repeat the last editing action [*n*](Count) times.
    EditRepeat(Count),

    /// Navigate through the cursor positions in [the specified list](PositionList).
    Jump(PositionList, MoveDir1D, Count),

    /// Create a new [Mark] at the current cursor position.
    Mark(Specifier<Mark>),

    /// Scroll the viewport in [the specified manner](ScrollStyle).
    Scroll(ScrollStyle),

    /// Change the placement of the cursor and anchor of a visual selection.
    SelectionCursorSet(SelectionCursorChange),

    /// Modify the current cursor group.
    Cursor(CursorAction),

    /// Split [matching selections](TargetShapeFilter) into multiple selections, each on their own
    /// line.
    SelectionSplitLines(TargetShapeFilter),

    /// Lookup the keyword under the cursor.
    KeywordLookup,

    /// Redraw the screen.
    RedrawScreen,

    /// Suspend the process.
    Suspend,

    /// Find the [*n*<sup>th</sup>](Count) occurrence of the current application-level search.
    Search(MoveDirMod, Count),

    /// Perform a command-related action.
    Command(CommandAction),

    /// Perform a command bar-related action.
    CommandBar(CommandBarAction),

    /// Perform a tab-related action.
    Tab(TabAction),

    /// Perform a window-related action.
    Window(WindowAction),

    /// Application-specific command.
    Application(P::Action),
}

impl<P: Application> From<InsertTextAction> for Action<P> {
    fn from(act: InsertTextAction) -> Self {
        Action::InsertText(act)
    }
}

impl<P: Application> From<HistoryAction> for Action<P> {
    fn from(act: HistoryAction) -> Self {
        Action::History(act)
    }
}

impl<P: Application> From<CursorAction> for Action<P> {
    fn from(act: CursorAction) -> Self {
        Action::Cursor(act)
    }
}

impl<P: Application> From<MacroAction> for Action<P> {
    fn from(act: MacroAction) -> Self {
        Action::Macro(act)
    }
}

impl<P: Application> From<CommandAction> for Action<P> {
    fn from(act: CommandAction) -> Self {
        Action::Command(act)
    }
}

impl<P: Application> From<CommandBarAction> for Action<P> {
    fn from(act: CommandBarAction) -> Self {
        Action::CommandBar(act)
    }
}

impl<P: Application> From<WindowAction> for Action<P> {
    fn from(act: WindowAction) -> Self {
        Action::Window(act)
    }
}

impl<P: Application> From<TabAction> for Action<P> {
    fn from(act: TabAction) -> Self {
        Action::Tab(act)
    }
}

/// When focusing on the command bar, this is the type of command that should be submitted.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommandType {
    /// Prompt the user for a command.
    Command,

    /// Prompt the user for a search query.
    ///
    /// [MoveDir1D] controls which direction to search, and [bool] whether to perform an
    /// incremental search as the user types their query.
    Search(MoveDir1D, bool),
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
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Register {
    /// The default register.
    ///
    /// For example, `""` in Vim.
    Unnamed,

    /// Recently deleted text.
    ///
    /// For example, `"[1-9]` in Vim.
    RecentlyDeleted(usize),

    /// Most recently deleted text that was shorted than a line.
    ///
    /// For example, `"-` in Vim.
    SmallDelete,

    /// A register containing the last executed command.
    ///
    /// For example, `":` in Vim.
    LastCommand,

    /// A register containing the last inserted text.
    ///
    /// For example, `".` in Vim.
    LastInserted,

    /// A register containing the last search expression.
    ///
    /// For example, `"/` in Vim.
    LastSearch,

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

    /// The line matching a search using the value of [Register::LastSearch].
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

/// Trait for context objects used during editing operations.
pub trait EditContext:
    Resolve<Specifier<Char>, Option<Char>>
    + Resolve<Specifier<Mark>, Mark>
    + Resolve<Specifier<EditAction>, EditAction>
    + Resolve<Count, usize>
{
    /// Indicates a shape to be applied to an [EditAction].
    fn get_target_shape(&self) -> Option<TargetShape>;

    /// Indicates the style by which text should be inserted into the buffer.
    fn get_insert_style(&self) -> Option<InsertStyle>;

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
}

/// Trait for values that can be converted by the [EditContext].
pub trait Resolve<T, R> {
    /// Use contextual information to convert a `T` into an `R`.
    fn resolve(&self, t: &T) -> R;
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
            dimensions: self.dimensions.clone(),
            wrap: self.wrap.clone(),
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
#[non_exhaustive]
pub struct CursorMovementsContext<'a, 'b, 'c, Cursor, C: EditContext> {
    /// What operation this movement is being done as part of.
    ///
    /// Certain movements, like [MoveType::WordBegin], behave different depending on the action.
    pub action: &'a EditAction,

    /// Information about the user's view of the text, since this impacts movements that rely on
    /// how the text is displayed, such as [MoveType::ScreenLine].
    pub view: &'b ViewportContext<Cursor>,

    /// The editing context contains information about the current [InsertStyle], as well as the
    /// user-supplied [Count].
    pub context: &'c C,
}

/// Trait for objects capable of calculating contextual offsets from a cursor.
pub trait CursorMovements<Cursor, Context: EditContext> {
    /// Calculate the position of the first word on the line of the provided cursor.
    fn first_word<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
    ) -> Cursor;

    /// Calculate the position of the cursor after performing a movement.
    fn movement<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
    ) -> Option<Cursor>;

    /// Calculate a cursor range from the given cursor to the location after performing the
    /// given movement.
    fn range_of_movement<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
    ) -> Option<EditRange<Cursor>>;

    /// Calculate a cursor range based on a given cursor position and a [RangeType].
    fn range<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        range: &RangeType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
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

    /// Search for a regular expression.
    fn find_regex(
        &self,
        cursor: &Cursor,
        dir: MoveDir1D,
        needle: &Regex,
        count: usize,
    ) -> Option<Cursor>;
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
            MoveType::WordAfter(_, _) => false,
            MoveType::WordBegin(_, _) => false,
        }
    }

    /// Returns `true` if this is a motion that causes the cursor position to be saved to the jump
    /// list.
    pub fn is_jump_motion(&self) -> bool {
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
            MoveType::FirstWord(_) => false,
            MoveType::LineColumnOffset => false,
            MoveType::Line(_) => false,
            MoveType::LinePercent => false,
            MoveType::LinePos(_) => false,
            MoveType::ScreenFirstWord(_) => false,
            MoveType::ScreenLine(_) => false,
            MoveType::ScreenLinePos(_) => false,
            MoveType::SectionEnd(_) => false,
            MoveType::WordAfter(_, _) => false,
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
            MoveType::ItemMatch => TargetShape::CharWise,
            MoveType::LineColumnOffset => TargetShape::CharWise,
            MoveType::LinePercent => TargetShape::CharWise,
            MoveType::LinePos(_) => TargetShape::CharWise,
            MoveType::ParagraphBegin(_) => TargetShape::CharWise,
            MoveType::ScreenFirstWord(_) => TargetShape::CharWise,
            MoveType::ScreenLinePos(_) => TargetShape::CharWise,
            MoveType::ScreenLine(_) => TargetShape::CharWise,
            MoveType::SentenceBegin(_) => TargetShape::CharWise,
            MoveType::WordAfter(_, _) => TargetShape::CharWise,
            MoveType::WordBegin(_, _) => TargetShape::CharWise,
            MoveType::WordEnd(_, _) => TargetShape::CharWise,
        }
    }
}

impl MoveDirMod {
    /// Modify a given direction.
    pub fn resolve(&self, dir: &MoveDir1D) -> MoveDir1D {
        match self {
            MoveDirMod::Same => dir.clone(),
            MoveDirMod::Flip => dir.flip(),
            MoveDirMod::Exact(exact) => exact.clone(),
        }
    }
}

/// Additional information returned after an editing operation.
pub struct EditInfo {
    msg: String,
}

impl std::fmt::Display for EditInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

/// Errors returned from editing operation.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum EditError {
    /// Failure to fetch a word at a cursor position.
    #[error("No word underneath cursor")]
    NoCursorWord,

    /// Failure to determine a search expression to use.
    #[error("No current search specified")]
    NoSearch,

    /// Failure due to lack of a current selection.
    #[error("No current selection")]
    NoSelection,

    /// Failure due to an invalid cursor group.
    #[error("Invalid cursor group")]
    InvalidCursorGroup,

    /// Failure due to an invalid cursor.
    #[error("Invalid cursor")]
    InvalidCursor,

    /// Failure due to an umapped digraph.
    #[error("Invalid digraph: {0:?} {1:?}")]
    InvalidDigraph(char, char),

    /// Failure due to a bad regular expression.
    #[error("Invalid regular expression: {0}")]
    InvalidRegex(#[from] regex::Error),

    /// Failure due to an unset [Mark].
    #[error("Mark not set")]
    MarkNotSet(Mark),

    /// Failure due to invalid input where an integer was expected.
    #[error("Integer conversion error: {0}")]
    IntConversionError(#[from] std::num::TryFromIntError),

    /// Generic failure.
    #[error("Error: {0}")]
    Failure(String),
}

/// Wrapper for various Errors that consumers may want to combine.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum UIError<C: Command> {
    /// Failure during Input/Output.
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),

    /// Failure during editing.
    #[error("Editing error: {0}")]
    EditingFailure(#[from] EditError),

    /// Failure while attempting to execute a command.
    #[error("Failed command: {0}")]
    CommandFailure(#[from] CommandError<C>),
}

/// Common result type for editing operations.
pub type EditResult<V = Option<EditInfo>> = Result<V, EditError>;

/// Common result type for rendering and application functions.
pub type UIResult<C, V = Option<EditInfo>> = Result<V, UIError<C>>;
