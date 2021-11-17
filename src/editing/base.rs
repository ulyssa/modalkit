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
//! let _ = Action::Edit(EditAction::Delete.into(), EditTarget::Selection);
//!
//! // Copy the next three lines.
//! use modalkit::editing::base::{RangeType};
//!
//! let _ = Action::Edit(EditAction::Yank.into(), EditTarget::Range(RangeType::Line, 3.into()));
//!
//! // Make some contextually specified number of words lowercase.
//! use modalkit::editing::base::{Case, Count, MoveDir1D, MoveType, WordStyle};
//!
//! let _ = Action::Edit(
//!     EditAction::ChangeCase(Case::Lower).into(),
//!     EditTarget::Motion(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next), Count::Contextual)
//! );
//!
//! // Scroll the viewport so that line 10 is at the top of the screen.
//! use modalkit::editing::base::{MovePosition, ScrollStyle};
//!
//! let _ = Action::Scroll(ScrollStyle::LinePos(MovePosition::Beginning, 10.into()));
//! ```

use bitflags::bitflags;
use regex::Regex;

use crate::util::sort2;

/// Specify how to change the case of a string.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Case {
    /// Make the targeted text uppercase.
    Upper,
    /// Make the targeted text lowercase.
    Lower,
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
    pub fn new(a: Cursor, b: Cursor, shape: TargetShape, inclusive: bool) -> Self {
        let (start, end) = sort2(a, b);

        EditRange { start, end, shape, inclusive }
    }

    pub fn inclusive(a: Cursor, b: Cursor, shape: TargetShape) -> Self {
        Self::new(a, b, shape, true)
    }

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

    /// Search for the regular expression indicated by [EditContext::get_search_regex].
    Regex,
}

/// The different ways of grouping a buffer's contents into words.
#[derive(Clone, Debug, Eq, PartialEq)]
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

    /// Select the current word the cursor is in.
    Word(WordStyle),

    /// Select the current block specified by the start and end characters.
    Bracketed(char, char, bool), // start, end, inclusive

    Quote(char, bool), // quote mark, inclusive

    XmlTag(bool), // inclusive
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
    Previous,
    Next,
}

/// Represent movement along the horizontal or vertical axes.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveDir2D {
    Left,
    Right,
    Up,
    Down,
}

/// Represent movement to a position along a 1-dimensional line.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MovePosition {
    Beginning,
    Middle,
    End,
}

/// Represent a modification of a previous movement.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveDirMod {
    Same,
    Flip,
}

/// This represents a selection of an axis.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Axis {
    Horizontal,
    Vertical,
}

/// This represents the units used when scrolling.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScrollSize {
    Cell,
    HalfPage,
    Page,
}

/// This represents the way in which the viewport should be scrolled.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScrollStyle {
    /// Scroll the viewport in MoveDir2D direction by ScrollSize units, Count times.
    Direction2D(MoveDir2D, ScrollSize, Count),

    /// Scroll the viewport so that the cursor is placed at MovePosition relative to Axis.
    CursorPos(Axis, MovePosition),

    /// Scroll the viewport so that the Count specified line is at MovePosition on the screen.
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
pub enum SizeChange {
    Equal,
    Exact,
    Decrease,
    Increase,
}

/// This represents how to change the indentation of a range.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IndentChange {
    Auto,
    Decrease(Count),
    Increase(Count),
}

/// This represents how to change a number in text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NumberChange {
    DecreaseOne,
    DecreaseAll,
    IncreaseOne,
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
        const NONE = 0b00000000;

        const WRITE = 0b00000001;
        const FORCE = 0b00000010;
        const QUIT  = 0b00000100;

        const WQ = CloseFlags::WRITE.bits | CloseFlags::QUIT.bits;
        const FQ = CloseFlags::FORCE.bits | CloseFlags::QUIT.bits;
    }
}

/// The result of either pressing a complete keybinding sequence, or parsing a command.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Action {
    /// Do nothing.
    NoOp,

    /// Create a new editing history checkpoint.
    Checkpoint,

    /// Complete the rest of the word typed thus far.
    Complete(MoveDir1D, bool),

    /// Perform the specified [action](EditAction) on [a target](EditTarget).
    Edit(Specifier<EditAction>, EditTarget),

    /// Repeat the last editing action [*n*](Count) times.
    EditRepeat(Count),

    /// Navigate through the cursor positions in [the specified list](PositionList).
    Jump(PositionList, MoveDir1D, Count),

    /// Create a new [Mark] at the current cursor position.
    Mark(Specifier<Mark>),

    /// Open [*n*](Count) new lines before or after the current line.
    OpenLine(MoveDir1D),

    /// Paste before or after the current cursor position [*n*](Count) times.
    Paste(MoveDir1D, Count),

    Redo(Count),

    Undo(Count),

    /// Scroll the viewport in [the specified manner](ScrollStyle).
    Scroll(ScrollStyle),

    /// Change the placement of the cursor and anchor of a visual selection.
    SelectionCursorSet(SelectionCursorChange),

    /// Close the [targeted cursors](CursorCloseTarget) in the current cursor group.
    CursorClose(CursorCloseTarget),

    /// Rotate which cursor in the cursor group is the current leader .
    CursorRotate(MoveDir1D, Count),

    /// Convert a cursor into [*n*](Count) cursors.
    CursorSplit(Count),

    /// Split [matching selections](TargetShapeFilter) into multiple selections, each on their own
    /// line.
    SelectionSplitLines(TargetShapeFilter),

    /// Lookup the keyword under the cursor.
    KeywordLookup,

    /// Redraw the screen.
    RedrawScreen,

    /// Perform the submit action for the currently focused UI element.
    Submit,

    /// Suspend the process.
    Suspend,

    /// Execute the contents of the contextually specified Register [*n* times](Count).
    MacroExecute(Count),

    /// Execute the contents of the previously specified macro [*n* times](Count).
    MacroRepeat(Count),

    /// Start or stop recording a macro.
    MacroRecordToggle,

    /// Type a [character](Char).
    Type(Specifier<Char>),

    /// Repeat the last executed command [*n* times](Count).
    CommandRepeat(Count),

    /// Switch focus to the command bar so that the user can enter [CommandType] text.
    CommandFocus(CommandType),

    CommandUnfocus,

    /// Close the [CloseTarget] tabs with [CloseFlags] options.
    TabClose(CloseTarget, CloseFlags),

    /// Change the currently focus to the tab targeted by [FocusChange].
    TabFocus(FocusChange),

    /// Close the [CloseTarget] windows with [CloseFlags] options.
    WindowClose(CloseTarget, CloseFlags),

    /// Exchange the currently focused window with the window targeted by [FocusChange].
    WindowExchange(FocusChange),

    /// Change the current focus to the window targeted by [FocusChange].
    WindowFocus(FocusChange),

    /// Move the currently focused window to the [MoveDir2D] side of the screen.
    WindowMoveSide(MoveDir2D),

    /// Visually rotate the windows in [MoveDir2D] direction.
    WindowRotate(MoveDir1D),

    /// Split the currently focused window along [*n* times](Count) along [an axis](Axis), moving
    /// the focus in [MoveDir1D] direction after performing the split.
    WindowSplit(Axis, MoveDir1D, Count),

    /// Resize the currently focused window according to [SizeChange].
    WindowResize(Axis, SizeChange, Count),
}

/// When focusing on the command bar, this is the type of command that should be submitted.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommandType {
    Command,
    Search(MoveDir1D),
}

/// This specifies which list of cursors to use when jumping, the change list or the jump list.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PositionList {
    ChangeList,
    JumpList,
}

/// This specifies the behaviour of entering and backspacing over characters.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InsertStyle {
    Insert,
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
pub enum Register {
    /// The default register.
    ///
    /// For example, `""` in Vim.
    Unnamed,

    /// Recently deleted text.
    ///
    /// For example, `"[1-9]` in Vim.
    RecentlyDeleted(usize), // "[1-9]

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
    CharWise,
    LineWise,
    BlockWise,
}

bitflags! {
    pub struct TargetShapeFilter: u32 {
        const NONE = 0b00000000;
        const ALL = 0b00000111;

        const CHAR = 0b00000001;
        const LINE = 0b00000010;
        const BLOCK = 0b00000100;
    }
}

impl TargetShapeFilter {
    pub fn matches(&self, shape: &TargetShape) -> bool {
        match shape {
            TargetShape::CharWise => self.contains(TargetShapeFilter::CHAR),
            TargetShape::LineWise => self.contains(TargetShapeFilter::LINE),
            TargetShape::BlockWise => self.contains(TargetShapeFilter::BLOCK),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeEndingType {
    Absolute(Count),
    All,
    Current,
    Last,
    Mark(Specifier<Mark>),
    Search(MoveDir1D),
    SubPatSearch(MoveDir1D),
    Unspecified,
}

#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeEndingModifier {
    Offset(MoveDir1D, Count),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RangeEnding(pub RangeEndingType, pub Vec<RangeEndingModifier>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeSearchInit {
    Cursor,
    Start,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RangeSpec {
    Single(RangeEnding),
    Double(RangeEnding, RangeEnding, RangeSearchInit),
}

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

    /// Returns a regular expression to search for in the buffer, and the direction in
    /// which to search.
    fn get_search_regex(&self) -> Option<(MoveDir1D, Regex)>;

    /// Returns a character to search for on the current line, and the direction in
    /// which to search.
    fn get_search_char(&self) -> Option<(MoveDir1D, bool, Char)>;

    /// Returns a [character](Char) to use when performing an [EditAction::Replace] operation.
    fn get_replace_char(&self) -> Option<Char>;
}

/// Trait for values that can be converted by the [EditContext].
pub trait Resolve<T, R> {
    fn resolve(&self, t: &T) -> R;
}

pub trait Wrappable {
    fn set_wrap(&mut self, wrap: bool);
}

pub struct ViewportContext<Cursor> {
    pub corner: Cursor,
    pub dimensions: (usize, usize),
    pub wrap: bool,
}

impl<Cursor: Default> ViewportContext<Cursor> {
    pub fn new() -> Self {
        ViewportContext {
            corner: Cursor::default(),
            dimensions: (0, 0),
            wrap: false,
        }
    }

    pub fn get_height(&self) -> usize {
        self.dimensions.1
    }

    pub fn get_width(&self) -> usize {
        self.dimensions.0
    }
}

impl<Cursor: Default> Default for ViewportContext<Cursor> {
    fn default() -> Self {
        ViewportContext::new()
    }
}

impl<Cursor: Wrappable> Wrappable for ViewportContext<Cursor> {
    fn set_wrap(&mut self, wrap: bool) {
        self.wrap = wrap;
        self.corner.set_wrap(wrap);
    }
}

#[non_exhaustive]
pub struct CursorMovementsContext<'a, 'b, 'c, Cursor, C: EditContext> {
    pub action: &'a EditAction,
    pub view: &'b ViewportContext<Cursor>,
    pub context: &'c C,
}

pub trait CursorMovements<Cursor, Context: EditContext> {
    fn first_word<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
    ) -> Cursor;

    fn movement<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
    ) -> Option<Cursor>;

    fn range_of_movement<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        movement: &MoveType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
    ) -> Option<EditRange<Cursor>>;

    fn range<'a, 'b, 'c>(
        &self,
        cursor: &Cursor,
        range: &RangeType,
        count: &Count,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, Context>,
    ) -> Option<EditRange<Cursor>>;
}

pub trait CursorSearch<Cursor> {
    fn find_char(
        &self,
        cursor: &Cursor,
        inclusive: bool,
        dir: MoveDir1D,
        multiline: bool,
        needle: char,
        count: usize,
    ) -> Option<Cursor>;

    fn find_regex(
        &self,
        cursor: &Cursor,
        dir: MoveDir1D,
        needle: Regex,
        count: usize,
    ) -> Option<Cursor>;
}

/// Trait for directions capable of being flipped.
pub trait Flip {
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
            MoveType::WordBegin(_, _) => TargetShape::CharWise,
            MoveType::WordEnd(_, _) => TargetShape::CharWise,
        }
    }
}

impl MoveDirMod {
    pub fn resolve<T: Flip + Clone>(&self, dir: &T) -> T {
        match self {
            MoveDirMod::Same => dir.clone(),
            MoveDirMod::Flip => dir.flip(),
        }
    }
}

pub struct EditInfo {
    msg: String,
}

impl std::fmt::Display for EditInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum EditError {
    #[error("No current selection")]
    NoSelection,
    #[error("Invalid cursor group")]
    InvalidCursorGroup,
    #[error("Invalid cursor")]
    InvalidCursor,
    #[error("Invalid digraph: {0:?} {1:?}")]
    InvalidDigraph(char, char),
    #[error("Mark not set")]
    MarkNotSet(Mark),
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Input/Output Error: {0}")]
    TerminalError(#[from] crossterm::ErrorKind),
    #[error("Error: {0}")]
    Failure(String),
}

pub type EditResult<V = Option<EditInfo>> = Result<V, EditError>;
