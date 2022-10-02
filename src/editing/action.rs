//! # Editor Actions
//!
//! ## Overview
//!
//! The types in this module provides a defunctionalized view of a text editor. Consumers of these
//! types should map them into text manipulation or user interface actions.
//!
//! The traits contained here can be implemented to indicate that an object is capable of
//! processing the associated action.
//!
//! ## Examples
//!
//! ```
//! use modalkit::editing::action::{Action, EditAction};
//! use modalkit::editing::base::EditTarget;
//!
//! // Delete the current text selection.
//! let _: Action = Action::Edit(EditAction::Delete.into(), EditTarget::Selection);
//!
//! // Copy the next three lines.
//! use modalkit::editing::base::{RangeType};
//!
//! let _: Action = Action::Edit(EditAction::Yank.into(), EditTarget::Range(RangeType::Line, true, 3.into()));
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
use crate::{
    input::bindings::SequenceStatus,
    input::commands::{Command, CommandError, CommandMachine},
    input::key::MacroError,
};

use super::base::*;

/// The various actions that can be taken on text.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EditAction {
    /// Move the cursor.
    ///
    /// If a shape is [specified contextually](EditContext::get_target_shape), then visually select
    /// text while moving, as if using [SelectionAction::Resize] with
    /// [SelectionResizeStyle::Extend].
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
    Join(JoinStyle),

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

/// An object capable of performing editing operations.
pub trait Editable<C> {
    /// Perform an editing operation over the targeted text.
    fn edit(&mut self, action: &EditAction, target: &EditTarget, ctx: &C) -> EditResult;

    /// Create or update a cursor mark.
    fn mark(&mut self, name: Mark, ctx: &C) -> EditResult;

    /// Insert text relative to the current cursor position.
    fn insert_text(&mut self, act: InsertTextAction, ctx: &C) -> EditResult;

    /// Modify the current selection.
    fn selection_command(&mut self, act: SelectionAction, ctx: &C) -> EditResult;

    /// Perform an action over a cursor group.
    fn cursor_command(&mut self, act: CursorAction, ctx: &C) -> EditResult;

    /// Move to a different point in the buffer's editing history.
    fn history_command(&mut self, act: HistoryAction, ctx: &C) -> EditResult;
}

/// Selection manipulation
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SelectionAction {
    /// Duplicate selections [*n* times](Count) to adjacent lines in [MoveDir1D] direction.
    ///
    /// If the column positions are too large to fit on the adjacent lines, then the next line
    /// large enough to hold the selection is used instead.
    Duplicate(MoveDir1D, Count),

    /// Change the placement of the cursor and anchor of a visual selection.
    CursorSet(SelectionCursorChange),

    /// Change the bounds of the current selection as described by the
    /// [style](SelectionResizeStyle) and [target](EditTarget).
    ///
    /// If the context doesn't specify a selection shape, then the selection will determine its
    /// shape from the [EditTarget].
    Resize(SelectionResizeStyle, EditTarget),

    /// Split [matching selections](TargetShapeFilter) into multiple selections line.
    ///
    /// All of the new selections are of the same shape as the one they were split from.
    Split(SelectionSplitStyle, TargetShapeFilter),

    /// Remove whitespace from around [matching selections](TargetShapeFilter).
    ///
    /// Specifically, the anchor and cursor are moved so that:
    ///
    /// - For [TargetShape::CharWise], they are positioned over a non-whitespace character.
    /// - For [TargetShape::LineWise], they are not positioned on blank lines.
    /// - For [TargetShape::BlockWise], columns and rows only containing whitespace are removed.
    Trim(TargetShapeFilter),
}

/// Text insertion actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum InsertTextAction {
    /// Insert a new line [shape-wise](TargetShape) before or after the current position.
    OpenLine(TargetShape, MoveDir1D, Count),

    /// Paste before or after the current cursor position [*n*](Count) times.
    Paste(MoveDir1D, Count),

    /// Insert the contents of a [String] on [either side](MoveDir1D) of the cursor.
    Transcribe(String, MoveDir1D, Count),

    /// Type a [character](Char) on [either side](MoveDir1D) of the cursor [*n*](Count) times.
    Type(Specifier<Char>, MoveDir1D, Count),
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

    /// Rotate which cursor in the cursor group is the current leader .
    Rotate(MoveDir1D, Count),

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

/// Trait for objects which can process [CommandActions](CommandAction).
pub trait Commandable<C: Command> {
    /// Execute a command action.
    fn command(
        &mut self,
        action: CommandAction,
        ctx: &C::Context,
    ) -> UIResult<Vec<(C::Action, C::Context)>>;
}

impl<C: Command> Commandable<C> for CommandMachine<C>
where
    C::Context: EditContext,
{
    fn command(
        &mut self,
        action: CommandAction,
        ctx: &C::Context,
    ) -> UIResult<Vec<(C::Action, C::Context)>> {
        match action {
            CommandAction::Repeat(count) => {
                let count = ctx.resolve(&count);
                let mut acts = Vec::new();
                let cmd = self.get_last_command();

                for _ in 0..count {
                    let mut res = self.input_cmd(cmd.as_str(), ctx.clone())?;

                    acts.append(&mut res);
                }

                Ok(acts)
            },
            CommandAction::Execute(cmd) => {
                let acts = self.input_cmd(cmd, ctx.clone())?;

                Ok(acts)
            },
        }
    }
}

/// Command bar actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum CommandBarAction {
    /// Focus the command bar
    Focus(CommandType),

    /// Unfocus the command bar.
    Unfocus,
}

/// Prompt actions
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum PromptAction {
    /// Abort command entry.
    ///
    /// [bool] indicates whether this requires the prompt to be empty. (For example, how `<C-D>`
    /// behaves in shells.)
    Abort(bool),

    /// Submit the currently entered text.
    Submit,

    /// Move backwards and forwards through previous entries.
    Recall(MoveDir1D, Count),
}

/// A widget that the user can switch focus of keyboard input to.
pub trait Promptable<A, C> {
    /// Execute a prompt action.
    fn prompt(&mut self, act: PromptAction, ctx: &C) -> EditResult<Vec<(A, C)>>;
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

    /// Extract the currently focused window from the currently focused tab, and place it in a new
    /// tab.
    ///
    /// If there is only one window in the current tab, then this does nothing.
    ///
    /// The new tab will be placed on [MoveDir1D] side of the tab targeted by [FocusChange]. If
    /// [FocusChange] doesn't resolve to a valid tab, then the new tab is placed after the
    /// currently focused tab.
    Extract(FocusChange, MoveDir1D),

    /// Change the current focus to the tab targeted by [FocusChange].
    Focus(FocusChange),

    /// Move the currently focused tab to the position targeted by [FocusChange].
    Move(FocusChange),

    /// Open a new tab after the tab targeted by [FocusChange].
    Open(FocusChange),
}

/// Trait for objects that contain tabbed content.
pub trait TabContainer<C> {
    /// Number of currently open tabs.
    fn tabs(&self) -> usize;

    /// Execute a tab action.
    fn tab_command(&mut self, act: TabAction, ctx: &C) -> UIResult;
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

/// The result of either pressing a complete keybinding sequence, or parsing a command.
#[derive(Debug, Eq, PartialEq)]
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

    /// Navigate through the cursor positions in [the specified list](PositionList).
    Jump(PositionList, MoveDir1D, Count),

    /// Create a new [Mark] at the current cursor position.
    Mark(Specifier<Mark>),

    /// Repeat an action sequence with the current context.
    Repeat(RepeatType),

    /// Scroll the viewport in [the specified manner](ScrollStyle).
    Scroll(ScrollStyle),

    /// Modify the current selection.
    Selection(SelectionAction),

    /// Modify the current cursor group.
    Cursor(CursorAction),

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

    /// Perform a prompt-related action.
    Prompt(PromptAction),

    /// Perform a tab-related action.
    Tab(TabAction),

    /// Perform a window-related action.
    Window(WindowAction),

    /// Application-specific command.
    Application(P::Action),
}

impl<P: Application> Action<P> {
    /// Indicates how an action gets included in [RepeatType::EditSequence].
    ///
    /// `motion` indicates what to do with [EditAction::Motion].
    pub fn is_edit_sequence<C: EditContext>(
        &self,
        motion: SequenceStatus,
        ctx: &C,
    ) -> SequenceStatus {
        match self {
            Action::Repeat(_) => SequenceStatus::Ignore,

            Action::Application(act) => act.is_edit_sequence(ctx),

            Action::Command(_) => SequenceStatus::Break,
            Action::CommandBar(_) => SequenceStatus::Break,
            Action::History(_) => SequenceStatus::Break,
            Action::Jump(_, _, _) => SequenceStatus::Break,
            Action::Macro(_) => SequenceStatus::Break,
            Action::Mark(_) => SequenceStatus::Break,
            Action::Prompt(_) => SequenceStatus::Break,
            Action::Tab(_) => SequenceStatus::Break,
            Action::Window(_) => SequenceStatus::Break,

            Action::KeywordLookup => SequenceStatus::Ignore,
            Action::NoOp => SequenceStatus::Ignore,
            Action::RedrawScreen => SequenceStatus::Ignore,
            Action::Scroll(_) => SequenceStatus::Ignore,
            Action::Search(_, _) => SequenceStatus::Ignore,
            Action::Suspend => SequenceStatus::Ignore,

            Action::InsertText(_) => SequenceStatus::Track,
            Action::Cursor(_) => SequenceStatus::Track,
            Action::Selection(_) => SequenceStatus::Track,
            Action::Complete(_, _) => SequenceStatus::Track,

            Action::Edit(act, _) => {
                match ctx.resolve(act) {
                    EditAction::Motion => motion,
                    EditAction::Yank => SequenceStatus::Ignore,
                    _ => SequenceStatus::Track,
                }
            },
        }
    }

    /// Indicates how an action gets included in [RepeatType::LastAction].
    pub fn is_last_action<C: EditContext>(&self, ctx: &C) -> SequenceStatus {
        match self {
            Action::Repeat(RepeatType::EditSequence) => SequenceStatus::Atom,
            Action::Repeat(RepeatType::LastAction) => SequenceStatus::Ignore,
            Action::Repeat(RepeatType::LastSelection) => SequenceStatus::Atom,

            Action::History(HistoryAction::Checkpoint) => SequenceStatus::Ignore,
            Action::History(HistoryAction::Undo(_)) => SequenceStatus::Atom,
            Action::History(HistoryAction::Redo(_)) => SequenceStatus::Atom,

            Action::Application(act) => act.is_last_action(ctx),

            Action::Command(_) => SequenceStatus::Atom,
            Action::CommandBar(_) => SequenceStatus::Atom,
            Action::Jump(_, _, _) => SequenceStatus::Atom,
            Action::Macro(_) => SequenceStatus::Atom,
            Action::Mark(_) => SequenceStatus::Atom,
            Action::Tab(_) => SequenceStatus::Atom,
            Action::Window(_) => SequenceStatus::Atom,
            Action::KeywordLookup => SequenceStatus::Atom,
            Action::NoOp => SequenceStatus::Atom,
            Action::Prompt(_) => SequenceStatus::Atom,
            Action::RedrawScreen => SequenceStatus::Atom,
            Action::Scroll(_) => SequenceStatus::Atom,
            Action::Search(_, _) => SequenceStatus::Atom,
            Action::Suspend => SequenceStatus::Atom,
            Action::InsertText(_) => SequenceStatus::Atom,
            Action::Cursor(_) => SequenceStatus::Atom,
            Action::Selection(_) => SequenceStatus::Atom,
            Action::Complete(_, _) => SequenceStatus::Atom,
            Action::Edit(_, _) => SequenceStatus::Atom,
        }
    }

    /// Indicates how an action gets included in [RepeatType::LastSelection].
    pub fn is_last_selection<C: EditContext>(&self, ctx: &C) -> SequenceStatus {
        match self {
            Action::Repeat(_) => SequenceStatus::Ignore,

            Action::Application(act) => act.is_last_selection(ctx),

            Action::Command(_) => SequenceStatus::Ignore,
            Action::CommandBar(_) => SequenceStatus::Ignore,
            Action::History(_) => SequenceStatus::Ignore,
            Action::Jump(_, _, _) => SequenceStatus::Ignore,
            Action::Macro(_) => SequenceStatus::Ignore,
            Action::Mark(_) => SequenceStatus::Ignore,
            Action::Tab(_) => SequenceStatus::Ignore,
            Action::Window(_) => SequenceStatus::Ignore,
            Action::KeywordLookup => SequenceStatus::Ignore,
            Action::NoOp => SequenceStatus::Ignore,
            Action::Prompt(_) => SequenceStatus::Ignore,
            Action::RedrawScreen => SequenceStatus::Ignore,
            Action::Scroll(_) => SequenceStatus::Ignore,
            Action::Search(_, _) => SequenceStatus::Ignore,
            Action::Suspend => SequenceStatus::Ignore,
            Action::InsertText(_) => SequenceStatus::Ignore,
            Action::Cursor(_) => SequenceStatus::Ignore,
            Action::Complete(_, _) => SequenceStatus::Ignore,

            Action::Selection(SelectionAction::Resize(_, _)) => SequenceStatus::Track,
            Action::Selection(_) => SequenceStatus::Ignore,

            Action::Edit(act, _) => {
                if let EditAction::Motion = ctx.resolve(act) {
                    if ctx.get_target_shape().is_some() {
                        SequenceStatus::Restart
                    } else {
                        SequenceStatus::Ignore
                    }
                } else {
                    SequenceStatus::Ignore
                }
            },
        }
    }
}

impl<P: Application> Clone for Action<P> {
    fn clone(&self) -> Self {
        match self {
            Action::Application(act) => Action::Application(act.clone()),
            Action::Command(act) => Action::Command(act.clone()),
            Action::CommandBar(act) => Action::CommandBar(act.clone()),
            Action::Complete(dir, show) => Action::Complete(dir.clone(), show.clone()),
            Action::Cursor(act) => Action::Cursor(act.clone()),
            Action::Edit(action, mov) => Action::Edit(action.clone(), mov.clone()),
            Action::History(act) => Action::History(act.clone()),
            Action::InsertText(act) => Action::InsertText(act.clone()),
            Action::Jump(list, dir, count) => Action::Jump(*list, *dir, count.clone()),
            Action::KeywordLookup => Action::KeywordLookup,
            Action::Macro(act) => Action::Macro(act.clone()),
            Action::Mark(mark) => Action::Mark(mark.clone()),
            Action::NoOp => Action::NoOp,
            Action::Prompt(act) => Action::Prompt(act.clone()),
            Action::RedrawScreen => Action::RedrawScreen,
            Action::Repeat(rt) => Action::Repeat(rt.clone()),
            Action::Scroll(style) => Action::Scroll(style.clone()),
            Action::Search(dir, count) => Action::Search(dir.clone(), count.clone()),
            Action::Selection(act) => Action::Selection(act.clone()),
            Action::Suspend => Action::Suspend,
            Action::Tab(act) => Action::Tab(act.clone()),
            Action::Window(act) => Action::Window(act.clone()),
        }
    }
}

impl<P: Application> From<SelectionAction> for Action<P> {
    fn from(act: SelectionAction) -> Self {
        Action::Selection(act)
    }
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

impl<P: Application> From<PromptAction> for Action<P> {
    fn from(act: PromptAction) -> Self {
        Action::Prompt(act)
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

/// Trait for objects that can be scrolled.
pub trait Scrollable<C> {
    /// Scroll the viewable content in this object.
    fn scroll(&mut self, style: &ScrollStyle, ctx: &C) -> EditResult;
}

/// Trait for objects that can be searched.
pub trait Searchable<C> {
    /// Search for the [*n*<sup>th</sup>](Count) result in [MoveDirMod] direction.
    fn search(&mut self, dir: MoveDirMod, count: Count, ctx: &C) -> UIResult;
}

/// Additional information returned after an editing operation.
pub struct EditInfo {
    msg: String,
}

impl EditInfo {
    pub(crate) fn new(msg: &str) -> Self {
        EditInfo { msg: msg.to_string() }
    }
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

    /// Failure to determine a macro register to use.
    #[error("No macro previously executed")]
    NoMacro,

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

    /// Failure due to an unimplemented feature.
    #[error("Unimplemented: {0}")]
    Unimplemented(String),

    /// Generic failure.
    #[error("Macro error: {0}")]
    MacroFailure(#[from] MacroError),

    /// Generic failure.
    #[error("Error: {0}")]
    Failure(String),
}

/// Wrapper for various Errors that consumers may want to combine.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum UIError {
    /// Failure during Input/Output.
    #[error("Input/Output Error: {0}")]
    IOError(#[from] std::io::Error),

    /// Failure during editing.
    #[error("Editing error: {0}")]
    EditingFailure(#[from] EditError),

    /// Failure while attempting to execute a command.
    #[error("Failed command: {0}")]
    CommandFailure(#[from] CommandError),

    /// Failure when there's no currently selected tab.
    #[error("No tab currently selected")]
    NoTab,

    /// Failure when there's no currently selected window.
    #[error("No window currently selected")]
    NoWindow,
}

/// Common result type for editing operations.
pub type EditResult<V = Option<EditInfo>> = Result<V, EditError>;

/// Common result type for rendering and application functions.
pub type UIResult<V = Option<EditInfo>> = Result<V, UIError>;
