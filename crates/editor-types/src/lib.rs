//! # Editor Types
//!
//! ## Overview
//!
//! The types in this crate provides a defunctionalized view of a text editor. Consumers of these
//! types should map them into text manipulation or user interface actions.
//!
//! ## Examples
//!
//! ```
//! use editor_types::{Action, EditAction, EditorAction};
//! use editor_types::prelude::*;
//!
//! // Delete the current text selection.
//! let _: Action = EditorAction::Edit(EditAction::Delete.into(), EditTarget::Selection).into();
//!
//! // Copy the next three lines.
//! let _: Action = EditorAction::Edit(EditAction::Yank.into(), EditTarget::Range(RangeType::Line, true, 3.into())).into();
//!
//! // Make some contextually specified number of words lowercase.
//! let _: Action = EditorAction::Edit(
//!     EditAction::ChangeCase(Case::Lower).into(),
//!     EditTarget::Motion(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next), Count::Contextual)
//! ).into();
//!
//! // Scroll the viewport so that line 10 is at the top of the screen.
//! let _: Action = Action::Scroll(ScrollStyle::LinePos(MovePosition::Beginning, 10.into()));
//! ```
pub mod application;
pub mod context;
pub mod prelude;
pub mod util;

use self::application::*;
use self::context::{EditContext, Resolve};
use self::prelude::*;
use keybindings::SequenceStatus;

/// The various actions that can be taken on text.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum EditAction {
    /// Move the cursor.
    ///
    /// If a shape is [specified contextually](EditContext::get_target_shape), then visually select
    /// text while moving, as if using [SelectionAction::Resize] with
    /// [SelectionResizeStyle::Extend].
    #[default]
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

    /// Change the first number on each line within the targeted text.
    ///
    /// The [bool] argument controls whether to increment by an additional count on each line.
    ChangeNumber(NumberChange, bool),

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
    /// Returns true if this [EditAction] doesn't modify a buffer's text.
    pub fn is_readonly(&self) -> bool {
        match self {
            EditAction::Motion => true,
            EditAction::Yank => true,

            EditAction::ChangeCase(_) => false,
            EditAction::ChangeNumber(_, _) => false,
            EditAction::Delete => false,
            EditAction::Format => false,
            EditAction::Indent(_) => false,
            EditAction::Join(_) => false,
            EditAction::Replace(_) => false,
        }
    }

    /// Returns true if the value is [EditAction::Motion].
    pub fn is_motion(&self) -> bool {
        matches!(self, EditAction::Motion)
    }

    /// Returns true if this [EditAction] is allowed to trigger a [WindowAction::Switch] after an
    /// error.
    pub fn is_switchable(&self, _: &EditContext) -> bool {
        self.is_motion()
    }
}

/// Actions for manipulating text selections.
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

    /// Expand a selection by repositioning its cursor and anchor such that they are placed on the
    /// specified boundary.
    ///
    /// Be aware that since this repositions the start and end of the selection, this may not do
    /// what you want with [TargetShape::BlockWise] selections.
    Expand(SelectionBoundary, TargetShapeFilter),

    /// Filter selections using the last regular expression entered for [CommandType::Search].
    Filter(MatchAction),

    /// Join adjacent selections together.
    Join,

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

    /// Shrink a selection by repositioning its cursor and anchor such that they are placed on the
    /// specified boundary.
    ///
    /// Be aware that since this repositions the start and end of the selection, this may not do
    /// what you want with [TargetShape::BlockWise] selections.
    Trim(SelectionBoundary, TargetShapeFilter),
}

/// Actions for inserting text into a buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum InsertTextAction {
    /// Insert a new line [shape-wise](TargetShape) before or after the current position.
    OpenLine(TargetShape, MoveDir1D, Count),

    /// Paste before or after the current cursor position [*n*](Count) times.
    Paste(PasteStyle, Count),

    /// Insert the contents of a [String] on [either side](MoveDir1D) of the cursor.
    Transcribe(String, MoveDir1D, Count),

    /// Type a [character](Char) on [either side](MoveDir1D) of the cursor [*n*](Count) times.
    Type(Specifier<Char>, MoveDir1D, Count),
}

/// Actions for manipulating a buffer's history.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HistoryAction {
    /// Create a new editing history checkpoint.
    Checkpoint,

    /// Redo [*n*](Count) edits.
    Redo(Count),

    /// Undo [*n*](Count) edits.
    Undo(Count),
}

impl HistoryAction {
    /// Returns true if this [HistoryAction] doesn't modify a buffer's text.
    pub fn is_readonly(&self) -> bool {
        match self {
            HistoryAction::Redo(_) => false,
            HistoryAction::Undo(_) => false,
            HistoryAction::Checkpoint => true,
        }
    }
}

/// Actions for manipulating cursor groups.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum CursorAction {
    /// Close the [targeted cursors](CursorCloseTarget) in the current cursor group.
    Close(CursorCloseTarget),

    /// Restore a saved cursor group.
    ///
    /// If a combining style is specified, then the saved group will be merged with the current one
    /// as specified.
    Restore(CursorGroupCombineStyle),

    /// Rotate which cursor in the cursor group is the current leader .
    Rotate(MoveDir1D, Count),

    /// Save the current cursor group.
    ///
    /// If a combining style is specified, then the current group will be merged with any
    /// previously saved group as specified.
    Save(CursorGroupCombineStyle),

    /// Split each cursor in the cursor group [*n*](Count) times.
    Split(Count),
}

impl CursorAction {
    /// Returns true if this [CursorAction] is allowed to trigger a [WindowAction::Switch] after an
    /// error.
    pub fn is_switchable(&self, _: &EditContext) -> bool {
        match self {
            CursorAction::Restore(_) => true,

            CursorAction::Close(_) => false,
            CursorAction::Rotate(..) => false,
            CursorAction::Save(_) => false,
            CursorAction::Split(_) => false,
        }
    }
}

/// Actions for running application commands (e.g. `:w` or `:quit`).
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum CommandAction {
    /// Run a command string.
    ///
    /// This should update [Register::LastCommand].
    Run(String),

    /// Execute the last [CommandType::Command] entry [*n* times](Count).
    Execute(Count),
}

/// Actions for manipulating the application's command bar.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommandBarAction<I: ApplicationInfo> {
    /// Focus the command bar
    Focus(String, CommandType, Box<Action<I>>),

    /// Unfocus the command bar.
    Unfocus,
}

/// Actions for manipulating prompts.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PromptAction {
    /// Abort command entry.
    ///
    /// [bool] indicates whether this requires the prompt to be empty. (For example, how `<C-D>`
    /// behaves in shells.)
    Abort(bool),

    /// Submit the currently entered text.
    Submit,

    /// Move backwards and forwards through previous entries.
    ///
    /// If [bool] is `true`, then this should only move through entries that share an initially
    /// typed prefix.
    Recall(MoveDir1D, Count, bool),
}

/// Actions for recording and running macros.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum MacroAction {
    /// Execute the contents of the contextually specified Register [*n* times](Count).
    ///
    /// If no register is specified, then this should default to [Register::UnnamedMacro].
    Execute(Count),

    /// Run the given macro string [*n* times](Count).
    Run(String, Count),

    /// Execute the contents of the previously specified register [*n* times](Count).
    Repeat(Count),

    /// Start or stop recording a macro.
    ToggleRecording,
}

/// Actions for manipulating application tabs.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum TabAction<I: ApplicationInfo> {
    /// Close the [TabTarget] tabs with [CloseFlags] options.
    Close(TabTarget, CloseFlags),

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

    /// Open a new tab after the tab targeted by [FocusChange] that displays the requested content.
    Open(OpenTarget<I::WindowId>, FocusChange),
}

/// Actions for manipulating application windows.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum WindowAction<I: ApplicationInfo> {
    /// Close the [WindowTarget] windows with [CloseFlags] options.
    Close(WindowTarget, CloseFlags),

    /// Exchange the currently focused window with the window targeted by [FocusChange].
    Exchange(FocusChange),

    /// Change the current focus to the window targeted by [FocusChange].
    Focus(FocusChange),

    /// Move the currently focused window to the [MoveDir2D] side of the screen.
    MoveSide(MoveDir2D),

    /// Open a new window that is [*n*](Count) columns along [an axis](Axis), positioned relative to
    /// the current window as indicated by [MoveDir1D].
    Open(OpenTarget<I::WindowId>, Axis, MoveDir1D, Count),

    /// Visually rotate the windows in [MoveDir2D] direction.
    Rotate(MoveDir1D),

    /// Split the currently focused window [*n* times](Count) along [an axis](Axis), moving
    /// the focus in [MoveDir1D] direction after performing the split.
    Split(OpenTarget<I::WindowId>, Axis, MoveDir1D, Count),

    /// Switch what content the window is currently showing.
    ///
    /// If there are no currently open windows in the tab, then this behaves like
    /// [WindowAction::Open].
    Switch(OpenTarget<I::WindowId>),

    /// Clear all of the explicitly set window sizes, and instead try to equally distribute
    /// available rows and columns.
    ClearSizes,

    /// Resize the window targeted by [FocusChange] according to [SizeChange].
    Resize(FocusChange, Axis, SizeChange),

    /// Write the contents of the windows targeted by [WindowTarget].
    Write(WindowTarget, Option<String>, WriteFlags),

    /// Zoom in on the currently focused window so that it takes up the whole screen. If there is
    /// already a zoomed-in window, then return to showing all windows.
    ZoomToggle,
}

/// Actions for editing text within buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum EditorAction {
    /// Complete the text before the cursor group leader.
    Complete(CompletionType, CompletionSelection, CompletionDisplay),

    /// Modify the current cursor group.
    Cursor(CursorAction),

    /// Perform the specified [action](EditAction) on [a target](EditTarget).
    Edit(Specifier<EditAction>, EditTarget),

    /// Perform a history operation.
    History(HistoryAction),

    /// Insert text.
    InsertText(InsertTextAction),

    /// Create a new [Mark] at the current leader position.
    Mark(Specifier<Mark>),

    /// Modify the current selection.
    Selection(SelectionAction),
}

impl EditorAction {
    /// Indicates if this is a read-only action.
    pub fn is_readonly(&self, ctx: &EditContext) -> bool {
        match self {
            EditorAction::Complete(_, _, _) => false,
            EditorAction::History(act) => act.is_readonly(),
            EditorAction::InsertText(_) => false,

            EditorAction::Cursor(_) => true,
            EditorAction::Mark(_) => true,
            EditorAction::Selection(_) => true,

            EditorAction::Edit(act, _) => ctx.resolve(act).is_readonly(),
        }
    }

    /// Indicates how an action gets included in [RepeatType::EditSequence].
    ///
    /// `motion` indicates what to do with [EditAction::Motion].
    pub fn is_edit_sequence(&self, motion: SequenceStatus, ctx: &EditContext) -> SequenceStatus {
        match self {
            EditorAction::History(_) => SequenceStatus::Break,
            EditorAction::Mark(_) => SequenceStatus::Break,
            EditorAction::InsertText(_) => SequenceStatus::Track,
            EditorAction::Cursor(_) => SequenceStatus::Track,
            EditorAction::Selection(_) => SequenceStatus::Track,
            EditorAction::Complete(_, _, _) => SequenceStatus::Track,
            EditorAction::Edit(act, _) => {
                match ctx.resolve(act) {
                    EditAction::Motion => motion,
                    EditAction::Yank => SequenceStatus::Ignore,
                    _ => SequenceStatus::Track,
                }
            },
        }
    }

    /// Indicates how an action gets included in [RepeatType::LastAction].
    pub fn is_last_action(&self, _: &EditContext) -> SequenceStatus {
        match self {
            EditorAction::History(HistoryAction::Checkpoint) => SequenceStatus::Ignore,
            EditorAction::History(HistoryAction::Undo(_)) => SequenceStatus::Atom,
            EditorAction::History(HistoryAction::Redo(_)) => SequenceStatus::Atom,

            EditorAction::Complete(_, _, _) => SequenceStatus::Atom,
            EditorAction::Cursor(_) => SequenceStatus::Atom,
            EditorAction::Edit(_, _) => SequenceStatus::Atom,
            EditorAction::InsertText(_) => SequenceStatus::Atom,
            EditorAction::Mark(_) => SequenceStatus::Atom,
            EditorAction::Selection(_) => SequenceStatus::Atom,
        }
    }

    /// Indicates how an action gets included in [RepeatType::LastSelection].
    pub fn is_last_selection(&self, ctx: &EditContext) -> SequenceStatus {
        match self {
            EditorAction::History(_) => SequenceStatus::Ignore,
            EditorAction::Mark(_) => SequenceStatus::Ignore,
            EditorAction::InsertText(_) => SequenceStatus::Ignore,
            EditorAction::Cursor(_) => SequenceStatus::Ignore,
            EditorAction::Complete(_, _, _) => SequenceStatus::Ignore,

            EditorAction::Selection(SelectionAction::Resize(_, _)) => SequenceStatus::Track,
            EditorAction::Selection(_) => SequenceStatus::Ignore,

            EditorAction::Edit(act, _) => {
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

    /// Returns true if this [Action] is allowed to trigger a [WindowAction::Switch] after an error.
    pub fn is_switchable(&self, ctx: &EditContext) -> bool {
        match self {
            EditorAction::Cursor(act) => act.is_switchable(ctx),
            EditorAction::Edit(act, _) => ctx.resolve(act).is_switchable(ctx),
            EditorAction::Complete(_, _, _) => false,
            EditorAction::History(_) => false,
            EditorAction::InsertText(_) => false,
            EditorAction::Mark(_) => false,
            EditorAction::Selection(_) => false,
        }
    }
}

impl From<CursorAction> for EditorAction {
    fn from(act: CursorAction) -> Self {
        EditorAction::Cursor(act)
    }
}

impl From<HistoryAction> for EditorAction {
    fn from(act: HistoryAction) -> Self {
        EditorAction::History(act)
    }
}

impl From<InsertTextAction> for EditorAction {
    fn from(act: InsertTextAction) -> Self {
        EditorAction::InsertText(act)
    }
}

impl From<SelectionAction> for EditorAction {
    fn from(act: SelectionAction) -> Self {
        EditorAction::Selection(act)
    }
}

/// The result of either pressing a complete keybinding sequence, or parsing a command.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Action<I: ApplicationInfo = EmptyInfo> {
    /// Do nothing.
    NoOp,

    /// Perform an editor action.
    Editor(EditorAction),

    /// Perform a macro-related action.
    Macro(MacroAction),

    /// Navigate through the cursor positions in [the specified list](PositionList).
    ///
    /// If the current window cannot satisfy the given [Count], then this may jump to other
    /// windows.
    Jump(PositionList, MoveDir1D, Count),

    /// Repeat an action sequence with the current context.
    Repeat(RepeatType),

    /// Scroll the viewport in [the specified manner](ScrollStyle).
    Scroll(ScrollStyle),

    /// Lookup the keyword under the cursor.
    KeywordLookup,

    /// Redraw the screen.
    RedrawScreen,

    /// Show an [InfoMessage].
    ShowInfoMessage(InfoMessage),

    /// Suspend the process.
    Suspend,

    /// Find the [*n*<sup>th</sup>](Count) occurrence of the current application-level search.
    Search(MoveDirMod, Count),

    /// Perform a command-related action.
    Command(CommandAction),

    /// Perform a command bar-related action.
    CommandBar(CommandBarAction<I>),

    /// Perform a prompt-related action.
    Prompt(PromptAction),

    /// Perform a tab-related action.
    Tab(TabAction<I>),

    /// Perform a window-related action.
    Window(WindowAction<I>),

    /// Application-specific command.
    Application(I::Action),
}

impl<I: ApplicationInfo> Action<I> {
    /// Indicates how an action gets included in [RepeatType::EditSequence].
    ///
    /// `motion` indicates what to do with [EditAction::Motion].
    pub fn is_edit_sequence(&self, motion: SequenceStatus, ctx: &EditContext) -> SequenceStatus {
        match self {
            Action::Repeat(_) => SequenceStatus::Ignore,

            Action::Application(act) => act.is_edit_sequence(ctx),
            Action::Editor(act) => act.is_edit_sequence(motion, ctx),

            Action::Command(_) => SequenceStatus::Break,
            Action::CommandBar(_) => SequenceStatus::Break,
            Action::Jump(_, _, _) => SequenceStatus::Break,
            Action::Macro(_) => SequenceStatus::Break,
            Action::Prompt(_) => SequenceStatus::Break,
            Action::Tab(_) => SequenceStatus::Break,
            Action::Window(_) => SequenceStatus::Break,

            Action::KeywordLookup => SequenceStatus::Ignore,
            Action::NoOp => SequenceStatus::Ignore,
            Action::RedrawScreen => SequenceStatus::Ignore,
            Action::Scroll(_) => SequenceStatus::Ignore,
            Action::Search(_, _) => SequenceStatus::Ignore,
            Action::ShowInfoMessage(_) => SequenceStatus::Ignore,
            Action::Suspend => SequenceStatus::Ignore,
        }
    }

    /// Indicates how an action gets included in [RepeatType::LastAction].
    pub fn is_last_action(&self, ctx: &EditContext) -> SequenceStatus {
        match self {
            Action::Repeat(RepeatType::EditSequence) => SequenceStatus::Atom,
            Action::Repeat(RepeatType::LastAction) => SequenceStatus::Ignore,
            Action::Repeat(RepeatType::LastSelection) => SequenceStatus::Atom,

            Action::Application(act) => act.is_last_action(ctx),
            Action::Editor(act) => act.is_last_action(ctx),

            Action::Command(_) => SequenceStatus::Atom,
            Action::CommandBar(_) => SequenceStatus::Atom,
            Action::Jump(_, _, _) => SequenceStatus::Atom,
            Action::Macro(_) => SequenceStatus::Atom,
            Action::Tab(_) => SequenceStatus::Atom,
            Action::Window(_) => SequenceStatus::Atom,
            Action::KeywordLookup => SequenceStatus::Atom,
            Action::NoOp => SequenceStatus::Atom,
            Action::Prompt(_) => SequenceStatus::Atom,
            Action::RedrawScreen => SequenceStatus::Atom,
            Action::Scroll(_) => SequenceStatus::Atom,
            Action::Search(_, _) => SequenceStatus::Atom,
            Action::ShowInfoMessage(_) => SequenceStatus::Atom,
            Action::Suspend => SequenceStatus::Atom,
        }
    }

    /// Indicates how an action gets included in [RepeatType::LastSelection].
    pub fn is_last_selection(&self, ctx: &EditContext) -> SequenceStatus {
        match self {
            Action::Repeat(_) => SequenceStatus::Ignore,

            Action::Application(act) => act.is_last_selection(ctx),
            Action::Editor(act) => act.is_last_selection(ctx),

            Action::Command(_) => SequenceStatus::Ignore,
            Action::CommandBar(_) => SequenceStatus::Ignore,
            Action::Jump(_, _, _) => SequenceStatus::Ignore,
            Action::Macro(_) => SequenceStatus::Ignore,
            Action::Tab(_) => SequenceStatus::Ignore,
            Action::Window(_) => SequenceStatus::Ignore,
            Action::KeywordLookup => SequenceStatus::Ignore,
            Action::NoOp => SequenceStatus::Ignore,
            Action::Prompt(_) => SequenceStatus::Ignore,
            Action::RedrawScreen => SequenceStatus::Ignore,
            Action::Scroll(_) => SequenceStatus::Ignore,
            Action::Search(_, _) => SequenceStatus::Ignore,
            Action::ShowInfoMessage(_) => SequenceStatus::Ignore,
            Action::Suspend => SequenceStatus::Ignore,
        }
    }

    /// Returns true if this [Action] is allowed to trigger a [WindowAction::Switch] after an error.
    pub fn is_switchable(&self, ctx: &EditContext) -> bool {
        match self {
            Action::Application(act) => act.is_switchable(ctx),
            Action::Editor(act) => act.is_switchable(ctx),
            Action::Jump(..) => true,

            Action::CommandBar(_) => false,
            Action::Command(_) => false,
            Action::KeywordLookup => false,
            Action::Macro(_) => false,
            Action::NoOp => false,
            Action::Prompt(_) => false,
            Action::RedrawScreen => false,
            Action::Repeat(_) => false,
            Action::Scroll(_) => false,
            Action::Search(_, _) => false,
            Action::ShowInfoMessage(_) => false,
            Action::Suspend => false,
            Action::Tab(_) => false,
            Action::Window(_) => false,
        }
    }
}

#[allow(clippy::derivable_impls)]
impl<I: ApplicationInfo> Default for Action<I> {
    fn default() -> Self {
        Action::NoOp
    }
}

impl<I: ApplicationInfo> From<SelectionAction> for Action<I> {
    fn from(act: SelectionAction) -> Self {
        Action::Editor(EditorAction::Selection(act))
    }
}

impl<I: ApplicationInfo> From<InsertTextAction> for Action<I> {
    fn from(act: InsertTextAction) -> Self {
        Action::Editor(EditorAction::InsertText(act))
    }
}

impl<I: ApplicationInfo> From<HistoryAction> for Action<I> {
    fn from(act: HistoryAction) -> Self {
        Action::Editor(EditorAction::History(act))
    }
}

impl<I: ApplicationInfo> From<CursorAction> for Action<I> {
    fn from(act: CursorAction) -> Self {
        Action::Editor(EditorAction::Cursor(act))
    }
}

impl<I: ApplicationInfo> From<EditorAction> for Action<I> {
    fn from(act: EditorAction) -> Self {
        Action::Editor(act)
    }
}

impl<I: ApplicationInfo> From<MacroAction> for Action<I> {
    fn from(act: MacroAction) -> Self {
        Action::Macro(act)
    }
}

impl<I: ApplicationInfo> From<CommandAction> for Action<I> {
    fn from(act: CommandAction) -> Self {
        Action::Command(act)
    }
}

impl<I: ApplicationInfo> From<CommandBarAction<I>> for Action<I> {
    fn from(act: CommandBarAction<I>) -> Self {
        Action::CommandBar(act)
    }
}

impl<I: ApplicationInfo> From<PromptAction> for Action<I> {
    fn from(act: PromptAction) -> Self {
        Action::Prompt(act)
    }
}

impl<I: ApplicationInfo> From<WindowAction<I>> for Action<I> {
    fn from(act: WindowAction<I>) -> Self {
        Action::Window(act)
    }
}

impl<I: ApplicationInfo> From<TabAction<I>> for Action<I> {
    fn from(act: TabAction<I>) -> Self {
        Action::Tab(act)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_readonly() {
        let mut ctx = EditContext::default();

        let act = SelectionAction::Duplicate(MoveDir1D::Next, Count::Contextual);
        assert_eq!(EditorAction::from(act).is_readonly(&ctx), true);

        let act = HistoryAction::Checkpoint;
        assert_eq!(EditorAction::from(act).is_readonly(&ctx), true);

        let act = HistoryAction::Undo(Count::Contextual);
        assert_eq!(EditorAction::from(act).is_readonly(&ctx), false);

        let act = EditorAction::Edit(Specifier::Contextual, EditTarget::CurrentPosition);
        ctx.operation = EditAction::Motion;
        assert_eq!(act.is_readonly(&ctx), true);

        let act = EditorAction::Edit(Specifier::Contextual, EditTarget::CurrentPosition);
        ctx.operation = EditAction::Delete;
        assert_eq!(act.is_readonly(&ctx), false);
    }
}
