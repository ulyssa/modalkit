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

/// A macro that turns a shorthand command DSL into an [Action].
pub use editor_types_macros::action;

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
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let count = Count::Contextual;
    /// let act: Action = SelectionAction::Duplicate(MoveDir1D::Next, count.clone()).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(action!("selection duplicate -d next"), act);
    /// assert_eq!(action!("selection duplicate -d next -c ctx"), act);
    /// assert_eq!(action!("selection duplicate -d next -c {count}"), act);
    /// ```
    Duplicate(MoveDir1D, Count),

    /// Change the placement of the cursor and anchor of a visual selection.
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
    CursorSet(SelectionCursorChange),

    /// Expand a selection by repositioning its cursor and anchor such that they are placed on the
    /// specified boundary.
    ///
    /// Be aware that since this repositions the start and end of the selection, this may not do
    /// what you want with [TargetShape::BlockWise] selections.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionBoundary::Line;
    /// let split: Action = action!("selection expand -b line -t all");
    /// assert_eq!(split, SelectionAction::Expand(style, TargetShapeFilter::ALL).into());
    /// ```
    Expand(SelectionBoundary, TargetShapeFilter),

    /// Filter selections using the last regular expression entered for [CommandType::Search].
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
    Filter(MatchAction),

    /// Join adjacent selections together.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let act: Action = SelectionAction::Join.into();
    /// assert_eq!(act, action!("selection join"));
    /// ```
    Join,

    /// Change the bounds of the current selection as described by the
    /// [style](SelectionResizeStyle) and [target](EditTarget).
    ///
    /// If the context doesn't specify a selection shape, then the selection will determine its
    /// shape from the [EditTarget].
    ///
    /// See the documentation for the [SelectionResizeStyle] variants for how to construct all of the
    /// possible values using [action].
    Resize(SelectionResizeStyle, EditTarget),

    /// Split [matching selections](TargetShapeFilter) into multiple selections line.
    ///
    /// All of the new selections are of the same shape as the one they were split from.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionSplitStyle::Lines;
    /// let split: Action = action!("selection split -s lines -F all");
    /// assert_eq!(split, SelectionAction::Split(style, TargetShapeFilter::ALL).into());
    /// ```
    Split(SelectionSplitStyle, TargetShapeFilter),

    /// Shrink a selection by repositioning its cursor and anchor such that they are placed on the
    /// specified boundary.
    ///
    /// Be aware that since this repositions the start and end of the selection, this may not do
    /// what you want with [TargetShape::BlockWise] selections.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let style = SelectionBoundary::Line;
    /// let split: Action = action!("selection trim -b line -t all");
    /// assert_eq!(split, SelectionAction::Trim(style, TargetShapeFilter::ALL).into());
    /// ```
    Trim(SelectionBoundary, TargetShapeFilter),
}

/// Actions for inserting text into a buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum InsertTextAction {
    /// Insert a new line [shape-wise](TargetShape) before or after the current position.
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
    /// assert_eq!(act, action!("insert open-line -S line -d next -c ctx"));
    /// assert_eq!(act, action!("insert open-line -S line -d next"));
    /// ```
    OpenLine(TargetShape, MoveDir1D, Count),

    /// Paste before or after the current cursor position [*n*](Count) times.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let count = 5;
    /// let paste: Action = action!("insert paste -s (side -d next) -c {count}");
    /// assert_eq!(paste, InsertTextAction::Paste(PasteStyle::Side(MoveDir1D::Next), Count::Exact(5)).into());
    /// ```
    Paste(PasteStyle, Count),

    /// Insert the contents of a [String] on [either side](MoveDir1D) of the cursor.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let input: Action = action!(r#"insert transcribe -i "hello" -d next -c 1"#);
    /// assert_eq!(input, InsertTextAction::Transcribe("hello".into(), MoveDir1D::Next, 1.into()).into());
    /// ```
    Transcribe(String, MoveDir1D, Count),

    /// Type a [character](Char) on [either side](MoveDir1D) of the cursor [*n*](Count) times.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let c = Specifier::Exact(Char::from('a'));
    /// let dir = MoveDir1D::Previous;
    /// let count = Count::Contextual;
    /// let act: Action = InsertTextAction::Type(c.clone(), dir, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("insert type -i (exact \'a\') -d previous -c ctx"));
    /// assert_eq!(act, action!("insert type -i (exact \'a\') -c ctx"));
    /// assert_eq!(act, action!("insert type -i (exact \'a\') -d previous"));
    /// assert_eq!(act, action!("insert type -i (exact \'a\')"));
    /// assert_eq!(act, action!("insert type -i {c}"));
    /// ```
    Type(Specifier<Char>, MoveDir1D, Count),
}

/// Actions for manipulating a buffer's history.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HistoryAction {
    /// Create a new editing history checkpoint.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    ///
    /// let check: Action = action!("history checkpoint");
    /// assert_eq!(check, HistoryAction::Checkpoint.into());
    /// ```
    Checkpoint,

    /// Redo [*n*](Count) edits.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    ///
    /// let redo: Action = action!("history redo");
    /// assert_eq!(redo, HistoryAction::Redo(Count::Contextual).into());
    ///
    /// let redo: Action = action!("history redo -c 1");
    /// assert_eq!(redo, HistoryAction::Redo(Count::Exact(1)).into());
    /// ```
    Redo(Count),

    /// Undo [*n*](Count) edits.
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    ///
    /// let undo: Action = action!("history undo");
    /// assert_eq!(undo, HistoryAction::Undo(Count::Contextual).into());
    ///
    /// let undo: Action = action!("history undo -c 1");
    /// assert_eq!(undo, HistoryAction::Undo(Count::Exact(1)).into());
    /// ```
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
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let close: Action = action!("cursor close -t leader");
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Leader).into());
    ///
    /// let close: Action = action!("cursor close -t followers");
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Followers).into());
    /// ```
    Close(CursorCloseTarget),

    /// Restore a saved cursor group.
    ///
    /// If a combining style is specified, then the saved group will be merged with the current one
    /// as specified.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let restore: Action = action!("cursor restore -s append");
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Append).into());
    ///
    /// let restore: Action = action!("cursor restore -s replace");
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Replace).into());
    ///
    /// let restore: Action = action!("cursor restore -s (merge select-cursor -d prev)");
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Merge(CursorMergeStyle::SelectCursor(MoveDir1D::Previous))).into());
    /// ```
    ///
    /// See the documentation for [CursorGroupCombineStyle] for how to construct each of its
    /// variants with [action].
    Restore(CursorGroupCombineStyle),

    /// Rotate which cursor in the cursor group is the current leader .
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let rotate: Action = action!("cursor rotate -d prev");
    /// assert_eq!(rotate, CursorAction::Rotate(MoveDir1D::Previous, Count::Contextual).into());
    ///
    /// let rotate: Action = action!("cursor rotate -d next -c 2");
    /// assert_eq!(rotate, CursorAction::Rotate(MoveDir1D::Next, Count::Exact(2)).into());
    /// ```
    Rotate(MoveDir1D, Count),

    /// Save the current cursor group.
    ///
    /// If a combining style is specified, then the current group will be merged with any
    /// previously saved group as specified.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let save: Action = action!("cursor save -s append");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Append).into());
    ///
    /// let save: Action = action!("cursor save -s replace");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Replace).into());
    ///
    /// let save: Action = action!("cursor save -s (merge union)");
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Merge(CursorMergeStyle::Union)).into());
    /// ```
    ///
    /// See the documentation for [CursorGroupCombineStyle] for how to construct each of its
    /// variants with [action].
    Save(CursorGroupCombineStyle),

    /// Split each cursor in the cursor group [*n*](Count) times.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let split: Action = action!("cursor split -c {}", Count::Contextual);
    /// assert_eq!(split, CursorAction::Split(Count::Contextual).into());
    ///
    /// let split: Action = action!("cursor split -c {}", 5);
    /// assert_eq!(split, CursorAction::Split(Count::Exact(5)).into());
    /// ```
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
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandAction};
    ///
    /// let quitall: Action = action!(r#"command run -i "quitall" "#);
    /// assert_eq!(quitall, CommandAction::Run("quitall".into()).into());
    /// ```
    Run(String),

    /// Execute the last [CommandType::Command] entry [*n* times](Count).
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandAction};
    ///
    /// let exec: Action = action!("command execute");
    /// assert_eq!(exec, CommandAction::Execute(Count::Contextual).into());
    ///
    /// let exec5: Action = action!("command execute -c 5");
    /// assert_eq!(exec5, CommandAction::Execute(5.into()).into());
    /// ```
    Execute(Count),
}

/// Actions for manipulating the application's command bar.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommandBarAction<I: ApplicationInfo> {
    /// Focus the command bar
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let focus: Action = action!(r#"cmdbar focus -p "/" -s search -a (search -d same)"#);
    /// assert_eq!(focus, CommandBarAction::Focus(
    ///     "/".into(),
    ///     CommandType::Search,
    ///     Action::Search(MoveDirMod::Same, Count::Contextual).into(),
    /// ).into());
    /// ```
    Focus(String, CommandType, Box<Action<I>>),

    /// Unfocus the command bar.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let unfocus: Action = action!("cmdbar unfocus");
    /// assert_eq!(unfocus, CommandBarAction::Unfocus.into());
    /// ```
    Unfocus,
}

/// Actions for manipulating prompts.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PromptAction {
    /// Abort command entry.
    ///
    /// [bool] indicates whether this requires the prompt to be empty. (For example, how `<C-D>`
    /// behaves in shells.)
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    ///
    /// let act: Action = action!("prompt abort");
    /// let exp: Action = PromptAction::Abort(false).into();
    /// assert_eq!(act, exp);
    /// ```
    Abort(bool),

    /// Submit the currently entered text.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    ///
    /// let act: Action = action!("prompt submit");
    /// let exp: Action = PromptAction::Submit.into();
    /// assert_eq!(act, exp);
    /// ```
    Submit,

    /// Move backwards and forwards through previous entries.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    ///
    /// let filter = RecallFilter::All;
    /// let act: Action = PromptAction::Recall(filter.clone(), MoveDir1D::Next, Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("prompt recall -d next -c ctx -F all"));
    /// assert_eq!(act, action!("prompt recall -d next -c ctx -F {filter}"));
    /// assert_eq!(act, action!("prompt recall -d next -c ctx"));
    /// assert_eq!(act, action!("prompt recall -d next"));
    /// ```
    Recall(RecallFilter, MoveDir1D, Count),
}

/// Actions for recording and running macros.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum MacroAction {
    /// Execute the contents of the contextually specified Register [*n* times](Count).
    ///
    /// If no register is specified, then this should default to [Register::UnnamedMacro].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    ///
    /// let act: Action = MacroAction::Execute(Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("macro execute -c ctx"));
    /// assert_eq!(act, action!("macro execute"));
    /// assert_eq!(act, action!("macro exec"));
    /// ```
    Execute(Count),

    /// Run the given macro string [*n* times](Count).
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    ///
    /// let mac = "hjkl".to_string();
    /// let act: Action = MacroAction::Run(mac, Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("macro run -i \"hjkl\" -c ctx"));
    /// assert_eq!(act, action!("macro run -i \"hjkl\""));
    /// ```
    Run(String, Count),

    /// Execute the contents of the previously specified register [*n* times](Count).
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    ///
    /// let act: Action = MacroAction::Repeat(Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("macro repeat -c ctx"));
    /// assert_eq!(act, action!("macro repeat"));
    /// ```
    Repeat(Count),

    /// Start or stop recording a macro.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    ///
    /// let act: Action = MacroAction::ToggleRecording.into();
    /// assert_eq!(act, action!("macro toggle-recording"));
    /// ```
    ToggleRecording,
}

/// Actions for manipulating application tabs.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum TabAction<I: ApplicationInfo> {
    /// Close the [TabTarget] tabs with [CloseFlags] options.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let fc = TabTarget::Single(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let extract: Action = TabAction::Close(fc, flags).into();
    /// assert_eq!(extract, action!("tab close -t (single current) -F none"));
    /// ```
    Close(TabTarget, CloseFlags),

    /// Extract the currently focused window from the currently focused tab, and place it in a new
    /// tab.
    ///
    /// If there is only one window in the current tab, then this does nothing.
    ///
    /// The new tab will be placed on [MoveDir1D] side of the tab targeted by [FocusChange]. If
    /// [FocusChange] doesn't resolve to a valid tab, then the new tab is placed after the
    /// currently focused tab.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let extract: Action = TabAction::Extract(FocusChange::Current, MoveDir1D::Next).into();
    /// assert_eq!(extract, action!("tab extract -f current -d next"));
    /// ```
    ///
    /// See the documentation for [FocusChange] for how to construct each of its variants with
    /// [action].
    Extract(FocusChange, MoveDir1D),

    /// Change the current focus to the tab targeted by [FocusChange].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let extract: Action = TabAction::Focus(FocusChange::PreviouslyFocused).into();
    /// assert_eq!(extract, action!("tab focus -f previously-focused"));
    /// ```
    ///
    /// See the documentation for [FocusChange] for how to construct each of its variants with
    /// [action].
    Focus(FocusChange),

    /// Move the currently focused tab to the position targeted by [FocusChange].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let extract: Action = TabAction::Move(FocusChange::PreviouslyFocused).into();
    /// assert_eq!(extract, action!("tab move -f previously-focused"));
    /// ```
    ///
    /// See the documentation for [FocusChange] for how to construct each of its variants with
    /// [action].
    Move(FocusChange),

    /// Open a new tab after the tab targeted by [FocusChange] that displays the requested content.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let extract: Action = TabAction::Open(OpenTarget::Current, FocusChange::PreviouslyFocused).into();
    /// assert_eq!(extract, action!("tab open -t current -f previously-focused"));
    /// ```
    ///
    /// See the documentation for [OpenTarget] and [FocusChange] for how to construct each of their
    /// variants with [action].
    Open(OpenTarget<I::WindowId>, FocusChange),
}

/// Actions for manipulating application windows.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum WindowAction<I: ApplicationInfo> {
    /// Close the [WindowTarget] windows with [CloseFlags] options.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let fc = WindowTarget::Single(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let extract: Action = WindowAction::Close(fc, flags).into();
    /// assert_eq!(extract, action!("window close -t (single current) -F none"));
    /// ```
    Close(WindowTarget, CloseFlags),

    /// Exchange the currently focused window with the window targeted by [FocusChange].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let fc = FocusChange::PreviouslyFocused;
    /// let act: Action = WindowAction::Exchange(fc).into();
    /// assert_eq!(act, action!("window exchange -f previously-focused"));
    /// ```
    Exchange(FocusChange),

    /// Change the current focus to the window targeted by [FocusChange].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let fc = FocusChange::PreviouslyFocused;
    /// let act: Action = WindowAction::Focus(fc).into();
    /// assert_eq!(act, action!("window focus -f previously-focused"));
    /// ```
    Focus(FocusChange),

    /// Move the currently focused window to the [MoveDir2D] side of the screen.
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
    MoveSide(MoveDir2D),

    /// Open a new window that is [*n*](Count) columns along [an axis](Axis), positioned relative to
    /// the current window as indicated by [MoveDir1D].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Unnamed;
    /// let axis = Axis::Horizontal;
    /// let dir = MoveDir1D::Next;
    /// let count = Count::Contextual;
    /// let act: Action = WindowAction::Open(target, axis, dir, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("window open -t unnamed -x horizontal -d next -c ctx"));
    /// assert_eq!(act, action!("window open -t unnamed -x horizontal -d next"));
    /// ```
    Open(OpenTarget<I::WindowId>, Axis, MoveDir1D, Count),

    /// Visually rotate the windows in [MoveDir2D] direction.
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
    Rotate(MoveDir1D),

    /// Split the currently focused window [*n* times](Count) along [an axis](Axis), moving
    /// the focus in [MoveDir1D] direction after performing the split.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = OpenTarget::Current;
    /// let axis = Axis::Vertical;
    /// let dir = MoveDir1D::Next;
    /// let count = Count::Contextual;
    /// let act: Action = WindowAction::Split(target, axis, dir, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("window split -t current -x vertical -d next -c ctx"));
    /// assert_eq!(act, action!("window split -t current -x vertical -d next"));
    /// ```
    Split(OpenTarget<I::WindowId>, Axis, MoveDir1D, Count),

    /// Switch what content the window is currently showing.
    ///
    /// If there are no currently open windows in the tab, then this behaves like
    /// [WindowAction::Open].
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
    Switch(OpenTarget<I::WindowId>),

    /// Clear all of the explicitly set window sizes, and instead try to equally distribute
    /// available rows and columns.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::ClearSizes.into();
    /// assert_eq!(act, action!("window clear-sizes"));
    /// ```
    ClearSizes,

    /// Resize the window targeted by [FocusChange] according to [SizeChange].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let size = SizeChange::Equal;
    /// let act: Action = WindowAction::Resize(FocusChange::Current, Axis::Vertical, size).into();
    /// assert_eq!(act, action!("window resize -f current -x vertical -z equal"));
    /// ```
    Resize(FocusChange, Axis, SizeChange),

    /// Write the contents of the windows targeted by [WindowTarget].
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
    Write(WindowTarget, Option<String>, WriteFlags),

    /// Zoom in on the currently focused window so that it takes up the whole screen. If there is
    /// already a zoomed-in window, then return to showing all windows.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = WindowAction::ZoomToggle.into();
    /// assert_eq!(act, action!("window zoom-toggle"));
    /// ```
    ZoomToggle,
}

/// Actions for editing text within buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum EditorAction {
    /// Complete the text before the cursor group leader.
    ///
    /// See the documentation for the [CompletionStyle] variants for how to construct all of the
    /// different [EditorAction::Complete] values using [action].
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
    /// let act: Action = EditorAction::Complete(style, ct.clone(), display).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, action!("complete -s prefix -T auto -D list"));
    /// assert_eq!(act, action!("complete -s prefix -T {ct} -D list"));
    /// assert_eq!(act, action!("complete -s prefix -D list"));
    /// ```
    Complete(CompletionStyle, CompletionType, CompletionDisplay),

    /// Modify the current cursor group.
    ///
    /// See the documentation for the [CursorAction] variants for how to construct all of the
    /// different [EditorAction::Cursor] values using [action].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    ///
    /// let close: Action = action!("cursor close -t leader");
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Leader).into());
    ///
    /// let restore: Action = action!("cursor restore -s append");
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Append).into());
    /// ```
    Cursor(CursorAction),

    /// Perform the specified [action](EditAction) on [a target](EditTarget).
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let ctx = Specifier::Contextual;
    /// let target = EditTarget::CurrentPosition;
    /// let act: Action = EditorAction::Edit(ctx, target.clone()).into();
    /// assert_eq!(act, action!("edit -o ctx -t {target}"));
    /// ```
    Edit(Specifier<EditAction>, EditTarget),

    /// Perform a history operation.
    ///
    /// See the documentation for the [HistoryAction] variants for how to construct all of the
    /// different [EditorAction::History] values using [action].
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    ///
    /// let undo: Action = action!("history undo");
    /// assert_eq!(undo, HistoryAction::Undo(Count::Contextual).into());
    ///
    /// let redo: Action = action!("history redo");
    /// assert_eq!(redo, HistoryAction::Redo(Count::Contextual).into());
    /// ```
    History(HistoryAction),

    /// Insert text.
    ///
    /// See the documentation for the [InsertTextAction] variants for how to construct all of the
    /// different [EditorAction::InsertText] values using [action].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let paste: Action = action!("insert paste -s cursor -c 10");
    /// assert_eq!(paste, InsertTextAction::Paste(PasteStyle::Cursor, 10.into()).into());
    /// ```
    InsertText(InsertTextAction),

    /// Create a new [Mark] at the current leader position.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    ///
    /// let mark = Mark::LastYankedBegin;
    /// let set_mark: Action = action!("mark -m {}", mark.clone());
    /// assert_eq!(set_mark, EditorAction::Mark(mark.into()).into());
    ///
    /// let set_mark: Action = action!("mark -m ctx");
    /// assert_eq!(set_mark, EditorAction::Mark(Specifier::Contextual).into());
    /// ```
    Mark(Specifier<Mark>),

    /// Modify the current selection.
    ///
    /// See the documentation for the [SelectionAction] variants for how to construct all of the
    /// different [EditorAction::Selection] values using [action].
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    ///
    /// let act: Action = SelectionAction::Duplicate(MoveDir1D::Next, Count::Contextual).into();
    /// assert_eq!(act, action!("selection duplicate -d next"));
    /// ```
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
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::{action, Action};
    ///
    /// // All of these are equivalent:
    /// let noop: Action = Action::NoOp;
    /// assert_eq!(action!("nop"), noop);
    /// assert_eq!(action!("noop"), noop);
    /// assert_eq!(action!("no-op"), noop);
    /// assert_eq!(Action::default(), noop);
    /// ```
    NoOp,

    /// Perform an editor action.
    ///
    /// See the documentation for the [EditorAction] variants for how to construct all of the
    /// different [Action::Editor] values using [action].
    Editor(EditorAction),

    /// Perform a macro-related action.
    ///
    /// See the documentation for the [MacroAction] variants for how to construct all of the
    /// different [Action::Macro] values using [action].
    Macro(MacroAction),

    /// Navigate through the cursor positions in [the specified list](PositionList).
    ///
    /// If the current window cannot satisfy the given [Count], then this may jump to other
    /// windows.
    Jump(PositionList, MoveDir1D, Count),

    /// Repeat an action sequence with the current context.
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
    ///
    /// See the [RepeatType] documentation for how to construct each of its variants.
    Repeat(RepeatType),

    /// Scroll the viewport in [the specified manner](ScrollStyle).
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let scroll: Action = Action::Scroll(
    ///     ScrollStyle::LinePos(MovePosition::Beginning, 1.into()));
    /// assert_eq!(scroll, action!("scroll -s (line-pos -p beginning -c 1)"));
    /// ```
    ///
    /// See the [ScrollStyle] documentation for how to construct each of its variants.
    Scroll(ScrollStyle),

    /// Lookup the keyword under the cursor.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    ///
    /// let kw: Action = Action::KeywordLookup(KeywordTarget::Selection);
    /// assert_eq!(kw, action!("keyword-lookup -t selection"));
    /// ```
    KeywordLookup(KeywordTarget),

    /// Redraw the screen.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::{action, Action};
    ///
    /// let redraw: Action = action!("redraw-screen");
    /// assert_eq!(redraw, Action::RedrawScreen);
    /// ```
    RedrawScreen,

    /// Show an [InfoMessage].
    ShowInfoMessage(InfoMessage),

    /// Suspend the process.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::{action, Action};
    ///
    /// let suspend: Action = action!("suspend");
    /// assert_eq!(suspend, Action::Suspend);
    /// ```
    Suspend,

    /// Find the [*n*<sup>th</sup>](Count) occurrence of the current application-level search.
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let search: Action = action!("search -d same");
    /// assert_eq!(search, Action::Search(MoveDirMod::Same, Count::Contextual));
    /// ```
    ///
    /// See the documentation for [MoveDirMod] for how to construct all of its values using
    /// [action].
    Search(MoveDirMod, Count),

    /// Perform a command-related action.
    ///
    /// See the documentation for the [CommandAction] variants for how to construct all of the
    /// different [Action::Command] values using [action].
    Command(CommandAction),

    /// Perform a command bar-related action.
    ///
    /// See the documentation for the [CommandBarAction] variants for how to construct all of the
    /// different [Action::CommandBar] values using [action].
    CommandBar(CommandBarAction<I>),

    /// Perform a prompt-related action.
    ///
    /// See the documentation for the [PromptAction] variants for how to construct all of the
    /// different [Action::Prompt] values using [action].
    Prompt(PromptAction),

    /// Perform a tab-related action.
    ///
    /// See the documentation for the [TabAction] variants for how to construct all of the
    /// different [Action::Tab] values using [action].
    Tab(TabAction<I>),

    /// Perform a window-related action.
    ///
    /// See the documentation for the [WindowAction] variants for how to construct all of the
    /// different [Action::Window] values using [action].
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

            Action::KeywordLookup(_) => SequenceStatus::Ignore,
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
            Action::KeywordLookup(_) => SequenceStatus::Atom,
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
            Action::KeywordLookup(_) => SequenceStatus::Ignore,
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
            Action::KeywordLookup(_) => false,
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
