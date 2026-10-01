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
use std::str::FromStr;

pub mod application;
pub mod context;
pub mod prelude;
pub mod util;

mod parser;

use self::application::*;
use self::context::{EditContext, Resolve};
use self::prelude::*;
use keybindings::SequenceStatus;

/// A macro that turns a shorthand command DSL into an [Action].
pub use editor_types_macros::action;

/// A macro that turns a shorthand command DSL into an [EditTarget];
pub use editor_types_macros::edit_target;

/// A macro that turns a shorthand command DSL into a [MoveType];
pub use editor_types_macros::motion;

/// A macro that turns a shorthand command DSL into a [RangeType];
pub use editor_types_macros::range;

/// The various actions that can be taken on text.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum EditAction {
    /// Move the cursor.
    ///
    /// If a shape is [specified contextually](EditContext::get_target_shape), then visually select
    /// text while moving, as if using [SelectionAction::Resize] with
    /// [SelectionResizeStyle::Extend].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Motion.into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact motion)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Motion.into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact motion)"));
    /// ```
    #[default]
    Motion,

    /// Delete the targeted text.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Delete.into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact delete)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Delete.into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact delete)"));
    /// ```
    Delete,

    /// Yank the targeted text into a [Register].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Yank.into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact yank)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Yank.into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact yank)"));
    /// ```
    Yank,

    /// Replace characters within the targeted text with a new character.
    ///
    /// If [bool] is true, virtually replace characters by how many columns they occupy.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Replace(true).into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact replace --virtual true)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Replace(true).into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact replace --virtual true)"));
    /// ```
    Replace(bool),

    /// Automatically format the targeted text.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Format.into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact format)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Format.into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact format)"));
    /// ```
    Format,

    /// Change the first number on each line within the targeted text.
    ///
    /// The [bool] argument controls whether to increment by an additional count on each line.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let change = NumberChange::Decrease(Count::Contextual);
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::ChangeNumber(change.clone(), false).into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact change-number -s decrease --multiply false)").unwrap());
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact change-num -s decrease --multiply false)").unwrap());
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact change-num -s (decrease -c ctx) --multiply false)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let change = NumberChange::Decrease(Count::Contextual);
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::ChangeNumber(change.clone(), false).into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact change-number -s decrease --multiply false)"));
    /// assert_eq!(act, action!("edit -t selection -o (exact change-num -s decrease --multiply false)"));
    /// assert_eq!(act, action!("edit -t selection -o (exact change-num -s (decrease -c ctx) --multiply false)"));
    /// assert_eq!(act, action!("edit -t selection -o (exact change-num -s {change} --multiply false)"));
    /// ```
    ChangeNumber(NumberChange, bool),

    /// Join the lines within the targeted text together.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let style = JoinStyle::NoChange;
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Join(style).into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact join -s no-change)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let style = JoinStyle::NoChange;
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Join(style).into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact join -s no-change)"));
    /// assert_eq!(act, action!("edit -t selection -o (exact join -s {style})"));
    /// ```
    Join(JoinStyle),

    /// Change the indent level of the targeted text.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let change = IndentChange::Decrease(Count::Contextual);
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Indent(change.clone()).into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact indent -s decrease)").unwrap());
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact indent -s (decrease -c ctx))").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let change = IndentChange::Decrease(Count::Contextual);
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::Indent(change.clone()).into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact indent -s decrease)"));
    /// assert_eq!(act, action!("edit -t selection -o (exact indent -s (decrease -c ctx))"));
    /// assert_eq!(act, action!("edit -t selection -o (exact indent -s {change})"));
    /// ```
    Indent(IndentChange),

    /// Change the case of the targeted text.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::ChangeCase(Case::Lower).into(), EditTarget::Selection).into();
    /// assert_eq!(act, Action::from_str("edit -t selection -o (exact change-case -s lower)").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditAction, EditorAction};
    ///
    /// let act: Action = EditorAction::Edit(
    ///     EditAction::ChangeCase(Case::Lower).into(), EditTarget::Selection).into();
    /// assert_eq!(act, action!("edit -t selection -o (exact change-case -s lower)"));
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let count = Count::Contextual;
    /// let act: Action = SelectionAction::Duplicate(MoveDir1D::Next, count.clone()).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(Action::from_str("selection duplicate -d next").unwrap(), act);
    /// assert_eq!(Action::from_str("selection duplicate -d next -c ctx").unwrap(), act);
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let change = SelectionCursorChange::End;
    /// let act: Action = Action::from_str("selection cursor-set -f end").unwrap();
    /// assert_eq!(act, SelectionAction::CursorSet(change).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let style = SelectionBoundary::Line;
    /// let split: Action = Action::from_str("selection expand -b line -t all").unwrap();
    /// assert_eq!(split, SelectionAction::Expand(style, TargetShapeFilter::ALL).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let act = SelectionAction::Filter(MatchAction::Keep);
    /// let split: Action = Action::from_str("selection filter -F keep").unwrap();
    /// assert_eq!(split, act.into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = SelectionAction::Join.into();
    /// assert_eq!(act, Action::from_str("selection join").unwrap());
    /// ```
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
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let style = SelectionResizeStyle::Restart;
    /// let target = EditTarget::CurrentPosition;
    /// let act: Action = SelectionAction::Resize(style, target).into();
    /// assert_eq!(act, Action::from_str("selection resize -s restart -t curr-pos").unwrap());
    /// ```
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
    /// assert_eq!(act, action!("selection resize -s restart -t curr-pos"));
    /// assert_eq!(act, action!("selection resize -s restart -t {}", target.clone()));
    /// assert_eq!(act, action!("selection resize -s {style} -t {target}"));
    /// ```
    Resize(SelectionResizeStyle, EditTarget),

    /// Split [matching selections](TargetShapeFilter) into multiple selections line.
    ///
    /// All of the new selections are of the same shape as the one they were split from.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let style = SelectionSplitStyle::Lines;
    /// let split: Action = Action::from_str("selection split -s lines -F all").unwrap();
    /// assert_eq!(split, SelectionAction::Split(style, TargetShapeFilter::ALL).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let style = SelectionBoundary::Line;
    /// let split: Action = Action::from_str("selection trim -b line -t all").unwrap();
    /// assert_eq!(split, SelectionAction::Trim(style, TargetShapeFilter::ALL).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    /// use std::str::FromStr;
    ///
    /// let shape = TargetShape::LineWise;
    /// let count = Count::Contextual;
    /// let act: Action = InsertTextAction::OpenLine(shape, MoveDir1D::Next, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("insert open-line -S line -d next -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("insert open-line -S line -d next").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    /// use std::str::FromStr;
    ///
    /// let paste: Action = Action::from_str("insert paste -s (side -d next) -c 5").unwrap();
    /// assert_eq!(paste, InsertTextAction::Paste(PasteStyle::Side(MoveDir1D::Next), Count::Exact(5)).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    /// use std::str::FromStr;
    ///
    /// let input: Action = Action::from_str(r#"insert transcribe -i "hello" -d next -c 1"#).unwrap();
    /// assert_eq!(input, InsertTextAction::Transcribe("hello".into(), MoveDir1D::Next, 1.into()).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    /// use std::str::FromStr;
    ///
    /// let c = Specifier::Exact(Char::from('a'));
    /// let dir = MoveDir1D::Previous;
    /// let count = Count::Contextual;
    /// let act: Action = InsertTextAction::Type(c.clone(), dir, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("insert type -i (exact \'a\') -d previous -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("insert type -i (exact \'a\') -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("insert type -i (exact \'a\') -d previous").unwrap());
    /// assert_eq!(act, Action::from_str("insert type -i (exact \'a\')").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    /// use std::str::FromStr;
    ///
    /// let check: Action = Action::from_str("history checkpoint").unwrap();
    /// assert_eq!(check, HistoryAction::Checkpoint.into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    /// use std::str::FromStr;
    ///
    /// let redo: Action = Action::from_str("history redo").unwrap();
    /// assert_eq!(redo, HistoryAction::Redo(Count::Contextual).into());
    ///
    /// let redo: Action = Action::from_str("history redo -c 1").unwrap();
    /// assert_eq!(redo, HistoryAction::Redo(Count::Exact(1)).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    /// use std::str::FromStr;
    ///
    /// let undo: Action = Action::from_str("history undo").unwrap();
    /// assert_eq!(undo, HistoryAction::Undo(Count::Contextual).into());
    ///
    /// let undo: Action = Action::from_str("history undo -c 1").unwrap();
    /// assert_eq!(undo, HistoryAction::Undo(Count::Exact(1)).into());
    /// ```
    ///
    /// ## Example: Using `action!`
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    /// use std::str::FromStr;
    ///
    /// let close: Action = Action::from_str("cursor close -t leader").unwrap();
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Leader).into());
    ///
    /// let close: Action = Action::from_str("cursor close -t followers").unwrap();
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Followers).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    /// use std::str::FromStr;
    ///
    /// let restore: Action = Action::from_str("cursor restore -s append").unwrap();
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Append).into());
    ///
    /// let restore: Action = Action::from_str("cursor restore -s replace").unwrap();
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Replace).into());
    ///
    /// let restore: Action = Action::from_str("cursor restore -s (merge select-cursor -d prev)").unwrap();
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Merge(CursorMergeStyle::SelectCursor(MoveDir1D::Previous))).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    /// use std::str::FromStr;
    ///
    /// let rotate: Action = Action::from_str("cursor rotate -d prev").unwrap();
    /// assert_eq!(rotate, CursorAction::Rotate(MoveDir1D::Previous, Count::Contextual).into());
    ///
    /// let rotate: Action = Action::from_str("cursor rotate -d next -c 2").unwrap();
    /// assert_eq!(rotate, CursorAction::Rotate(MoveDir1D::Next, Count::Exact(2)).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    /// use std::str::FromStr;
    ///
    /// let save: Action = Action::from_str("cursor save -s append").unwrap();
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Append).into());
    ///
    /// let save: Action = Action::from_str("cursor save -s replace").unwrap();
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Replace).into());
    ///
    /// let save: Action = Action::from_str("cursor save -s (merge union)").unwrap();
    /// assert_eq!(save, CursorAction::Save(CursorGroupCombineStyle::Merge(CursorMergeStyle::Union)).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    /// use std::str::FromStr;
    ///
    /// let split: Action = Action::from_str("cursor split -c ctx").unwrap();
    /// assert_eq!(split, CursorAction::Split(Count::Contextual).into());
    ///
    /// let split: Action = Action::from_str("cursor split -c 5").unwrap();
    /// assert_eq!(split, CursorAction::Split(Count::Exact(5)).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandAction};
    /// use std::str::FromStr;
    ///
    /// let quitall: Action = Action::from_str(r#"command run -i "quitall" "#).unwrap();
    /// assert_eq!(quitall, CommandAction::Run("quitall".into()).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandAction};
    /// use std::str::FromStr;
    ///
    /// let exec: Action = Action::from_str("command execute").unwrap();
    /// assert_eq!(exec, CommandAction::Execute(Count::Contextual).into());
    ///
    /// let exec5: Action = Action::from_str("command execute -c 5").unwrap();
    /// assert_eq!(exec5, CommandAction::Execute(5.into()).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    /// use std::str::FromStr;
    ///
    /// let focus: Action = Action::from_str(r#"cmdbar focus -P "/" -s search -a (search -d same)"#).unwrap();
    /// assert_eq!(focus, CommandBarAction::Focus(
    ///     "/".into(),
    ///     CommandType::Search,
    ///     Action::Search(MoveDirMod::Same, Count::Contextual).into(),
    /// ).into());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let focus: Action = action!(r#"cmdbar focus -P "/" -s search -a (search -d same)"#);
    /// assert_eq!(focus, CommandBarAction::Focus(
    ///     "/".into(),
    ///     CommandType::Search,
    ///     Action::Search(MoveDirMod::Same, Count::Contextual).into(),
    /// ).into());
    /// ```
    Focus(String, CommandType, Box<Action<I>>),

    /// Unfocus the command bar.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    /// use std::str::FromStr;
    ///
    /// let unfocus: Action = Action::from_str("cmdbar unfocus").unwrap();
    /// assert_eq!(unfocus, CommandBarAction::Unfocus.into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("prompt abort").unwrap();
    /// let exp: Action = PromptAction::Abort(false).into();
    /// assert_eq!(act, exp);
    /// assert_eq!(Action::from_str("prompt abort --empty false").unwrap(), exp);
    ///
    /// // Require the prompt to be empty:
    /// let act: Action = Action::from_str("prompt abort --empty true").unwrap();
    /// let exp: Action = PromptAction::Abort(true).into();
    /// assert_eq!(act, exp);
    /// ```
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
    /// assert_eq!(action!("prompt abort --empty false"), exp);
    ///
    /// // Require the prompt to be empty:
    /// let act: Action = action!("prompt abort --empty true");
    /// let exp: Action = PromptAction::Abort(true).into();
    /// assert_eq!(act, exp);
    ///
    /// let empty = true;
    /// assert_eq!(action!("prompt abort --empty {empty}"), exp);
    /// ```
    Abort(bool),

    /// Submit the currently entered text.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("prompt submit").unwrap();
    /// let exp: Action = PromptAction::Submit.into();
    /// assert_eq!(act, exp);
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    /// use std::str::FromStr;
    ///
    /// let filter = RecallFilter::All;
    /// let act: Action = PromptAction::Recall(filter.clone(), MoveDir1D::Next, Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("prompt recall -d next -c ctx -F all").unwrap());
    /// assert_eq!(act, Action::from_str("prompt recall -d next -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("prompt recall -d next").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = MacroAction::Execute(Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("macro execute -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("macro execute").unwrap());
    /// assert_eq!(act, Action::from_str("macro exec").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    /// use std::str::FromStr;
    ///
    /// let mac = "hjkl".to_string();
    /// let act: Action = MacroAction::Run(mac, Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("macro run -i \"hjkl\" -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("macro run -i \"hjkl\"").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = MacroAction::Repeat(Count::Contextual).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("macro repeat -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("macro repeat").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = MacroAction::ToggleRecording.into();
    /// assert_eq!(act, Action::from_str("macro toggle-recording").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    /// use std::str::FromStr;
    ///
    /// let fc = TabTarget::Single(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let extract: Action = TabAction::Close(fc, flags).into();
    /// assert_eq!(extract, Action::from_str("tab close -t (single current) -F none").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    /// use std::str::FromStr;
    ///
    /// let extract: Action = TabAction::Extract(FocusChange::Current, MoveDir1D::Next).into();
    /// assert_eq!(extract, Action::from_str("tab extract -f current -d next").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    /// use std::str::FromStr;
    ///
    /// let extract: Action = TabAction::Focus(FocusChange::PreviouslyFocused).into();
    /// assert_eq!(extract, Action::from_str("tab focus -f previously-focused").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    /// use std::str::FromStr;
    ///
    /// let extract: Action = TabAction::Move(FocusChange::PreviouslyFocused).into();
    /// assert_eq!(extract, Action::from_str("tab move -f previously-focused").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    /// use std::str::FromStr;
    ///
    /// let extract: Action = TabAction::Open(OpenTarget::Current, FocusChange::PreviouslyFocused).into();
    /// assert_eq!(extract, Action::from_str("tab open -t current -f previously-focused").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let fc = WindowTarget::Single(FocusChange::Current);
    /// let flags = CloseFlags::NONE;
    /// let extract: Action = WindowAction::Close(fc, flags).into();
    /// assert_eq!(extract, Action::from_str("window close -t (single current) -F none").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let fc = FocusChange::PreviouslyFocused;
    /// let act: Action = WindowAction::Exchange(fc).into();
    /// assert_eq!(act, Action::from_str("window exchange -f previously-focused").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let fc = FocusChange::PreviouslyFocused;
    /// let act: Action = WindowAction::Focus(fc).into();
    /// assert_eq!(act, Action::from_str("window focus -f previously-focused").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = WindowAction::MoveSide(MoveDir2D::Left).into();
    /// assert_eq!(act, Action::from_str("window move-side -d left").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let target = OpenTarget::Unnamed;
    /// let axis = Axis::Horizontal;
    /// let dir = MoveDir1D::Next;
    /// let count = Count::Contextual;
    /// let act: Action = WindowAction::Open(target, axis, dir, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("window open -t unnamed -x horizontal -d next -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("window open -t unnamed -x horizontal -d next").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = WindowAction::Rotate(MoveDir1D::Next).into();
    /// assert_eq!(act, Action::from_str("window rotate -d next").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let target = OpenTarget::Current;
    /// let axis = Axis::Vertical;
    /// let dir = MoveDir1D::Next;
    /// let count = Count::Contextual;
    /// let act: Action = WindowAction::Split(target, axis, dir, count).into();
    ///
    /// // All of these are equivalent:
    /// assert_eq!(act, Action::from_str("window split -t current -x vertical -d next -c ctx").unwrap());
    /// assert_eq!(act, Action::from_str("window split -t current -x vertical -d next").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let target = OpenTarget::Offset(MoveDir1D::Next, 5.into());
    /// let switch: Action = WindowAction::Switch(target).into();
    /// assert_eq!(switch, Action::from_str("window switch -t (offset -d next -c 5)").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = WindowAction::ClearSizes.into();
    /// assert_eq!(act, Action::from_str("window clear-sizes").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let size = SizeChange::Equal;
    /// let act: Action = WindowAction::Resize(FocusChange::Current, Axis::Vertical, size).into();
    /// assert_eq!(act, Action::from_str("window resize -f current -x vertical -z equal").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let target = WindowTarget::All;
    /// let flags = WriteFlags::NONE;
    /// let act: Action = WindowAction::Write(target.clone(), None, flags).into();
    /// assert_eq!(act, Action::from_str("window write -t all -F none").unwrap());
    ///
    /// // Write to a specific path:
    /// let act: Action = WindowAction::Write(target, Some("out.txt".into()), flags).into();
    /// let s = r#"window write -t all -i "out.txt" -F none"#;
    /// assert_eq!(act, Action::from_str(s).unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let target = WindowTarget::All;
    /// let flags = WriteFlags::NONE;
    /// let act: Action = WindowAction::Write(target.clone(), None, flags).into();
    /// assert_eq!(act, action!("window write -t all -F none"));
    ///
    /// // Write to a specific path:
    /// let name = String::from("out.txt");
    /// let act: Action = WindowAction::Write(target, Some(name.clone()), flags).into();
    /// assert_eq!(act, action!(r#"window write -t all -i "out.txt" -F none"#));
    /// assert_eq!(act, action!("window write -t all -i {name} -F none"));
    /// ```
    Write(WindowTarget, Option<String>, WriteFlags),

    /// Zoom in on the currently focused window so that it takes up the whole screen. If there is
    /// already a zoomed-in window, then return to showing all windows.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = WindowAction::ZoomToggle.into();
    /// assert_eq!(act, Action::from_str("window zoom-toggle").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let ct = CompletionType::Auto;
    /// let style = CompletionStyle::Prefix;
    /// let display = CompletionDisplay::List;
    /// let act: Action = EditorAction::Complete(style, ct, display).into();
    ///
    /// // Both of these are equivalent:
    /// assert_eq!(act, Action::from_str("complete -s prefix -T auto -D list").unwrap());
    /// assert_eq!(act, Action::from_str("complete -s prefix -D list").unwrap());
    /// ```
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
    ///
    /// // Specify everything as positional arguments:
    /// let style = CompletionStyle::Prefix;
    /// let ct = CompletionType::Auto;
    /// let display = CompletionDisplay::List;
    /// assert_eq!(act, action!("complete -s {} -T {} -D {}", style, ct, display));
    /// ```
    Complete(CompletionStyle, CompletionType, CompletionDisplay),

    /// Modify the current cursor group.
    ///
    /// See the documentation for the [CursorAction] variants for how to construct all of the
    /// different [EditorAction::Cursor] values using [action].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CursorAction};
    /// use std::str::FromStr;
    ///
    /// let close: Action = Action::from_str("cursor close -t leader").unwrap();
    /// assert_eq!(close, CursorAction::Close(CursorCloseTarget::Leader).into());
    ///
    /// let restore: Action = Action::from_str("cursor restore -s append").unwrap();
    /// assert_eq!(restore, CursorAction::Restore(CursorGroupCombineStyle::Append).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let ctx = Specifier::Contextual;
    /// let target = EditTarget::CurrentPosition;
    /// let act: Action = EditorAction::Edit(ctx, target).into();
    /// assert_eq!(act, Action::from_str("edit -o ctx -t curr-pos").unwrap());
    /// ```
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
    /// assert_eq!(act, action!("edit -o ctx -t curr-pos"));
    /// assert_eq!(act, action!("edit -o ctx -t {target}"));
    /// ```
    Edit(Specifier<EditAction>, EditTarget),

    /// Perform a history operation.
    ///
    /// See the documentation for the [HistoryAction] variants for how to construct all of the
    /// different [EditorAction::History] values using [action].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, HistoryAction};
    /// use std::str::FromStr;
    ///
    /// let undo: Action = Action::from_str("history undo").unwrap();
    /// assert_eq!(undo, HistoryAction::Undo(Count::Contextual).into());
    ///
    /// let redo: Action = Action::from_str("history redo").unwrap();
    /// assert_eq!(redo, HistoryAction::Redo(Count::Contextual).into());
    /// ```
    ///
    /// ## Example: Using `action!`
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    /// use std::str::FromStr;
    ///
    /// let paste: Action = Action::from_str("insert paste -s cursor -c 10").unwrap();
    /// assert_eq!(paste, InsertTextAction::Paste(PasteStyle::Cursor, 10.into()).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction};
    /// use std::str::FromStr;
    ///
    /// let mark = Mark::LastYankedBegin;
    /// let set_mark: Action = Action::from_str("mark -m (exact last-yanked-begin)").unwrap();
    /// assert_eq!(set_mark, EditorAction::Mark(mark.into()).into());
    ///
    /// let set_mark: Action = Action::from_str("mark -m ctx").unwrap();
    /// assert_eq!(set_mark, EditorAction::Mark(Specifier::Contextual).into());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, SelectionAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = SelectionAction::Duplicate(MoveDir1D::Next, Count::Contextual).into();
    /// assert_eq!(act, Action::from_str("selection duplicate -d next").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::{action, Action};
    /// use std::str::FromStr;
    ///
    /// // All of these are equivalent:
    /// let noop: Action = Action::NoOp;
    /// assert_eq!(Action::from_str("nop").unwrap(), noop);
    /// assert_eq!(Action::from_str("noop").unwrap(), noop);
    /// assert_eq!(Action::from_str("no-op").unwrap(), noop);
    /// assert_eq!(Action::default(), noop);
    /// ```
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
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction, HistoryAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("history checkpoint").unwrap();
    /// assert_eq!(act, Action::Editor(EditorAction::History(HistoryAction::Checkpoint)));
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, EditorAction, HistoryAction};
    ///
    /// let act: Action = action!("history checkpoint");
    /// assert_eq!(act, Action::Editor(EditorAction::History(HistoryAction::Checkpoint)));
    /// ```
    Editor(EditorAction),

    /// Perform a macro-related action.
    ///
    /// See the documentation for the [MacroAction] variants for how to construct all of the
    /// different [Action::Macro] values using [action].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("macro toggle-recording").unwrap();
    /// assert_eq!(act, Action::Macro(MacroAction::ToggleRecording));
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, MacroAction};
    ///
    /// let act: Action = action!("macro toggle-recording");
    /// assert_eq!(act, Action::Macro(MacroAction::ToggleRecording));
    /// ```
    Macro(MacroAction),

    /// Navigate through the cursor positions in [the specified list](PositionList).
    ///
    /// If the current window cannot satisfy the given [Count], then this may jump to other
    /// windows.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    /// use std::str::FromStr;
    ///
    /// let list = PositionList::JumpList;
    /// let count = Count::Contextual;
    ///
    /// let act: Action = Action::Jump(list, MoveDir1D::Next, count.clone());
    /// assert_eq!(act, Action::from_str("jump -t jump-list -d next -c ctx").unwrap());
    ///
    /// let act: Action = Action::Jump(list, MoveDir1D::Previous, count);
    /// assert_eq!(act, Action::from_str("jump -t jump-list -d previous -c ctx").unwrap());
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, InsertTextAction};
    ///
    /// let list = PositionList::JumpList;
    /// let count = Count::Contextual;
    ///
    /// let act: Action = Action::Jump(list, MoveDir1D::Next, count.clone());
    /// assert_eq!(act, action!("jump -t jump-list -d next -c ctx"));
    ///
    /// let act: Action = Action::Jump(list, MoveDir1D::Previous, count);
    /// assert_eq!(act, action!("jump -t jump-list -d previous -c ctx"));
    /// ```
    Jump(PositionList, MoveDir1D, Count),

    /// Repeat an action sequence with the current context.
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    /// use std::str::FromStr;
    ///
    /// let rep: Action = Action::from_str("repeat -s edit-sequence").unwrap();
    /// assert_eq!(rep, Action::Repeat(RepeatType::EditSequence));
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    /// use std::str::FromStr;
    ///
    /// let scroll: Action = Action::Scroll(
    ///     ScrollStyle::LinePos(MovePosition::Beginning, 1.into()));
    /// assert_eq!(scroll, Action::from_str("scroll -s (line-pos -p beginning -c 1)").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action};
    /// use std::str::FromStr;
    ///
    /// let kw: Action = Action::KeywordLookup(KeywordTarget::Selection);
    /// assert_eq!(kw, Action::from_str("keyword-lookup -t selection").unwrap());
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::{action, Action};
    /// use std::str::FromStr;
    ///
    /// let redraw: Action = Action::from_str("redraw-screen").unwrap();
    /// assert_eq!(redraw, Action::RedrawScreen);
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::{action, Action};
    /// use std::str::FromStr;
    ///
    /// let suspend: Action = Action::from_str("suspend").unwrap();
    /// assert_eq!(suspend, Action::Suspend);
    /// ```
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
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    /// use std::str::FromStr;
    ///
    /// let search: Action = Action::from_str("search -d same").unwrap();
    /// assert_eq!(search, Action::Search(MoveDirMod::Same, Count::Contextual));
    /// ```
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
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("command execute").unwrap();
    /// assert_eq!(act, Action::Command(CommandAction::Execute(Count::Contextual)));
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandAction};
    ///
    /// let act: Action = action!("command execute");
    /// assert_eq!(act, Action::Command(CommandAction::Execute(Count::Contextual)));
    /// ```
    Command(CommandAction),

    /// Perform a command bar-related action.
    ///
    /// See the documentation for the [CommandBarAction] variants for how to construct all of the
    /// different [Action::CommandBar] values using [action].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("cmdbar unfocus").unwrap();
    /// assert_eq!(act, Action::CommandBar(CommandBarAction::Unfocus));
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, CommandBarAction};
    ///
    /// let act: Action = action!("cmdbar unfocus");
    /// assert_eq!(act, Action::CommandBar(CommandBarAction::Unfocus));
    /// ```
    CommandBar(CommandBarAction<I>),

    /// Perform a prompt-related action.
    ///
    /// See the documentation for the [PromptAction] variants for how to construct all of the
    /// different [Action::Prompt] values using [action].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("prompt submit").unwrap();
    /// assert_eq!(act, Action::Prompt(PromptAction::Submit));
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, PromptAction};
    ///
    /// let act: Action = action!("prompt submit");
    /// assert_eq!(act, Action::Prompt(PromptAction::Submit));
    /// ```
    Prompt(PromptAction),

    /// Perform a tab-related action.
    ///
    /// See the documentation for the [TabAction] variants for how to construct all of the
    /// different [Action::Tab] values using [action].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("tab focus -f current").unwrap();
    /// assert_eq!(act, Action::Tab(TabAction::Focus(FocusChange::Current)));
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, TabAction};
    ///
    /// let act: Action = action!("tab focus -f current");
    /// assert_eq!(act, Action::Tab(TabAction::Focus(FocusChange::Current)));
    /// ```
    Tab(TabAction<I>),

    /// Perform a window-related action.
    ///
    /// See the documentation for the [WindowAction] variants for how to construct all of the
    /// different [Action::Window] values using [action].
    ///
    /// ## Example: Using `Action::from_str`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    /// use std::str::FromStr;
    ///
    /// let act: Action = Action::from_str("window zoom-toggle").unwrap();
    /// assert_eq!(act, Action::Window(WindowAction::ZoomToggle));
    /// ```
    ///
    /// ## Example: Using `action!`
    ///
    /// ```
    /// use editor_types::prelude::*;
    /// use editor_types::{action, Action, WindowAction};
    ///
    /// let act: Action = action!("window zoom-toggle");
    /// assert_eq!(act, Action::Window(WindowAction::ZoomToggle));
    /// ```
    Window(WindowAction<I>),

    /// Application-specific command.
    Application(I::Action),
}

impl<I: ApplicationInfo> FromStr for Action<I> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        use editor_types_parser::ActionParserExt;
        let tokens = editor_types_parser::tokenize(s)
            .map_err(|e| anyhow::anyhow!("failed to parse {s:?}: {e}"))?;
        let mut reader = parser::ActionReader::default();
        let act = reader.parse_action(&tokens)?;
        Ok(act)
    }
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
