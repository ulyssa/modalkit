//! # Application Customization
//!
//! ## Overview
//!
//! This module contains traits that library consumers can use for extending modalkit to better fit
//! their own needs.
//!
//! ## Example
//!
//! ```
//! use std::fmt;
//! use std::path::PathBuf;
//!
//! use editor_types::{
//!     application::{
//!         ApplicationAction,
//!         ApplicationError,
//!         ApplicationWindowId,
//!         ApplicationContentId,
//!         ApplicationInfo,
//!         ApplicationStore,
//!     },
//!     context::EditContext,
//!     prelude::*,
//! };
//! use keybindings::SequenceStatus;
//!
//! // Unique identifier for a review.
//! #[derive(Clone, Debug, Eq, Hash, PartialEq)]
//! struct ReviewId(usize);
//!
//! // Unique identifier for a user.
//! #[derive(Clone, Debug, Eq, Hash, PartialEq)]
//! struct UserId(usize);
//!
//! #[derive(Clone, Debug, Eq, PartialEq)]
//! enum CodeReviewAction {
//!     // Approve a review for merging.
//!     Approve(ReviewId),
//!
//!     // Leave a comment on a line in a file in a review.
//!     Comment(ReviewId, PathBuf, usize, String),
//!
//!     // Show more lines around the hunk.
//!     ExpandHunk(Count),
//!
//!     // Merge changes after review and approval.
//!     Merge(ReviewId),
//! }
//!
//! impl ApplicationAction for CodeReviewAction {
//!     fn is_edit_sequence(&self, _: &EditContext) -> SequenceStatus {
//!         match self {
//!             CodeReviewAction::Approve(..) => SequenceStatus::Break,
//!             CodeReviewAction::Comment(..) => SequenceStatus::Break,
//!             CodeReviewAction::ExpandHunk(..) => SequenceStatus::Atom,
//!             CodeReviewAction::Merge(..) => SequenceStatus::Break,
//!         }
//!     }
//!
//!     fn is_last_action(&self, _: &EditContext) -> SequenceStatus {
//!         match self {
//!             CodeReviewAction::Approve(..) => SequenceStatus::Atom,
//!             CodeReviewAction::Comment(..) => SequenceStatus::Atom,
//!             CodeReviewAction::ExpandHunk(..) => SequenceStatus::Atom,
//!             CodeReviewAction::Merge(..) => SequenceStatus::Atom,
//!         }
//!     }
//!
//!     fn is_last_selection(&self, _: &EditContext) -> SequenceStatus {
//!         match self {
//!             CodeReviewAction::Approve(..) => SequenceStatus::Ignore,
//!             CodeReviewAction::Comment(..) => SequenceStatus::Ignore,
//!             CodeReviewAction::ExpandHunk(..) => SequenceStatus::Ignore,
//!             CodeReviewAction::Merge(..) => SequenceStatus::Ignore,
//!         }
//!     }
//!
//!     fn is_switchable(&self, _: &EditContext) -> bool {
//!         match self {
//!             CodeReviewAction::Approve(..) => false,
//!             CodeReviewAction::Comment(..) => false,
//!             CodeReviewAction::ExpandHunk(..) => false,
//!             CodeReviewAction::Merge(..) => false,
//!         }
//!     }
//! }
//!
//! struct CodeReviewStore {
//!     user: UserId,
//! }
//!
//! impl ApplicationStore for CodeReviewStore {}
//!
//! #[derive(Clone, Debug, Eq, Hash, PartialEq)]
//! enum CodeReviewWindowId {
//!     // A window that shows a code review.
//!     Review(ReviewId),
//!
//!     // A window that shows a user's open reviews.
//!     User(UserId),
//! }
//!
//! impl ApplicationWindowId for CodeReviewWindowId {}
//!
//! #[derive(Clone, Debug, Eq, Hash, PartialEq)]
//! enum CodeReviewContentId {
//!     // Different buffer used by the command bar.
//!     Command(CommandType),
//!
//!     // Buffer for a comment left on a line.
//!     Review(ReviewId, usize),
//! }
//!
//! impl ApplicationContentId for CodeReviewContentId {}
//!
//! #[derive(Debug)]
//! enum CodeReviewError {
//!     NoReview(ReviewId),
//! }
//!
//! impl fmt::Display for CodeReviewError {
//!    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//!        match self {
//!            CodeReviewError::NoReview(id) => write!(f, "No review with ID {:?}", id),
//!        }
//!    }
//! }
//!
//! impl ApplicationError for CodeReviewError {}
//!
//! #[derive(Clone, Debug, Eq, PartialEq)]
//! enum CodeReviewInfo {}
//!
//! impl ApplicationInfo for CodeReviewInfo {
//!     type Error = CodeReviewError;
//!     type Action = CodeReviewAction;
//!     type Store = CodeReviewStore;
//!     type WindowId = CodeReviewWindowId;
//!     type ContentId = CodeReviewContentId;
//!
//!     fn content_of_command(ct: CommandType) -> CodeReviewContentId {
//!         CodeReviewContentId::Command(ct)
//!     }
//! }
//! ```
use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::context::EditContext;
use crate::prelude::CommandType;
use keybindings::SequenceStatus;

/// Trait for objects that describe application-specific actions.
///
/// Implementors of this trait can be used with [Action::Application]. This can then be used to
/// create additional keybindings and commands on top of the more editing-specific ones provided
/// by this crate.
///
/// [Action::Application]: crate::Action::Application
pub trait ApplicationAction: Clone + Debug + Eq + PartialEq + Send {
    /// Allows controlling how application-specific actions are included in
    /// [RepeatType::EditSequence](crate::prelude::RepeatType::EditSequence).
    fn is_edit_sequence(&self, ctx: &EditContext) -> SequenceStatus;

    /// Allows controlling how application-specific actions are included in
    /// [RepeatType::LastAction](crate::prelude::RepeatType::LastAction).
    fn is_last_action(&self, ctx: &EditContext) -> SequenceStatus;

    /// Allows controlling how application-specific actions are included in
    /// [RepeatType::LastSelection](crate::prelude::RepeatType::LastSelection).
    fn is_last_selection(&self, ctx: &EditContext) -> SequenceStatus;

    /// Allows controlling whether an application-specific action should be retried in
    /// a new buffer when the currently targeted buffer returns a `WrongBuffer`-style error.
    ///
    /// For example, jumping to a mark in a different buffer when the current one doesn't
    /// contain the mark.
    fn is_switchable(&self, ctx: &EditContext) -> bool;
}

impl ApplicationAction for () {
    fn is_edit_sequence(&self, _: &EditContext) -> SequenceStatus {
        SequenceStatus::Break
    }

    fn is_last_action(&self, _: &EditContext) -> SequenceStatus {
        SequenceStatus::Ignore
    }

    fn is_last_selection(&self, _: &EditContext) -> SequenceStatus {
        SequenceStatus::Ignore
    }

    fn is_switchable(&self, _: &EditContext) -> bool {
        false
    }
}

/// Trait for application-specific errors.
pub trait ApplicationError: Debug + Display {}

impl ApplicationError for String {}

/// Trait for objects that hold application-specific information.
pub trait ApplicationStore {}

impl ApplicationStore for () {}

/// Trait for window identifiers in an application.
pub trait ApplicationWindowId: Clone + Debug + Eq + Hash + Send {}

impl ApplicationWindowId for () {}
impl ApplicationWindowId for usize {}
impl ApplicationWindowId for Option<usize> {}
impl ApplicationWindowId for String {}

/// Trait for identifiers of specific content within a window in an application.
pub trait ApplicationContentId: Clone + Debug + Eq + Hash + Send {}

impl ApplicationContentId for () {}
impl ApplicationContentId for usize {}
impl ApplicationContentId for Option<usize> {}
impl ApplicationContentId for String {}

/// Trait for objects that describe application-specific behaviour and types.
#[allow(unused)]
pub trait ApplicationInfo: Clone + Debug + Eq + PartialEq {
    /// An application-specific error type.
    type Error: ApplicationError;

    /// The type for application-specific actions.
    type Action: ApplicationAction;

    /// The type for application-specific storage.
    type Store: ApplicationStore;

    /// The type for application-specific windows.
    type WindowId: ApplicationWindowId;

    /// The type for application-specific content within a window.
    type ContentId: ApplicationContentId;

    /// Get the [ApplicationContentId] used to show a given command type.
    fn content_of_command(cmdtype: CommandType) -> Self::ContentId;
}

/// A default implementor of [ApplicationInfo] for consumers that don't require any customization.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EmptyInfo {}

impl ApplicationInfo for EmptyInfo {
    type Error = String;
    type Action = ();
    type Store = ();
    type WindowId = String;
    type ContentId = String;

    fn content_of_command(cmdtype: CommandType) -> String {
        match cmdtype {
            CommandType::Search => "*search*".into(),
            CommandType::Command => "*command*".into(),
        }
    }
}
