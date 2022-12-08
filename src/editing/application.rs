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
//! use modalkit::{
//!     input::bindings::SequenceStatus,
//!     editing::{
//!         application::{
//!             ApplicationAction,
//!             ApplicationError,
//!             ApplicationWindowId,
//!             ApplicationContentId,
//!             ApplicationInfo,
//!             ApplicationStore,
//!         },
//!         base::Count,
//!         context::EditContext,
//!     },
//! };
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
//!     fn is_edit_sequence<C: EditContext>(&self, _: &C) -> SequenceStatus {
//!         match self {
//!             CodeReviewAction::Approve(..) => SequenceStatus::Break,
//!             CodeReviewAction::Comment(..) => SequenceStatus::Break,
//!             CodeReviewAction::ExpandHunk(..) => SequenceStatus::Atom,
//!             CodeReviewAction::Merge(..) => SequenceStatus::Break,
//!         }
//!     }
//!
//!     fn is_last_action<C: EditContext>(&self, _: &C) -> SequenceStatus {
//!         match self {
//!             CodeReviewAction::Approve(..) => SequenceStatus::Atom,
//!             CodeReviewAction::Comment(..) => SequenceStatus::Atom,
//!             CodeReviewAction::ExpandHunk(..) => SequenceStatus::Atom,
//!             CodeReviewAction::Merge(..) => SequenceStatus::Atom,
//!         }
//!     }
//!
//!     fn is_last_selection<C: EditContext>(&self, _: &C) -> SequenceStatus {
//!         match self {
//!             CodeReviewAction::Approve(..) => SequenceStatus::Ignore,
//!             CodeReviewAction::Comment(..) => SequenceStatus::Ignore,
//!             CodeReviewAction::ExpandHunk(..) => SequenceStatus::Ignore,
//!             CodeReviewAction::Merge(..) => SequenceStatus::Ignore,
//!         }
//!     }
//!
//!     fn is_switchable<C: EditContext>(&self, _: &C) -> bool {
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
//! }
//! ```
use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::{editing::context::EditContext, input::bindings::SequenceStatus};

/// Trait for objects that describe application-specific actions.
///
/// Implementors of this trait can be used with [Action::Application]. This can then be used to
/// create additional keybindings and commands on top of the defaults provided within
/// [modalkit::env](crate::env).
///
/// [Action::Application]: super::action::Action::Application
pub trait ApplicationAction: Clone + Debug + Eq + PartialEq {
    /// Allows controlling how application-specific actions are included in
    /// [RepeatType::EditSequence](crate::editing::base::RepeatType::EditSequence).
    fn is_edit_sequence<C: EditContext>(&self, ctx: &C) -> SequenceStatus;

    /// Allows controlling how application-specific actions are included in
    /// [RepeatType::LastAction](crate::editing::base::RepeatType::LastAction).
    fn is_last_action<C: EditContext>(&self, ctx: &C) -> SequenceStatus;

    /// Allows controlling how application-specific actions are included in
    /// [RepeatType::LastSelection](crate::editing::base::RepeatType::LastSelection).
    fn is_last_selection<C: EditContext>(&self, ctx: &C) -> SequenceStatus;

    /// Allows controlling whether an application-specific action can cause
    /// a buffer switch on an
    /// [EditError::WrongBuffer](crate::editing::action::EditError::WrongBuffer).
    fn is_switchable<C: EditContext>(&self, ctx: &C) -> bool;
}

impl ApplicationAction for () {
    fn is_edit_sequence<C: EditContext>(&self, _: &C) -> SequenceStatus {
        SequenceStatus::Break
    }

    fn is_last_action<C: EditContext>(&self, _: &C) -> SequenceStatus {
        SequenceStatus::Ignore
    }

    fn is_last_selection<C: EditContext>(&self, _: &C) -> SequenceStatus {
        SequenceStatus::Ignore
    }

    fn is_switchable<C: EditContext>(&self, _: &C) -> bool {
        false
    }
}

/// Trait for application-specific errors.
pub trait ApplicationError: Debug + Display {}

impl ApplicationError for String {}

/// Trait for objects that hold application-specific information.
///
/// Implementors of this trait can be embedded in [Store](super::store::Store).
pub trait ApplicationStore {}

impl ApplicationStore for () {}

/// Trait for window identifiers in an application.
pub trait ApplicationWindowId: Clone + Debug + Eq + Hash + PartialEq {}

impl ApplicationWindowId for () {}
impl ApplicationWindowId for usize {}
impl ApplicationWindowId for String {}

/// Trait for identifiers of specific content within a window in an application.
pub trait ApplicationContentId: Clone + Debug + Eq + Hash + PartialEq {}

impl ApplicationContentId for () {}
impl ApplicationContentId for usize {}
impl ApplicationContentId for String {}

/// Trait for objects that describe application-specific behaviour and types.
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
}
