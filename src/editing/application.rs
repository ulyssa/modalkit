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
//! use std::path::PathBuf;
//!
//! use modalkit::{
//!     input::bindings::SequenceStatus,
//!     editing::{
//!         application::{
//!             ApplicationAction,
//!             ApplicationInfo,
//!             ApplicationStore,
//!         },
//!         base::Count,
//!         context::EditContext,
//!     },
//! };
//!
//! // Unique identifier for a review.
//! #[derive(Clone, Debug, Eq, PartialEq)]
//! struct ReviewId(usize);
//!
//! // Unique identifier for a user.
//! #[derive(Clone, Debug, Eq, PartialEq)]
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
//! }
//!
//! struct CodeReviewStore {
//!     user: UserId,
//! }
//!
//! impl ApplicationStore for CodeReviewStore {}
//!
//! #[derive(Clone, Debug, Eq, PartialEq)]
//! enum CodeReviewInfo {}
//!
//! impl ApplicationInfo for CodeReviewInfo {
//!     type Action = CodeReviewAction;
//!     type Store = CodeReviewStore;
//! }
//! ```
use std::fmt::Debug;

use crate::editing::context::EditContext;
use crate::input::bindings::SequenceStatus;

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
}

/// Trait for objects that hold application-specific information.
///
/// Implementors of this trait can be embedded in [Store](super::store::Store).
pub trait ApplicationStore {}

impl ApplicationStore for () {}

/// Trait for objects that describe application-specific behaviour and types.
pub trait ApplicationInfo: Clone + Debug + Eq + PartialEq {
    /// The type for application-specific actions.
    type Action: ApplicationAction;

    /// The type for application-specific storage.
    type Store: ApplicationStore;
}

/// A default implementor of [ApplicationInfo] for consumers that don't require any customization.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EmptyInfo {}

impl ApplicationInfo for EmptyInfo {
    type Action = ();
    type Store = ();
}
