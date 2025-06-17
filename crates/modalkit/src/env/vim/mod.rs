//! # Vim-like User Interfaces
//!
//! ## Overview
//!
//! This module contains components to help with building applications that mimic Vim's user
//! interfaces.
//!
use std::marker::PhantomData;

use crate::{
    actions::{Action, CursorAction, EditAction, EditorAction, HistoryAction, InsertTextAction},
    key::TerminalKey,
    keybindings::{
        EdgeEvent,
        InputKey,
        InputKeyState,
        InputState,
        Mode,
        ModeKeys,
        ModeSequence,
        SequenceStatus,
    },
    prelude::*,
    util::{keycode_to_num, option_muladd_u32, option_muladd_usize},
};

use crate::editing::{
    application::{ApplicationInfo, EmptyInfo},
    context::{EditContext, EditContextBuilder},
};

use super::{CharacterContext, CommonKeyClass};

pub mod command;
pub mod keybindings;

/// Vim's input modes
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum VimMode {
    /// Normal mode keypresses.
    #[default]
    Normal,

    /// Insert mode keypresses.
    Insert,

    /// Visual mode keypresses.
    Visual,

    /// Select mode keypresses.
    Select,

    /// Keypresses following an operator (e.g. "d" or "y").
    OperationPending,

    /// Command mode keypresses.
    Command,

    #[doc(hidden)]
    LangArg,

    #[doc(hidden)]
    CharReplaceSuffix,

    #[doc(hidden)]
    CharSearchSuffix,
}

impl<I: ApplicationInfo> Mode<Action<I>, VimState<I>> for VimMode {
    fn enter(&self, prev: Self, ctx: &mut VimState<I>) -> Vec<Action<I>> {
        match self {
            VimMode::Normal => {
                ctx.persist.shape = None;
                ctx.persist.insert = None;

                match prev {
                    VimMode::Normal => {
                        return vec![HistoryAction::Checkpoint.into()];
                    },
                    VimMode::Insert => {
                        /*
                         * Our editing context has changed, so we move the cursor to the current position
                         * that it's in. This takes care of clearing any Visual/Select selection, as well
                         * as clamping the cursor at the end of the line when moving from Insert to Normal.
                         */
                        let action = EditAction::Motion.into();
                        let target = EditTarget::Motion(
                            MoveType::Column(MoveDir1D::Previous, false),
                            Count::Exact(1),
                        );

                        return vec![
                            CursorAction::Close(CursorCloseTarget::Followers).into(),
                            EditorAction::Edit(action, target).into(),
                            HistoryAction::Checkpoint.into(),
                        ];
                    },
                    _ => {
                        /*
                         * Our editing context has changed, so we move the cursor to the current position
                         * that it's in. This takes care of clearing any Visual/Select selection, as well
                         * as clamping the cursor at the end of the line when moving from Insert to Normal.
                         */
                        let action = EditAction::Motion.into();
                        let target = EditTarget::CurrentPosition;

                        return vec![
                            CursorAction::Close(CursorCloseTarget::Followers).into(),
                            EditorAction::Edit(action, target).into(),
                            HistoryAction::Checkpoint.into(),
                        ];
                    },
                }
            },
            VimMode::Visual => {
                ctx.persist.insert = None;
                return vec![];
            },
            VimMode::Select => {
                return vec![];
            },
            VimMode::Insert => {
                assert!(ctx.persist.insert.is_some());
                ctx.persist.shape = None;

                match prev {
                    VimMode::Normal | VimMode::Insert | VimMode::Command => {
                        return vec![];
                    },
                    _ => {
                        let action = EditAction::Motion.into();
                        let target = EditTarget::CurrentPosition;

                        return vec![EditorAction::Edit(action, target).into()];
                    },
                }
            },
            VimMode::OperationPending => {
                return vec![];
            },
            VimMode::LangArg => {
                return vec![];
            },
            VimMode::Command => {
                ctx.persist.shape = None;
                ctx.persist.insert = Some(InsertStyle::Insert);
                return vec![];
            },

            VimMode::CharSearchSuffix => {
                return vec![];
            },
            VimMode::CharReplaceSuffix => {
                return vec![];
            },
        }
    }

    fn show(&self, ctx: &VimState<I>) -> Option<String> {
        let recording = ctx.persist.recording.as_ref().and_then(register_to_char);

        let msg = match self {
            VimMode::Visual => {
                match ctx.persist.shape {
                    None | Some(TargetShape::CharWise) => "-- VISUAL --",
                    Some(TargetShape::LineWise) => "-- VISUAL LINE --",
                    Some(TargetShape::BlockWise) => "-- VISUAL BLOCK --",
                }
                .into()
            },
            VimMode::Select => {
                match ctx.persist.shape {
                    None | Some(TargetShape::CharWise) => "-- SELECT --",
                    Some(TargetShape::LineWise) => "-- SELECT LINE --",
                    Some(TargetShape::BlockWise) => "-- SELECT BLOCK --",
                }
                .into()
            },
            VimMode::Insert => {
                match ctx.persist.insert {
                    None | Some(InsertStyle::Insert) => "-- INSERT --",
                    Some(InsertStyle::Replace) => "-- REPLACE --",
                }
                .into()
            },
            VimMode::Normal => None,
            VimMode::OperationPending => None,
            VimMode::CharReplaceSuffix => None,
            VimMode::CharSearchSuffix => None,
            VimMode::LangArg => None,
            VimMode::Command => None,
        };

        match (recording, msg) {
            (Some(c), Some(msg)) => format!("{msg} (recording @{c})").into(),
            (Some(c), None) => format!("recording @{c}").into(),
            (None, Some(msg)) => msg.to_string().into(),
            (None, None) => None,
        }
    }
}

impl<I: ApplicationInfo> ModeSequence<RepeatType, Action<I>, VimState<I>> for VimMode {
    fn sequences(
        &self,
        action: &Action<I>,
        ctx: &EditContext,
    ) -> Vec<(RepeatType, SequenceStatus)> {
        let motion = match self {
            VimMode::Command => {
                // Don't track anything done in Command mode.
                return vec![];
            },
            VimMode::Normal => {
                if ctx.get_insert_style().is_some() {
                    SequenceStatus::Restart
                } else {
                    SequenceStatus::Break
                }
            },
            VimMode::Visual | VimMode::Select => SequenceStatus::Track,
            _ => SequenceStatus::Break,
        };

        return vec![
            (RepeatType::EditSequence, action.is_edit_sequence(motion, ctx)),
            (RepeatType::LastAction, action.is_last_action(ctx)),
            (RepeatType::LastSelection, action.is_last_selection(ctx)),
        ];
    }
}

impl<I: ApplicationInfo> ModeKeys<TerminalKey, Action<I>, VimState<I>> for VimMode {
    fn unmapped(&self, ke: &TerminalKey, ctx: &mut VimState<I>) -> (Vec<Action<I>>, Option<Self>) {
        match self {
            VimMode::Normal => {
                return (vec![], None);
            },
            VimMode::Visual => {
                return (vec![], None);
            },
            VimMode::Select => {
                if let Some(c) = ke.get_char() {
                    ctx.persist.insert = Some(InsertStyle::Insert);

                    let delete = EditAction::Delete.into();
                    let delete = EditorAction::Edit(delete, EditTarget::Selection);

                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, 1.into());

                    (vec![delete.into(), it.into()], Some(VimMode::Insert))
                } else {
                    (vec![], None)
                }
            },
            VimMode::Insert => {
                if let Some(c) = ke.get_char() {
                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, 1.into());

                    (vec![it.into()], None)
                } else {
                    (vec![], None)
                }
            },
            VimMode::OperationPending => {
                return (vec![], None);
            },
            VimMode::LangArg => {
                return (vec![], None);
            },
            VimMode::Command => {
                if let Some(c) = ke.get_char() {
                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, 1.into());

                    (vec![it.into()], None)
                } else {
                    (vec![], None)
                }
            },

            VimMode::CharSearchSuffix => {
                return (vec![], None);
            },
            VimMode::CharReplaceSuffix => {
                return (vec![], None);
            },
        }
    }
}

/// This is the context specific to an action, and gets reset every time a full sequence of
/// keybindings is pressed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ActionContext {
    // Fields for managing entered counts.
    pub(crate) count: Option<usize>,
    pub(crate) counting: Option<usize>,

    // Other arguments to key sequences.
    pub(crate) replace: Option<Char>,
    pub(crate) register: Option<Register>,
    pub(crate) register_append: bool,
    pub(crate) mark: Option<Mark>,

    // An editing action to take, and what text to target.
    pub(crate) operation: EditAction,
    pub(crate) target: Option<EditTarget>,

    // Where to place the cursor after editing.
    pub(crate) cursor_end: Option<CursorEnd>,

    // Temporary character search parameters.
    pub(crate) charsearch_params: Option<(MoveDir1D, bool)>,

    // Delayed mode transition.
    pub(crate) postmode: Option<VimMode>,

    // Cursor indicator to show on-screen.
    pub(crate) cursor: Option<char>,
}

/// This is the context preserved across actions, and changes either with the mode, or through
/// future keybinding sequences.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PersistentContext {
    pub(crate) regexsearch_dir: MoveDir1D,
    pub(crate) regexsearch_inc: bool,
    pub(crate) charsearch_params: (MoveDir1D, bool),
    pub(crate) charsearch: Option<Char>,
    pub(crate) shape: Option<TargetShape>,
    pub(crate) insert: Option<InsertStyle>,
    pub(crate) recording: Option<(Register, bool)>,
    pub(crate) postcmd: (VimMode, Option<TargetShape>, Option<Box<ActionContext>>),
}

/// This wraps both action specific context, and persistent context.
#[derive(Debug, Eq, PartialEq)]
pub struct VimState<I: ApplicationInfo = EmptyInfo> {
    pub(crate) action: ActionContext,
    pub(crate) persist: PersistentContext,
    pub(self) ch: CharacterContext,

    _p: PhantomData<I>,
}

impl<I: ApplicationInfo> Clone for VimState<I> {
    fn clone(&self) -> Self {
        Self {
            action: self.action.clone(),
            persist: self.persist.clone(),
            ch: self.ch.clone(),
            _p: PhantomData,
        }
    }
}

impl<I: ApplicationInfo> InputState for VimState<I> {
    type Output = EditContext;

    fn merge(original: EditContext, overrides: &EditContext) -> EditContext {
        let mut builder = EditContextBuilder::from(original);

        // Allow overriding the two fields that can prefix keybindings.
        if let n @ Some(_) = overrides.get_count() {
            builder = builder.count(n);
        }

        if let reg @ Some(_) = overrides.get_register() {
            builder = builder.register(reg).register_append(overrides.get_register_append());
        }

        builder.build()
    }

    fn reset(&mut self) {
        self.action = ActionContext::default();
    }

    fn take(&mut self) -> Self::Output {
        let state = Self {
            persist: self.persist.clone(),
            action: std::mem::take(&mut self.action),
            ch: std::mem::take(&mut self.ch),

            _p: PhantomData,
        };

        EditContext::from(state)
    }
}

impl<I: ApplicationInfo> InputKeyState<TerminalKey, CommonKeyClass> for VimState<I> {
    fn event(&mut self, ev: &EdgeEvent<TerminalKey, CommonKeyClass>, ke: &TerminalKey) {
        match ev {
            EdgeEvent::Key(_) | EdgeEvent::Fallthrough => {
                // Do nothing.
            },
            EdgeEvent::Class(CommonKeyClass::Count) => {
                if let Some(n) = keycode_to_num(ke, 10) {
                    let new = option_muladd_usize(&self.action.counting, 10, n as usize);

                    self.action.counting = Some(new);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Mark) => {
                if let Some(c) = ke.get_char() {
                    self.action.mark = char_to_mark(c);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Register) => {
                if let Some((reg, append)) = key_to_register(ke) {
                    self.action.register = Some(reg);
                    self.action.register_append = append;
                }
            },

            // Track literals, codepoints, etc.
            EdgeEvent::Any => {
                self.ch.any = Some(*ke);
            },
            EdgeEvent::Class(CommonKeyClass::Octal) => {
                if let Some(n) = keycode_to_num(ke, 8) {
                    let new = option_muladd_u32(&self.ch.oct, 8, n);

                    self.ch.oct = Some(new);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Decimal) => {
                if let Some(n) = keycode_to_num(ke, 10) {
                    let new = option_muladd_u32(&self.ch.dec, 10, n);

                    self.ch.dec = Some(new);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Hexadecimal) => {
                if let Some(n) = keycode_to_num(ke, 16) {
                    let new = option_muladd_u32(&self.ch.hex, 16, n);

                    self.ch.hex = Some(new);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Digraph1) => {
                if let Some(c) = ke.get_char() {
                    self.ch.digraph1 = Some(c);
                }
            },
            EdgeEvent::Class(CommonKeyClass::Digraph2) => {
                if let Some(c) = ke.get_char() {
                    self.ch.digraph2 = Some(c);
                }
            },
        }
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        self.action.cursor
    }
}

impl<I: ApplicationInfo> From<VimState<I>> for EditContext {
    fn from(ctx: VimState<I>) -> Self {
        let search_char = if let Some(c) = &ctx.persist.charsearch {
            let (dir, inc) = ctx.persist.charsearch_params;

            Some((dir, inc, c.clone()))
        } else {
            None
        };

        EditContextBuilder::default()
            .operation(ctx.action.operation)
            .count(ctx.action.count)
            .mark(ctx.action.mark)
            .typed_char(ctx.ch.get_typed())
            .cursor_end(ctx.action.cursor_end.unwrap_or(CursorEnd::Auto))
            .replace_char(ctx.action.replace.clone())
            .search_char(search_char)
            .search_regex_dir(ctx.persist.regexsearch_dir)
            .target_shape(ctx.persist.shape)
            .insert_style(ctx.persist.insert)
            .last_column(ctx.persist.insert.is_some())
            .register(ctx.action.register.clone())
            .register_append(ctx.action.register_append)
            .search_incremental(ctx.persist.regexsearch_inc)
            .build()
    }
}

impl Default for ActionContext {
    fn default() -> Self {
        ActionContext {
            count: None,
            counting: None,

            replace: None,
            register: None,
            register_append: false,
            mark: None,

            operation: EditAction::Motion,
            target: None,

            cursor_end: None,

            charsearch_params: None,

            postmode: None,

            cursor: None,
        }
    }
}

impl Default for PersistentContext {
    fn default() -> Self {
        PersistentContext {
            regexsearch_dir: MoveDir1D::Next,
            regexsearch_inc: false,
            charsearch_params: (MoveDir1D::Next, false),
            charsearch: None,
            insert: None,
            shape: None,
            recording: None,
            postcmd: Default::default(),
        }
    }
}

impl<I: ApplicationInfo> Default for VimState<I> {
    fn default() -> Self {
        Self {
            action: ActionContext::default(),
            persist: PersistentContext::default(),
            ch: CharacterContext::default(),

            _p: PhantomData,
        }
    }
}

fn register_to_char((reg, append): &(Register, bool)) -> Option<String> {
    let c = match reg {
        Register::Named(c) => {
            if *append {
                return c.to_uppercase().to_string().into();
            } else {
                return c.to_string().into();
            }
        },
        Register::RecentlyDeleted(n) => {
            return (n + 1).to_string().into();
        },

        Register::Unnamed => '"',
        Register::UnnamedMacro => '@',
        Register::UnnamedCursorGroup => return None,
        Register::SmallDelete => '-',
        Register::LastCommand(CommandType::Command) => ':',
        Register::LastCommand(CommandType::Search) => '/',
        Register::LastInserted => '.',
        Register::LastYanked => '0',
        Register::AltBufName => '#',
        Register::CurBufName => '%',
        Register::Blackhole => '_',
        Register::SelectionPrimary => '*',
        Register::SelectionClipboard => '+',

        // Unimplemented:
        _ => return None,
    };

    return c.to_string().into();
}

fn char_to_register(c: char) -> Option<(Register, bool)> {
    let r = match c {
        // Numbers
        '0' => Register::LastYanked,
        '1' => Register::RecentlyDeleted(0),
        '2' => Register::RecentlyDeleted(1),
        '3' => Register::RecentlyDeleted(2),
        '4' => Register::RecentlyDeleted(3),
        '5' => Register::RecentlyDeleted(4),
        '6' => Register::RecentlyDeleted(5),
        '7' => Register::RecentlyDeleted(6),
        '8' => Register::RecentlyDeleted(7),
        '9' => Register::RecentlyDeleted(8),

        // Lowercase letters
        c @ 'a'..='z' => Register::Named(c),

        // Uppercase letters
        c @ 'A'..='Z' => return Some((Register::Named(c.to_ascii_lowercase()), true)),

        // Special Characters
        '"' => Register::Unnamed,
        '-' => Register::SmallDelete,
        '#' => Register::AltBufName,
        '_' => Register::Blackhole,
        '%' => Register::CurBufName,
        ':' => Register::LastCommand(CommandType::Command),
        '/' => Register::LastCommand(CommandType::Search),
        '.' => Register::LastInserted,
        '*' => Register::SelectionPrimary,
        '+' => Register::SelectionClipboard,

        _ => return None,
    };

    return Some((r, false));
}

fn key_to_register(ke: &TerminalKey) -> Option<(Register, bool)> {
    ke.get_char().and_then(char_to_register)
}

fn char_to_mark(c: char) -> Option<Mark> {
    let m = match c {
        c @ 'a'..='z' => Mark::BufferNamed(c),
        c @ 'A'..='Z' => Mark::GlobalNamed(c),

        '0' => Mark::GlobalLastExited(0),
        '1' => Mark::GlobalLastExited(1),
        '2' => Mark::GlobalLastExited(2),
        '3' => Mark::GlobalLastExited(3),
        '4' => Mark::GlobalLastExited(4),
        '5' => Mark::GlobalLastExited(5),
        '6' => Mark::GlobalLastExited(6),
        '7' => Mark::GlobalLastExited(7),
        '8' => Mark::GlobalLastExited(8),
        '9' => Mark::GlobalLastExited(9),

        '\'' | '`' => Mark::LastJump,

        '<' => Mark::VisualBegin,
        '>' => Mark::VisualEnd,
        '[' => Mark::LastYankedBegin,
        ']' => Mark::LastYankedEnd,
        '"' => Mark::BufferLastExited,
        '^' => Mark::LastInserted,
        '.' => Mark::LastChanged,

        _ => return None,
    };

    return Some(m);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_show_mode() {
        let mut ctx = VimState::<EmptyInfo>::default();

        let normal = VimMode::Normal;
        let visual = VimMode::Visual;
        let select = VimMode::Select;
        let insert = VimMode::Insert;

        // Mode strings with a default context.
        assert_eq!(normal.show(&ctx), None);
        assert_eq!(visual.show(&ctx).unwrap(), "-- VISUAL --");
        assert_eq!(select.show(&ctx).unwrap(), "-- SELECT --");
        assert_eq!(insert.show(&ctx).unwrap(), "-- INSERT --");

        // Mode strings when shape is CharWise is the same as default.
        ctx.persist.shape = Some(TargetShape::CharWise);
        assert_eq!(visual.show(&ctx).unwrap(), "-- VISUAL --");
        assert_eq!(select.show(&ctx).unwrap(), "-- SELECT --");

        // Mode strings when shape is LineWise.
        ctx.persist.shape = Some(TargetShape::LineWise);
        assert_eq!(visual.show(&ctx).unwrap(), "-- VISUAL LINE --");
        assert_eq!(select.show(&ctx).unwrap(), "-- SELECT LINE --");

        // Mode strings impacted by a BlockWise shape.
        ctx.persist.shape = Some(TargetShape::BlockWise);
        assert_eq!(visual.show(&ctx).unwrap(), "-- VISUAL BLOCK --");
        assert_eq!(select.show(&ctx).unwrap(), "-- SELECT BLOCK --");

        // Mode string when inserting text is the same as default.
        ctx.persist.shape = None;
        ctx.persist.insert = Some(InsertStyle::Insert);
        assert_eq!(insert.show(&ctx).unwrap(), "-- INSERT --");

        // Mode string when replacing text.
        ctx.persist.insert = Some(InsertStyle::Replace);
        assert_eq!(insert.show(&ctx).unwrap(), "-- REPLACE --");
    }

    #[test]
    fn test_char_to_register() {
        assert_eq!(char_to_register('a'), Some((Register::Named('a'), false)));
        assert_eq!(char_to_register('A'), Some((Register::Named('a'), true)));
        assert_eq!(char_to_register('0'), Some((Register::LastYanked, false)));
        assert_eq!(char_to_register('1'), Some((Register::RecentlyDeleted(0), false)));
        assert_eq!(char_to_register('3'), Some((Register::RecentlyDeleted(2), false)));
        assert_eq!(char_to_register('"'), Some((Register::Unnamed, false)));
        assert_eq!(
            char_to_register('/'),
            Some((Register::LastCommand(CommandType::Search), false))
        );

        // Unmapped names.
        assert_eq!(char_to_register('['), None);
    }

    #[test]
    fn test_register_to_char() {
        assert_eq!(register_to_char(&(Register::Named('a'), false)).unwrap(), "a");
        assert_eq!(register_to_char(&(Register::Named('a'), true)).unwrap(), "A");
        assert_eq!(register_to_char(&(Register::LastYanked, false)).unwrap(), "0");
        assert_eq!(register_to_char(&(Register::RecentlyDeleted(0), false)).unwrap(), "1");
        assert_eq!(register_to_char(&(Register::RecentlyDeleted(2), false)).unwrap(), "3");
        assert_eq!(register_to_char(&(Register::Unnamed, false)).unwrap(), "\"");
        assert_eq!(
            register_to_char(&(Register::LastCommand(CommandType::Search), false)).unwrap(),
            "/"
        );

        // Registers that don't have names.
        assert_eq!(register_to_char(&(Register::UnnamedCursorGroup, false)), None);
    }
}
