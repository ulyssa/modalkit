//! # Kakoune-like User Interfaces (WIP)
//!
//! This module contains components to help with building applications that mimic Kakoune's user
//! interfaces.
//!
//! This is still a work in progress and you may encounter bugs, missing keybindings,
//! and differences in editing behaviour while using this. If you do, please open
//! an issue with a description of the problem.
//!
use std::marker::PhantomData;

use crate::{
    actions::{Action, EditAction, EditorAction, HistoryAction, InsertTextAction},
    editing::{
        application::{ApplicationInfo, EmptyInfo},
        context::{EditContext, EditContextBuilder},
        cursor::CursorStyle,
    },
    env::{CharacterContext, CommonKeyClass},
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

pub mod keybindings;

/// Kakoune's input modes
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum KakouneMode {
    /// Normal mode keypresses.
    #[default]
    Normal,

    /// Insert mode keypresses.
    Insert,

    /// Prompt mode keypresses.
    Prompt,

    /// User mode keypresses.
    User,

    /// Goto mode keypresses.
    Goto,

    /// View mode keypresses.
    View,

    /// Object selection keypresses.
    ObjectSelect,
}

impl<I: ApplicationInfo> Mode<Action<I>, KakouneState<I>> for KakouneMode {
    fn enter(&self, prev: Self, ctx: &mut KakouneState<I>) -> Vec<Action<I>> {
        match self {
            KakouneMode::Normal => {
                ctx.persist.insert = None;

                return vec![HistoryAction::Checkpoint.into()];
            },
            KakouneMode::Insert => {
                ctx.persist.insert = Some(InsertStyle::Insert);

                match prev {
                    KakouneMode::Normal | KakouneMode::Insert => {
                        return vec![];
                    },
                    _ => {
                        let action = EditAction::Motion.into();
                        let target = EditTarget::CurrentPosition;
                        let act = EditorAction::Edit(action, target);

                        return vec![act.into()];
                    },
                }
            },
            KakouneMode::Prompt => {
                ctx.persist.insert = Some(InsertStyle::Insert);

                return vec![];
            },
            KakouneMode::User => {
                ctx.persist.insert = Some(InsertStyle::Insert);

                return vec![];
            },
            KakouneMode::View | KakouneMode::ObjectSelect | KakouneMode::Goto => {
                ctx.persist.insert = None;

                return vec![];
            },
        }
    }

    fn show(&self, ctx: &KakouneState<I>) -> Option<String> {
        let msg = match self {
            KakouneMode::Normal | KakouneMode::Prompt | KakouneMode::ObjectSelect => "",
            KakouneMode::Goto => "goto",
            KakouneMode::View => "view",
            KakouneMode::User => "user",
            KakouneMode::Insert => "insert",
        };

        let mut res = String::from(msg);

        fn push(s: &mut String, suffix: String) {
            if !s.is_empty() {
                s.push(' ');
            }

            s.push_str(suffix.as_str());
        }

        if let Some(n) = ctx.action.count {
            push(&mut res, format!("param={n}"));
        }

        if let Some(r) = ctx.action.register.as_ref().and_then(register_to_char) {
            push(&mut res, format!("reg={r}"));
        }

        if !res.is_empty() {
            return Some(res);
        } else {
            return None;
        }
    }
}

impl<I: ApplicationInfo> ModeSequence<RepeatType, Action<I>, KakouneState<I>> for KakouneMode {
    fn sequences(
        &self,
        action: &Action<I>,
        ctx: &EditContext,
    ) -> Vec<(RepeatType, SequenceStatus)> {
        match self {
            KakouneMode::Normal |
            KakouneMode::Insert |
            KakouneMode::ObjectSelect |
            KakouneMode::User |
            KakouneMode::View |
            KakouneMode::Goto => {
                vec![
                    (RepeatType::EditSequence, action.is_edit_sequence(SequenceStatus::Break, ctx)),
                    (RepeatType::LastAction, action.is_last_action(ctx)),
                    (RepeatType::LastSelection, action.is_last_selection(ctx)),
                ]
            },
            KakouneMode::Prompt => {
                vec![]
            },
        }
    }
}

impl<I: ApplicationInfo> ModeKeys<TerminalKey, Action<I>, KakouneState<I>> for KakouneMode {
    fn unmapped(
        &self,
        ke: &TerminalKey,
        _: &mut KakouneState<I>,
    ) -> (Vec<Action<I>>, Option<Self>) {
        match self {
            KakouneMode::Normal | KakouneMode::View => {
                return (vec![], None);
            },
            KakouneMode::Insert => {
                if let Some(c) = ke.get_char() {
                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, 1.into());

                    (vec![it.into()], None)
                } else {
                    (vec![], None)
                }
            },
            KakouneMode::Prompt => {
                if let Some(c) = ke.get_char() {
                    let ch = Char::Single(c).into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, 1.into());

                    (vec![it.into()], None)
                } else {
                    (vec![], None)
                }
            },
            KakouneMode::User | KakouneMode::ObjectSelect | KakouneMode::Goto => {
                (vec![], Some(KakouneMode::Normal))
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ObjectPosition {
    Beginning,
    End,
    Whole,
}

/// This is the context specific to an action, and gets reset every time a full sequence of
/// keybindings is pressed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ActionContext {
    // Fields for managing entered counts.
    pub(crate) count: Option<usize>,

    // Other arguments to key sequences.
    pub(crate) register: Option<Register>,
    pub(crate) register_append: bool,

    // Where to place the cursor after performing an operation.
    pub(crate) cursor_end: CursorEnd,

    // Control object selection.
    pub(self) objsel: Option<(SelectionResizeStyle, ObjectPosition, bool)>,

    // Control text selection.
    pub(crate) shape: Option<TargetShape>,

    // Cursor indicator to show on-screen.
    pub(crate) cursor: Option<char>,
}

impl Default for ActionContext {
    fn default() -> Self {
        Self {
            count: None,

            register: None,
            register_append: false,

            cursor_end: CursorEnd::Auto,

            objsel: None,

            shape: None,

            cursor: None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PersistentContext {
    pub(crate) regexsearch_dir: MoveDir1D,
    pub(crate) regexsearch_inc: bool,
    pub(crate) charsearch_params: (MoveDir1D, bool),
    pub(crate) charsearch: Option<Char>,
    pub(crate) insert: Option<InsertStyle>,
}

impl Default for PersistentContext {
    fn default() -> Self {
        Self {
            regexsearch_dir: MoveDir1D::Next,
            regexsearch_inc: true,
            charsearch_params: (MoveDir1D::Next, false),
            charsearch: None,
            insert: None,
        }
    }
}

/// This wraps both action specific context, and persistent context.
#[derive(Debug, Eq, PartialEq)]
pub struct KakouneState<I: ApplicationInfo = EmptyInfo> {
    pub(crate) action: ActionContext,
    pub(crate) persist: PersistentContext,
    pub(self) ch: CharacterContext,

    _p: PhantomData<I>,
}

impl<I: ApplicationInfo> Clone for KakouneState<I> {
    fn clone(&self) -> Self {
        Self {
            action: self.action.clone(),
            persist: self.persist.clone(),
            ch: self.ch.clone(),

            _p: PhantomData,
        }
    }
}

impl<I: ApplicationInfo> Default for KakouneState<I> {
    fn default() -> Self {
        KakouneState {
            action: ActionContext::default(),
            persist: PersistentContext::default(),
            ch: CharacterContext::default(),

            _p: PhantomData,
        }
    }
}

impl<I: ApplicationInfo> InputState for KakouneState<I> {
    type CursorHint = CursorStyle;
    type Output = EditContext;

    fn merge(original: EditContext, _: &EditContext) -> EditContext {
        // Don't allow any overrides for now.
        original
    }

    fn reset(&mut self) {
        self.action = ActionContext::default();
    }

    fn take(&mut self) -> Self::Output {
        let ctx = Self {
            persist: self.persist.clone(),
            action: std::mem::take(&mut self.action),
            ch: std::mem::take(&mut self.ch),

            _p: PhantomData,
        };

        EditContext::from(ctx)
    }

    fn get_cursor_hint(&self) -> Self::CursorHint {
        CursorStyle {
            indicator: self.action.cursor,
            insert: self.persist.insert,
        }
    }
}

impl<I: ApplicationInfo> InputKeyState<TerminalKey, CommonKeyClass> for KakouneState<I> {
    fn event(&mut self, ev: &EdgeEvent<TerminalKey, CommonKeyClass>, ke: &TerminalKey) {
        match ev {
            EdgeEvent::Key(_) | EdgeEvent::Fallthrough => {
                // Do nothing.
            },
            EdgeEvent::Class(CommonKeyClass::Mark) => {
                // Do nothing for now.
            },

            EdgeEvent::Class(CommonKeyClass::Count) => {
                if let Some(n) = keycode_to_num(ke, 10) {
                    let new = option_muladd_usize(&self.action.count, 10, n as usize);

                    self.action.count = Some(new);
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
}

impl<I: ApplicationInfo> From<KakouneState<I>> for EditContext {
    fn from(ctx: KakouneState<I>) -> Self {
        let search_char = if let Some(c) = &ctx.persist.charsearch {
            let (dir, inc) = ctx.persist.charsearch_params;

            Some((dir, inc, c.clone()))
        } else {
            None
        };

        let typed = ctx.ch.get_typed();

        EditContextBuilder::default()
            .count(ctx.action.count)
            .typed_char(typed.clone())
            .cursor_end(ctx.action.cursor_end)
            .replace_char(typed)
            .search_char(search_char)
            .search_regex_dir(ctx.persist.regexsearch_dir)
            .target_shape(ctx.action.shape)
            .insert_style(ctx.persist.insert)
            .last_column(true)
            .register(ctx.action.register.clone())
            .register_append(ctx.action.register_append)
            .search_incremental(ctx.persist.regexsearch_inc)
            .build()
    }
}

fn register_to_char(reg: &Register) -> Option<char> {
    match reg {
        Register::Named(c) => (*c).into(),
        Register::Unnamed => '"'.into(),
        Register::UnnamedMacro => '@'.into(),
        Register::UnnamedCursorGroup => '^'.into(),
        Register::Blackhole => '_'.into(),
        Register::CurBufName => '%'.into(),
        Register::LastCommand(CommandType::Command) => ':'.into(),
        Register::LastCommand(CommandType::Search) => '/'.into(),

        Register::RecentlyDeleted(_) => None,
        Register::SmallDelete => None,
        Register::LastYanked => None,
        Register::LastInserted => None,
        Register::AltBufName => None,
        Register::SelectionPrimary => None,
        Register::SelectionClipboard => None,

        // Catch non-exhaustive pattern:
        _ => None,
    }
}

fn char_to_register(c: char) -> Option<(Register, bool)> {
    let r = match c {
        // Lowercase letters
        c @ 'a'..='z' => Register::Named(c),

        // Uppercase letters
        c @ 'A'..='Z' => Register::Named(c.to_ascii_lowercase()),

        // Special Characters
        '"' => Register::Unnamed,
        '@' => Register::UnnamedMacro,
        '^' => Register::UnnamedCursorGroup,

        '_' => Register::Blackhole,
        '%' => Register::CurBufName,
        ':' => Register::LastCommand(CommandType::Command),
        '/' => Register::LastCommand(CommandType::Search),

        // XXX: implement
        '1'..='9' => return None,
        '#' => return None,
        '.' => return None,
        '|' => return None,

        _ => return None,
    };

    return Some((r, false));
}

fn key_to_register(ke: &TerminalKey) -> Option<(Register, bool)> {
    char_to_register(ke.get_char()?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mode_show() {
        let mut ctx: KakouneState = KakouneState::default();

        // No count has no prompt.
        assert_eq!(KakouneMode::Normal.show(&ctx), None);

        // Count is displayed.
        ctx.action.count = Some(5);
        assert_eq!(KakouneMode::Normal.show(&ctx), Some("param=5".into()));

        // Register is also displayed.
        ctx.action.register = Some(Register::Named('a'));
        assert_eq!(KakouneMode::Normal.show(&ctx), Some("param=5 reg=a".into()));

        // Move to Insert mode.
        assert_eq!(KakouneMode::Insert.show(&ctx), Some("insert param=5 reg=a".into()));
    }
}
