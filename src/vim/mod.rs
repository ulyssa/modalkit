//! # Vim-like User Interfaces
//!
//! ## Overview
//!
//! This module contains components to help with building applications that mimic Vim's user
//! interfaces.
//!
use crossterm::event::KeyEvent;
use regex::Regex;

use crate::{
    input::bindings::{
        EdgeEvent,
        EdgePath,
        EdgePathPart,
        InputKeyClass,
        InputKeyContext,
        Mode,
        ModeKeys,
    },
    input::InputContext,
};

use crate::editing::base::{
    Action,
    Application,
    Char,
    Count,
    CursorAction,
    CursorCloseTarget,
    EditAction,
    EditContext,
    EditTarget,
    InsertStyle,
    Mark,
    MoveDir1D,
    MoveType,
    Register,
    Resolve,
    Specifier,
    TargetShape,
};

use crate::util::{
    get_char,
    get_literal_char,
    keycode_to_num,
    option_muladd_u32,
    option_muladd_usize,
};

pub mod command;
pub mod keybindings;
mod keyparse;

/// Vim's input modes
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum VimMode {
    /// Normal mode keypresses.
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

impl Default for VimMode {
    fn default() -> Self {
        VimMode::Normal
    }
}

impl<P: Application> Mode<Action<P>, VimContext<P>> for VimMode {
    fn enter(&self, prev: VimMode, ctx: &mut VimContext<P>) -> Vec<Action<P>> {
        match self {
            VimMode::Normal => {
                ctx.persist.shape = None;
                ctx.persist.insert = None;

                match prev {
                    VimMode::Normal => {
                        return vec![];
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
                            Action::Edit(action, target),
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
                            Action::Edit(action, target),
                        ];
                    },
                }
            },
            VimMode::Visual => {
                return vec![];
            },
            VimMode::Select => {
                return vec![];
            },
            VimMode::Insert => {
                assert!(ctx.persist.insert.is_some());
                ctx.persist.shape = None;

                match prev {
                    VimMode::Normal | VimMode::Insert => {
                        return vec![];
                    },
                    _ => {
                        let action = EditAction::Motion.into();
                        let target = EditTarget::CurrentPosition;

                        return vec![Action::Edit(action, target)];
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

    fn show(&self, ctx: &VimContext<P>) -> Option<String> {
        match self {
            VimMode::Normal => {
                return None;
            },
            VimMode::Visual => {
                let msg = match ctx.persist.shape {
                    None | Some(TargetShape::CharWise) => "-- VISUAL --",
                    Some(TargetShape::LineWise) => "-- VISUAL LINE --",
                    Some(TargetShape::BlockWise) => "-- VISUAL BLOCK --",
                };

                return Some(msg.to_string());
            },
            VimMode::Select => {
                let msg = match ctx.persist.shape {
                    None | Some(TargetShape::CharWise) => "-- SELECT --",
                    Some(TargetShape::LineWise) => "-- SELECT LINE --",
                    Some(TargetShape::BlockWise) => "-- SELECT BLOCK --",
                };

                return Some(msg.to_string());
            },
            VimMode::Insert => {
                let msg = match ctx.persist.insert {
                    None | Some(InsertStyle::Insert) => "-- INSERT --",
                    Some(InsertStyle::Replace) => "-- REPLACE --",
                };

                return Some(msg.to_string());
            },
            VimMode::OperationPending => {
                return None;
            },
            VimMode::LangArg => {
                return None;
            },
            VimMode::Command => {
                return None;
            },
            VimMode::CharReplaceSuffix => {
                return None;
            },
            VimMode::CharSearchSuffix => {
                return None;
            },
        }
    }
}

impl<P: Application> ModeKeys<KeyEvent, Action<P>, VimContext<P>> for VimMode {
    fn unmapped(&self, ke: &KeyEvent, ctx: &mut VimContext<P>) -> (Vec<Action<P>>, Option<Self>) {
        match self {
            VimMode::Normal => {
                return (vec![], None);
            },
            VimMode::Visual => {
                return (vec![], None);
            },
            VimMode::Select => {
                if let Some(c) = get_char(ke) {
                    ctx.persist.insert = Some(InsertStyle::Insert);

                    let delete = Action::Edit(EditAction::Delete.into(), EditTarget::Selection);
                    let typech = Action::Type(Char::Single(c).into());

                    (vec![delete, typech], Some(VimMode::Insert))
                } else {
                    (vec![], None)
                }
            },
            VimMode::Insert => {
                if let Some(c) = get_char(ke) {
                    (vec![Action::Type(Char::Single(c).into())], None)
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
                if let Some(c) = get_char(ke) {
                    (vec![Action::Type(Char::Single(c).into())], None)
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

pub(crate) type VimEdgeEvent = EdgeEvent<KeyEvent, VimKeyClass>;
pub(crate) type VimEdgePathPart = EdgePathPart<KeyEvent, VimKeyClass>;
pub(crate) type VimEdgePath = EdgePath<KeyEvent, VimKeyClass>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum VimKeyClass {
    Count,
    Register,
    Mark,
    Octal,
    Decimal,
    Hexadecimal,
    Digraph1,
    Digraph2,
}

impl InputKeyClass<KeyEvent> for VimKeyClass {
    fn memberships(ke: &KeyEvent) -> Vec<Self> {
        let mut classes = Vec::new();

        if let Some(c) = get_char(ke) {
            if let '0'..='9' = c {
                classes.push(VimKeyClass::Count);
            }

            if is_register_char(c) {
                classes.push(VimKeyClass::Register);
            }

            if is_mark_char(c) {
                classes.push(VimKeyClass::Mark);
            }

            if let '0'..='7' = c {
                classes.push(VimKeyClass::Octal);
            }

            if let '0'..='9' = c {
                classes.push(VimKeyClass::Decimal);
            }

            if let '0'..='9' | 'a'..='f' | 'A'..='F' = c {
                classes.push(VimKeyClass::Hexadecimal);
            }

            classes.push(VimKeyClass::Digraph1);
            classes.push(VimKeyClass::Digraph2);
        }

        return classes;
    }
}

/// This is the context specific to an action, and gets reset every time a full sequence of
/// keybindings is pressed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ActionContext<P: Application> {
    // Fields for managing entered counts.
    pub(crate) count: Option<usize>,
    pub(crate) counting: Option<usize>,

    // Fields for storing entered codepoints, literals and digraphs.
    pub(crate) dec: Option<u32>,
    pub(crate) oct: Option<u32>,
    pub(crate) hex: Option<u32>,
    pub(crate) any: Option<KeyEvent>,
    pub(crate) digraph1: Option<char>,
    pub(crate) digraph2: Option<char>,

    // Other arguments to key sequences.
    pub(crate) replace: Option<Char>,
    pub(crate) register: Option<Register>,
    pub(crate) register_append: bool,
    pub(crate) mark: Option<Mark>,

    // The editing action to take.
    pub(crate) operation: EditAction,

    // Temporary character search parameters.
    pub(crate) charsearch_params: Option<(MoveDir1D, bool)>,

    // Delayed actions and mode transitions.
    pub(crate) postaction: Option<Action<P>>,
    pub(crate) postmode: Option<VimMode>,

    // Cursor indicator to show on-screen.
    pub(crate) cursor: Option<char>,
}

/// This is the context preserved across actions, and changes either with the mode, or through
/// future keybinding sequences.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PersistentContext {
    pub(crate) charsearch_params: (MoveDir1D, bool),
    pub(crate) charsearch: Option<Char>,
    pub(crate) shape: Option<TargetShape>,
    pub(crate) insert: Option<InsertStyle>,
}

/// This wraps both action specific context, and persistent context.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VimContext<P: Application = ()> {
    pub(crate) action: ActionContext<P>,
    pub(crate) persist: PersistentContext,
}

impl<P: Application> InputContext for VimContext<P> {
    fn reset(&mut self) {
        self.action = ActionContext::default();
    }

    fn take(&mut self) -> Self {
        VimContext {
            persist: self.persist.clone(),
            action: std::mem::take(&mut self.action),
        }
    }
}

impl<P: Application> VimContext<P> {
    fn get_typed(&self) -> Option<Char> {
        if let Some((d1, d2)) = self.get_digraph() {
            let c = Char::Digraph(d1, d2);

            Some(c)
        } else if let Some(cp) = self.get_codepoint() {
            let c = char::from_u32(cp)?;
            let c = Char::Single(c);

            Some(c)
        } else if let Some(c) = self.get_literal_char() {
            let c = Char::Single(c);

            Some(c)
        } else if let Some(s) = self.get_literal_string() {
            let c = Char::CtrlSeq(s);

            Some(c)
        } else {
            None
        }
    }

    fn get_digraph(&self) -> Option<(char, char)> {
        if let (Some(a), Some(b)) = (self.action.digraph1, self.action.digraph2) {
            Some((a, b))
        } else {
            None
        }
    }

    fn get_codepoint(&self) -> Option<u32> {
        self.action.hex.or(self.action.dec).or(self.action.oct)
    }

    fn get_literal_char(&self) -> Option<char> {
        self.action.any.as_ref().and_then(get_literal_char)
    }

    fn get_literal_string(&self) -> Option<String> {
        unimplemented!();
    }
}

impl<P: Application> InputKeyContext<KeyEvent, VimKeyClass> for VimContext<P> {
    fn event(&mut self, ev: &EdgeEvent<KeyEvent, VimKeyClass>, ke: &KeyEvent) {
        match ev {
            EdgeEvent::Key(_) | EdgeEvent::Fallthrough => {
                // Do nothing.
            },
            EdgeEvent::Any => {
                self.action.any = Some(ke.clone());
            },
            EdgeEvent::Class(VimKeyClass::Count) => {
                if let Some(n) = keycode_to_num(ke, 10) {
                    let new = option_muladd_usize(&self.action.counting, 10, n as usize);

                    self.action.counting = Some(new);
                }
            },
            EdgeEvent::Class(VimKeyClass::Octal) => {
                if let Some(n) = keycode_to_num(ke, 8) {
                    let new = option_muladd_u32(&self.action.oct, 8, n);

                    self.action.oct = Some(new);
                }
            },
            EdgeEvent::Class(VimKeyClass::Decimal) => {
                if let Some(n) = keycode_to_num(ke, 10) {
                    let new = option_muladd_u32(&self.action.dec, 10, n);

                    self.action.dec = Some(new);
                }
            },
            EdgeEvent::Class(VimKeyClass::Hexadecimal) => {
                if let Some(n) = keycode_to_num(ke, 16) {
                    let new = option_muladd_u32(&self.action.hex, 16, n);

                    self.action.hex = Some(new);
                }
            },
            EdgeEvent::Class(VimKeyClass::Digraph1) => {
                if let Some(c) = get_char(ke) {
                    self.action.digraph1 = Some(c);
                }
            },
            EdgeEvent::Class(VimKeyClass::Digraph2) => {
                if let Some(c) = get_char(ke) {
                    self.action.digraph2 = Some(c);
                }
            },
            EdgeEvent::Class(VimKeyClass::Mark) => {
                if let Some(c) = get_char(ke) {
                    self.action.mark = char_to_mark(c);
                }
            },
            EdgeEvent::Class(VimKeyClass::Register) => {
                if let Some((reg, append)) = key_to_register(ke) {
                    self.action.register = Some(reg);
                    self.action.register_append = append;
                }
            },
        }
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        self.action.cursor.clone()
    }
}

impl<P: Application> EditContext for VimContext<P> {
    fn get_replace_char(&self) -> Option<Char> {
        self.action.replace.clone()
    }

    fn get_search_regex(&self) -> Option<(MoveDir1D, Regex)> {
        None
    }

    fn get_search_char(&self) -> Option<(MoveDir1D, bool, Char)> {
        if let Some(c) = &self.persist.charsearch {
            let (dir, inc) = self.persist.charsearch_params;

            Some((dir, inc, c.clone()))
        } else {
            None
        }
    }

    fn get_target_shape(&self) -> Option<TargetShape> {
        self.persist.shape.clone()
    }

    fn get_insert_style(&self) -> Option<InsertStyle> {
        self.persist.insert.clone()
    }

    fn get_register(&self) -> Option<Register> {
        self.action.register.clone()
    }

    fn get_register_append(&self) -> bool {
        self.action.register_append
    }
}

impl<P: Application> Default for ActionContext<P> {
    fn default() -> Self {
        ActionContext {
            count: None,
            counting: None,

            dec: None,
            oct: None,
            hex: None,
            any: None,
            digraph1: None,
            digraph2: None,

            replace: None,
            register: None,
            register_append: false,
            mark: None,

            operation: EditAction::default(),

            charsearch_params: None,

            postaction: None,
            postmode: None,

            cursor: None,
        }
    }
}

impl Default for PersistentContext {
    fn default() -> Self {
        PersistentContext {
            charsearch_params: (MoveDir1D::Next, false),
            charsearch: None,
            insert: None,
            shape: None,
        }
    }
}

impl<P: Application> Default for VimContext<P> {
    fn default() -> Self {
        VimContext {
            action: ActionContext::default(),
            persist: PersistentContext::default(),
        }
    }
}

impl<P: Application> Resolve<Count, usize> for VimContext<P> {
    fn resolve(&self, count: &Count) -> usize {
        match count {
            Count::Contextual => self.action.count.unwrap_or(1),
            Count::MinusOne => self.action.count.unwrap_or(0).saturating_sub(1),
            Count::Exact(n) => *n,
        }
    }
}

impl<P: Application> Resolve<Specifier<Char>, Option<Char>> for VimContext<P> {
    fn resolve(&self, c: &Specifier<Char>) -> Option<Char> {
        match c {
            Specifier::Contextual => self.get_typed(),
            Specifier::Exact(c) => Some(c.clone()),
        }
    }
}

impl<P: Application> Resolve<Specifier<Mark>, Mark> for VimContext<P> {
    fn resolve(&self, mark: &Specifier<Mark>) -> Mark {
        match mark {
            Specifier::Contextual => self.action.mark.unwrap_or(Mark::LastJump),
            Specifier::Exact(m) => *m,
        }
    }
}

impl<P: Application> Resolve<Specifier<EditAction>, EditAction> for VimContext<P> {
    fn resolve(&self, mark: &Specifier<EditAction>) -> EditAction {
        match mark {
            Specifier::Contextual => self.action.operation.clone(),
            Specifier::Exact(a) => a.clone(),
        }
    }
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
        ':' => Register::LastCommand,
        '.' => Register::LastInserted,
        '/' => Register::LastSearch,
        '*' => Register::SelectionPrimary,
        '+' => Register::SelectionClipboard,

        _ => return None,
    };

    return Some((r, false));
}

fn key_to_register(ke: &KeyEvent) -> Option<(Register, bool)> {
    char_to_register(get_char(ke)?)
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

#[inline]
fn is_register_char(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '0'..='9' => true,
        '"' => true,
        '-' => true,
        '#' => true,
        '_' => true,
        '%' => true,
        ':' => true,
        '.' => true,
        '/' => true,
        '*' => true,
        '+' => true,
        _ => false,
    }
}

#[inline]
fn is_mark_char(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '0'..='9' => true,
        '\'' | '`' => true,
        '<' | '>' => true,
        '[' | ']' => true,
        '"' => true,
        '^' => true,
        '.' => true,

        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_show_mode() {
        let mut ctx = VimContext::<()>::default();

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
}
