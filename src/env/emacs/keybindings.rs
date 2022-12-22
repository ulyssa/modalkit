//! # Emacs Keybindings
//!
//! ## Overview
//!
//! This module handles mapping the keybindings used in Emacs onto the
//! [Action] type.
//!
//! ## Divergences
//!
//! The keybindings here diverge from the defaults in Emacs in the following ways:
//!
//! - `C-_` and `C-x u` behave like `M-x undo-only`
//!
use bitflags::bitflags;

use crate::editing::{
    action::{
        Action,
        CommandBarAction,
        EditAction,
        EditorAction,
        HistoryAction,
        InsertTextAction,
        PromptAction,
        SelectionAction,
        WindowAction,
    },
    application::{ApplicationInfo, EmptyInfo},
    base::{
        Axis,
        Case,
        Char,
        CloseFlags,
        CloseTarget,
        CommandType,
        Count,
        EditTarget,
        FocusChange,
        InsertStyle,
        JoinStyle,
        MoveDir1D,
        MoveDir2D,
        MoveDirMod,
        MovePosition,
        MoveTerminus,
        MoveType,
        OpenTarget,
        RangeType,
        Register,
        RepeatType,
        ScrollSize,
        ScrollStyle,
        SearchType,
        SelectionCursorChange,
        SelectionResizeStyle,
        Specifier,
        TargetShape,
        WordStyle,
    },
};

use super::{
    super::{keyparse::parse, CommonKeyClass, ShellBindings},
    EmacsContext,
    EmacsMode,
};

use crate::input::{
    bindings::{InputBindings, ModalMachine, Step},
    key::TerminalKey,
};

bitflags! {
    struct MappedModes: u32 {
        const I = 0b0000000000000001;
        const C = 0b0000000000000010;
        const S = 0b0000000000000100;

        const IC = MappedModes::I.bits | MappedModes::C.bits;
        const ICS = MappedModes::IC.bits | MappedModes::S.bits;
    }
}

const MAP: MappedModes = MappedModes::ICS;
const ICMAP: MappedModes = MappedModes::IC;
const IMAP: MappedModes = MappedModes::I;
const CMAP: MappedModes = MappedModes::C;
const SMAP: MappedModes = MappedModes::S;

impl MappedModes {
    pub fn split(&self) -> Vec<EmacsMode> {
        let mut modes = Vec::new();

        if self.contains(MappedModes::I) {
            modes.push(EmacsMode::Insert);
        }

        if self.contains(MappedModes::C) {
            modes.push(EmacsMode::Command);
        }

        if self.contains(MappedModes::S) {
            modes.push(EmacsMode::Search);
        }

        return modes;
    }
}

#[derive(Clone, Debug)]
enum InternalAction {
    ClearTargetShape(bool),
    SaveCounting(Option<usize>),
    SetInsertStyle(InsertStyle),
    SetRegister(Register),
    SetTargetShape(TargetShape, bool),
}

impl InternalAction {
    pub fn run<I: ApplicationInfo>(&self, ctx: &mut EmacsContext<I>) {
        match self {
            InternalAction::ClearTargetShape(shiftreq) => {
                if *shiftreq {
                    if ctx.persist.shift {
                        ctx.persist.shape = None;
                        ctx.persist.shift = false;
                    }
                } else {
                    ctx.persist.shape = None;
                }
            },
            InternalAction::SaveCounting(optn) => {
                let counting = match (optn, ctx.action.counting) {
                    (Some(n1), Some(n2)) => n1.saturating_mul(10).saturating_add(n2),
                    (Some(n), None) => *n,
                    (None, Some(n)) => n,
                    (None, None) => 4,
                };

                ctx.action.count = Some(match ctx.action.count {
                    None => counting,
                    Some(prev) => prev.saturating_mul(counting),
                });

                ctx.action.counting = None;
            },
            InternalAction::SetInsertStyle(style) => {
                if style == &ctx.persist.insert {
                    ctx.persist.insert = !*style;
                } else {
                    ctx.persist.insert = *style;
                }
            },
            InternalAction::SetRegister(reg) => {
                ctx.action.register = Some(*reg);
            },
            InternalAction::SetTargetShape(shape, shifted) => {
                ctx.persist.shape = Some(*shape);
                ctx.persist.shift = *shifted;
            },
        }
    }
}

#[derive(Debug)]
enum ExternalAction<I: ApplicationInfo> {
    Something(Action<I>),
    Repeat(bool),
}

impl<I: ApplicationInfo> ExternalAction<I> {
    fn resolve(&self, ctx: &mut EmacsContext<I>) -> Vec<Action<I>> {
        match self {
            ExternalAction::Something(act) => {
                ctx.persist.repeating = false;

                vec![act.clone()]
            },
            ExternalAction::Repeat(reqrep) => {
                if *reqrep && !ctx.persist.repeating {
                    let ch = Char::Single('z').into();
                    let it = InsertTextAction::Type(ch, MoveDir1D::Previous, Count::Contextual);

                    return vec![it.into()];
                }

                ctx.persist.repeating = true;

                return vec![Action::Repeat(RepeatType::LastAction)];
            },
        }
    }
}

impl<I: ApplicationInfo> Clone for ExternalAction<I> {
    fn clone(&self) -> Self {
        match self {
            ExternalAction::Something(act) => ExternalAction::Something(act.clone()),
            ExternalAction::Repeat(reqrep) => ExternalAction::Repeat(*reqrep),
        }
    }
}

impl<I: ApplicationInfo> From<Action<I>> for ExternalAction<I> {
    fn from(act: Action<I>) -> Self {
        ExternalAction::Something(act)
    }
}

/// Description of actions to take after an input sequence.
#[derive(Debug)]
pub struct InputStep<I: ApplicationInfo> {
    internal: Vec<InternalAction>,
    external: Vec<ExternalAction<I>>,
    nextm: Option<EmacsMode>,
}

impl<I: ApplicationInfo> InputStep<I> {
    /// Create a new step that input keys can map to.
    pub fn new() -> Self {
        InputStep { internal: vec![], external: vec![], nextm: None }
    }

    /// Set the [actions](Action) that this step produces.
    pub fn actions(mut self, acts: Vec<Action<I>>) -> Self {
        self.external = acts.into_iter().map(ExternalAction::Something).collect();
        self
    }
}

impl<I: ApplicationInfo> Clone for InputStep<I> {
    fn clone(&self) -> Self {
        Self {
            internal: self.internal.clone(),
            external: self.external.clone(),
            nextm: self.nextm.clone(),
        }
    }
}

impl<I: ApplicationInfo> Step<TerminalKey> for InputStep<I> {
    type A = Action<I>;
    type C = EmacsContext<I>;
    type M = EmacsMode;
    type Class = CommonKeyClass;
    type Sequence = RepeatType;

    fn is_unmapped(&self) -> bool {
        match self {
            InputStep { internal, external, nextm: None } => {
                internal.len() == 0 && external.len() == 0
            },
            _ => false,
        }
    }

    fn fallthrough(&self) -> Option<Self::M> {
        None
    }

    fn step(&self, ctx: &mut EmacsContext<I>) -> (Vec<Action<I>>, Option<Self::M>) {
        for iact in self.internal.iter() {
            iact.run(ctx);
        }

        let external: Vec<Action<I>> =
            self.external.iter().flat_map(|act| act.resolve(ctx)).collect();

        return (external, self.nextm);
    }
}

macro_rules! act {
    ($ext: expr) => {
        isv!(vec![], vec![ExternalAction::Something($ext)])
    };
    ($ext: expr, $ns: expr) => {
        isv!(vec![], vec![ExternalAction::Something($ext)], $ns)
    };
}

macro_rules! iact {
    ($int: expr) => {
        isv!(vec![$int], vec![])
    };
    ($int: expr, $ns: expr) => {
        isv!(vec![$int], vec![], $ns)
    };
}

macro_rules! isv {
    () => {
        InputStep { internal: vec![], external: vec![], nextm: None }
    };
    ($ints: expr, $exts: expr) => {
        InputStep { internal: $ints, external: $exts, nextm: None }
    };
    ($ints: expr, $exts: expr, $ns: expr) => {
        InputStep { internal: $ints, external: $exts, nextm: Some($ns) }
    };
}

macro_rules! is {
    ($int: expr, $ext: expr) => {
        isv!(vec![$int], vec![ExternalAction::Something($ext.into())])
    };
    ($int: expr, $ext: expr, $ns: expr) => {
        isv!(vec![$int], vec![ExternalAction::Something($ext.into())], $ns)
    };
}

macro_rules! blackhole {
    ($act: expr) => {
        is!(InternalAction::SetRegister(Register::Blackhole), $act)
    };
    ($act: expr, $nm: expr) => {
        is!(InternalAction::SetRegister(Register::Blackhole), $act, $nm)
    };
}

macro_rules! motion {
    ($mt: expr) => {
        is!(
            InternalAction::ClearTargetShape(true),
            EditorAction::Edit(
                Specifier::Exact(EditAction::Motion),
                EditTarget::Motion($mt, Count::Contextual)
            )
        )
    };
    ($mt: expr, $c: literal) => {
        is!(
            InternalAction::ClearTargetShape(true),
            EditorAction::Edit(
                Specifier::Exact(EditAction::Motion),
                EditTarget::Motion($mt, Count::Exact($c))
            )
        )
    };
    ($mt: expr, $c: expr) => {
        is!(
            InternalAction::ClearTargetShape(true),
            EditorAction::Edit(Specifier::Exact(EditAction::Motion), EditTarget::Motion($mt, $c))
        )
    };
}

macro_rules! yank_target {
    ($target: expr) => {
        edit_target!(EditAction::Yank, $target)
    };
    ($target: expr, $c: expr) => {
        edit_target!(EditAction::Yank, $target, $c)
    };
}

macro_rules! kill_target {
    ($target: expr) => {
        edit_target!(EditAction::Delete, $target)
    };
    ($target: expr, $c: expr) => {
        edit_target!(EditAction::Delete, $target, $c)
    };
}

macro_rules! kill {
    ($mt: expr) => {
        edit!(EditAction::Delete, $mt)
    };
    ($mt: expr, $c: expr) => {
        edit!(EditAction::Delete, $mt, $c)
    };
}

macro_rules! just_one_space {
    () => {
        isv!(vec![InternalAction::SetRegister(Register::Blackhole)], vec![
            Action::from(EditorAction::Edit(
                EditAction::Delete.into(),
                RangeType::Word(WordStyle::Whitespace(false)).into()
            ))
            .into(),
            Action::from(InsertTextAction::Type(
                Char::Single(' ').into(),
                MoveDir1D::Previous,
                Count::Contextual
            ))
            .into()
        ])
    };
}

macro_rules! cmdbar_focus {
    ($type: expr, $nm: expr) => {
        cmdbar!(CommandBarAction::Focus($type), $nm)
    };
}

macro_rules! window {
    ($act: expr) => {
        act!(Action::Window($act))
    };
}

macro_rules! window_focus {
    ($fc: expr) => {
        window!(WindowAction::Focus($fc))
    };
}

macro_rules! window_switch {
    ($st: expr) => {
        window!(WindowAction::Switch($st))
    };
}

macro_rules! window_split {
    ($axis: expr) => {
        window!(WindowAction::Split(
            OpenTarget::Current,
            $axis,
            MoveDir1D::Previous,
            Count::Contextual
        ))
    };
}

macro_rules! window_close {
    ($style: expr, $fc: expr, $flags: expr) => {
        window!(WindowAction::Close($style($fc), $flags))
    };
}

macro_rules! window_quit {
    ($style: expr, $f: expr) => {
        window_close!($style, $f, CloseFlags::QUIT)
    };
}

macro_rules! search {
    ($dir: expr) => {
        act!(Action::Search(MoveDirMod::Exact($dir), Count::Exact(1)))
    };
}

macro_rules! search_word {
    ($style: expr) => {
        edit_target!(
            EditAction::Motion,
            EditTarget::Search(SearchType::Word($style, true), MoveDirMod::Same, Count::Exact(1))
        )
    };
}

macro_rules! start_selection {
    ($shape: expr) => {
        is!(
            InternalAction::SetTargetShape($shape, false),
            EditorAction::Selection(SelectionAction::Resize(
                SelectionResizeStyle::Restart,
                EditTarget::CurrentPosition
            ))
        )
    };
    ($shape: expr, $target: expr) => {
        is!(
            InternalAction::SetTargetShape($shape, false),
            EditorAction::Selection(SelectionAction::Resize(
                SelectionResizeStyle::Restart,
                $target
            ))
        )
    };
}

macro_rules! start_shift_selection {
    ($target: expr) => {
        is!(
            InternalAction::SetTargetShape(TargetShape::CharWise, true),
            EditorAction::Edit(Specifier::Exact(EditAction::Motion), $target)
        )
    };
}

#[rustfmt::skip]
fn default_keys<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // Insert, Command and Search mode keybindings.
        ( MAP, "<C-Y>", paste!(MoveDir1D::Previous) ),
        ( MAP, "<M-BS>", kill!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( MAP, "<M-Del>", kill!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( MAP, "<BS>", erase!(MoveType::Column(MoveDir1D::Previous, true)) ),

        // Insert and Command mode keybindings.
        ( ICMAP, "<C-A>", motion!(MoveType::LinePos(MovePosition::Beginning), Count::MinusOne) ),
        ( ICMAP, "<C-B>", motion!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( ICMAP, "<C-D>", erase!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( ICMAP, "<C-E>", motion!(MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( ICMAP, "<C-F>", motion!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( ICMAP, "<C-K>", kill!(MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( ICMAP, "<C-X>z", isv!(vec![], vec![ExternalAction::Repeat(false)]) ),
        ( ICMAP, "<C-W>", kill_target!(EditTarget::Selection) ),
        ( ICMAP, "<C-Left>", motion!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( ICMAP, "<C-Right>", motion!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next)) ),
        ( ICMAP, "<C-S-Left>", start_shift_selection!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous).into()) ),
        ( ICMAP, "<C-S-Right>", start_shift_selection!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next).into()) ),
        ( ICMAP, "<M-b>", motion!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( ICMAP, "<M-B>", start_shift_selection!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous).into()) ),
        ( ICMAP, "<M-c>", edit!(EditAction::ChangeCase(Case::Title), MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( ICMAP, "<M-d>", kill!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( ICMAP, "<M-f>", motion!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next)) ),
        ( ICMAP, "<M-F>", start_shift_selection!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next).into()) ),
        ( ICMAP, "<M-l>", edit!(EditAction::ChangeCase(Case::Lower), MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( ICMAP, "<M-u>", edit!(EditAction::ChangeCase(Case::Upper), MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( ICMAP, "<M-w>", yank_target!(EditTarget::Selection) ),
        ( ICMAP, "<M-W>", yank_target!(EditTarget::Selection) ),
        ( ICMAP, "<M-Left>", motion!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( ICMAP, "<M-Right>", motion!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next)) ),
        ( ICMAP, "<M-S-Left>", start_shift_selection!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous).into()) ),
        ( ICMAP, "<M-S-Right>", start_shift_selection!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next).into()) ),
        ( ICMAP, "<S-End>", start_shift_selection!(MoveType::LinePos(MovePosition::End).into()) ),
        ( ICMAP, "<S-Home>", start_shift_selection!(MoveType::LinePos(MovePosition::Beginning).into()) ),
        ( ICMAP, "<S-Left>", start_shift_selection!(MoveType::Column(MoveDir1D::Previous, true).into()) ),
        ( ICMAP, "<S-Right>", start_shift_selection!(MoveType::Column(MoveDir1D::Next, true).into()) ),
        ( ICMAP, "<Del>", erase!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( ICMAP, "<End>", motion!(MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( ICMAP, "<Home>", motion!(MoveType::LinePos(MovePosition::Beginning), Count::MinusOne) ),
        ( ICMAP, "<Left>", motion!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( ICMAP, "<Right>", motion!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( ICMAP, "z", isv!(vec![], vec![ExternalAction::Repeat(true)]) ),

        // Insert mode keybindings.
        ( IMAP, "<C-G>", is!(InternalAction::ClearTargetShape(false), EditorAction::Edit(EditAction::Motion.into(), EditTarget::CurrentPosition), EmacsMode::Insert) ),
        ( IMAP, "<C-J>", chartype!(Char::Single('\n')) ),
        ( IMAP, "<C-L>", scrollcp!(MovePosition::Middle, Axis::Vertical) ),
        ( IMAP, "<C-N>", motion!(MoveType::Line(MoveDir1D::Next)) ),
        ( IMAP, "<C-O>", insert_text!(InsertTextAction::OpenLine(TargetShape::CharWise, MoveDir1D::Previous, Count::Contextual)) ),
        ( IMAP, "<C-P>", motion!(MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<C-Q>{oct<=3}", chartype!() ),
        ( IMAP, "<C-Q>{any}", chartype!() ),
        ( IMAP, "<C-R>", cmdbar_focus!(CommandType::Search(MoveDir1D::Previous, true), EmacsMode::Search) ),
        ( IMAP, "<C-S>", cmdbar_focus!(CommandType::Search(MoveDir1D::Next, true), EmacsMode::Search) ),
        ( IMAP, "<C-T>", unmapped!() ),
        ( IMAP, "<C-U><C-X>s", unmapped!() ),
        ( IMAP, "<C-V>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( IMAP, "<C-X><C-C>", window!(WindowAction::Close(CloseTarget::All, CloseFlags::QUIT)) ),
        ( IMAP, "<C-X><C-O>", unmapped!() ),
        ( IMAP, "<C-X><C-Q>", unmapped!() ),
        ( IMAP, "<C-X><C-S>", unmapped!() ),
        ( IMAP, "<C-X><C-T>", unmapped!() ),
        ( IMAP, "<C-X><C-W>", unmapped!() ),
        ( IMAP, "<C-X><C-X>", selection!(SelectionAction::CursorSet(SelectionCursorChange::SwapAnchor(false))) ),
        ( IMAP, "<C-X><C-Z>", act!(Action::Suspend) ),
        ( IMAP, "<C-X><Space>", unmapped!() ),
        ( IMAP, "<C-X><Left>", window_switch!(OpenTarget::Offset(MoveDir1D::Previous, Count::Contextual)) ),
        ( IMAP, "<C-X><Right>", window_switch!(OpenTarget::Offset(MoveDir1D::Next, Count::Contextual)) ),
        ( IMAP, "<C-X>b", unmapped!() ),
        ( IMAP, "<C-X>h", start_selection!(TargetShape::CharWise, RangeType::Buffer.into()) ),
        ( IMAP, "<C-X>k", unmapped!() ),
        ( IMAP, "<C-X>o", window_focus!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), true)) ),
        ( IMAP, "<C-X>r<M-w>", unmapped!() ),
        ( IMAP, "<C-X>rb", unmapped!() ),
        ( IMAP, "<C-X>rc", unmapped!() ),
        ( IMAP, "<C-X>rd", unmapped!() ),
        ( IMAP, "<C-X>rk", unmapped!() ),
        ( IMAP, "<C-X>rl", unmapped!() ),
        ( IMAP, "<C-X>rm", unmapped!() ),
        ( IMAP, "<C-X>ro", unmapped!() ),
        ( IMAP, "<C-X>ry", unmapped!() ),
        ( IMAP, "<C-X>rN", unmapped!() ),
        ( IMAP, "<C-X>s", unmapped!() ),
        ( IMAP, "<C-X>u", history!(HistoryAction::Undo(Count::Contextual)) ),
        ( IMAP, "<C-X>0", window_quit!(CloseTarget::Single, FocusChange::Current) ),
        ( IMAP, "<C-X>1", window_quit!(CloseTarget::AllBut, FocusChange::Current) ),
        ( IMAP, "<C-X>2", window_split!(Axis::Horizontal) ),
        ( IMAP, "<C-X>3", window_split!(Axis::Vertical) ),
        ( IMAP, "<C-X><", scroll2d!(MoveDir2D::Left, ScrollSize::Page) ),
        ( IMAP, "<C-X>>", scroll2d!(MoveDir2D::Right, ScrollSize::Page) ),
        ( IMAP, "<C-Z>", act!(Action::Suspend) ),
        ( IMAP, "<C-@>", start_selection!(TargetShape::CharWise) ),
        ( IMAP, "<C-_>", history!(HistoryAction::Undo(Count::Contextual)) ),
        ( IMAP, "<C-Del>", kill!(MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( IMAP, "<C-Space>", start_selection!(TargetShape::CharWise) ),
        ( IMAP, "<M-s>.", search_word!(WordStyle::Big) ),
        ( IMAP, "<M-t>", unmapped!() ),
        ( IMAP, "<M-v>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),
        ( IMAP, "<M-x>", cmdbar_focus!(CommandType::Command, EmacsMode::Command) ),
        ( IMAP, "<M-z>{char}", kill_target!(EditTarget::Search(SearchType::Char(true), MoveDirMod::Same, Count::Contextual)) ),
        ( IMAP, "<M-S>.", search_word!(WordStyle::Big) ),
        ( IMAP, "<M-\\>", erase_range!(RangeType::Word(WordStyle::Whitespace(false))) ),
        ( IMAP, "<M-^>", edit!(EditAction::Join(JoinStyle::OneSpace), MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<M-<>", edit_buffer!(EditAction::Motion, MoveTerminus::Beginning) ),
        ( IMAP, "<M->>", edit_buffer!(EditAction::Motion, MoveTerminus::End) ),
        ( IMAP, "<M-Space>", just_one_space!() ),
        ( IMAP, "<S-Up>", start_shift_selection!(MoveType::Line(MoveDir1D::Previous).into()) ),
        ( IMAP, "<S-Down>", start_shift_selection!(MoveType::Line(MoveDir1D::Next).into()) ),
        ( IMAP, "<Insert>", iact!(InternalAction::SetInsertStyle(InsertStyle::Replace)) ),
        ( IMAP, "<Up>", motion!(MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<Down>", motion!(MoveType::Line(MoveDir1D::Next)) ),
        ( IMAP, "<PageDown>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( IMAP, "<PageUp>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),

        // Command mode keybindings.
        ( CMAP, "<C-G>", prompt!(PromptAction::Abort(false), EmacsMode::Insert) ),
        ( CMAP, "<Up>", prompt!(PromptAction::Recall(MoveDir1D::Previous, Count::Contextual)) ),
        ( CMAP, "<Down>", prompt!(PromptAction::Recall(MoveDir1D::Next, Count::Contextual)) ),

        // Search mode keybindings.
        ( SMAP, "<C-G>", prompt!(PromptAction::Abort(false), EmacsMode::Insert) ),
        ( SMAP, "<C-R>", search!(MoveDir1D::Previous) ),
        ( SMAP, "<C-S>", search!(MoveDir1D::Next) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_pfxs<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, Option<InputStep<I>>)> {
    [
        // Insert and Command mode allow count arguments.
        ( ICMAP, "<C-U>{count*}", Some(iact!(InternalAction::SaveCounting(None))) ),
        ( ICMAP, "<M-1>{count*}", Some(iact!(InternalAction::SaveCounting(1.into()))) ),
        ( ICMAP, "<M-2>{count*}", Some(iact!(InternalAction::SaveCounting(2.into()))) ),
        ( ICMAP, "<M-3>{count*}", Some(iact!(InternalAction::SaveCounting(3.into()))) ),
        ( ICMAP, "<M-4>{count*}", Some(iact!(InternalAction::SaveCounting(4.into()))) ),
        ( ICMAP, "<M-5>{count*}", Some(iact!(InternalAction::SaveCounting(5.into()))) ),
        ( ICMAP, "<M-6>{count*}", Some(iact!(InternalAction::SaveCounting(6.into()))) ),
        ( ICMAP, "<M-7>{count*}", Some(iact!(InternalAction::SaveCounting(7.into()))) ),
        ( ICMAP, "<M-8>{count*}", Some(iact!(InternalAction::SaveCounting(8.into()))) ),
        ( ICMAP, "<M-9>{count*}", Some(iact!(InternalAction::SaveCounting(9.into()))) ),
        ( ICMAP, "<M-0>{count*}", Some(iact!(InternalAction::SaveCounting(0.into()))) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_enter<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // <Enter> in Insert mode types a newline character.
        ( IMAP, "<Enter>", chartype!(Char::Single('\n')) ),

        // <Enter> in Command mode submits the commands.
        ( CMAP, "<Enter>", prompt!(PromptAction::Submit, EmacsMode::Insert) ),
    ].to_vec()
}

#[rustfmt::skip]
fn submit_on_enter<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // <Enter> in Insert and Command mode submits the command.
        ( ICMAP, "<Enter>", prompt!(PromptAction::Submit, EmacsMode::Insert) ),
    ].to_vec()
}

#[inline]
fn add_prefix<I: ApplicationInfo>(
    machine: &mut EmacsMachine<TerminalKey, I>,
    modes: &MappedModes,
    keys: &str,
    action: &Option<InputStep<I>>,
) {
    let (_, evs) = parse(keys).expect(&format!("invalid Emacs keybinding: {}", keys));
    let modes = modes.split();

    for mode in modes {
        machine.add_prefix(mode, &evs, &action);
    }
}

#[inline]
fn add_mapping<I: ApplicationInfo>(
    machine: &mut EmacsMachine<TerminalKey, I>,
    modes: &MappedModes,
    keys: &str,
    action: &InputStep<I>,
) {
    let (_, evs) = parse(keys).expect(&format!("invalid Emacs keybinding: {}", keys));
    let modes = modes.split();

    for mode in modes {
        machine.add_mapping(mode, &evs, &action);
    }
}

/// A configurable collection of Emacs bindings that can be added to a [ModalMachine].
#[derive(Debug)]
pub struct EmacsBindings<I: ApplicationInfo> {
    prefixes: Vec<(MappedModes, &'static str, Option<InputStep<I>>)>,
    mappings: Vec<(MappedModes, &'static str, InputStep<I>)>,
    enter: Vec<(MappedModes, &'static str, InputStep<I>)>,
}

impl<I: ApplicationInfo> EmacsBindings<I> {
    /// Remap the Enter key to [submit](PromptAction::Submit) instead.
    pub fn submit_on_enter(mut self) -> Self {
        self.enter = submit_on_enter();
        self
    }
}

impl<I: ApplicationInfo> ShellBindings for EmacsBindings<I> {
    fn shell(self) -> Self {
        self.submit_on_enter()
    }
}

impl<I: ApplicationInfo> Default for EmacsBindings<I> {
    fn default() -> Self {
        EmacsBindings {
            prefixes: default_pfxs(),
            mappings: default_keys(),
            enter: default_enter(),
        }
    }
}

impl<I: ApplicationInfo> InputBindings<TerminalKey, InputStep<I>> for EmacsBindings<I> {
    fn setup(&self, machine: &mut EmacsMachine<TerminalKey, I>) {
        for (modes, keys, action) in self.prefixes.iter() {
            add_prefix(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.mappings.iter() {
            add_mapping(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.enter.iter() {
            add_mapping(machine, modes, keys, action);
        }
    }
}

/// Manage Emacs keybindings and modes.
pub type EmacsMachine<Key, T = EmptyInfo> = ModalMachine<Key, InputStep<T>>;

impl<I: ApplicationInfo> Default for EmacsMachine<TerminalKey, I> {
    fn default() -> Self {
        ModalMachine::from_bindings::<EmacsBindings<I>>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::bindings::BindingMachine;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    macro_rules! mv {
        ($mt: expr) => {
            Action::from(EditorAction::Edit(
                EditAction::Motion.into(),
                EditTarget::Motion($mt, Count::Contextual),
            ))
        };
        ($mt: expr, $c: literal) => {
            Action::from(EditorAction::Edit(
                EditAction::Motion.into(),
                EditTarget::Motion($mt, Count::Exact($c)),
            ))
        };
        ($mt: expr, $c: expr) => {
            Action::from(EditorAction::Edit(EditAction::Motion.into(), EditTarget::Motion($mt, $c)))
        };
    }

    macro_rules! typechar {
        ($c: literal) => {
            Action::from(EditorAction::InsertText(InsertTextAction::Type(
                Char::Single($c).into(),
                MoveDir1D::Previous,
                Count::Contextual,
            )))
        };
    }

    const CMDBAR_ABORT: Action = Action::Prompt(PromptAction::Abort(false));
    const CMDBAR_SEARCH_NEXT: Action =
        Action::CommandBar(CommandBarAction::Focus(CommandType::Search(MoveDir1D::Next, true)));
    const CMDBAR_SEARCH_PREV: Action =
        Action::CommandBar(CommandBarAction::Focus(CommandType::Search(MoveDir1D::Previous, true)));

    #[test]
    fn test_selection_shift() {
        let mut vm: EmacsMachine<TerminalKey> = EmacsMachine::default();
        let mut ctx = EmacsContext::default();

        // Start out in Insert mode.
        assert_eq!(vm.mode(), EmacsMode::Insert);

        // <S-Left> begins shift selection.
        let mov = mv!(MoveType::Column(MoveDir1D::Previous, true));
        ctx.persist.shift = true;
        ctx.persist.shape = Some(TargetShape::CharWise);

        vm.input_key(key!(KeyCode::Left, KeyModifiers::SHIFT));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), EmacsMode::Insert);

        // <Left> ends shift selection
        let mov = mv!(MoveType::Column(MoveDir1D::Previous, true));
        ctx.persist.shift = false;
        ctx.persist.shape = None;

        vm.input_key(key!(KeyCode::Left));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), EmacsMode::Insert);
    }

    #[test]
    fn test_selection_no_shift() {
        let mut vm: EmacsMachine<TerminalKey> = EmacsMachine::default();
        let mut ctx = EmacsContext::default();

        ctx.persist.shape = Some(TargetShape::CharWise);

        // Start out in Insert mode.
        assert_eq!(vm.mode(), EmacsMode::Insert);

        // <C-Space> begins selection.
        let act =
            SelectionAction::Resize(SelectionResizeStyle::Restart, EditTarget::CurrentPosition);
        vm.input_key(ctl!(' '));
        assert_pop2!(vm, Action::from(act), ctx);

        // <Left> continues to select.
        let mov = mv!(MoveType::Column(MoveDir1D::Previous, true));
        vm.input_key(key!(KeyCode::Left));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), EmacsMode::Insert);
    }

    #[test]
    fn test_search() {
        let mut vm: EmacsMachine<TerminalKey> = EmacsMachine::default();
        let ctx = EmacsContext::default();

        // Start out in Insert mode.
        assert_eq!(vm.mode(), EmacsMode::Insert);

        // ^R moves to Search mode.
        vm.input_key(ctl!('r'));
        assert_pop2!(vm, CMDBAR_SEARCH_PREV, ctx);
        assert_eq!(vm.mode(), EmacsMode::Search);

        // Unmapped, typable character types a character.
        let it =
            InsertTextAction::Type(Char::from('r').into(), MoveDir1D::Previous, Count::Contextual);
        vm.input_key(key!('r'));
        assert_pop2!(vm, Action::from(it), ctx);
        assert_eq!(vm.mode(), EmacsMode::Search);

        // ^R in Search mode results in Action::Search.
        let act = Action::Search(MoveDirMod::Exact(MoveDir1D::Previous), 1.into());
        vm.input_key(ctl!('r'));
        assert_pop2!(vm, act, ctx);
        assert_eq!(vm.mode(), EmacsMode::Search);

        // ^S in Search mode results in Action::Search.
        let act = Action::Search(MoveDirMod::Exact(MoveDir1D::Next), 1.into());
        vm.input_key(ctl!('s'));
        assert_pop2!(vm, act, ctx);
        assert_eq!(vm.mode(), EmacsMode::Search);

        // Unmapped, non-typable character moves back to Insert mode.
        vm.input_key(key!(KeyCode::Left));
        assert_pop2!(vm, CMDBAR_ABORT, ctx);
        assert_eq!(vm.mode(), EmacsMode::Insert);

        // ^S moves to Search mode.
        vm.input_key(ctl!('s'));
        assert_pop2!(vm, CMDBAR_SEARCH_NEXT, ctx);
        assert_eq!(vm.mode(), EmacsMode::Search);

        // ^G leaves Search mode.
        vm.input_key(ctl!('g'));
        assert_pop2!(vm, CMDBAR_ABORT, ctx);
        assert_eq!(vm.mode(), EmacsMode::Insert);
    }

    #[test]
    fn test_count() {
        let mut vm: EmacsMachine<TerminalKey> = EmacsMachine::default();
        let mut ctx = EmacsContext::default();

        // Start out in Insert mode.
        assert_eq!(vm.mode(), EmacsMode::Insert);

        // Typing a number by default types a character.
        vm.input_key(key!('2'));
        assert_pop2!(vm, typechar!('2'), ctx);

        // Adding Alt will make it a count.
        vm.input_key(key!('2', KeyModifiers::ALT));
        assert_eq!(vm.pop(), None);

        // Now we can type w/o Alt and it is still a count.
        vm.input_key(key!('2'));
        assert_eq!(vm.pop(), None);

        // Type space 22 times.
        ctx.action.count = Some(22);
        vm.input_key(key!(' '));
        assert_pop2!(vm, typechar!(' '), ctx);

        // We can also type C-U 2 2 SPC.
        ctx.action.count = Some(22);
        vm.input_key(ctl!('u'));
        vm.input_key(key!('2'));
        vm.input_key(key!('2'));
        vm.input_key(key!(' '));
        assert_pop2!(vm, typechar!(' '), ctx);

        // Typing C-U multiple times multiplies by 4 each time.
        ctx.action.count = Some(64);
        vm.input_key(ctl!('u'));
        vm.input_key(ctl!('u'));
        vm.input_key(ctl!('u'));
        vm.input_key(key!(' '));
        assert_pop2!(vm, typechar!(' '), ctx);
    }

    #[test]
    fn test_repeat_action() {
        let mut vm: EmacsMachine<TerminalKey> = EmacsMachine::default();
        let mut ctx = EmacsContext::default();

        // Start out in Insert mode.
        assert_eq!(vm.mode(), EmacsMode::Insert);

        // Type several characters.
        vm.input_key(key!('a'));
        vm.input_key(key!('b'));
        vm.input_key(key!('c'));
        assert_pop1!(vm, typechar!('a'), ctx);
        assert_pop1!(vm, typechar!('b'), ctx);
        assert_pop2!(vm, typechar!('c'), ctx);

        // Press C-X z to repeat typing 'c'.
        ctx.persist.repeating = true;
        vm.input_key(ctl!('x'));
        vm.input_key(key!('z'));
        assert_pop2!(vm, Action::Repeat(RepeatType::LastAction), ctx);

        // Only 'c' comes back.
        ctx.persist.repeating = false;
        vm.repeat(RepeatType::LastAction, Some(ctx.clone()));
        assert_pop2!(vm, typechar!('c'), ctx);

        // Pressing 'z' keeps repeating.
        ctx.persist.repeating = true;
        vm.input_key(key!('z'));
        assert_pop2!(vm, Action::Repeat(RepeatType::LastAction), ctx);

        // Get back 'c' again.
        ctx.persist.repeating = false;
        vm.repeat(RepeatType::LastAction, Some(ctx.clone()));
        assert_pop2!(vm, typechar!('c'), ctx);

        // Pressing 'z' keeps repeating.
        ctx.persist.repeating = true;
        vm.input_key(key!('z'));
        assert_pop2!(vm, Action::Repeat(RepeatType::LastAction), ctx);

        // Get back 'c' again.
        ctx.persist.repeating = false;
        vm.repeat(RepeatType::LastAction, Some(ctx.clone()));
        assert_pop2!(vm, typechar!('c'), ctx);

        // Pressing 'd', an unmapped key, interrupts the sequence.
        ctx.persist.repeating = false;
        vm.input_key(key!('d'));
        assert_pop2!(vm, typechar!('d'), ctx);

        // Pressing 'z' doesn't repeat anymore.
        ctx.persist.repeating = false;
        vm.input_key(key!('z'));
        assert_pop2!(vm, typechar!('z'), ctx);

        // Press C-X z to repeat typing the 'z' character.
        ctx.persist.repeating = true;
        vm.input_key(ctl!('x'));
        vm.input_key(key!('z'));
        assert_pop2!(vm, Action::Repeat(RepeatType::LastAction), ctx);

        // Get back 'z'.
        ctx.persist.repeating = false;
        vm.repeat(RepeatType::LastAction, Some(ctx.clone()));
        assert_pop2!(vm, typechar!('z'), ctx);

        // Pressing 'z' keeps repeating.
        ctx.persist.repeating = true;
        vm.input_key(key!('z'));
        assert_pop2!(vm, Action::Repeat(RepeatType::LastAction), ctx);

        // Get back 'z'.
        ctx.persist.repeating = false;
        vm.repeat(RepeatType::LastAction, Some(ctx.clone()));
        assert_pop2!(vm, typechar!('z'), ctx);

        // Pressing <Right>, a mapped key, interrupts the sequence.
        ctx.persist.repeating = false;
        vm.input_key(key!(KeyCode::Right));
        assert_pop2!(vm, mv!(MoveType::Column(MoveDir1D::Next, true)), ctx);

        // Pressing 'z' doesn't repeat anymore.
        ctx.persist.repeating = false;
        vm.input_key(key!('z'));
        assert_pop2!(vm, typechar!('z'), ctx);
    }
}
