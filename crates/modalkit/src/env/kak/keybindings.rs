//! # Kakoune Keybindings (WIP)
//!
//! ## Overview
//!
//! This module handles mapping the keybindings used in Kakoune onto the
//! [Action] type.
//!
//! NOTE: A lot of this is still a work in progress, and some things
//! might not work the way you expect yet!
//!
//! ## Example
//!
//! ```
//! use modalkit::env::kak::KakouneMode;
//! use modalkit::env::kak::keybindings::{default_kakoune_keys, KakouneMachine};
//!
//! use modalkit::actions::{Action, EditAction, EditorAction, SelectionAction};
//! use modalkit::keybindings::BindingMachine;
//! use modalkit::key::TerminalKey;
//! use modalkit::prelude::*;
//!
//! use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
//!
//! const fn key(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
//!     KeyEvent::new(code, modifiers)
//! }
//!
//! fn main() {
//!     let mut keybindings: KakouneMachine<TerminalKey> = default_kakoune_keys();
//!
//!     // Begins in Normal mode.
//!     assert_eq!(keybindings.mode(), KakouneMode::Normal);
//!
//!     // Typing "i" enters Insert mode:
//!     keybindings.input_key(key(KeyCode::Char('i'), KeyModifiers::NONE).into());
//!     assert_eq!(keybindings.mode(), KakouneMode::Insert);
//!
//!     // Pop action produced by typing "i" to set cursor position for insert.
//!     let (act, _) = keybindings.pop().unwrap();
//!     let exp = SelectionAction::CursorSet(SelectionCursorChange::Beginning);
//!     assert_eq!(act, EditorAction::Selection(exp).into());
//!
//!     // End of available actions.
//!     assert_eq!(keybindings.pop(), None);
//! }
//! ```
use bitflags::bitflags;

use editor_types::{
    action,
    application::{ApplicationInfo, EmptyInfo},
    Action,
    EditAction,
    EditorAction,
    SelectionAction,
};

use crate::{
    env::{keyparse::parse, CommonKeyClass, ShellBindings},
    key::TerminalKey,
    keybindings::{InputBindings, ModalMachine, Step},
    prelude::*,
};

use super::{KakouneMode, KakouneState, ObjectPosition};

bitflags! {
    #[derive(Debug, Clone, Copy)]
    struct MappedModes: u32 {
        const N = 0b0000000000000001;
        const I = 0b0000000000000010;
        const P = 0b0000000000000100;
        const V = 0b0000000000001000;
        const O = 0b0000000000010000;
        const G = 0b0000000000100000;

        const NIP = Self::N.bits() | Self::I.bits() | Self::P.bits();
        const IP = Self::I.bits() | Self::P.bits();
    }
}

const MAP: MappedModes = MappedModes::NIP;
const NMAP: MappedModes = MappedModes::N;
const IMAP: MappedModes = MappedModes::I;
const PMAP: MappedModes = MappedModes::P;
const VMAP: MappedModes = MappedModes::V;
const OMAP: MappedModes = MappedModes::O;
const GMAP: MappedModes = MappedModes::G;
const IPMAP: MappedModes = MappedModes::IP;

impl MappedModes {
    pub fn split(&self) -> Vec<KakouneMode> {
        let mut modes = Vec::new();

        if self.contains(MappedModes::N) {
            modes.push(KakouneMode::Normal);
        }

        if self.contains(MappedModes::I) {
            modes.push(KakouneMode::Insert);
        }

        if self.contains(MappedModes::P) {
            modes.push(KakouneMode::Prompt);
        }

        if self.contains(MappedModes::V) {
            modes.push(KakouneMode::View);
        }

        if self.contains(MappedModes::O) {
            modes.push(KakouneMode::ObjectSelect);
        }

        if self.contains(MappedModes::G) {
            modes.push(KakouneMode::Goto);
        }

        return modes;
    }
}

#[derive(Clone, Debug)]
enum InternalAction {
    SetCursorChar(char),
    SetCursorEnd(CursorEnd),
    SetObjectSelect(SelectionResizeStyle, ObjectPosition, bool),
    SetRegister(Register),
    SetSearchChar,
    SetSearchCharParams(MoveDir1D, bool),
    SetTargetShape(TargetShape),
}

impl InternalAction {
    pub fn run<I: ApplicationInfo>(&self, ctx: &mut KakouneState<I>) {
        match self {
            InternalAction::SetCursorChar(c) => {
                ctx.action.cursor = Some(*c);
            },
            InternalAction::SetCursorEnd(end) => {
                ctx.action.cursor_end = *end;
            },
            InternalAction::SetObjectSelect(style, pos, inc) => {
                ctx.action.objsel = Some((*style, *pos, *inc));
            },
            InternalAction::SetRegister(reg) => {
                ctx.action.register = Some(reg.clone());
            },
            InternalAction::SetSearchChar => {
                ctx.persist.charsearch = ctx.ch.get_typed();
            },
            InternalAction::SetSearchCharParams(dir, inclusive) => {
                ctx.persist.charsearch_params = (*dir, *inclusive);
            },
            InternalAction::SetTargetShape(shape) => {
                ctx.action.shape = Some(*shape);
            },
        }
    }
}

#[derive(Debug)]
enum ExternalAction<I: ApplicationInfo> {
    CountOnly(Action<I>),
    ObjectSelect(RangeType, Option<RangeType>),
    Something(Action<I>),
}

impl<I: ApplicationInfo> ExternalAction<I> {
    fn resolve(&self, ctx: &mut KakouneState<I>) -> Vec<Action<I>> {
        match self {
            ExternalAction::CountOnly(act) => {
                if ctx.action.count.is_some() {
                    vec![act.clone()]
                } else {
                    vec![]
                }
            },
            ExternalAction::ObjectSelect(rt1, rt2) => {
                if let Some((style, pos, inclusive)) = ctx.action.objsel {
                    let rt = match (rt2, inclusive) {
                        (Some(rt), true) => rt.clone(),
                        (_, _) => rt1.clone(),
                    };

                    let target = match pos {
                        ObjectPosition::Whole => {
                            EditTarget::Range(rt, inclusive, Count::Contextual)
                        },
                        ObjectPosition::Beginning => {
                            EditTarget::Boundary(
                                rt,
                                inclusive,
                                MoveTerminus::Beginning,
                                Count::Contextual,
                            )
                        },
                        ObjectPosition::End => {
                            EditTarget::Boundary(
                                rt,
                                inclusive,
                                MoveTerminus::End,
                                Count::Contextual,
                            )
                        },
                    };

                    vec![action!("selection resize -s {style} -t {target}")]
                } else {
                    vec![]
                }
            },
            ExternalAction::Something(act) => vec![act.clone()],
        }
    }
}

impl<I: ApplicationInfo> Clone for ExternalAction<I> {
    fn clone(&self) -> Self {
        match self {
            ExternalAction::CountOnly(act) => ExternalAction::CountOnly(act.clone()),
            ExternalAction::ObjectSelect(range1, range2) => {
                ExternalAction::ObjectSelect(range1.clone(), range2.clone())
            },
            ExternalAction::Something(act) => ExternalAction::Something(act.clone()),
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
    fallthrough_mode: Option<KakouneMode>,
    nextm: Option<KakouneMode>,
}

impl<I: ApplicationInfo> InputStep<I> {
    /// Create a new step that input keys can map to.
    pub fn new() -> Self {
        InputStep {
            internal: vec![],
            external: vec![],
            fallthrough_mode: None,
            nextm: None,
        }
    }

    /// Set the [actions](Action) that this step produces.
    pub fn actions(mut self, acts: Vec<Action<I>>) -> Self {
        self.external = acts.into_iter().map(ExternalAction::Something).collect();
        self
    }
}

impl<I: ApplicationInfo> Default for InputStep<I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: ApplicationInfo> Clone for InputStep<I> {
    fn clone(&self) -> Self {
        Self {
            internal: self.internal.clone(),
            external: self.external.clone(),
            fallthrough_mode: self.fallthrough_mode,
            nextm: self.nextm,
        }
    }
}

impl<I: ApplicationInfo> Step<TerminalKey> for InputStep<I> {
    type A = Action<I>;
    type State = KakouneState<I>;
    type M = KakouneMode;
    type Class = CommonKeyClass;
    type Sequence = RepeatType;

    fn is_unmapped(&self) -> bool {
        match self {
            InputStep {
                internal,
                external,
                fallthrough_mode: None,
                nextm: None,
            } => internal.is_empty() && external.is_empty(),
            _ => false,
        }
    }

    fn fallthrough(&self) -> Option<Self::M> {
        self.fallthrough_mode
    }

    fn step(&self, ctx: &mut KakouneState<I>) -> (Vec<Action<I>>, Option<Self::M>) {
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

macro_rules! action_step {
    ($cmd: expr) => {
        act!(action!($cmd))
    };
    ($cmd: expr, $ns: expr) => {
        act!(action!($cmd), $ns)
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
        InputStep {
            internal: vec![],
            external: vec![],
            fallthrough_mode: None,
            nextm: None,
        }
    };
    ($ints: expr, $exts: expr) => {
        InputStep {
            internal: $ints,
            external: $exts,
            fallthrough_mode: None,
            nextm: None,
        }
    };
    ($ints: expr, $exts: expr, $ns: expr) => {
        InputStep {
            internal: $ints,
            external: $exts,
            fallthrough_mode: None,
            nextm: Some($ns),
        }
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

macro_rules! fallthrough {
    ($mode: expr) => {
        InputStep {
            internal: vec![],
            external: vec![],
            fallthrough_mode: Some($mode),

            nextm: None,
        }
    };
    ($mode: expr, $iacts: expr) => {
        InputStep {
            internal: $iacts,
            external: vec![],
            fallthrough_mode: Some($mode),

            nextm: None,
        }
    };
    ($mode: expr, $iacts: expr, $eacts: expr) => {
        InputStep {
            internal: $iacts,
            external: $eacts,
            fallthrough_mode: Some($mode),

            nextm: None,
        }
    };
}

macro_rules! goto_goto {
    ($iacts: expr) => {
        fallthrough!(KakouneMode::Goto, $iacts, vec![ExternalAction::CountOnly(
            EditorAction::Edit(
                Specifier::Exact(EditAction::Motion),
                MoveType::BufferLineOffset.into()
            )
            .into()
        )])
    };
}

macro_rules! shaped {
    ($shape: expr, $act: expr) => {
        is!(InternalAction::SetTargetShape($shape), $act)
    };
    ($shape: expr, $act: expr, $nm: expr) => {
        is!(InternalAction::SetTargetShape($shape), $act, $nm)
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

macro_rules! insert {
    ($mt: expr, $c: literal) => {
        edit!(EditAction::Motion, $mt, Count::Exact(0), KakouneMode::Insert)
    };
    ($mt: expr, $c: expr) => {
        edit!(EditAction::Motion, $mt, $c, KakouneMode::Insert)
    };
}

macro_rules! edit_target_cursor_end {
    ($ea: expr, $et: expr, $end: expr) => {
        is!(InternalAction::SetCursorEnd($end), EditorAction::Edit(Specifier::Exact($ea), $et))
    };
    ($ea: expr, $et: expr, $end: expr, $mode: expr) => {
        is!(
            InternalAction::SetCursorEnd($end),
            EditorAction::Edit(Specifier::Exact($ea), $et),
            $mode
        )
    };
}

macro_rules! edit_target_cursor_keep {
    ($ea: expr, $et: expr) => {
        edit_target_cursor_end!($ea, $et, CursorEnd::Keep)
    };
    ($ea: expr, $et: expr, $mode: expr) => {
        edit_target_cursor_end!($ea, $et, CursorEnd::Keep, $mode)
    };
}

macro_rules! edit_selection_cursor_keep {
    ($ea: expr) => {
        edit_target_cursor_keep!($ea, EditTarget::Selection)
    };
}

macro_rules! action_selection {
    ($cmd: expr) => {
        is!(InternalAction::SetCursorEnd(CursorEnd::Selection), action!($cmd))
    };
}

macro_rules! action_keep {
    ($cmd: expr) => {
        is!(InternalAction::SetCursorEnd(CursorEnd::Keep), action!($cmd))
    };
    ($cmd: expr, $nm: expr) => {
        is!(InternalAction::SetCursorEnd(CursorEnd::Keep), action!($cmd), $nm)
    };
}

macro_rules! open_lines {
    ($dir: expr) => {
        isv!(
            vec![],
            vec![
                ExternalAction::Something(action!("cursor split -c ctx-sub-one")),
                ExternalAction::Something(action!("insert open-line -S line -d {} -c 1", $dir))
            ],
            KakouneMode::Insert
        )
    };
}

macro_rules! extend_target {
    ($et: expr) => {
        selection_resize!(SelectionResizeStyle::Extend, $et)
    };
}

macro_rules! extend {
    ($mt: expr) => {
        extend_target!(EditTarget::Motion($mt, Count::Contextual))
    };
    ($mt: expr, $c: literal) => {
        extend_target!(EditTarget::Motion($mt, Count::Exact($c)))
    };
    ($mt: expr, $c: expr) => {
        extend_target!(EditTarget::Motion($mt, $c))
    };
}

macro_rules! extend_search {
    ($st: expr, $mod: expr) => {
        is!(
            InternalAction::SetSearchChar,
            EditorAction::Selection(SelectionAction::Resize(
                SelectionResizeStyle::Extend,
                EditTarget::Search($st, $mod, Count::Contextual)
            ))
        )
    };
    ($st: expr, $mod: expr, $c: literal) => {
        is!(
            InternalAction::SetSearchChar,
            EditorAction::Selection(SelectionAction::Resize(
                SelectionResizeStyle::Extend,
                EditTarget::Search($st, $mod, Count::Exact($c))
            ))
        )
    };
    ($st: expr, $mod: expr, $c: expr) => {
        is!(
            InternalAction::SetSearchChar,
            EditorAction::Selection(SelectionAction::Resize(
                SelectionResizeStyle::Extend,
                EditTarget::Search($st, $mod, $c)
            ))
        )
    };
}

macro_rules! selection_resize {
    ($style: expr, $et: expr) => {
        shaped!(
            TargetShape::CharWise,
            EditorAction::Selection(SelectionAction::Resize($style, $et))
        )
    };
}

macro_rules! selection_restart_target {
    ($et: expr) => {
        selection_resize!(SelectionResizeStyle::Restart, $et)
    };
}

macro_rules! selection_restart {
    ($mt: expr) => {
        selection_restart_target!(EditTarget::Motion($mt, Count::Contextual))
    };
    ($mt: expr, $c: literal) => {
        selection_restart_target!(EditTarget::Motion($mt, Count::Exact($c)))
    };
}

macro_rules! selection_restart_search {
    ($st: expr, $mod: expr) => {
        is!(
            InternalAction::SetSearchChar,
            EditorAction::Selection(SelectionAction::Resize(
                SelectionResizeStyle::Restart,
                EditTarget::Search($st, $mod, Count::Contextual)
            ))
        )
    };
}

macro_rules! selection_object_search {
    ($dir: expr) => {
        selection_resize!(
            SelectionResizeStyle::Object,
            EditTarget::Search(SearchType::Regex, MoveDirMod::Exact($dir), Count::Contextual)
        )
    };
}

macro_rules! object_select {
    ($style: expr, $pos: expr, $inc: expr) => {
        fallthrough!(KakouneMode::ObjectSelect, vec![
            InternalAction::SetTargetShape(TargetShape::CharWise),
            InternalAction::SetObjectSelect($style, $pos, $inc)
        ])
    };
}

macro_rules! object_end {
    ($rt: expr) => {
        isv!(vec![], vec![ExternalAction::ObjectSelect($rt, None)], KakouneMode::Normal)
    };
    ($rt1: expr, $rt2: expr) => {
        isv!(vec![], vec![ExternalAction::ObjectSelect($rt1, Some($rt2))], KakouneMode::Normal)
    };
}

macro_rules! object_whitespace_end {
    () => {
        object_end!(
            RangeType::Word(WordStyle::Whitespace(false)),
            RangeType::Word(WordStyle::Whitespace(true))
        )
    };
}

macro_rules! delete_selection {
    () => {
        editor!(EditorAction::Edit(Specifier::Exact(EditAction::Delete), EditTarget::Selection))
    };
    ($nm: expr) => {
        editor!(
            EditorAction::Edit(Specifier::Exact(EditAction::Delete), EditTarget::Selection),
            $nm
        )
    };
    ($nm: expr, $register: expr) => {
        is!(
            InternalAction::SetRegister($register),
            EditorAction::Edit(Specifier::Exact(EditAction::Delete), EditTarget::Selection),
            $nm
        )
    };
}

#[rustfmt::skip]
fn default_keys<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // Normal, Insert, and Command mode keys.
        ( MAP, "<Left>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Previous, true)) ),
        ( MAP, "<Right>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Next, true)) ),

        // Normal mode keys
        ( NMAP, "<A-a>", object_select!(SelectionResizeStyle::Object, ObjectPosition::Whole, true) ),
        ( NMAP, "<A-b>", selection_restart!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous)) ),
        ( NMAP, "<A-B>", extend!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous)) ),
        ( NMAP, "<A-c>", delete_selection!(KakouneMode::Insert, Register::Blackhole) ),
        ( NMAP, "<A-C>", action_step!("selection duplicate -d previous -c ctx") ),
        ( NMAP, "<A-d>", delete_selection!(KakouneMode::Normal, Register::Blackhole) ),
        ( NMAP, "<A-e>", selection_restart!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next)) ),
        ( NMAP, "<A-E>", extend!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next)) ),
        ( NMAP, "<A-f>", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Previous, true)) ),
        ( NMAP, "<A-f>{any}", selection_restart_search!(SearchType::Char(true), MoveDirMod::Same) ),
        ( NMAP, "<A-F>", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Previous, true)) ),
        ( NMAP, "<A-F>{any}", extend_search!(SearchType::Char(true), MoveDirMod::Same, Count::Contextual) ),
        ( NMAP, "<A-h>", selection_restart!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( NMAP, "<A-H>", extend!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( NMAP, "<A-i>", object_select!(SelectionResizeStyle::Object, ObjectPosition::Whole, false) ),
        ( NMAP, "<A-j>", edit_selection_cursor_keep!(EditAction::Join(JoinStyle::NewSpace)) ),
        ( NMAP, "<A-J>", edit_selection!(EditAction::Join(JoinStyle::NewSpace)) ),
        ( NMAP, "<A-l>", selection_restart!(MoveType::LinePos(MovePosition::End), 0) ),
        ( NMAP, "<A-L>", extend!(MoveType::LinePos(MovePosition::End), 0) ),
        ( NMAP, "<A-k>", action_step!(r#"cmdbar focus -p "keep matching:" -s search -a (selection filter -F keep)"#, KakouneMode::Prompt) ),
        ( NMAP, "<A-K>", action_step!(r#"cmdbar focus -p "keep not matching:" -s search -a (selection filter -F drop)"#, KakouneMode::Prompt) ),
        ( NMAP, "<A-o>", action_keep!("insert open-line -S line -d next -c ctx", KakouneMode::Normal) ),
        ( NMAP, "<A-O>", action_keep!("insert open-line -S line -d prev -c ctx", KakouneMode::Normal) ),
        ( NMAP, "<A-p>", unmapped!() ),
        ( NMAP, "<A-P>", unmapped!() ),
        ( NMAP, "<A-R>", unmapped!() ),
        ( NMAP, "<A-s>", action_step!("selection split -s lines") ),
        ( NMAP, "<A-S>", action_step!("selection split -s anchor") ),
        ( NMAP, "<A-t>", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Previous, false)) ),
        ( NMAP, "<A-t>{any}", selection_restart_search!(SearchType::Char(true), MoveDirMod::Same) ),
        ( NMAP, "<A-T>", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Previous, false)) ),
        ( NMAP, "<A-T>{any}", extend_search!(SearchType::Char(true), MoveDirMod::Same, Count::Contextual) ),
        ( NMAP, "<A-u>", action_step!("history undo -c ctx", Default::default()) ),
        ( NMAP, "<A-U>", action_step!("history redo -c ctx", Default::default()) ),
        ( NMAP, "<A-w>", selection_restart!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next)) ),
        ( NMAP, "<A-W>", extend!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next)) ),
        ( NMAP, "<A-x>", action_step!("selection trim -b line -t all") ),
        ( NMAP, "<A-X>", action_step!("selection expand -b line -t all") ),
        ( NMAP, "<A-z>a", action_step!("cursor save -s append") ),
        ( NMAP, "<A-z>u", action_step!("cursor save -s (merge union)") ),
        ( NMAP, "<A-z>i", action_step!("cursor save -s (merge intersect)") ),
        ( NMAP, "<A-z><", action_step!("cursor save -s (merge select-cursor -d previous)") ),
        ( NMAP, "<A-z>>", action_step!("cursor save -s (merge select-cursor -d next)") ),
        ( NMAP, "<A-z>+", action_step!("cursor save -s (merge select-long)") ),
        ( NMAP, "<A-z>-", action_step!("cursor save -s (merge select-short)") ),
        ( NMAP, "<A-Z>a", action_step!("cursor restore -s append") ),
        ( NMAP, "<A-Z>u", action_step!("cursor restore -s (merge union)") ),
        ( NMAP, "<A-Z>i", action_step!("cursor restore -s (merge intersect)") ),
        ( NMAP, "<A-Z><", action_step!("cursor restore -s (merge select-cursor -d previous)") ),
        ( NMAP, "<A-Z>>", action_step!("cursor restore -s (merge select-cursor -d next)") ),
        ( NMAP, "<A-Z>+", action_step!("cursor restore -s (merge select-long)") ),
        ( NMAP, "<A-Z>-", action_step!("cursor restore -s (merge select-short)") ),
        ( NMAP, "<A-[>", object_select!(SelectionResizeStyle::Restart, ObjectPosition::Beginning, false) ),
        ( NMAP, "<A-]>", object_select!(SelectionResizeStyle::Restart, ObjectPosition::End, false) ),
        ( NMAP, "<A-{>", object_select!(SelectionResizeStyle::Extend, ObjectPosition::Beginning, false) ),
        ( NMAP, "<A-}>", object_select!(SelectionResizeStyle::Extend, ObjectPosition::End, false) ),
        ( NMAP, "<A-_>", action_step!("selection join") ),
        ( NMAP, "<A-&>", unmapped!() ),
        ( NMAP, "<A-@>", unmapped!() ),
        ( NMAP, "<A-(>", unmapped!() ),
        ( NMAP, "<A-)>", unmapped!() ),
        ( NMAP, "<A-|>", unmapped!() ),
        ( NMAP, "<A-!>", unmapped!() ),
        ( NMAP, "<A-`>", edit_selection!(EditAction::ChangeCase(Case::Toggle)) ),
        ( NMAP, "<A-/>", action_step!(r#"cmdbar focus -p "reverse search:" -s search -a (search -d (exact previous))"#, KakouneMode::Prompt) ),
        ( NMAP, "<A-?>", action_step!(r#"cmdbar focus -p "reverse search (extend):" -s search -a (search -d (exact previous))"#, KakouneMode::Prompt) ),
        ( NMAP, "<A-*>", unmapped!() ),
        ( NMAP, "<A-.>", action_step!("repeat -s last-selection") ),
        ( NMAP, "<A-;>", action_step!("selection cursor-set -f swap-anchor") ),
        ( NMAP, "<A-:>", action_step!("selection cursor-set -f end") ),
        ( NMAP, "<A-,>", action_step!("cursor close -t leader") ),
        ( NMAP, "<C-B>", action_step!("scroll -s (dir2d -d up -z half-page)") ),
        ( NMAP, "<C-D>", action_step!("scroll -s (dir2d -d down -z page)") ),
        ( NMAP, "<C-F>", action_step!("scroll -s (dir2d -d down -z half-page)") ),
        ( NMAP, "<C-U>", action_step!("scroll -s (dir2d -d up -z page)") ),
        ( NMAP, "<C-I>", action_step!("jump -t jump-list -d next -c ctx") ),
        ( NMAP, "<C-O>", action_step!("jump -t jump-list -d prev -c ctx") ),
        ( NMAP, "<C-S>", unmapped!() ),
        ( NMAP, "a", action_step!("selection cursor-set -f end", KakouneMode::Insert) ),
        ( NMAP, "A", insert!(MoveType::LinePos(MovePosition::End), 0) ),
        ( NMAP, "b", selection_restart!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( NMAP, "B", extend!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( NMAP, "c", delete_selection!(KakouneMode::Insert) ),
        ( NMAP, "C", action_step!("selection duplicate -d next -c ctx") ),
        ( NMAP, "d", delete_selection!() ),
        ( NMAP, "e", selection_restart!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( NMAP, "E", extend!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( NMAP, "f", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Next, true)) ),
        ( NMAP, "f{any}", selection_restart_search!(SearchType::Char(true), MoveDirMod::Same) ),
        ( NMAP, "F", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Next, true)) ),
        ( NMAP, "F{any}", extend_search!(SearchType::Char(true), MoveDirMod::Same, Count::Contextual) ),
        ( NMAP, "g", goto_goto!(vec![]) ),
        ( NMAP, "G", goto_goto!(vec![InternalAction::SetTargetShape(TargetShape::CharWise)]) ),
        ( NMAP, "h", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Previous, true)) ),
        ( NMAP, "H", extend!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( NMAP, "i", action_step!("selection cursor-set -f beginning", KakouneMode::Insert) ),
        ( NMAP, "I", insert!(MoveType::FirstWord(MoveDir1D::Next), 0) ),
        ( NMAP, "j", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Next)) ),
        ( NMAP, "J", extend!(MoveType::Line(MoveDir1D::Next)) ),
        ( NMAP, "k", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Previous)) ),
        ( NMAP, "K", extend!(MoveType::Line(MoveDir1D::Previous)) ),
        ( NMAP, "l", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Next, true)) ),
        ( NMAP, "L", extend!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( NMAP, "m", selection_resize!(SelectionResizeStyle::Object, RangeType::Item.into()) ),
        ( NMAP, "M", extend_target!(RangeType::Item.into()) ),
        ( NMAP, "o", open_lines!(MoveDir1D::Next) ),
        ( NMAP, "O", open_lines!(MoveDir1D::Previous) ),
        ( NMAP, "p", action_selection!("insert paste -s (side -d next)") ),
        ( NMAP, "P", action_selection!("insert paste -s (side -d previous)") ),
        ( NMAP, "q", action_step!("macro execute -c ctx") ),
        ( NMAP, "Q", action_step!("macro toggle-recording") ),
        ( NMAP, "r{any}", edit_selection!(EditAction::Replace(false)) ),
        ( NMAP, "R", action_selection!("insert paste -s replace") ),
        ( NMAP, "s", action_step!(r#"cmdbar focus -p "select:" -s search -a (selection split -s (regex keep) -F all)"#, KakouneMode::Prompt) ),
        ( NMAP, "S", action_step!(r#"cmdbar focus -p "split:" -s search -a (selection split -s (regex drop) -F all)"#, KakouneMode::Prompt) ),
        ( NMAP, "t", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Next, false)) ),
        ( NMAP, "t{any}", selection_restart_search!(SearchType::Char(true), MoveDirMod::Same) ),
        ( NMAP, "T", iact!(InternalAction::SetSearchCharParams(MoveDir1D::Next, false)) ),
        ( NMAP, "T{any}", extend_search!(SearchType::Char(true), MoveDirMod::Same, Count::Contextual) ),
        ( NMAP, "u", action_step!("history undo -c ctx", Default::default()) ),
        ( NMAP, "U", action_step!("history redo -c ctx", Default::default()) ),
        ( NMAP, "v", fallthrough!(KakouneMode::View) ),
        ( NMAP, "V", goto!(KakouneMode::View) ),
        ( NMAP, "w", selection_restart!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next)) ),
        ( NMAP, "W", extend!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next)) ),
        ( NMAP, "x", selection_resize!(SelectionResizeStyle::Object, RangeType::Line.into()) ),
        ( NMAP, "X", extend_target!(RangeType::Line.into()) ),
        ( NMAP, "y", edit_selection!(EditAction::Yank) ),
        ( NMAP, "z", action_step!("cursor save -s replace") ),
        ( NMAP, "Z", action_step!("cursor restore -s replace") ),
        ( NMAP, "<", edit_selection!(EditAction::Indent(IndentChange::Decrease(Count::Contextual))) ),
        ( NMAP, ">", edit_selection!(EditAction::Indent(IndentChange::Increase(Count::Contextual))) ),
        ( NMAP, "[", object_select!(SelectionResizeStyle::Restart, ObjectPosition::Beginning, true) ),
        ( NMAP, "]", object_select!(SelectionResizeStyle::Restart, ObjectPosition::End, true) ),
        ( NMAP, "{", object_select!(SelectionResizeStyle::Extend, ObjectPosition::Beginning, true) ),
        ( NMAP, "}", object_select!(SelectionResizeStyle::Extend, ObjectPosition::End, true) ),
        ( NMAP, ",", action_step!("cursor close -t followers") ),
        ( NMAP, ".", action_step!("repeat -s edit-sequence") ),
        ( NMAP, "%", selection_restart_target!(RangeType::Buffer.into()) ),
        ( NMAP, "&", unmapped!() ),
        ( NMAP, "|", unmapped!() ),
        ( NMAP, "!", unmapped!() ),
        ( NMAP, "`", edit_selection!(EditAction::ChangeCase(Case::Lower)) ),
        ( NMAP, "~", edit_selection!(EditAction::ChangeCase(Case::Upper)) ),
        ( NMAP, ";", selection_restart_target!(EditTarget::CurrentPosition) ),
        ( NMAP, "@", unmapped!() ),
        ( NMAP, "_", action_step!("selection trim -b non-whitespace -t all") ),
        ( NMAP, "/", action_step!(r#"cmdbar focus -p "search:" -s search -a (search -d (exact next))"#, KakouneMode::Prompt) ),
        ( NMAP, "?", action_step!(r#"cmdbar focus -p "search (extend):" -s search -a (search -d (exact next))"#, KakouneMode::Prompt) ),
        ( NMAP, "*", unmapped!() ),
        ( NMAP, "$", unmapped!() ),
        ( NMAP, ":", action_step!(r#"cmdbar focus -p ":" -s command -a (command execute -c 1)"#, KakouneMode::Prompt) ),
        ( NMAP, ")", action_step!("cursor rotate -d next -c ctx") ),
        ( NMAP, "(", action_step!("cursor rotate -d previous -c ctx") ),
        ( NMAP, "<Space>", fallthrough!(KakouneMode::User) ),
        ( NMAP, "<Home>", selection_restart!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( NMAP, "<End>", selection_restart!(MoveType::LinePos(MovePosition::End), 0) ),
        ( NMAP, "<PageDown>", action_step!("scroll -s (dir2d -d down -z half-page)") ),
        ( NMAP, "<PageUp>", action_step!("scroll -s (dir2d -d up -z half-page)") ),
        ( NMAP, "<Down>", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Next)) ),
        ( NMAP, "<Up>", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Previous)) ),
        ( NMAP, "<S-Left>", extend!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( NMAP, "<S-Right>", extend!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( NMAP, "<S-Down>", extend!(MoveType::Line(MoveDir1D::Next)) ),
        ( NMAP, "<S-Up>", extend!(MoveType::Line(MoveDir1D::Previous)) ),
        ( NMAP, "<S-Home>", extend!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( NMAP, "<S-End>", extend!(MoveType::LinePos(MovePosition::End), 0) ),

        // Insert and Command mode keys.
        ( IPMAP, "<C-R>", iact!(InternalAction::SetCursorChar('"')) ),
        ( IPMAP, "<C-R>{register}", action_step!("insert paste -s cursor") ),
        ( IPMAP, "<Home>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( IPMAP, "<End>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::End), 0) ),
        ( IPMAP, "<BS>", erase!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( IPMAP, "<Del>", erase!(MoveType::Column(MoveDir1D::Next, true)) ),

        // Insert mode keys
        ( IMAP, "<A-;>", fallthrough!(KakouneMode::Normal) ),
        ( IMAP, "<C-N>", action_step!("complete -s (list -d next --toggle true) -T auto -D list") ),
        ( IMAP, "<C-O>", unmapped!() ),
        ( IMAP, "<C-P>", action_step!("complete -s (list -d prev --toggle true) -T auto -D list") ),
        ( IMAP, "<C-U>", action_step!("history checkpoint", Default::default()) ),
        ( IMAP, "<C-V>", iact!(InternalAction::SetCursorChar('^')) ),
        ( IMAP, "<C-V>{any}", action_step!("insert type -i ctx -d prev -c 1") ),
        ( IMAP, "<C-X>f", action_step!("complete -s none -T file -D list") ),
        ( IMAP, "<C-X>w", action_step!("complete -s none -T (word buffer) -D list") ),
        ( IMAP, "<C-X>W", action_step!("complete -s none -T (word global) -D list") ),
        ( IMAP, "<C-X>l", action_step!("complete -s none -T (line buffer) -D list") ),
        ( IMAP, "<C-X>L", action_step!("complete -s none -T (line global) -D list") ),
        ( IMAP, "<Esc>", goto!(KakouneMode::Normal) ),
        ( IMAP, "<Up>", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<Down>", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Next)) ),

        ( PMAP, "<A-b>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( PMAP, "<A-B>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous)) ),
        ( PMAP, "<A-e>", edit!(EditAction::Motion, MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( PMAP, "<A-E>", edit!(EditAction::Motion, MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next)) ),
        ( PMAP, "<A-f>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next)) ),
        ( PMAP, "<A-F>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next)) ),
        ( PMAP, "<A-;>", fallthrough!(KakouneMode::Normal) ),
        ( PMAP, "<C-A>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( PMAP, "<C-B>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Previous, false)) ),
        ( PMAP, "<C-D>", erase!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( PMAP, "<C-E>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::End), 0) ),
        ( PMAP, "<C-F>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Next, false)) ),
        ( PMAP, "<C-H>", erase!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( PMAP, "<C-K>", erase!(MoveType::LinePos(MovePosition::End), 0) ),
        ( PMAP, "<C-N>", action_step!("prompt recall -d next -c ctx -F all") ),
        ( PMAP, "<C-P>", action_step!("prompt recall -d previous -c ctx -F all") ),
        ( PMAP, "<C-U>", erase!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( PMAP, "<C-V>{any}", action_step!("insert type -i ctx -d prev -c 1") ),
        ( PMAP, "<C-W>", erase!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( PMAP, "<C-Y>", action_step!("insert paste -s cursor") ),
        ( PMAP, "<Esc>", action_step!("prompt abort", KakouneMode::Normal) ),
        ( PMAP, "<Up>", action_step!("prompt recall -d previous -c ctx -F all") ),
        ( PMAP, "<Down>", action_step!("prompt recall -d next -c ctx -F all") ),
        ( PMAP, "<Tab>", action_step!("complete -s (list -d next --toggle true) -T auto -D none") ),
        ( PMAP, "<S-Tab>", action_step!("complete -s (list -d previous --toggle true) -T auto -D none") ),

        // View mode keys
        ( VMAP, "<Esc>", goto!(KakouneMode::Normal) ),
        ( VMAP, "b", action_step!("scroll -s (cursor-pos -p end -x vertical)") ),
        ( VMAP, "c", action_step!("scroll -s (cursor-pos -p middle -x vertical)") ),
        ( VMAP, "h", action_step!("scroll -s (dir2d -d left -z cell)") ),
        ( VMAP, "j", action_step!("scroll -s (dir2d -d down -z cell)") ),
        ( VMAP, "k", action_step!("scroll -s (dir2d -d up -z cell)") ),
        ( VMAP, "l", action_step!("scroll -s (dir2d -d right -z cell)") ),
        ( VMAP, "m", action_step!("scroll -s (cursor-pos -p middle -x horizontal)") ),
        ( VMAP, "t", action_step!("scroll -s (cursor-pos -p beginning -x vertical)") ),
        ( VMAP, "v", action_step!("scroll -s (cursor-pos -p middle -x vertical)") ),

        // Goto mode keys
        ( GMAP, "a", action_step!("window switch -t alternate") ),
        ( GMAP, "b", edit!(EditAction::Motion, MoveType::ViewportPos(MovePosition::End)) ),
        ( GMAP, "B", edit!(EditAction::Motion, MoveType::ViewportPos(MovePosition::End)) ),
        ( GMAP, "c", edit!(EditAction::Motion, MoveType::ViewportPos(MovePosition::Middle)) ),
        ( GMAP, "C", edit!(EditAction::Motion, MoveType::ViewportPos(MovePosition::Middle)) ),
        ( GMAP, "e", edit_buffer!(EditAction::Motion, MoveTerminus::End) ),
        ( GMAP, "E", edit_buffer!(EditAction::Motion, MoveTerminus::End) ),
        ( GMAP, "f", action_step!("window switch -t selection") ),
        ( GMAP, "g", edit!(EditAction::Motion, MoveType::BufferPos(MovePosition::Beginning)) ),
        ( GMAP, "G", edit!(EditAction::Motion, MoveType::BufferPos(MovePosition::Beginning)) ),
        ( GMAP, "h", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( GMAP, "H", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( GMAP, "i", edit!(EditAction::Motion, MoveType::FirstWord(MoveDir1D::Next), 0)),
        ( GMAP, "I", edit!(EditAction::Motion, MoveType::FirstWord(MoveDir1D::Next), 0)),
        ( GMAP, "j", edit!(EditAction::Motion, MoveType::BufferPos(MovePosition::End)) ),
        ( GMAP, "J", edit!(EditAction::Motion, MoveType::BufferPos(MovePosition::End)) ),
        ( GMAP, "k", edit_buffer!(EditAction::Motion, MoveTerminus::Beginning) ),
        ( GMAP, "K", edit_buffer!(EditAction::Motion, MoveTerminus::Beginning) ),
        ( GMAP, "l", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::End), 0) ),
        ( GMAP, "L", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::End), 0) ),
        ( GMAP, "t", edit!(EditAction::Motion, MoveType::ViewportPos(MovePosition::Beginning)) ),
        ( GMAP, "T", edit!(EditAction::Motion, MoveType::ViewportPos(MovePosition::Beginning)) ),
        ( GMAP, ".", edit_target!(EditAction::Motion, EditTarget::CharJump(Mark::LastInserted.into())) ),

        // Object keys
        ( OMAP, "<A-w>", object_end!(RangeType::Word(WordStyle::Big)) ),
        ( OMAP, "<A-;>", unmapped!() ),
        ( OMAP, "<Space>", object_whitespace_end!() ),
        ( OMAP, "a", object_end!(RangeType::Bracketed('<', '>')) ),
        ( OMAP, "b", object_end!(RangeType::Bracketed('(', ')')) ),
        ( OMAP, "c", unmapped!() ),
        ( OMAP, "B", object_end!(RangeType::Bracketed('{', '}')) ),
        ( OMAP, "i", unmapped!() ),
        ( OMAP, "g", object_end!(RangeType::Quote('`')) ),
        ( OMAP, "n", object_end!(RangeType::Word(WordStyle::Number(Radix::Decimal))) ),
        ( OMAP, "p", object_end!(RangeType::Paragraph) ),
        ( OMAP, "q", object_end!(RangeType::Quote('\'')) ),
        ( OMAP, "Q", object_end!(RangeType::Quote('"')) ),
        ( OMAP, "r", object_end!(RangeType::Bracketed('[', ']')) ),
        ( OMAP, "s", object_end!(RangeType::Sentence) ),
        ( OMAP, "u", unmapped!() ),
        ( OMAP, "w", object_end!(RangeType::Word(WordStyle::Little)) ),
        ( OMAP, "(", object_end!(RangeType::Bracketed('(', ')')) ),
        ( OMAP, ")", object_end!(RangeType::Bracketed('(', ')')) ),
        ( OMAP, "<", object_end!(RangeType::Bracketed('<', '>')) ),
        ( OMAP, ">", object_end!(RangeType::Bracketed('<', '>')) ),
        ( OMAP, "[", object_end!(RangeType::Bracketed('[', ']')) ),
        ( OMAP, "]", object_end!(RangeType::Bracketed('[', ']')) ),
        ( OMAP, "{", object_end!(RangeType::Bracketed('{', '}')) ),
        ( OMAP, "}", object_end!(RangeType::Bracketed('{', '}')) ),
        ( OMAP, "'", object_end!(RangeType::Quote('\'')) ),
        ( OMAP, "\"", object_end!(RangeType::Quote('"')) ),
        ( OMAP, "`", object_end!(RangeType::Quote('`')) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_pfxs<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, Option<InputStep<I>>)> {
    [
        // Normal mode commands can be prefixed w/ a count.
        ( NMAP, "{count}", None ),
        ( NMAP, "\"{register}", None ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_enter<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // <Enter> in Insert mode types a newline character.
        ( IMAP, "<Enter>", action_step!("insert type -i (exact '\\n') -d prev -c 1") ),

        // <Enter> in Command mode submits the command.
        ( PMAP, "<Enter>", action_step!("prompt submit", KakouneMode::Normal) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_search<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // Visually select searches in Normal mode.
        ( NMAP, "<A-n>", selection_object_search!(MoveDir1D::Previous) ),
        ( NMAP, "<A-N>", extend_search!(SearchType::Regex, MoveDirMod::Exact(MoveDir1D::Previous)) ),
        ( NMAP, "n", selection_object_search!(MoveDir1D::Next) ),
        ( NMAP, "N", extend_search!(SearchType::Regex, MoveDirMod::Exact(MoveDir1D::Next)) ),
    ].to_vec()
}

#[rustfmt::skip]
fn submit_on_enter<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // <Enter> in Normal, Insert and Command modes submits the command.
        ( MAP, "<Enter>", action_step!("prompt submit") ),
    ].to_vec()
}

#[rustfmt::skip]
fn search_is_action<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // Perform an application-level search in Normal mode.
        ( NMAP, "<A-n>", action_step!("search -d (exact previous) -c ctx") ),
        ( NMAP, "<A-N>", action_step!("search -d (exact previous) -c ctx") ),
        ( NMAP, "n", action_step!("search -d (exact next) -c ctx") ),
        ( NMAP, "N", action_step!("search -d (exact next) -c ctx") ),
    ].to_vec()
}

#[inline]
fn add_prefix<I: ApplicationInfo>(
    machine: &mut KakouneMachine<TerminalKey, I>,
    modes: &MappedModes,
    keys: &str,
    action: &Option<InputStep<I>>,
) {
    let (_, evs) = parse(keys).unwrap_or_else(|_| panic!("invalid kakoune keybinding: {keys}"));
    let modes = modes.split();

    for mode in modes {
        machine.add_prefix(mode, &evs, action);
    }
}

#[inline]
fn add_mapping<I: ApplicationInfo>(
    machine: &mut KakouneMachine<TerminalKey, I>,
    modes: &MappedModes,
    keys: &str,
    action: &InputStep<I>,
) {
    let (_, evs) = parse(keys).unwrap_or_else(|_| panic!("invalid kakoune keybinding: {keys}"));
    let modes = modes.split();

    for mode in modes {
        machine.add_mapping(mode, &evs, action);
    }
}

/// A configurable collection of Kakoune bindings that can be added to a [ModalMachine].
#[derive(Debug)]
pub struct KakouneBindings<I: ApplicationInfo> {
    prefixes: Vec<(MappedModes, &'static str, Option<InputStep<I>>)>,
    mappings: Vec<(MappedModes, &'static str, InputStep<I>)>,
    enter: Vec<(MappedModes, &'static str, InputStep<I>)>,
    search: Vec<(MappedModes, &'static str, InputStep<I>)>,
}

impl<I: ApplicationInfo> KakouneBindings<I> {
    /// Map the Enter key to [submit](PromptAction::Submit) in all modes.
    ///
    /// Normally, Enter is unmapped in Kakoune's Normal mode.
    pub fn submit_on_enter(mut self) -> Self {
        self.enter = submit_on_enter();
        self
    }

    /// Remap `n`, `N`, `<A-n>` and `<A-N>` in Normal mode to perform [Action::Search] instead.
    pub fn search_is_action(mut self) -> Self {
        self.search = search_is_action();
        self
    }
}

impl<I: ApplicationInfo> ShellBindings for KakouneBindings<I> {
    fn shell(self) -> Self {
        self.submit_on_enter().search_is_action()
    }
}

impl<I: ApplicationInfo> Default for KakouneBindings<I> {
    fn default() -> Self {
        KakouneBindings {
            prefixes: default_pfxs(),
            mappings: default_keys(),
            enter: default_enter(),
            search: default_search(),
        }
    }
}

impl<I: ApplicationInfo> InputBindings<TerminalKey, InputStep<I>> for KakouneBindings<I> {
    fn setup(&self, machine: &mut KakouneMachine<TerminalKey, I>) {
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

/// Manage Kakoune keybindings and modes.
pub type KakouneMachine<Key, T = EmptyInfo> = ModalMachine<Key, InputStep<T>>;

/// Create a new [KakouneMachine] populated with standard Kakoune keys.
pub fn default_kakoune_keys<I: ApplicationInfo>() -> KakouneMachine<TerminalKey, I> {
    ModalMachine::from_bindings::<KakouneBindings<I>>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::context::EditContext;
    use crate::keybindings::BindingMachine;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use editor_types::HistoryAction;

    macro_rules! action_reset {
        ($ctx: expr) => {
            $ctx.action.count = None;
            $ctx.action.cursor_end = CursorEnd::Auto;
            $ctx.action.register = None;
            $ctx.action.register_append = false;
            $ctx.action.shape = None;
            $ctx.ch = Default::default();
        };
    }

    macro_rules! assert_normal {
        ($mm: expr, $ctx: expr) => {
            let mut keep = $ctx.clone();
            action_reset!($ctx);
            $ctx.persist.insert = None;
            assert_pop2!($mm, CHECKPOINT, $ctx);
            assert_eq!($mm.mode(), KakouneMode::Normal);
            std::mem::swap(&mut keep, &mut $ctx);
        };
    }

    const CHECKPOINT: Action = Action::Editor(EditorAction::History(HistoryAction::Checkpoint));

    fn mkctx() -> KakouneState<EmptyInfo> {
        KakouneState::default()
    }

    #[test]
    fn test_mode_transitions() {
        let mut km: KakouneMachine<TerminalKey> = default_kakoune_keys();
        let mut ctx = mkctx();

        // Begin in Normal mode:
        assert_eq!(km.mode(), KakouneMode::Normal);

        // Move to View mode:
        km.input_key(key!('V'));
        assert_pop2!(km, Action::NoOp, ctx);
        assert_eq!(km.mode(), KakouneMode::View);

        // And then back to Normal mode:
        km.input_key(key!(KeyCode::Esc));
        assert_pop1!(km, Action::NoOp, ctx);
        assert_normal!(km, ctx);
    }

    #[test]
    fn test_charsearch_params_and_char() {
        let mut km: KakouneMachine<TerminalKey> = default_kakoune_keys();
        let mut ctx = mkctx();

        // Search for 'a':
        let search =
            EditTarget::Search(SearchType::Char(true), MoveDirMod::Same, Count::Contextual);
        let resize = SelectionAction::Resize(SelectionResizeStyle::Restart, search);
        let resize = Action::from(EditorAction::Selection(resize));

        km.input_key(key!('f'));
        km.input_key(key!('a'));
        ctx.persist.charsearch_params = (MoveDir1D::Next, true);
        ctx.persist.charsearch = Some(Char::Single('a'));
        ctx.ch.any = Some(key!('a'));
        assert_pop1!(km, resize, ctx);
        assert_normal!(km, ctx);

        // Repeat the selection:
        let repeat = Action::Repeat(RepeatType::LastSelection);

        km.input_key(alt!('.'));
        ctx.ch.any = None;
        assert_pop1!(km, repeat, ctx);
        assert_normal!(km, ctx);

        // Verify that it repeats the right actions and editing context:
        km.repeat(RepeatType::LastSelection, None);

        // Original charsearch context:
        ctx.ch.any = Some(key!('a'));
        assert_pop1!(km, resize, ctx);
        assert_eq!(km.pop(), None);
    }

    #[test]
    fn test_line_selection() {
        let mut km: KakouneMachine<TerminalKey> = default_kakoune_keys();
        let mut ctx = mkctx();

        let obj = SelectionResizeStyle::Object;
        let sel = SelectionAction::Resize(obj, RangeType::Line.into());
        let sel = EditorAction::Selection(sel);

        km.input_key(key!('x'));
        ctx.action.shape = Some(TargetShape::CharWise);
        assert_pop1!(km, Action::from(sel), ctx);
        assert_normal!(km, ctx);
    }

    #[test]
    fn test_alt_a_object_select() {
        let mut km: KakouneMachine<TerminalKey> = default_kakoune_keys();
        let mut ctx = mkctx();

        let word = EditTarget::Range(RangeType::Word(WordStyle::Little), true, Count::Contextual);
        let resize = SelectionAction::Resize(SelectionResizeStyle::Object, word);
        let resize = Action::from(EditorAction::Selection(resize));

        // Begin the object selection:
        km.input_key(alt!('a'));
        assert_eq!(km.pop(), None);

        // Select a word:
        km.input_key(key!('w'));
        ctx.action.shape = Some(TargetShape::CharWise);
        assert_pop1!(km, resize, ctx);
        assert_normal!(km, ctx);
    }
}
