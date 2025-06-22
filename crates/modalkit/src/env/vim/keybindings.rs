//! # Vim Keybindings
//!
//! ## Overview
//!
//! This module handles mapping the keybindings used in Vim onto the
//! [Action] type.
//!
//! ## Example
//!
//! ```
//! use modalkit::env::vim::VimMode;
//! use modalkit::env::vim::keybindings::{default_vim_keys, VimMachine};
//!
//! use modalkit::actions::{Action, EditAction, EditorAction, HistoryAction};
//! use modalkit::editing::context::Resolve;
//! use modalkit::prelude::*;
//!
//! use modalkit::keybindings::BindingMachine;
//! use modalkit::key::TerminalKey;
//!
//! use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
//!
//! const fn key(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
//!     KeyEvent::new(code, modifiers)
//! }
//!
//! fn main() {
//!     let mut keybindings: VimMachine<TerminalKey> = default_vim_keys();
//!
//!     // Begins in Normal mode.
//!     assert_eq!(keybindings.mode(), VimMode::Normal);
//!
//!     // Typing "5dd" deletes a line.
//!     keybindings.input_key(key(KeyCode::Char('5'), KeyModifiers::NONE).into());
//!     keybindings.input_key(key(KeyCode::Char('d'), KeyModifiers::NONE).into());
//!     keybindings.input_key(key(KeyCode::Char('d'), KeyModifiers::NONE).into());
//!
//!     let (act, ctx) = keybindings.pop().unwrap();
//!     let exp = EditorAction::Edit(EditAction::Delete.into(), RangeType::Line.into());
//!     assert_eq!(act, Action::from(exp));
//!     assert_eq!(ctx.resolve(&Count::Contextual), 5);
//!
//!     // Returning to Normal mode causes history checkpoint.
//!     let (act, _) = keybindings.pop().unwrap();
//!     assert_eq!(act, HistoryAction::Checkpoint.into());
//!
//!     // End of available actions.
//!     assert_eq!(keybindings.pop(), None);
//! }
//! ```
use bitflags::bitflags;

use crate::actions::{
    Action,
    CommandAction,
    CommandBarAction,
    CursorAction,
    EditAction,
    EditorAction,
    HistoryAction,
    InsertTextAction,
    MacroAction,
    PromptAction,
    SelectionAction,
    TabAction,
    WindowAction,
};
use crate::editing::application::{ApplicationInfo, EmptyInfo};
use crate::prelude::*;

use super::{
    super::{keyparse::parse, CommonKeyClass, ShellBindings},
    VimMode,
    VimState,
};

use crate::key::TerminalKey;
use crate::keybindings::{InputBindings, ModalMachine, Step};

bitflags! {
    #[derive(Debug, Clone, Copy)]
    struct MappedModes: u32 {
        const N = 0b0000000000000001;
        const X = 0b0000000000000010;
        const S = 0b0000000000000100;
        const O = 0b0000000000001000;
        const I = 0b0000000000010000;
        const C = 0b0000000000100000;
        const L = 0b0000000001000000;

        const SUFFIX_CHARSRCH = 0b1000000000000000;
        const SUFFIX_CHARREPL = 0b0100000000000000;

        const V = MappedModes::X.bits() | MappedModes::S.bits();

        const NVI = MappedModes::N.bits() | MappedModes::V.bits() | MappedModes::I.bits();
        const NVO = MappedModes::N.bits() | MappedModes::V.bits() | MappedModes::O.bits();
        const NXO = MappedModes::N.bits() | MappedModes::X.bits() | MappedModes::O.bits();
        const NV = MappedModes::N.bits() | MappedModes::V.bits();
        const NX = MappedModes::N.bits() | MappedModes::X.bits();
        const VO = MappedModes::V.bits() | MappedModes::O.bits();
        const IC = MappedModes::I.bits() | MappedModes::C.bits();
    }
}

const MAP: MappedModes = MappedModes::NVO;
const NVMAP: MappedModes = MappedModes::NV;
const NXMAP: MappedModes = MappedModes::NX;
const NXOMAP: MappedModes = MappedModes::NXO;
const NVIMAP: MappedModes = MappedModes::NVI;
const VOMAP: MappedModes = MappedModes::VO;
const ICMAP: MappedModes = MappedModes::IC;

const NMAP: MappedModes = MappedModes::N;
const OMAP: MappedModes = MappedModes::O;
const IMAP: MappedModes = MappedModes::I;
const CMAP: MappedModes = MappedModes::C;
const VMAP: MappedModes = MappedModes::V;
const XMAP: MappedModes = MappedModes::X;
const SMAP: MappedModes = MappedModes::S;

const SUFFIX_CHARREPL: MappedModes = MappedModes::SUFFIX_CHARREPL;
const SUFFIX_CHARSRCH: MappedModes = MappedModes::SUFFIX_CHARSRCH;

impl MappedModes {
    pub fn split(&self) -> Vec<VimMode> {
        let mut modes = Vec::new();

        if self.contains(MappedModes::N) {
            modes.push(VimMode::Normal);
        }

        if self.contains(MappedModes::X) {
            modes.push(VimMode::Visual);
        }

        if self.contains(MappedModes::S) {
            modes.push(VimMode::Select);
        }

        if self.contains(MappedModes::O) {
            modes.push(VimMode::OperationPending);
        }

        if self.contains(MappedModes::I) {
            modes.push(VimMode::Insert);
        }

        if self.contains(MappedModes::L) {
            modes.push(VimMode::LangArg);
        }

        if self.contains(MappedModes::C) {
            modes.push(VimMode::Command);
        }

        if self.contains(MappedModes::SUFFIX_CHARREPL) {
            modes.push(VimMode::CharReplaceSuffix);
        }

        if self.contains(MappedModes::SUFFIX_CHARSRCH) {
            modes.push(VimMode::CharSearchSuffix);
        }

        return modes;
    }
}

#[derive(Clone, Debug)]
enum InternalAction {
    SetCursorEnd(CursorEnd),
    SetTarget(EditTarget),
    SetSearchCharParams(MoveDir1D, bool),
    SetSearchChar,
    SetSearchRegexParams(MoveDir1D, bool),
    SetRegister(Register),
    SetReplaceChar(Option<Char>),
    SaveCounting,
    SetCursorChar(char),
    SetCursorDigraph,
    SetInsertStyle(InsertStyle),
    SetTargetShape(TargetShapeFilter, TargetShape),
    SetOperation(EditAction),
    SetPostMode(VimMode),
}

impl InternalAction {
    pub fn run<I: ApplicationInfo>(&self, ctx: &mut VimState<I>) {
        match self {
            InternalAction::SetCursorEnd(end) => {
                ctx.action.cursor_end = Some(*end);
            },
            InternalAction::SetSearchCharParams(dir, inclusive) => {
                ctx.action.charsearch_params = Some((*dir, *inclusive));
            },
            InternalAction::SetSearchChar => {
                if let Some((d, i)) = ctx.action.charsearch_params.take() {
                    ctx.persist.charsearch_params = (d, i);
                }

                ctx.persist.charsearch = ctx.ch.get_typed();
                ctx.ch = Default::default();
            },
            InternalAction::SetSearchRegexParams(dir, incremental) => {
                ctx.persist.regexsearch_dir = *dir;
                ctx.persist.regexsearch_inc = *incremental;
            },
            InternalAction::SetRegister(reg) => {
                ctx.action.register = Some(reg.clone());
            },
            InternalAction::SetReplaceChar(c) => {
                if c.is_some() {
                    ctx.action.replace = c.clone();
                } else {
                    ctx.action.replace = ctx.ch.get_typed();
                    ctx.ch = Default::default();
                }
            },
            InternalAction::SaveCounting => {
                match ctx.action.count {
                    None => {
                        ctx.action.count = ctx.action.counting;
                        ctx.action.counting = None;
                    },
                    Some(prev) => {
                        ctx.action.count =
                            Some(prev.saturating_mul(ctx.action.counting.unwrap_or(1)));
                        ctx.action.counting = None;
                    },
                }
            },
            InternalAction::SetCursorChar(c) => {
                ctx.action.cursor = Some(*c);
            },
            InternalAction::SetCursorDigraph => {
                if ctx.ch.digraph1.is_some() {
                    ctx.action.cursor = ctx.ch.digraph1;
                }
            },
            InternalAction::SetInsertStyle(style) => {
                match ctx.persist.insert {
                    None => {
                        ctx.persist.insert = Some(*style);
                    },
                    Some(ref old) => {
                        if style == old {
                            ctx.persist.insert = Some(!*style);
                        } else {
                            ctx.persist.insert = Some(*style);
                        }
                    },
                }
            },
            InternalAction::SetTargetShape(f, shape) => {
                match ctx.persist.shape {
                    Some(curr) if f.matches(&curr) => {
                        ctx.persist.shape = Some(*shape);
                    },
                    None => {
                        ctx.persist.shape = Some(*shape);
                    },
                    _ => {},
                }
            },
            InternalAction::SetOperation(op) => {
                ctx.action.operation = op.clone();
            },
            InternalAction::SetTarget(et) => {
                ctx.action.target = Some(et.clone());
            },
            InternalAction::SetPostMode(ps) => {
                ctx.action.postmode = Some(*ps);
            },
        }
    }
}

#[derive(Clone, Debug)]
enum ExternalAction<I: ApplicationInfo> {
    Something(Action<I>),
    CountAlters(Vec<Action<I>>, Vec<Action<I>>),
    CommandEnter(String, CommandType, Action<I>, VimMode),
    CommandExit(PromptAction),
    MacroToggle(bool),
    PostAction,
}

impl<I: ApplicationInfo> ExternalAction<I> {
    fn resolve(&self, context: &mut VimState<I>) -> Vec<Action<I>> {
        match self {
            ExternalAction::Something(act) => vec![act.clone()],
            ExternalAction::CountAlters(acts1, acts2) => {
                if context.action.count.is_none() {
                    acts1.clone()
                } else {
                    acts2.clone()
                }
            },
            ExternalAction::CommandEnter(s, ct, act, mode) => {
                let act = CommandBarAction::Focus(s.clone(), *ct, Box::new(act.clone()));
                let mode = context.action.postmode.take().unwrap_or(*mode);
                let shape = context.persist.shape.take();
                let actx = Box::new(context.action.clone()).into();
                context.persist.postcmd = (mode, shape, actx);

                return vec![act.into()];
            },
            ExternalAction::CommandExit(act) => {
                let (mode, shape, actx) = std::mem::take(&mut context.persist.postcmd);
                context.action = *actx.unwrap_or_default();
                context.action.postmode = mode.into();
                context.persist.shape = shape;

                return vec![act.clone().into()];
            },
            ExternalAction::PostAction => {
                if let Some(target) = context.action.target.take() {
                    vec![EditorAction::Edit(Specifier::Contextual, target).into()]
                } else {
                    vec![Action::NoOp]
                }
            },
            ExternalAction::MacroToggle(reqrec) => {
                let recording = context.persist.recording.is_some();

                if *reqrec && !recording {
                    return vec![];
                } else if recording {
                    context.persist.recording = None;
                } else if let Some(ref reg) = context.action.register {
                    let append = context.action.register_append;
                    context.persist.recording = Some((reg.clone(), append));
                } else {
                    context.persist.recording = Some((Register::UnnamedMacro, false));
                }

                vec![MacroAction::ToggleRecording.into()]
            },
        }
    }
}

/// Description of actions to take after an input sequence.
#[derive(Debug, Default)]
pub struct InputStep<I: ApplicationInfo> {
    internal: Vec<InternalAction>,
    external: Vec<ExternalAction<I>>,
    fallthrough_mode: Option<VimMode>,
    nextm: Option<VimMode>,
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

    /// Create an operator that is followed by a movement.
    ///
    /// `mode` specifies which mode to enter after the movement is provided. This defaults to
    /// [VimMode::Normal].
    pub fn operator(mut self, op: EditAction, mode: Option<VimMode>) -> Self {
        let insert = matches!(mode, Some(VimMode::Insert));

        self.fallthrough_mode = Some(VimMode::OperationPending);
        self.internal = vec![
            InternalAction::SetOperation(op),
            InternalAction::SetPostMode(mode.unwrap_or_default()),
        ];

        if insert {
            self.internal.push(InternalAction::SetInsertStyle(InsertStyle::Insert));
        }

        self
    }

    /// Set the [VimMode] to switch to after this step.
    pub fn goto(mut self, mode: VimMode) -> Self {
        self.nextm = Some(mode);
        self
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
            fallthrough_mode: self.fallthrough_mode,
            nextm: self.nextm,
        }
    }
}

impl<I: ApplicationInfo> Step<TerminalKey> for InputStep<I> {
    type A = Action<I>;
    type State = VimState<I>;
    type Class = CommonKeyClass;
    type M = VimMode;
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

    fn step(&self, ctx: &mut VimState<I>) -> (Vec<Action<I>>, Option<Self::M>) {
        match (self.nextm, self.internal.as_slice(), ctx.persist.shape) {
            (Some(VimMode::Visual), [InternalAction::SetTargetShape(f, s1)], Some(ref s2))
                if f.matches(s2) && s1 == s2 =>
            {
                return (vec![], Some(VimMode::Normal));
            },
            (Some(VimMode::Select), [InternalAction::SetTargetShape(f, s1)], Some(ref s2))
                if f.matches(s2) && s1 == s2 =>
            {
                return (vec![], Some(VimMode::Normal));
            },
            (_, internal, _) => {
                for iact in internal.iter() {
                    iact.run(ctx);
                }

                let external: Vec<Action<I>> =
                    self.external.iter().flat_map(|act| act.resolve(ctx)).collect();

                if external.is_empty() {
                    return (external, self.nextm);
                } else {
                    return (external, ctx.action.postmode.take().or(self.nextm));
                }
            },
        }
    }
}

macro_rules! act {
    ($ext: expr) => {
        isv!(vec![], vec![ExternalAction::Something($ext.into())])
    };
    ($ext: expr, $ns: expr) => {
        isv!(vec![], vec![ExternalAction::Something($ext.into())], $ns)
    };
}

macro_rules! count_alters {
    ($act1: expr, $act2: expr) => {
        isv!(vec![], vec![ExternalAction::CountAlters(vec![$act1], vec![$act2])])
    };
    ($act1: expr, $act2: expr, $ns: expr) => {
        isv!(vec![], vec![ExternalAction::CountAlters(vec![$act1], vec![$act2])], $ns)
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
}

macro_rules! shaped_filter {
    ($f: expr, $shape: expr) => {
        iact!(InternalAction::SetTargetShape($f, $shape))
    };
    ($f: expr, $shape: expr, $act: expr) => {
        is!(InternalAction::SetTargetShape($f, $shape), $act)
    };
    ($f: expr, $shape: expr, $act: expr, $nm: expr) => {
        is!(InternalAction::SetTargetShape($f, $shape), $act, $nm)
    };
}

macro_rules! shaped {
    ($shape: expr) => {
        shaped_filter!(TargetShapeFilter::ALL, $shape)
    };
    ($shape: expr, $act: expr) => {
        shaped_filter!(TargetShapeFilter::ALL, $shape, $act)
    };
    ($shape: expr, $act: expr, $nm: expr) => {
        shaped_filter!(TargetShapeFilter::ALL, $shape, $act, $nm)
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

macro_rules! scrollcpv {
    ($p: expr, $fw: literal) => {
        if $fw {
            isv!(vec![], vec![
                ExternalAction::Something(
                    EditorAction::Edit(
                        Specifier::Exact(EditAction::Motion),
                        EditTarget::Motion(MoveType::FirstWord(MoveDir1D::Next), Count::Exact(0))
                    )
                    .into()
                ),
                ExternalAction::CountAlters(
                    vec![Action::Scroll(ScrollStyle::CursorPos($p, Axis::Vertical))],
                    vec![Action::Scroll(ScrollStyle::LinePos($p, Count::Contextual))],
                ),
            ])
        } else {
            count_alters!(
                Action::Scroll(ScrollStyle::CursorPos($p, Axis::Vertical)),
                Action::Scroll(ScrollStyle::LinePos($p, Count::Contextual))
            )
        }
    };
}

macro_rules! scrollcph {
    ($p: expr) => {
        scrollcp!($p, Axis::Horizontal)
    };
}

macro_rules! edit_target_nocount {
    ($ea: expr, $et: expr, $mode: expr) => {
        count_alters!(EditorAction::Edit(Specifier::Exact($ea), $et).into(), Action::NoOp, $mode)
    };
    ($ea: expr, $et: expr) => {
        count_alters!(EditorAction::Edit(Specifier::Exact($ea), $et).into(), Action::NoOp)
    };
}

macro_rules! edit_nocount {
    ($ea: expr, $mt: expr, $c: expr, $mode: expr) => {
        edit_target_nocount!($ea, EditTarget::Motion($mt, $c), $mode)
    };
    ($ea: expr, $mt: expr, $c: literal) => {
        edit_target_nocount!($ea, EditTarget::Motion($mt, Count::Exact($c)))
    };
    ($ea: expr, $mt: expr) => {
        edit_target_nocount!($ea, EditTarget::Motion($mt, Count::Contextual))
    };
}

macro_rules! edit_selection_nocount {
    ($ea: expr) => {
        edit_target_nocount!($ea, EditTarget::Selection, VimMode::Normal)
    };
    ($ea: expr, $mode: expr) => {
        edit_target_nocount!($ea, EditTarget::Selection, $mode)
    };
}

macro_rules! tilde {
    () => {
        isv!(vec![InternalAction::SetCursorEnd(CursorEnd::End)], vec![ExternalAction::Something(
            EditorAction::Edit(
                Specifier::Exact(EditAction::ChangeCase(Case::Toggle)),
                EditTarget::Motion(MoveType::Column(MoveDir1D::Next, false), Count::Contextual),
            )
            .into()
        )])
    };
}

macro_rules! change_target {
    ($et: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle(InsertStyle::Insert)],
            vec![ExternalAction::Something(
                EditorAction::Edit(Specifier::Exact(EditAction::Delete), $et).into()
            )],
            VimMode::Insert
        )
    };
}

macro_rules! change_range {
    ($rt: expr) => {
        change_target!(EditTarget::Range($rt, true, Count::Contextual))
    };
}

macro_rules! change {
    ($mt: expr) => {
        change_target!(EditTarget::Motion($mt, Count::Contextual))
    };
    ($mt: expr, $c: literal) => {
        change_target!(EditTarget::Motion($mt, Count::Exact($c)))
    };
    ($mt: expr, $c: expr) => {
        change_target!(EditTarget::Motion($mt, $c))
    };
}

macro_rules! charsearch {
    ($d: expr, $i: expr) => {
        fallthrough!(VimMode::CharSearchSuffix, vec![
            InternalAction::SetSearchCharParams($d, $i),
            InternalAction::SetTarget(EditTarget::Search(
                SearchType::Char(false),
                MoveDirMod::Same,
                Count::Contextual
            ))
        ])
    };
}

macro_rules! charsearch_suffix {
    () => {
        isv!(vec![InternalAction::SetSearchChar], vec![ExternalAction::PostAction])
    };
}

macro_rules! charreplace {
    ($v: expr) => {
        fallthrough!(VimMode::CharReplaceSuffix, vec![
            InternalAction::SetOperation(EditAction::Replace($v)),
            InternalAction::SetTarget(EditTarget::Motion(
                MoveType::Column(MoveDir1D::Next, false),
                Count::Contextual
            ))
        ])
    };
    ($v: expr, $et: expr) => {
        fallthrough!(VimMode::CharReplaceSuffix, vec![
            InternalAction::SetOperation(EditAction::Replace($v)),
            InternalAction::SetTarget($et)
        ])
    };
}

macro_rules! charreplace_suffix {
    () => {
        isv!(
            vec![InternalAction::SetReplaceChar(None)],
            vec![ExternalAction::PostAction],
            VimMode::Normal
        )
    };
    ($c: expr) => {
        isv!(
            vec![InternalAction::SetReplaceChar(Some($c))],
            vec![ExternalAction::PostAction],
            VimMode::Normal
        )
    };
}

macro_rules! change_selection_lines {
    () => {
        isv!(
            vec![
                InternalAction::SetTargetShape(TargetShapeFilter::ALL, TargetShape::LineWise),
                InternalAction::SetInsertStyle(InsertStyle::Insert),
            ],
            vec![ExternalAction::Something(
                EditorAction::Edit(Specifier::Exact(EditAction::Delete), EditTarget::Selection)
                    .into()
            )],
            VimMode::Insert
        )
    };
}

macro_rules! motion {
    ($iacts: expr) => {
        fallthrough!(VimMode::OperationPending, $iacts)
    };
}

macro_rules! edit_motion {
    ($ea: expr) => {
        motion!(vec![
            InternalAction::SetOperation($ea),
            InternalAction::SetPostMode(VimMode::Normal),
        ])
    };
    ($ea: expr, $mode: expr) => {
        motion!(vec![
            InternalAction::SetOperation($ea),
            InternalAction::SetPostMode($mode),
        ])
    };
    ($ea: expr, $mode: expr, $style: expr) => {
        motion!(vec![
            InternalAction::SetOperation($ea),
            InternalAction::SetPostMode($mode),
            InternalAction::SetInsertStyle($style),
        ])
    };
}

macro_rules! edit_lines {
    ($ea: expr) => {
        edit_target!($ea, RangeType::Line.into(), VimMode::Normal)
    };
}

macro_rules! edit_target_end {
    ($et: expr) => {
        act!(EditorAction::Edit(Specifier::Contextual, $et))
    };
}

macro_rules! edit_target_end_shaped {
    ($shape: expr, $et: expr) => {
        shaped!($shape, EditorAction::Edit(Specifier::Contextual, $et))
    };
}

macro_rules! edit_target_end_ca {
    ($et1: expr, $et2: expr) => {
        count_alters!(
            EditorAction::Edit(Specifier::Contextual, $et1).into(),
            EditorAction::Edit(Specifier::Contextual, $et2).into()
        )
    };
}

macro_rules! edit_range_end {
    ($rt: expr) => {
        edit_target_end!(EditTarget::Range($rt, true, Count::Contextual))
    };
    ($rt: expr, $inc: expr) => {
        edit_target_end!(EditTarget::Range($rt, $inc, Count::Contextual))
    };
    ($rt: expr, $inc: expr, $c: literal) => {
        edit_target_end!(EditTarget::Range($rt, $inc, Count::Exact($c)))
    };
    ($rt: expr, $inc: expr, $c: expr) => {
        edit_target_end!(EditTarget::Range($rt, $inc, $c))
    };
}

macro_rules! edit_search_end {
    ($st: expr, $mod: expr) => {
        edit_target_end!(EditTarget::Search($st, $mod, Count::Contextual))
    };
    ($st: expr, $mod: expr, $c: literal) => {
        edit_target_end!(EditTarget::Search($st, $mod, Count::Exact($c)))
    };
    ($st: expr, $mod: expr, $c: expr) => {
        edit_target_end!(EditTarget::Search($st, $mod, $c))
    };
}

macro_rules! edit_word_search_end {
    ($style: expr, $boundary: expr, $dir: expr) => {
        is!(
            InternalAction::SetSearchRegexParams($dir, false),
            EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Search(
                    SearchType::Word($style, $boundary),
                    MoveDirMod::Same,
                    Count::Contextual
                )
            )
        )
    };
}

macro_rules! edit_end {
    ($mt: expr) => {
        edit_target_end!(EditTarget::Motion($mt, Count::Contextual))
    };
    ($mt: expr, $c: literal) => {
        edit_target_end!(EditTarget::Motion($mt, Count::Exact($c)))
    };
    ($mt: expr, $c: expr) => {
        edit_target_end!(EditTarget::Motion($mt, $c))
    };
}

macro_rules! edit_end_shaped {
    ($shape: expr, $mt: expr) => {
        edit_target_end_shaped!($shape, EditTarget::Motion($mt, Count::Contextual))
    };
    ($shape: expr, $mt: expr, $c: literal) => {
        edit_target_end_shaped!($shape, EditTarget::Motion($mt, Count::Exact($c)))
    };
    ($shape: expr, $mt: expr, $c: expr) => {
        edit_target_end_shaped!($shape, EditTarget::Motion($mt, $c))
    };
}

macro_rules! edit_end_ca {
    ($mt1: expr, $mt2: expr) => {
        edit_target_end_ca!(
            EditTarget::Motion($mt1, Count::Contextual),
            EditTarget::Motion($mt2, Count::Contextual)
        )
    };
}

macro_rules! normal {
    () => {
        goto!(VimMode::Normal)
    };
}

macro_rules! insert {
    ($style: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle($style)],
            vec![ExternalAction::Something(
                CursorAction::Split(Count::MinusOne).into()
            )],
            VimMode::Insert
        )
    };
    ($style: expr, $mt: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle($style)],
            vec![
                ExternalAction::Something(
                    EditorAction::Edit(
                        Specifier::Exact(EditAction::Motion),
                        EditTarget::Motion($mt, Count::Exact(1))
                    )
                    .into()
                ),
                ExternalAction::Something(CursorAction::Split(Count::MinusOne).into()),
            ],
            VimMode::Insert
        )
    };
    ($style: expr, $mt: expr, $c: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle($style)],
            vec![
                ExternalAction::Something(
                    EditorAction::Edit(
                        Specifier::Exact(EditAction::Motion),
                        EditTarget::Motion($mt, Count::Exact($c))
                    )
                    .into()
                ),
                ExternalAction::Something(CursorAction::Split(Count::MinusOne).into()),
            ],
            VimMode::Insert
        )
    };
}

macro_rules! paste_register {
    ($dir: expr, $reg: expr) => {
        is!(
            InternalAction::SetRegister($reg),
            InsertTextAction::Paste(PasteStyle::Side($dir), Count::Contextual)
        )
    };
    ($dir: expr, $reg: expr, $nm: expr) => {
        is!(
            InternalAction::SetRegister($reg),
            InsertTextAction::Paste(PasteStyle::Side($dir), Count::Contextual),
            $nm
        )
    };
}

macro_rules! open_lines {
    ($dir: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle(InsertStyle::Insert)],
            vec![
                ExternalAction::Something(CursorAction::Split(Count::MinusOne).into()),
                ExternalAction::Something(
                    InsertTextAction::OpenLine(TargetShape::LineWise, $dir, 1.into()).into()
                )
            ],
            VimMode::Insert
        )
    };
}

macro_rules! edit_selection_nochar {
    ($ea: expr) => {
        shaped_filter!(
            TargetShapeFilter::CHAR,
            TargetShape::LineWise,
            EditorAction::Edit(Specifier::Exact($ea), EditTarget::Selection),
            VimMode::Normal
        )
    };
}

macro_rules! delete_selection_nochar {
    ($cursor: expr, $et: expr) => {
        isv!(
            vec![InternalAction::SetTargetShape(
                TargetShapeFilter::CHAR,
                TargetShape::LineWise
            )],
            vec![
                ExternalAction::Something(
                    SelectionAction::Split(SelectionSplitStyle::Lines, TargetShapeFilter::ALL)
                        .into()
                ),
                ExternalAction::Something(SelectionAction::CursorSet($cursor).into()),
                ExternalAction::Something(
                    EditorAction::Edit(EditAction::Delete.into(), $et).into()
                ),
            ],
            VimMode::Normal
        )
    };
}

macro_rules! change_selection_nochar {
    ($cursor: expr, $et: expr) => {
        isv!(
            vec![
                InternalAction::SetTargetShape(TargetShapeFilter::CHAR, TargetShape::LineWise),
                InternalAction::SetInsertStyle(InsertStyle::Insert),
            ],
            vec![
                ExternalAction::Something(
                    SelectionAction::Split(SelectionSplitStyle::Lines, TargetShapeFilter::ALL)
                        .into()
                ),
                ExternalAction::Something(SelectionAction::CursorSet($cursor).into()),
                ExternalAction::Something(
                    EditorAction::Edit(EditAction::Delete.into(), $et).into()
                ),
                ExternalAction::Something(CursorAction::Split(Count::MinusOne).into()),
            ],
            VimMode::Insert
        )
    };
}

macro_rules! insert_visual {
    ($cursor: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle(InsertStyle::Insert)],
            vec![
                ExternalAction::Something(
                    SelectionAction::Split(SelectionSplitStyle::Lines, TargetShapeFilter::BLOCK)
                        .into()
                ),
                ExternalAction::Something(SelectionAction::CursorSet($cursor).into()),
                ExternalAction::Something(CursorAction::Split(Count::MinusOne).into()),
            ],
            VimMode::Insert
        )
    };
    ($cursor: expr, $mt: expr, $c: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle(InsertStyle::Insert)],
            vec![
                ExternalAction::Something(
                    SelectionAction::Split(SelectionSplitStyle::Lines, TargetShapeFilter::BLOCK)
                        .into()
                ),
                ExternalAction::Something(SelectionAction::CursorSet($cursor).into()),
                ExternalAction::Something(CursorAction::Split(Count::MinusOne).into()),
                ExternalAction::Something(
                    EditorAction::Edit(EditAction::Motion.into(), EditTarget::Motion($mt, $c))
                        .into()
                ),
            ],
            VimMode::Insert
        )
    };
}

macro_rules! change_visual {
    ($cursor: expr, $et: expr) => {
        isv!(
            vec![InternalAction::SetInsertStyle(InsertStyle::Insert)],
            vec![
                ExternalAction::Something(
                    SelectionAction::Split(SelectionSplitStyle::Lines, TargetShapeFilter::BLOCK)
                        .into()
                ),
                ExternalAction::Something(SelectionAction::CursorSet($cursor).into()),
                ExternalAction::Something(
                    EditorAction::Edit(EditAction::Delete.into(), $et).into()
                ),
                ExternalAction::Something(CursorAction::Split(Count::MinusOne).into()),
            ],
            VimMode::Insert
        )
    };
}

macro_rules! start_selection {
    ($shape: expr, $mode: expr) => {
        shaped!(
            $shape,
            EditorAction::Edit(Specifier::Exact(EditAction::Motion), EditTarget::CurrentPosition),
            $mode
        )
    };
}

macro_rules! selection_resize_search {
    ($style: expr, $dir: expr) => {
        shaped!(
            TargetShape::CharWise,
            EditorAction::Selection(SelectionAction::Resize(
                $style,
                EditTarget::Search(SearchType::Regex, MoveDirMod::Exact($dir), Count::Contextual)
            )),
            VimMode::Visual
        )
    };
}

macro_rules! visual {
    ($shape: expr) => {
        start_selection!($shape, VimMode::Visual)
    };
}

macro_rules! select {
    ($shape: expr) => {
        start_selection!($shape, VimMode::Select)
    };
}

macro_rules! tab {
    ($act: expr) => {
        act!(Action::Tab($act), VimMode::Normal)
    };
    ($act1: expr, $act2: expr) => {
        count_alters!(Action::Tab($act1), Action::Tab($act2), VimMode::Normal)
    };
}

macro_rules! tab_open {
    ($target: expr, $fc: expr) => {
        tab!(TabAction::Open($target, $fc))
    };
}

macro_rules! tab_focus {
    ($fc: expr) => {
        tab!(TabAction::Focus($fc))
    };
    ($fc1: expr, $fc2: expr) => {
        tab!(TabAction::Focus($fc1), TabAction::Focus($fc2))
    };
}

macro_rules! window {
    ($act: expr) => {
        act!(Action::Window($act), VimMode::Normal)
    };
    ($act1: expr, $act2: expr) => {
        count_alters!(Action::Window($act1), Action::Window($act2), VimMode::Normal)
    };
}

macro_rules! window_switch {
    ($st: expr) => {
        window!(WindowAction::Switch($st))
    };
    ($st1: expr, $st2: expr) => {
        window!(WindowAction::Switch($st1), WindowAction::Switch($st2))
    };
}

macro_rules! window_resize {
    ($axis: expr, $change: expr) => {
        window!(WindowAction::Resize(FocusChange::Current, $axis, $change))
    };
}

macro_rules! window_clear_size {
    () => {
        window!(WindowAction::ClearSizes)
    };
}

macro_rules! window_exchange {
    ($fc: expr) => {
        window!(WindowAction::Exchange($fc))
    };
    ($fc1: expr, $fc2: expr) => {
        window!(WindowAction::Exchange($fc1), WindowAction::Exchange($fc2))
    };
}

macro_rules! window_focus {
    ($fc: expr) => {
        window!(WindowAction::Focus($fc))
    };
    ($fc1: expr, $fc2: expr) => {
        window!(WindowAction::Focus($fc1), WindowAction::Focus($fc2))
    };
}

macro_rules! window_close_one {
    ($style: expr, $fc: expr, $flags: expr) => {
        window!(WindowAction::Close($style($fc), $flags))
    };
}

macro_rules! window_close {
    ($style: expr, $f1: expr, $f2: expr) => {
        window!(
            WindowAction::Close($style($f1), CloseFlags::NONE),
            WindowAction::Close($style($f2), CloseFlags::NONE)
        )
    };
    ($style: expr, $f1: expr, $f2: expr, $flags: expr) => {
        window!(WindowAction::Close($style($f1), $flags), WindowAction::Close($style($f2), $flags))
    };
}

macro_rules! window_quit {
    ($style: expr, $f1: expr, $f2: expr) => {
        window_close!($style, $f1, $f2, CloseFlags::QUIT)
    };
}

macro_rules! window_split {
    ($axis: expr) => {
        isv!(
            vec![],
            vec![ExternalAction::CountAlters(
                vec![WindowAction::Split(
                    OpenTarget::Current,
                    $axis,
                    MoveDir1D::Previous,
                    Count::Contextual
                )
                .into()],
                vec![WindowAction::Open(
                    OpenTarget::Current,
                    $axis,
                    MoveDir1D::Previous,
                    Count::Contextual
                )
                .into()],
            )],
            VimMode::Normal
        )
    };
}

macro_rules! window_file {
    ($target: expr) => {
        window!(WindowAction::Split($target, Axis::Horizontal, MoveDir1D::Previous, 1.into()))
    };
}

macro_rules! cmdbar_focus {
    ($type: expr, $mode: expr) => {
        isv!(
            vec![],
            vec![ExternalAction::CommandEnter(
                ":".into(),
                $type,
                CommandAction::Execute(1.into()).into(),
                $mode
            )],
            VimMode::Command
        )
    };
}

macro_rules! search {
    ($dir: expr, $mode: expr) => {
        isv!(
            vec![InternalAction::SetSearchRegexParams($dir, false)],
            vec![ExternalAction::CommandEnter(
                match $dir {
                    MoveDir1D::Next => "/".into(),
                    MoveDir1D::Previous => "?".into(),
                },
                CommandType::Search,
                EditorAction::Edit(
                    Specifier::Contextual,
                    EditTarget::Search(SearchType::Regex, MoveDirMod::Same, Count::Contextual)
                )
                .into(),
                $mode
            )],
            VimMode::Command
        )
    };
}

macro_rules! command_exit {
    ($act: expr) => {
        isv!(vec![], vec![ExternalAction::CommandExit($act)])
    };
}

macro_rules! command_unfocus {
    () => {
        command_exit!(PromptAction::Abort(false))
    };
}

#[rustfmt::skip]
fn default_keys<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // Normal, Visual, Select, Insert mode keys
        ( NVIMAP, "<C-\\><C-N>", normal!() ),
        ( NVIMAP, "<C-4><C-N>", normal!() ),
        ( NVIMAP, "<C-End>", edit_target_end!(EditTarget::Boundary(RangeType::Buffer, true, MoveTerminus::End, Count::Contextual)) ),
        ( NVIMAP, "<C-PageDown>", tab_focus!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), true), FocusChange::Offset(Count::Contextual, false)) ),
        ( NVIMAP, "<C-PageUp>", tab_focus!(FocusChange::Direction1D(MoveDir1D::Previous, Count::Contextual, true)) ),

        // Normal, Visual, Select, Operation Pending mode keys
        ( MAP, "<C-H>", edit_end!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( MAP, "<C-J>", edit_end!(MoveType::Line(MoveDir1D::Next)) ),
        ( MAP, "<C-N>", edit_end!(MoveType::Line(MoveDir1D::Next)) ),
        ( MAP, "<C-P>", edit_end!(MoveType::Line(MoveDir1D::Previous)) ),
        ( MAP, "<C-?>", edit_end!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( MAP, "<Up>", edit_end!(MoveType::Line(MoveDir1D::Previous)) ),
        ( MAP, "<Down>", edit_end!(MoveType::Line(MoveDir1D::Next)) ),
        ( MAP, "<Left>", edit_end!(MoveType::Column(MoveDir1D::Previous, false)) ),
        ( MAP, "<Right>", edit_end!(MoveType::Column(MoveDir1D::Next, false)) ),
        ( MAP, "<S-Left>", edit_end!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( MAP, "<S-Right>", edit_end!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next)) ),
        ( MAP, "<C-Left>", edit_end!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous)) ),
        ( MAP, "<C-Right>", edit_end!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next)) ),
        ( MAP, "<BS>", edit_end!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( MAP, "<End>", edit_end!(MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( MAP, "<Home>", edit_end!(MoveType::LinePos(MovePosition::Beginning), 0) ),

        // Normal, Visual, Operation Pending mode keys
        ( NXOMAP, "0", edit_end!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( NXOMAP, "b", edit_end!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( NXOMAP, "B", edit_end!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous)) ),
        ( NXOMAP, "e", edit_end!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( NXOMAP, "E", edit_end!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next)) ),
        ( NXOMAP, "f", charsearch!(MoveDir1D::Next, true) ),
        ( NXOMAP, "F", charsearch!(MoveDir1D::Previous, true) ),
        ( NXOMAP, "g0", edit_end!(MoveType::ScreenLinePos(MovePosition::Beginning), 0) ),
        ( NXOMAP, "ge", edit_end!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Previous)) ),
        ( NXOMAP, "gE", edit_end!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Previous)) ),
        ( NXOMAP, "gg", edit_end_ca!(MoveType::BufferPos(MovePosition::Beginning), MoveType::BufferLineOffset) ),
        ( NXOMAP, "gj", edit_end!(MoveType::ScreenLine(MoveDir1D::Next)) ),
        ( NXOMAP, "gk", edit_end!(MoveType::ScreenLine(MoveDir1D::Previous)) ),
        ( NXOMAP, "gm", edit_end!(MoveType::ScreenLinePos(MovePosition::Middle), 0) ),
        ( NXOMAP, "gM", edit_target_end_ca!(EditTarget::Motion(MoveType::LinePos(MovePosition::Middle), Count::MinusOne), EditTarget::Motion(MoveType::LinePercent, Count::Contextual)) ),
        ( NXOMAP, "go", edit_end!(MoveType::BufferByteOffset) ),
        ( NXOMAP, "g_", edit_end!(MoveType::FinalNonBlank(MoveDir1D::Next), Count::MinusOne) ),
        ( NXOMAP, "g^", edit_end!(MoveType::ScreenFirstWord(MoveDir1D::Next), 0) ),
        ( NXOMAP, "g$", edit_end!(MoveType::ScreenLinePos(MovePosition::End), Count::MinusOne) ),
        ( NXOMAP, "g#", edit_word_search_end!(WordStyle::Little, false, MoveDir1D::Previous) ),
        ( NXOMAP, "g*", edit_word_search_end!(WordStyle::Little, false, MoveDir1D::Next) ),
        ( NXOMAP, "g'{mark}", unmapped!() ),
        ( NXOMAP, "g`{mark}", unmapped!() ),
        ( NXOMAP, "g<Down>", edit_end!(MoveType::ScreenLine(MoveDir1D::Next)) ),
        ( NXOMAP, "g<Up>", edit_end!(MoveType::ScreenLine(MoveDir1D::Previous)) ),
        ( NXOMAP, "g<End>", edit_end!(MoveType::ScreenLinePos(MovePosition::End), Count::MinusOne) ),
        ( NXOMAP, "g<Home>", edit_end!(MoveType::ScreenLinePos(MovePosition::Beginning), 1) ),
        ( NXOMAP, "G", edit_end_ca!(MoveType::BufferPos(MovePosition::End), MoveType::BufferLineOffset) ),
        ( NXOMAP, "h", edit_end!(MoveType::Column(MoveDir1D::Previous, false)) ),
        ( NXOMAP, "H", edit_end!(MoveType::ViewportPos(MovePosition::Beginning)) ),
        ( NXOMAP, "j", edit_end!(MoveType::Line(MoveDir1D::Next)) ),
        ( NXOMAP, "k", edit_end!(MoveType::Line(MoveDir1D::Previous)) ),
        ( NXOMAP, "l", edit_end!(MoveType::Column(MoveDir1D::Next, false)) ),
        ( NXOMAP, "L", edit_end!(MoveType::ViewportPos(MovePosition::End)) ),
        ( NXOMAP, "M", edit_end!(MoveType::ViewportPos(MovePosition::Middle)) ),
        ( NXOMAP, "t", charsearch!(MoveDir1D::Next, false) ),
        ( NXOMAP, "T", charsearch!(MoveDir1D::Previous, false) ),
        ( NXOMAP, "w", edit_end!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next)) ),
        ( NXOMAP, "W", edit_end!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next)) ),
        ( NXOMAP, "`{mark}", edit_target_end!(EditTarget::CharJump(Specifier::Contextual)) ),
        ( NXOMAP, "'{mark}", edit_target_end!(EditTarget::LineJump(Specifier::Contextual)) ),
        ( NXOMAP, " ", edit_end!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( NXOMAP, "^", edit_end_shaped!(TargetShape::CharWise, MoveType::FirstWord(MoveDir1D::Next), 0) ),
        ( NXOMAP, "$", edit_end!(MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( NXOMAP, "_", edit_end!(MoveType::FirstWord(MoveDir1D::Next), Count::MinusOne) ),
        ( NXOMAP, "-", edit_end!(MoveType::FirstWord(MoveDir1D::Previous)) ),
        ( NXOMAP, "+", edit_end!(MoveType::FirstWord(MoveDir1D::Next)) ),
        ( NXOMAP, "%", edit_end_ca!(MoveType::ItemMatch, MoveType::BufferLinePercent) ),
        ( NXOMAP, "#", edit_word_search_end!(WordStyle::Little, true, MoveDir1D::Previous) ),
        ( NXOMAP, "*", edit_word_search_end!(WordStyle::Little, true, MoveDir1D::Next) ),
        ( NXOMAP, "|", edit_end!(MoveType::LineColumnOffset) ),
        ( NXOMAP, ";", edit_search_end!(SearchType::Char(false), MoveDirMod::Same) ),
        ( NXOMAP, ",", edit_search_end!(SearchType::Char(false), MoveDirMod::Flip) ),
        ( NXOMAP, "(", edit_end!(MoveType::SentenceBegin(MoveDir1D::Previous)) ),
        ( NXOMAP, ")", edit_end!(MoveType::SentenceBegin(MoveDir1D::Next)) ),
        ( NXOMAP, "{", edit_end!(MoveType::ParagraphBegin(MoveDir1D::Previous)) ),
        ( NXOMAP, "}", edit_end!(MoveType::ParagraphBegin(MoveDir1D::Next)) ),
        ( NXOMAP, "[[", edit_end!(MoveType::SectionBegin(MoveDir1D::Previous)) ),
        ( NXOMAP, "[]", edit_end!(MoveType::SectionEnd(MoveDir1D::Previous)) ),
        ( NXOMAP, "][", edit_end!(MoveType::SectionEnd(MoveDir1D::Next)) ),
        ( NXOMAP, "]]", edit_end!(MoveType::SectionBegin(MoveDir1D::Next)) ),
        ( NXOMAP, "[(", unmapped!() ),
        ( NXOMAP, "[{", unmapped!() ),
        ( NXOMAP, "])", unmapped!() ),
        ( NXOMAP, "]}", unmapped!() ),
        ( NXOMAP, "]'", unmapped!() ),
        ( NXOMAP, "]`", unmapped!() ),
        ( NXOMAP, "['", unmapped!() ),
        ( NXOMAP, "[`", unmapped!() ),

        // Normal, Visual, Select mode keys
        ( NVMAP, "<C-B>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),
        ( NVMAP, "<C-E>", scroll2d!(MoveDir2D::Down, ScrollSize::Cell) ),
        ( NVMAP, "<C-F>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( NVMAP, "<C-U>", scroll2d!(MoveDir2D::Up, ScrollSize::HalfPage) ),
        ( NVMAP, "<C-V>", visual!(TargetShape::BlockWise) ),
        ( NVMAP, "<C-W>b", window_focus!(FocusChange::Position(MovePosition::End)) ),
        ( NVMAP, "<C-W>c", window_close!(WindowTarget::Single, FocusChange::Current, FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W>d", unmapped!() ),
        ( NVMAP, "<C-W>g<Tab>", tab_focus!(FocusChange::PreviouslyFocused) ),
        ( NVMAP, "<C-W>h", window_focus!(FocusChange::Direction2D(MoveDir2D::Left, Count::Contextual)) ),
        ( NVMAP, "<C-W>H", window!(WindowAction::MoveSide(MoveDir2D::Left)) ),
        ( NVMAP, "<C-W>i", unmapped!() ),
        ( NVMAP, "<C-W>j", window_focus!(FocusChange::Direction2D(MoveDir2D::Down, Count::Contextual)) ),
        ( NVMAP, "<C-W>J", window!(WindowAction::MoveSide(MoveDir2D::Down)) ),
        ( NVMAP, "<C-W>k", window_focus!(FocusChange::Direction2D(MoveDir2D::Up, Count::Contextual)) ),
        ( NVMAP, "<C-W>K", window!(WindowAction::MoveSide(MoveDir2D::Up)) ),
        ( NVMAP, "<C-W>l", window_focus!(FocusChange::Direction2D(MoveDir2D::Right, Count::Contextual)) ),
        ( NVMAP, "<C-W>L", window!(WindowAction::MoveSide(MoveDir2D::Right)) ),
        ( NVMAP, "<C-W>n", window_file!(OpenTarget::Unnamed) ),
        ( NVMAP, "<C-W>o", window_quit!(WindowTarget::AllBut, FocusChange::Current, FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W>p", window_focus!(FocusChange::PreviouslyFocused) ),
        ( NVMAP, "<C-W>q", window_quit!(WindowTarget::Single, FocusChange::Current, FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W>r", window!(WindowAction::Rotate(MoveDir1D::Next)) ),
        ( NVMAP, "<C-W>R", window!(WindowAction::Rotate(MoveDir1D::Previous)) ),
        ( NVMAP, "<C-W>s", window_split!(Axis::Horizontal) ),
        ( NVMAP, "<C-W>S", window_split!(Axis::Horizontal) ),
        ( NVMAP, "<C-W>t", window_focus!(FocusChange::Position(MovePosition::Beginning)) ),
        ( NVMAP, "<C-W>T", tab!(TabAction::Extract(FocusChange::Current, MoveDir1D::Next), TabAction::Extract(FocusChange::Offset(Count::Contextual, false), MoveDir1D::Previous)) ),
        ( NVMAP, "<C-W>v", window_split!(Axis::Vertical) ),
        ( NVMAP, "<C-W>w", window_focus!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), true), FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W>W", window_focus!(FocusChange::Direction1D(MoveDir1D::Previous, Count::Exact(1), true), FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W>x", window_exchange!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), false), FocusChange::Offset(Count::Contextual, false)) ),
        ( NVMAP, "<C-W>z", unmapped!() ),
        ( NVMAP, "<C-W>=", window_clear_size!() ),
        ( NVMAP, "<C-W>-", window_resize!(Axis::Horizontal, SizeChange::Decrease(Count::Contextual)) ),
        ( NVMAP, "<C-W>+", window_resize!(Axis::Horizontal, SizeChange::Increase(Count::Contextual)) ),
        ( NVMAP, "<C-W>_", window_resize!(Axis::Horizontal, SizeChange::Exact(Count::Contextual)) ),
        ( NVMAP, "<C-W><", window_resize!(Axis::Vertical, SizeChange::Decrease(Count::Contextual)) ),
        ( NVMAP, "<C-W>>", window_resize!(Axis::Vertical, SizeChange::Increase(Count::Contextual)) ),
        ( NVMAP, "<C-W>|", window_resize!(Axis::Vertical, SizeChange::Exact(Count::Contextual)) ),
        ( NVMAP, "<C-W><C-B>", window_focus!(FocusChange::Position(MovePosition::End)) ),
        ( NVMAP, "<C-W><C-C>", normal!() ),
        ( NVMAP, "<C-W><C-D>", unmapped!() ),
        ( NVMAP, "<C-W><C-I>", unmapped!() ),
        ( NVMAP, "<C-W><C-H>", window_focus!(FocusChange::Direction2D(MoveDir2D::Left, Count::Contextual)) ),
        ( NVMAP, "<C-W><C-J>", window_focus!(FocusChange::Direction2D(MoveDir2D::Down, Count::Contextual)) ),
        ( NVMAP, "<C-W><C-K>", window_focus!(FocusChange::Direction2D(MoveDir2D::Up, Count::Contextual)) ),
        ( NVMAP, "<C-W><C-L>", window_focus!(FocusChange::Direction2D(MoveDir2D::Right, Count::Contextual)) ),
        ( NVMAP, "<C-W><C-N>", window_file!(OpenTarget::Unnamed) ),
        ( NVMAP, "<C-W><C-O>", window_quit!(WindowTarget::AllBut, FocusChange::Current, FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W><C-Q>", window_quit!(WindowTarget::Single, FocusChange::Current, FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W><C-R>", window!(WindowAction::Rotate(MoveDir1D::Next)) ),
        ( NVMAP, "<C-W><C-S>", window_split!(Axis::Horizontal) ),
        ( NVMAP, "<C-W><C-T>", window_focus!(FocusChange::Position(MovePosition::Beginning)) ),
        ( NVMAP, "<C-W><C-V>", window_split!(Axis::Vertical) ),
        ( NVMAP, "<C-W><C-W>", window_focus!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), true), FocusChange::Offset(Count::Contextual, true)) ),
        ( NVMAP, "<C-W><C-X>", window_exchange!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), false), FocusChange::Offset(Count::Contextual, false)) ),
        ( NVMAP, "<C-W><C-Z>", unmapped!() ),
        ( NVMAP, "<C-W><Left>", window_focus!(FocusChange::Direction2D(MoveDir2D::Left, Count::Contextual)) ),
        ( NVMAP, "<C-W><Down>", window_focus!(FocusChange::Direction2D(MoveDir2D::Down, Count::Contextual)) ),
        ( NVMAP, "<C-W><Up>", window_focus!(FocusChange::Direction2D(MoveDir2D::Up, Count::Contextual)) ),
        ( NVMAP, "<C-W><Right>", window_focus!(FocusChange::Direction2D(MoveDir2D::Right, Count::Contextual)) ),
        ( NVMAP, "<C-Y>", scroll2d!(MoveDir2D::Up, ScrollSize::Cell) ),
        ( NVMAP, "<S-Up>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),
        ( NVMAP, "<S-Down>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( NVMAP, "<PageDown>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( NVMAP, "<PageUp>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),

        // Normal, Visual mode keys
        ( NXMAP, "gh", select!(TargetShape::CharWise) ),
        ( NXMAP, "gH", select!(TargetShape::LineWise) ),
        ( NXMAP, "gt", tab_focus!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), true), FocusChange::Offset(Count::Contextual, false)) ),
        ( NXMAP, "gT", tab_focus!(FocusChange::Direction1D(MoveDir1D::Previous, Count::Contextual, true)) ),
        ( NXMAP, "g<C-H>", select!(TargetShape::BlockWise) ),
        ( NXMAP, "m{mark}", act!(EditorAction::Mark(Specifier::Contextual)) ),
        ( NXMAP, "q{register}", isv!(vec![], vec![ExternalAction::MacroToggle(false)]) ),
        ( NXMAP, "q", isv!(vec![], vec![ExternalAction::MacroToggle(true)]) ),
        ( NXMAP, "v", visual!(TargetShape::CharWise) ),
        ( NXMAP, "V", visual!(TargetShape::LineWise) ),
        ( NXMAP, "zb", scrollcpv!(MovePosition::End, false) ),
        ( NXMAP, "ze", scrollcph!(MovePosition::End) ),
        ( NXMAP, "zg", unmapped!() ),
        ( NXMAP, "zG", unmapped!() ),
        ( NXMAP, "zh", scroll2d!(MoveDir2D::Left, ScrollSize::Cell) ),
        ( NXMAP, "zH", scroll2d!(MoveDir2D::Left, ScrollSize::HalfPage) ),
        ( NXMAP, "zl", scroll2d!(MoveDir2D::Right, ScrollSize::Cell) ),
        ( NXMAP, "zL", scroll2d!(MoveDir2D::Right, ScrollSize::HalfPage) ),
        ( NXMAP, "zp", unmapped!() ),
        ( NXMAP, "zP", unmapped!() ),
        ( NXMAP, "zs", scrollcph!(MovePosition::Beginning) ),
        ( NXMAP, "zt", scrollcpv!(MovePosition::Beginning, false) ),
        ( NXMAP, "zug", unmapped!() ),
        ( NXMAP, "zuG", unmapped!() ),
        ( NXMAP, "zuw", unmapped!() ),
        ( NXMAP, "zuW", unmapped!() ),
        ( NXMAP, "zw", unmapped!() ),
        ( NXMAP, "zW", unmapped!() ),
        ( NXMAP, "zy", unmapped!() ),
        ( NXMAP, "zz", scrollcpv!(MovePosition::Middle, false) ),
        ( NXMAP, "z+", unmapped!() ),
        ( NXMAP, "z-", scrollcpv!(MovePosition::End, true) ),
        ( NXMAP, "z.", scrollcpv!(MovePosition::Middle, true) ),
        ( NXMAP, "z=", unmapped!() ),
        ( NXMAP, "z^", unmapped!() ),
        ( NXMAP, "z<Left>", scroll2d!(MoveDir2D::Left, ScrollSize::Cell) ),
        ( NXMAP, "z<Right>", scroll2d!(MoveDir2D::Right, ScrollSize::Cell) ),
        ( NXMAP, "z<Enter>", scrollcpv!(MovePosition::Beginning, true) ),
        ( NXMAP, "z{count}<Enter>", window_resize!(Axis::Horizontal, SizeChange::Exact(Count::Contextual)) ),
        ( NXMAP, ":", cmdbar_focus!(CommandType::Command, VimMode::Normal) ),

        // Visual, Operator Pending mode keys
        ( VOMAP, "aw", edit_range_end!(RangeType::Word(WordStyle::Little)) ),
        ( VOMAP, "iw", edit_range_end!(RangeType::Word(WordStyle::Little)) ),
        ( VOMAP, "aW", edit_range_end!(RangeType::Word(WordStyle::Big)) ),
        ( VOMAP, "iW", edit_range_end!(RangeType::Word(WordStyle::Big)) ),
        ( VOMAP, "as", edit_range_end!(RangeType::Sentence) ),
        ( VOMAP, "is", edit_range_end!(RangeType::Sentence) ),
        ( VOMAP, "ap", edit_range_end!(RangeType::Paragraph) ),
        ( VOMAP, "ip", edit_range_end!(RangeType::Paragraph) ),
        ( VOMAP, "a]", edit_range_end!(RangeType::Bracketed('[', ']'), true) ),
        ( VOMAP, "a[", edit_range_end!(RangeType::Bracketed('[', ']'), true) ),
        ( VOMAP, "i]", edit_range_end!(RangeType::Bracketed('[', ']'), false) ),
        ( VOMAP, "i[", edit_range_end!(RangeType::Bracketed('[', ']'), false) ),
        ( VOMAP, "a)", edit_range_end!(RangeType::Bracketed('(', ')'), true) ),
        ( VOMAP, "a(", edit_range_end!(RangeType::Bracketed('(', ')'), true) ),
        ( VOMAP, "ab", edit_range_end!(RangeType::Bracketed('(', ')'), true) ),
        ( VOMAP, "i)", edit_range_end!(RangeType::Bracketed('(', ')'), false) ),
        ( VOMAP, "i(", edit_range_end!(RangeType::Bracketed('(', ')'), false) ),
        ( VOMAP, "ib", edit_range_end!(RangeType::Bracketed('(', ')'), false) ),
        ( VOMAP, "a>", edit_range_end!(RangeType::Bracketed('<', '>'), true) ),
        ( VOMAP, "a<", edit_range_end!(RangeType::Bracketed('<', '>'), true) ),
        ( VOMAP, "i>", edit_range_end!(RangeType::Bracketed('<', '>'), false) ),
        ( VOMAP, "i<", edit_range_end!(RangeType::Bracketed('<', '>'), false) ),
        ( VOMAP, "at", edit_range_end!(RangeType::XmlTag, true) ),
        ( VOMAP, "it", edit_range_end!(RangeType::XmlTag, false) ),
        ( VOMAP, "a}", edit_range_end!(RangeType::Bracketed('{', '}'), true) ),
        ( VOMAP, "a{", edit_range_end!(RangeType::Bracketed('{', '}'), true) ),
        ( VOMAP, "aB", edit_range_end!(RangeType::Bracketed('{', '}'), true) ),
        ( VOMAP, "i}", edit_range_end!(RangeType::Bracketed('{', '}'), false) ),
        ( VOMAP, "i{", edit_range_end!(RangeType::Bracketed('{', '}'), false) ),
        ( VOMAP, "iB", edit_range_end!(RangeType::Bracketed('{', '}'), false) ),
        ( VOMAP, "a\"", edit_range_end!(RangeType::Quote('\"'), true) ),
        ( VOMAP, "i\"", edit_range_end!(RangeType::Quote('\"'), false) ),
        ( VOMAP, "a\'", edit_range_end!(RangeType::Quote('\''), true) ),
        ( VOMAP, "i\'", edit_range_end!(RangeType::Quote('\''), false) ),
        ( VOMAP, "a`", edit_range_end!(RangeType::Quote('`'), true) ),
        ( VOMAP, "i`", edit_range_end!(RangeType::Quote('`'), false) ),

        // Normal mode keys
        ( NMAP, "a", insert!(InsertStyle::Insert, MoveType::Column(MoveDir1D::Next, false)) ),
        ( NMAP, "A", insert!(InsertStyle::Insert, MoveType::LinePos(MovePosition::End), 0) ),
        ( NMAP, "c", edit_motion!(EditAction::Delete, VimMode::Insert, InsertStyle::Insert) ),
        ( NMAP, "cc", change_range!(RangeType::Line) ),
        ( NMAP, "cw", edit_end!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( NMAP, "cW", edit_end!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next)) ),
        ( NMAP, "C", change!(MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( NMAP, "d", edit_motion!(EditAction::Delete) ),
        ( NMAP, "dd", edit_lines!(EditAction::Delete) ),
        ( NMAP, "D", edit!(EditAction::Delete, MoveType::LinePos(MovePosition::End), Count::MinusOne) ),
        ( NMAP, "ga", unmapped!() ),
        ( NMAP, "gi", unmapped!() ),
        ( NMAP, "gI", insert!(InsertStyle::Insert, MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( NMAP, "gJ", edit_lines!(EditAction::Join(JoinStyle::NoChange)) ),
        ( NMAP, "gn", selection_resize_search!(SelectionResizeStyle::Object, MoveDir1D::Next) ),
        ( NMAP, "gN", selection_resize_search!(SelectionResizeStyle::Object, MoveDir1D::Previous) ),
        ( NMAP, "gq", edit_motion!(EditAction::Format) ),
        ( NMAP, "gqgq", edit_lines!(EditAction::Format) ),
        ( NMAP, "gqq", edit_lines!(EditAction::Format) ),
        ( NMAP, "gr", charreplace!(true) ),
        ( NMAP, "gR", unmapped!() ),
        ( NMAP, "gu", edit_motion!(EditAction::ChangeCase(Case::Lower)) ),
        ( NMAP, "gugu", edit_lines!(EditAction::ChangeCase(Case::Lower)) ),
        ( NMAP, "guu", edit_lines!(EditAction::ChangeCase(Case::Lower)) ),
        ( NMAP, "gU", edit_motion!(EditAction::ChangeCase(Case::Upper)) ),
        ( NMAP, "gUgU", edit_lines!(EditAction::ChangeCase(Case::Upper)) ),
        ( NMAP, "gUU", edit_lines!(EditAction::ChangeCase(Case::Upper)) ),
        ( NMAP, "gv", unmapped!() ),
        ( NMAP, "gw", edit_motion!(EditAction::Format) ),
        ( NMAP, "gwgw", edit_lines!(EditAction::Format) ),
        ( NMAP, "gww", edit_lines!(EditAction::Format) ),
        ( NMAP, "g8", unmapped!() ),
        ( NMAP, "g&", unmapped!() ),
        ( NMAP, "g~", edit_motion!(EditAction::ChangeCase(Case::Toggle)) ),
        ( NMAP, "g~~", edit_lines!(EditAction::ChangeCase(Case::Toggle)) ),
        ( NMAP, "g,", jump!(PositionList::ChangeList, MoveDir1D::Next) ),
        ( NMAP, "g;", jump!(PositionList::ChangeList, MoveDir1D::Previous) ),
        ( NMAP, "g<Tab>", tab_focus!(FocusChange::PreviouslyFocused) ),
        ( NMAP, "i", insert!(InsertStyle::Insert) ),
        ( NMAP, "I", insert!(InsertStyle::Insert, MoveType::FirstWord(MoveDir1D::Next), 0) ),
        ( NMAP, "J", edit_lines!(EditAction::Join(JoinStyle::OneSpace)) ),
        ( NMAP, "o", open_lines!(MoveDir1D::Next) ),
        ( NMAP, "O", open_lines!(MoveDir1D::Previous) ),
        ( NMAP, "p", paste_dir!(MoveDir1D::Next) ),
        ( NMAP, "P", paste_dir!(MoveDir1D::Previous) ),
        ( NMAP, "Q", unmapped!() ),
        ( NMAP, "r", charreplace!(false) ),
        ( NMAP, "R", insert!(InsertStyle::Replace) ),
        ( NMAP, "s", change!(MoveType::Column(MoveDir1D::Next, false)) ),
        ( NMAP, "S", change_range!(RangeType::Line) ),
        ( NMAP, "u", history!(HistoryAction::Undo(Count::Contextual)) ),
        ( NMAP, "U", unmapped!() ),
        ( NMAP, "x", edit!(EditAction::Delete, MoveType::Column(MoveDir1D::Next, false)) ),
        ( NMAP, "X", edit!(EditAction::Delete, MoveType::Column(MoveDir1D::Previous, false)) ),
        ( NMAP, "y", edit_motion!(EditAction::Yank) ),
        ( NMAP, "yy", edit_lines!(EditAction::Yank) ),
        ( NMAP, "Y", edit_lines!(EditAction::Yank) ),
        ( NMAP, "ZZ", window_close_one!(WindowTarget::Single, FocusChange::Current, CloseFlags::WQ) ),
        ( NMAP, "ZQ", window_close_one!(WindowTarget::Single, FocusChange::Current, CloseFlags::FQ) ),
        ( NMAP, "=", edit_motion!(EditAction::Indent(IndentChange::Auto)) ),
        ( NMAP, "==", edit_lines!(EditAction::Indent(IndentChange::Auto)) ),
        ( NMAP, "<", edit_motion!(EditAction::Indent(IndentChange::Decrease(Count::Exact(1)))) ),
        ( NMAP, "<<", edit_lines!(EditAction::Indent(IndentChange::Decrease(Count::Exact(1)))) ),
        ( NMAP, ">", edit_motion!(EditAction::Indent(IndentChange::Increase(Count::Exact(1)))) ),
        ( NMAP, ">>", edit_lines!(EditAction::Indent(IndentChange::Increase(Count::Exact(1)))) ),
        ( NMAP, "?", search!(MoveDir1D::Previous, VimMode::Normal) ),
        ( NMAP, "/", search!(MoveDir1D::Next, VimMode::Normal) ),
        ( NMAP, "~", tilde!() ),
        ( NMAP, ".", act!(Action::Repeat(RepeatType::EditSequence)) ),
        ( NMAP, "@{register}", act!(MacroAction::Execute(Count::Contextual)) ),
        ( NMAP, "@:", command!(CommandAction::Execute(Count::Contextual)) ),
        ( NMAP, "@@", act!(MacroAction::Repeat(Count::Contextual)) ),
        ( NMAP, "<C-A>", edit_target!(EditAction::ChangeNumber(NumberChange::Increase(Count::Contextual), false), EditTarget::Motion(MoveType::LinePos(MovePosition::End), 0.into())) ),
        ( NMAP, "<C-I>", jump!(PositionList::JumpList, MoveDir1D::Next) ),
        ( NMAP, "<C-G>", unmapped!() ),
        ( NMAP, "<C-L>", act!(Action::RedrawScreen) ),
        ( NMAP, "<C-O>", jump!(PositionList::JumpList, MoveDir1D::Previous) ),
        ( NMAP, "<C-R>", history!(HistoryAction::Redo(Count::Contextual)) ),
        ( NMAP, "<C-T>", unmapped!() ),
        ( NMAP, "<C-X>", edit_target!(EditAction::ChangeNumber(NumberChange::Decrease(Count::Contextual), false), EditTarget::Motion(MoveType::LinePos(MovePosition::End), 0.into())) ),
        ( NMAP, "<C-Z>", act!(Action::Suspend) ),
        ( NMAP, "<C-[>", normal!() ),
        ( NMAP, "<C-^>", window_switch!(OpenTarget::Alternate, OpenTarget::List(Count::Contextual)) ),
        ( NMAP, "<C-6>", window_switch!(OpenTarget::Alternate, OpenTarget::List(Count::Contextual)) ),
        ( NMAP, "<Del>", edit_nocount!(EditAction::Delete, MoveType::Column(MoveDir1D::Next, false)) ),
        ( NMAP, "<Esc>", normal!() ),
        ( NMAP, "<Insert>", insert!(InsertStyle::Insert) ),
        ( NMAP, "<Tab>", jump!(PositionList::JumpList, MoveDir1D::Next) ),

        // Visual, Select mode keys
        ( VMAP, "<C-A>", edit_selection!(EditAction::ChangeNumber(NumberChange::Increase(Count::Contextual), false)) ),
        ( VMAP, "<C-D>", scroll2d!(MoveDir2D::Down, ScrollSize::HalfPage) ),
        ( VMAP, "<C-C>", normal!() ),
        ( VMAP, "<C-L>", act!(Action::RedrawScreen) ),
        ( VMAP, "<C-X>", edit_selection!(EditAction::ChangeNumber(NumberChange::Decrease(Count::Contextual), false)) ),
        ( VMAP, "<C-Z>", act!(Action::Suspend) ),
        ( VMAP, "<Del>", edit_selection_nocount!(EditAction::Delete) ),
        ( VMAP, "<C-[>", normal!() ),
        ( VMAP, "<Esc>", normal!() ),

        // Visual mode keys
        ( XMAP, "A", insert_visual!(SelectionCursorChange::End, MoveType::Column(MoveDir1D::Next, false), 1.into()) ),
        ( XMAP, "c", change_visual!(SelectionCursorChange::Beginning, EditTarget::Selection) ),
        ( XMAP, "C", change_selection_nochar!(SelectionCursorChange::Beginning, EditTarget::Motion(MoveType::LinePos(MovePosition::End), Count::Exact(0))) ),
        ( XMAP, "d", edit_selection!(EditAction::Delete) ),
        ( XMAP, "D", delete_selection_nochar!(SelectionCursorChange::Beginning, EditTarget::Motion(MoveType::LinePos(MovePosition::End), Count::Exact(0))) ),
        ( XMAP, "gf", window_switch!(OpenTarget::Selection) ),
        ( XMAP, "gJ", edit_selection!(EditAction::Join(JoinStyle::NoChange)) ),
        ( XMAP, "gn", selection_resize_search!(SelectionResizeStyle::Extend, MoveDir1D::Next) ),
        ( XMAP, "gN", selection_resize_search!(SelectionResizeStyle::Extend, MoveDir1D::Previous) ),
        ( XMAP, "gq", edit_selection!(EditAction::Format) ),
        ( XMAP, "gr", charreplace!(true, EditTarget::Selection) ),
        ( XMAP, "gu", edit_selection!(EditAction::ChangeCase(Case::Lower)) ),
        ( XMAP, "gU", edit_selection!(EditAction::ChangeCase(Case::Upper)) ),
        ( XMAP, "gw", edit_selection!(EditAction::Format) ),
        ( XMAP, "g~", edit_selection!(EditAction::ChangeCase(Case::Toggle)) ),
        ( XMAP, "g<C-A>", edit_selection!(EditAction::ChangeNumber(NumberChange::Increase(Count::Contextual), true)) ),
        ( XMAP, "g<C-X>", edit_selection!(EditAction::ChangeNumber(NumberChange::Decrease(Count::Contextual), true)) ),
        ( XMAP, "I", insert_visual!(SelectionCursorChange::Beginning) ),
        ( XMAP, "J", edit_selection!(EditAction::Join(JoinStyle::OneSpace)) ),
        ( XMAP, "o", selection!(SelectionAction::CursorSet(SelectionCursorChange::SwapAnchor)) ),
        ( XMAP, "O", selection!(SelectionAction::CursorSet(SelectionCursorChange::SwapSide)) ),
        ( XMAP, "p", paste!(PasteStyle::Replace, Count::Contextual, VimMode::Normal) ),
        ( XMAP, "P", paste!(PasteStyle::Replace, Count::Contextual, VimMode::Normal) ),
        ( XMAP, "r", charreplace!(false, EditTarget::Selection) ),
        ( XMAP, "R", change_selection_lines!() ),
        ( XMAP, "S", change_selection_lines!() ),
        ( XMAP, "u", edit_selection!(EditAction::ChangeCase(Case::Lower)) ),
        ( XMAP, "U", edit_selection!(EditAction::ChangeCase(Case::Upper)) ),
        ( XMAP, "v", visual!(TargetShape::CharWise) ),
        ( XMAP, "V", visual!(TargetShape::LineWise) ),
        ( XMAP, "x", edit_selection!(EditAction::Delete) ),
        ( XMAP, "X", delete_selection_nochar!(SelectionCursorChange::Beginning, EditTarget::Selection) ),
        ( XMAP, "y", edit_selection!(EditAction::Yank) ),
        ( XMAP, "Y", edit_selection_nochar!(EditAction::Yank) ),
        ( XMAP, "~", edit_selection!(EditAction::ChangeCase(Case::Toggle)) ),
        ( XMAP, "=", edit_selection!(EditAction::Indent(IndentChange::Auto)) ),
        ( XMAP, "<", edit_selection!(EditAction::Indent(IndentChange::Decrease(Count::Contextual))) ),
        ( XMAP, ">", edit_selection!(EditAction::Indent(IndentChange::Increase(Count::Contextual))) ),
        ( XMAP, "?", search!(MoveDir1D::Previous, VimMode::Visual) ),
        ( XMAP, "/", search!(MoveDir1D::Next, VimMode::Visual) ),
        ( XMAP, "<C-G>", goto!(VimMode::Select) ),
        ( XMAP, "<C-W>f", window_file!(OpenTarget::Selection) ),
        ( XMAP, "<C-W>gf", tab_open!(OpenTarget::Selection, FocusChange::Current) ),
        ( XMAP, "<C-W><C-F>", window_file!(OpenTarget::Selection) ),

        // Select mode
        ( SMAP, "<C-G>", goto!(VimMode::Visual) ),
        ( SMAP, "<C-O>", fallthrough!(VimMode::Visual) ),

        // Insert, Command mode
        ( ICMAP, "<C-H>", erase!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( ICMAP, "<C-K>", iact!(InternalAction::SetCursorChar('?')) ),
        ( ICMAP, "<C-K>{digraph1}", iact!(InternalAction::SetCursorDigraph) ),
        ( ICMAP, "<C-K>{digraph1}{digraph2}", chartype!() ),
        ( ICMAP, "<C-R>", iact!(InternalAction::SetCursorChar('"')) ),
        ( ICMAP, "<C-R>{register}", paste!(PasteStyle::Cursor, 1) ),
        ( ICMAP, "<C-R><C-C>", normal!() ),
        ( ICMAP, "<C-R><C-O>{register}", unmapped!() ),
        ( ICMAP, "<C-R><C-R>{register}", unmapped!() ),
        ( ICMAP, "<C-U>", erase!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( ICMAP, "<C-V>", iact!(InternalAction::SetCursorChar('^')) ),
        ( ICMAP, "<C-V>o{oct<=3}", chartype!() ),
        ( ICMAP, "<C-V>O{oct<=3}", chartype!() ),
        ( ICMAP, "<C-V>x{hex<=2}", chartype!() ),
        ( ICMAP, "<C-V>X{hex<=2}", chartype!() ),
        ( ICMAP, "<C-V>u{hex<=4}", chartype!() ),
        ( ICMAP, "<C-V>U{hex<=8}", chartype!() ),
        ( ICMAP, "<C-V>{dec<=3}", chartype!() ),
        ( ICMAP, "<C-V>{any}", chartype!() ),
        ( ICMAP, "<C-W>", erase!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( ICMAP, "<BS>", erase!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( ICMAP, "<Del>", erase!(MoveType::Column(MoveDir1D::Next, true)) ),

        // Insert Mode
        ( IMAP, "<C-@>", paste_register!(MoveDir1D::Previous, Register::LastInserted, VimMode::Normal) ),
        ( IMAP, "<C-A>", paste_register!(MoveDir1D::Previous, Register::LastInserted) ),
        ( IMAP, "<C-E>", chartype!(Char::CopyLine(MoveDir1D::Next)) ),
        ( IMAP, "<C-G>j", unmapped!() ),
        ( IMAP, "<C-G>k", unmapped!() ),
        ( IMAP, "<C-G>u", unmapped!() ),
        ( IMAP, "<C-G>U", unmapped!() ),
        ( IMAP, "<C-G><C-J>", unmapped!() ),
        ( IMAP, "<C-G><C-K>", unmapped!() ),
        ( IMAP, "<C-G><Down>", unmapped!() ),
        ( IMAP, "<C-G><Up>", unmapped!() ),
        ( IMAP, "<C-N>", complete!(CompletionStyle::List(MoveDir1D::Next), CompletionType::Auto, CompletionDisplay::List) ),
        ( IMAP, "<C-O>", fallthrough!(VimMode::Normal) ),
        ( IMAP, "<C-P>", complete!(CompletionStyle::List(MoveDir1D::Previous), CompletionType::Auto, CompletionDisplay::List) ),
        ( IMAP, "<C-R><C-P>{register}", unmapped!() ),
        ( IMAP, "<C-T>", edit_lines!(EditAction::Indent(IndentChange::Increase(Count::Exact(1)))) ),
        ( IMAP, "<C-X><C-E>", scroll2d!(MoveDir2D::Down, ScrollSize::Cell) ),
        ( IMAP, "<C-X><C-Y>", scroll2d!(MoveDir2D::Up, ScrollSize::Cell) ),
        ( IMAP, "<C-Y>", chartype!(Char::CopyLine(MoveDir1D::Previous)) ),
        ( IMAP, "<C-[>", normal!() ),
        ( IMAP, "<Home>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( IMAP, "<End>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::End), 0) ),
        ( IMAP, "<Up>", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<Down>", edit!(EditAction::Motion, MoveType::Line(MoveDir1D::Next)) ),
        ( IMAP, "<Left>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Previous, false)) ),
        ( IMAP, "<Right>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Next, false)) ),
        ( IMAP, "<S-Up>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),
        ( IMAP, "<S-Down>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( IMAP, "<S-Left>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( IMAP, "<S-Right>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next)) ),
        ( IMAP, "<C-Left>", edit_end!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( IMAP, "<C-Right>", edit_end!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next)) ),
        ( IMAP, "<C-Space>", paste_register!(MoveDir1D::Previous, Register::LastInserted, VimMode::Normal) ),
        ( IMAP, "<Esc>", normal!() ),
        ( IMAP, "<Tab>", chartype!(Char::Single('\t')) ),
        ( IMAP, "<C-Home>", edit!(EditAction::Motion, MoveType::BufferPos(MovePosition::Beginning)) ),
        ( IMAP, "<Insert>", insert!(InsertStyle::Replace) ),
        ( IMAP, "<PageDown>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( IMAP, "<PageUp>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),

        // Command mode
        ( CMAP, "<C-A>", unmapped!() ),
        ( CMAP, "<C-B>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( CMAP, "<C-C>", command_unfocus!() ),
        ( CMAP, "<C-D>", complete!(CompletionStyle::None, CompletionType::Auto, CompletionDisplay::Bar) ),
        ( CMAP, "<C-E>", edit!(EditAction::Motion, MoveType::LinePos(MovePosition::End), 0) ),
        ( CMAP, "<C-G>", unmapped!() ),
        ( CMAP, "<C-L>", unmapped!() ),
        ( CMAP, "<C-N>", unmapped!() ),
        ( CMAP, "<C-P>", unmapped!() ),
        ( CMAP, "<C-\\><C-N>", command_unfocus!() ),
        ( CMAP, "<C-4><C-N>", command_unfocus!() ),
        ( CMAP, "<C-[>", command_unfocus!() ),
        ( CMAP, "<Home>", edit_buffer!(EditAction::Motion, MoveTerminus::Beginning, VimMode::Command) ),
        ( CMAP, "<End>", edit_buffer!(EditAction::Motion, MoveTerminus::End, VimMode::Command) ),
        ( CMAP, "<Esc>", command_unfocus!() ),
        ( CMAP, "<NL>", command_exit!(PromptAction::Submit) ),
        ( CMAP, "<Tab>", complete!(CompletionStyle::List(MoveDir1D::Next), CompletionType::Auto, CompletionDisplay::None) ),
        ( CMAP, "<S-Tab>", complete!(CompletionStyle::List(MoveDir1D::Previous), CompletionType::Auto, CompletionDisplay::None) ),
        ( CMAP, "<S-Left>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous)) ),
        ( CMAP, "<C-Left>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous)) ),
        ( CMAP, "<S-Right>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next)) ),
        ( CMAP, "<C-Right>", edit!(EditAction::Motion, MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next)) ),
        ( CMAP, "<Up>", prompt!(PromptAction::Recall(RecallFilter::PrefixMatch, MoveDir1D::Previous, Count::Contextual)) ),
        ( CMAP, "<Down>", prompt!(PromptAction::Recall(RecallFilter::PrefixMatch, MoveDir1D::Next, Count::Contextual)) ),
        ( CMAP, "<Left>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Previous, true)) ),
        ( CMAP, "<Right>", edit!(EditAction::Motion, MoveType::Column(MoveDir1D::Next, true)) ),
        ( CMAP, "<S-Up>", prompt!(PromptAction::Recall(RecallFilter::All, MoveDir1D::Previous, Count::Contextual)) ),
        ( CMAP, "<S-Down>", prompt!(PromptAction::Recall(RecallFilter::All, MoveDir1D::Next, Count::Contextual)) ),
        ( CMAP, "<PageUp>", prompt!(PromptAction::Recall(RecallFilter::All, MoveDir1D::Previous, Count::Contextual)) ),
        ( CMAP, "<PageDown>", prompt!(PromptAction::Recall(RecallFilter::All, MoveDir1D::Next, Count::Contextual)) ),
        ( CMAP, "<Insert>", iact!(InternalAction::SetInsertStyle(InsertStyle::Replace)) ),

        // Operator-Pending mode
        ( OMAP, "gn", unmapped!() ),
        ( OMAP, "gN", unmapped!() ),
        ( OMAP, "?", search!(MoveDir1D::Previous, VimMode::Normal) ),
        ( OMAP, "/", search!(MoveDir1D::Next, VimMode::Normal) ),

        // Internal mode to simplify keypresses allowed after f/F/t/T.
        ( SUFFIX_CHARSRCH, "<C-K>{digraph1}{digraph2}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>o{oct<=3}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>O{oct<=3}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>x{hex<=2}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>X{hex<=2}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>u{hex<=4}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>U{hex<=8}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>{dec<=3}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-V>{any}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "{any}", charsearch_suffix!() ),
        ( SUFFIX_CHARSRCH, "<C-[>", act!(Action::NoOp) ),
        ( SUFFIX_CHARSRCH, "<Esc>", act!(Action::NoOp) ),

        // Internal mode to simplify keypresses allowed after r/gr.
        ( SUFFIX_CHARREPL, "<C-K>{digraph1}{digraph2}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-V>o{oct<=3}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-V>O{oct<=3}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-V>x{hex<=2}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-V>X{hex<=2}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-V>u{hex<=4}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-V>U{hex<=8}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-V>{dec<=3}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "{any}", charreplace_suffix!() ),
        ( SUFFIX_CHARREPL, "<C-C>", normal!() ),
        ( SUFFIX_CHARREPL, "<C-[>", act!(Action::NoOp) ),
        ( SUFFIX_CHARREPL, "<Esc>", act!(Action::NoOp) ),
        ( SUFFIX_CHARREPL, "<C-E>", charreplace_suffix!(Char::CopyLine(MoveDir1D::Previous)) ),
        ( SUFFIX_CHARREPL, "<C-Y>", charreplace_suffix!(Char::CopyLine(MoveDir1D::Next)) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_pfxs<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, Option<InputStep<I>>)> {
    [
        // Normal, Visual and Operator-Pending mode commands can be prefixed w/ a count.
        ( NXOMAP, "{count}", Some(iact!(InternalAction::SaveCounting)) ),

        // Normal and Visual mode keys can be prefixed w/ a register.
        ( NXMAP, "\"{register}", None ),

        // Operator-Pending mode keys can be prefixed w/ the forced-motion keys.
        ( OMAP, "v", Some(shaped!(TargetShape::CharWise)) ),
        ( OMAP, "V", Some(shaped!(TargetShape::LineWise)) ),
        ( OMAP, "<C-V>", Some(shaped!(TargetShape::BlockWise)) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_enter<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // <Enter> in Normal, Visual, and Operator-Pending mode moves to next line.
        ( MAP, "<Enter>", edit_end!(MoveType::FirstWord(MoveDir1D::Next)) ),

        // <Enter> in Insert mode types a newlines.
        ( IMAP, "<Enter>", chartype!(Char::Single('\n')) ),

        // <Enter> in Command mode submits the command.
        ( CMAP, "<Enter>", command_exit!(PromptAction::Submit) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_search<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        ( NXOMAP, "n", edit_search_end!(SearchType::Regex, MoveDirMod::Same) ),
        ( NXOMAP, "N", edit_search_end!(SearchType::Regex, MoveDirMod::Flip) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_ctrlcd<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        ( NMAP, "<C-C>", normal!() ),
        ( NMAP, "<C-D>", scroll2d!(MoveDir2D::Down, ScrollSize::HalfPage) ),

        ( IMAP, "<C-C>", normal!() ),
        ( IMAP, "<C-D>", edit_lines!(EditAction::Indent(IndentChange::Decrease(Count::Exact(1)))) ),
    ].to_vec()
}

#[rustfmt::skip]
fn submit_on_enter<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // <Enter> in Normal and Visual mode submits contents.
        ( NVMAP, "<Enter>", prompt!(PromptAction::Submit, VimMode::Normal) ),

        // <Enter> in Insert mode submits contents and stays in Insert mode.
        ( IMAP, "<Enter>", prompt!(PromptAction::Submit, VimMode::Insert) ),

        // <Enter> in Command mode submits the command.
        ( CMAP, "<Enter>", command_exit!(PromptAction::Submit) ),

        // <Enter> in Operator-Pending mode moves to the next line.
        ( OMAP, "<Enter>", edit_end!(MoveType::FirstWord(MoveDir1D::Next)) ),
    ].to_vec()
}

#[rustfmt::skip]
fn search_is_action<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // Perform an application-level search in Normal mode.
        ( NMAP, "n", act!(Action::Search(MoveDirMod::Same, Count::Contextual)) ),
        ( NMAP, "N", act!(Action::Search(MoveDirMod::Flip, Count::Contextual)) ),

        // Perform text search in Operator-Pending mode.
        ( OMAP, "n", edit_search_end!(SearchType::Regex, MoveDirMod::Same) ),
        ( OMAP, "N", edit_search_end!(SearchType::Regex, MoveDirMod::Flip) ),

        // Perform text search in Visual mode.
        ( XMAP, "n", edit_search_end!(SearchType::Regex, MoveDirMod::Same) ),
        ( XMAP, "N", edit_search_end!(SearchType::Regex, MoveDirMod::Flip) ),
    ].to_vec()
}

#[rustfmt::skip]
fn ctrlcd_is_abort<I: ApplicationInfo>() -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        // Abort if prompt is empty in both Normal and Insert mode.
        ( NMAP, "<C-C>", prompt!(PromptAction::Abort(true)) ),
        ( NMAP, "<C-D>", prompt!(PromptAction::Abort(true)) ),
        ( IMAP, "<C-C>", prompt!(PromptAction::Abort(true)) ),
        ( IMAP, "<C-D>", prompt!(PromptAction::Abort(true)) ),
    ].to_vec()
}

#[rustfmt::skip]
fn cursor_open<I: ApplicationInfo>(style: WordStyle) -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        ( NMAP, "gf", window_switch!(OpenTarget::Cursor(style.clone())) ),
        ( NMAP, "<C-W>f", window_file!(OpenTarget::Cursor(style.clone())) ),
        ( NMAP, "<C-W>gf", tab_open!(OpenTarget::Cursor(style.clone()), FocusChange::Current) ),
        ( NMAP, "<C-W><C-F>", window_file!(OpenTarget::Cursor(style)) ),
    ].to_vec()
}

#[rustfmt::skip]
fn keyword_lookup<I: ApplicationInfo>(style: WordStyle) -> Vec<(MappedModes, &'static str, InputStep<I>)> {
    [
        ( NMAP, "K", act!(Action::KeywordLookup(KeywordTarget::Word(style))) ),
        ( XMAP, "K", act!(Action::KeywordLookup(KeywordTarget::Selection)) ),
    ].to_vec()
}

#[inline]
fn add_prefix<I: ApplicationInfo>(
    machine: &mut VimMachine<TerminalKey, I>,
    modes: &MappedModes,
    keys: &str,
    action: &Option<InputStep<I>>,
) {
    if let Ok((_, evs)) = parse(keys) {
        for mode in modes.split() {
            machine.add_prefix(mode, &evs, action);
        }
    } else {
        panic!("invalid vim keybinding: {}", keys);
    }
}

#[inline]
fn add_mapping<I: ApplicationInfo>(
    machine: &mut VimMachine<TerminalKey, I>,
    modes: &MappedModes,
    keys: &str,
    action: &InputStep<I>,
) {
    if let Ok((_, evs)) = parse(keys) {
        for mode in modes.split() {
            machine.add_mapping(mode, &evs, action);
        }
    } else {
        panic!("invalid vim keybinding: {}", keys);
    }
}

/// A configurable collection of Vim bindings that can be added to a [ModalMachine].
#[derive(Debug)]
pub struct VimBindings<I: ApplicationInfo> {
    prefixes: Vec<(MappedModes, &'static str, Option<InputStep<I>>)>,
    mappings: Vec<(MappedModes, &'static str, InputStep<I>)>,
    enter: Vec<(MappedModes, &'static str, InputStep<I>)>,
    search: Vec<(MappedModes, &'static str, InputStep<I>)>,
    ctrlcd: Vec<(MappedModes, &'static str, InputStep<I>)>,
    cursor_open: Vec<(MappedModes, &'static str, InputStep<I>)>,
    kw_lookup: Vec<(MappedModes, &'static str, InputStep<I>)>,
}

impl<I: ApplicationInfo> VimBindings<I> {
    /// Remap the Enter key in Normal, Visual, Select, and Insert mode to
    /// [submit](PromptAction::Submit) instead.
    pub fn submit_on_enter(mut self) -> Self {
        self.enter = submit_on_enter();
        self
    }

    /// Remap `n` and `N` in Normal mode to perform [Action::Search] instead.
    pub fn search_is_action(mut self) -> Self {
        self.search = search_is_action();
        self
    }

    /// Remap `<C-D>` in Normal and Insert mode to abort entry when the prompt is empty.
    pub fn ctrlcd_is_abort(mut self) -> Self {
        self.ctrlcd = ctrlcd_is_abort();
        self
    }

    /// Change what [WordStyle] is used with keys that map to a [OpenTarget::Cursor] value.
    pub fn cursor_open(mut self, style: WordStyle) -> Self {
        self.cursor_open = cursor_open(style);
        self
    }

    /// Change what [WordStyle] is used with keys that map to a [KeywordTarget::Word] value.
    pub fn keyword_lookup(mut self, style: WordStyle) -> Self {
        self.kw_lookup = keyword_lookup(style);
        self
    }
}

impl<I: ApplicationInfo> ShellBindings for VimBindings<I> {
    fn shell(self) -> Self {
        self.submit_on_enter().search_is_action().ctrlcd_is_abort()
    }
}

impl<I: ApplicationInfo> Default for VimBindings<I> {
    fn default() -> Self {
        VimBindings {
            prefixes: default_pfxs(),
            mappings: default_keys(),
            enter: default_enter(),
            search: default_search(),
            ctrlcd: default_ctrlcd(),
            cursor_open: cursor_open(WordStyle::FilePath),
            kw_lookup: keyword_lookup(WordStyle::Little),
        }
    }
}

impl<I: ApplicationInfo> InputBindings<TerminalKey, InputStep<I>> for VimBindings<I> {
    fn setup(&self, machine: &mut VimMachine<TerminalKey, I>) {
        for (modes, keys, action) in self.prefixes.iter() {
            add_prefix(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.mappings.iter() {
            add_mapping(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.enter.iter() {
            add_mapping(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.search.iter() {
            add_mapping(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.ctrlcd.iter() {
            add_mapping(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.cursor_open.iter() {
            add_mapping(machine, modes, keys, action);
        }
    }
}

/// Manage Vim keybindings and modes.
pub type VimMachine<Key, T = EmptyInfo> = ModalMachine<Key, InputStep<T>>;

/// Create a new [VimMachine] populated with standard Vim keys.
pub fn default_vim_keys<I: ApplicationInfo>() -> VimMachine<TerminalKey, I> {
    ModalMachine::from_bindings::<VimBindings<I>>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::context::EditContext;
    use crate::keybindings::BindingMachine;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    macro_rules! assert_insert_exit {
        ($mm: expr, $ctx: expr) => {
            $ctx.action.operation = EditAction::Motion;
            assert_pop1!($mm, Action::NoOp, $ctx);

            $ctx.persist.shape = None;
            $ctx.persist.insert = None;

            assert_pop1!($mm, CURSOR_CLOSE, $ctx);
            assert_pop1!($mm, COLUMN_PREV, $ctx);
            assert_pop2!($mm, CHECKPOINT, $ctx);
            assert_eq!($mm.mode(), VimMode::Normal);
        };
    }

    macro_rules! action_reset {
        ($ctx: expr) => {
            $ctx.action.count = None;
            $ctx.action.cursor_end = Some(CursorEnd::Auto);
            $ctx.action.mark = None;
            $ctx.action.operation = EditAction::Motion;
            $ctx.action.register = None;
            $ctx.action.register_append = false;
            $ctx.action.replace = None;
            $ctx.ch = Default::default();
        };
    }

    macro_rules! assert_visual_exit {
        ($mm: expr, $ctx: expr) => {
            assert_pop1!($mm, Action::NoOp, $ctx);

            action_reset!($ctx);
            $ctx.persist.shape = None;
            $ctx.persist.insert = None;

            assert_pop1!($mm, CURSOR_CLOSE, $ctx);
            assert_pop1!($mm, CURRENT_POS, $ctx);
            assert_pop2!($mm, CHECKPOINT, $ctx);
            assert_eq!($mm.mode(), VimMode::Normal);
        };
    }

    macro_rules! assert_normal {
        ($mm: expr, $ctx: expr) => {
            let mut keep = $ctx.clone();
            action_reset!($ctx);
            $ctx.persist.shape = None;
            $ctx.persist.insert = None;
            assert_pop2!($mm, CHECKPOINT, $ctx);
            assert_eq!($mm.mode(), VimMode::Normal);
            std::mem::swap(&mut keep, &mut $ctx);
        };
    }

    macro_rules! mvop {
        ($ea: expr, $mt: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Exact($ea.clone()),
                EditTarget::Motion($mt.clone(), Count::Contextual),
            ))
        };
        ($ea: expr, $mt: expr, $c: literal) => {
            Action::from(EditorAction::Edit(
                Specifier::Exact($ea.clone()),
                EditTarget::Motion($mt.clone(), Count::Exact($c)),
            ))
        };
        ($ea: expr, $mt: expr, $c: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Exact($ea.clone()),
                EditTarget::Motion($mt.clone(), $c),
            ))
        };
    }

    macro_rules! mv {
        ($mt: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Motion($mt.clone(), Count::Contextual),
            ))
        };
        ($mt: expr, $c: literal) => {
            Action::from(EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Motion($mt.clone(), Count::Exact($c)),
            ))
        };
        ($mt: expr, $c: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Motion($mt.clone(), $c),
            ))
        };
    }

    macro_rules! rangeop {
        ($ea: expr, $rt: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Exact($ea),
                EditTarget::Range($rt, true, Count::Contextual),
            ))
        };
        ($ea: expr, $rt: expr, $inc: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Exact($ea),
                EditTarget::Range($rt, $inc, Count::Contextual),
            ))
        };
        ($ea: expr, $rt: expr, $inc: expr, $c: literal) => {
            Action::from(EditorAction::Edit(
                Specifier::Exact($ea),
                EditTarget::Range($rt, $inc, Count::Exact($c)),
            ))
        };
        ($ea: expr, $rt: expr, $inc: expr, $c: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Exact($ea),
                EditTarget::Range($rt, $inc, $c),
            ))
        };
    }

    macro_rules! range {
        ($rt: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Range($rt, true, Count::Contextual),
            ))
        };
        ($rt: expr, $inc: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Range($rt, $inc, Count::Contextual),
            ))
        };
        ($rt: expr, $inc: expr, $c: literal) => {
            Action::from(EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Range($rt, $inc, Count::Exact($c)),
            ))
        };
        ($rt: expr, $inc: expr, $c: expr) => {
            Action::from(EditorAction::Edit(
                Specifier::Contextual,
                EditTarget::Range($rt, $inc, $c),
            ))
        };
    }

    macro_rules! selop {
        ($ea: expr) => {
            Action::from(EditorAction::Edit(Specifier::Exact($ea), EditTarget::Selection))
        };
    }

    macro_rules! typechar {
        ($c: literal) => {
            Action::from(EditorAction::InsertText(InsertTextAction::Type(
                Char::Single($c).into(),
                MoveDir1D::Previous,
                1.into(),
            )))
        };
    }

    const CURRENT_POS: Action = Action::Editor(EditorAction::Edit(
        Specifier::Exact(EditAction::Motion),
        EditTarget::CurrentPosition,
    ));
    const COLUMN_NEXT: Action = Action::Editor(EditorAction::Edit(
        Specifier::Exact(EditAction::Motion),
        EditTarget::Motion(MoveType::Column(MoveDir1D::Next, false), Count::Exact(1)),
    ));
    const COLUMN_PREV: Action = Action::Editor(EditorAction::Edit(
        Specifier::Exact(EditAction::Motion),
        EditTarget::Motion(MoveType::Column(MoveDir1D::Previous, false), Count::Exact(1)),
    ));
    const CHECKPOINT: Action = Action::Editor(EditorAction::History(HistoryAction::Checkpoint));
    const CMDBAR_ABORT: Action = Action::Prompt(PromptAction::Abort(false));
    const CURSOR_CLOSE: Action =
        Action::Editor(EditorAction::Cursor(CursorAction::Close(CursorCloseTarget::Followers)));
    const CURSOR_SPLIT: Action =
        Action::Editor(EditorAction::Cursor(CursorAction::Split(Count::MinusOne)));
    const SEL_SPLIT: Action = Action::Editor(EditorAction::Selection(SelectionAction::Split(
        SelectionSplitStyle::Lines,
        TargetShapeFilter::ALL,
    )));
    const BLOCK_SPLIT: Action = Action::Editor(EditorAction::Selection(SelectionAction::Split(
        SelectionSplitStyle::Lines,
        TargetShapeFilter::BLOCK,
    )));
    const BLOCK_BEG: Action = Action::Editor(EditorAction::Selection(SelectionAction::CursorSet(
        SelectionCursorChange::Beginning,
    )));
    const BLOCK_END: Action = Action::Editor(EditorAction::Selection(SelectionAction::CursorSet(
        SelectionCursorChange::End,
    )));
    const TYPE_CONTEXTUAL: Action = Action::Editor(EditorAction::InsertText(
        InsertTextAction::Type(Specifier::Contextual, MoveDir1D::Previous, Count::Exact(1)),
    ));

    fn cmdbar_focus(s: &str, ct: CommandType, act: Action) -> Action {
        Action::CommandBar(CommandBarAction::Focus(s.into(), ct, act.into()))
    }

    fn cmdbar_command() -> Action {
        let exec = CommandAction::Execute(1.into());
        cmdbar_focus(":", CommandType::Command, exec.into())
    }

    fn cmdbar_search(s: &str) -> Action {
        let search = EditTarget::Search(SearchType::Regex, MoveDirMod::Same, Count::Contextual);
        let search = EditorAction::Edit(Specifier::Contextual, search);
        cmdbar_focus(s, CommandType::Search, search.into())
    }

    fn mkctx() -> VimState<EmptyInfo> {
        VimState::default()
    }

    #[test]
    fn test_transitions_normal() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let op = EditAction::Motion;

        // Starts in Normal mode
        assert_eq!(vm.mode(), VimMode::Normal);

        // Normal -> Insert mode using "i".
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('i'));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Insert -> Normal mode using Escape.
        vm.input_key(key!(KeyCode::Esc));
        assert_insert_exit!(vm, ctx);

        // Normal -> Insert mode using Insert.
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!(KeyCode::Insert));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Insert -> Normal mode using ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Normal -> Insert mode using "gI".
        let mov = mvop!(op, MoveType::LinePos(MovePosition::Beginning), 0);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('g'));
        vm.input_key(key!('I'));
        assert_pop1!(vm, mov, ctx);
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Insert -> Normal mode using ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Normal -> Insert mode using "A".
        let mov = mvop!(op, MoveType::LinePos(MovePosition::End), 0);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('A'));
        assert_pop1!(vm, mov, ctx);
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Insert -> Normal mode using ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Normal -> Insert mode using "I".
        let mov = mvop!(op, MoveType::FirstWord(MoveDir1D::Next), 0);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('I'));
        assert_pop1!(vm, mov, ctx);
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Insert -> Normal mode using ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Normal -> Replace mode using "R".
        ctx.persist.insert = Some(InsertStyle::Replace);
        vm.input_key(key!('R'));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Replace -> Insert mode using Insert.
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!(KeyCode::Insert));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);
        assert_eq!(vm.state().persist.insert, Some(InsertStyle::Insert));

        // Insert -> Replace mode using Insert.
        ctx.persist.insert = Some(InsertStyle::Replace);
        vm.input_key(key!(KeyCode::Insert));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);
        assert_eq!(vm.state().persist.insert, Some(InsertStyle::Replace));

        // Replace -> Normal mode using ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Normal -> Visual mode (charwise) using "v".
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Visual -> Normal mode using ^C.
        vm.input_key(ctl!('c'));
        assert_visual_exit!(vm, ctx);

        // Normal -> Visual mode (linewise) using "V".
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('V'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Visual -> Normal mode using Escape.
        vm.input_key(key!(KeyCode::Esc));
        assert_visual_exit!(vm, ctx);

        // Normal -> Visual mode (blockwise) using ^V
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);
    }

    #[test]
    fn test_transitions_command() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Starts in Normal mode
        assert_eq!(vm.mode(), VimMode::Normal);

        // Move to Command mode using ":".
        vm.input_key(key!(':'));
        assert_pop2!(vm, cmdbar_command(), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!(':'));
        assert_pop2!(vm, typechar!(':'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        vm.input_key(key!('a'));
        assert_pop2!(vm, typechar!('a'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        vm.input_key(key!('A'));
        assert_pop2!(vm, typechar!('A'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Go back to Normal mode via Escape.
        vm.input_key(key!(KeyCode::Esc));
        assert_pop1!(vm, CMDBAR_ABORT, ctx);

        ctx.persist.insert = None;
        assert_eq!(vm.mode(), VimMode::Normal);
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move to Command mode (forward search) using "/".
        ctx.persist.regexsearch_dir = MoveDir1D::Next;
        vm.input_key(key!('/'));
        assert_pop2!(vm, cmdbar_search("/"), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('1'));
        assert_pop2!(vm, typechar!('1'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Go back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_pop1!(vm, CMDBAR_ABORT, ctx);

        ctx.persist.insert = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move to Command mode (reverse search) using "?".
        ctx.persist.regexsearch_dir = MoveDir1D::Previous;
        vm.input_key(key!('?'));
        assert_pop2!(vm, cmdbar_search("?"), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('^'));
        assert_pop2!(vm, typechar!('^'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Go back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_pop1!(vm, CMDBAR_ABORT, ctx);

        ctx.persist.insert = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Go to Visual mode via "v".
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Move to Command mode using "/".
        ctx.persist.shape = None;
        ctx.persist.regexsearch_dir = MoveDir1D::Next;
        vm.input_key(key!('/'));
        assert_pop2!(vm, cmdbar_search("/"), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        ctx.persist.insert = Some(InsertStyle::Insert);
        ctx.persist.shape = None;
        vm.input_key(key!('1'));
        assert_pop2!(vm, typechar!('1'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // <Enter> submits and takes us back to Visual mode.
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!(KeyCode::Enter));
        assert_pop2!(vm, Action::from(PromptAction::Submit), ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Move to Visual mode (linewise) using "V".
        ctx.persist.insert = None;
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('V'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Move to Command mode using "/".
        ctx.persist.shape = None;
        ctx.persist.regexsearch_dir = MoveDir1D::Next;
        vm.input_key(key!('/'));
        assert_pop2!(vm, cmdbar_search("/"), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        ctx.persist.insert = Some(InsertStyle::Insert);
        ctx.persist.shape = None;
        vm.input_key(key!('1'));
        assert_pop2!(vm, typechar!('1'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // <Enter> submits and takes us back to Visual (linewise) mode.
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!(KeyCode::Enter));
        assert_pop2!(vm, Action::from(PromptAction::Submit), ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Visual -> Normal mode using ^C.
        ctx.persist.insert = None;
        vm.input_key(ctl!('c'));
        assert_visual_exit!(vm, ctx);

        // "2c/" should take us to Command mode, with a pending entry into Insert mode.
        ctx.action.count = Some(2);
        ctx.action.operation = EditAction::Delete;
        ctx.persist.insert = Some(InsertStyle::Insert);
        ctx.persist.regexsearch_dir = MoveDir1D::Next;
        vm.input_key(key!('2'));
        vm.input_key(key!('c'));
        vm.input_key(key!('/'));
        assert_pop2!(vm, cmdbar_search("/"), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // Unmapped key types that character.
        ctx.action.count = None;
        ctx.action.operation = EditAction::Motion;
        vm.input_key(key!('1'));
        assert_pop2!(vm, typechar!('1'), ctx);
        assert_eq!(vm.mode(), VimMode::Command);

        // <Enter> submits and takes us to Insert mode.
        ctx.action.count = Some(2);
        ctx.action.operation = EditAction::Delete;
        vm.input_key(key!(KeyCode::Enter));
        assert_pop2!(vm, Action::from(PromptAction::Submit), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);
    }

    #[test]
    fn test_transitions_visual() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Move to Visual mode (charwise) and back using "v".
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        vm.input_key(key!('v'));
        assert_visual_exit!(vm, ctx);

        // Move to Visual mode (linewise) and back using "V".
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('V'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        vm.input_key(key!('V'));
        assert_visual_exit!(vm, ctx);

        // Move to Visual mode (blockwise) and back using ^V.
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        vm.input_key(ctl!('v'));
        assert_visual_exit!(vm, ctx);

        // Cycle through the different Visual modes.
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('V'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('V'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        vm.input_key(key!('v'));
        assert_visual_exit!(vm, ctx);
    }

    #[test]
    fn test_transitions_visual_select() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Test charwise shapes.
        ctx.persist.shape = Some(TargetShape::CharWise);

        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Move to Select mode (charwise) and back using ^G.
        vm.input_key(ctl!('g'));
        assert_pop2!(vm, Action::NoOp, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        vm.input_key(ctl!('g'));
        assert_pop2!(vm, Action::NoOp, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Test linewise shapes.
        ctx.persist.shape = Some(TargetShape::LineWise);

        // Move to Visual mode (linewise) using "V".
        vm.input_key(key!('V'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Move to Select mode (linewise) and back using ^G.
        vm.input_key(ctl!('g'));
        assert_pop2!(vm, Action::NoOp, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        vm.input_key(ctl!('g'));
        assert_pop2!(vm, Action::NoOp, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Test blockwise shapes.
        ctx.persist.shape = Some(TargetShape::BlockWise);

        // Move to Visual mode (blockwise) using ^V.
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Move to Select mode (blockwise) and back using ^G.
        vm.input_key(ctl!('g'));
        assert_pop2!(vm, Action::NoOp, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        vm.input_key(ctl!('g'));
        assert_pop2!(vm, Action::NoOp, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Back to Select mode using ^G.
        vm.input_key(ctl!('g'));
        assert_pop2!(vm, Action::NoOp, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        // Back to Normal mode by repeating ^V.
        vm.input_key(ctl!('v'));
        assert_visual_exit!(vm, ctx);
    }

    #[test]
    fn test_transitions_select() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Enter Select mode (charwise) via "gh".
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('g'));
        vm.input_key(key!('h'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        // Select text using the Right arrow key.
        let mov = mv!(MoveType::Column(MoveDir1D::Next, false));
        vm.input_key(key!(KeyCode::Right));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        // Enter a single Visual mode command by using ^O.
        vm.input_key(ctl!('o'));
        assert_eq!(vm.pop(), None);
        assert_eq!(vm.mode(), VimMode::Select);

        let mov = mv!(MoveType::Column(MoveDir1D::Next, false));
        vm.input_key(key!('l'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));
        vm.input_key(key!(KeyCode::Right, KeyModifiers::SHIFT));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        ctx.persist.insert = Some(InsertStyle::Insert);

        let mov = selop!(EditAction::Delete);
        vm.input_key(key!('H'));
        assert_pop1!(vm, mov, ctx);
        assert_pop1!(vm, typechar!('H'), ctx);

        ctx.persist.shape = None;
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        vm.input_key(key!('l'));
        assert_pop2!(vm, typechar!('l'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Back to Normal mode.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Move to Select mode (blockwise) using g^H.
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(key!('g'));
        vm.input_key(ctl!('h'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        // Move the cursor down one line, doing a blockwise selection.
        let mov = mv!(MoveType::Line(MoveDir1D::Next));
        vm.input_key(key!(KeyCode::Down));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Select);

        // Using ^O to repeat g^H goes back to Normal mode.
        vm.input_key(ctl!('o'));
        assert_eq!(vm.pop(), None);
        assert_eq!(vm.mode(), VimMode::Select);

        vm.input_key(key!('g'));
        assert_eq!(vm.pop(), None);
        assert_eq!(vm.mode(), VimMode::Select);

        vm.input_key(ctl!('h'));
        assert_visual_exit!(vm, ctx);
    }

    #[test]
    fn test_count() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));

        // "0" does not count, but moves to the first column.
        vm.input_key(key!('0'));
        assert_pop1!(vm, mv!(MoveType::LinePos(MovePosition::Beginning), 0), ctx);
        assert_normal!(vm, ctx);

        // Test initial non-"0" number.
        ctx.action.count = Some(5);
        vm.input_key(key!('5'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Test "0" after initial non-"0" number.
        ctx.action.count = Some(10);
        vm.input_key(key!('1'));
        vm.input_key(key!('0'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Test multiple "0" keys after initial non-"0" number.
        ctx.action.count = Some(100);
        vm.input_key(key!('1'));
        vm.input_key(key!('0'));
        vm.input_key(key!('0'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Test every number.
        ctx.action.count = Some(1234567890);
        vm.input_key(key!('1'));
        vm.input_key(key!('2'));
        vm.input_key(key!('3'));
        vm.input_key(key!('4'));
        vm.input_key(key!('5'));
        vm.input_key(key!('6'));
        vm.input_key(key!('7'));
        vm.input_key(key!('8'));
        vm.input_key(key!('9'));
        vm.input_key(key!('0'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Delete;

        // Operator-Pending mode count is multiplied by Normal mode count.
        ctx.action.count = Some(4);
        vm.input_key(key!('2'));
        vm.input_key(key!('d'));
        vm.input_key(key!('2'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.count = Some(16);
        vm.input_key(key!('8'));
        vm.input_key(key!('d'));
        vm.input_key(key!('2'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Initial "0" in Operator-Pending mode is a movement.
        let mov = mv!(MoveType::LinePos(MovePosition::Beginning), 0);

        ctx.action.count = None;
        vm.input_key(key!('d'));
        vm.input_key(key!('0'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.count = Some(2);
        vm.input_key(key!('2'));
        vm.input_key(key!('d'));
        vm.input_key(key!('0'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_register() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let op = EditAction::Yank;
        let mov = rangeop!(op, RangeType::Line);
        ctx.action.operation = EditAction::Yank;

        ctx.action.register = None;
        vm.input_key(key!('y'));
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.register = Some(Register::Named('a'));
        ctx.action.register_append = false;
        vm.input_key(key!('"'));
        vm.input_key(key!('a'));
        vm.input_key(key!('y'));
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.register = Some(Register::Named('a'));
        ctx.action.register_append = true;
        vm.input_key(key!('"'));
        vm.input_key(key!('A'));
        vm.input_key(key!('y'));
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.register = Some(Register::LastYanked);
        ctx.action.register_append = false;
        vm.input_key(key!('"'));
        vm.input_key(key!('0'));
        vm.input_key(key!('y'));
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.register = Some(Register::RecentlyDeleted(4));
        vm.input_key(key!('"'));
        vm.input_key(key!('5'));
        vm.input_key(key!('y'));
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.register = Some(Register::Unnamed);
        vm.input_key(key!('"'));
        vm.input_key(key!('"'));
        vm.input_key(key!('y'));
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.register = Some(Register::Blackhole);
        vm.input_key(key!('"'));
        vm.input_key(key!('_'));
        vm.input_key(key!('y'));
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_mark() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Create local mark 'c.
        let act = EditorAction::Mark(Specifier::Contextual);
        ctx.action.mark = Some(Mark::BufferNamed('c'));
        vm.input_key(key!('m'));
        vm.input_key(key!('c'));
        assert_pop1!(vm, Action::from(act), ctx);
        assert_normal!(vm, ctx);

        // Create global mark 'C.
        let act = EditorAction::Mark(Specifier::Contextual);
        ctx.action.mark = Some(Mark::GlobalNamed('C'));
        vm.input_key(key!('m'));
        vm.input_key(key!('C'));
        assert_pop1!(vm, Action::from(act), ctx);
        assert_normal!(vm, ctx);

        // Go to the line of last inserted text.
        let target = EditTarget::LineJump(Specifier::Contextual);
        let mov = EditorAction::Edit(Specifier::Contextual, target);
        ctx.action.mark = Some(Mark::LastInserted);
        vm.input_key(key!('\''));
        vm.input_key(key!('^'));
        assert_pop1!(vm, Action::from(mov), ctx);
        assert_normal!(vm, ctx);

        // Go to the column of the end of the last visual selection.
        let target = EditTarget::CharJump(Specifier::Contextual);
        let mov = EditorAction::Edit(Specifier::Contextual, target);
        ctx.action.mark = Some(Mark::VisualEnd);
        vm.input_key(key!('`'));
        vm.input_key(key!('>'));
        assert_pop1!(vm, Action::from(mov), ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_normal_ops() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));

        ctx.action.operation = EditAction::Yank;
        vm.input_key(key!('y'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Format;
        vm.input_key(key!('g'));
        vm.input_key(key!('q'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::ChangeCase(Case::Lower);
        vm.input_key(key!('g'));
        vm.input_key(key!('u'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::ChangeCase(Case::Upper);
        vm.input_key(key!('g'));
        vm.input_key(key!('U'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::ChangeCase(Case::Toggle);
        vm.input_key(key!('g'));
        vm.input_key(key!('~'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Indent(IndentChange::Auto);
        vm.input_key(key!('='));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Indent(IndentChange::Decrease(Count::Exact(1)));
        vm.input_key(key!('<'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Indent(IndentChange::Increase(Count::Exact(1)));
        vm.input_key(key!('>'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        let mov = range!(RangeType::Word(WordStyle::Little));

        ctx.action.operation = EditAction::Format;
        vm.input_key(key!('g'));
        vm.input_key(key!('w'));
        vm.input_key(key!('a'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Motion;

        let op = EditAction::Join(JoinStyle::OneSpace);
        let lines = rangeop!(op, RangeType::Line);
        vm.input_key(key!('J'));
        assert_pop1!(vm, lines, ctx);
        assert_normal!(vm, ctx);

        let op = EditAction::Join(JoinStyle::NoChange);
        let lines = rangeop!(op, RangeType::Line);
        vm.input_key(key!('g'));
        vm.input_key(key!('J'));
        assert_pop1!(vm, lines, ctx);
        assert_normal!(vm, ctx);

        let col = MoveType::Column(MoveDir1D::Next, false);
        ctx.action.cursor_end = Some(CursorEnd::End);
        vm.input_key(key!('~'));
        assert_pop1!(vm, mvop!(EditAction::ChangeCase(Case::Toggle), col), ctx);
        assert_normal!(vm, ctx);

        let mov = mv!(MoveType::Column(MoveDir1D::Next, false));
        ctx.action.cursor_end = Some(CursorEnd::Auto);
        ctx.action.operation = EditAction::Replace(false);
        ctx.action.replace = Some('A'.into());
        vm.input_key(key!('r'));
        vm.input_key(key!('A'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Motion;
        ctx.action.replace = None;

        let op = EditAction::ChangeNumber(NumberChange::Increase(Count::Contextual), false);
        let mov = mvop!(op, MoveType::LinePos(MovePosition::End), 0);
        vm.input_key(ctl!('a'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        let op = EditAction::ChangeNumber(NumberChange::Decrease(Count::Contextual), false);
        let mov = mvop!(op, MoveType::LinePos(MovePosition::End), 0);
        vm.input_key(ctl!('x'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_delete_ops() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let op = EditAction::Delete;

        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));
        ctx.action.operation = op.clone();
        vm.input_key(key!('d'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Motion;

        let movend = mvop!(op, MoveType::LinePos(MovePosition::End), Count::MinusOne);
        vm.input_key(key!('D'));
        assert_pop1!(vm, movend, ctx);
        assert_normal!(vm, ctx);

        let mov = mvop!(op, MoveType::Column(MoveDir1D::Next, false));
        vm.input_key(key!(KeyCode::Delete));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        let mov = mvop!(op, MoveType::Column(MoveDir1D::Next, false));
        vm.input_key(key!('x'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        let mov = mvop!(op, MoveType::Column(MoveDir1D::Previous, false));
        vm.input_key(key!('X'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_change_ops() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Change a word around the cursor with "caw".
        let mov = range!(RangeType::Word(WordStyle::Little));
        ctx.action.operation = EditAction::Delete;
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('c'));
        vm.input_key(key!('a'));
        vm.input_key(key!('w'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Change from cursor to end of a word with "cw".
        let mov = mv!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next));
        ctx.action.operation = EditAction::Delete;
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('c'));
        vm.input_key(key!('w'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Change from cursor to end of a WORD with "cW".
        let mov = mv!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next));
        ctx.action.operation = EditAction::Delete;
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('c'));
        vm.input_key(key!('W'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Substitute a character with "s".
        let op = EditAction::Delete;
        let mov = mvop!(op, MoveType::Column(MoveDir1D::Next, false));
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('s'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Change from cursor to end of the line with "C".
        let op = EditAction::Delete;
        let mov = mvop!(op, MoveType::LinePos(MovePosition::End), Count::MinusOne);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('C'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Change the current line with "S".
        let op = EditAction::Delete;
        let mov = rangeop!(op, RangeType::Line);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('S'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back to Normal mode via ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Pressing c^C should not go to Insert mode.
        ctx.action.operation = EditAction::Delete;
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('c'));
        vm.input_key(ctl!('c'));
        assert_pop1!(vm, Action::NoOp, ctx);
        assert_normal!(vm, ctx);

        // We should have reset, and can now type a Normal mode command.
        ctx.action.operation = EditAction::Motion;
        ctx.persist.insert = None;
        vm.input_key(ctl!('l'));
        assert_pop1!(vm, Action::RedrawScreen, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_normal_motion_charsearch() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let same_target =
            EditTarget::Search(SearchType::Char(false), MoveDirMod::Same, Count::Contextual);
        let flip_target =
            EditTarget::Search(SearchType::Char(false), MoveDirMod::Flip, Count::Contextual);

        let same = Action::from(EditorAction::Edit(Specifier::Contextual, same_target.clone()));
        let flip = Action::from(EditorAction::Edit(Specifier::Contextual, flip_target.clone()));

        // "fa" should update search params, and then continue character search.
        ctx.persist.charsearch_params = (MoveDir1D::Next, true);
        ctx.persist.charsearch = Some('a'.into());
        vm.input_key(key!('f'));
        vm.input_key(key!('a'));
        assert_pop1!(vm, same, ctx);
        assert_normal!(vm, ctx);

        // ";" should continue character search.
        ctx.persist.charsearch_params = (MoveDir1D::Next, true);
        ctx.persist.charsearch = Some('a'.into());
        vm.input_key(key!(';'));
        assert_pop1!(vm, same, ctx);
        assert_normal!(vm, ctx);

        // "," should continue character search in reverse direction.
        ctx.persist.charsearch_params = (MoveDir1D::Next, true);
        ctx.persist.charsearch = Some('a'.into());
        vm.input_key(key!(','));
        assert_pop1!(vm, flip, ctx);
        assert_normal!(vm, ctx);

        // "T<C-V>o125" should update params and continue search for codepoint.
        ctx.persist.charsearch_params = (MoveDir1D::Previous, false);
        ctx.persist.charsearch = Some('U'.into());
        vm.input_key(key!('T'));
        vm.input_key(ctl!('v'));
        vm.input_key(key!('o'));
        vm.input_key(key!('1'));
        vm.input_key(key!('2'));
        vm.input_key(key!('5'));
        assert_pop1!(vm, same, ctx);
        assert_normal!(vm, ctx);

        // ";" should continue search.
        ctx.persist.charsearch_params = (MoveDir1D::Previous, false);
        ctx.persist.charsearch = Some('U'.into());
        vm.input_key(key!(';'));
        assert_pop1!(vm, same, ctx);
        assert_normal!(vm, ctx);

        // "F<C-K>Z<" should update params and continue search for digraph.
        ctx.persist.charsearch_params = (MoveDir1D::Previous, true);
        ctx.persist.charsearch = Some(Char::Digraph('Z', '<'));
        vm.input_key(key!('F'));
        vm.input_key(ctl!('k'));
        vm.input_key(key!('Z'));
        vm.input_key(key!('<'));
        assert_pop1!(vm, same, ctx);
        assert_normal!(vm, ctx);

        // "," should continue search in reverse direction.
        ctx.persist.charsearch_params = (MoveDir1D::Previous, true);
        ctx.persist.charsearch = Some(Char::Digraph('Z', '<'));
        vm.input_key(key!(','));
        assert_pop1!(vm, flip, ctx);
        assert_normal!(vm, ctx);

        // "t<Esc>" should do nothing leave persistent search parameters alone.
        vm.input_key(key!('t'));
        vm.input_key(key!(KeyCode::Esc));
        ctx.persist.charsearch_params = (MoveDir1D::Previous, true);
        ctx.persist.charsearch = Some(Char::Digraph('Z', '<'));
        assert_pop1!(vm, Action::NoOp, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_normal_motion_special_key() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // <C-H>
        let mov = mv!(MoveType::Column(MoveDir1D::Previous, true));
        vm.input_key(ctl!('h'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <C-?> (backspace)
        let mov = mv!(MoveType::Column(MoveDir1D::Previous, true));
        vm.input_key(key!(KeyCode::Backspace));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Space
        let mov = mv!(MoveType::Column(MoveDir1D::Next, true));
        vm.input_key(key!(' '));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <C-J> (newline)
        let mov = mv!(MoveType::Line(MoveDir1D::Next));
        vm.input_key(ctl!('j'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <C-N>
        let mov = mv!(MoveType::Line(MoveDir1D::Next));
        vm.input_key(ctl!('n'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <C-P>
        let mov = mv!(MoveType::Line(MoveDir1D::Previous));
        vm.input_key(ctl!('p'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <Up>
        let mov = mv!(MoveType::Line(MoveDir1D::Previous));
        vm.input_key(key!(KeyCode::Up));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <Down>
        let mov = mv!(MoveType::Line(MoveDir1D::Next));
        vm.input_key(key!(KeyCode::Down));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <Left>
        let mov = mv!(MoveType::Column(MoveDir1D::Previous, false));
        vm.input_key(key!(KeyCode::Left));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <Right>
        let mov = mv!(MoveType::Column(MoveDir1D::Next, false));
        vm.input_key(key!(KeyCode::Right));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <S-Left>
        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous));
        vm.input_key(key!(KeyCode::Left, KeyModifiers::SHIFT));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <S-Right>
        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));
        vm.input_key(key!(KeyCode::Right, KeyModifiers::SHIFT));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <C-Left>
        let mov = mv!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous));
        vm.input_key(key!(KeyCode::Left, KeyModifiers::CONTROL));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <C-Right>
        let mov = mv!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next));
        vm.input_key(key!(KeyCode::Right, KeyModifiers::CONTROL));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <End>
        let mov = mv!(MoveType::LinePos(MovePosition::End), Count::MinusOne);
        vm.input_key(key!(KeyCode::End));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <Enter>
        let mov = mv!(MoveType::FirstWord(MoveDir1D::Next));
        vm.input_key(key!(KeyCode::Enter));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // <Home>
        let mov = mv!(MoveType::LinePos(MovePosition::Beginning), 0);
        ctx.persist.shape = None;
        vm.input_key(key!(KeyCode::Home));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_visual_ops() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Delete with "d"
        let mov = selop!(EditAction::Delete);
        vm.input_key(key!('d'));
        assert_pop1!(vm, mov, ctx);

        // We move back to Normal mode after deletion.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Yank with "y"
        let mov = selop!(EditAction::Yank);
        vm.input_key(key!('y'));
        assert_pop1!(vm, mov, ctx);

        // We move back to Normal after yanking.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Uppercase with "gu"
        let mov = selop!(EditAction::ChangeCase(Case::Lower));
        vm.input_key(key!('g'));
        vm.input_key(key!('u'));
        assert_pop1!(vm, mov, ctx);

        // We move back to Normal mode after changing case.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Uppercase with "gu"
        let mov = selop!(EditAction::ChangeCase(Case::Upper));
        vm.input_key(key!('g'));
        vm.input_key(key!('U'));
        assert_pop1!(vm, mov, ctx);

        // Move back to Normal mode after changing case.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Shape made LineWise with "D"
        let op = EditAction::Delete;
        let mov = mvop!(op, MoveType::LinePos(MovePosition::End), 0);
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('D'));
        assert_pop1!(vm, SEL_SPLIT, ctx);
        assert_pop1!(vm, BLOCK_BEG, ctx);
        assert_pop1!(vm, mov, ctx);

        // Move back into Normal mode after deletion.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Shape made LineWise with "Y".
        let mov = selop!(EditAction::Yank);
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('Y'));
        assert_pop1!(vm, mov, ctx);

        // Move back into Normal mode after yanking.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Shape made LineWise with "X"
        let mov = selop!(EditAction::Delete);
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('X'));
        assert_pop1!(vm, SEL_SPLIT, ctx);
        assert_pop1!(vm, BLOCK_BEG, ctx);
        assert_pop1!(vm, mov, ctx);

        // Move back into Normal mode after deletion.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Shape made LineWise with "R"
        let mov = selop!(EditAction::Delete);
        ctx.persist.shape = Some(TargetShape::LineWise);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('R'));
        assert_pop1!(vm, mov, ctx);

        // Moves into Insert mode after "R".
        ctx.persist.shape = None;
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move into Normal mode with ^C.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Move into Visual mode (charwise)
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Shape made LineWise with "S"
        let mov = selop!(EditAction::Delete);
        ctx.persist.shape = Some(TargetShape::LineWise);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('S'));
        assert_pop1!(vm, mov, ctx);

        // Move into Insert mode after "S".
        ctx.persist.shape = None;
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back into Normal mode.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Move into Visual mode (blockwise)
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Shape remains BlockWise with "X"
        let mov = selop!(EditAction::Delete);
        vm.input_key(key!('X'));
        assert_pop1!(vm, SEL_SPLIT, ctx);
        assert_pop1!(vm, BLOCK_BEG, ctx);
        assert_pop1!(vm, mov, ctx);

        // Move back into Normal mode after deletion.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);

        // Move into Visual mode (blockwise)
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Shape remains BlockWise with "Y".
        let mov = selop!(EditAction::Yank);
        vm.input_key(key!('Y'));
        assert_pop1!(vm, mov, ctx);

        // Move back into Normal mode after yanking.
        ctx.persist.shape = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);
        assert_pop1!(vm, CURRENT_POS, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_visual_block_insert() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Move into Visual mode (blockwise)
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Insert at beginning of block ("I").
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('I'));
        assert_pop1!(vm, BLOCK_SPLIT, ctx);
        assert_pop1!(vm, BLOCK_BEG, ctx);
        assert_pop1!(vm, CURSOR_SPLIT, ctx);

        // Moves into Insert mode after "I".
        ctx.persist.shape = None;
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back into Normal mode.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Move into Visual mode (blockwise)
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Insert at beginning of block ("A").
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('A'));
        assert_pop1!(vm, BLOCK_SPLIT, ctx);
        assert_pop1!(vm, BLOCK_END, ctx);
        assert_pop1!(vm, CURSOR_SPLIT, ctx);
        assert_pop1!(vm, COLUMN_NEXT, ctx);

        // Moves into Insert mode after "A".
        ctx.persist.shape = None;
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back into Normal mode.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Move into Visual mode (blockwise)
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(ctl!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // Change block ("c").
        ctx.persist.insert = Some(InsertStyle::Insert);
        let act = selop!(EditAction::Delete);
        vm.input_key(key!('c'));
        assert_pop1!(vm, BLOCK_SPLIT, ctx);
        assert_pop1!(vm, BLOCK_BEG, ctx);
        assert_pop1!(vm, act, ctx);
        assert_pop1!(vm, CURSOR_SPLIT, ctx);

        // Moves into Insert mode after "c".
        ctx.persist.shape = None;
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Move back into Normal mode.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);
    }

    #[test]
    fn test_visual_motion() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "w" moves forward to the next word beginning
        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));
        ctx.action.count = Some(5);
        vm.input_key(key!('5'));
        vm.input_key(key!('w'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "e" moves forward to the next word end
        let mov = mv!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next));
        ctx.action.count = Some(5);
        vm.input_key(key!('5'));
        vm.input_key(key!('e'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "W" moves forward to the next WORD beginning
        let mov = mv!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Next));
        ctx.action.count = Some(10);
        vm.input_key(key!('1'));
        vm.input_key(key!('0'));
        vm.input_key(key!('W'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "E" moves forward to the next WORD ending
        let mov = mv!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Next));
        ctx.action.count = Some(10);
        vm.input_key(key!('1'));
        vm.input_key(key!('0'));
        vm.input_key(key!('E'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "b" moves backward to the previous WORD beginning
        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous));
        ctx.action.count = None;
        vm.input_key(key!('b'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "B" moves WORD forward
        let mov = mv!(MoveType::WordBegin(WordStyle::Big, MoveDir1D::Previous));
        ctx.action.count = None;
        vm.input_key(key!('B'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "E" moves forward to the next WORD ending
        let mov = mv!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Previous));
        ctx.action.count = Some(7);
        vm.input_key(key!('7'));
        vm.input_key(key!('g'));
        vm.input_key(key!('e'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        // "E" moves forward to the next WORD ending
        let mov = mv!(MoveType::WordEnd(WordStyle::Big, MoveDir1D::Previous));
        ctx.action.count = Some(9);
        vm.input_key(key!('9'));
        vm.input_key(key!('g'));
        vm.input_key(key!('E'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);
    }

    #[test]
    fn test_force_motion() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));

        ctx.action.operation = EditAction::Delete;

        // By default, there's no shape in the context.
        ctx.persist.shape = None;
        vm.input_key(key!('d'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // "v" forces charwise
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('d'));
        vm.input_key(key!('v'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
        assert_eq!(vm.state().persist.shape, None);

        // "V" forces linewise
        ctx.persist.shape = Some(TargetShape::LineWise);
        vm.input_key(key!('d'));
        vm.input_key(key!('V'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
        assert_eq!(vm.state().persist.shape, None);

        // <C-V> forces blockwise
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(key!('d'));
        vm.input_key(ctl!('v'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
        assert_eq!(vm.state().persist.shape, None);

        // If multiple force-motion keys are pressed, most recent is used.
        ctx.persist.shape = Some(TargetShape::BlockWise);
        vm.input_key(key!('d'));
        vm.input_key(key!('v'));
        vm.input_key(key!('V'));
        vm.input_key(ctl!('v'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
        assert_eq!(vm.state().persist.shape, None);
    }

    #[test]
    fn test_insert_mode() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        ctx.persist.insert = Some(InsertStyle::Insert);

        vm.input_key(key!('i'));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        vm.input_key(key!('i'));
        assert_pop1!(vm, typechar!('i'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        vm.input_key(key!('I'));
        assert_pop1!(vm, typechar!('I'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        vm.input_key(key!('d'));
        assert_pop1!(vm, typechar!('d'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        vm.input_key(key!('C'));
        assert_pop1!(vm, typechar!('C'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        vm.input_key(key!('$'));
        assert_pop1!(vm, typechar!('$'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Type a digraph.
        vm.input_key(ctl!('k'));
        assert_eq!(vm.get_cursor_indicator(), Some('?'));

        vm.input_key(key!('L'));
        assert_eq!(vm.get_cursor_indicator(), Some('L'));

        ctx.ch.digraph1 = Some('L');
        ctx.ch.digraph2 = Some('i');
        vm.input_key(key!('i'));
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Type a literal.
        vm.input_key(ctl!('v'));
        assert_eq!(vm.get_cursor_indicator(), Some('^'));

        ctx.ch.digraph1 = None;
        ctx.ch.digraph2 = None;
        ctx.ch.hex = Some(0x07);
        vm.input_key(ctl!('g'));
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Type a codepoint.
        ctx.ch.hex = Some(b'a' as u32);
        vm.input_key(ctl!('v'));
        vm.input_key(key!('o'));
        vm.input_key(key!('1'));
        vm.input_key(key!('4'));
        vm.input_key(key!('1'));
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        ctx.ch.hex = None;

        // Enter Replace mode by pressing <Ins>.
        ctx.persist.insert = Some(InsertStyle::Replace);
        vm.input_key(key!(KeyCode::Insert));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        vm.input_key(key!('d'));
        assert_pop1!(vm, typechar!('d'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        let mov =
            mvop!(EditAction::Motion, MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous));

        vm.input_key(key!(KeyCode::Left, KeyModifiers::SHIFT));
        assert_pop1!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        let mov =
            mvop!(EditAction::Motion, MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));

        vm.input_key(key!(KeyCode::Right, KeyModifiers::SHIFT));
        assert_pop1!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        let it = InsertTextAction::Paste(PasteStyle::Cursor, Count::Exact(1));
        ctx.action.register = Some(Register::Named('z'));
        ctx.action.register_append = false;
        vm.input_key(ctl!('r'));
        assert_eq!(vm.pop(), None);
        assert_eq!(vm.get_cursor_indicator(), Some('"'));
        vm.input_key(key!('z'));
        assert_pop1!(vm, Action::from(it), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);
        assert_eq!(vm.get_cursor_indicator(), None);

        // Pressing ^R^C should go back to Normal mode.
        ctx.action.register = None;
        vm.input_key(ctl!('r'));
        assert_eq!(vm.pop(), None);
        assert_eq!(vm.get_cursor_indicator(), Some('"'));
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);
        assert_eq!(vm.get_cursor_indicator(), None);
    }

    #[test]
    fn test_insert_jk() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        ctx.persist.insert = Some(InsertStyle::Insert);

        let step = InputStep::new().goto(VimMode::Normal);
        add_mapping(&mut vm, &IMAP, "jk", &step);

        // Go to Insert mode
        vm.input_key(key!('i'));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Input "jjk", which should type the first "j", and then go to Normal mode.
        vm.input_key(key!('j'));
        vm.input_key(key!('j'));
        vm.input_key(key!('k'));
        assert_pop1!(vm, typechar!('j'), ctx);
        assert_insert_exit!(vm, ctx);
    }

    #[test]
    fn test_custom_operators() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let step = InputStep::new().operator(EditAction::Delete, Some(VimMode::Insert));
        add_mapping(&mut vm, &NMAP, "R", &step);

        let mov = mv!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next));
        ctx.action.operation = EditAction::Delete;
        ctx.action.count = Some(5);
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('5'));
        vm.input_key(key!('R'));
        vm.input_key(key!('w'));
        assert_pop1!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);
    }

    #[test]
    fn test_override() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Check the original Normal mode mapping.
        let mov = mv!(MoveType::ScreenLine(MoveDir1D::Next));
        vm.input_key(key!('g'));
        vm.input_key(key!('j'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Override "gj" so that it doesn't do screen line movement.
        let step = edit_end!(MoveType::Line(MoveDir1D::Next));
        add_mapping(&mut vm, &NMAP, "gj", &step);

        // Normal mode "gj" should be overridden now.
        let mov = mv!(MoveType::Line(MoveDir1D::Next));
        vm.input_key(key!('g'));
        vm.input_key(key!('j'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Other Normal mode mappings beginning with "g" should still work.
        let mov = mv!(MoveType::ScreenLine(MoveDir1D::Previous));
        vm.input_key(key!('g'));
        vm.input_key(key!('k'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        let mov = mv!(MoveType::ScreenLinePos(MovePosition::Beginning), 0);
        vm.input_key(key!('g'));
        vm.input_key(key!('0'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);

        // Visual mode "gj" should still be the original mapping.
        let mov = mv!(MoveType::ScreenLine(MoveDir1D::Next));
        ctx.persist.shape = Some(TargetShape::CharWise);
        vm.input_key(key!('v'));
        assert_pop2!(vm, CURRENT_POS, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);

        vm.input_key(key!('g'));
        vm.input_key(key!('j'));
        assert_pop2!(vm, mov, ctx);
        assert_eq!(vm.mode(), VimMode::Visual);
    }

    #[test]
    fn test_count_alters_motion() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Without a count, "%" is ItemMatch.
        let mot = mv!(MoveType::ItemMatch);

        vm.input_key(key!('%'));
        assert_pop1!(vm, mot, ctx);
        assert_normal!(vm, ctx);

        ctx.action.operation = EditAction::Delete;
        vm.input_key(key!('d'));
        vm.input_key(key!('%'));
        assert_pop1!(vm, mot, ctx);
        assert_normal!(vm, ctx);

        // With a count, "%" becomes BufferLinePercent.
        let mot = mv!(MoveType::BufferLinePercent);

        ctx.action.count = Some(1);
        ctx.action.operation = EditAction::Motion;
        vm.input_key(key!('1'));
        vm.input_key(key!('%'));
        assert_pop1!(vm, mot, ctx);
        assert_normal!(vm, ctx);

        ctx.action.count = Some(88);
        ctx.action.operation = EditAction::Yank;
        vm.input_key(key!('8'));
        vm.input_key(key!('8'));
        vm.input_key(key!('y'));
        vm.input_key(key!('%'));
        assert_pop1!(vm, mot, ctx);
        assert_normal!(vm, ctx);

        ctx.action.count = Some(101);
        ctx.action.operation = EditAction::Delete;
        vm.input_key(key!('d'));
        vm.input_key(key!('1'));
        vm.input_key(key!('0'));
        vm.input_key(key!('1'));
        vm.input_key(key!('%'));
        assert_pop1!(vm, mot, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_count_alters_window() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Without a count, ^Wo closes all windows besides the currently focused one.
        let target = WindowTarget::AllBut(FocusChange::Current);
        let act: Action = WindowAction::Close(target, CloseFlags::QUIT).into();

        ctx.action.count = None;
        vm.input_key(ctl!('w'));
        vm.input_key(key!('o'));
        assert_pop1!(vm, act, ctx);
        assert_normal!(vm, ctx);

        ctx.action.count = None;
        vm.input_key(ctl!('w'));
        vm.input_key(ctl!('o'));
        assert_pop1!(vm, act, ctx);
        assert_normal!(vm, ctx);

        // With a count ^Wo closes all but the specified window.
        let target = WindowTarget::AllBut(FocusChange::Offset(Count::Contextual, true));
        let act: Action = WindowAction::Close(target, CloseFlags::QUIT).into();

        ctx.action.count = Some(5);
        vm.input_key(key!('5'));
        vm.input_key(ctl!('w'));
        vm.input_key(key!('o'));
        assert_pop1!(vm, act, ctx);
        assert_normal!(vm, ctx);

        ctx.action.count = Some(8);
        vm.input_key(key!('8'));
        vm.input_key(ctl!('w'));
        vm.input_key(ctl!('o'));
        assert_pop1!(vm, act, ctx);
        assert_normal!(vm, ctx);

        // Without a count ^Wv splits the window.
        let act: Action = WindowAction::Split(
            OpenTarget::Current,
            Axis::Vertical,
            MoveDir1D::Previous,
            Count::Contextual,
        )
        .into();
        ctx.action.count = None;
        vm.input_key(ctl!('w'));
        vm.input_key(key!('v'));
        assert_pop1!(vm, act, ctx);
        assert_normal!(vm, ctx);

        // With a count ^Wv splits the window and resizes it.
        let acts: Action = WindowAction::Open(
            OpenTarget::Current,
            Axis::Vertical,
            MoveDir1D::Previous,
            Count::Contextual,
        )
        .into();
        ctx.action.count = Some(10);
        vm.input_key(key!('1'));
        vm.input_key(key!('0'));
        vm.input_key(ctl!('w'));
        vm.input_key(key!('v'));
        assert_pop1!(vm, acts, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_scrollcp() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Place cursored line at the top of the screen with "zt".
        let act = Action::Scroll(ScrollStyle::CursorPos(MovePosition::Beginning, Axis::Vertical));
        vm.input_key(key!('z'));
        vm.input_key(key!('t'));
        assert_pop1!(vm, act, ctx);
        assert_normal!(vm, ctx);

        // Adding a count to "zt" makes it go to a specific line.
        let act = Action::Scroll(ScrollStyle::LinePos(MovePosition::Beginning, Count::Contextual));
        ctx.action.count = Some(2118);
        vm.input_key(key!('2'));
        vm.input_key(key!('1'));
        vm.input_key(key!('1'));
        vm.input_key(key!('8'));
        vm.input_key(key!('z'));
        vm.input_key(key!('t'));
        assert_pop1!(vm, act, ctx);
        assert_normal!(vm, ctx);

        // "z<Enter>" works like "zt", but it also goes to the first word on the line.
        let actfw = mvop!(EditAction::Motion, MoveType::FirstWord(MoveDir1D::Next), 0);
        let actcp = Action::Scroll(ScrollStyle::CursorPos(MovePosition::Beginning, Axis::Vertical));
        ctx.action.count = None;
        vm.input_key(key!('z'));
        vm.input_key(key!(KeyCode::Enter));
        assert_pop1!(vm, actfw, ctx);
        assert_pop1!(vm, actcp, ctx);
        assert_normal!(vm, ctx);

        // Like with "zt", giving "z<Enter>" a count goes to that line.
        let actfw = mvop!(EditAction::Motion, MoveType::FirstWord(MoveDir1D::Next), 0);
        let actlp =
            Action::Scroll(ScrollStyle::LinePos(MovePosition::Beginning, Count::Contextual));
        ctx.action.count = Some(1312);
        vm.input_key(key!('1'));
        vm.input_key(key!('3'));
        vm.input_key(key!('1'));
        vm.input_key(key!('2'));
        vm.input_key(key!('z'));
        vm.input_key(key!(KeyCode::Enter));
        assert_pop1!(vm, actfw, ctx);
        assert_pop1!(vm, actlp, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_literal() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('i'));
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Test that cursor indicator gets set to '^' while we're typing.
        vm.input_key(ctl!('v'));
        assert_eq!(vm.pop(), None);
        assert_eq!(vm.mode(), VimMode::Insert);
        assert_eq!(vm.get_cursor_indicator(), Some('^'));

        ctx.ch.hex = Some(0x1B);
        vm.input_key(key!(KeyCode::Esc));
        assert_pop2!(vm, TYPE_CONTEXTUAL, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);
        assert_eq!(vm.get_cursor_indicator(), None);

        // Test that typing in a full octal sequence works.
        ctx.ch.hex = Some(0x7F);
        vm.input_key(ctl!('v'));
        vm.input_key(key!('o'));
        vm.input_key(key!('1'));
        vm.input_key(key!('7'));
        vm.input_key(key!('7'));
        assert_pop2!(vm, TYPE_CONTEXTUAL, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Test that valid octal character types normally afterwards.
        ctx.ch.hex = None;
        vm.input_key(key!('7'));
        assert_pop2!(vm, typechar!('7'), ctx);

        // Test that typing in an incomplete octal sequence works.
        vm.input_key(ctl!('v'));
        vm.input_key(key!('o'));
        vm.input_key(key!('7'));
        vm.input_key(key!('8'));

        ctx.ch.hex = Some(0x7);
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);

        ctx.ch.hex = None;
        assert_pop2!(vm, typechar!('8'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Test that typing in a decimal sequence works.
        vm.input_key(ctl!('v'));
        vm.input_key(key!('1'));
        vm.input_key(key!('2'));
        vm.input_key(key!('3'));
        vm.input_key(key!('4'));

        ctx.ch.hex = Some(0x7B);
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);

        ctx.ch.hex = None;
        assert_pop2!(vm, typechar!('4'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Test that typing in a hexadecimal sequence works.
        vm.input_key(ctl!('v'));
        vm.input_key(key!('u'));
        vm.input_key(key!('2'));
        vm.input_key(key!('6'));
        vm.input_key(key!('0'));
        vm.input_key(key!('3'));
        vm.input_key(key!('3'));

        ctx.ch.hex = Some(0x2603);
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);

        ctx.ch.hex = None;
        assert_pop2!(vm, typechar!('3'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Test that typing in a full lowercase hexadecimal sequence works.
        vm.input_key(ctl!('v'));
        vm.input_key(key!('U'));
        vm.input_key(key!('0'));
        vm.input_key(key!('0'));
        vm.input_key(key!('0'));
        vm.input_key(key!('1'));
        vm.input_key(key!('f'));
        vm.input_key(key!('7'));
        vm.input_key(key!('5'));
        vm.input_key(key!('e'));
        vm.input_key(key!('a'));

        ctx.ch.hex = Some(0x1F75E);
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);

        ctx.ch.hex = None;
        assert_pop2!(vm, typechar!('a'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Test that typing in an incomplete uppercase hexadecimal sequence works.
        vm.input_key(ctl!('v'));
        vm.input_key(key!('U'));
        vm.input_key(key!('1'));
        vm.input_key(key!('F'));
        vm.input_key(key!('4'));
        vm.input_key(key!('6'));
        vm.input_key(key!('D'));
        vm.input_key(key!('G'));

        ctx.ch.hex = Some(0x1F46D);
        assert_pop1!(vm, TYPE_CONTEXTUAL, ctx);

        ctx.ch.hex = None;
        assert_pop2!(vm, typechar!('G'), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);
    }

    #[test]
    fn test_unmapped_reset() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        /*
         * The key "z" is not mapped in Operator Pending mode, so the action context should be
         * reset when it's pressed, causing "l" to be interpreted as a movement.
         */
        vm.input_key(key!('c'));
        vm.input_key(key!('z'));

        ctx.action.operation = EditAction::Delete;
        ctx.persist.insert = Some(InsertStyle::Insert);
        assert_pop1!(vm, Action::NoOp, ctx);
        assert_normal!(vm, ctx);

        let mov = mv!(MoveType::Column(MoveDir1D::Next, false));
        ctx.action.operation = EditAction::Motion;
        ctx.persist.insert = None;
        vm.input_key(key!('l'));
        assert_pop1!(vm, mov, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_count_nullifies() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        // Without a count, Delete deletes one character.
        let op = EditAction::Delete;
        let mov = MoveType::Column(MoveDir1D::Next, false);
        let mov = EditTarget::Motion(mov, Count::Contextual);
        let mov = EditorAction::Edit(op.into(), mov);
        vm.input_key(key!(KeyCode::Delete));
        assert_pop1!(vm, Action::from(mov), ctx);
        assert_normal!(vm, ctx);

        // With a count, Delete does nothing.
        ctx.action.count = Some(1);
        ctx.action.operation = EditAction::Motion;
        vm.input_key(key!('1'));
        vm.input_key(key!(KeyCode::Delete));
        assert_pop1!(vm, Action::NoOp, ctx);
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_macro_toggle() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let toggle = Action::from(MacroAction::ToggleRecording);

        // The first "q" does nothing.
        vm.input_key(key!('q'));
        assert_eq!(vm.pop(), None);

        // The second "q" is treated as a register name, and recording starts.
        vm.input_key(key!('q'));

        ctx.action.register = Some(Register::Named('q'));
        assert_pop1!(vm, toggle, ctx);

        ctx.action.register = None;
        assert_normal!(vm, ctx);
        assert_eq!(vm.state().persist.recording, Some((Register::Named('q'), false)));

        // Type "gqq" to format.
        let format = rangeop!(EditAction::Format, RangeType::Line);
        ctx.action.operation = EditAction::Format;
        vm.input_key(key!('g'));
        vm.input_key(key!('q'));
        vm.input_key(key!('q'));
        assert_pop1!(vm, format, ctx);
        assert_normal!(vm, ctx);

        // Type "q" to end recording.
        ctx.action.operation = EditAction::Motion;
        vm.input_key(key!('q'));
        assert_pop1!(vm, toggle, ctx);
        assert_normal!(vm, ctx);
        assert_eq!(vm.state().persist.recording, None);
    }

    #[test]
    fn test_edit_repeat() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let col = MoveType::Column(MoveDir1D::Next, false);

        // Feed in a tracked sequence.
        ctx.action.operation = EditAction::Delete;
        ctx.action.count = Some(2);
        vm.input_key(key!('2'));
        vm.input_key(key!('d'));
        vm.input_key(key!('l'));
        assert_pop1!(vm, mv!(col), ctx);
        assert_normal!(vm, ctx);

        // Feed in a breaking sequence.
        ctx.action.operation = EditAction::Motion;
        ctx.action.count = Some(3);
        vm.input_key(key!('3'));
        vm.input_key(key!('l'));
        assert_pop1!(vm, mv!(col), ctx);
        assert_normal!(vm, ctx);

        // Press ".".
        ctx.action.operation = EditAction::Motion;
        ctx.action.count = None;
        vm.input_key(key!('.'));
        assert_pop1!(vm, Action::Repeat(RepeatType::EditSequence), ctx);

        // Check that repeating does 2dw action.
        ctx.action.operation = EditAction::Delete;
        ctx.action.count = Some(2);
        vm.repeat(RepeatType::EditSequence, None);
        assert_pop1!(vm, mv!(col), ctx);

        // Read the Checkpoint from pressing "." earlier.
        assert_normal!(vm, ctx);

        // Press ".".
        ctx.action.operation = EditAction::Motion;
        ctx.action.count = None;
        vm.input_key(key!('.'));
        assert_pop1!(vm, Action::Repeat(RepeatType::EditSequence), ctx);

        // Check that we can override the count and register.
        ctx.action.operation = EditAction::Delete;
        ctx.action.count = Some(4);
        ctx.action.register = Some(Register::Named('a'));
        vm.repeat(RepeatType::EditSequence, Some(ctx.clone().into()));
        assert_pop1!(vm, mv!(col), ctx);

        // Read the Checkpoint from pressing "." earlier.
        assert_normal!(vm, ctx);

        // Press ".".
        ctx.action.operation = EditAction::Motion;
        ctx.action.count = None;
        ctx.action.register = None;
        vm.input_key(key!('.'));
        assert_pop1!(vm, Action::Repeat(RepeatType::EditSequence), ctx);

        // Repeating without a context now uses the overriden context.
        ctx.action.operation = EditAction::Delete;
        ctx.action.count = Some(4);
        ctx.action.register = Some(Register::Named('a'));
        vm.repeat(RepeatType::EditSequence, None);
        assert_pop1!(vm, mv!(col), ctx);

        // Read the Checkpoint from pressing "." earlier.
        assert_normal!(vm, ctx);

        // Feed in a change sequence.
        ctx.action.count = Some(2);
        ctx.action.register = None;
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('2'));
        vm.input_key(key!('c'));
        vm.input_key(key!('l'));
        assert_pop2!(vm, mv!(col), ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Type some new characters.
        let ch_2 = typechar!('2');
        let ch_c = typechar!('c');
        let ch_l = typechar!('l');

        ctx.action.operation = EditAction::Motion;
        ctx.action.count = None;
        vm.input_key(key!('2'));
        vm.input_key(key!('c'));
        vm.input_key(key!('l'));

        assert_pop1!(vm, ch_2.clone(), ctx);
        assert_pop1!(vm, ch_c.clone(), ctx);
        assert_pop1!(vm, ch_l.clone(), ctx);

        // Back to Normal mode.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Press ".".
        vm.input_key(key!('.'));
        assert_pop1!(vm, Action::Repeat(RepeatType::EditSequence), ctx);

        // Repeat the whole change sequence.
        vm.repeat(RepeatType::EditSequence, None);

        ctx.action.operation = EditAction::Delete;
        ctx.action.count = Some(2);
        ctx.persist.insert = Some(InsertStyle::Insert);
        assert_pop1!(vm, mv!(col), ctx);

        ctx.action.operation = EditAction::Motion;
        ctx.action.count = None;
        assert_pop1!(vm, ch_2.clone(), ctx);
        assert_pop1!(vm, ch_c.clone(), ctx);
        assert_pop1!(vm, ch_l.clone(), ctx);

        ctx.persist.insert = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);

        // Read the Checkpoint from pressing "." earlier.
        assert_normal!(vm, ctx);
    }

    #[test]
    fn test_edit_repeat_append_line() {
        let mut vm: VimMachine<TerminalKey> = default_vim_keys();
        let mut ctx = mkctx();

        let op = EditAction::Motion;
        let mov = mvop!(op, MoveType::LinePos(MovePosition::End), 0);

        // Move down a line, so we do SequenceStatus::Break.
        vm.input_key(key!('j'));
        assert_pop1!(vm, mv!(MoveType::Line(MoveDir1D::Next)), ctx);
        assert_normal!(vm, ctx);

        // Move to Insert mode at end of line.
        ctx.persist.insert = Some(InsertStyle::Insert);
        vm.input_key(key!('A'));
        assert_pop1!(vm, mov, ctx);
        assert_pop2!(vm, CURSOR_SPLIT, ctx);
        assert_eq!(vm.mode(), VimMode::Insert);

        // Type some characters.
        vm.input_key(key!('a'));
        assert_pop1!(vm, typechar!('a'), ctx);

        vm.input_key(key!('b'));
        assert_pop1!(vm, typechar!('b'), ctx);

        vm.input_key(key!('c'));
        assert_pop1!(vm, typechar!('c'), ctx);

        // Back to Normal mode.
        vm.input_key(ctl!('c'));
        assert_insert_exit!(vm, ctx);

        // Move down a line.
        vm.input_key(key!('j'));
        assert_pop1!(vm, mv!(MoveType::Line(MoveDir1D::Next)), ctx);
        assert_normal!(vm, ctx);

        // Press ".".
        vm.input_key(key!('.'));
        assert_pop1!(vm, Action::Repeat(RepeatType::EditSequence), ctx);

        // Repeat the whole append sequence, including moving to EOL.
        vm.repeat(RepeatType::EditSequence, Some(ctx.clone().into()));

        ctx.persist.insert = Some(InsertStyle::Insert);
        assert_pop1!(vm, mov, ctx);
        assert_pop1!(vm, CURSOR_SPLIT, ctx);
        assert_pop1!(vm, typechar!('a'), ctx);
        assert_pop1!(vm, typechar!('b'), ctx);
        assert_pop1!(vm, typechar!('c'), ctx);

        ctx.persist.insert = None;
        assert_pop1!(vm, CURSOR_CLOSE, ctx);

        // Read the Checkpoint from pressing "." earlier.
        assert_normal!(vm, ctx);
    }
}
