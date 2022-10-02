//! # Emacs Keybindings
//!
//! ## Overview
//!
//! This module handles mapping the keybindings used in Emacs onto the
//! [Action](crate::editing::base::Action) type.
//!
//! ## Divergences
//!
//! The keybindings here diverge from the defaults in Emacs in the following ways:
//!
//! - `C-_` and `C-x u` behave like `M-x undo-only`
//!
use bitflags::bitflags;
use crossterm::event::KeyEvent;

use crate::editing::base::{
    Action,
    Application,
    Axis,
    Case,
    Char,
    CloseFlags,
    CloseTarget,
    CommandBarAction,
    CommandType,
    Count,
    EditAction,
    EditTarget,
    FocusChange,
    HistoryAction,
    InsertStyle,
    InsertTextAction,
    MoveDir1D,
    MoveDir2D,
    MoveDirMod,
    MovePosition,
    MoveTerminus,
    MoveType,
    RangeType,
    Register,
    ScrollSize,
    ScrollStyle,
    SearchType,
    SelectionAction,
    SelectionCursorChange,
    SelectionResizeStyle,
    Specifier,
    TargetShape,
    WindowAction,
    WordStyle,
};

use super::{
    super::{keyparse::parse, CommonKeyClass},
    EmacsContext,
    EmacsMode,
};

use crate::input::bindings::{InputBindings, ModalMachine, Step};

bitflags! {
    struct MappedModes: u32 {
        const I = 0b0000000000000001;
        const S = 0b0000000000000010;

        const IS = MappedModes::I.bits | MappedModes::S.bits;
    }
}

const MAP: MappedModes = MappedModes::IS;
const IMAP: MappedModes = MappedModes::I;
const SMAP: MappedModes = MappedModes::S;

impl MappedModes {
    pub fn split(&self) -> Vec<EmacsMode> {
        let mut modes = Vec::new();

        if self.contains(MappedModes::I) {
            modes.push(EmacsMode::Insert);
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
    SetInsertStyle(InsertStyle),
    SetRegister(Register),
    SetTargetShape(TargetShape, bool),
}

impl InternalAction {
    pub fn run<P: Application>(&self, ctx: &mut EmacsContext<P>) {
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

#[derive(Clone, Debug)]
enum ExternalAction<P: Application> {
    Something(Action<P>),
}

impl<P: Application> ExternalAction<P> {
    fn resolve(&self, _: &mut EmacsContext<P>) -> Vec<Action<P>> {
        match self {
            ExternalAction::Something(act) => vec![act.clone()],
        }
    }
}

impl<P: Application> From<Action<P>> for ExternalAction<P> {
    fn from(act: Action<P>) -> Self {
        ExternalAction::Something(act)
    }
}

/// Description of actions to take after an input sequence.
#[derive(Clone, Debug)]
pub struct InputStep<P: Application> {
    internal: Vec<InternalAction>,
    external: Vec<ExternalAction<P>>,
    nextm: Option<EmacsMode>,
}

impl<P: Application> InputStep<P> {
    /// Create a new step that input keys can map to.
    pub fn new() -> Self {
        InputStep { internal: vec![], external: vec![], nextm: None }
    }

    /// Set the [actions](Action) that this step produces.
    pub fn actions(mut self, acts: Vec<Action<P>>) -> Self {
        self.external = acts.into_iter().map(ExternalAction::Something).collect();
        self
    }
}

impl<P: Application> Step<KeyEvent> for InputStep<P> {
    type A = Action<P>;
    type C = EmacsContext<P>;
    type M = EmacsMode;
    type Class = CommonKeyClass;

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

    fn step(&self, ctx: &mut EmacsContext<P>) -> (Vec<Action<P>>, Option<Self::M>) {
        for iact in self.internal.iter() {
            iact.run(ctx);
        }

        let external: Vec<Action<P>> =
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
        isv!(vec![$int], vec![ExternalAction::Something($ext)])
    };
    ($int: expr, $ext: expr, $ns: expr) => {
        isv!(vec![$int], vec![ExternalAction::Something($ext)], $ns)
    };
}

macro_rules! motion {
    ($mt: expr) => {
        is!(
            InternalAction::ClearTargetShape(true),
            Action::Edit(
                Specifier::Exact(EditAction::Motion),
                EditTarget::Motion($mt, Count::Contextual)
            )
        )
    };
    ($mt: expr, $c: literal) => {
        is!(
            InternalAction::ClearTargetShape(true),
            Action::Edit(
                Specifier::Exact(EditAction::Motion),
                EditTarget::Motion($mt, Count::Exact($c))
            )
        )
    };
    ($mt: expr, $c: expr) => {
        is!(
            InternalAction::ClearTargetShape(true),
            Action::Edit(Specifier::Exact(EditAction::Motion), EditTarget::Motion($mt, $c))
        )
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

macro_rules! erase_target {
    ($et: expr) => {
        is!(
            InternalAction::SetRegister(Register::Blackhole),
            Action::Edit(EditAction::Delete.into(), $et)
        )
    };
}

macro_rules! erase {
    ($mt: expr) => {
        erase_target!(EditTarget::Motion($mt, Count::Contextual))
    };
}

macro_rules! erase_range {
    ($rt: expr) => {
        erase_target!(EditTarget::Range($rt, true, Count::Contextual))
    };
}

macro_rules! just_one_space {
    () => {
        isv!(vec![InternalAction::SetRegister(Register::Blackhole)], vec![
            Action::Edit(
                EditAction::Delete.into(),
                RangeType::Word(WordStyle::Whitespace(false)).into()
            )
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
    ($type: expr) => {
        cmdbar!(CommandBarAction::Focus($type), EmacsMode::Search)
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

macro_rules! window_split {
    ($axis: expr) => {
        window!(WindowAction::Split($axis, MoveDir1D::Previous, Count::Contextual))
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
            Action::Selection(SelectionAction::Resize(
                SelectionResizeStyle::Restart,
                EditTarget::CurrentPosition
            ))
        )
    };
    ($shape: expr, $target: expr) => {
        is!(
            InternalAction::SetTargetShape($shape, false),
            Action::Selection(SelectionAction::Resize(SelectionResizeStyle::Restart, $target))
        )
    };
}

macro_rules! start_shift_selection {
    ($shape: expr, $target: expr) => {
        is!(
            InternalAction::SetTargetShape($shape, true),
            Action::Edit(Specifier::Exact(EditAction::Motion), $target)
        )
    };
}

#[rustfmt::skip]
fn default_keys<P: Application>() -> Vec<(MappedModes, &'static str, InputStep<P>)> {
    [
        // Insert and Search mode keybindings.
        ( MAP, "<C-Y>", paste!(MoveDir1D::Previous) ),
        ( MAP, "<M-BS>", kill!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( MAP, "<M-Del>", kill!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( MAP, "<BS>", erase!(MoveType::Column(MoveDir1D::Previous, true)) ),

        // Insert mode keybindings.
        ( IMAP, "<C-A>", motion!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( IMAP, "<C-B>", motion!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( IMAP, "<C-D>", erase!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( IMAP, "<C-E>", motion!(MoveType::LinePos(MovePosition::End), 0) ),
        ( IMAP, "<C-F>", motion!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( IMAP, "<C-G>", is!(InternalAction::ClearTargetShape(false), Action::Edit(EditAction::Motion.into(), EditTarget::CurrentPosition), EmacsMode::Insert) ),
        ( IMAP, "<C-J>", chartype!(Char::Single('\n')) ),
        ( IMAP, "<C-K>", kill!(MoveType::LinePos(MovePosition::End), 0) ),
        ( IMAP, "<C-L>", scroll!(ScrollStyle::CursorPos(MovePosition::Middle, Axis::Vertical)) ),
        ( IMAP, "<C-N>", motion!(MoveType::Line(MoveDir1D::Next)) ),
        ( IMAP, "<C-O>", insert_text!(InsertTextAction::OpenLine(TargetShape::CharWise, MoveDir1D::Previous, 1.into())) ),
        ( IMAP, "<C-P>", motion!(MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<C-Q>{oct<=3}", chartype!() ),
        ( IMAP, "<C-Q>{any}", chartype!() ),
        ( IMAP, "<C-R>", cmdbar_focus!(CommandType::Search(MoveDir1D::Previous, true)) ),
        ( IMAP, "<C-S>", cmdbar_focus!(CommandType::Search(MoveDir1D::Next, true)) ),
        ( IMAP, "<C-T>", unmapped!() ),
        ( IMAP, "<C-U><C-X>s", unmapped!() ),
        ( IMAP, "<C-V>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( IMAP, "<C-W>", kill_target!(EditTarget::Selection) ),
        ( IMAP, "<C-X><C-C>", window!(WindowAction::Close(CloseTarget::All, CloseFlags::QUIT)) ),
        ( IMAP, "<C-X><C-O>", unmapped!() ),
        ( IMAP, "<C-X><C-Q>", unmapped!() ),
        ( IMAP, "<C-X><C-S>", unmapped!() ),
        ( IMAP, "<C-X><C-T>", unmapped!() ),
        ( IMAP, "<C-X><C-W>", unmapped!() ),
        ( IMAP, "<C-X><C-X>", selection!(SelectionAction::CursorSet(SelectionCursorChange::SwapAnchor(false))) ),
        ( IMAP, "<C-X><C-Z>", act!(Action::Suspend) ),
        ( IMAP, "<C-X><Left>", unmapped!() ),
        ( IMAP, "<C-X><Right>", unmapped!() ),
        ( IMAP, "<C-X>b", unmapped!() ),
        ( IMAP, "<C-X>h", start_selection!(TargetShape::CharWise, RangeType::Buffer.into()) ),
        ( IMAP, "<C-X>k", unmapped!() ),
        ( IMAP, "<C-X>o", window_focus!(FocusChange::Direction1D(MoveDir1D::Next, Count::Exact(1), true)) ),
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
        ( IMAP, "<C-Del>", kill!(MoveType::LinePos(MovePosition::End), 0) ),
        ( IMAP, "<C-Space>", start_selection!(TargetShape::CharWise) ),
        ( IMAP, "<C-Left>", motion!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( IMAP, "<C-Right>", motion!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next)) ),
        ( IMAP, "<M-b>", motion!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( IMAP, "<M-c>", edit!(EditAction::ChangeCase(Case::Title), MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( IMAP, "<M-d>", kill!(MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( IMAP, "<M-f>", motion!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next)) ),
        ( IMAP, "<M-l>", edit!(EditAction::ChangeCase(Case::Lower), MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( IMAP, "<M-s>.", search_word!(WordStyle::Big) ),
        ( IMAP, "<M-t>", unmapped!() ),
        ( IMAP, "<M-u>", edit!(EditAction::ChangeCase(Case::Upper), MoveType::WordEnd(WordStyle::Little, MoveDir1D::Next)) ),
        ( IMAP, "<M-v>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),
        ( IMAP, "<M-w>", kill_target!(EditTarget::Selection) ),
        ( IMAP, "<M-z>{char}", kill_target!(EditTarget::Search(SearchType::Char(true), MoveDirMod::Same, Count::Contextual)) ),
        ( IMAP, "<M-S>.", search_word!(WordStyle::Big) ),
        ( IMAP, "<M-\\>", erase_range!(RangeType::Word(WordStyle::Whitespace(false))) ),
        ( IMAP, "<M-^>", edit!(EditAction::Join(true), MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<M-<>", edit_buffer!(EditAction::Motion, MoveTerminus::Beginning) ),
        ( IMAP, "<M->>", edit_buffer!(EditAction::Motion, MoveTerminus::End) ),
        ( IMAP, "<M-Space>", just_one_space!() ),
        ( IMAP, "<M-Left>", motion!(MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous)) ),
        ( IMAP, "<M-Right>", motion!(MoveType::WordBegin(WordStyle::NonAlphaNum, MoveDir1D::Next)) ),
        ( IMAP, "<S-End>", start_shift_selection!(TargetShape::CharWise, MoveType::LinePos(MovePosition::End).into()) ),
        ( IMAP, "<S-Home>", start_shift_selection!(TargetShape::CharWise, MoveType::LinePos(MovePosition::Beginning).into()) ),
        ( IMAP, "<S-Up>", start_shift_selection!(TargetShape::CharWise, MoveType::Line(MoveDir1D::Previous).into()) ),
        ( IMAP, "<S-Down>", start_shift_selection!(TargetShape::CharWise, MoveType::Line(MoveDir1D::Next).into()) ),
        ( IMAP, "<S-Left>", start_shift_selection!(TargetShape::CharWise, MoveType::Column(MoveDir1D::Previous, true).into()) ),
        ( IMAP, "<S-Right>", start_shift_selection!(TargetShape::CharWise, MoveType::Column(MoveDir1D::Next, true).into()) ),
        ( IMAP, "<Del>", erase!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( IMAP, "<End>", motion!(MoveType::LinePos(MovePosition::End), 0) ),
        ( IMAP, "<Home>", motion!(MoveType::LinePos(MovePosition::Beginning), 0) ),
        ( IMAP, "<Insert>", iact!(InternalAction::SetInsertStyle(InsertStyle::Replace)) ),
        ( IMAP, "<Up>", motion!(MoveType::Line(MoveDir1D::Previous)) ),
        ( IMAP, "<Down>", motion!(MoveType::Line(MoveDir1D::Next)) ),
        ( IMAP, "<Left>", motion!(MoveType::Column(MoveDir1D::Previous, true)) ),
        ( IMAP, "<Right>", motion!(MoveType::Column(MoveDir1D::Next, true)) ),
        ( IMAP, "<PageDown>", scroll2d!(MoveDir2D::Down, ScrollSize::Page) ),
        ( IMAP, "<PageUp>", scroll2d!(MoveDir2D::Up, ScrollSize::Page) ),

        // Search mode keybindings.
        ( SMAP, "<C-G>", cmdbar!(CommandBarAction::Abort, EmacsMode::Insert) ),
        ( SMAP, "<C-R>", search!(MoveDir1D::Previous) ),
        ( SMAP, "<C-S>", search!(MoveDir1D::Next) ),
    ].to_vec()
}

#[rustfmt::skip]
fn default_enter<P: Application>() -> Vec<(MappedModes, &'static str, InputStep<P>)> {
    [
        // <Enter> in Insert mode types a newline character.
        ( IMAP, "<Enter>", chartype!(Char::Single('\n')) ),
    ].to_vec()
}

#[rustfmt::skip]
fn submit_on_enter<P: Application>() -> Vec<(MappedModes, &'static str, InputStep<P>)> {
    [
        // <Enter> in Insert mode submits the command.
        ( IMAP, "<Enter>", cmdbar!(CommandBarAction::Submit) ),
    ].to_vec()
}

#[inline]
fn add_mapping<P: Application>(
    machine: &mut EmacsMachine<KeyEvent, P>,
    modes: &MappedModes,
    keys: &str,
    action: &InputStep<P>,
) {
    let (_, evs) = parse(keys).expect(&format!("invalid vim keybinding: {}", keys));
    let modes = modes.split();

    for mode in modes {
        machine.add_mapping(mode, &evs, &action);
    }
}

/// A configurable collection of Emacs bindings that can be added to a [ModalMachine].
#[derive(Debug)]
pub struct EmacsBindings<P: Application> {
    mappings: Vec<(MappedModes, &'static str, InputStep<P>)>,
    enter: Vec<(MappedModes, &'static str, InputStep<P>)>,
}

impl<P: Application> EmacsBindings<P> {
    /// Remap the Enter key to [submit](CommandBarAction::Submit) instead.
    pub fn submit_on_enter(mut self) -> Self {
        self.enter = submit_on_enter();
        self
    }
}

impl<P: Application> Default for EmacsBindings<P> {
    fn default() -> Self {
        EmacsBindings { mappings: default_keys(), enter: default_enter() }
    }
}

impl<P: Application> InputBindings<KeyEvent, InputStep<P>> for EmacsBindings<P> {
    fn setup(&self, machine: &mut EmacsMachine<KeyEvent, P>) {
        for (modes, keys, action) in self.mappings.iter() {
            add_mapping(machine, modes, keys, action);
        }

        for (modes, keys, action) in self.enter.iter() {
            add_mapping(machine, modes, keys, action);
        }
    }
}

/// Manage Emacs keybindings and modes.
pub type EmacsMachine<Key, T = ()> = ModalMachine<Key, InputStep<T>>;

impl<P: Application> Default for EmacsMachine<KeyEvent, P> {
    fn default() -> Self {
        ModalMachine::from_bindings::<EmacsBindings<P>>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    macro_rules! mv {
        ($mt: expr) => {
            Action::Edit(EditAction::Motion.into(), EditTarget::Motion($mt, Count::Contextual))
        };
        ($mt: expr, $c: literal) => {
            Action::Edit(EditAction::Motion.into(), EditTarget::Motion($mt, Count::Exact($c)))
        };
        ($mt: expr, $c: expr) => {
            Action::Edit(EditAction::Motion.into(), EditTarget::Motion($mt, $c))
        };
    }

    const CMDBAR_ABORT: Action = Action::CommandBar(CommandBarAction::Abort);
    const CMDBAR_SEARCH_NEXT: Action =
        Action::CommandBar(CommandBarAction::Focus(CommandType::Search(MoveDir1D::Next, true)));
    const CMDBAR_SEARCH_PREV: Action =
        Action::CommandBar(CommandBarAction::Focus(CommandType::Search(MoveDir1D::Previous, true)));

    #[test]
    fn test_selection_shift() {
        let mut vm: EmacsMachine<KeyEvent> = EmacsMachine::default();
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
        let mut vm: EmacsMachine<KeyEvent> = EmacsMachine::default();
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
        let mut vm: EmacsMachine<KeyEvent> = EmacsMachine::default();
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
}
