//! # keybindings
//!
//! ## Overview
//!
//! This crate provides environment-agnostic interfaces for tracking and processing modal
//! keybindings.
//!
//! The [ModalMachine] component allows consumers to build input processors that support multiple
//! modes of input, similar to applications descended from [vi](https://en.wikipedia.org/wiki/Vi).
//!
//! ModalMachine maintains a graph of [Step]-containing nodes, and follows edges with each
//! [InputKey] it receives. Consumers populate the graph by mapping an [EdgePath] that describes a
//! series of acceptible inputs to reach a [Step].
//!
//! When a new node is reached, [Step::step] is used to determine whether we can produce any
//! actions, and whether we need to transition to another [Mode]. If any actions are produced, the
//! keybinding is considered fully entered and the current [InputState] is taken. If the [Step]
//! transitions to another [Mode], then future input keys will be processed there. Otherwise,
//! future keys will continue to be processed from the top of the currently entered [Mode].
//!
//! ## Customization
//!
//! For straightforward keybindings, consumers only need an action type and a [Mode]. More complex
//! setups might require doing one or more of the following:
//!
//! * Implementing [InputKey] for the environment's key input (see [TerminalKey] for example)
//! * Defining a custom [Step] type
//! * Switching from [EmptyKeyClass] to a custom [InputKeyClass]
//! * Switching from [EmptyKeyState] to a custom [InputKeyState]
//! * Updating the context during [Mode::enter]
//!
//! ## Example
//!
//! Here is a program that builds keybindings with a Normal mode for sending commands
//! to the program, and an Insert mode for typing text. The Normal mode supports a "qq" sequence to
//! quit the program, and the Insert mode supports Escape to return to Normal mode.
//!
//! ```
//! use keybindings::{
//!     BindingMachine,
//!     EmptyKeyClass,
//!     EmptyKeyState,
//!     InputBindings,
//!     InputKey,
//!     InputState,
//!     ModalMachine,
//!     Mode,
//!     ModeKeys,
//!     Step,
//! };
//!
//! const ESC: char = '\u{1B}';
//!
//! #[derive(Clone, Copy, Debug, Default, Hash, Eq, PartialEq)]
//! enum ProgMode {
//!     #[default]
//!     Normal,
//!     Insert
//! }
//!
//! #[derive(Clone, Debug, Default, Eq, PartialEq)]
//! enum ProgAction {
//!     Type(char),
//!     #[default]
//!     NoOp,
//!     Quit
//! }
//!
//! #[derive(Default)]
//! struct ProgBindings { }
//!
//! impl Mode<ProgAction, EmptyKeyState> for ProgMode { }
//!
//! impl<K: InputKey> ModeKeys<K, ProgAction, EmptyKeyState> for ProgMode {
//!     fn unmapped(&self, key: &K, _: &mut EmptyKeyState) -> (Vec<ProgAction>, Option<ProgMode>) {
//!         match self {
//!             ProgMode::Normal => {
//!                 return (vec![], None);
//!             },
//!             ProgMode::Insert => {
//!                 if let Some(c) = key.get_char() {
//!                     return (vec![ProgAction::Type(c)], None);
//!                 }
//!
//!                 return (vec![], None);
//!             },
//!         }
//!     }
//! }
//!
//! impl InputBindings<char, ProgStep> for ProgBindings {
//!     fn setup(&self, machine: &mut ProgMachine) {
//!         use keybindings::EdgeRepeat::Once;
//!         use keybindings::EdgeEvent::Key;
//!
//!         // Insert mode mappings
//!         machine.add_mapping(ProgMode::Insert, &[
//!             (Once, Key(ESC))
//!         ], &(None, Some(ProgMode::Normal)));
//!
//!         // Normal mode mappings
//!         machine.add_mapping(ProgMode::Normal, &[
//!             (Once, Key('i'))
//!         ], &(None, Some(ProgMode::Insert)));
//!         machine.add_mapping(ProgMode::Normal, &[
//!             (Once, Key('q')),
//!             (Once, Key('q')),
//!         ], &(Some(ProgAction::Quit), None));
//!     }
//! }
//!
//! type ProgStep = (Option<ProgAction>, Option<ProgMode>);
//! type ProgMachine = ModalMachine<char, ProgStep>;
//!
//! fn main() {
//!     let mut pm = ProgMachine::from_bindings::<ProgBindings>();
//!     let ctx = EmptyKeyState::default();
//!
//!     // We begin in the Default mode, Normal.
//!     assert_eq!(pm.mode(), ProgMode::Normal);
//!
//!     // Pressing "i" takes us to Insert mode.
//!     pm.input_key('i');
//!     assert_eq!(pm.pop(), Some((ProgAction::NoOp, ctx.clone())));
//!     assert_eq!(pm.mode(), ProgMode::Insert);
//!
//!     // "q" is unmapped in Insert mode, and types a key.
//!     pm.input_key('q');
//!     assert_eq!(pm.pop(), Some((ProgAction::Type('q'), ctx.clone())));
//!     assert_eq!(pm.mode(), ProgMode::Insert);
//!
//!     // Escape takes us back to Normal mode.
//!     pm.input_key(ESC);
//!     assert_eq!(pm.pop(), Some((ProgAction::NoOp, ctx.clone())));
//!     assert_eq!(pm.mode(), ProgMode::Normal);
//!
//!     // A single "q" does nothing.
//!     pm.input_key('q');
//!     assert_eq!(pm.pop(), None);
//!     assert_eq!(pm.mode(), ProgMode::Normal);
//!
//!     // A second "q" produces the Quit action.
//!     pm.input_key('q');
//!     assert_eq!(pm.pop(), Some((ProgAction::Quit, ctx.clone())));
//!     assert_eq!(pm.mode(), ProgMode::Normal);
//! }
//! ```
//!
//! [TerminalKey]: https://docs.rs/modalkit/0.0.17/modalkit/input/key/struct.TerminalKey.html

// Require docs for public APIs, and disable the more annoying clippy lints.
#![deny(missing_docs)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::manual_range_contains)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::needless_return)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::type_complexity)]
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

pub mod dialog;

use self::dialog::Dialog;

/// Represents contextual information that is updated upon user input.
pub trait InputState {
    /// The output context type returned along with actions.
    type Output: Clone + Default;

    /// Reset any action-specific state.
    fn reset(&mut self);

    /// Return a copy of the InputState, and reset any action-specific state.
    fn take(&mut self) -> Self::Output;

    /// Copy any overriding values into an `Output` object.
    fn merge(original: Self::Output, overrides: &Self::Output) -> Self::Output;
}

/// Trait for keys that can be used with [ModalMachine].
pub trait InputKey: Clone + Hash + Eq {
    /// The error type returned when parsing a macro string fails.
    type Error;

    /// If the input that produced this key could possibly represent two keys, split out the first
    /// key.
    ///
    /// For example, this can occur in Unix terminals where ^[ is used to represent when Alt has
    /// been pressed. Rapidly typing Escape + "b" will produce ^[b, which may then be parsed as a
    /// single Alt-b keypress.
    fn decompose(&mut self) -> Option<Self>;

    /// Parse a string representing a series of keypresses.
    fn from_macro_str(mstr: &str) -> Result<Vec<Self>, Self::Error>;

    /// Return this key's representation as a single, printable codepoint, if it exists.
    fn get_char(&self) -> Option<char>;
}

impl InputKey for char {
    type Error = std::convert::Infallible;

    fn decompose(&mut self) -> Option<Self> {
        None
    }

    fn from_macro_str(mstr: &str) -> Result<Vec<Self>, Self::Error> {
        Ok(mstr.chars().collect())
    }

    fn get_char(&self) -> Option<char> {
        Some(*self)
    }
}

/// Trait for context objects used within [ModalMachine].
#[allow(unused_variables)]
pub trait InputKeyState<Key, C: InputKeyClass<Key>>: InputState {
    /// Update the context as needed after a `Key` has matched an [EdgeEvent].
    fn event(&mut self, event: &EdgeEvent<Key, C>, key: &Key) {}

    /// Return a character to show at the current cursor position.
    fn get_cursor_indicator(&self) -> Option<char> {
        None
    }
}

/// Trait for the input modes specific to a consumer.
#[allow(unused_variables)]
pub trait Mode<A, C>: Copy + Clone + Debug + Default + Hash + Eq + PartialEq {
    /// Perform any necessary updates when entering a new mode.
    ///
    /// This method is only called when a mode has been fully entered. Modes entered
    /// via [Fallthrough](EdgeEvent::Fallthrough) do not result in this method being called.
    ///
    /// If no actions are generated here, then the default action will be generated.
    fn enter(&self, previous_mode: Self, context: &mut C) -> Vec<A> {
        vec![]
    }

    /// Return a string to show on-screen that describes the current mode.
    fn show(&self, context: &C) -> Option<String> {
        None
    }
}

/// Key-specific behaviour associated with a [Mode].
#[allow(unused_variables)]
pub trait ModeKeys<Key, A, C>: Mode<A, C> {
    /// Return the default behaviour for the current mode when the given key is unmapped.
    ///
    /// If no actions are returned, then the [Default] value will be used.
    fn unmapped(&self, key: &Key, context: &mut C) -> (Vec<A>, Option<Self>) {
        (vec![], None)
    }
}

/// Sequence-specific behaviour associated with a [Mode].
#[allow(unused_variables)]
pub trait ModeSequence<S, A, C: InputState>: Mode<A, C> {
    /// Controls how and what gets included in the sequences of actions tracked by
    /// [ModalMachine]. When implementing, if there are actions that trigger calls to
    /// [BindingMachine::repeat], be careful that they do not get included in a way that can
    /// create cycles.
    ///
    /// By default, this will not place the action in any sequence.
    fn sequences(&self, action: &A, ctx: &C::Output) -> Vec<(S, SequenceStatus)> {
        vec![]
    }
}

/// Trait for the classes of input keys specific to a consumer.
///
/// For example, the input keys "0" to "9" might correspond to a `Count` variant in an
/// implementation.
///
/// You can use [InputKeyState::event] if you need to record what key was typed for a class
/// during a sequence of input keys.
pub trait InputKeyClass<T>: Clone + Hash + Eq {
    /// Return the classes that the [InputKey] belongs to.
    ///
    /// The order returned here is the order of priority for which [EdgeEvent::Class] edge to
    /// follow.
    fn memberships(ke: &T) -> Vec<Self>;
}

/// Different ways to include an action in the current action sequence.
pub enum SequenceStatus {
    /// Clear the sequence, start a new one with this action, and then perform a
    /// [SequenceStatus::Break].
    Atom,

    /// Don't include this action in the last sequence, and start a new sequence on the next
    /// [SequenceStatus::Track].
    Break,

    /// Don't include this action in the last sequence.
    Ignore,

    /// Clear the sequence and start a new one with this action.
    Restart,

    /// Include this action in the last sequence.
    Track,
}

/// Trait for controlling the behaviour of [ModalMachine] during a sequence of input keys.
pub trait Step<Key>: Clone {
    /// The type of output action produced after input.
    type A: Clone + Default;

    /// A context object for managing state that accompanies actions.
    type State: InputKeyState<Key, Self::Class>;

    /// Classes of input keys.
    type Class: InputKeyClass<Key>;

    /// The possible modes for mapping keys.
    type M: ModeKeys<Key, Self::A, Self::State> + ModeSequence<Self::Sequence, Self::A, Self::State>;

    /// A key type for identifying tracked action sequences which can be later repeated.
    type Sequence: Eq + Hash;

    /// Indicates whether this step should be treated as if it's an unmapped key, and
    /// reset to the root of the current mode.
    fn is_unmapped(&self) -> bool;

    /// Indicate a mode to fall through to when there is no other valid edge.
    ///
    /// This is useful for setting up keybindings that allow executing another mode's keys without
    /// permanently changing modes (e.g., ^O in Insert mode in Vim), or for creating a mode that
    /// represents common suffixes (e.g., Operation Pending mode in Vim).
    fn fallthrough(&self) -> Option<Self::M> {
        None
    }

    /// Called once the bindings that lead to this Step have been pressed.
    ///
    /// If this returns zero actions and no mode to go to, then [ModalMachine] will wait for further
    /// keypresses. This allows creating intermediate Steps that only change the context, or steps
    /// that are triggered conditionally (e.g., "q" in Vim stops a recording macro if it's already
    /// doing so, otherwise it waits for the next key to indicate the register for starting macro
    /// recording).
    fn step(&self, ctx: &mut Self::State) -> (Vec<Self::A>, Option<Self::M>);
}

/// A collection of bindings that can be added to a [ModalMachine].
pub trait InputBindings<Key: InputKey, S: Step<Key>> {
    /// Add new bindings to a [ModalMachine] instance.
    fn setup(&self, machine: &mut ModalMachine<Key, S>);
}

/// Trait for objects that can process input keys using previously mapped bindings.
pub trait BindingMachine<K, A, S, C>
where
    K: InputKey,
{
    /// Process a typed key.
    fn input_key(&mut self, input: K);

    /// Fetch the next action produced by previously typed keys.
    fn pop(&mut self) -> Option<(A, C)>;

    /// Get current output context after the most recent keypress.
    fn context(&mut self) -> C;

    /// Returns the message to display for the current interactive dialog, if there is one.
    ///
    /// Since interactive dialogs capture user input, displaying this should take priority over any
    /// other status line information in order to help the user understand what input is expected.
    fn show_dialog(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>>;

    /// Returns a user-friendly string to display for the current mode.
    fn show_mode(&self) -> Option<String>;

    /// Resets the current mode to the default.
    fn reset_mode(&mut self);

    /// Returns a character to show for the cursor.
    fn get_cursor_indicator(&self) -> Option<char>;

    /// Repeat a recent sequence of tracked actions, and optionally override their original
    /// contexts using [InputState::merge]. The repeated sequence will be inserted at
    /// the beginning of the action queue, before any other pending actions.
    ///
    /// See [ModeSequence::sequences] for how to control what is repeated here.
    fn repeat(&mut self, sequence: S, ctx: Option<C>);

    /// Start an interactive user dialog.
    fn run_dialog(&mut self, dialog: Box<dyn Dialog<A>>);
}

/// A default [InputKeyClass] with no members.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub enum EmptyKeyClass {}

impl<T> InputKeyClass<T> for EmptyKeyClass {
    fn memberships(_: &T) -> Vec<Self> {
        vec![]
    }
}

/// A default type for [Step::Sequence] with no members.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub enum EmptySequence {}

impl<M, A, C> ModeSequence<EmptySequence, A, C> for M
where
    M: Mode<A, C>,
    C: InputState,
{
}

/// An implementation of [InputKeyState] that stores nothing.
#[derive(Clone, Debug, Default, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub struct EmptyKeyState {}

impl InputState for EmptyKeyState {
    type Output = Self;

    fn merge(original: Self, _: &Self) -> Self {
        original
    }

    fn reset(&mut self) {}

    fn take(&mut self) -> Self {
        self.clone()
    }
}

impl<Key: InputKey, Class: InputKeyClass<Key>> InputKeyState<Key, Class> for EmptyKeyState {}

impl<Key, A, M> Step<Key> for (Option<A>, Option<M>)
where
    Key: InputKey,
    A: Clone + Default,
    M: ModeKeys<Key, A, EmptyKeyState>,
{
    type A = A;
    type State = EmptyKeyState;
    type Class = EmptyKeyClass;
    type M = M;
    type Sequence = EmptySequence;

    fn is_unmapped(&self) -> bool {
        self.0.is_none() && self.1.is_none()
    }

    fn step(&self, _: &mut EmptyKeyState) -> (Vec<A>, Option<M>) {
        let act = self.0.clone().into_iter().collect();

        (act, self.1)
    }
}

#[derive(Debug, Default)]
struct IdGenerator {
    next_id: u64,
}

impl IdGenerator {
    pub fn next(&mut self) -> NodeId {
        let id = self.next_id;

        if self.next_id == u64::MAX {
            panic!("no more node IDs available!");
        }

        self.next_id += 1;

        return NodeId(id);
    }
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
struct NodeId(u64);

enum FollowResult<'a, Key, S: Step<Key>> {
    Successor(&'a Edge<Key, S>),
    NoSuccessor,
    Fallthrough(NodeId),
}

enum InputResult<S> {
    /// Clear the input trail, and try the key again.
    ClearTrail,
    /// Clear the input trail, and move on to the next key.
    Consumed,
    /// Clear the input trail, and run a [Step].
    Step(Arc<S>),
    /// Clear the input trail, run a [Step], and try the key again.
    RetryAfter(Arc<S>),
    /// Need more input to do anything; add the key to the trail.
    NeedMore,
    /// The input key sequence is unmapped, so do default behaviour on the first key and retry
    /// the rest of the input trail.
    Unmapped,
}

enum NodeAction<S> {
    Empty,
    Root,
    Step(Arc<S>),
}

impl<S> NodeAction<S> {
    fn new(step: S) -> Self {
        NodeAction::Step(Arc::new(step))
    }
}

impl<S> From<Option<S>> for NodeAction<S> {
    fn from(step: Option<S>) -> Self {
        if let Some(step) = step {
            NodeAction::new(step)
        } else {
            NodeAction::Empty
        }
    }
}

struct Node<M, S> {
    mode: M,
    action: NodeAction<S>,
}

/// What kind of input is acceptible for continuing towards a [Step].
///
/// [ModalMachine] will prioritize competing applicable edges in the following order:
///
/// * A matching [EdgeEvent::Key] edge
/// * A matching [EdgeEvent::Class] edge
/// * A matching [EdgeEvent::Any] edge
/// * If nothing else matches, an [EdgeEvent::Fallthrough] edge
///
/// For example, if there is both a `Key('0')` edge, and a `Class(Number)` edge, then the first one
/// will be used. If '1' is input instead, and there's no `Key('1')` edge, then the `Class(Number)`
/// will be followed instead.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum EdgeEvent<Key, K: InputKeyClass<Key>> {
    /// Allow only a single, specific input key.
    Key(Key),
    /// Allow a class of possible keys.
    Class(K),
    /// Any key is accepted.
    Any,
    /// If no keys are accepted, continue along the [EdgePath] and try again.
    ///
    /// This can be used to create [Steps](Step) reached via unmapped keys. Internally, it used to
    /// support [Min](EdgeRepeat::Min) and [Max](EdgeRepeat::Max).
    Fallthrough,
}

/// Specifies how many times an [EdgeEvent] is allowed to be repeated.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EdgeRepeat {
    /// Allow [EdgeEvent] to occur once before continuing to the next part of the [EdgePath].
    Once,
    /// Require [EdgeEvent] to occur at least `usize` times before continuing to the next part of
    /// the [EdgePath].
    Min(usize),
    /// Don't allow [EdgeEvent] to happen more than `usize` times before continuing to the next
    /// part of the [EdgePath].
    Max(usize),
}

/// Part of a sequence of input keys that leads to a [Step].
pub type EdgePathPart<Key, Class> = (EdgeRepeat, EdgeEvent<Key, Class>);

/// A description of a sequence of input keys that leads to a [Step].
pub type EdgePath<Key, Class> = [EdgePathPart<Key, Class>];

#[derive(Clone, Debug)]
struct Edge<Key, S: Step<Key>> {
    evt: EdgeEvent<Key, S::Class>,
    end: NodeId,
}

struct Graph<Key: InputKey, S: Step<Key>> {
    idgen: IdGenerator,
    modes: HashMap<S::M, NodeId>,
    nodes: HashMap<NodeId, Node<S::M, S>>,
    edges: HashMap<NodeId, HashMap<EdgeEvent<Key, S::Class>, Edge<Key, S>>>,
}

impl<Key: InputKey, S: Step<Key>> Graph<Key, S> {
    fn add_node(&mut self, mode: S::M, action: NodeAction<S>) -> NodeId {
        let id = self.idgen.next();
        let node = Node { mode, action };

        self.nodes.insert(id, node);

        return id;
    }

    fn upsert_node(
        &mut self,
        mode: S::M,
        prev: NodeId,
        ev: &EdgeEvent<Key, S::Class>,
        action: NodeAction<S>,
    ) -> NodeId {
        if let Some(e) = self.get_edge(prev, ev) {
            let id = e.end;

            if matches!(action, NodeAction::Step(_)) {
                let node = self.nodes.get_mut(&id).unwrap();
                node.action = action;
                assert_eq!(node.mode, mode);
            }

            id
        } else {
            self.add_node(mode, action)
        }
    }

    fn add_edge(&mut self, from: NodeId, to: NodeId, ev: EdgeEvent<Key, S::Class>) {
        let e = Edge { evt: ev.clone(), end: to };

        if let Some(m) = self.edges.get_mut(&from) {
            m.insert(ev, e);
        } else {
            let mut m = HashMap::new();
            m.insert(ev, e);
            self.edges.insert(from, m);
        }
    }

    fn get_node(&self, id: NodeId) -> &Node<S::M, S> {
        self.nodes.get(&id).unwrap()
    }

    fn get_mode(&mut self, mode: S::M) -> NodeId {
        match self.modes.get(&mode) {
            None => {
                let id = self.add_node(mode, NodeAction::Root);
                self.modes.insert(mode, id);
                return id;
            },
            Some(id) => {
                return *id;
            },
        }
    }

    fn get_edge(&self, id: NodeId, ev: &EdgeEvent<Key, S::Class>) -> Option<&Edge<Key, S>> {
        if let Some(m) = self.edges.get(&id) {
            return m.get(ev);
        }

        return None;
    }

    fn has_edges(&self, id: NodeId) -> bool {
        self.edges.get(&id).map(|es| !es.is_empty()).unwrap_or(false)
    }

    fn follow_edge(&self, id: NodeId, ke: &Key) -> FollowResult<Key, S> {
        if let Some(m) = self.edges.get(&id) {
            if let Some(e) = m.get(&EdgeEvent::Key(ke.clone())) {
                return FollowResult::Successor(e);
            }

            for class in S::Class::memberships(ke).into_iter() {
                if let Some(e) = m.get(&EdgeEvent::Class(class)) {
                    return FollowResult::Successor(e);
                }
            }

            if let Some(e) = m.get(&EdgeEvent::Any) {
                return FollowResult::Successor(e);
            }

            if let Some(e) = m.get(&EdgeEvent::Fallthrough) {
                return FollowResult::Fallthrough(e.end);
            }
        }

        return FollowResult::NoSuccessor;
    }
}

impl<Key: InputKey, S: Step<Key>> Default for Graph<Key, S> {
    fn default() -> Self {
        Graph {
            idgen: IdGenerator::default(),
            modes: HashMap::new(),
            nodes: HashMap::new(),
            edges: HashMap::new(),
        }
    }
}

/// Iterate over the actions produced by [ModalMachine], feeding keys into it as needed.
pub struct InputIterator<'a, Key: InputKey, S: Step<Key>> {
    bindings: &'a mut ModalMachine<Key, S>,
    keys: std::vec::IntoIter<Key>,
}

impl<'a, K, S> Iterator for InputIterator<'a, K, S>
where
    K: InputKey,
    S: Step<K>,
{
    type Item = (S::A, <S::State as InputState>::Output);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let res = self.bindings.pop();

            if res.is_some() {
                return res;
            }

            if let Some(key) = self.keys.next() {
                self.bindings.input_key(key);
            } else {
                return None;
            }
        }
    }
}

struct InputMachine<Key: InputKey, S: Step<Key>> {
    graph: Graph<Key, S>,
    curr: NodeId,
}

impl<Key: InputKey, S: Step<Key>> InputMachine<Key, S> {
    fn add_simple_step(
        &mut self,
        mode: S::M,
        ev: &EdgeEvent<Key, S::Class>,
        step: Option<S>,
        prev: &mut NodeId,
        fallthrough: &mut bool,
    ) -> NodeId {
        let id = self.graph.upsert_node(mode, *prev, ev, step.into());

        if *fallthrough {
            self.graph.add_edge(*prev, id, EdgeEvent::Fallthrough);
            *fallthrough = false;
        }

        self.graph.add_edge(*prev, id, ev.clone());

        return id;
    }

    fn add_simple(
        &mut self,
        mode: S::M,
        ev: &EdgeEvent<Key, S::Class>,
        prev: &mut NodeId,
        fallthrough: &mut bool,
    ) -> NodeId {
        self.add_simple_step(mode, ev, None, prev, fallthrough)
    }

    fn add_intermediate(
        &mut self,
        mode: S::M,
        rep: &EdgeRepeat,
        ev: &EdgeEvent<Key, S::Class>,
        prev: &mut NodeId,
        fallthrough: &mut bool,
    ) {
        if let Some(e) = self.graph.get_edge(*prev, ev) {
            *prev = e.end;
            return;
        }

        match rep {
            EdgeRepeat::Once => {
                *prev = self.add_simple(mode, ev, prev, fallthrough);
            },
            EdgeRepeat::Min(n) => {
                let many = self.graph.add_node(mode, NodeAction::Empty);

                self.graph.add_edge(many, many, ev.clone());

                if *n == 0 {
                    self.graph.add_edge(*prev, many, EdgeEvent::Fallthrough);
                } else {
                    for _ in 1..*n {
                        *prev = self.add_simple(mode, ev, prev, fallthrough);
                    }

                    self.graph.add_edge(*prev, many, ev.clone());
                }

                *fallthrough = true;
                *prev = many;
            },
            EdgeRepeat::Max(n) => {
                if *n == 0 {
                    return;
                }

                let end = self.graph.add_node(mode, NodeAction::Empty);

                self.graph.add_edge(*prev, end, EdgeEvent::Fallthrough);

                for _ in 1..*n {
                    let id = self.add_simple(mode, ev, prev, fallthrough);

                    self.graph.add_edge(id, end, EdgeEvent::Fallthrough);

                    *prev = id;
                }

                self.graph.add_edge(*prev, end, ev.clone());

                *prev = end;
            },
        }
    }

    fn add_prefix(&mut self, mode: S::M, evs: &EdgePath<Key, S::Class>, action: &Option<S>) {
        let root = self.graph.get_mode(mode);
        let mut prev = root;
        let mut fallthrough = false;
        let prev = &mut prev;
        let fallthrough = &mut fallthrough;

        if let Some((last, prefix)) = evs.split_last() {
            let mut single = true;

            for (rep, ev) in prefix {
                self.add_intermediate(mode, rep, ev, prev, fallthrough);
                single = false;
            }

            match last {
                (EdgeRepeat::Once, ev) => {
                    if action.is_some() {
                        /*
                         * Because we end on the root node, we can't store the result InputStep there, so
                         * we need to create a node to hold it and then fall through to the root.
                         */
                        let id = self.graph.add_node(mode, action.clone().into());

                        self.graph.add_edge(*prev, id, ev.clone());
                        self.graph.add_edge(id, root, EdgeEvent::Fallthrough);
                    } else {
                        self.graph.add_edge(*prev, root, ev.clone());
                    }
                },
                (EdgeRepeat::Min(n), ev) => {
                    let n = if single { 1.max(*n) } else { *n };

                    let end = self.graph.add_node(mode, action.clone().into());
                    let many = self.graph.add_node(mode, NodeAction::Empty);

                    self.graph.add_edge(many, many, ev.clone());
                    self.graph.add_edge(many, end, EdgeEvent::Fallthrough);
                    self.graph.add_edge(end, root, EdgeEvent::Fallthrough);

                    if n == 0 {
                        self.graph.add_edge(*prev, end, EdgeEvent::Fallthrough);
                    } else {
                        for _ in 1..n {
                            *prev = self.add_simple(mode, ev, prev, fallthrough);
                        }
                    }

                    self.graph.add_edge(*prev, many, ev.clone());
                },
                (EdgeRepeat::Max(n), ev) => {
                    if single && *n == 0 {
                        return;
                    }

                    let end = self.graph.add_node(mode, action.clone().into());

                    self.graph.add_edge(*prev, end, EdgeEvent::Fallthrough);
                    self.graph.add_edge(end, root, EdgeEvent::Fallthrough);

                    if *n != 0 {
                        for _ in 1..*n {
                            let id = self.add_simple(mode, ev, prev, fallthrough);

                            self.graph.add_edge(id, end, EdgeEvent::Fallthrough);

                            *prev = id;
                        }

                        self.graph.add_edge(*prev, end, ev.clone());
                    }
                },
            }
        }
    }

    fn add_mapping(&mut self, mode: S::M, evs: &EdgePath<Key, S::Class>, step: &S) {
        let mut prev = self.graph.get_mode(mode);
        let mut fallthrough = false;
        let prev = &mut prev;
        let fallthrough = &mut fallthrough;

        if let Some((last, prefix)) = evs.split_last() {
            for (rep, ev) in prefix {
                self.add_intermediate(mode, rep, ev, prev, fallthrough);
            }

            match last {
                (EdgeRepeat::Once, ev) => {
                    let step = Some(step.clone());
                    *prev = self.add_simple_step(mode, ev, step, prev, fallthrough);
                },
                (EdgeRepeat::Min(n), ev) => {
                    let end = self.graph.add_node(mode, NodeAction::new(step.clone()));
                    let many = self.graph.add_node(mode, NodeAction::Empty);

                    self.graph.add_edge(many, end, EdgeEvent::Fallthrough);
                    self.graph.add_edge(many, many, ev.clone());

                    if *n == 0 {
                        self.graph.add_edge(*prev, end, EdgeEvent::Fallthrough);
                    } else {
                        for _ in 1..*n {
                            *prev = self.add_simple(mode, ev, prev, fallthrough);
                        }
                    }

                    self.graph.add_edge(*prev, many, ev.clone());

                    *prev = end;
                },
                (EdgeRepeat::Max(n), ev) => {
                    let end = self.graph.add_node(mode, NodeAction::new(step.clone()));

                    self.graph.add_edge(*prev, end, EdgeEvent::Fallthrough);

                    if *n != 0 {
                        for _ in 1..*n {
                            let id = self.add_simple(mode, ev, prev, fallthrough);

                            self.graph.add_edge(id, end, EdgeEvent::Fallthrough);

                            *prev = id;
                        }

                        self.graph.add_edge(*prev, end, ev.clone());
                    }

                    *prev = end;
                },
            }

            if let Some(mode) = step.fallthrough() {
                let mid = self.graph.get_mode(mode);

                self.graph.add_edge(*prev, mid, EdgeEvent::Fallthrough);
            }
        }
    }

    fn input(&mut self, ke: &Key, ctx: &mut S::State) -> InputResult<S> {
        loop {
            match self.graph.follow_edge(self.curr, ke) {
                FollowResult::Successor(e) => {
                    ctx.event(&e.evt, ke);
                    self.curr = e.end;

                    let node = self.graph.get_node(self.curr);

                    match &node.action {
                        NodeAction::Step(is) => {
                            if is.is_unmapped() {
                                if self.graph.has_edges(self.curr) {
                                    return InputResult::NeedMore;
                                } else {
                                    return InputResult::Unmapped;
                                }
                            } else {
                                return InputResult::Step(is.clone());
                            }
                        },
                        NodeAction::Empty => {
                            // Still waiting on pending input.
                            return InputResult::NeedMore;
                        },
                        NodeAction::Root => {
                            return InputResult::Consumed;
                        },
                    }
                },
                FollowResult::NoSuccessor => {
                    return InputResult::Unmapped;
                },
                FollowResult::Fallthrough(id) => {
                    self.curr = id;

                    let node = self.graph.get_node(self.curr);

                    match &node.action {
                        NodeAction::Empty => continue,
                        NodeAction::Root => return InputResult::ClearTrail,
                        NodeAction::Step(s) => return InputResult::RetryAfter(s.clone()),
                    }
                },
            }
        }
    }

    fn goto_mode(&mut self, mode: S::M) {
        self.curr = self.graph.get_mode(mode);
    }

    fn mode(&self) -> S::M {
        self.graph.get_node(self.curr).mode
    }
}

impl<Key: InputKey, S: Step<Key>> Default for InputMachine<Key, S> {
    fn default() -> Self {
        let mut graph = Graph::<Key, S>::default();
        let curr = graph.get_mode(S::M::default());

        InputMachine { graph, curr }
    }
}

struct SequenceTracker<A, C> {
    sequence: Vec<(A, C)>,
    sequence_break: bool,
}

impl<A, C> SequenceTracker<A, C>
where
    A: Clone,
    C: Clone,
{
    fn fetch<F>(&mut self, overrides: F) -> Vec<(A, C)>
    where
        F: Fn(C) -> C,
    {
        self.sequence = std::mem::take(&mut self.sequence)
            .into_iter()
            .map(|(a, c)| (a, overrides(c)))
            .collect();

        return self.sequence.clone();
    }

    fn push(&mut self, status: SequenceStatus, pair: &(A, C)) {
        match status {
            SequenceStatus::Atom => {
                self.sequence = vec![pair.clone()];
                self.sequence_break = true;
            },
            SequenceStatus::Break => {
                self.sequence_break = true;
            },
            SequenceStatus::Ignore => {
                // Do nothing.
            },
            SequenceStatus::Restart => {
                self.sequence = vec![pair.clone()];
                self.sequence_break = false;
            },
            SequenceStatus::Track => {
                if self.sequence_break {
                    self.sequence = vec![pair.clone()];
                    self.sequence_break = false;
                } else {
                    self.sequence.push(pair.clone());
                }
            },
        }
    }
}

impl<A, C> Default for SequenceTracker<A, C> {
    fn default() -> Self {
        Self { sequence: vec![], sequence_break: false }
    }
}

/// Manage and process modal keybindings.
pub struct ModalMachine<Key: InputKey, S: Step<Key>> {
    state: S::M,
    ctx: S::State,
    im: InputMachine<Key, S>,
    actions: VecDeque<(S::A, <S::State as InputState>::Output)>,
    sequences: HashMap<S::Sequence, SequenceTracker<S::A, <S::State as InputState>::Output>>,
    dialogs: Vec<Box<dyn Dialog<S::A>>>,

    /// Previously en
    key_trail: Vec<Key>,
}

impl<Key, S> ModalMachine<Key, S>
where
    Key: InputKey,
    S: Step<Key>,
    S::State: Default,
{
    /// Return a new instance without any bindings.
    pub fn empty() -> Self {
        ModalMachine::from_state(S::State::default())
    }

    /// Return an instance that contains default bindings provided by `B`.
    pub fn from_bindings<B: InputBindings<Key, S> + Default>() -> Self {
        let mut machine = ModalMachine::empty();

        B::default().setup(&mut machine);

        machine
    }
}

impl<Key, S> ModalMachine<Key, S>
where
    Key: InputKey,
    S: Step<Key>,
{
    /// Return a new instance using an already initialized state.
    pub fn from_state(state: S::State) -> Self {
        Self {
            state: S::M::default(),
            ctx: state,
            im: InputMachine::default(),
            actions: VecDeque::new(),
            sequences: HashMap::new(),
            dialogs: Vec::new(),
            key_trail: Vec::new(),
        }
    }

    /// Prefix a mode with the key sequence described by [EdgePath].
    pub fn add_prefix(&mut self, mode: S::M, evs: &EdgePath<Key, S::Class>, action: &Option<S>) {
        self.im.add_prefix(mode, evs, action);
    }

    /// Map a sequence of keys to an action in the given mode.
    pub fn add_mapping(&mut self, mode: S::M, evs: &EdgePath<Key, S::Class>, action: &S) {
        self.im.add_mapping(mode, evs, action);
    }

    fn unmapped(&mut self, ke: Key) {
        let (mut acts, ms) = self.im.mode().unmapped(&ke, &mut self.ctx);
        let res = self.ctx.take();

        if acts.is_empty() {
            acts.push(S::A::default());
        }

        for act in acts.into_iter() {
            self.push((act, res.clone()));
        }

        self.goto_mode(ms.unwrap_or(self.state));
    }

    /// Process multiple input keys.
    pub fn execute(&mut self, input: Vec<Key>) -> InputIterator<'_, Key, S> {
        InputIterator { bindings: self, keys: input.into_iter() }
    }

    /// Interpret the given string as a macro, and process the keys it represents a given number of
    /// times.
    pub fn execute_macro(
        &mut self,
        mstr: &str,
        count: usize,
    ) -> Result<InputIterator<'_, Key, S>, Key::Error> {
        let mut keys = vec![];

        for _ in 0..count {
            let mut m = Key::from_macro_str(mstr)?;

            keys.append(&mut m);
        }

        return Ok(self.execute(keys));
    }

    fn goto_mode(&mut self, mode: S::M) {
        let prev = self.state;

        self.im.goto_mode(mode);
        self.state = mode;

        let acts = self.state.enter(prev, &mut self.ctx);
        let res = self.ctx.take();

        for act in acts.into_iter() {
            self.push((act, res.clone()));
        }
    }

    fn step(&mut self, step: &S) {
        let (acts, nexts) = step.step(&mut self.ctx);

        match (acts.len(), nexts) {
            (0, None) => {},
            (0, Some(mode)) => {
                let res = self.ctx.take();

                self.push((S::A::default(), res));
                self.goto_mode(mode);
            },
            (_, ms) => {
                let res = self.ctx.take();

                for act in acts.into_iter() {
                    self.push((act, res.clone()));
                }

                /*
                 * Completed actions implicitly return to the top of the current state if no next
                 * state is specified.
                 */
                self.goto_mode(ms.unwrap_or(self.state));
            },
        }
    }

    fn sequence(
        &mut self,
        seq: S::Sequence,
        status: SequenceStatus,
        pair: &(S::A, <S::State as InputState>::Output),
    ) {
        self.sequences.entry(seq).or_default().push(status, pair);
    }

    fn push(&mut self, pair: (S::A, <S::State as InputState>::Output)) {
        let seqs = self.state.sequences(&pair.0, &pair.1);

        for (seq, status) in seqs {
            self.sequence(seq, status, &pair);
        }

        self.actions.push_back(pair);
    }

    /// Returns the mode we've most recently entered.
    ///
    /// Modes reached via [Fallthrough](EdgeEvent::Fallthrough) will not change what this returns.
    pub fn mode(&self) -> S::M {
        self.state
    }

    /// Get a mutable reference to the keybinding state.
    pub fn state(&mut self) -> &mut S::State {
        &mut self.ctx
    }
}

impl<Key, S> BindingMachine<Key, S::A, S::Sequence, <S::State as InputState>::Output>
    for ModalMachine<Key, S>
where
    Key: InputKey,
    S: Step<Key>,
{
    fn input_key(&mut self, input: Key) {
        // Ongoing dialogs intercept all keypresses.
        if let Some(dialog) = self.dialogs.last_mut() {
            if let Some(c) = input.get_char() {
                if let Some(mut acts) = dialog.input(c) {
                    // Dialog-generated actions skip sequence tracking,
                    // and go to the front of the action queue.
                    while let Some(act) = acts.pop() {
                        self.actions.push_front((act, <S::State as InputState>::Output::default()));
                    }

                    let _ = self.dialogs.pop();
                }
            }

            return;
        }

        let mut stack = vec![input];

        while let Some(mut ke) = stack.pop() {
            loop {
                match self.im.input(&ke, &mut self.ctx) {
                    InputResult::Consumed => {
                        self.key_trail.clear();
                        break;
                    },
                    InputResult::NeedMore => {
                        self.key_trail.push(ke);
                        break;
                    },
                    InputResult::Unmapped => {
                        if let Some(mut ke2) = ke.decompose() {
                            std::mem::swap(&mut ke, &mut ke2);
                            stack.push(ke2);
                            continue;
                        }

                        // The path we took to get here was unmapped. We now
                        // need to do the unmapped behaviour for the first key,
                        // and try reprocessing the successors. For example,
                        // consider the two mappings, "jj" and "kk". When typing
                        // "jkk" the "j" will start taking us down the path
                        // towards the "jj" node, but the "k" won't go anywhere.
                        //
                        // We then need to do the unmapped behaviour for "j",
                        // and reprocess the "k" at the root node so that we can reach
                        // "kk".
                        let mut key_trail = std::mem::take(&mut self.key_trail);
                        key_trail.push(ke);
                        self.unmapped(key_trail.remove(0));
                        key_trail.into_iter().rev().for_each(|k| stack.push(k));
                        break;
                    },
                    InputResult::ClearTrail => {
                        self.key_trail.clear();
                        continue;
                    },
                    InputResult::RetryAfter(step) => {
                        self.key_trail.clear();
                        self.step(step.as_ref());
                        continue;
                    },
                    InputResult::Step(step) => {
                        self.key_trail.clear();
                        self.step(step.as_ref());
                        break;
                    },
                }
            }
        }
    }

    fn pop(&mut self) -> Option<(S::A, <S::State as InputState>::Output)> {
        if !self.dialogs.is_empty() {
            // Wait until we've finished interacting w/ the dialog.
            return None;
        }

        self.actions.pop_front()
    }

    fn context(&mut self) -> <S::State as InputState>::Output {
        self.ctx.take()
    }

    fn show_dialog(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>> {
        match self.dialogs.last_mut() {
            Some(dialog) => dialog.render(max_rows, max_cols),
            None => Vec::new(),
        }
    }

    fn reset_mode(&mut self) {
        self.goto_mode(S::M::default());
    }

    fn show_mode(&self) -> Option<String> {
        self.state.show(&self.ctx)
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        self.ctx.get_cursor_indicator()
    }

    fn repeat(&mut self, seq: S::Sequence, ctx: Option<<S::State as InputState>::Output>) {
        let merge = |c| {
            match &ctx {
                Some(ctx) => S::State::merge(c, ctx),
                None => c,
            }
        };

        let tracker = self.sequences.entry(seq).or_default();
        let mut seq = VecDeque::from(tracker.fetch(merge));

        std::mem::swap(&mut self.actions, &mut seq);
        self.actions.append(&mut seq);
    }

    fn run_dialog(&mut self, dialog: Box<dyn Dialog<S::A>>) {
        self.dialogs.push(dialog);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    enum TestKey {
        Left,
        Right,
        Up,
        Down,
        Esc,
        Char(char),
        Alt(char),
        Ctl(char),
    }

    impl From<char> for TestKey {
        fn from(c: char) -> Self {
            TestKey::Char(c)
        }
    }

    impl InputKey for TestKey {
        type Error = std::convert::Infallible;

        fn decompose(&mut self) -> Option<Self> {
            match self {
                TestKey::Alt(c) => {
                    *self = TestKey::Char(*c);
                    Some(TestKey::Esc)
                },
                _ => None,
            }
        }

        fn from_macro_str(mstr: &str) -> Result<Vec<Self>, Self::Error> {
            Ok(mstr.chars().map(TestKey::from).collect())
        }

        fn get_char(&self) -> Option<char> {
            if let TestKey::Char(c) = self {
                Some(*c)
            } else {
                None
            }
        }
    }

    macro_rules! alt {
        ($c: expr) => {
            TestKey::Alt($c)
        };
    }

    macro_rules! ctl {
        ($c: expr) => {
            TestKey::Ctl($c)
        };
    }

    macro_rules! key {
        ($k: expr) => {
            TestKey::from($k)
        };
    }

    macro_rules! once {
        ($ee: expr) => {
            (EdgeRepeat::Once, $ee)
        };
    }

    macro_rules! keys {
        ($( $k: expr ),*) => {
            [ $( once!(EdgeEvent::Key(key!($k).into())), )* ]
        };
    }

    macro_rules! assert_pop1 {
        ($mm: expr, $act: expr, $ctx: expr) => {
            assert_eq!($mm.pop(), Some(($act.clone(), $ctx.clone())));
        };
    }

    macro_rules! assert_pop2 {
        ($mm: expr, $act: expr, $ctx: expr) => {
            assert_pop1!($mm, $act, $ctx);
            assert_eq!($mm.pop(), None);
        };
    }

    macro_rules! action {
        ($act: expr) => {
            TestStep {
                run: None,
                action: Some($act),
                goto_mode: None,
                fall_mode: None,
            }
        };
    }

    macro_rules! goto {
        ($mode: expr) => {
            TestStep {
                run: None,
                action: None,
                goto_mode: Some($mode),
                fall_mode: None,
            }
        };
    }

    macro_rules! fall {
        ($mode: expr) => {
            TestStep {
                run: None,
                action: None,
                goto_mode: None,
                fall_mode: Some($mode),
            }
        };
    }

    macro_rules! op {
        ($op: expr) => {
            TestStep {
                run: Some($op),
                action: None,
                goto_mode: None,
                fall_mode: None,
            }
        };
        ($op: expr, $mode: expr) => {
            TestStep {
                run: Some($op),
                action: None,
                goto_mode: None,
                fall_mode: Some($mode),
            }
        };
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum TestOperation {
        Decimate,
        Delete,
        Yank,
    }

    #[derive(Clone, Debug, Default, Eq, PartialEq)]
    enum TestAction {
        EditLine,
        EditTillChar,
        EditWord,
        Inveigle,
        MoveDown,
        MoveLeft,
        MoveRight,
        MoveUp,
        #[default]
        NoOp,
        Palaver,
        Paste,
        Query,
        Type(char),
    }

    #[derive(Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
    enum TestMode {
        #[default]
        Insert,
        Normal,
        Suffix,
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    enum TestKeyClass {
        Register,
        TillChar,
        Count,
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    enum TestSequence {
        Edit,
    }

    #[derive(Clone, Debug, Default, Eq, PartialEq)]
    struct TestTempContext {
        count: Option<usize>,
        cursor: Option<char>,
        operation: Option<TestOperation>,
        register: Option<char>,
    }

    #[derive(Clone, Debug, Default, Eq, PartialEq)]
    struct TestKeepContext {
        tillchar: Option<char>,
    }

    #[derive(Clone, Debug, Default, Eq, PartialEq)]
    struct TestContext {
        temp: TestTempContext,
        keep: TestKeepContext,
    }

    #[derive(Clone)]
    struct TestStep {
        run: Option<for<'a> fn(&'a mut TestContext)>,
        action: Option<TestAction>,
        goto_mode: Option<TestMode>,
        fall_mode: Option<TestMode>,
    }

    #[derive(Default)]
    struct TestBindings {}

    type TestEdgeEvent = EdgeEvent<TestKey, TestKeyClass>;

    impl InputKeyClass<TestKey> for TestKeyClass {
        fn memberships(ke: &TestKey) -> Vec<Self> {
            let mut kcs = vec![];

            if let Some(c) = ke.get_char() {
                if let '0'..='9' = c {
                    kcs.push(TestKeyClass::Count);
                }

                if let 'a'..='z' = c {
                    kcs.push(TestKeyClass::Register);
                }

                kcs.push(TestKeyClass::TillChar);
            }

            return kcs;
        }
    }

    impl Mode<TestAction, TestContext> for TestMode {
        fn show(&self, _: &TestContext) -> Option<String> {
            match self {
                TestMode::Insert => Some("-- insert --".to_string()),
                TestMode::Normal => Some("-- normal --".to_string()),
                TestMode::Suffix => None,
            }
        }
    }

    impl ModeSequence<TestSequence, TestAction, TestContext> for TestMode {
        fn sequences(
            &self,
            action: &TestAction,
            ctx: &TestContext,
        ) -> Vec<(TestSequence, SequenceStatus)> {
            let status = match action {
                // Don't let a NoOp impact current sequence.
                TestAction::NoOp => SequenceStatus::Ignore,

                // Movement and typing break current sequence.
                TestAction::MoveDown => SequenceStatus::Break,
                TestAction::MoveLeft => SequenceStatus::Break,
                TestAction::MoveRight => SequenceStatus::Break,
                TestAction::MoveUp => SequenceStatus::Break,
                TestAction::Type(_) => SequenceStatus::Break,

                // These actions are always on their own.
                TestAction::Paste => SequenceStatus::Atom,
                TestAction::Query => SequenceStatus::Atom,

                // These actions are always at the start
                TestAction::Inveigle => SequenceStatus::Restart,
                TestAction::Palaver => SequenceStatus::Restart,

                // Sequences of Decimate and Delete are tracked and can be repeated.
                TestAction::EditLine | TestAction::EditTillChar | TestAction::EditWord => {
                    match ctx.temp.operation {
                        Some(TestOperation::Decimate) => SequenceStatus::Track,
                        Some(TestOperation::Delete) => SequenceStatus::Track,
                        Some(TestOperation::Yank) => SequenceStatus::Break,
                        None => SequenceStatus::Break,
                    }
                },
            };

            vec![(TestSequence::Edit, status)]
        }
    }

    impl ModeKeys<TestKey, TestAction, TestContext> for TestMode {
        fn unmapped(&self, ke: &TestKey, _: &mut TestContext) -> (Vec<TestAction>, Option<Self>) {
            match self {
                TestMode::Insert => {
                    if let Some(c) = ke.get_char() {
                        (vec![TestAction::Type(c)], None)
                    } else {
                        (vec![], None)
                    }
                },
                TestMode::Normal => (vec![], None),
                TestMode::Suffix => (vec![], None),
            }
        }
    }

    impl InputState for TestContext {
        type Output = Self;

        fn merge(mut original: Self, other: &Self) -> Self {
            if other.temp.count.is_some() {
                original.temp.count = other.temp.count;
            }

            if other.temp.operation.is_some() {
                original.temp.operation = other.temp.operation.clone();
            }

            original
        }

        fn reset(&mut self) {
            self.temp = TestTempContext::default();
        }

        fn take(&mut self) -> Self {
            TestContext {
                temp: std::mem::take(&mut self.temp),
                keep: self.keep.clone(),
            }
        }
    }

    impl InputKeyState<TestKey, TestKeyClass> for TestContext {
        fn event(&mut self, ev: &TestEdgeEvent, ke: &TestKey) {
            match ev {
                EdgeEvent::Any => {
                    // Do nothing.
                },
                EdgeEvent::Key(_) => {
                    // Do nothing.
                },
                EdgeEvent::Class(TestKeyClass::Register) => {
                    if let Some(c) = ke.get_char() {
                        self.temp.register = Some(c);
                    }
                },
                EdgeEvent::Class(TestKeyClass::Count) => {
                    if let Some(n) = ke.get_char().and_then(|c| c.to_digit(10)) {
                        let count = self.temp.count.unwrap_or(0);
                        let count = count.saturating_mul(10);
                        let count = count.saturating_add(n as usize);

                        self.temp.count = Some(count);
                    }
                },
                EdgeEvent::Class(TestKeyClass::TillChar) => {
                    if let Some(c) = ke.get_char() {
                        self.keep.tillchar = Some(c);
                    }
                },
                EdgeEvent::Fallthrough => {
                    // Do nothing.
                },
            }
        }

        fn get_cursor_indicator(&self) -> Option<char> {
            self.temp.cursor
        }
    }

    impl Step<TestKey> for TestStep {
        type A = TestAction;
        type State = TestContext;
        type Class = TestKeyClass;
        type M = TestMode;
        type Sequence = TestSequence;

        fn is_unmapped(&self) -> bool {
            match self {
                TestStep {
                    run: None,
                    action: None,
                    fall_mode: None,
                    goto_mode: None,
                } => true,
                _ => false,
            }
        }

        fn fallthrough(&self) -> Option<Self::M> {
            self.fall_mode
        }

        fn step(&self, ctx: &mut Self::State) -> (Vec<Self::A>, Option<Self::M>) {
            let actions: Vec<Self::A> = self.action.clone().into_iter().collect();

            if let Some(f) = &self.run {
                f(ctx);
            }

            (actions, self.goto_mode)
        }
    }

    impl InputBindings<TestKey, TestStep> for TestBindings {
        fn setup(&self, machine: &mut ModalMachine<TestKey, TestStep>) {
            // Insert mode mappings
            machine.add_mapping(
                TestMode::Insert,
                &keys!(TestKey::Left),
                &action!(TestAction::MoveLeft),
            );
            machine.add_mapping(
                TestMode::Insert,
                &keys!(TestKey::Right),
                &action!(TestAction::MoveRight),
            );
            machine.add_mapping(
                TestMode::Insert,
                &keys!(TestKey::Up),
                &action!(TestAction::MoveUp),
            );
            machine.add_mapping(
                TestMode::Insert,
                &keys!(TestKey::Down),
                &action!(TestAction::MoveDown),
            );
            machine.add_mapping(
                TestMode::Insert,
                &[once!(EdgeEvent::Key(ctl!('l')))],
                &goto!(TestMode::Normal),
            );
            machine.add_mapping(
                TestMode::Insert,
                &[once!(EdgeEvent::Key(ctl!('o')))],
                &fall!(TestMode::Normal),
            );
            machine.add_mapping(
                TestMode::Insert,
                &[once!(EdgeEvent::Key(ctl!('r')))],
                &op!(move |ctx| ctx.temp.cursor = Some('^')),
            );
            machine.add_mapping(
                TestMode::Insert,
                &[
                    once!(EdgeEvent::Key(ctl!('r'))),
                    once!(EdgeEvent::Class(TestKeyClass::Register)),
                ],
                &action!(TestAction::Paste),
            );

            // Normal mode mappings
            machine.add_mapping(TestMode::Normal, &keys!('n'), &action!(TestAction::NoOp));
            machine.add_mapping(
                TestMode::Normal,
                &keys!('d'),
                &op!(move |ctx| ctx.temp.operation = Some(TestOperation::Delete), TestMode::Suffix),
            );
            machine.add_mapping(
                TestMode::Normal,
                &keys!('y'),
                &op!(move |ctx| ctx.temp.operation = Some(TestOperation::Yank), TestMode::Suffix),
            );
            machine.add_mapping(TestMode::Normal, &keys!('d', 'd'), &action!(TestAction::EditLine));
            machine.add_mapping(TestMode::Normal, &keys!('y', 'y'), &action!(TestAction::EditLine));
            machine.add_mapping(TestMode::Normal, &keys!(TestKey::Esc), &goto!(TestMode::Insert));

            // Normal mode prefixes
            machine.add_prefix(
                TestMode::Normal,
                &[(EdgeRepeat::Min(1), EdgeEvent::Class(TestKeyClass::Count))],
                &None,
            );

            machine.add_prefix(TestMode::Normal, &keys!('m'), &Some(op!(|_| ())));

            // Suffix mode mappings
            machine.add_mapping(TestMode::Suffix, &keys!('w'), &action!(TestAction::EditWord));
            machine.add_mapping(
                TestMode::Suffix,
                &[
                    once!(EdgeEvent::Key(key!('t'))),
                    once!(EdgeEvent::Class(TestKeyClass::TillChar)),
                ],
                &action!(TestAction::EditTillChar),
            );
            machine.add_mapping(TestMode::Suffix, &keys!(';'), &action!(TestAction::EditTillChar));
        }
    }

    /*
     * The default bindings for the TestMachine work a lot like a simplified version of Vim when
     * 'insertmode' is set.
     *
     * Insert mode supports:
     *     - Navigation via arrow keys
     *     - Typing text
     *     - Enter Normal mode via ^O for a single command
     *     - Enter Normal mode via ^L for multiple commands
     *     - Pasting via ^R{register}
     *
     * Normal mode supports:
     *     - Entering a count prefix, which has no step
     *     - Entering a no-op "m" prefix, which has a step
     *     - Deletion via "d" followed by a suffix
     *     - Yanking via "y" followed by a suffix
     *     - Delete lines with "dd"
     *     - Yank lines with "yy"
     *     - No-op with "n"
     *     - Escape goes to Insert mode
     *
     * Suffix mode supports:
     *     - Edit from cursor to next occurrence of character with "t" + character
     *     - Edit from cursor to word beginning with "w"
     *
     * The following should be unmapped in the default bindings for tests:
     *     - "a", "b", "c" and "e" in Normal mode.
     *     - "?", "$" and "!" in all modes.
     *     - "d" and "y" in Suffix mode.
     */
    type TestMachine = ModalMachine<TestKey, TestStep>;

    impl Default for TestMachine {
        fn default() -> Self {
            ModalMachine::from_bindings::<TestBindings>()
        }
    }

    #[test]
    fn test_input_mapped() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);

        // Left arrow key moves left.
        tm.input_key(key!(TestKey::Left));
        assert_pop2!(tm, TestAction::MoveLeft, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Typing ^O allows entering a single Normal mode command, "dd", deleting a line.
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(ctl!('o'));
        tm.input_key(key!('d'));
        tm.input_key(key!('d'));
        assert_pop2!(tm, TestAction::EditLine, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Down arrow key moves down.
        ctx.temp.operation = None;
        tm.input_key(key!(TestKey::Down));
        assert_pop2!(tm, TestAction::MoveDown, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Typing ^L goes to Normal mode, allowing multiple commands to be typed.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Pressing "yy" yanks a line.
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('y'));
        assert_pop2!(tm, TestAction::EditLine, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Pressing Escape goes back to Insert mode.
        ctx.temp.operation = None;
        tm.input_key(key!(TestKey::Esc));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Right arrow key moves right.
        tm.input_key(key!(TestKey::Right));
        assert_pop2!(tm, TestAction::MoveRight, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);
    }

    #[test]
    fn test_input_prefix() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Delete 257 words ("257dw")
        ctx.temp.count = Some(257);
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('2'));
        tm.input_key(key!('5'));
        tm.input_key(key!('7'));
        tm.input_key(key!('d'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Count is reset for the next action.
        ctx.temp.count = None;
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Yank 1,000,000 lines ("1000000yy")
        ctx.temp.count = Some(1000000);
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('1'));
        tm.input_key(key!('0'));
        tm.input_key(key!('0'));
        tm.input_key(key!('0'));
        tm.input_key(key!('0'));
        tm.input_key(key!('0'));
        tm.input_key(key!('0'));
        tm.input_key(key!('y'));
        tm.input_key(key!('y'));
        assert_pop2!(tm, TestAction::EditLine, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_input_unmapped() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);

        // Unmapped key in Insert mode types.
        tm.input_key(key!('d'));
        assert_pop2!(tm, TestAction::Type('d'), ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Unmapped key in Normal mode via ^O does nothing, and resets to Insert mode.
        tm.input_key(ctl!('o'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Unmapped key when falling through to Suffix mode via ^Od does nothing, and resets to
        // Insert mode.
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(ctl!('o'));
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Unmapped key in Normal mode via ^L does nothing, and resets to Normal mode.
        ctx.temp.operation = None;
        tm.input_key(ctl!('l'));
        tm.input_key(key!('?'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Unmapped key in Normal mode with count prefix.
        ctx.temp.operation = None;
        ctx.temp.count = Some(123);
        tm.input_key(key!('1'));
        tm.input_key(key!('2'));
        tm.input_key(key!('3'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Unmapped key in Normal mode with "m" prefix.
        ctx.temp.operation = None;
        ctx.temp.count = None;
        tm.input_key(key!('m'));
        tm.input_key(key!('m'));
        tm.input_key(key!('m'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Back to Insert mode.
        tm.input_key(key!(TestKey::Esc));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Unmapped key when falling through to Suffix mode via ^Ld does nothing, and resets to
        // Normal mode.
        tm.input_key(ctl!('l'));
        assert_pop1!(tm, TestAction::NoOp, ctx);

        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Back to Insert mode.
        ctx.temp.operation = None;
        tm.input_key(key!(TestKey::Esc));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Add an explicit, unmapped step for "?" to Normal mode.
        tm.add_mapping(
            TestMode::Normal,
            &keys!('?'),
            &TestStep {
                run: None,
                action: None,
                fall_mode: None,
                goto_mode: None,
            },
        );

        // Access the explicitly unmapped "?" via ^O?
        tm.input_key(ctl!('o'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);
    }

    #[test]
    fn test_add_mapping() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);

        // Unmapped "?" in Insert mode types.
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::Type('?'), ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Add "?" to Insert mode.
        tm.add_mapping(TestMode::Insert, &keys!('?'), &action!(TestAction::Query));

        // "?" now produces mapped action.
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::Query, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Unmapped "?" does nothing.
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Add "?" to Normal mode.
        tm.add_mapping(TestMode::Normal, &keys!('?'), &action!(TestAction::Query));

        // "?" now produces mapped action.
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::Query, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Pressing "y?" does nothing.
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Pressing "d?" also does nothing.
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Add "?" to Suffix mode.
        tm.add_mapping(TestMode::Suffix, &keys!('?'), &action!(TestAction::Query));

        // "y?" now produces mapped action.
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::Query, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // "d?" can also now produce the mapped action.
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::Query, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // We can map "y?" in Normal mode to prevent using the Suffix mode action.
        tm.add_mapping(TestMode::Normal, &keys!('y', '?'), &action!(TestAction::Inveigle));

        // "y?" now produces the override action.
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // The Suffix mode "?" is still accesible via "d".
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        assert_pop2!(tm, TestAction::Query, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // "yw" still works, and is unimpacted by any of our mappings thus far.
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_add_prefix() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Add the prefix "??" to Suffix mode.
        tm.add_prefix(
            TestMode::Suffix,
            &keys!('?', '?'),
            &Some(op!(move |ctx| {
                let count = ctx.temp.count.unwrap_or(0);
                ctx.temp.count = Some(count.saturating_add(10));
                ctx.temp.operation = Some(TestOperation::Decimate);
            })),
        );

        // Add the prefix "!" to Suffix mode.
        tm.add_prefix(
            TestMode::Suffix,
            &keys!('!'),
            &Some(op!(move |ctx| {
                let count = ctx.temp.count.unwrap_or(0);
                ctx.temp.count = Some(count.saturating_sub(1));
            })),
        );

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Test using "??"
        ctx.temp.operation = Some(TestOperation::Decimate);
        ctx.temp.count = Some(10);
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // "?" is not a valid prefix, so we do unmapped behaviour for '?' and 'w'.
        ctx.temp.count = None;
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('w'));
        ctx.temp.operation = Some(TestOperation::Delete);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        ctx.temp.operation = None;
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Test using "??" multiple times.
        ctx.temp.operation = Some(TestOperation::Decimate);
        ctx.temp.count = Some(30);
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // An odd number of "?" characters should do unmapped behaviour for the final '?' and 'w'.
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('w'));
        ctx.temp.operation = Some(TestOperation::Decimate);
        ctx.temp.count = Some(10);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        ctx.temp.operation = None;
        ctx.temp.count = None;
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Test using "!"
        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.temp.count = Some(14);
        tm.input_key(key!('1'));
        tm.input_key(key!('5'));
        tm.input_key(key!('d'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Test using "!" multiple times.
        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.temp.count = Some(20);
        tm.input_key(key!('2'));
        tm.input_key(key!('6'));
        tm.input_key(key!('d'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Test interleaving "??" and "!"
        ctx.temp.operation = Some(TestOperation::Decimate);
        ctx.temp.count = Some(27);
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('!'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_update_mapping() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Update the existing "yy" mapping.
        tm.add_mapping(TestMode::Normal, &keys!('y', 'y'), &action!(TestAction::Palaver));

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing "yy" produces overriding action.
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('y'));
        assert_pop2!(tm, TestAction::Palaver, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // We can still access Suffix mode.
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // "y" has not been mapped in Suffix mode.
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('y'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_context_persist() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Using "d;" has no tillchar set.
        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.keep.tillchar = None;
        tm.input_key(key!('d'));
        tm.input_key(key!(';'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Using "dtZ" sets tillchar.
        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.keep.tillchar = Some('Z');
        tm.input_key(key!('d'));
        tm.input_key(key!('t'));
        tm.input_key(key!('Z'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Using "d;" now has a tillchar set.
        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.keep.tillchar = Some('Z');
        tm.input_key(key!('d'));
        tm.input_key(key!(';'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Using "y;" now has a tillchar set.
        ctx.temp.operation = Some(TestOperation::Yank);
        ctx.keep.tillchar = Some('Z');
        tm.input_key(key!('y'));
        tm.input_key(key!(';'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Using "yt7" changes the tillchar.
        ctx.temp.operation = Some(TestOperation::Yank);
        ctx.keep.tillchar = Some('7');
        tm.input_key(key!('y'));
        tm.input_key(key!('t'));
        tm.input_key(key!('7'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Using "d;" has the updated tillchar.
        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.keep.tillchar = Some('7');
        tm.input_key(key!('d'));
        tm.input_key(key!(';'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Using "y;" has the updated tillchar.
        ctx.temp.operation = Some(TestOperation::Yank);
        ctx.keep.tillchar = Some('7');
        tm.input_key(key!('y'));
        tm.input_key(key!(';'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_repeat_sequence() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);

        // Feed a sequence.
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('d'));
        assert_pop2!(tm, TestAction::EditLine, ctx);

        ctx.temp.operation = None;
        tm.input_key(key!('n'));
        assert_pop2!(tm, TestAction::NoOp, ctx);

        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.temp.count = Some(3);
        tm.input_key(key!('3'));
        tm.input_key(key!('d'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);

        ctx.temp.operation = Some(TestOperation::Yank);
        ctx.temp.count = None;
        ctx.keep.tillchar = Some('a');
        tm.input_key(key!('y'));
        tm.input_key(key!('t'));
        tm.input_key(key!('a'));
        assert_pop2!(tm, TestAction::EditTillChar, ctx);

        // Now we can repeat the deletions, but not the yank.
        ctx.keep.tillchar = None;
        tm.repeat(TestSequence::Edit, None);

        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.temp.count = None;
        assert_pop1!(tm, TestAction::EditLine, ctx);

        ctx.temp.operation = Some(TestOperation::Delete);
        ctx.temp.count = Some(3);
        assert_pop2!(tm, TestAction::EditWord, ctx);

        // We can override the context, and change the operation.
        ctx.temp.operation = Some(TestOperation::Decimate);
        ctx.temp.count = None;
        tm.repeat(TestSequence::Edit, Some(ctx.clone()));

        ctx.temp.operation = Some(TestOperation::Decimate);
        ctx.temp.count = None;
        assert_pop1!(tm, TestAction::EditLine, ctx);

        ctx.temp.operation = Some(TestOperation::Decimate);
        ctx.temp.count = Some(3);
        assert_pop2!(tm, TestAction::EditWord, ctx);

        // Move back to Insert mode.
        ctx.keep.tillchar = Some('a');
        ctx.temp.operation = None;
        ctx.temp.count = None;
        tm.input_key(key!(TestKey::Esc));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);

        // Paste twice.
        ctx.temp.register = Some('b');
        ctx.temp.cursor = Some('^');
        tm.input_key(ctl!('r'));
        tm.input_key(key!('b'));
        assert_pop2!(tm, TestAction::Paste, ctx);

        tm.input_key(ctl!('r'));
        tm.input_key(key!('b'));
        assert_pop2!(tm, TestAction::Paste, ctx);

        // Repeat only pastes once.
        tm.repeat(TestSequence::Edit, None);
        assert_pop2!(tm, TestAction::Paste, ctx);
    }

    #[test]
    fn test_show_mode() {
        let mut tm = TestMachine::default();

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);
        assert_eq!(tm.show_mode().unwrap(), "-- insert --");

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_eq!(tm.mode(), TestMode::Normal);
        assert_eq!(tm.show_mode().unwrap(), "-- normal --");

        // XXX: it would be nice to support showing fallthrough modes
    }

    #[test]
    fn test_reset_mode() {
        let mut tm = TestMachine::default();

        // Start out in Insert mode.
        assert_eq!(tm.mode(), TestMode::Insert);

        tm.reset_mode();

        assert_eq!(tm.mode(), TestMode::default());

        // Go to Normal mode.
        tm.input_key(ctl!('l'));
        assert_eq!(tm.mode(), TestMode::Normal);

        tm.reset_mode();

        assert_eq!(tm.mode(), TestMode::default());
    }

    #[test]
    fn test_add_prefix_min() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Enter normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Prefix Min(3)
        tm.add_prefix(
            TestMode::Suffix,
            &[(EdgeRepeat::Min(3), EdgeEvent::Key(key!('!')))],
            &Some(op!(move |ctx| {
                let count = ctx.temp.count.unwrap_or(0);
                ctx.temp.count = Some(count.saturating_add(5));
            })),
        );

        // Typing once fails, and should do unmapped behaviour for '!' and 'w'.
        tm.input_key(key!('y'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        ctx.temp.operation = Some(TestOperation::Yank);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        ctx.temp.operation = None;
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing twice fails, and should do unmapped behaviour for '!', '!' and 'w'.
        tm.input_key(key!('y'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        ctx.temp.operation = Some(TestOperation::Yank);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        ctx.temp.operation = None;
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);

        // Typing thrice succeeds.
        ctx.temp.count = Some(5);
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);

        // Typing four times succeeds, only runs action once.
        ctx.temp.count = Some(5);
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);

        // Typing six times succeeds, only runs action once.
        ctx.temp.count = Some(5);
        ctx.temp.operation = Some(TestOperation::Yank);
        tm.input_key(key!('y'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
    }

    #[test]
    fn test_add_mapping_min() {
        let mut tm = TestMachine::default();
        let ctx = TestContext::default();

        // Enter normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Intermediate Min(0)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('a'))),
                (EdgeRepeat::Min(0), EdgeEvent::Key(key!('?'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('$'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Type it zero times.
        tm.input_key(key!('!'));
        tm.input_key(key!('a'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Type it once.
        tm.input_key(key!('!'));
        tm.input_key(key!('a'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Type it thrice.
        tm.input_key(key!('!'));
        tm.input_key(key!('a'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Intermediate Min(3)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('b'))),
                (EdgeRepeat::Min(3), EdgeEvent::Key(key!('?'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('$'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Typing it zero times fails, and does unmapped behaviour for all keys.
        tm.input_key(key!('!'));
        tm.input_key(key!('b'));
        tm.input_key(key!('$'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it once fails, and does unmapped behaviour for all keys.
        tm.input_key(key!('!'));
        tm.input_key(key!('b'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it thrice succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('b'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Final Min(0)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('c'))),
                (EdgeRepeat::Min(0), EdgeEvent::Key(key!('?'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Type it zero times.
        tm.input_key(key!('!'));
        tm.input_key(key!('c'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Type it once.
        tm.input_key(key!('!'));
        tm.input_key(key!('c'));
        tm.input_key(key!('?'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Type it thrice.
        tm.input_key(key!('!'));
        tm.input_key(key!('c'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Final Min(3)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('e'))),
                (EdgeRepeat::Min(3), EdgeEvent::Key(key!('?'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Typing it zero times fails, and does unmapped behaviour for all keys.
        tm.input_key(key!('!'));
        tm.input_key(key!('e'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it once fails, and does unmapped behaviour for all keys.
        tm.input_key(key!('!'));
        tm.input_key(key!('e'));
        tm.input_key(key!('?'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it thrice succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('e'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it thrice succeeds even when the ending key is unmapped.
        tm.input_key(key!('!'));
        tm.input_key(key!('e'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('a'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_add_prefix_max() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // Enter normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Add "!" to Normal mode so we can detect when we've fallen through.
        tm.add_mapping(
            TestMode::Normal,
            &[(EdgeRepeat::Once, EdgeEvent::Key(key!('!')))],
            &action!(TestAction::Inveigle),
        );

        // Prefix Max(2)
        tm.add_prefix(
            TestMode::Suffix,
            &[(EdgeRepeat::Max(2), EdgeEvent::Key(key!('!')))],
            &Some(op!(move |ctx| {
                let count = ctx.temp.count.unwrap_or(0);
                ctx.temp.count = Some(count.saturating_add(3));
            })),
        );

        // Typing zero times should not be triggered, since we have zero intermediates.
        ctx.temp.count = None;
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing once succeeds.
        ctx.temp.count = Some(3);
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing twice succeeds.
        ctx.temp.count = Some(3);
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing thrice succeeds, and step is processed twice.
        ctx.temp.count = Some(6);
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing six times means processing the step three times.
        ctx.temp.count = Some(9);
        ctx.temp.operation = Some(TestOperation::Delete);
        tm.input_key(key!('d'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('!'));
        tm.input_key(key!('w'));
        assert_pop2!(tm, TestAction::EditWord, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_add_mapping_max() {
        let mut tm = TestMachine::default();
        let ctx = TestContext::default();

        // Add "!" and "?" to Normal mode to detect when we've typed past Max.
        tm.add_mapping(TestMode::Normal, &keys!('$'), &action!(TestAction::Palaver));
        tm.add_mapping(TestMode::Normal, &keys!('?'), &action!(TestAction::Query));

        // Enter normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Intermediate Max(0)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('a'))),
                (EdgeRepeat::Max(0), EdgeEvent::Key(key!('?'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('$'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Typing it zero times succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('a'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it once fails, and all of the keys are reprocessed at root of Normal mode.
        tm.input_key(key!('!'));
        tm.input_key(key!('a'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::Query, ctx);
        assert_pop2!(tm, TestAction::Palaver, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Intermediate Max(2)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('b'))),
                (EdgeRepeat::Max(2), EdgeEvent::Key(key!('?'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('$'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Typing it zero times succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('b'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it once succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('b'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it twice succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('b'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop2!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Third "?" is treated as unmapped, so the keys are reprocessed at root of Normal mode.
        tm.input_key(key!('!'));
        tm.input_key(key!('b'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('$'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop1!(tm, TestAction::Query, ctx);
        assert_pop1!(tm, TestAction::Query, ctx);
        assert_pop1!(tm, TestAction::Query, ctx);
        assert_pop2!(tm, TestAction::Palaver, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Final Max(0)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('c'))),
                (EdgeRepeat::Max(0), EdgeEvent::Key(key!('?'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Typing it zero times succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('c'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it once succeeds, and the single "?" is processed in Normal mode.
        tm.input_key(key!('!'));
        tm.input_key(key!('c'));
        tm.input_key(key!('?'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::Query, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Final Max(3)
        tm.add_mapping(
            TestMode::Normal,
            &[
                (EdgeRepeat::Once, EdgeEvent::Key(key!('!'))),
                (EdgeRepeat::Once, EdgeEvent::Key(key!('d'))),
                (EdgeRepeat::Max(2), EdgeEvent::Key(key!('?'))),
            ],
            &action!(TestAction::Inveigle),
        );

        // Typing it zero times succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('d'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it once succeeds.
        tm.input_key(key!('!'));
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('n'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it once succeeds, even when the key that ends it early is unmapped.
        tm.input_key(key!('!'));
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('a'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it twice succeeds, no key needed to end early.
        tm.input_key(key!('!'));
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Typing it thrice succeeds, and third "?" is processed in Normal mode.
        tm.input_key(key!('!'));
        tm.input_key(key!('d'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        tm.input_key(key!('?'));
        assert_pop1!(tm, TestAction::Inveigle, ctx);
        assert_pop2!(tm, TestAction::Query, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);
    }

    #[test]
    fn test_decompose() {
        let mut tm = TestMachine::default();
        let ctx = TestContext::default();

        // Enter normal mode.
        tm.input_key(ctl!('l'));
        assert_pop2!(tm, TestAction::NoOp, ctx);
        assert_eq!(tm.mode(), TestMode::Normal);

        // Alt-b should be translated into Esc + "b"
        tm.input_key(alt!('b'));
        assert_pop1!(tm, TestAction::NoOp, ctx);
        assert_pop2!(tm, TestAction::Type('b'), ctx);
        assert_eq!(tm.mode(), TestMode::Insert);
    }

    #[test]
    fn test_cursor_indicator() {
        let mut tm = TestMachine::default();
        let mut ctx = TestContext::default();

        // No cursor indicator yet.
        assert_eq!(tm.mode(), TestMode::Insert);
        assert_eq!(tm.get_cursor_indicator(), None);

        // Set cursor indicator while we wait for the register.
        tm.input_key(ctl!('r'));
        assert_eq!(tm.pop(), None);
        assert_eq!(tm.mode(), TestMode::Insert);
        assert_eq!(tm.get_cursor_indicator(), Some('^'));

        // Set cursor indicator while we wait for the register.
        ctx.temp.register = Some('a');
        ctx.temp.cursor = Some('^');
        tm.input_key(key!('a'));
        assert_pop2!(tm, TestAction::Paste, ctx);
        assert_eq!(tm.mode(), TestMode::Insert);
        assert_eq!(tm.get_cursor_indicator(), None);
    }
}
