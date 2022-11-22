//! # Macro recording and execution
//!
//! ## Overview
//!
//! This module provides a wrapper for [BindingMachine] implementors. The wrapper can then process
//! [MacroAction] values, and handle recording and executing macros.
//!
//! Every [BindingMachine::input_key] call is considered to be a key actually typed at the
//! terminal, and will be treated as part of a macro recording. These terminal keypresses will
//! interrupt any pending macro keypresses, which are fed incrementally as needed during every
//! [BindingMachine::pop] call. This allows creating stop-on-error behaviour when executing
//! macros.
//!
//! ## Examples
//!
//! ```
//! use modalkit::{
//!     editing::key::KeyManager,
//!     editing::store::Store,
//!     env::vim::{VimContext, keybindings::VimMachine},
//! };
//!
//! let store = Store::<VimContext, ()>::new();
//! let bindings = VimMachine::default();
//! let bindings = KeyManager::new(bindings, store);
//! ```
use std::collections::VecDeque;

use crate::input::{bindings::BindingMachine, key::InputKey, InputContext};

use super::{
    action::{EditError, EditResult, MacroAction},
    base::{Application, EditContext, Register},
    rope::EditRope,
    store::{RegisterPutFlags, SharedStore},
};

/// Wraps keybindings so that they can be fed simulated keypresses from macros.
pub struct KeyManager<K, A, S, C, P = ()>
where
    K: InputKey,
    C: EditContext + InputContext,
    P: Application,
{
    store: SharedStore<C, P>,
    bindings: Box<dyn BindingMachine<K, A, S, C>>,
    keystack: VecDeque<K>,

    recording: Option<(Register, bool)>,
    commit_on_input: bool,
    committed: EditRope,
    pending: EditRope,
}

impl<K, A, S, C, P> KeyManager<K, A, S, C, P>
where
    K: InputKey,
    C: EditContext + InputContext,
    P: Application,
{
    /// Create a new instance.
    pub fn new<B: BindingMachine<K, A, S, C> + 'static>(
        bindings: B,
        store: SharedStore<C, P>,
    ) -> Self {
        let bindings = Box::new(bindings);

        Self {
            store,
            bindings,
            keystack: VecDeque::new(),

            recording: None,
            commit_on_input: false,
            committed: EditRope::from(""),
            pending: EditRope::from(""),
        }
    }

    /// Process a macro action.
    pub fn macro_command(&mut self, act: MacroAction, ctx: &C) -> EditResult {
        let (mstr, count) = match act {
            MacroAction::Execute(count) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedMacro);
                let mut locked = self.store.write().unwrap();
                let rope = locked.registers.get_macro(reg);

                (rope.to_string(), ctx.resolve(&count))
            },
            MacroAction::Repeat(count) => {
                let locked = self.store.read().unwrap();
                let rope = locked.registers.get_last_macro().ok_or(EditError::NoMacro)?;

                (rope.to_string(), ctx.resolve(&count))
            },
            MacroAction::ToggleRecording => {
                if let Some((reg, append)) = self.recording {
                    // Save macro to register.
                    let mut locked = self.store.write().unwrap();
                    let mut rope = EditRope::from("");
                    std::mem::swap(&mut rope, &mut self.committed);

                    let mut flags = RegisterPutFlags::NOTEXT;

                    if append {
                        flags |= RegisterPutFlags::APPEND;
                    }

                    locked.registers.put(&reg, rope.into(), flags);

                    // Stop recording.
                    self.recording = None;
                    self.commit_on_input = false;
                    self.pending = EditRope::from("");
                } else {
                    let reg = ctx.get_register().unwrap_or(Register::UnnamedMacro);

                    self.recording = Some((reg, ctx.get_register_append()));
                }

                return Ok(None);
            },
        };

        for _ in 0..count {
            let mut keys = VecDeque::from(K::from_macro_str(mstr.as_ref())?);
            keys.append(&mut self.keystack);
            self.keystack = keys;
        }

        return Ok(None);
    }
}

impl<K, A, S, C, P> BindingMachine<K, A, S, C> for KeyManager<K, A, S, C, P>
where
    K: InputKey,
    C: EditContext + InputContext,
    P: Application,
{
    fn input_key(&mut self, key: K) {
        if self.recording.is_some() {
            let mut rope = EditRope::from(key.to_string());

            if self.commit_on_input {
                std::mem::swap(&mut self.pending, &mut rope);
                self.committed += rope;
                self.commit_on_input = false;
            } else {
                self.pending += rope;
            }
        }

        self.keystack.clear();
        self.bindings.input_key(key);
    }

    fn pop(&mut self) -> Option<(A, C)> {
        loop {
            if let res @ Some(_) = self.bindings.pop() {
                self.commit_on_input = true;

                return res;
            }

            match self.keystack.pop_front() {
                Some(key) => self.bindings.input_key(key),
                None => return None,
            }
        }
    }

    fn context(&self) -> C {
        self.bindings.context()
    }

    fn showmode(&self) -> Option<String> {
        self.bindings.showmode()
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        self.bindings.get_cursor_indicator()
    }

    fn repeat(&mut self, seq: S, other: Option<C>) {
        self.bindings.repeat(seq, other)
    }
}

#[cfg(test)]
#[macro_use]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyEvent};

    use crate::{
        editing::base::Count,
        editing::store::{SharedStore, Store},
        env::vim::VimContext,
        env::CommonKeyClass,
        input::{
            bindings::EdgeEvent::{Class, Key},
            bindings::{EmptySequence, ModalMachine, Mode, ModeKeys, Step},
            key::TerminalKey,
        },
    };

    type TestStore = SharedStore<VimContext, ()>;
    type TestMachine = ModalMachine<TerminalKey, TestStep>;
    type TestKeyManager = KeyManager<TerminalKey, TestAction, EmptySequence, VimContext>;

    #[derive(Clone)]
    struct TestStep(Option<TestAction>, Option<TestMode>);

    impl Step<TerminalKey> for TestStep {
        type A = TestAction;
        type C = VimContext;
        type Class = CommonKeyClass;
        type M = TestMode;
        type Sequence = EmptySequence;

        fn is_unmapped(&self) -> bool {
            self.0.is_none() && self.1.is_none()
        }

        fn step(&self, ctx: &mut VimContext) -> (Vec<TestAction>, Option<TestMode>) {
            let act = self.0.clone().into_iter().collect();

            ctx.action.count = ctx.action.counting;

            (act, self.1.clone())
        }
    }

    impl From<TestAction> for TestStep {
        fn from(action: TestAction) -> Self {
            TestStep(Some(action), None)
        }
    }

    impl From<TestMode> for TestStep {
        fn from(mode: TestMode) -> Self {
            TestStep(None, Some(mode))
        }
    }

    #[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
    enum TestMode {
        Normal,
        Insert,
    }

    impl Default for TestMode {
        fn default() -> Self {
            TestMode::Normal
        }
    }

    impl Mode<TestAction, VimContext> for TestMode {}

    impl ModeKeys<TerminalKey, TestAction, VimContext> for TestMode {
        fn unmapped(
            &self,
            key: &TerminalKey,
            _: &mut VimContext,
        ) -> (Vec<TestAction>, Option<TestMode>) {
            match self {
                TestMode::Normal => {
                    return (vec![], None);
                },
                TestMode::Insert => {
                    if let Some(c) = key.get_char() {
                        return (vec![TestAction::Type(c)], None);
                    }

                    return (vec![], None);
                },
            }
        }
    }

    #[derive(Clone)]
    enum TestAction {
        Macro(MacroAction),
        SetFlag,
        Type(char),
        NoOp,
    }

    impl Default for TestAction {
        fn default() -> Self {
            TestAction::NoOp
        }
    }

    fn setup_bindings() -> (TestKeyManager, TestStore) {
        use crate::input::bindings::EdgeRepeat::{Min, Once};

        let mut bindings = TestMachine::empty();
        let store = Store::new();

        // Normal mode mappings
        bindings.add_mapping(
            TestMode::Normal,
            &vec![
                (Once, Key("q".parse().unwrap())),
                (Once, Key("q".parse().unwrap())),
                (Once, Key("q".parse().unwrap())),
            ],
            &TestAction::Macro(MacroAction::ToggleRecording).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &vec![(Once, Key("@".parse().unwrap()))],
            &TestAction::Macro(MacroAction::Repeat(Count::Contextual)).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &vec![(Once, Key("Q".parse().unwrap()))],
            &TestAction::Macro(MacroAction::Execute(Count::Contextual)).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &vec![(Once, Key("f".parse().unwrap()))],
            &TestAction::SetFlag.into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &vec![(Once, Key("i".parse().unwrap()))],
            &TestMode::Insert.into(),
        );

        // Normal mode prefixes
        bindings.add_prefix(
            TestMode::Normal,
            &vec![
                (Once, Key("\"".parse().unwrap())),
                (Once, Class(CommonKeyClass::Register)),
            ],
            &None,
        );
        bindings.add_prefix(TestMode::Normal, &vec![(Min(1), Class(CommonKeyClass::Count))], &None);

        // Insert mode mappings
        bindings.add_mapping(
            TestMode::Insert,
            &vec![(Once, Key("<Esc>".parse().unwrap()))],
            &TestMode::Normal.into(),
        );

        (TestKeyManager::new(bindings, store.clone()), store)
    }

    #[test]
    fn test_record_and_execute() {
        let (mut bindings, store) = setup_bindings();
        let mut s = String::new();
        let mut flag = false;

        let get_register =
            |reg: Register| -> EditRope { store.read().unwrap().registers.get(&reg).value };

        let mut input = |key: TerminalKey, s: &mut String, flag: &mut bool| {
            bindings.input_key(key);

            while let Some((act, ctx)) = bindings.pop() {
                match act {
                    TestAction::NoOp => continue,
                    TestAction::Macro(act) => {
                        let _ = bindings.macro_command(act, &ctx).unwrap();
                    },
                    TestAction::SetFlag => {
                        *flag = true;
                    },
                    TestAction::Type(c) => s.push(c),
                }
            }
        };

        macro_rules! input {
            ($key: expr) => {
                input($key, &mut s, &mut flag)
            };
        }

        // Register::UnnamedMacro is currently empty.
        assert_eq!(get_register(Register::UnnamedMacro).to_string(), "");

        // Press an unmapped key before recording.
        input!(key!('l'));

        // Start recording to "a.
        input!(key!('"'));
        input!(key!('a'));
        input!(key!('q'));
        input!(key!('q'));
        input!(key!('q'));

        // Type some unmapped keys.
        input!(key!('n'));
        input!(key!('z'));

        // Type a mapped key.
        input!(key!('f'));

        // Move to Insert mode and type some characters.
        input!(key!('i'));
        input!(key!('a'));
        input!(key!('b'));
        input!(key!('c'));

        // Move back to Normal mode.
        input!("<Esc>".parse().unwrap());

        // End recording.
        input!(key!('q'));
        input!(key!('q'));
        input!(key!('q'));

        // Register::Named('a') now contains macro text.
        assert_eq!(get_register(Register::Named('a')).to_string(), "nzfiabc<Esc>");
        assert_eq!(s, "abc");
        assert_eq!(flag, true);

        // Reset flag before replaying.
        flag = false;

        // Replay macro twice.
        input!(key!('"'));
        input!(key!('a'));
        input!(key!('2'));
        input!(key!('Q'));
        assert_eq!(s, "abcabcabc");
        assert_eq!(flag, true);

        // Reset flag before replaying.
        flag = false;

        // Replay last macro.
        input!(key!('@'));
        assert_eq!(s, "abcabcabcabc");
        assert_eq!(flag, true);

        // Reset flag before replaying.
        flag = false;

        // Record an unnamed macro that executes "a.
        input!(key!('q'));
        input!(key!('q'));
        input!(key!('q'));
        input!(key!('i'));
        input!(key!('d'));
        input!(key!('e'));
        input!(key!('f'));
        input!("<Esc>".parse().unwrap());
        input!(key!('"'));
        input!(key!('a'));
        input!(key!('Q'));
        input!(key!('q'));
        input!(key!('q'));
        input!(key!('q'));

        // Register::UnnamedMacro now contains macro text.
        assert_eq!(get_register(Register::UnnamedMacro).to_string(), "idef<Esc>\"aQ");
        assert_eq!(get_register(Register::Named('a')).to_string(), "nzfiabc<Esc>");
        assert_eq!(s, "abcabcabcabcdefabc");
        assert_eq!(flag, true);

        // Reset flag before replaying.
        flag = false;

        // Execute Register::UnnamedMacro.
        input!(key!('3'));
        input!(key!('Q'));
        assert_eq!(s, "abcabcabcabcdefabcdefabcdefabcdefabc");
        assert_eq!(flag, true);

        // Reset flag before replaying.
        flag = false;

        // Replaying replays "a, since that was last used inside "@.
        input!(key!('2'));
        input!(key!('@'));
        assert_eq!(s, "abcabcabcabcdefabcdefabcdefabcdefabcabcabc");
        assert_eq!(flag, true);
    }
}
