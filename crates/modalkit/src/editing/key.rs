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
//!     editing::application::EmptyInfo,
//!     editing::key::KeyManager,
//!     env::vim::keybindings::default_vim_keys,
//!     key::TerminalKey,
//! };
//!
//! let bindings = default_vim_keys::<EmptyInfo>();
//! let bindings = KeyManager::new(bindings);
//! ```
use std::borrow::Cow;
use std::collections::VecDeque;

use crate::actions::MacroAction;
use crate::errors::{EditError, EditResult};
use crate::key::MacroError;
use crate::keybindings::{dialog::Dialog, BindingMachine, InputKey};
use crate::prelude::*;

use super::{
    application::ApplicationInfo,
    context::{EditContext, Resolve},
    rope::EditRope,
    store::{RegisterPutFlags, Store},
};

const MAX_MACRO_EXEC_DEPTH: usize = 100;

/// Wraps keybindings so that they can be fed simulated keypresses from macros.
pub struct KeyManager<K, A, S>
where
    K: InputKey,
{
    bindings: Box<dyn BindingMachine<K, A, S, EditContext>>,
    keystack: VecDeque<K>,

    recording: Option<(Register, bool)>,
    macro_exec_depth: usize,
    commit_on_input: bool,
    committed: EditRope,
    pending: EditRope,
}

impl<K, A, S> KeyManager<K, A, S>
where
    K: InputKey,
{
    /// Create a new instance.
    pub fn new<B: BindingMachine<K, A, S, EditContext> + 'static>(bindings: B) -> Self {
        let bindings = Box::new(bindings);

        Self {
            bindings,
            keystack: VecDeque::new(),

            recording: None,
            macro_exec_depth: 0,
            commit_on_input: false,
            committed: EditRope::from(""),
            pending: EditRope::from(""),
        }
    }

    /// Process a macro action.
    pub fn macro_command<I: ApplicationInfo>(
        &mut self,
        act: &MacroAction,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>
    where
        K: InputKey<Error = MacroError>,
    {
        let (mstr, count) = match act {
            MacroAction::Execute(count) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedMacro);
                let rope = store.registers.get_macro(reg)?;

                (rope.to_string(), ctx.resolve(count))
            },
            MacroAction::Run(s, count) => (s.clone(), ctx.resolve(count)),
            MacroAction::Repeat(count) => {
                let rope = store.registers.get_last_macro()?;
                (rope.to_string(), ctx.resolve(count))
            },
            MacroAction::ToggleRecording => {
                if let Some((reg, append)) = &self.recording {
                    // Save macro to register.
                    let mut rope = EditRope::from("");
                    std::mem::swap(&mut rope, &mut self.committed);

                    let mut flags = RegisterPutFlags::NOTEXT;

                    if *append {
                        flags |= RegisterPutFlags::APPEND;
                    }

                    store.registers.put(reg, rope.into(), flags)?;

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
            act => {
                return Err(EditError::Unimplemented(format!("unknown action: {act:?}")));
            },
        };

        self.macro_exec_depth += 1;

        if self.macro_exec_depth >= MAX_MACRO_EXEC_DEPTH {
            let err = MacroError::LoopingMacro(self.macro_exec_depth);
            return Err(err.into());
        }

        for _ in 0..count {
            let mut keys = VecDeque::from(K::from_macro_str(mstr.as_ref())?);
            keys.append(&mut self.keystack);
            self.keystack = keys;
        }

        return Ok(None);
    }
}

impl<K, A, S> BindingMachine<K, A, S, EditContext> for KeyManager<K, A, S>
where
    K: InputKey + ToString,
{
    fn input_key(&mut self, key: K) {
        self.macro_exec_depth = 0;

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

    fn pop(&mut self) -> Option<(A, EditContext)> {
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

    fn context(&mut self) -> EditContext {
        self.bindings.context()
    }

    fn show_dialog(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>> {
        self.bindings.show_dialog(max_rows, max_cols)
    }

    fn show_mode(&self) -> Option<String> {
        self.bindings.show_mode()
    }

    fn reset_mode(&mut self) {
        self.bindings.reset_mode()
    }

    fn get_cursor_indicator(&self) -> Option<char> {
        self.bindings.get_cursor_indicator()
    }

    fn repeat(&mut self, seq: S, other: Option<EditContext>) {
        self.bindings.repeat(seq, other)
    }

    fn run_dialog(&mut self, dialog: Box<dyn Dialog<A>>) {
        self.bindings.run_dialog(dialog)
    }
}

#[cfg(test)]
#[macro_use]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyEvent};

    use crate::{
        editing::application::EmptyInfo,
        editing::store::{RegisterError, Store},
        env::vim::VimState,
        env::CommonKeyClass,
        errors::EditError,
        key::TerminalKey,
        keybindings::EdgeEvent::{Class, Key},
        keybindings::{dialog::PromptYesNo, EmptySequence, ModalMachine, Mode, ModeKeys, Step},
    };

    type TestStore = Store<EmptyInfo>;
    type TestMachine = ModalMachine<TerminalKey, TestStep>;
    type TestKeyManager = KeyManager<TerminalKey, TestAction, EmptySequence>;

    #[derive(Clone)]
    struct TestStep(Option<TestAction>, Option<TestMode>);

    impl Step<TerminalKey> for TestStep {
        type A = TestAction;
        type State = VimState;
        type Class = CommonKeyClass;
        type M = TestMode;
        type Sequence = EmptySequence;

        fn is_unmapped(&self) -> bool {
            self.0.is_none() && self.1.is_none()
        }

        fn step(&self, ctx: &mut VimState) -> (Vec<TestAction>, Option<TestMode>) {
            let act = self.0.clone().into_iter().collect();

            ctx.action.count = ctx.action.counting;

            (act, self.1)
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

    #[derive(Clone, Copy, Debug, Default, Hash, Eq, PartialEq)]
    enum TestMode {
        #[default]
        Normal,
        Insert,
    }

    impl Mode<TestAction, VimState> for TestMode {}

    impl ModeKeys<TerminalKey, TestAction, VimState> for TestMode {
        fn unmapped(
            &self,
            key: &TerminalKey,
            _: &mut VimState,
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

    #[derive(Clone, Debug, Default, Eq, PartialEq)]
    enum TestAction {
        Macro(MacroAction),
        SetFlag(bool),
        Type(char),
        #[default]
        NoOp,
    }

    fn setup_recursive_bindings() -> (TestKeyManager, TestStore) {
        use crate::keybindings::EdgeRepeat::{Min, Once};

        let mut bindings = TestMachine::empty();
        let store = Store::default();

        // Normal mode mappings
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("n".parse().unwrap()))],
            &TestAction::NoOp.into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("a".parse().unwrap()))],
            &TestAction::Macro(MacroAction::Run("n".into(), Count::Contextual)).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("b".parse().unwrap()))],
            &TestAction::Macro(MacroAction::Run("aa".into(), Count::Contextual)).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("c".parse().unwrap()))],
            &TestAction::Macro(MacroAction::Run("c".into(), Count::Contextual)).into(),
        );

        // Normal mode prefixes
        bindings.add_prefix(
            TestMode::Normal,
            &[
                (Once, Key("\"".parse().unwrap())),
                (Once, Class(CommonKeyClass::Register)),
            ],
            &None,
        );
        bindings.add_prefix(TestMode::Normal, &[(Min(1), Class(CommonKeyClass::Count))], &None);

        (TestKeyManager::new(bindings), store)
    }

    fn setup_bindings(skip_confirm: bool) -> (TestKeyManager, TestStore) {
        use crate::keybindings::EdgeRepeat::{Min, Once};

        let mut bindings = TestMachine::empty();
        let store = Store::default();

        // Normal mode mappings
        bindings.add_mapping(
            TestMode::Normal,
            &[
                (Once, Key("q".parse().unwrap())),
                (Once, Key("q".parse().unwrap())),
                (Once, Key("q".parse().unwrap())),
            ],
            &TestAction::Macro(MacroAction::ToggleRecording).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("@".parse().unwrap()))],
            &TestAction::Macro(MacroAction::Repeat(Count::Contextual)).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("Q".parse().unwrap()))],
            &TestAction::Macro(MacroAction::Execute(Count::Contextual)).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("f".parse().unwrap()))],
            &TestAction::SetFlag(skip_confirm).into(),
        );
        bindings.add_mapping(
            TestMode::Normal,
            &[(Once, Key("i".parse().unwrap()))],
            &TestMode::Insert.into(),
        );

        // Normal mode prefixes
        bindings.add_prefix(
            TestMode::Normal,
            &[
                (Once, Key("\"".parse().unwrap())),
                (Once, Class(CommonKeyClass::Register)),
            ],
            &None,
        );
        bindings.add_prefix(TestMode::Normal, &[(Min(1), Class(CommonKeyClass::Count))], &None);

        // Insert mode mappings
        bindings.add_mapping(
            TestMode::Insert,
            &[(Once, Key("<Esc>".parse().unwrap()))],
            &TestMode::Normal.into(),
        );

        (TestKeyManager::new(bindings), store)
    }

    fn input(
        key: TerminalKey,
        bindings: &mut TestKeyManager,
        store: &mut TestStore,
        s: &mut String,
        flag: &mut bool,
        noops: &mut usize,
        err: &mut Option<EditError<EmptyInfo>>,
    ) {
        *noops = 0;
        *err = None;

        bindings.input_key(key);

        while let Some((act, ctx)) = bindings.pop() {
            match act {
                TestAction::NoOp => {
                    *noops += 1;
                    continue;
                },
                TestAction::Macro(act) => {
                    *err = bindings.macro_command(&act, &ctx, store).err();

                    if err.is_some() {
                        return;
                    }
                },
                TestAction::SetFlag(skip_confirm) => {
                    if skip_confirm {
                        *flag = true;
                    } else {
                        let act = vec![TestAction::SetFlag(true)];
                        let msg = "Are you sure you want to set the flag";
                        let confirm = PromptYesNo::new(msg, act);
                        bindings.run_dialog(Box::new(confirm));
                    }
                },
                TestAction::Type(c) => s.push(c),
            }
        }
    }

    #[test]
    fn test_macro_run() {
        let (mut bindings, mut store) = setup_bindings(true);
        let ctx = EditContext::from(VimState::<EmptyInfo>::default());

        // Run a sequence of key presses twice.
        let run = MacroAction::Run("flif<Esc>fi".into(), 2.into());
        let _ = bindings.macro_command(&run, &ctx, &mut store).unwrap();

        // Run the first time, starting in Normal mode.
        assert_pop1!(bindings, TestAction::SetFlag(true), ctx);
        assert_pop1!(bindings, TestAction::NoOp, ctx);
        assert_pop1!(bindings, TestAction::NoOp, ctx);
        assert_pop1!(bindings, TestAction::Type('f'), ctx);
        assert_pop1!(bindings, TestAction::NoOp, ctx);
        assert_pop1!(bindings, TestAction::SetFlag(true), ctx);
        assert_pop1!(bindings, TestAction::NoOp, ctx);

        // Run the second time, but this time in Insert mode.
        assert_pop1!(bindings, TestAction::Type('f'), ctx);
        assert_pop1!(bindings, TestAction::Type('l'), ctx);
        assert_pop1!(bindings, TestAction::Type('i'), ctx);
        assert_pop1!(bindings, TestAction::Type('f'), ctx);
        assert_pop1!(bindings, TestAction::NoOp, ctx);
        assert_pop1!(bindings, TestAction::SetFlag(true), ctx);
        assert_pop2!(bindings, TestAction::NoOp, ctx);
    }

    #[test]
    fn test_record_and_execute() {
        let (mut bindings, mut store) = setup_bindings(true);
        let mut s = String::new();
        let mut flag = false;
        let mut err = None;
        let mut noops = 0;

        macro_rules! get_register {
            ($reg: expr) => {
                store.registers.get(&$reg).unwrap().value
            };
        }

        macro_rules! input {
            ($key: expr) => {
                input($key, &mut bindings, &mut store, &mut s, &mut flag, &mut noops, &mut err)
            };
        }

        // Register::UnnamedMacro is currently empty.
        assert_eq!(get_register!(Register::UnnamedMacro).to_string(), "");

        // No last macro to repeat.
        input!(key!('@'));
        assert!(matches!(err, Some(EditError::Register(RegisterError::NoLastMacro))), "{:?}", err);

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
        assert_eq!(get_register!(Register::Named('a')).to_string(), "nzfiabc<Esc>");
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
        assert_eq!(get_register!(Register::UnnamedMacro).to_string(), "idef<Esc>\"aQ");
        assert_eq!(get_register!(Register::Named('a')).to_string(), "nzfiabc<Esc>");
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

    #[test]
    fn test_macro_dialog() {
        let (mut bindings, mut store) = setup_bindings(false);
        let mut s = String::new();
        let mut flag = false;
        let mut err = None;
        let mut noops = 0;

        macro_rules! get_register {
            ($reg: expr) => {
                store.registers.get(&$reg).unwrap().value
            };
        }

        macro_rules! input {
            ($key: expr) => {
                input($key, &mut bindings, &mut store, &mut s, &mut flag, &mut noops, &mut err)
            };
        }

        assert_eq!(bindings.show_dialog(10, 100).len(), 0);
        assert_eq!(flag, false);

        // Start recording to "a.
        input!(key!('"'));
        input!(key!('a'));
        input!(key!('q'));
        input!(key!('q'));
        input!(key!('q'));

        // Typing `f` starts a new dialog.
        input!(key!('f'));
        assert_eq!(bindings.show_dialog(10, 100).len(), 1);
        assert_eq!(flag, false);

        // Answer `y` to the prompt to clear dialog and set flag.
        input!(key!('y'));
        assert_eq!(bindings.show_dialog(10, 100).len(), 0);
        assert_eq!(flag, true);

        // End recording.
        input!(key!('q'));
        input!(key!('q'));
        input!(key!('q'));

        // Register::Named('a') now contains macro text.
        assert_eq!(get_register!(Register::Named('a')).to_string(), "fy");

        // Now reset the flag.
        flag = false;

        // Replay macro.
        input!(key!('"'));
        input!(key!('a'));
        input!(key!('Q'));

        // Flag is now true, and we end with the dialog cleared.
        assert_eq!(bindings.show_dialog(10, 100).len(), 0);
        assert_eq!(flag, true);
    }

    #[test]
    fn test_macro_limit_depth() {
        let (mut bindings, mut store) = setup_recursive_bindings();
        let mut s = String::new();
        let mut flag = false;
        let mut err = None;
        let mut noops = 0;

        macro_rules! input {
            ($key: expr) => {
                input($key, &mut bindings, &mut store, &mut s, &mut flag, &mut noops, &mut err)
            };
        }

        // It's okay to run a MacroAction::Run with a count >100.
        input!(key!('1'));
        input!(key!('1'));
        input!(key!('0'));
        input!(key!('a'));
        assert!(err.is_none());
        assert_eq!(noops, 110);

        // It's okay to run a macro that calls other macros multiple times.
        input!(key!('4'));
        input!(key!('9'));
        input!(key!('b'));
        assert!(err.is_none());
        assert_eq!(noops, 98);

        // But if they repeatedly call other macros enough times, we abort.
        input!(key!('5'));
        input!(key!('0'));
        input!(key!('b'));
        println!("err = {err:?}");
        assert!(matches!(
            err.take().unwrap(),
            EditError::MacroFailure(MacroError::LoopingMacro(100))
        ));
        assert_eq!(noops, 98);

        // And a key that just always triggers itself should error.
        input!(key!('c'));
        println!("err = {err:?}");
        assert!(matches!(
            err.take().unwrap(),
            EditError::MacroFailure(MacroError::LoopingMacro(100))
        ));
        assert_eq!(noops, 0);
    }
}
