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
    base::{Application, EditContext, EditError, EditResult, MacroAction, Register},
    rope::EditRope,
    store::SharedStore,
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
    typed: EditRope,
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
            typed: EditRope::from(""),
        }
    }

    /// Process a macro action.
    pub fn macro_command(&mut self, act: MacroAction, ctx: &C) -> EditResult {
        let (mstr, count) = match act {
            MacroAction::Execute(count) => {
                let reg = ctx.get_register().unwrap_or(Register::UnnamedMacro);
                let locked = self.store.read().unwrap();
                let rope = locked.registers.get(&Some(reg)).value;

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
                    std::mem::swap(&mut rope, &mut self.typed);

                    locked.registers.put(&Some(reg), rope.into(), append, false);

                    // Stop recording.
                    self.recording = None;
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
            self.typed += EditRope::from(key.to_string());
        }

        self.keystack.clear();
        self.bindings.input_key(key);
    }

    fn pop(&mut self) -> Option<(A, C)> {
        loop {
            if let res @ Some(_) = self.bindings.pop() {
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
