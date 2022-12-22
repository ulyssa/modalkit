//! # Command processing
//!
//! ## Overview
//!
//! This module contains components to help consumers map commands into actions.
//!
use std::collections::HashMap;
use std::fmt::Debug;
use std::str::FromStr;

use super::InputContext;

/// Context passed to commands when they're executed.
pub trait InputCmdContext: InputContext {}

/// Result from executing a single command in a sequence.
pub enum CommandStep<C: Command> {
    /// Return an action, and continue onto the next part of the command sequence.
    Continue(C::Action, C::Context),

    /// Return an action, and skip anything else in the command sequence.
    Stop(C::Action, C::Context),

    /// Process the given string as a command.
    Again(String),
}

/// Errors that can be encountered during command processing.
#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum CommandError {
    /// Error for unmapped commands.
    #[error("Invalid command: {0}")]
    InvalidCommand(String),

    /// Error for bad command arguments.
    #[error("Invalid argument")]
    InvalidArgument,

    /// Error for bad ranges.
    #[error("Invalid range")]
    InvalidRange,

    /// Error for command parse failures.
    #[error("Failed to parse command: {0}")]
    ParseFailed(String),

    /// Generic error.
    #[error("Error: {0}")]
    Error(String),
}

/// Result type for individual, mapped commands.
pub type CommandResult<C> = Result<CommandStep<C>, CommandError>;

/// Trait for result type of parsing commands.
pub trait ParsedCommand: Debug + FromStr<Err = String> {
    /// Get the name of the command being executed.
    fn name(&self) -> String;
}

/// Trait for mapped commands.
pub trait Command: Clone {
    /// Result of parsing a command string.
    type Parsed: ParsedCommand;

    /// Result of running a command.
    type Action;

    /// Context provided with each command string.
    type Context: InputContext;

    /// Context to be passed to [Command::exec].
    type CommandContext: InputCmdContext + From<Self::Context>;

    /// Names to map this command under.
    fn names(&self) -> Vec<String>;

    /// Execute this command.
    fn exec(&self, cmd: Self::Parsed, ctx: &mut Self::CommandContext) -> CommandResult<Self>;
}

/// A collection of default commands.
pub trait DefaultCommands<C: Command>: Default {
    /// Insert the commands contained by this object into a [CommandMachine].
    fn setup(self, machine: &mut CommandMachine<C>);
}

/// Track mapped commands and handle their execution.
#[derive(Debug)]
pub struct CommandMachine<C: Command> {
    commands: HashMap<String, C>,
    last_cmd: String,
}

impl<C: Command> CommandMachine<C> {
    /// Create a new instance.
    pub fn new() -> Self {
        let commands = HashMap::new();
        let last_cmd = "".to_string();

        CommandMachine { commands, last_cmd }
    }

    /// Map a command under its names.
    pub fn add_command(&mut self, cmd: C) {
        for name in cmd.names().into_iter() {
            self.commands.insert(name, cmd.clone());
        }
    }

    /// Get the previously executed command.
    pub fn get_last_command(&self) -> String {
        self.last_cmd.clone()
    }

    /// Parse and execute a command string.
    pub fn input_cmd<T: Into<String>>(
        &mut self,
        input: T,
        ctx: C::Context,
    ) -> Result<Vec<(C::Action, C::Context)>, CommandError> {
        let mut input: String = input.into();
        let mut results = Vec::new();
        let mut ctx = C::CommandContext::from(ctx);

        self.last_cmd = input.clone();

        loop {
            let cmd = match C::Parsed::from_str(&input) {
                Ok(cmd) => cmd,
                Err(e) => return Err(CommandError::ParseFailed(e)),
            };
            let name = cmd.name();

            if name.is_empty() {
                return Ok(results);
            }

            if let Some(mapping) = self.commands.get(&name) {
                match mapping.exec(cmd, &mut ctx)? {
                    CommandStep::Continue(act, c) => {
                        // XXX: need to support processing the next command.
                        results.push((act, c));

                        return Ok(results);
                    },
                    CommandStep::Stop(act, c) => {
                        results.push((act, c));

                        return Ok(results);
                    },
                    CommandStep::Again(next) => {
                        input = next;
                    },
                }
            } else {
                return Err(CommandError::InvalidCommand(name));
            }
        }
    }
}
