//! # Tracking and processing for commands
//!
//! ## Overview
//!
//! This module contains components to help consumers map commands into actions.
//!
use std::fmt::Debug;
use std::str::FromStr;

use radix_trie::Trie;

use crate::util::completion_keys;

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
    type Context;

    /// Context to be passed to [Command::exec].
    type CommandContext: From<Self::Context>;

    /// The primary name to map this command under.
    fn name(&self) -> String;

    /// Additional names to map this command under.
    fn aliases(&self) -> Vec<String>;

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
    names: Trie<String, C>,
    aliases: Trie<String, C>,
    last_cmd: String,
}

impl<C: Command> CommandMachine<C> {
    /// Create a new instance.
    pub fn new() -> Self {
        let names = Trie::new();
        let aliases = Trie::new();
        let last_cmd = "".to_string();

        CommandMachine { names, aliases, last_cmd }
    }

    /// Map a command under its names.
    pub fn add_command(&mut self, cmd: C) {
        for alias in cmd.aliases().into_iter() {
            self.aliases.insert(alias, cmd.clone());
        }

        self.names.insert(cmd.name(), cmd);
    }

    /// Generate a list of completion candidates for command names.
    pub fn complete_name(&self, prefix: &str) -> Vec<String> {
        completion_keys(&self.names, prefix)
    }

    /// Generate a list of completion candidates for command aliases.
    pub fn complete_aliases(&self, prefix: &str) -> Vec<String> {
        completion_keys(&self.aliases, prefix)
    }

    /// Get the previously executed command.
    pub fn get(&self, name: &str) -> Result<&C, CommandError> {
        if let Some(m) = self.names.get(name) {
            Ok(m)
        } else if let Some(m) = self.aliases.get(name) {
            Ok(m)
        } else {
            Err(CommandError::InvalidCommand(name.into()))
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
            let cmd = C::Parsed::from_str(&input).map_err(CommandError::ParseFailed)?;
            let name = cmd.name();

            if name.is_empty() {
                return Ok(results);
            }

            match self.get(&name)?.exec(cmd, &mut ctx)? {
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
        }
    }
}
