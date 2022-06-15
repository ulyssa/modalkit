use std::collections::HashMap;
use std::fmt::Debug;
use std::str::FromStr;

use super::InputContext;

pub trait InputCmdContext: InputContext {}

pub enum CommandStep<C: Command> {
    Continue(C::Action, C::Context),
    Stop(C::Action, C::Context),
    Again(String),
}

#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum CommandError<C: Command> {
    #[error("Invalid command: {0}")]
    InvalidCommand(String),
    #[error("Invalid argument")]
    InvalidArgument,
    #[error("Invalid range")]
    InvalidRange,
    #[error("Failed to parse command: {0}")]
    ParseFailed(<C::Parsed as FromStr>::Err),
    #[error("Error: {0}")]
    Error(String),
}

pub type CommandResult<C> = Result<CommandStep<C>, CommandError<C>>;

pub trait ParsedCommand: Debug + FromStr<Err = String> {
    fn name(&self) -> String;
}

pub trait Command: Clone {
    type Parsed: ParsedCommand;
    type Action;
    type Context;
    type CommandContext: InputCmdContext + From<Self::Context>;

    fn names(&self) -> Vec<String>;
    fn exec(&self, cmd: Self::Parsed, ctx: &mut Self::CommandContext) -> CommandResult<Self>;
}

pub trait DefaultCommands<C: Command>: Default {
    fn setup(self, machine: &mut CommandMachine<C>);
}

pub struct CommandMachine<C: Command> {
    commands: HashMap<String, C>,
}

impl<C: Command> CommandMachine<C> {
    pub fn new() -> Self {
        let commands = HashMap::new();

        CommandMachine { commands }
    }

    pub fn add_command(&mut self, cmd: C) {
        for name in cmd.names().into_iter() {
            self.commands.insert(name, cmd.clone());
        }
    }

    pub fn input_cmd<T: Into<String>>(
        &self,
        input: T,
        ctx: C::Context,
    ) -> Result<Vec<(C::Action, C::Context)>, CommandError<C>> {
        let mut input: String = input.into();
        let mut results = Vec::new();
        let mut ctx = C::CommandContext::from(ctx);

        loop {
            let cmd = match C::Parsed::from_str(&input) {
                Ok(cmd) => cmd,
                Err(e) => return Err(CommandError::ParseFailed(e)),
            };
            let name = cmd.name();

            if name == "" {
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
