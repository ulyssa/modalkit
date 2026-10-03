use std::borrow::Cow;
use std::collections::HashSet;

pub mod error;
mod tokenizer;

pub use tokenizer::tokenize;

pub const DEFAULT_FALSE: [ActionToken<'static>; 1] = [ActionToken::Bool(false)];
pub const DEFAULT_TRUE: [ActionToken<'static>; 1] = [ActionToken::Bool(true)];
pub const DEFAULT_FILTER: [ActionToken<'static>; 1] = [ActionToken::Word("all")];
pub const DEFAULT_COMPTYPE: [ActionToken<'static>; 1] = [ActionToken::Word("auto")];
pub const DEFAULT_COUNT: [ActionToken<'static>; 1] = [ActionToken::Word("ctx")];
pub const DEFAULT_MARK: [ActionToken<'static>; 1] = [ActionToken::Word("ctx")];
pub const DEFAULT_OP: [ActionToken<'static>; 1] = [ActionToken::Word("ctx")];
pub const DEFAULT_PREV: [ActionToken<'static>; 1] = [ActionToken::Word("previous")];

const EMPTY_ACTION: [ActionToken<'static>; 0] = [];

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArgError {
    ExpectedFlag(Flag),
    ExpectedFlagBefore(String),
    MissingArg(Flag),
    UnexpectedFlag(Flag),
    DuplicateFlag(Flag),
}

impl ArgError {
    pub fn display<'a>(&'a self, cmd: &'a str) -> ArgErrorDisplay<'a> {
        ArgErrorDisplay { cmd, err: self }
    }
}

pub struct ArgErrorDisplay<'a> {
    cmd: &'a str,
    err: &'a ArgError,
}

impl<'a> std::fmt::Display for ArgErrorDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let cmd = self.cmd;
        match self.err {
            ArgError::ExpectedFlagBefore(s) => {
                write!(f, "`{cmd}` expected to find a flag argument before `{s}`")
            },
            ArgError::ExpectedFlag(flag) => write!(f, "`{cmd}` requires a `{flag}` argument"),
            ArgError::MissingArg(flag) => {
                write!(f, "`{cmd}` expects an argument following `{flag}`")
            },
            ArgError::UnexpectedFlag(flag) => write!(f, "`{cmd}` does not take `{flag}`"),
            ArgError::DuplicateFlag(flag) => write!(f, "`{cmd}` only takes one `{flag}`"),
        }
    }
}

pub fn ungroup<'a>(value: &'a [ActionToken<'a>]) -> &'a [ActionToken<'a>] {
    match value {
        [] => value,
        [ActionToken::Group(grouped), ..] => grouped.as_slice(),
        [_, ..] => &value[..=0],
    }
}

/// Break up the arguments to a command into pairs of flags and their arguments. If an argument is
/// an [ActionToken::Group], then its contents will be returned with the flag.
///
/// This returns an error if all of the arguments cannot be broken up into pairs of flags and
/// non-flags.
pub fn flag_pairs<'a>(
    args: &'a [ActionToken<'a>],
) -> Result<Vec<(&'a Flag, &'a [ActionToken<'a>])>, ArgError> {
    let mut pairs = vec![];
    let mut seen = HashSet::new();

    for pair in args.chunks(2) {
        match pair {
            [] => break,
            [ActionToken::Flag(f)] => return Err(ArgError::MissingArg(f.clone())),
            [ActionToken::Flag(f), ActionToken::Flag(_)] => {
                return Err(ArgError::MissingArg(f.clone()));
            },
            [ActionToken::Flag(f), _] => {
                if seen.contains(f) {
                    return Err(ArgError::DuplicateFlag(f.clone()));
                }

                seen.insert(f);
                pairs.push((f, ungroup(&pair[1..])));
            },
            [t, ..] => return Err(ArgError::ExpectedFlagBefore(t.to_string())),
        }
    }

    Ok(pairs)
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum Flag {
    /// A `--count` or `-c` flag in the input.
    ///
    /// In order to encourage common flag initials, `-c` should always take a `Count`.
    Count,

    /// A `--dir` or `-d` flag in the input.
    ///
    /// In order to encourage common flag initials, `-d` should always take one of the direction
    /// types (e.g., `MoveDirMod`, `MoveDir1D`, or `MoveDir2D`).
    Dir,

    /// A `--focus` or `-f` flag in the input.
    ///
    /// In order to encourage common flag initials, `-f` should always take a `FocusChange`.
    Focus,

    /// A `--input` or `-i` flag in the input.
    ///
    /// In order to encourage common flag initials, `-i` should always take an input `String`.
    Input,

    /// A `--mark` or `-m` flag in the input.
    ///
    /// In order to encourage common flag initials, `-m` should always take an input `Mark`.
    Mark,

    /// A `--position` or `-p` flag in the input.
    ///
    /// In order to encourage common flag initials, `-p` should always take an input of
    /// `MovePosition` or `MoveTerminus`.
    Position,

    /// A `--style` or `-s` flag in the input.
    ///
    /// In order to encourage common flag initials, `-s` should always take one of the `*Style`
    /// types.
    Style,

    /// A `--target` or `-t` flag in the input.
    ///
    /// In order to encourage common flag initials, `-t` should always take one of the
    /// `*Target` types.
    Target,

    /// A `--wrap` or `-w` flag in the input.
    ///
    /// In order to encourage common flag initials, `-w` should always take a `bool` to indicate
    /// whether or not to wrap.
    Wrap,

    /// A short flag with an action-specific meaning.
    #[cfg_attr(test, proptest(skip))]
    Short(char),

    /// A short flag with an action-specific meaning.
    #[cfg_attr(test, proptest(skip))]
    Long(String),
}

impl std::fmt::Display for Flag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Flag::Count => write!(f, "--count"),
            Flag::Dir => write!(f, "--dir"),
            Flag::Focus => write!(f, "--focus"),
            Flag::Input => write!(f, "--input"),
            Flag::Mark => write!(f, "--mark"),
            Flag::Position => write!(f, "--position"),
            Flag::Style => write!(f, "--style"),
            Flag::Target => write!(f, "--target"),
            Flag::Long(s) => write!(f, "--{s}"),
            Flag::Short(c) => write!(f, "-{c}"),
            Flag::Wrap => write!(f, "--wrap"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ActionToken<'a> {
    /// A bare, unquoted word in the input.
    Word(&'a str),

    /// An action-specific flag in the input.
    Flag(Flag),

    /// A quoted string in the input.
    ///
    /// Special characters can be escaped in the same way they are within Rust strings.
    Str(Cow<'a, str>),

    /// An `{id}` in the input that expands in the `action!` macro to an identifier.
    ///
    /// This will be `None` when the input is `{}`, and a positional argument will
    /// be used from the macro arguments instead.
    Id(Option<Cow<'a, str>>),

    /// A boolean in the input.
    ///
    /// This will always be either `true` or `false` in the input.
    Bool(bool),

    /// A number in the input.
    ///
    /// This can be a simple number like `5` or `1234567`, or one prefixed with `0x`, `0o`, or `0b`
    /// in order to parse the suffix in base 16, 8 or 2 respectively.
    Number(usize),

    /// A character in the input surrounded by single quotes (e.g., `'c'`).
    ///
    /// Special characters can be escaped in the same way they are within Rust.
    Char(char),

    /// A collection of tokens between parenthesis (e.g. `(foo bar 5)`).
    Group(Vec<ActionToken<'a>>),
}

impl ActionToken<'_> {
    /// Provide identifiers for each of the positional `{}` arguments nested within this [ActionToken].
    pub fn bind_positional<E, F>(&mut self, f: &mut F) -> Result<(), E>
    where
        F: FnMut() -> Result<String, E>,
    {
        match self {
            Self::Bool(..) |
            Self::Char(..) |
            Self::Flag(..) |
            Self::Id(Some(..)) |
            Self::Number(..) |
            Self::Str(..) |
            Self::Word(..) => Ok(()),

            Self::Id(id @ None) => {
                *id = Some(Cow::Owned(f()?));
                Ok(())
            },

            Self::Group(group) => {
                for token in group.iter_mut() {
                    token.bind_positional(f)?;
                }
                Ok(())
            },
        }
    }
}

impl std::fmt::Display for ActionToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Word(w) => write!(f, "{w}"),
            Self::Flag(flag) => write!(f, "{flag}"),
            Self::Str(s) => write!(f, "{s:?}"),
            Self::Id(None) => write!(f, "{{}}"),
            Self::Id(Some(id)) => write!(f, "{{{id}}}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Char(c) => write!(f, "{c:?}"),
            Self::Group(group) => {
                write!(f, "(")?;
                for (i, token) in group.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{token}")?;
                    } else {
                        write!(f, " {token}")?;
                    }
                }
                write!(f, ")")
            },
        }
    }
}

pub trait ActionParser {
    type Output;

    /// Output an error for the current parse.
    fn fail<T: std::fmt::Display>(&self, msg: T) -> Self::Output;

    /// Parse `keyword-lookup`.
    fn visit_keyword_lookup(&mut self, target: &[ActionToken]) -> Self::Output;

    /// Parse `noop`.
    fn visit_noop(&mut self) -> Self::Output;

    /// Parse `redraw-screen`.
    fn visit_redraw_screen(&mut self) -> Self::Output;

    /// Parse `suspend`.
    fn visit_suspend(&mut self) -> Self::Output;

    /// Parse `cmdbar focus`.
    fn visit_cmdbar_focus(
        &mut self,
        prompt: &[ActionToken],
        cmdtype: &[ActionToken],
        action: &[ActionToken],
    ) -> Self::Output;

    /// Parse `cmdbar unfocus`.
    fn visit_cmdbar_unfocus(&mut self) -> Self::Output;

    /// Parse `command execute` and its arguments.
    fn visit_command_execute(&mut self, count: &[ActionToken]) -> Self::Output;

    /// Parse `command run` and its arguments.
    fn visit_command_run(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse `complete` and its arguments.
    fn visit_complete(
        &mut self,
        style: &[ActionToken],
        comptype: &[ActionToken],
        display: &[ActionToken],
    ) -> Self::Output;

    /// Parse `edit`.
    fn visit_edit(&mut self, action: &[ActionToken], target: &[ActionToken]) -> Self::Output;

    /// Parse `history checkpoint`.
    fn visit_history_checkpoint(&mut self) -> Self::Output;

    /// Parse `history undo` and its arguments.
    fn visit_history_undo(&mut self, count: &[ActionToken]) -> Self::Output;

    /// Parse `history redo` and its arguments.
    fn visit_history_redo(&mut self, count: &[ActionToken]) -> Self::Output;

    /// Parse `macro execute` and its arguments.
    fn visit_macro_execute(&mut self, count: &[ActionToken]) -> Self::Output;

    /// Parse `macro run` and its arguments.
    fn visit_macro_run(&mut self, input: &[ActionToken], count: &[ActionToken]) -> Self::Output;

    /// Parse `macro repeat` and its arguments.
    fn visit_macro_repeat(&mut self, count: &[ActionToken]) -> Self::Output;

    /// Parse `macro toggle-recording`.
    fn visit_macro_toggle_recording(&mut self) -> Self::Output;

    /// Parse `prompt abort` and its arguments.
    fn visit_prompt_abort(&mut self, empty: &[ActionToken]) -> Self::Output;

    /// Parse `prompt recall` and its arguments.
    fn visit_prompt_recall(
        &mut self,
        filter: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `prompt submit`.
    fn visit_prompt_submit(&mut self) -> Self::Output;

    /// Parse `mark` and its arguments.
    fn visit_mark(&mut self, mark: &[ActionToken]) -> Self::Output;

    /// Parse `cursor close` and its arguments.
    fn visit_cursor_close(&mut self, target: &[ActionToken]) -> Self::Output;

    /// Parse `cursor restore` and its arguments.
    fn visit_cursor_restore(&mut self, style: &[ActionToken]) -> Self::Output;

    /// Parse `cursor rotate` and its arguments.
    fn visit_cursor_rotate(&mut self, dir: &[ActionToken], count: &[ActionToken]) -> Self::Output;

    /// Parse `cursor save` and its arguments.
    fn visit_cursor_save(&mut self, style: &[ActionToken]) -> Self::Output;

    /// Parse `cursor split` and its arguments.
    fn visit_cursor_split(&mut self, count: &[ActionToken]) -> Self::Output;

    /// Parse `tab close` and its arguments.
    fn visit_tab_close(&mut self, target: &[ActionToken], flags: &[ActionToken]) -> Self::Output;

    /// Parse `tab extract` and its arguments.
    fn visit_tab_extract(&mut self, fc: &[ActionToken], dir: &[ActionToken]) -> Self::Output;

    /// Parse `tab open` and its arguments.
    fn visit_tab_open(&mut self, target: &[ActionToken], fc: &[ActionToken]) -> Self::Output;

    /// Parse `tab focus` and its arguments.
    fn visit_tab_focus(&mut self, fc: &[ActionToken]) -> Self::Output;

    /// Parse `tab move` and its arguments.
    fn visit_tab_move(&mut self, fc: &[ActionToken]) -> Self::Output;

    /// Parse `window close` and its arguments.
    fn visit_window_close(&mut self, target: &[ActionToken], flags: &[ActionToken])
    -> Self::Output;

    /// Parse `window open` and its arguments.
    fn visit_window_open(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `window resize` and its arguments.
    fn visit_window_resize(
        &mut self,
        fc: &[ActionToken],
        axis: &[ActionToken],
        size: &[ActionToken],
    ) -> Self::Output;

    /// Parse `window split` and its arguments.
    fn visit_window_split(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `window switch` and its arguments.
    fn visit_window_switch(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse `window write` and its arguments.
    fn visit_window_write(
        &mut self,
        target: &[ActionToken],
        name: &[ActionToken],
        flags: &[ActionToken],
    ) -> Self::Output;

    /// Parse `window exchange` and its arguments.
    fn visit_window_exchange(&mut self, fc: &[ActionToken]) -> Self::Output;

    /// Parse `window focus` and its arguments.
    fn visit_window_focus(&mut self, fc: &[ActionToken]) -> Self::Output;

    /// Parse `window move-side` and its arguments.
    fn visit_window_move_side(&mut self, dir: &[ActionToken]) -> Self::Output;

    /// Parse `window rotate` and its arguments.
    fn visit_window_rotate(&mut self, dir: &[ActionToken]) -> Self::Output;

    /// Parse `window clear-sizes` and its arguments.
    fn visit_window_clear_sizes(&mut self) -> Self::Output;

    /// Parse `window zoom-toggle` and its arguments.
    fn visit_window_zoom_toggle(&mut self) -> Self::Output;

    /// Parse `insert open-line` and its arguments.
    fn visit_insert_open_line(
        &mut self,
        shape: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `insert transcribe` and its arguments.
    fn visit_insert_transcribe(
        &mut self,
        input: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `insert type` and its arguments.
    fn visit_insert_type(
        &mut self,
        c: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `insert paste` and its arguments.
    fn visit_insert_paste(&mut self, style: &[ActionToken], count: &[ActionToken]) -> Self::Output;

    /// Parse `jump` and its arguments.
    fn visit_jump(
        &mut self,
        list: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `repeat` and its arguments.
    fn visit_repeat(&mut self, style: &[ActionToken]) -> Self::Output;

    /// Parse `scroll` and its arguments.
    fn visit_scroll(&mut self, style: &[ActionToken]) -> Self::Output;

    /// Parse `search` and its arguments.
    fn visit_search(&mut self, dir: &[ActionToken], count: &[ActionToken]) -> Self::Output;

    /// Parse `selection duplicate` and its arguments.
    fn visit_selection_duplicate(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    /// Parse `selection join` and its arguments.
    fn visit_selection_join(&mut self) -> Self::Output;

    /// Parse `selection cursor-set` and its arguments.
    fn visit_selection_cursor_set(&mut self, change: &[ActionToken]) -> Self::Output;

    /// Parse `selection expand` and its arguments.
    fn visit_selection_expand(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output;

    /// Parse `selection filter` and its arguments.
    fn visit_selection_filter(&mut self, drop: &[ActionToken]) -> Self::Output;

    /// Parse `selection resize` and its arguments.
    fn visit_selection_resize(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output;

    /// Parse `selection split` and its arguments.
    fn visit_selection_split(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output;

    /// Parse `selection trim` and its arguments.
    fn visit_selection_trim(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output;
}

/// Parse a series of ActionTokens into `editor_types::prelude::RangeType`.
pub trait RangeParser {
    type Output;

    /// Output an error for the current parse.
    fn range_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output;

    fn visit_word(&mut self, style: &[ActionToken]) -> Self::Output;

    fn visit_buffer(&mut self) -> Self::Output;

    fn visit_paragraph(&mut self) -> Self::Output;

    fn visit_sentence(&mut self) -> Self::Output;

    fn visit_line(&mut self) -> Self::Output;

    fn visit_bracketed(&mut self, left: &[ActionToken], right: &[ActionToken]) -> Self::Output;

    fn visit_item(&mut self) -> Self::Output;

    fn visit_quote(&mut self, surround: &[ActionToken]) -> Self::Output;

    fn visit_xml_tag(&mut self) -> Self::Output;
}

pub fn parse_single_flag<'a>(
    flag: Flag,
    input: &'a [ActionToken<'a>],
) -> Result<&'a [ActionToken<'a>], ArgError> {
    let mut arg = None;

    for (f, act) in flag_pairs(input)? {
        if f != &flag {
            return Err(ArgError::UnexpectedFlag(f.clone()));
        }

        arg = Some(act);
    }

    arg.ok_or(ArgError::MissingArg(flag))
}

pub fn parse_flag<'a>(
    flag: Flag,
    input: &'a [ActionToken<'a>],
) -> Result<Option<&'a [ActionToken<'a>]>, ArgError> {
    let mut arg = None;

    for (f, act) in flag_pairs(input)? {
        if f != &flag {
            return Err(ArgError::UnexpectedFlag(f.clone()));
        }

        arg = Some(act);
    }

    Ok(arg)
}

pub fn parse_flags<'a, const N: usize>(
    flags: [(Flag, Option<&'a [ActionToken<'a>]>); N],
    input: &'a [ActionToken<'a>],
) -> Result<[&'a [ActionToken<'a>]; N], ArgError> {
    let mut output = [EMPTY_ACTION.as_slice(); N];
    let pairs = flag_pairs(input)?;

    for (f, _) in &pairs {
        if !flags.iter().any(|(flag, _)| f == &flag) {
            return Err(ArgError::UnexpectedFlag((*f).clone()));
        }
    }

    let iter = flags.into_iter().enumerate();

    for (i, (flag, default)) in iter {
        let mut matches = pairs.iter().filter(|(f, _)| f == &&flag);

        if let Some((_, act)) = matches.next() {
            output[i] = act;
        } else if let Some(default) = default {
            output[i] = default;
        } else {
            return Err(ArgError::ExpectedFlag(flag));
        }
    }

    Ok(output)
}

pub fn parse_required_flags<'a, const N: usize>(
    flags: [Flag; N],
    input: &'a [ActionToken<'a>],
) -> Result<[&'a [ActionToken<'a>]; N], ArgError> {
    let mut output = [EMPTY_ACTION.as_slice(); N];
    let pairs = flag_pairs(input)?;

    for (f, _) in &pairs {
        if !flags.contains(f) {
            return Err(ArgError::UnexpectedFlag((*f).clone()));
        }
    }

    for (i, flag) in flags.into_iter().enumerate() {
        let mut matches = pairs.iter().filter(|(f, _)| f == &&flag);

        if let Some((_, act)) = matches.next() {
            output[i] = act;
        } else {
            return Err(ArgError::ExpectedFlag(flag));
        }
    }

    Ok(output)
}

pub fn parse_single_count<'a>(
    input: &'a [ActionToken<'a>],
) -> Result<&'a [ActionToken<'a>], ArgError> {
    let count = parse_flag(Flag::Count, input)?;
    let count = count.unwrap_or(&DEFAULT_COUNT[..]);
    Ok(count)
}

fn fail_cmd_flag_msg(cmd: &str, err: ArgError) -> String {
    err.display(cmd).to_string()
}

fn fail_cmd_flag<V: ActionParser>(v: &V, cmd: &str, err: ArgError) -> V::Output {
    v.fail(fail_cmd_flag_msg(cmd, err))
}

pub trait ActionParserExt: ActionParser {
    /// Parse an action.
    fn parse_action(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `cmdbar` action.
    fn parse_action_cmdbar(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `command` action.
    fn parse_action_command(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `complete` action.
    fn parse_action_complete(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `cursor` action.
    fn parse_action_cursor(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse an `edit` action.
    fn parse_action_edit(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `history` action.
    fn parse_action_history(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse an `insert` action.
    fn parse_action_insert(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `jump` action.
    fn parse_action_jump(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `macro` action.
    fn parse_action_macro(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `mark` action.
    fn parse_action_mark(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `prompt` action.
    fn parse_action_prompt(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `search` action.
    fn parse_action_search(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `selection` action.
    fn parse_action_selection(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `scroll` action.
    fn parse_action_scroll(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `repeat` action.
    fn parse_action_repeat(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `tab` action.
    fn parse_action_tab(&mut self, input: &[ActionToken]) -> Self::Output;

    /// Parse a `window` action.
    fn parse_action_window(&mut self, input: &[ActionToken]) -> Self::Output;
}

impl<V: ActionParser> ActionParserExt for V {
    fn parse_action_cmdbar(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No cmdbar action specified");
        };

        match cmd {
            ActionToken::Word("focus") => {
                match parse_flags(
                    [
                        (Flag::Short('P'), None),
                        (Flag::Style, None),
                        (Flag::Short('a'), None),
                    ],
                    rest,
                ) {
                    Ok([prompt, cmdtype, action]) => {
                        self.visit_cmdbar_focus(prompt, cmdtype, action)
                    },
                    Err(e) => fail_cmd_flag(self, "cmdbar focus", e),
                }
            },
            ActionToken::Word("unfocus") => {
                if rest.is_empty() {
                    self.visit_cmdbar_unfocus()
                } else {
                    self.fail("`cmdbar unfocus` takes no arguments")
                }
            },
            ActionToken::Word(w) => self.fail(format!("`cmdbar {w}` is not a valid action")),
            _ => self.fail("expected a command bar action after `cmdbar`"),
        }
    }

    fn parse_action_command(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No command action specified");
        };

        match cmd {
            ActionToken::Word("execute" | "exec") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_command_execute(count),
                    Err(e) => fail_cmd_flag(self, "command execute", e),
                }
            },
            ActionToken::Word("run") => {
                match parse_flags([(Flag::Input, None)], rest) {
                    Ok([input]) => self.visit_command_run(input),
                    Err(e) => fail_cmd_flag(self, "command run", e),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`command {w}` is not a valid action")),
            _ => self.fail("expected a command action after `command`"),
        }
    }

    fn parse_action_complete(&mut self, input: &[ActionToken]) -> Self::Output {
        match parse_flags(
            [
                (Flag::Style, None),
                (Flag::Short('T'), Some(&DEFAULT_COMPTYPE[..])),
                (Flag::Short('D'), None),
            ],
            input,
        ) {
            Ok([style, comptype, display]) => self.visit_complete(style, comptype, display),
            Err(e) => fail_cmd_flag(self, "complete", e),
        }
    }

    fn parse_action_edit(&mut self, input: &[ActionToken]) -> Self::Output {
        match parse_flags(
            [
                (Flag::Short('o'), Some(&DEFAULT_OP[..])),
                (Flag::Target, None),
            ],
            input,
        ) {
            Ok([action, target]) => self.visit_edit(action, target),
            Err(e) => fail_cmd_flag(self, "edit", e),
        }
    }

    fn parse_action_history(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No history action specified");
        };

        match cmd {
            ActionToken::Word("checkpoint") => {
                if rest.is_empty() {
                    self.visit_history_checkpoint()
                } else {
                    self.fail("`history checkpoint` takes no arguments")
                }
            },
            ActionToken::Word("redo") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_history_redo(count),
                    Err(e) => fail_cmd_flag(self, "history redo", e),
                }
            },
            ActionToken::Word("undo") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_history_undo(count),
                    Err(e) => fail_cmd_flag(self, "history undo", e),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`history {w}` is not a valid action")),
            _ => self.fail("expected a history action after `history`"),
        }
    }

    fn parse_action_macro(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No macro action specified");
        };

        match cmd {
            ActionToken::Word("execute" | "exec") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_macro_execute(count),
                    Err(e) => fail_cmd_flag(self, "macro execute", e),
                }
            },
            ActionToken::Word("run") => {
                match parse_flags(
                    [(Flag::Input, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([input, count]) => self.visit_macro_run(input, count),
                    Err(e) => fail_cmd_flag(self, "macro run", e),
                }
            },
            ActionToken::Word("repeat") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_macro_repeat(count),
                    Err(e) => fail_cmd_flag(self, "macro repeat", e),
                }
            },
            ActionToken::Word("toggle-recording") => {
                if rest.is_empty() {
                    self.visit_macro_toggle_recording()
                } else {
                    self.fail("`macro toggle-recording` takes no arguments")
                }
            },
            ActionToken::Word(w) => self.fail(format!("`macro {w}` is not a valid action")),
            _ => self.fail("expected a macro action after `macro`"),
        }
    }

    fn parse_action_mark(&mut self, input: &[ActionToken]) -> Self::Output {
        match parse_flags([(Flag::Mark, Some(&DEFAULT_MARK[..]))], input) {
            Ok([mark]) => self.visit_mark(mark),
            Err(e) => fail_cmd_flag(self, "mark", e),
        }
    }

    fn parse_action_prompt(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No prompt action specified");
        };

        match cmd {
            ActionToken::Word("abort") => {
                match parse_flags([(Flag::Long("empty".into()), Some(&DEFAULT_FALSE[..]))], rest) {
                    Ok([empty]) => self.visit_prompt_abort(empty),
                    Err(e) => fail_cmd_flag(self, "prompt abort", e),
                }
            },
            ActionToken::Word("recall") => {
                match parse_flags(
                    [
                        (Flag::Short('F'), Some(&DEFAULT_FILTER[..])),
                        (Flag::Dir, None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([filter, dir, count]) => self.visit_prompt_recall(filter, dir, count),
                    Err(e) => fail_cmd_flag(self, "prompt recall", e),
                }
            },
            ActionToken::Word("submit") => {
                if rest.is_empty() {
                    self.visit_prompt_submit()
                } else {
                    self.fail("`prompt submit` takes no arguments")
                }
            },
            ActionToken::Word(w) => self.fail(format!("`prompt {w}` is not a valid action")),
            _ => self.fail("expected a prompt action after `prompt`"),
        }
    }

    fn parse_action_cursor(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No cursor action specified");
        };

        match cmd {
            ActionToken::Word("close") => {
                match parse_single_flag(Flag::Target, rest) {
                    Ok(target) => self.visit_cursor_close(target),
                    Err(e) => fail_cmd_flag(self, "cursor close", e),
                }
            },
            ActionToken::Word("restore") => {
                match parse_single_flag(Flag::Style, rest) {
                    Ok(style) => self.visit_cursor_restore(style),
                    Err(e) => fail_cmd_flag(self, "cursor restore", e),
                }
            },
            ActionToken::Word("rotate") => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => self.visit_cursor_rotate(dir, count),
                    Err(e) => fail_cmd_flag(self, "cursor rotate", e),
                }
            },
            ActionToken::Word("save") => {
                match parse_single_flag(Flag::Style, rest) {
                    Ok(style) => self.visit_cursor_save(style),
                    Err(e) => fail_cmd_flag(self, "cursor save", e),
                }
            },
            ActionToken::Word("split") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_cursor_split(count),
                    Err(e) => fail_cmd_flag(self, "cursor split", e),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`cursor {w}` is not a valid action")),
            _ => self.fail("expected a cursor action after `cursor`"),
        }
    }

    fn parse_action_tab(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No tab action specified");
        };

        match cmd {
            ActionToken::Word("close") => {
                match parse_required_flags([Flag::Target, Flag::Short('F')], rest) {
                    Ok([target, flags]) => self.visit_tab_close(target, flags),
                    Err(e) => fail_cmd_flag(self, "tab close", e),
                }
            },
            ActionToken::Word("extract") => {
                match parse_required_flags([Flag::Focus, Flag::Dir], rest) {
                    Ok([fc, dir]) => self.visit_tab_extract(fc, dir),
                    Err(e) => fail_cmd_flag(self, "tab extract", e),
                }
            },
            ActionToken::Word("focus") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_tab_focus(fc),
                    Err(e) => fail_cmd_flag(self, "tab focus", e),
                }
            },
            ActionToken::Word("move") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_tab_move(fc),
                    Err(e) => fail_cmd_flag(self, "tab move", e),
                }
            },
            ActionToken::Word("open") => {
                match parse_required_flags([Flag::Target, Flag::Focus], rest) {
                    Ok([target, fc]) => self.visit_tab_open(target, fc),
                    Err(e) => fail_cmd_flag(self, "tab open", e),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`tab {w}` is not a valid action")),
            _ => self.fail("expected a tab action after `tab`"),
        }
    }

    fn parse_action_window(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No window action specified");
        };

        match cmd {
            ActionToken::Word("close") => {
                match parse_required_flags([Flag::Target, Flag::Short('F')], rest) {
                    Ok([target, flags]) => self.visit_window_close(target, flags),
                    Err(e) => fail_cmd_flag(self, "window close", e),
                }
            },
            ActionToken::Word("exchange") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_window_exchange(fc),
                    Err(e) => fail_cmd_flag(self, "window exchange", e),
                }
            },
            ActionToken::Word("focus") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_window_focus(fc),
                    Err(e) => fail_cmd_flag(self, "window focus", e),
                }
            },
            ActionToken::Word("move-side") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(fc) => self.visit_window_move_side(fc),
                    Err(e) => fail_cmd_flag(self, "window move-side", e),
                }
            },
            ActionToken::Word("open") => {
                match parse_flags(
                    [
                        (Flag::Target, None),
                        (Flag::Short('x'), None),
                        (Flag::Dir, None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([target, axis, dir, count]) => {
                        self.visit_window_open(target, axis, dir, count)
                    },
                    Err(e) => fail_cmd_flag(self, "window open", e),
                }
            },
            ActionToken::Word("rotate") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_window_rotate(dir),
                    Err(e) => fail_cmd_flag(self, "window rotate", e),
                }
            },
            ActionToken::Word("split") => {
                match parse_flags(
                    [
                        (Flag::Target, None),
                        (Flag::Short('x'), None),
                        (Flag::Dir, None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([target, axis, dir, count]) => {
                        self.visit_window_split(target, axis, dir, count)
                    },
                    Err(e) => fail_cmd_flag(self, "window split", e),
                }
            },
            ActionToken::Word("switch") => {
                match parse_single_flag(Flag::Target, rest) {
                    Ok(target) => self.visit_window_switch(target),
                    Err(e) => fail_cmd_flag(self, "window switch", e),
                }
            },
            ActionToken::Word("resize") => {
                match parse_required_flags([Flag::Focus, Flag::Short('x'), Flag::Short('z')], rest)
                {
                    Ok([fc, axis, size]) => self.visit_window_resize(fc, axis, size),
                    Err(e) => fail_cmd_flag(self, "window resize", e),
                }
            },
            ActionToken::Word("write") => {
                match parse_flags(
                    [
                        (Flag::Target, None),
                        (Flag::Input, Some(&EMPTY_ACTION[..])),
                        (Flag::Short('F'), None),
                    ],
                    rest,
                ) {
                    Ok([target, name, flags]) => self.visit_window_write(target, name, flags),
                    Err(e) => fail_cmd_flag(self, "window write", e),
                }
            },
            ActionToken::Word("clear-sizes") => {
                if rest.is_empty() {
                    self.visit_window_clear_sizes()
                } else {
                    self.fail("`window clear-sizes` takes no arguments")
                }
            },
            ActionToken::Word("zoom-toggle") => {
                if rest.is_empty() {
                    self.visit_window_zoom_toggle()
                } else {
                    self.fail("`window zoom-toggle` takes no arguments")
                }
            },
            ActionToken::Word(w) => self.fail(format!("`window {w}` is not a valid action")),
            _ => self.fail("expected a window action after `window`"),
        }
    }

    fn parse_action_insert(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No insert action specified");
        };

        match cmd {
            ActionToken::Word("open-line") => {
                match parse_flags(
                    [
                        (Flag::Short('S'), None),
                        (Flag::Dir, None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([shape, dir, count]) => self.visit_insert_open_line(shape, dir, count),
                    Err(e) => fail_cmd_flag(self, "insert open-line", e),
                }
            },
            ActionToken::Word("paste") => {
                match parse_flags(
                    [(Flag::Style, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([style, count]) => self.visit_insert_paste(style, count),
                    Err(e) => fail_cmd_flag(self, "insert paste", e),
                }
            },
            ActionToken::Word("transcribe") => {
                match parse_flags(
                    [
                        (Flag::Input, None),
                        (Flag::Dir, None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([s, dir, count]) => self.visit_insert_transcribe(s, dir, count),
                    Err(e) => fail_cmd_flag(self, "insert transcribe", e),
                }
            },
            ActionToken::Word("type") => {
                match parse_flags(
                    [
                        (Flag::Input, None),
                        (Flag::Dir, Some(&DEFAULT_PREV[..])),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([c, dir, count]) => self.visit_insert_type(c, dir, count),
                    Err(e) => fail_cmd_flag(self, "insert type", e),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`insert {w}` is not a valid action")),
            _ => self.fail("Expected an action after `insert`"),
        }
    }

    fn parse_action_jump(&mut self, input: &[ActionToken]) -> Self::Output {
        match parse_flags(
            [
                (Flag::Target, None),
                (Flag::Dir, None),
                (Flag::Count, Some(&DEFAULT_COUNT[..])),
            ],
            input,
        ) {
            Ok([list, dir, count]) => self.visit_jump(list, dir, count),
            Err(e) => fail_cmd_flag(self, "jump", e),
        }
    }

    fn parse_action_search(&mut self, input: &[ActionToken]) -> Self::Output {
        match parse_flags([(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))], input) {
            Ok([dir, count]) => self.visit_search(dir, count),
            Err(e) => fail_cmd_flag(self, "search", e),
        }
    }

    fn parse_action_selection(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No selection action specified");
        };

        match cmd {
            ActionToken::Word("duplicate") => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => self.visit_selection_duplicate(dir, count),
                    Err(e) => fail_cmd_flag(self, "selection duplicate", e),
                }
            },
            ActionToken::Word("cursor-set") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(change) => self.visit_selection_cursor_set(change),
                    Err(e) => fail_cmd_flag(self, "selection cursor-set", e),
                }
            },
            ActionToken::Word("expand") => {
                match parse_flags(
                    [
                        (Flag::Short('b'), None),
                        (Flag::Target, Some(&DEFAULT_FILTER[..])),
                    ],
                    rest,
                ) {
                    Ok([boundary, target]) => self.visit_selection_expand(boundary, target),
                    Err(e) => fail_cmd_flag(self, "selection expand", e),
                }
            },
            ActionToken::Word("filter") => {
                match parse_single_flag(Flag::Short('F'), rest) {
                    Ok(filter) => self.visit_selection_filter(filter),
                    Err(e) => fail_cmd_flag(self, "selection filter", e),
                }
            },
            ActionToken::Word("join") => {
                if rest.is_empty() {
                    self.visit_selection_join()
                } else {
                    self.fail("`selection join` takes no arguments")
                }
            },
            ActionToken::Word("resize") => {
                match parse_required_flags([Flag::Style, Flag::Target], rest) {
                    Ok([style, target]) => self.visit_selection_resize(style, target),
                    Err(e) => fail_cmd_flag(self, "selection resize", e),
                }
            },
            ActionToken::Word("split") => {
                match parse_flags(
                    [
                        (Flag::Style, None),
                        (Flag::Short('F'), Some(&DEFAULT_FILTER[..])),
                    ],
                    rest,
                ) {
                    Ok([style, filter]) => self.visit_selection_split(style, filter),
                    Err(e) => fail_cmd_flag(self, "selection split", e),
                }
            },
            ActionToken::Word("trim") => {
                match parse_flags(
                    [
                        (Flag::Short('b'), None),
                        (Flag::Target, Some(&DEFAULT_FILTER[..])),
                    ],
                    rest,
                ) {
                    Ok([boundary, target]) => self.visit_selection_trim(boundary, target),
                    Err(e) => fail_cmd_flag(self, "selection trim", e),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`selection {w}` is not a valid action")),
            _ => self.fail("expected a selection action after `selection`"),
        }
    }

    fn parse_action_scroll(&mut self, input: &[ActionToken]) -> Self::Output {
        match parse_single_flag(Flag::Style, input) {
            Ok(style) => self.visit_scroll(style),
            Err(e) => fail_cmd_flag(self, "scroll", e),
        }
    }

    fn parse_action_repeat(&mut self, input: &[ActionToken]) -> Self::Output {
        match parse_single_flag(Flag::Style, input) {
            Ok(style) => self.visit_repeat(style),
            Err(e) => fail_cmd_flag(self, "repeat", e),
        }
    }

    fn parse_action(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No action specified");
        };

        match cmd {
            ActionToken::Word("cmdbar") => self.parse_action_cmdbar(rest),
            ActionToken::Word("command") => self.parse_action_command(rest),
            ActionToken::Word("complete") => self.parse_action_complete(rest),
            ActionToken::Word("cursor") => self.parse_action_cursor(rest),
            ActionToken::Word("edit") => self.parse_action_edit(rest),
            ActionToken::Word("history") => self.parse_action_history(rest),
            ActionToken::Word("insert") => self.parse_action_insert(rest),
            ActionToken::Word("jump") => self.parse_action_jump(rest),
            ActionToken::Word("macro") => self.parse_action_macro(rest),
            ActionToken::Word("mark") => self.parse_action_mark(rest),
            ActionToken::Word("prompt") => self.parse_action_prompt(rest),
            ActionToken::Word("repeat") => self.parse_action_repeat(rest),
            ActionToken::Word("search") => self.parse_action_search(rest),
            ActionToken::Word("selection") => self.parse_action_selection(rest),
            ActionToken::Word("scroll") => self.parse_action_scroll(rest),
            ActionToken::Word("tab") => self.parse_action_tab(rest),
            ActionToken::Word("window") => self.parse_action_window(rest),
            ActionToken::Word(w @ ("kw-lookup" | "keyword-lookup")) => {
                match parse_single_flag(Flag::Target, rest) {
                    Ok(target) => self.visit_keyword_lookup(target),
                    Err(e) => fail_cmd_flag(self, w, e),
                }
            },
            ActionToken::Word(w @ ("nop" | "noop" | "no-op")) => {
                if rest.is_empty() {
                    self.visit_noop()
                } else {
                    self.fail(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "redraw-screen") => {
                if rest.is_empty() {
                    self.visit_redraw_screen()
                } else {
                    self.fail(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word("suspend") => {
                if rest.is_empty() {
                    self.visit_suspend()
                } else {
                    self.fail("`suspend` takes no arguments")
                }
            },

            ActionToken::Word(w) => self.fail(format!("unknown action keyword `{w}`")),

            _ => self.fail("expect action keyword at start of command"),
        }
    }
}

pub trait RangeParserExt: RangeParser {
    fn parse_tokens(&mut self, input: &[ActionToken]) -> Self::Output;
}

impl<V: RangeParser> RangeParserExt for V {
    fn parse_tokens(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((range, rest)) = input.split_first() else {
            return self.range_invalid("No range specified");
        };

        match range {
            ActionToken::Word(w @ "word") => {
                match parse_single_flag(Flag::Style, rest) {
                    Ok(style) => self.visit_word(style),
                    Err(e) => self.range_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "buffer") => {
                if rest.is_empty() {
                    self.visit_buffer()
                } else {
                    self.range_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "paragraph") => {
                if rest.is_empty() {
                    self.visit_paragraph()
                } else {
                    self.range_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "sentence") => {
                if rest.is_empty() {
                    self.visit_sentence()
                } else {
                    self.range_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "line") => {
                if rest.is_empty() {
                    self.visit_line()
                } else {
                    self.range_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "bracketed") => {
                match parse_required_flags(
                    [Flag::Long("left".into()), Flag::Long("right".into())],
                    rest,
                ) {
                    Ok([left, right]) => self.visit_bracketed(left, right),
                    Err(e) => self.range_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "item") => {
                if rest.is_empty() {
                    self.visit_item()
                } else {
                    self.range_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "quote") => {
                if rest.len() == 1 {
                    self.visit_quote(rest)
                } else {
                    self.range_invalid(format!("`{w}` expected a single argument"))
                }
            },
            ActionToken::Word(w @ "xml-tag") => {
                if rest.is_empty() {
                    self.visit_xml_tag()
                } else {
                    self.range_invalid(format!("`{w}` takes no arguments"))
                }
            },

            t => self.range_invalid(format!("expected the name of a range type, found `{t}`")),
        }
    }
}

/// Parse a series of ActionTokens into `editor_types::prelude::MoveType`.
pub trait MotionParser {
    type Output;

    /// Output an error for the current parse.
    fn motion_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output;

    fn visit_buffer_pos(&mut self, position: &[ActionToken]) -> Self::Output;

    fn visit_buffer_byte_offset(&mut self) -> Self::Output;

    fn visit_buffer_line_offset(&mut self) -> Self::Output;

    fn visit_buffer_line_percent(&mut self) -> Self::Output;

    fn visit_column(&mut self, dir: &[ActionToken], multiline: &[ActionToken]) -> Self::Output;

    fn visit_final_non_blank(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_first_word(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_item_match(&mut self) -> Self::Output;

    fn visit_line(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_line_column_offset(&mut self) -> Self::Output;

    fn visit_line_percent(&mut self) -> Self::Output;

    fn visit_line_pos(&mut self, position: &[ActionToken]) -> Self::Output;

    fn visit_word_begin(&mut self, style: &[ActionToken], dir: &[ActionToken]) -> Self::Output;

    fn visit_word_end(&mut self, style: &[ActionToken], dir: &[ActionToken]) -> Self::Output;

    fn visit_paragraph_begin(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_sentence_begin(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_section_begin(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_section_end(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_screen_first_word(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_screen_line(&mut self, dir: &[ActionToken]) -> Self::Output;

    fn visit_screen_line_pos(&mut self, position: &[ActionToken]) -> Self::Output;

    fn visit_viewport_pos(&mut self, position: &[ActionToken]) -> Self::Output;
}

pub trait MotionParserExt: MotionParser {
    fn parse_tokens(&mut self, input: &[ActionToken]) -> Self::Output;
}

impl<V: MotionParser> MotionParserExt for V {
    fn parse_tokens(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((motion, rest)) = input.split_first() else {
            return self.motion_invalid("No motion specified");
        };

        match motion {
            ActionToken::Word(w @ "buffer-pos") => {
                match parse_single_flag(Flag::Position, rest) {
                    Ok(pos) => self.visit_buffer_pos(pos),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "buffer-byte-offset") => {
                if rest.is_empty() {
                    self.visit_buffer_byte_offset()
                } else {
                    self.motion_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "buffer-line-offset") => {
                if rest.is_empty() {
                    self.visit_buffer_line_offset()
                } else {
                    self.motion_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "buffer-line-percent") => {
                if rest.is_empty() {
                    self.visit_buffer_line_percent()
                } else {
                    self.motion_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "column") => {
                match parse_flags(
                    [
                        (Flag::Dir, None),
                        (Flag::Long("multiline".into()), Some(&DEFAULT_TRUE)),
                    ],
                    rest,
                ) {
                    Ok([dir, multiline]) => self.visit_column(dir, multiline),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "final-non-blank") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_final_non_blank(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "first-word") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_first_word(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "item-match") => {
                if rest.is_empty() {
                    self.visit_item_match()
                } else {
                    self.motion_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "line") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_line(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "line-column-offset") => {
                if rest.is_empty() {
                    self.visit_line_column_offset()
                } else {
                    self.motion_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "line-percent") => {
                if rest.is_empty() {
                    self.visit_line_percent()
                } else {
                    self.motion_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "line-pos") => {
                match parse_single_flag(Flag::Position, rest) {
                    Ok(pos) => self.visit_line_pos(pos),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "word-begin") => {
                match parse_required_flags([Flag::Style, Flag::Dir], rest) {
                    Ok([style, dir]) => self.visit_word_begin(style, dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "word-end") => {
                match parse_required_flags([Flag::Style, Flag::Dir], rest) {
                    Ok([style, dir]) => self.visit_word_end(style, dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "paragraph-begin") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_paragraph_begin(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "sentence-begin") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_sentence_begin(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "section-begin") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_section_begin(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "section-end") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_section_end(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "screen-first-word") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_screen_first_word(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "screen-line") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_screen_line(dir),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "screen-line-pos") => {
                match parse_single_flag(Flag::Position, rest) {
                    Ok(pos) => self.visit_screen_line_pos(pos),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "viewport-pos") => {
                match parse_single_flag(Flag::Position, rest) {
                    Ok(pos) => self.visit_viewport_pos(pos),
                    Err(e) => self.motion_invalid(fail_cmd_flag_msg(w, e)),
                }
            },

            ActionToken::Word(w) => {
                self.motion_invalid(format!("expected the name of a motion type, found `{w}`"))
            },

            t => self.motion_invalid(format!("expected the name of a motion type, found `{t}`")),
        }
    }
}

/// Parse a series of ActionTokens into `editor_types::prelude::EditTarget`.
pub trait EditTargetParser {
    type Output;

    /// Output an error for the current parse.
    fn edit_target_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output;

    fn visit_boundary(
        &mut self,
        range: &[ActionToken],
        inclusive: &[ActionToken],
        terminus: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    fn visit_current_position(&mut self) -> Self::Output;

    fn visit_char_jump(&mut self, mark: &[ActionToken]) -> Self::Output;

    fn visit_line_jump(&mut self, mark: &[ActionToken]) -> Self::Output;

    fn visit_motion(&mut self, motion: &[ActionToken], count: &[ActionToken]) -> Self::Output;

    fn visit_range(
        &mut self,
        range: &[ActionToken],
        inclusive: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    fn visit_search(
        &mut self,
        search: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output;

    fn visit_selection(&mut self) -> Self::Output;
}

pub trait EditTargetParserExt: EditTargetParser {
    fn parse_tokens(&mut self, input: &[ActionToken]) -> Self::Output;
}

impl<V: EditTargetParser> EditTargetParserExt for V {
    fn parse_tokens(&mut self, input: &[ActionToken]) -> Self::Output {
        let Some((target, rest)) = input.split_first() else {
            return self.edit_target_invalid("No edit target specified");
        };

        match target {
            ActionToken::Word(w @ "boundary") => {
                match parse_flags(
                    [
                        (Flag::Short('T'), None),
                        (Flag::Long("inclusive".into()), None),
                        (Flag::Position, None),
                        (Flag::Count, Some(&DEFAULT_COUNT)),
                    ],
                    rest,
                ) {
                    Ok([t, inclusive, terminus, count]) => {
                        self.visit_boundary(t, inclusive, terminus, count)
                    },
                    Err(e) => self.edit_target_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ ("current-position" | "curr-pos")) => {
                if rest.is_empty() {
                    self.visit_current_position()
                } else {
                    self.edit_target_invalid(format!("`{w}` takes no arguments"))
                }
            },
            ActionToken::Word(w @ "char-jump") => {
                match parse_flags([(Flag::Mark, Some(&DEFAULT_MARK[..]))], rest) {
                    Ok([mark]) => self.visit_char_jump(mark),
                    Err(e) => self.edit_target_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "line-jump") => {
                match parse_flags([(Flag::Mark, Some(&DEFAULT_MARK[..]))], rest) {
                    Ok([mark]) => self.visit_line_jump(mark),
                    Err(e) => self.edit_target_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "motion") => {
                match parse_flags(
                    [
                        (Flag::Short('T'), None),
                        (Flag::Count, Some(&DEFAULT_COUNT)),
                    ],
                    rest,
                ) {
                    Ok([t, count]) => self.visit_motion(t, count),
                    Err(e) => self.edit_target_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "range") => {
                match parse_flags(
                    [
                        (Flag::Short('T'), None),
                        (Flag::Long("inclusive".into()), None),
                        (Flag::Count, Some(&DEFAULT_COUNT)),
                    ],
                    rest,
                ) {
                    Ok([t, inclusive, count]) => self.visit_range(t, inclusive, count),
                    Err(e) => self.edit_target_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "search") => {
                match parse_flags(
                    [
                        (Flag::Short('T'), None),
                        (Flag::Dir, None),
                        (Flag::Count, Some(&DEFAULT_COUNT)),
                    ],
                    rest,
                ) {
                    Ok([t, dir, count]) => self.visit_search(t, dir, count),
                    Err(e) => self.edit_target_invalid(fail_cmd_flag_msg(w, e)),
                }
            },
            ActionToken::Word(w @ "selection") => {
                if rest.is_empty() {
                    self.visit_selection()
                } else {
                    self.edit_target_invalid(format!("`{w}` takes no arguments"))
                }
            },

            t => {
                self.edit_target_invalid(format!(
                    "expected the name of an edit target, found `{t}`"
                ))
            },
        }
    }
}
