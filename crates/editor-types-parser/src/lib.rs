use std::collections::HashSet;

mod tokenizer;

pub use tokenizer::tokenize;

pub const DEFAULT_FALSE: [ActionToken<'static>; 1] = [ActionToken::Bool(false)];
pub const DEFAULT_FILTER: [ActionToken<'static>; 1] = [ActionToken::Word("all")];
pub const DEFAULT_COMPTYPE: [ActionToken<'static>; 1] = [ActionToken::Word("auto")];
pub const DEFAULT_COUNT: [ActionToken<'static>; 1] = [ActionToken::Word("ctx")];
pub const DEFAULT_MARK: [ActionToken<'static>; 1] = [ActionToken::Word("ctx")];
pub const DEFAULT_OP: [ActionToken<'static>; 1] = [ActionToken::Word("ctx")];
pub const DEFAULT_PREV: [ActionToken<'static>; 1] = [ActionToken::Word("previous")];

const EMPTY_ACTION: [ActionToken<'static>; 0] = [];

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArgError {
    ExpectedFlag(Option<Flag>),
    MissingArg(Flag),
    UnexpectedFlag(Flag),
    DuplicateFlag(Flag),
}

pub fn ungroup<'a>(value: &'a [ActionToken<'a>]) -> &'a [ActionToken<'a>] {
    match value {
        [] => value,
        [ActionToken::Group(ref grouped), ..] => grouped.as_slice(),
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
            [ActionToken::Flag(ref f)] => return Err(ArgError::MissingArg(f.clone())),
            [ActionToken::Flag(ref f), ActionToken::Flag(_)] => {
                return Err(ArgError::MissingArg(f.clone()))
            },
            [ActionToken::Flag(ref f), _] => {
                if seen.contains(f) {
                    return Err(ArgError::DuplicateFlag(f.clone()));
                }

                seen.insert(f);
                pairs.push((f, ungroup(&pair[1..])));
            },
            [_, ..] => return Err(ArgError::ExpectedFlag(None)),
        }
    }

    Ok(pairs)
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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

    /// A `--input` `-i` flag in the input.
    ///
    /// In order to encourage common flag initials, `-f` should always take an input `String`.
    Input,

    /// A `--mark` or `-m` flag in the input.
    ///
    /// In order to encourage common flag initials, `-m` should always take an input `Mark`.
    Mark,

    /// A --style` or `-s` flag in the input.
    ///
    /// In order to encourage common flag initials, `-s` should always take one of the `*Style`
    /// types.
    Style,

    /// A `--target` or `-t` flag in the input.
    ///
    /// In order to encourage common flag initials, `-t` should always take of the `*Target` types.
    Target,

    /// A `--wrap` or `-w` flag in the input.
    ///
    /// In order to encourage common flag initials, `-w` should always take a `bool` to indicate
    /// whether or not to wrap.
    Wrap,

    /// A short flag with an action-specific meaning.
    Short(char),

    /// A short flag with an action-specific meaning.
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
    /// A bare word in the input.
    Word(&'a str),

    /// An action-specific flag in the input.
    Flag(Flag),

    /// A quoted string in the input.
    Str(String),

    /// An `{id}` in the input that expands in the `action!` macro to an identifier.
    ///
    /// This will be `None` when the input is `{}`, and a positional argument will
    /// be used from the macro arguments instead.
    Id(Option<&'a str>),

    /// A boolean in the input.
    Bool(bool),

    /// A number in the input.
    Number(usize),

    /// A character in the input surrounded by single quotes (e.g., `'c'`).
    Char(char),

    /// A collection of tokens between parenthesis (e.g. `(foo bar 5)`).
    Group(Vec<ActionToken<'a>>),
}

pub trait ActionParser {
    type Output;
    type Span;

    /// Output an error for the current parse.
    fn fail<T: std::fmt::Display>(&self, msg: T, span: Self::Span) -> Self::Output;

    /// Parse `keyword-lookup`.
    fn visit_keyword_lookup(&mut self, target: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `noop`.
    fn visit_noop(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `redraw-screen`.
    fn visit_redraw_screen(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `suspend`.
    fn visit_suspend(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `cmdbar focus`.
    fn visit_cmdbar_focus(
        &mut self,
        prompt: &[ActionToken],
        cmdtype: &[ActionToken],
        action: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `cmdbar unfocus`.
    fn visit_cmdbar_unfocus(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `cmd execute` and its arguments.
    fn visit_command_execute(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `cmd run` and its arguments.
    fn visit_command_run(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `complete` and its arguments.
    fn visit_complete(
        &mut self,
        style: &[ActionToken],
        comptype: &[ActionToken],
        display: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `edit`.
    fn visit_edit(
        &mut self,
        action: &[ActionToken],
        target: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `history checkpoint`.
    fn visit_history_checkpoint(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `history undo` and its arguments.
    fn visit_history_undo(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `history redo` and its arguments.
    fn visit_history_redo(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `macro execute` and its arguments.
    fn visit_macro_execute(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `macro run` and its arguments.
    fn visit_macro_run(
        &mut self,
        input: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `macro repeat` and its arguments.
    fn visit_macro_repeat(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `macro toggle-recording`.
    fn visit_macro_toggle_recording(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `prompt abort` and its arguments.
    fn visit_prompt_abort(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `prompt recall` and its arguments.
    fn visit_prompt_recall(
        &mut self,
        filter: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `prompt submit`.
    fn visit_prompt_submit(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `mark` and its arguments.
    fn visit_mark(&mut self, mark: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `cursor close` and its arguments.
    fn visit_cursor_close(&mut self, target: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `cursor restore` and its arguments.
    fn visit_cursor_restore(&mut self, style: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `cursor rotate` and its arguments.
    fn visit_cursor_rotate(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `cursor save` and its arguments.
    fn visit_cursor_save(&mut self, style: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `cursor split` and its arguments.
    fn visit_cursor_split(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `tab close` and its arguments.
    fn visit_tab_close(
        &mut self,
        target: &[ActionToken],
        flags: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `tab extract` and its arguments.
    fn visit_tab_extract(
        &mut self,
        fc: &[ActionToken],
        dir: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `tab open` and its arguments.
    fn visit_tab_open(
        &mut self,
        target: &[ActionToken],
        fc: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `tab focus` and its arguments.
    fn visit_tab_focus(&mut self, fc: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `tab move` and its arguments.
    fn visit_tab_move(&mut self, fc: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `window close` and its arguments.
    fn visit_window_close(
        &mut self,
        target: &[ActionToken],
        falgs: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `window open` and its arguments.
    fn visit_window_open(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `window resize` and its arguments.
    fn visit_window_resize(
        &mut self,
        fc: &[ActionToken],
        axis: &[ActionToken],
        size: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `window split` and its arguments.
    fn visit_window_split(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `window switch` and its arguments.
    fn visit_window_switch(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `window write` and its arguments.
    fn visit_window_write(
        &mut self,
        target: &[ActionToken],
        flags: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `window exchange` and its arguments.
    fn visit_window_exchange(&mut self, fc: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `window focus` and its arguments.
    fn visit_window_focus(&mut self, fc: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `window move-side` and its arguments.
    fn visit_window_move_side(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `window rotate` and its arguments.
    fn visit_window_rotate(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `window clear-sizes` and its arguments.
    fn visit_window_clear_sizes(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `window zoom-toggle` and its arguments.
    fn visit_window_zoom_toggle(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `insert open-line` and its arguments.
    fn visit_insert_open_line(
        &mut self,
        shape: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `insert transcribe` and its arguments.
    fn visit_insert_transcribe(
        &mut self,
        input: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `insert type` and its arguments.
    fn visit_insert_type(
        &mut self,
        c: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `insert paste` and its arguments.
    fn visit_insert_paste(
        &mut self,
        style: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `repeat` and its arguments.
    fn visit_repeat(&mut self, style: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `scroll` and its arguments.
    fn visit_scroll(&mut self, style: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `search` and its arguments.
    fn visit_search(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `selection duplicate` and its arguments.
    fn visit_selection_duplicate(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `selection join` and its arguments.
    fn visit_selection_join(&mut self, span: Self::Span) -> Self::Output;

    /// Parse `selection cursor-set` and its arguments.
    fn visit_selection_cursor_set(
        &mut self,
        change: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `selection expand` and its arguments.
    fn visit_selection_expand(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `selection filter` and its arguments.
    fn visit_selection_filter(&mut self, drop: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse `selection resize` and its arguments.
    fn visit_selection_resize(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `selection split` and its arguments.
    fn visit_selection_split(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;

    /// Parse `selection trim` and its arguments.
    fn visit_selection_trim(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output;
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
            return Err(ArgError::ExpectedFlag(Some(flag)));
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
            return Err(ArgError::ExpectedFlag(Some(flag)));
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

fn fail_cmd_flag<V: ActionParser>(v: &V, cmd: &str, err: ArgError, span: V::Span) -> V::Output {
    match err {
        ArgError::ExpectedFlag(None) => v.fail(format!("`{cmd}` expects a flag argument"), span),
        ArgError::ExpectedFlag(Some(f)) => {
            v.fail(format!("`{cmd}` requires a `{f}` argument"), span)
        },
        ArgError::MissingArg(f) => {
            v.fail(format!("`{cmd}` expects an argument following `{f}`"), span)
        },
        ArgError::UnexpectedFlag(f) => v.fail(format!("`{cmd}` does not take `{f}`"), span),
        ArgError::DuplicateFlag(f) => v.fail(format!("`{cmd}` only takes one `{f}`"), span),
    }
}

pub trait ActionParserExt: ActionParser {
    /// Parse an action.
    fn parse_action(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `cmdbar` action.
    fn parse_action_cmdbar(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `command` action.
    fn parse_action_command(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `complete` action.
    fn parse_action_complete(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `cursor` action.
    fn parse_action_cursor(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse an `edit` action.
    fn parse_action_edit(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `history` action.
    fn parse_action_history(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse an `insert` action.
    fn parse_action_insert(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `macro` action.
    fn parse_action_macro(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `mark` action.
    fn parse_action_mark(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `prompt` action.
    fn parse_action_prompt(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `search` action.
    fn parse_action_search(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `selection` action.
    fn parse_action_selection(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `scroll` action.
    fn parse_action_scroll(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `repeat` action.
    fn parse_action_repeat(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `tab` action.
    fn parse_action_tab(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;

    /// Parse a `window` action.
    fn parse_action_window(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output;
}

impl<V: ActionParser> ActionParserExt for V {
    fn parse_action_cmdbar(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No cmdbar action specified", span);
        };

        match cmd {
            ActionToken::Word("focus") => {
                match parse_flags(
                    [
                        (Flag::Short('p'), None),
                        (Flag::Style, None),
                        (Flag::Short('a'), None),
                    ],
                    rest,
                ) {
                    Ok([prompt, cmdtype, action]) => {
                        self.visit_cmdbar_focus(prompt, cmdtype, action, span)
                    },
                    Err(e) => fail_cmd_flag(self, "cmdbar focus", e, span),
                }
            },
            ActionToken::Word("unfocus") => {
                if rest.is_empty() {
                    self.visit_cmdbar_unfocus(span)
                } else {
                    self.fail("`cmdbar unfocus` takes no arguments", span)
                }
            },
            ActionToken::Word(w) => self.fail(format!("`cmdbar {w}` is not a valid action"), span),
            _ => self.fail("expected a command bar action after `cmdbar`", span),
        }
    }

    fn parse_action_command(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No command action specified", span);
        };

        match cmd {
            ActionToken::Word("execute" | "exec") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_command_execute(count, span),
                    Err(e) => fail_cmd_flag(self, "command execute", e, span),
                }
            },
            ActionToken::Word("run") => {
                match parse_flags([(Flag::Input, None)], rest) {
                    Ok([input]) => self.visit_command_run(input, span),
                    Err(e) => fail_cmd_flag(self, "command run", e, span),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`command {w}` is not a valid action"), span),
            _ => self.fail("expected a command action after `command`", span),
        }
    }

    fn parse_action_complete(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        match parse_flags(
            [
                (Flag::Style, None),
                (Flag::Short('T'), Some(&DEFAULT_COMPTYPE[..])),
                (Flag::Short('D'), None),
            ],
            input,
        ) {
            Ok([style, comptype, display]) => self.visit_complete(style, comptype, display, span),
            Err(e) => fail_cmd_flag(self, "complete", e, span),
        }
    }

    fn parse_action_edit(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        match parse_flags(
            [
                (Flag::Short('o'), Some(&DEFAULT_OP[..])),
                (Flag::Target, None),
            ],
            input,
        ) {
            Ok([action, target]) => self.visit_edit(action, target, span),
            Err(e) => fail_cmd_flag(self, "edit", e, span),
        }
    }

    fn parse_action_history(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No history action specified", span);
        };

        match cmd {
            ActionToken::Word("checkpoint") => {
                if rest.is_empty() {
                    self.visit_history_checkpoint(span)
                } else {
                    self.fail("`history checkpoint` takes no arguments", span)
                }
            },
            ActionToken::Word("redo") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_history_redo(count, span),
                    Err(e) => fail_cmd_flag(self, "history redo", e, span),
                }
            },
            ActionToken::Word("undo") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_history_undo(count, span),
                    Err(e) => fail_cmd_flag(self, "history undo", e, span),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`history {w}` is not a valid action"), span),
            _ => self.fail("expected a history action after `history`", span),
        }
    }

    fn parse_action_macro(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No macro action specified", span);
        };

        match cmd {
            ActionToken::Word("execute" | "exec") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_macro_execute(count, span),
                    Err(e) => fail_cmd_flag(self, "macro execute", e, span),
                }
            },
            ActionToken::Word("run") => {
                match parse_flags(
                    [(Flag::Input, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([input, count]) => self.visit_macro_run(input, count, span),
                    Err(e) => fail_cmd_flag(self, "macro run", e, span),
                }
            },
            ActionToken::Word("repeat") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_macro_repeat(count, span),
                    Err(e) => fail_cmd_flag(self, "macro repeat", e, span),
                }
            },
            ActionToken::Word("toggle-recording") => {
                if rest.is_empty() {
                    self.visit_macro_toggle_recording(span)
                } else {
                    self.fail("`macro toggle-recording` takes no arguments", span)
                }
            },
            ActionToken::Word(w) => self.fail(format!("`macro {w}` is not a valid action"), span),
            _ => self.fail("expected a macro action after `macro`", span),
        }
    }

    fn parse_action_mark(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        match parse_flags([(Flag::Mark, Some(&DEFAULT_MARK[..]))], input) {
            Ok([mark]) => self.visit_mark(mark, span),
            Err(e) => fail_cmd_flag(self, "mark", e, span),
        }
    }

    fn parse_action_prompt(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No prompt action specified", span);
        };

        match cmd {
            ActionToken::Word("abort") => {
                if rest.is_empty() {
                    self.visit_prompt_abort(span)
                } else {
                    self.fail("`prompt abort` takes no arguments", span)
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
                    Ok([filter, dir, count]) => self.visit_prompt_recall(filter, dir, count, span),
                    Err(e) => fail_cmd_flag(self, "prompt recall", e, span),
                }
            },
            ActionToken::Word("submit") => {
                if rest.is_empty() {
                    self.visit_prompt_submit(span)
                } else {
                    self.fail("`prompt submit` takes no arguments", span)
                }
            },
            ActionToken::Word(w) => self.fail(format!("`prompt {w}` is not a valid action"), span),
            _ => self.fail("expected a prompt action after `prompt`", span),
        }
    }

    fn parse_action_cursor(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No cursor action specified", span);
        };

        match cmd {
            ActionToken::Word("close") => {
                match parse_single_flag(Flag::Target, rest) {
                    Ok(target) => self.visit_cursor_close(target, span),
                    Err(e) => fail_cmd_flag(self, "cursor close", e, span),
                }
            },
            ActionToken::Word("restore") => {
                match parse_single_flag(Flag::Style, rest) {
                    Ok(style) => self.visit_cursor_restore(style, span),
                    Err(e) => fail_cmd_flag(self, "cursor restore", e, span),
                }
            },
            ActionToken::Word("rotate") => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => self.visit_cursor_rotate(dir, count, span),
                    Err(e) => fail_cmd_flag(self, "cursor rotate", e, span),
                }
            },
            ActionToken::Word("save") => {
                match parse_single_flag(Flag::Style, rest) {
                    Ok(style) => self.visit_cursor_save(style, span),
                    Err(e) => fail_cmd_flag(self, "cursor save", e, span),
                }
            },
            ActionToken::Word("split") => {
                match parse_single_count(rest) {
                    Ok(count) => self.visit_cursor_split(count, span),
                    Err(e) => fail_cmd_flag(self, "cursor split", e, span),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`cursor {w}` is not a valid action"), span),
            _ => self.fail("expected a cursor action after `cursor`", span),
        }
    }

    fn parse_action_tab(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No tab action specified", span);
        };

        match cmd {
            ActionToken::Word("close") => {
                match parse_required_flags([Flag::Target, Flag::Short('F')], rest) {
                    Ok([target, flags]) => self.visit_tab_close(target, flags, span),
                    Err(e) => fail_cmd_flag(self, "tab close", e, span),
                }
            },
            ActionToken::Word("extract") => {
                match parse_required_flags([Flag::Focus, Flag::Dir], rest) {
                    Ok([fc, dir]) => self.visit_tab_extract(fc, dir, span),
                    Err(e) => fail_cmd_flag(self, "tab extract", e, span),
                }
            },
            ActionToken::Word("focus") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_tab_focus(fc, span),
                    Err(e) => fail_cmd_flag(self, "tab focus", e, span),
                }
            },
            ActionToken::Word("move") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_tab_move(fc, span),
                    Err(e) => fail_cmd_flag(self, "tab move", e, span),
                }
            },
            ActionToken::Word("open") => {
                match parse_required_flags([Flag::Target, Flag::Focus], rest) {
                    Ok([target, fc]) => self.visit_tab_open(target, fc, span),
                    Err(e) => fail_cmd_flag(self, "tab open", e, span),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`tab {w}` is not a valid action"), span),
            _ => self.fail("expected a tab action after `tab`", span),
        }
    }

    fn parse_action_window(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No window action specified", span);
        };

        match cmd {
            ActionToken::Word("close") => {
                match parse_required_flags([Flag::Target, Flag::Short('F')], rest) {
                    Ok([target, flags]) => self.visit_window_close(target, flags, span),
                    Err(e) => fail_cmd_flag(self, "window close", e, span),
                }
            },
            ActionToken::Word("exchange") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_window_exchange(fc, span),
                    Err(e) => fail_cmd_flag(self, "window exchange", e, span),
                }
            },
            ActionToken::Word("focus") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(fc) => self.visit_window_focus(fc, span),
                    Err(e) => fail_cmd_flag(self, "window focus", e, span),
                }
            },
            ActionToken::Word("move-side") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(fc) => self.visit_window_move_side(fc, span),
                    Err(e) => fail_cmd_flag(self, "window move-side", e, span),
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
                        self.visit_window_open(target, axis, dir, count, span)
                    },
                    Err(e) => fail_cmd_flag(self, "window open", e, span),
                }
            },
            ActionToken::Word("rotate") => {
                match parse_single_flag(Flag::Dir, rest) {
                    Ok(dir) => self.visit_window_rotate(dir, span),
                    Err(e) => fail_cmd_flag(self, "window rotate", e, span),
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
                        self.visit_window_split(target, axis, dir, count, span)
                    },
                    Err(e) => fail_cmd_flag(self, "window split", e, span),
                }
            },
            ActionToken::Word("switch") => {
                match parse_single_flag(Flag::Target, rest) {
                    Ok(target) => self.visit_window_switch(target, span),
                    Err(e) => fail_cmd_flag(self, "window switch", e, span),
                }
            },
            ActionToken::Word("resize") => {
                match parse_required_flags([Flag::Focus, Flag::Short('x'), Flag::Short('z')], rest)
                {
                    Ok([fc, axis, size]) => self.visit_window_resize(fc, axis, size, span),
                    Err(e) => fail_cmd_flag(self, "window resize", e, span),
                }
            },
            ActionToken::Word("write") => {
                match parse_required_flags([Flag::Target, Flag::Short('F')], rest) {
                    Ok([target, flags]) => self.visit_window_write(target, flags, span),
                    Err(e) => fail_cmd_flag(self, "window write", e, span),
                }
            },
            ActionToken::Word("clear-sizes") => {
                if rest.is_empty() {
                    self.visit_window_clear_sizes(span)
                } else {
                    self.fail("`window clear-sizes` takes no arguments", span)
                }
            },
            ActionToken::Word("zoom-toggle") => {
                if rest.is_empty() {
                    self.visit_window_zoom_toggle(span)
                } else {
                    self.fail("`window zoom-toggle` takes no arguments", span)
                }
            },
            ActionToken::Word(w) => self.fail(format!("`window {w}` is not a valid action"), span),
            _ => self.fail("expected a window action after `window`", span),
        }
    }

    fn parse_action_insert(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No insert action specified", span);
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
                    Ok([shape, dir, count]) => self.visit_insert_open_line(shape, dir, count, span),
                    Err(e) => fail_cmd_flag(self, "insert open-line", e, span),
                }
            },
            ActionToken::Word("paste") => {
                match parse_flags(
                    [(Flag::Style, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([style, count]) => self.visit_insert_paste(style, count, span),
                    Err(e) => fail_cmd_flag(self, "insert paste", e, span),
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
                    Ok([s, dir, count]) => self.visit_insert_transcribe(s, dir, count, span),
                    Err(e) => fail_cmd_flag(self, "insert transcribe", e, span),
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
                    Ok([c, dir, count]) => self.visit_insert_type(c, dir, count, span),
                    Err(e) => fail_cmd_flag(self, "insert type", e, span),
                }
            },
            ActionToken::Word(w) => self.fail(format!("`insert {w}` is not a valid action"), span),
            _ => self.fail("Expected an action after `insert`", span),
        }
    }

    fn parse_action_search(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        match parse_flags([(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))], input) {
            Ok([dir, count]) => self.visit_search(dir, count, span),
            Err(e) => fail_cmd_flag(self, "search", e, span),
        }
    }

    fn parse_action_selection(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No selection action specified", span);
        };

        match cmd {
            ActionToken::Word("duplicate") => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => self.visit_selection_duplicate(dir, count, span),
                    Err(e) => fail_cmd_flag(self, "selection duplicate", e, span),
                }
            },
            ActionToken::Word("cursor-set") => {
                match parse_single_flag(Flag::Focus, rest) {
                    Ok(change) => self.visit_selection_cursor_set(change, span),
                    Err(e) => fail_cmd_flag(self, "selection cursor-set", e, span),
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
                    Ok([boundary, target]) => self.visit_selection_expand(boundary, target, span),
                    Err(e) => fail_cmd_flag(self, "selection expand", e, span),
                }
            },
            ActionToken::Word("filter") => {
                match parse_single_flag(Flag::Short('F'), rest) {
                    Ok(filter) => self.visit_selection_filter(filter, span),
                    Err(e) => fail_cmd_flag(self, "selection filter", e, span),
                }
            },
            ActionToken::Word("join") => {
                if rest.is_empty() {
                    self.visit_selection_join(span)
                } else {
                    self.fail("`selection join` takes no arguments", span)
                }
            },
            ActionToken::Word("resize") => {
                match parse_required_flags([Flag::Style, Flag::Target], rest) {
                    Ok([style, target]) => self.visit_selection_resize(style, target, span),
                    Err(e) => fail_cmd_flag(self, "selection resize", e, span),
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
                    Ok([style, filter]) => self.visit_selection_split(style, filter, span),
                    Err(e) => fail_cmd_flag(self, "selection split", e, span),
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
                    Ok([boundary, target]) => self.visit_selection_trim(boundary, target, span),
                    Err(e) => fail_cmd_flag(self, "selection trim", e, span),
                }
            },
            ActionToken::Word(w) => {
                self.fail(format!("`selection {w}` is not a valid action"), span)
            },
            _ => self.fail("expected a selection action after `selection`", span),
        }
    }

    fn parse_action_scroll(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        match parse_single_flag(Flag::Style, input) {
            Ok(style) => self.visit_scroll(style, span),
            Err(e) => fail_cmd_flag(self, "scroll", e, span),
        }
    }

    fn parse_action_repeat(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        match parse_single_flag(Flag::Style, input) {
            Ok(style) => self.visit_repeat(style, span),
            Err(e) => fail_cmd_flag(self, "repeat", e, span),
        }
    }

    fn parse_action(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let Some((cmd, rest)) = input.split_first() else {
            return self.fail("No action specified", span);
        };

        match cmd {
            ActionToken::Word("cmdbar") => self.parse_action_cmdbar(rest, span),
            ActionToken::Word("command") => self.parse_action_command(rest, span),
            ActionToken::Word("complete") => self.parse_action_complete(rest, span),
            ActionToken::Word("cursor") => self.parse_action_cursor(rest, span),
            ActionToken::Word("edit") => self.parse_action_edit(rest, span),
            ActionToken::Word("history") => self.parse_action_history(rest, span),
            ActionToken::Word("insert") => self.parse_action_insert(rest, span),
            ActionToken::Word("macro") => self.parse_action_macro(rest, span),
            ActionToken::Word("mark") => self.parse_action_mark(rest, span),
            ActionToken::Word("prompt") => self.parse_action_prompt(rest, span),
            ActionToken::Word("repeat") => self.parse_action_repeat(rest, span),
            ActionToken::Word("search") => self.parse_action_search(rest, span),
            ActionToken::Word("selection") => self.parse_action_selection(rest, span),
            ActionToken::Word("scroll") => self.parse_action_scroll(rest, span),
            ActionToken::Word("tab") => self.parse_action_tab(rest, span),
            ActionToken::Word("window") => self.parse_action_window(rest, span),
            ActionToken::Word(w @ ("kw-lookup" | "keyword-lookup")) => {
                match parse_single_flag(Flag::Target, rest) {
                    Ok(target) => self.visit_keyword_lookup(target, span),
                    Err(e) => fail_cmd_flag(self, w, e, span),
                }
            },
            ActionToken::Word(w @ ("nop" | "noop" | "no-op")) => {
                if rest.is_empty() {
                    self.visit_noop(span)
                } else {
                    self.fail(format!("`{w}` takes no arguments"), span)
                }
            },
            ActionToken::Word(w @ "redraw-screen") => {
                if rest.is_empty() {
                    self.visit_redraw_screen(span)
                } else {
                    self.fail(format!("`{w}` takes no arguments"), span)
                }
            },
            ActionToken::Word("suspend") => {
                if rest.is_empty() {
                    self.visit_suspend(span)
                } else {
                    self.fail("`suspend` takes no arguments", span)
                }
            },

            ActionToken::Word(w) => self.fail(format!("unknown action keyword `{w}`"), span),

            _ => self.fail("expect action keyword at start of command", span),
        }
    }
}
