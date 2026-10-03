use anyhow::{anyhow, bail};
use editor_types_parser::{
    ActionParser,
    ActionParserExt,
    ActionToken,
    DEFAULT_COUNT,
    DEFAULT_TRUE,
    EditTargetParser,
    EditTargetParserExt,
    Flag,
    MotionParser,
    MotionParserExt,
    RangeParser,
    RangeParserExt,
    parse_flags,
    parse_required_flags,
    parse_single_flag,
};

use crate::prelude::*;
use crate::*;

mod action;
mod edit_target;
mod motion;
mod range;

pub struct ActionReader<I> {
    _p: std::marker::PhantomData<I>,
}

impl<I> Default for ActionReader<I> {
    fn default() -> Self {
        Self { _p: std::marker::PhantomData }
    }
}

macro_rules! bad_word_match_branch {
    ($w: ident, $msg: expr) => {
        bail!("`{}` is not a valid {}", $w, $msg)
    };
}

macro_rules! enum_no_args_branch {
    ($path: expr, $w: expr, $rest: ident) => {
        if $rest.is_empty() {
            Ok($path)
        } else {
            bail!("no arguments were expected after `{}`", $w)
        }
    };
}

fn parse_single_count(cmd: &str, input: &[ActionToken<'_>]) -> anyhow::Result<Count> {
    parse_flags([(Flag::Count, Some(&DEFAULT_COUNT))], input)
        .map_err(|e| anyhow!("{}", e.display(cmd)))
        .and_then(|[c]| Count::try_from(c))
}

fn parse_specifier<T>(input: &[ActionToken<'_>]) -> anyhow::Result<Specifier<T>>
where
    T: for<'a> TryFrom<&'a [ActionToken<'a>], Error = anyhow::Error>,
{
    match input {
        [ActionToken::Word(w @ "ctx"), rest @ ..] => {
            enum_no_args_branch!(Specifier::Contextual, w, rest)
        },
        [ActionToken::Word("exact"), rest @ ..] => {
            let t = T::try_from(rest)?;
            Ok(Specifier::Exact(t))
        },
        [t, ..] => bail!("expected either `ctx` or `exact`, not `{t}`"),
        _ => bail!("expected either `ctx` or `exact`"),
    }
}

impl TryFrom<&[ActionToken<'_>]> for Axis {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ ("h" | "horizontal")), rest @ ..] => {
                enum_no_args_branch!(Axis::Horizontal, w, rest)
            },
            [ActionToken::Word(w @ ("v" | "vertical")), rest @ ..] => {
                enum_no_args_branch!(Axis::Vertical, w, rest)
            },
            [t, ..] => bail!("expected `horizontal` or `vertical`, not `{t}`"),
            _ => bail!("expected a valid axis argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for Char {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "copy-line"), rest @ ..] => {
                let dir = parse_single_flag(Flag::Dir, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(MoveDir1D::try_from)?;

                Ok(Char::CopyLine(dir))
            },
            [ActionToken::Word(w @ "ctrl-seq"), rest @ ..] => {
                let input = parse_single_flag(Flag::Input, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(|d| parse_std_string(d))?;

                Ok(Char::CtrlSeq(input))
            },
            [ActionToken::Word("digraph"), rest @ ..] => {
                let (c1, rest) = match rest {
                    [ActionToken::Char(c1), rest @ ..] => (*c1, rest),
                    _ => bail!("`digraph` expects exactly two characters"),
                };

                let c2 = match rest {
                    [ActionToken::Char(c2)] => *c2,
                    _ => bail!("`digraph` expects exactly two characters"),
                };

                Ok(Char::Digraph(c1, c2))
            },
            [ActionToken::Char(c), rest @ ..] => {
                if rest.is_empty() {
                    Ok(Char::Single(*c))
                } else {
                    bail!("characters should not take any arguments")
                }
            },
            [t, ..] => bail!("expected a valid character type, not `{t}`"),
            _ => bail!("expected a digraph, character, or identifier"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CommandType {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "application"), rest @ ..] => {
                enum_no_args_branch!(CommandType::Application, w, rest)
            },
            [ActionToken::Word(w @ "command"), rest @ ..] => {
                enum_no_args_branch!(CommandType::Command, w, rest)
            },
            [ActionToken::Word(w @ "content"), rest @ ..] => {
                enum_no_args_branch!(CommandType::Content, w, rest)
            },
            [ActionToken::Word(w @ "search"), rest @ ..] => {
                enum_no_args_branch!(CommandType::Search, w, rest)
            },
            [ActionToken::Word(w @ "shell"), rest @ ..] => {
                enum_no_args_branch!(CommandType::Shell, w, rest)
            },
            [t, ..] => bail!("expected a valid command type, found `{t}`"),
            _ => bail!("expected a valid command type"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CompletionDisplay {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(CompletionDisplay::None, w, rest)
            },
            [ActionToken::Word(w @ "bar"), rest @ ..] => {
                enum_no_args_branch!(CompletionDisplay::Bar, w, rest)
            },
            [ActionToken::Word(w @ "list"), rest @ ..] => {
                enum_no_args_branch!(CompletionDisplay::List, w, rest)
            },
            [t, ..] => bail!("expected `none`, `bar` or `list`, found `{t}`"),
            _ => bail!("expected a valid completion display"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CompletionScope {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "buffer"), rest @ ..] => {
                enum_no_args_branch!(CompletionScope::Buffer, w, rest)
            },
            [ActionToken::Word(w @ "global"), rest @ ..] => {
                enum_no_args_branch!(CompletionScope::Global, w, rest)
            },
            [t, ..] => bail!("expected `buffer` or `global`, found `{t}`"),
            _ => bail!("expected `buffer` or `global`"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CompletionStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(CompletionStyle::None, w, rest)
            },
            [ActionToken::Word(w @ "prefix"), rest @ ..] => {
                enum_no_args_branch!(CompletionStyle::Prefix, w, rest)
            },
            [ActionToken::Word(w @ "single"), rest @ ..] => {
                enum_no_args_branch!(CompletionStyle::Single, w, rest)
            },
            [ActionToken::Word(w @ "list"), rest @ ..] => {
                match parse_flags(
                    [
                        (Flag::Dir, None),
                        (Flag::Long("toggle".into()), Some(&DEFAULT_TRUE[..])),
                    ],
                    rest,
                ) {
                    Ok([dir, toggle]) => {
                        let dir = MoveDir1D::try_from(dir)?;
                        let toggle = parse_std_bool(toggle)?;
                        Ok(CompletionStyle::List(dir, toggle))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [t, ..] => bail!("expected a valid completion selection, not `{t}`"),
            _ => bail!("expected a valid completion selection"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CompletionType {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "auto"), rest @ ..] => {
                enum_no_args_branch!(CompletionType::Auto, w, rest)
            },
            [ActionToken::Word(w @ "file"), rest @ ..] => {
                enum_no_args_branch!(CompletionType::File, w, rest)
            },
            [ActionToken::Word("line"), rest @ ..] => {
                let scope = CompletionScope::try_from(rest)?;
                Ok(CompletionType::Line(scope))
            },
            [ActionToken::Word("word"), rest @ ..] => {
                let scope = CompletionScope::try_from(rest)?;
                Ok(CompletionType::Word(scope))
            },
            [t, _] => bail!("expected a valid completion type, not `{t}`"),
            _ => bail!("expected a valid completion type"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CursorCloseTarget {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "leader"), rest @ ..] => {
                enum_no_args_branch!(CursorCloseTarget::Leader, w, rest)
            },
            [ActionToken::Word(w @ "followers"), rest @ ..] => {
                enum_no_args_branch!(CursorCloseTarget::Followers, w, rest)
            },
            [t, ..] => bail!("expected `leader` or `followers`, found `{t}`"),
            _ => bail!("expected a valid cursor target"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CursorGroupCombineStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "append"), rest @ ..] => {
                enum_no_args_branch!(CursorGroupCombineStyle::Append, w, rest)
            },
            [ActionToken::Word("merge"), rest @ ..] => {
                let style = CursorMergeStyle::try_from(rest)?;
                Ok(CursorGroupCombineStyle::Merge(style))
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                enum_no_args_branch!(CursorGroupCombineStyle::Replace, w, rest)
            },
            [t, ..] => bail!("expected `append`, `merge`, or `replace`, not `{t}`"),
            _ => bail!("expected `append`, `merge`, or `replace`"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CursorMergeStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "union"), rest @ ..] => {
                enum_no_args_branch!(CursorMergeStyle::Union, w, rest)
            },
            [ActionToken::Word(w @ "intersect"), rest @ ..] => {
                enum_no_args_branch!(CursorMergeStyle::Intersect, w, rest)
            },
            [ActionToken::Word(w @ "select-cursor"), rest @ ..] => {
                let dir = parse_single_flag(Flag::Dir, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(MoveDir1D::try_from)?;
                Ok(CursorMergeStyle::SelectCursor(dir))
            },
            [ActionToken::Word(w @ "select-short"), rest @ ..] => {
                enum_no_args_branch!(CursorMergeStyle::SelectShort, w, rest)
            },
            [ActionToken::Word(w @ "select-long"), rest @ ..] => {
                enum_no_args_branch!(CursorMergeStyle::SelectLong, w, rest)
            },
            [t, ..] => {
                bail!("`{t}` is not a valid cursor merge style")
            },
            _ => bail!("expected a valid merge style for combining cursor groups"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for Case {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "upper"), rest @ ..] => {
                enum_no_args_branch!(Case::Upper, w, rest)
            },
            [ActionToken::Word(w @ "lower"), rest @ ..] => {
                enum_no_args_branch!(Case::Lower, w, rest)
            },
            [ActionToken::Word(w @ "title"), rest @ ..] => {
                enum_no_args_branch!(Case::Title, w, rest)
            },
            [ActionToken::Word(w @ "toggle"), rest @ ..] => {
                enum_no_args_branch!(Case::Toggle, w, rest)
            },
            [t, ..] => bail!("expected a valid case change, found `{t}`"),
            _ => bail!("expected a valid case change"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for Count {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(Count::Contextual, w, rest)
            },
            [ActionToken::Word(w @ "ctx-sub-one"), rest @ ..] => {
                enum_no_args_branch!(Count::MinusOne, w, rest)
            },
            [ActionToken::Number(n), rest @ ..] => {
                if rest.is_empty() {
                    Ok(Count::Exact(*n))
                } else {
                    bail!("numbers cannot have arguments")
                }
            },
            [t, ..] => bail!("expected `ctx`, `ctx-sub-one`, or a number, not `{t}`"),
            _ => bail!("expected `ctx`, `ctx-sub-one`, or a number"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for EditAction {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "motion"), rest @ ..] => {
                enum_no_args_branch!(EditAction::Motion, w, rest)
            },
            [ActionToken::Word(w @ "delete"), rest @ ..] => {
                enum_no_args_branch!(EditAction::Delete, w, rest)
            },
            [ActionToken::Word(w @ "yank"), rest @ ..] => {
                enum_no_args_branch!(EditAction::Yank, w, rest)
            },
            [ActionToken::Word(w @ "format"), rest @ ..] => {
                enum_no_args_branch!(EditAction::Format, w, rest)
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                let virt = parse_single_flag(Flag::Long("virtual".into()), rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(parse_std_bool)?;

                Ok(EditAction::Replace(virt))
            },
            [
                ActionToken::Word(w @ ("change-number" | "change-num")),
                rest @ ..,
            ] => {
                match parse_required_flags([Flag::Style, Flag::Long("multiply".into())], rest) {
                    Ok([style, multiply]) => {
                        let style = NumberChange::try_from(style)?;
                        let multiply = parse_std_bool(multiply)?;
                        Ok(EditAction::ChangeNumber(style, multiply))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [ActionToken::Word(w @ "join"), rest @ ..] => {
                let style = parse_single_flag(Flag::Style, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(JoinStyle::try_from)?;

                Ok(EditAction::Join(style))
            },
            [ActionToken::Word(w @ "indent"), rest @ ..] => {
                let indent = parse_single_flag(Flag::Style, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(IndentChange::try_from)?;

                Ok(EditAction::Indent(indent))
            },
            [ActionToken::Word(w @ "change-case"), rest @ ..] => {
                let case = parse_single_flag(Flag::Style, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(Case::try_from)?;

                Ok(EditAction::ChangeCase(case))
            },
            [t, ..] => bail!("expected a valid edit action argument, not `{t}`"),
            _ => bail!("expected a valid edit action argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for FocusChange {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "current"), rest @ ..] => {
                enum_no_args_branch!(FocusChange::Current, w, rest)
            },
            [
                ActionToken::Word(w @ ("prev" | "previous" | "previously-focused")),
                rest @ ..,
            ] => {
                enum_no_args_branch!(FocusChange::PreviouslyFocused, w, rest)
            },
            [ActionToken::Word(w @ "offset"), rest @ ..] => {
                match parse_flags(
                    [
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                        (Flag::Short('l'), None),
                    ],
                    rest,
                ) {
                    Ok([count, clamp_last]) => {
                        let count = Count::try_from(count)?;
                        let clamp_last = parse_std_bool(clamp_last)?;
                        Ok(FocusChange::Offset(count, clamp_last))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [ActionToken::Word(w @ ("pos" | "position")), rest @ ..] => {
                match parse_single_flag(Flag::Position, rest) {
                    Ok(pos) => {
                        let pos = MovePosition::try_from(pos)?;
                        Ok(FocusChange::Position(pos))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [ActionToken::Word(w @ "dir1d"), rest @ ..] => {
                match parse_flags(
                    [
                        (Flag::Dir, None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                        (Flag::Wrap, None),
                    ],
                    rest,
                ) {
                    Ok([dir, count, wrap]) => {
                        let dir = MoveDir1D::try_from(dir)?;
                        let count = Count::try_from(count)?;
                        let wrap = parse_std_bool(wrap)?;
                        Ok(FocusChange::Direction1D(dir, count, wrap))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [ActionToken::Word(w @ "dir2d"), rest @ ..] => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => {
                        let dir = MoveDir2D::try_from(dir)?;
                        let count = Count::try_from(count)?;
                        Ok(FocusChange::Direction2D(dir, count))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [t, ..] => {
                bail!(
                    "expected `current`, `dir1d`, `dir2d`, `offset`, `pos` or `prev`, found `{t}`"
                )
            },
            _ => bail!("Expected a valid focus change argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for IndentChange {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "auto"), rest @ ..] => {
                enum_no_args_branch!(IndentChange::Auto, w, rest)
            },
            [ActionToken::Word(w @ "increase"), rest @ ..] => {
                let count = parse_single_count(w, rest)?;
                Ok(IndentChange::Increase(count))
            },
            [ActionToken::Word(w @ "decrease"), rest @ ..] => {
                let count = parse_single_count(w, rest)?;
                Ok(IndentChange::Decrease(count))
            },
            [t, ..] => bail!("expected a valid IndentChange, found `{t}`"),
            _ => bail!("expected a valid IndentChange"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for NumberChange {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "increase"), rest @ ..] => {
                let count = parse_single_count(w, rest)?;
                Ok(NumberChange::Increase(count))
            },
            [ActionToken::Word(w @ "decrease"), rest @ ..] => {
                let count = parse_single_count(w, rest)?;
                Ok(NumberChange::Decrease(count))
            },
            [t, ..] => bail!("expected a valid NumberChange, found `{t}`"),
            _ => bail!("expected a valid NumberChange"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for JoinStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "no-change"), rest @ ..] => {
                enum_no_args_branch!(JoinStyle::NoChange, w, rest)
            },
            [ActionToken::Word(w @ "one-space"), rest @ ..] => {
                enum_no_args_branch!(JoinStyle::OneSpace, w, rest)
            },
            [ActionToken::Word(w @ "new-space"), rest @ ..] => {
                enum_no_args_branch!(JoinStyle::NewSpace, w, rest)
            },
            [t, ..] => bail!("expected a valid join style, found `{t}`"),
            _ => bail!("expected a valid join style"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for KeywordTarget {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "selection"), rest @ ..] => {
                enum_no_args_branch!(KeywordTarget::Selection, w, rest)
            },
            [ActionToken::Word("word"), rest @ ..] => {
                let style = WordStyle::try_from(rest)?;
                Ok(KeywordTarget::Word(style))
            },
            [t, ..] => bail!("expected `selection` or `word`, found `{t}`"),
            _ => bail!("expected a valid keyword target"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for Mark {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "buffer-last-exited"), rest @ ..] => {
                enum_no_args_branch!(Mark::BufferLastExited, w, rest)
            },
            [ActionToken::Word("buffer-named"), rest @ ..] => {
                let c = parse_std_char(rest)?;
                Ok(Mark::BufferNamed(c))
            },
            [ActionToken::Word("global-last-exited"), rest @ ..] => {
                let n = parse_std_num(rest)?;
                Ok(Mark::GlobalLastExited(n))
            },
            [ActionToken::Word("global-named"), rest @ ..] => {
                let c = parse_std_char(rest)?;
                Ok(Mark::GlobalNamed(c))
            },
            [ActionToken::Word(w @ "last-changed"), rest @ ..] => {
                enum_no_args_branch!(Mark::LastChanged, w, rest)
            },
            [ActionToken::Word(w @ "last-inserted"), rest @ ..] => {
                enum_no_args_branch!(Mark::LastInserted, w, rest)
            },
            [ActionToken::Word(w @ "last-jump"), rest @ ..] => {
                enum_no_args_branch!(Mark::LastJump, w, rest)
            },
            [ActionToken::Word(w @ "visual-begin"), rest @ ..] => {
                enum_no_args_branch!(Mark::VisualBegin, w, rest)
            },
            [ActionToken::Word(w @ "visual-end"), rest @ ..] => {
                enum_no_args_branch!(Mark::VisualEnd, w, rest)
            },
            [ActionToken::Word(w @ "last-yanked-begin"), rest @ ..] => {
                enum_no_args_branch!(Mark::LastYankedBegin, w, rest)
            },
            [ActionToken::Word(w @ "last-yanked-end"), rest @ ..] => {
                enum_no_args_branch!(Mark::LastYankedEnd, w, rest)
            },
            [t, ..] => bail!("expected a valid mark name, not `{t}`"),
            _ => bail!("expected a valid mark name"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for MoveDir1D {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "next"), rest @ ..] => {
                enum_no_args_branch!(MoveDir1D::Next, w, rest)
            },
            [ActionToken::Word(w @ ("prev" | "previous")), rest @ ..] => {
                enum_no_args_branch!(MoveDir1D::Previous, w, rest)
            },
            [t, ..] => {
                bail!(format!("expected `next` or `prev`, found `{t}`"))
            },
            _ => bail!("expected one of the directions `next` or `prev`"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for MoveDir2D {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word("up")] => Ok(MoveDir2D::Up),
            [ActionToken::Word("down")] => Ok(MoveDir2D::Down),
            [ActionToken::Word("left")] => Ok(MoveDir2D::Left),
            [ActionToken::Word("right")] => Ok(MoveDir2D::Right),
            [t, ..] => bail!("expected `up`, `down`, `left`, or `right`, found `{t}`"),
            _ => bail!("expected one of the directions `up`, `down`, `left`, or `right`"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for MoveDirMod {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "same"), rest @ ..] => {
                enum_no_args_branch!(MoveDirMod::Same, w, rest)
            },
            [ActionToken::Word(w @ "flip"), rest @ ..] => {
                enum_no_args_branch!(MoveDirMod::Flip, w, rest)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let dir = MoveDir1D::try_from(rest)?;
                Ok(MoveDirMod::Exact(dir))
            },
            [t, ..] => {
                bail!("expected `same`, `flip`, or `exact`, found `{t}`")
            },
            _ => {
                bail!(
                    "expected one of the directions `same`, `flip`, `(exact prev)` or `(exact next)`",
                )
            },
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for MovePosition {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(MovePosition::Beginning, w, rest)
            },
            [ActionToken::Word(w @ ("m" | "middle")), rest @ ..] => {
                enum_no_args_branch!(MovePosition::Middle, w, rest)
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(MovePosition::End, w, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "move position"),
            _ => bail!("expected a valid move position"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for MoveTerminus {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(MoveTerminus::Beginning, w, rest)
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(MoveTerminus::End, w, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "move terminus"),
            _ => bail!("expected a valid move terminus"),
        }
    }
}

impl<W: ApplicationWindowId> TryFrom<&[ActionToken<'_>]> for OpenTarget<W> {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "alternate"), rest @ ..] => {
                enum_no_args_branch!(OpenTarget::Alternate, w, rest)
            },
            [ActionToken::Word(w @ "current"), rest @ ..] => {
                enum_no_args_branch!(OpenTarget::Current, w, rest)
            },
            [ActionToken::Word(w @ "selection"), rest @ ..] => {
                enum_no_args_branch!(OpenTarget::Selection, w, rest)
            },
            [ActionToken::Word(w @ "unnamed"), rest @ ..] => {
                enum_no_args_branch!(OpenTarget::Unnamed, w, rest)
            },
            [ActionToken::Word(w @ "cursor"), rest @ ..] => {
                let style = parse_single_flag(Flag::Style, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(WordStyle::try_from)?;
                Ok(OpenTarget::Cursor(style))
            },
            [ActionToken::Word(w @ "list"), rest @ ..] => {
                let count = parse_single_count(w, rest)?;
                Ok(OpenTarget::List(count))
            },
            [ActionToken::Word(w @ "name"), rest @ ..] => {
                let name = parse_single_flag(Flag::Input, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(parse_std_string)?;
                Ok(OpenTarget::Name(name))
            },
            [ActionToken::Word(w @ "offset"), rest @ ..] => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => {
                        let dir = MoveDir1D::try_from(dir)?;
                        let count = Count::try_from(count)?;
                        Ok(OpenTarget::Offset(dir, count))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [t, ..] => bail!("expected a valid open target, not `{t}`"),
            _ => bail!("expected a valid open target argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for PasteStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "cursor"), rest @ ..] => {
                enum_no_args_branch!(PasteStyle::Cursor, w, rest)
            },
            [ActionToken::Word(w @ "side"), rest @ ..] => {
                let dir = parse_single_flag(Flag::Dir, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(MoveDir1D::try_from)?;
                Ok(PasteStyle::Side(dir))
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                enum_no_args_branch!(PasteStyle::Replace, w, rest)
            },
            [t, ..] => bail!("expected a valid paste style, not `{t}`"),
            _ => bail!("expected a valid paste style"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for PositionList {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "jump-list"), rest @ ..] => {
                enum_no_args_branch!(PositionList::JumpList, w, rest)
            },
            [ActionToken::Word(w @ "change-list"), rest @ ..] => {
                enum_no_args_branch!(PositionList::ChangeList, w, rest)
            },
            [t, ..] => bail!("expected `jump-list` or `change-list`, found `{t}`"),
            _ => bail!("expected a valid position list"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for Radix {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Number(2), rest @ ..] => {
                enum_no_args_branch!(Radix::Binary, "2", rest)
            },
            [ActionToken::Number(8), rest @ ..] => {
                enum_no_args_branch!(Radix::Octal, "8", rest)
            },
            [ActionToken::Number(10), rest @ ..] => {
                enum_no_args_branch!(Radix::Decimal, "10", rest)
            },
            [ActionToken::Number(16), rest @ ..] => {
                enum_no_args_branch!(Radix::Hexadecimal, "16", rest)
            },
            [ActionToken::Word(w @ ("bin" | "binary")), rest @ ..] => {
                enum_no_args_branch!(Radix::Binary, w, rest)
            },
            [ActionToken::Word(w @ ("oct" | "octal")), rest @ ..] => {
                enum_no_args_branch!(Radix::Octal, w, rest)
            },
            [ActionToken::Word(w @ ("dec" | "decimal")), rest @ ..] => {
                enum_no_args_branch!(Radix::Decimal, w, rest)
            },
            [ActionToken::Word(w @ ("hex" | "hexadecimal")), rest @ ..] => {
                enum_no_args_branch!(Radix::Hexadecimal, w, rest)
            },
            _ => bail!("expected a valid radix argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for RecallFilter {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(RecallFilter::All, w, rest)
            },
            [
                ActionToken::Word(w @ ("prefix" | "prefix-match")),
                rest @ ..,
            ] => {
                enum_no_args_branch!(RecallFilter::PrefixMatch, w, rest)
            },
            [t, ..] => {
                bail!("expected `all` or `prefix-match`, found `{t}`")
            },
            _ => bail!("expected a valid prompt recall filter"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for SizeChange {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word("dec" | "decrease"), rest @ ..] => {
                let count = Count::try_from(rest)?;
                Ok(SizeChange::Decrease(count))
            },
            [ActionToken::Word("inc" | "increase"), rest @ ..] => {
                let count = Count::try_from(rest)?;
                Ok(SizeChange::Increase(count))
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let count = Count::try_from(rest)?;
                Ok(SizeChange::Exact(count))
            },
            [ActionToken::Word(w @ ("eq" | "equal")), rest @ ..] => {
                enum_no_args_branch!(SizeChange::Equal, w, rest)
            },
            [t, ..] => bail!("expected `decrease`, `increase`, `exact`, or `equal`, not `{t}`"),
            _ => bail!("expected a valid size change"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for ScrollSize {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "cell"), rest @ ..] => {
                enum_no_args_branch!(ScrollSize::Cell, w, rest)
            },
            [ActionToken::Word(w @ "half-page"), rest @ ..] => {
                enum_no_args_branch!(ScrollSize::HalfPage, w, rest)
            },
            [ActionToken::Word(w @ "page"), rest @ ..] => {
                enum_no_args_branch!(ScrollSize::Page, w, rest)
            },
            [t, ..] => bail!("expected `cell`, `half-page`, or `page`, not `{t}`"),
            _ => bail!("expected a valid scroll size"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for ScrollStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "dir2d"), rest @ ..] => {
                match parse_flags(
                    [
                        (Flag::Dir, None),
                        (Flag::Short('z'), None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([dir, size, count]) => {
                        let dir = MoveDir2D::try_from(dir)?;
                        let size = ScrollSize::try_from(size)?;
                        let count = Count::try_from(count)?;
                        Ok(ScrollStyle::Direction2D(dir, size, count))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [ActionToken::Word(w @ "cursor-pos"), rest @ ..] => {
                match parse_flags([(Flag::Position, None), (Flag::Short('x'), None)], rest) {
                    Ok([pos, axis]) => {
                        let pos = MovePosition::try_from(pos)?;
                        let axis = Axis::try_from(axis)?;

                        Ok(ScrollStyle::CursorPos(pos, axis))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [ActionToken::Word(w @ "line-pos"), rest @ ..] => {
                match parse_flags(
                    [
                        (Flag::Position, None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([pos, count]) => {
                        let pos = MovePosition::try_from(pos)?;
                        let count = Count::try_from(count)?;

                        Ok(ScrollStyle::LinePos(pos, count))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [t, ..] => bail!("expected a valid scroll style, not `{t}`"),
            _ => bail!("expected a valid scroll style"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for SearchType {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "regex"), rest @ ..] => {
                enum_no_args_branch!(SearchType::Regex, w, rest)
            },
            [ActionToken::Word(w @ "char"), rest @ ..] => {
                let multiline = parse_single_flag(Flag::Long("multiline".into()), rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(|b| parse_std_bool(b))?;

                Ok(SearchType::Char(multiline))
            },
            [ActionToken::Word(w @ "word"), rest @ ..] => {
                match parse_required_flags([Flag::Style, Flag::Short('b')], rest) {
                    Ok([style, boundary]) => {
                        let style = WordStyle::try_from(style)?;
                        let boundary = parse_std_bool(boundary)?;
                        Ok(SearchType::Word(style, boundary))
                    },
                    Err(e) => bail!("{}", e.display(w)),
                }
            },
            [t, ..] => bail!("expected `regex`, `char` or `word`, not `{t}`"),
            _ => bail!("expected `regex`, `char` or `word`"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for MatchAction {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "keep"), rest @ ..] => {
                enum_no_args_branch!(MatchAction::Keep, w, rest)
            },
            [ActionToken::Word(w @ "drop"), rest @ ..] => {
                enum_no_args_branch!(MatchAction::Drop, w, rest)
            },
            [t, ..] => {
                bail!("expected `drop` or `keep`, found `{t}`")
            },
            _ => bail!("expected a valid match action (`drop` or `keep`)"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for RepeatType {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "edit-sequence"), rest @ ..] => {
                enum_no_args_branch!(RepeatType::EditSequence, w, rest)
            },
            [ActionToken::Word(w @ "last-action"), rest @ ..] => {
                enum_no_args_branch!(RepeatType::LastAction, w, rest)
            },
            [ActionToken::Word(w @ "last-selection"), rest @ ..] => {
                enum_no_args_branch!(RepeatType::LastSelection, w, rest)
            },
            [t, ..] => {
                bail!("expected `edit-sequence`, `last-action`, or `last-selection`, not `{t}`")
            },
            _ => bail!("expected a valid repetition type"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for SelectionBoundary {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "line"), rest @ ..] => {
                enum_no_args_branch!(SelectionBoundary::Line, w, rest)
            },
            [
                ActionToken::Word(w @ ("non-ws" | "non-whitespace")),
                rest @ ..,
            ] => {
                enum_no_args_branch!(SelectionBoundary::NonWhitespace, w, rest)
            },
            [t, ..] => bail!("expected `line` or `non-whitespace`, not `{t}`"),
            _ => bail!("expected `line` or `non-whitespace`"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for SelectionCursorChange {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(SelectionCursorChange::Beginning, w, rest)
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(SelectionCursorChange::End, w, rest)
            },
            [ActionToken::Word(w @ "swap-anchor"), rest @ ..] => {
                enum_no_args_branch!(SelectionCursorChange::SwapAnchor, w, rest)
            },
            [ActionToken::Word(w @ "swap-side"), rest @ ..] => {
                enum_no_args_branch!(SelectionCursorChange::SwapSide, w, rest)
            },
            [t, ..] => {
                bail!("expected `beginning`, `end`, `swap-anchor` or `swap-side`, found `{t}`")
            },
            _ => bail!("Expected a valid selection cursor change"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for SelectionResizeStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "extend"), rest @ ..] => {
                enum_no_args_branch!(SelectionResizeStyle::Extend, w, rest)
            },
            [ActionToken::Word(w @ "object"), rest @ ..] => {
                enum_no_args_branch!(SelectionResizeStyle::Object, w, rest)
            },
            [ActionToken::Word(w @ "restart"), rest @ ..] => {
                enum_no_args_branch!(SelectionResizeStyle::Restart, w, rest)
            },
            _ => bail!("Expected a valid selection resize argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for SelectionSplitStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "anchor"), rest @ ..] => {
                enum_no_args_branch!(SelectionSplitStyle::Anchor, w, rest)
            },
            [ActionToken::Word(w @ "lines"), rest @ ..] => {
                enum_no_args_branch!(SelectionSplitStyle::Lines, w, rest)
            },
            [ActionToken::Word("regex"), rest @ ..] => {
                let act = MatchAction::try_from(rest)?;
                Ok(SelectionSplitStyle::Regex(act))
            },
            [t, ..] => {
                bail!("expected `anchor`, `object` or `regex`, found `{t}`")
            },
            _ => bail!("Expected a valid selection split argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for TargetShape {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ ("char" | "charwise")), rest @ ..] => {
                enum_no_args_branch!(TargetShape::CharWise, w, rest)
            },
            [ActionToken::Word(w @ ("line" | "linewise")), rest @ ..] => {
                enum_no_args_branch!(TargetShape::LineWise, w, rest)
            },
            [ActionToken::Word(w @ ("block" | "blockwise")), rest @ ..] => {
                enum_no_args_branch!(TargetShape::BlockWise, w, rest)
            },
            [t, ..] => {
                bail!("expected `charwise`, `linewise`, or `blockwise`, found `{t}`")
            },
            _ => bail!("expected a valid target shape"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for TargetShapeFilter {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        if input.is_empty() {
            bail!("expected a valid target shape filter");
        }

        let mut filter = TargetShapeFilter::NONE;

        for token in input {
            filter |= match token {
                ActionToken::Word("all") => TargetShapeFilter::ALL,
                ActionToken::Word("none") => TargetShapeFilter::NONE,
                ActionToken::Word("char" | "charwise") => TargetShapeFilter::CHAR,
                ActionToken::Word("line" | "linewise") => TargetShapeFilter::LINE,
                ActionToken::Word("block" | "blockwise") => TargetShapeFilter::BLOCK,
                t => bail!("expected a valid target shape filter, not `{t}`"),
            };
        }

        Ok(filter)
    }
}

impl TryFrom<&[ActionToken<'_>]> for TabTarget {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(TabTarget::All, w, rest)
            },
            [ActionToken::Word("all-but"), rest @ ..] => {
                let fc = FocusChange::try_from(rest)?;
                Ok(TabTarget::AllBut(fc))
            },
            [ActionToken::Word("single"), rest @ ..] => {
                let fc = FocusChange::try_from(rest)?;
                Ok(TabTarget::Single(fc))
            },
            [t, ..] => {
                bail!("expected `all`, `all-but` or `single`, found `{t}`")
            },
            _ => bail!("expected a valid tab target argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for CloseFlags {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        if input.is_empty() {
            bail!("Expected argument to be valid window closing flags");
        }

        let mut flags = CloseFlags::NONE;

        for token in input {
            flags |= match token {
                ActionToken::Word("none") => CloseFlags::NONE,
                ActionToken::Word("force") => CloseFlags::FORCE,
                ActionToken::Word("quit") => CloseFlags::QUIT,
                ActionToken::Word("write") => CloseFlags::WRITE,
                t => bail!("expected `none`, `force`, `quit` or `write`, found `{t}`"),
            };
        }

        Ok(flags)
    }
}

impl TryFrom<&[ActionToken<'_>]> for WriteFlags {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        if input.is_empty() {
            bail!("Expected argument to be valid window write flags");
        }

        let mut flags = WriteFlags::NONE;

        for token in input {
            flags |= match token {
                ActionToken::Word("none") => WriteFlags::NONE,
                ActionToken::Word("force") => WriteFlags::FORCE,
                t => bail!("expected `none` or `force`, found `{t}`"),
            };
        }

        Ok(flags)
    }
}

impl TryFrom<&[ActionToken<'_>]> for WindowTarget {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(WindowTarget::All, w, rest)
            },
            [ActionToken::Word("all-but"), rest @ ..] => {
                let fc = FocusChange::try_from(rest)?;
                Ok(WindowTarget::AllBut(fc))
            },
            [ActionToken::Word("single"), rest @ ..] => {
                let fc = FocusChange::try_from(rest)?;
                Ok(WindowTarget::Single(fc))
            },
            [t, ..] => {
                bail!("expected `all`, `all-but` or `single`, found `{t}`")
            },
            _ => bail!("expected a valid window target argument"),
        }
    }
}

impl TryFrom<&[ActionToken<'_>]> for WordStyle {
    type Error = anyhow::Error;

    fn try_from(input: &[ActionToken<'_>]) -> anyhow::Result<Self> {
        match input {
            [ActionToken::Word(w @ ("alphanum" | "alpha-num")), rest @ ..] => {
                enum_no_args_branch!(WordStyle::AlphaNum, w, rest)
            },
            [ActionToken::Word(w @ "big"), rest @ ..] => {
                enum_no_args_branch!(WordStyle::Big, w, rest)
            },
            [ActionToken::Word(w @ ("filename" | "file-name")), rest @ ..] => {
                enum_no_args_branch!(WordStyle::FileName, w, rest)
            },
            [ActionToken::Word(w @ ("filepath" | "file-path")), rest @ ..] => {
                enum_no_args_branch!(WordStyle::FilePath, w, rest)
            },
            [ActionToken::Word(w @ "little"), rest @ ..] => {
                enum_no_args_branch!(WordStyle::Little, w, rest)
            },
            [
                ActionToken::Word(
                    w @ ("non-alphanum" | "nonalphanum" | "non-alphanumeric" | "nonalphanumeric"),
                ),
                rest @ ..,
            ] => {
                enum_no_args_branch!(WordStyle::NonAlphaNum, w, rest)
            },
            [ActionToken::Word("radix"), rest @ ..] => {
                let radix = Radix::try_from(rest)?;
                Ok(WordStyle::Number(radix))
            },
            [ActionToken::Word(w @ "whitespace"), rest @ ..] => {
                let wrap = parse_single_flag(Flag::Wrap, rest)
                    .map_err(|e| anyhow!("{}", e.display(w)))
                    .and_then(|s| parse_std_bool(s))?;

                Ok(WordStyle::Whitespace(wrap))
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "word style"),
            _ => bail!("expected a valid word style"),
        }
    }
}

fn parse_std_bool(input: &[ActionToken<'_>]) -> anyhow::Result<bool> {
    match input {
        [ActionToken::Bool(b), rest @ ..] => {
            if rest.is_empty() {
                Ok(*b)
            } else {
                bail!("booleans should not take any arguments")
            }
        },
        [t, ..] => bail!("expected a boolean, not `{t}`"),
        _ => bail!("expected a boolean"),
    }
}

fn parse_std_string(input: &[ActionToken<'_>]) -> anyhow::Result<String> {
    match input {
        [ActionToken::Str(s), rest @ ..] => {
            if rest.is_empty() {
                Ok(s.clone().into_owned())
            } else {
                bail!("strings cannot have arguments")
            }
        },
        [t, ..] => bail!("expected a string, not `{t}`"),
        _ => bail!("expected a string argument"),
    }
}

fn parse_std_char(input: &[ActionToken<'_>]) -> anyhow::Result<char> {
    match input {
        [ActionToken::Char(c), rest @ ..] => {
            if rest.is_empty() {
                Ok(*c)
            } else {
                bail!("characters should not take any arguments")
            }
        },
        [t, ..] => bail!("expected a character, not `{t}`"),
        _ => bail!("expected a character"),
    }
}

fn parse_std_num(input: &[ActionToken<'_>]) -> anyhow::Result<usize> {
    match input {
        [ActionToken::Number(n), rest @ ..] => {
            if rest.is_empty() {
                Ok(*n)
            } else {
                bail!("numbers should not take any arguments")
            }
        },
        [t, ..] => bail!("expected a number, not `{t}`"),
        _ => bail!("expected a number"),
    }
}
