extern crate proc_macro;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::parse::{Error as ParseError, Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Expr, Ident, LitStr, Token, parse_macro_input};

use editor_types_parser::{
    ActionParser,
    ActionParserExt,
    ActionToken,
    ArgError,
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
    tokenize,
};

#[macro_use]
mod macros;
mod action;
mod edit_target;
mod motion;
mod range;

use action::*;
use edit_target::*;
use motion::*;
use range::*;

#[proc_macro]
pub fn action(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(stream as ActionMacroInput);
    input.into_stream().into()
}

#[proc_macro]
pub fn edit_target(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(stream as EditTargetMacroInput);
    input.into_stream().into()
}

#[proc_macro]
pub fn motion(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(stream as MotionMacroInput);
    input.into_stream().into()
}

#[proc_macro]
pub fn range(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(stream as RangeMacroInput);
    input.into_stream().into()
}

/// Tokenize the format string given to one of the procedural macros.
///
/// Any parsing errors are converted into a [ParseError] to get the compiler to print it out for us.
fn tokenize_fmt<'a>(
    macro_name: &str,
    fmt: &'a str,
    span: Span,
) -> syn::Result<Vec<ActionToken<'a>>> {
    tokenize(fmt).map_err(|e| {
        let msg = format!("invalid `{macro_name}` expression: {e}");
        ParseError::new(span, msg)
    })
}

/// Bind each positional argument to the identifier that the ActionTokens refers to
/// it by after `bind_positional` has filled them in.
fn bind_args(args: Vec<(Ident, Expr)>) -> TokenStream {
    let mut out = TokenStream::new();

    for (ident, expr) in args.into_iter() {
        out.extend(quote! { let #ident = { #expr }; });
    }

    out
}

struct ActionMacroParser {
    params: Vec<Ident>,
    pos: usize,
    span: Span,
}

impl ActionMacroParser {
    fn fail_cmd_flag(&self, cmd: &str, err: ArgError) -> TokenStream {
        self.fail(err.display(cmd).to_string())
    }

    fn advance(&mut self) -> syn::Result<String> {
        if self.pos < self.params.len() {
            let res = self.params[self.pos].clone();
            self.pos += 1;
            Ok(res.to_string())
        } else {
            Err(ParseError::new(self.span, "insufficient positional arguments provided"))
        }
    }

    fn bind_positional(&mut self, input: &mut [ActionToken<'_>]) -> syn::Result<()> {
        let mut f = || self.advance();

        for token in input.iter_mut() {
            // Ensure every positional reference has a binding:
            token.bind_positional(&mut f)?;
        }

        if self.pos < self.params.len() {
            // Don't allow accidentally unused arguments:
            Err(ParseError::new_spanned(&self.params[self.pos], "unused positional argument"))
        } else {
            Ok(())
        }
    }

    fn parse_single_dir1d<'a>(&mut self, cmd: &str, input: &'a [ActionToken<'a>]) -> TokenStream {
        match parse_single_flag(Flag::Dir, input) {
            Ok(c) => self.parse_dir1d(c),
            Err(e) => self.fail_cmd_flag(cmd, e),
        }
    }

    fn parse_single_count<'a>(&mut self, cmd: &str, input: &'a [ActionToken<'a>]) -> TokenStream {
        match editor_types_parser::parse_single_count(input) {
            Ok(c) => self.parse_count(c),
            Err(e) => self.fail_cmd_flag(cmd, e),
        }
    }

    fn parse_bool<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Bool(b), rest @ ..] => {
                if rest.is_empty() {
                    quote! { #b }
                } else {
                    self.fail(format!("the boolean `{b}` takes no arguments"))
                }
            },
            [ActionToken::Id(i), rest @ ..] => id_match_branch!(self, i, bool, rest),
            _ => self.fail("expected a valid boolean argument"),
        }
    }

    fn parse_num<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Number(n), rest @ ..] => {
                if rest.is_empty() {
                    quote! { #n }
                } else {
                    self.fail(format!("the number `{n}` takes no arguments"))
                }
            },
            [ActionToken::Id(i), rest @ ..] => id_match_branch!(self, i, usize, rest),
            _ => self.fail("expected a valid number argument"),
        }
    }

    fn parse_case<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "upper"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Case::Upper, w, rest)
            },
            [ActionToken::Word(w @ "lower"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Case::Lower, w, rest)
            },
            [ActionToken::Word(w @ "title"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Case::Title, w, rest)
            },
            [ActionToken::Word(w @ "toggle"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Case::Toggle, w, rest)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Case, rest)
            },
            _ => self.fail("expected a valid case change"),
        }
    }

    fn parse_indent_change<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "auto"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::IndentChange::Auto, w, rest)
            },
            [ActionToken::Word(w @ "increase"), rest @ ..] => {
                let count = self.parse_single_count(w, rest);
                quote! { ::editor_types::prelude::IndentChange::Increase(#count) }
            },
            [ActionToken::Word(w @ "decrease"), rest @ ..] => {
                let count = self.parse_single_count(w, rest);
                quote! { ::editor_types::prelude::IndentChange::Decrease(#count) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::IndentChange, rest)
            },
            _ => self.fail("expected a valid IndentChange"),
        }
    }

    fn parse_number_change<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "increase"), rest @ ..] => {
                let count = self.parse_single_count(w, rest);
                quote! { ::editor_types::prelude::NumberChange::Increase(#count) }
            },
            [ActionToken::Word(w @ "decrease"), rest @ ..] => {
                let count = self.parse_single_count(w, rest);
                quote! { ::editor_types::prelude::NumberChange::Decrease(#count) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::NumberChange, rest)
            },
            _ => self.fail("expected a valid NumberChange"),
        }
    }

    fn parse_join_style<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "no-change"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::JoinStyle::NoChange, w, rest)
            },
            [ActionToken::Word(w @ "one-space"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::JoinStyle::OneSpace, w, rest)
            },
            [ActionToken::Word(w @ "new-space"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::JoinStyle::NewSpace, w, rest)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::JoinStyle, rest)
            },
            _ => self.fail("expected a valid join style"),
        }
    }

    fn parse_edit_action<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "motion"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::EditAction::Motion, w, rest)
            },
            [ActionToken::Word(w @ "delete"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::EditAction::Delete, w, rest)
            },
            [ActionToken::Word(w @ "yank"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::EditAction::Yank, w, rest)
            },
            [ActionToken::Word(w @ "format"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::EditAction::Format, w, rest)
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                let virt = parse_single_flag(Flag::Long("virtual".into()), rest)
                    .map(|s| self.parse_bool(s))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::EditAction::Replace(#virt) }
            },
            [
                ActionToken::Word(w @ ("change-number" | "change-num")),
                rest @ ..,
            ] => {
                match parse_required_flags([Flag::Style, Flag::Long("multiply".into())], rest) {
                    Ok([style, multiply]) => {
                        let style = self.parse_number_change(style);
                        let multiply = self.parse_bool(multiply);
                        quote! { ::editor_types::EditAction::ChangeNumber(#style, #multiply) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Word(w @ "join"), rest @ ..] => {
                let style = parse_single_flag(Flag::Style, rest)
                    .map(|s| self.parse_join_style(s))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::EditAction::Join(#style) }
            },
            [ActionToken::Word(w @ "indent"), rest @ ..] => {
                let indent = parse_single_flag(Flag::Style, rest)
                    .map(|s| self.parse_indent_change(s))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::EditAction::Indent(#indent) }
            },
            [ActionToken::Word(w @ "change-case"), rest @ ..] => {
                let case = parse_single_flag(Flag::Style, rest)
                    .map(|s| self.parse_case(s))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::EditAction::ChangeCase(#case) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::EditAction, rest)
            },
            _ => self.fail("expected a valid edit action argument"),
        }
    }

    fn parse_specifier_edit_action<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Specifier::Contextual, w, rest)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let mark = self.parse_edit_action(rest);
                quote! { ::editor_types::prelude::Specifier::Exact(#mark) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Specifier, rest)
            },
            _ => self.fail("expected a valid edit action specifier"),
        }
    }

    fn parse_edit_target<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::EditTarget, rest)
            },
            [ActionToken::Word(_), ..] => EditTargetParserExt::parse_tokens(self, input),
            _ => self.fail("expected a valid EditTarget argument"),
        }
    }

    fn parse_motion_type<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveType, rest)
            },
            [ActionToken::Word(_), ..] => MotionParserExt::parse_tokens(self, input),
            _ => self.fail("expected a valid MoveType argument"),
        }
    }

    fn parse_range_type<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::RangeType, rest)
            },
            [ActionToken::Word(_), ..] => RangeParserExt::parse_tokens(self, input),
            _ => self.fail("expected a valid RangeType argument"),
        }
    }

    fn parse_command_type<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "application"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CommandType::Application,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "command"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CommandType::Command, w, rest)
            },
            [ActionToken::Word(w @ "content"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CommandType::Content, w, rest)
            },
            [ActionToken::Word(w @ "search"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CommandType::Search, w, rest)
            },
            [ActionToken::Word(w @ "shell"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CommandType::Shell, w, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected a valid command type, found `{w}`"))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CommandType, rest)
            },
            _ => self.fail("expected a valid command type"),
        }
    }

    fn parse_search_type<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "regex"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::SearchType::Regex, w, rest)
            },
            [ActionToken::Word(w @ "char"), rest @ ..] => {
                let multiline = parse_single_flag(Flag::Long("multiline".into()), rest)
                    .map(|b| self.parse_bool(b))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::prelude::SearchType::Char(#multiline) }
            },
            [ActionToken::Word(w @ "word"), rest @ ..] => {
                match parse_required_flags([Flag::Style, Flag::Short('b')], rest) {
                    Ok([style, boundary]) => {
                        let style = self.parse_word_style(style);
                        let boundary = self.parse_bool(boundary);
                        quote! { ::editor_types::prelude::SearchType::Word(#style, #boundary) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `regex`, `char` or `word`, found `{w}`"))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SearchType, rest)
            },
            _ => self.fail("expected a valid search type"),
        }
    }

    fn parse_completion_scope<'a>(
        &mut self,
        cmd: &str,
        input: &'a [ActionToken<'a>],
    ) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "buffer"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CompletionScope::Buffer,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "global"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CompletionScope::Global,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `buffer` or `global` after `{cmd}`, found `{w}`"))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionScope, rest)
            },
            _ => self.fail(format!("expected `buffer` or `global` after `{cmd}`")),
        }
    }

    fn parse_completion_style<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CompletionStyle::None, w, rest)
            },
            [ActionToken::Word(w @ "prefix"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CompletionStyle::Prefix,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "single"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CompletionStyle::Single,
                    w,
                    rest
                )
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
                        let dir = self.parse_dir1d(dir);
                        let toggle = self.parse_bool(toggle);
                        quote! { ::editor_types::prelude::CompletionStyle::List(#dir, #toggle) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionStyle, rest)
            },
            _ => self.fail("expected a valid completion selection"),
        }
    }

    fn parse_completion_type<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "auto"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CompletionType::Auto, w, rest)
            },
            [ActionToken::Word(w @ "file"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CompletionType::File, w, rest)
            },
            [ActionToken::Word(w @ "line"), rest @ ..] => {
                let scope = self.parse_completion_scope(w, rest);
                quote! { ::editor_types::prelude::CompletionType::Line(#scope) }
            },
            [ActionToken::Word(w @ "word"), rest @ ..] => {
                let scope = self.parse_completion_scope(w, rest);
                quote! { ::editor_types::prelude::CompletionType::Word(#scope) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionType, rest)
            },
            _ => self.fail("expected a valid completion type"),
        }
    }

    fn parse_completion_display<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CompletionDisplay::None,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "bar"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::CompletionDisplay::Bar, w, rest)
            },
            [ActionToken::Word(w @ "list"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CompletionDisplay::List,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `none`, `bar` or `list`, found `{w}`"))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionDisplay, rest)
            },
            _ => self.fail("expected a valid completion display"),
        }
    }

    fn parse_count<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Count::Contextual, w, rest)
            },
            [ActionToken::Word(w @ "ctx-sub-one"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Count::MinusOne, w, rest)
            },
            [ActionToken::Number(n), rest @ ..] => {
                if rest.is_empty() {
                    quote! { ::editor_types::prelude::Count::Exact(#n) }
                } else {
                    self.fail("numbers cannot have arguments")
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Count, rest)
            },
            _ => self.fail("expected a valid count argument"),
        }
    }

    fn parse_position_list<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "jump-list"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::PositionList::JumpList, w, rest)
            },
            [ActionToken::Word(w @ "change-list"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::PositionList::ChangeList,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `jump-list` or `change-list`, found `{w}`"))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::PositionList, rest)
            },
            _ => self.fail("expected a valid position list"),
        }
    }

    fn parse_match_action<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "keep"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MatchAction::Keep, w, rest)
            },
            [ActionToken::Word(w @ "drop"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MatchAction::Drop, w, rest)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MatchAction, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `drop` or `keep`, found `{w}`"))
            },
            _ => self.fail("expected a valid match action (`drop` or `keep`)"),
        }
    }

    fn parse_radix<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Number(2), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Binary, "2", rest)
            },
            [ActionToken::Number(8), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Octal, "8", rest)
            },
            [ActionToken::Number(10), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Decimal, "10", rest)
            },
            [ActionToken::Number(16), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Hexadecimal, "16", rest)
            },
            [ActionToken::Word(w @ ("bin" | "binary")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Binary, w, rest)
            },
            [ActionToken::Word(w @ ("oct" | "octal")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Octal, w, rest)
            },
            [ActionToken::Word(w @ ("dec" | "decimal")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Decimal, w, rest)
            },
            [ActionToken::Word(w @ ("hex" | "hexadecimal")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Radix::Hexadecimal, w, rest)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Radix, rest)
            },
            _ => self.fail("expected a valid radix argument"),
        }
    }

    fn parse_string<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Str(s), rest @ ..] => {
                if rest.is_empty() {
                    quote! { ::std::string::String::from(#s) }
                } else {
                    self.fail("strings cannot have arguments")
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::std::string::String, rest)
            },
            _ => self.fail("expected a string argument"),
        }
    }

    fn parse_target_shape<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("char" | "charwise")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::TargetShape::CharWise, w, rest)
            },
            [ActionToken::Word(w @ ("line" | "linewise")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::TargetShape::LineWise, w, rest)
            },
            [ActionToken::Word(w @ ("block" | "blockwise")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::TargetShape::BlockWise, w, rest)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::TargetShape, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `charwise`, `linewise`, or `blockwise`, found `{w}`"))
            },
            _ => self.fail("expected a valid target shape"),
        }
    }

    fn parse_dir1d<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "next"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MoveDir1D::Next, w, rest)
            },
            [ActionToken::Word(w @ ("prev" | "previous")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MoveDir1D::Previous, w, rest)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveDir1D, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `next` or `prev`, found `{w}`"))
            },
            _ => self.fail("expected one of the directions `next` or `prev`"),
        }
    }

    fn parse_dir2d<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word("up")] => quote! { ::editor_types::prelude::MoveDir2D::Up },
            [ActionToken::Word("down")] => quote! { ::editor_types::prelude::MoveDir2D::Down },
            [ActionToken::Word("left")] => quote! { ::editor_types::prelude::MoveDir2D::Left },
            [ActionToken::Word("right")] => quote! { ::editor_types::prelude::MoveDir2D::Right },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveDir2D, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `up`, `down`, `left`, or `right`, found `{w}`"))
            },
            _ => self.fail("expected one of the directions `up`, `down`, `left`, or `right`"),
        }
    }

    fn parse_move_dir_mod<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "same"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MoveDirMod::Same, w, rest)
            },
            [ActionToken::Word(w @ "flip"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MoveDirMod::Flip, w, rest)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let dir1d = self.parse_dir1d(rest);
                quote! { ::editor_types::prelude::MoveDirMod::Exact(#dir1d) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveDirMod, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `same`, `flip`, or `exact`, found `{w}`"))
            },
            _ => self.fail(
                "expected one of the directions `same`, `flip`, `(exact prev)` or `(exact next)`",
            ),
        }
    }

    fn parse_focus_change<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "current"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::FocusChange::Current, w, rest)
            },
            [
                ActionToken::Word(w @ ("prev" | "previous" | "previously-focused")),
                rest @ ..,
            ] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::FocusChange::PreviouslyFocused,
                    w,
                    rest
                )
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
                        let count = self.parse_count(count);
                        let clamp_last = self.parse_bool(clamp_last);
                        quote! { ::editor_types::prelude::FocusChange::Offset(#count, #clamp_last) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Word(w @ ("pos" | "position")), rest @ ..] => {
                match parse_single_flag(Flag::Position, rest) {
                    Ok(pos) => {
                        let pos = self.parse_move_position(pos);

                        quote! { ::editor_types::prelude::FocusChange::Position(#pos) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
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
                        let dir = self.parse_dir1d(dir);
                        let count = self.parse_count(count);
                        let wrap = self.parse_bool(wrap);
                        quote! { ::editor_types::prelude::FocusChange::Direction1D(#dir, #count, #wrap) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Word(w @ "dir2d"), rest @ ..] => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => {
                        let dir = self.parse_dir2d(dir);
                        let count = self.parse_count(count);
                        quote! { ::editor_types::prelude::FocusChange::Direction2D(#dir, #count) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::FocusChange, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!(
                    "expected `current`, `dir1d`, `dir2d`, `offset`, `pos` or `prev`, found `{w}`"
                ))
            },
            _ => self.fail("Expected a valid focus change argument"),
        }
    }

    fn parse_close_flags<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        if let [ActionToken::Id(i), rest @ ..] = input {
            return id_match_branch!(self, i, ::editor_types::prelude::CloseFlags, rest);
        }

        let mut flags = vec![];

        for token in input {
            flags.push(match token {
                ActionToken::Word("none") => quote! { ::editor_types::prelude::CloseFlags::NONE },
                ActionToken::Word("force") => quote! { ::editor_types::prelude::CloseFlags::FORCE },
                ActionToken::Word("quit") => quote! { ::editor_types::prelude::CloseFlags::QUIT },
                ActionToken::Word("write") => quote! { ::editor_types::prelude::CloseFlags::WRITE },
                t => {
                    let msg = format!("expected `none`, `force`, `quit` or `write`, found `{t}`");
                    return self.fail(msg);
                },
            });
        }

        if flags.is_empty() {
            return self.fail("Expected argument to be valid window closing flags");
        }

        quote! { #(#flags)|* }
    }

    fn parse_write_flags<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        if let [ActionToken::Id(i), rest @ ..] = input {
            return id_match_branch!(self, i, ::editor_types::prelude::WriteFlags, rest);
        }

        let mut flags = vec![];

        for token in input {
            flags.push(match token {
                ActionToken::Word("none") => quote! { ::editor_types::prelude::WriteFlags::NONE },
                ActionToken::Word("force") => quote! { ::editor_types::prelude::WriteFlags::FORCE },
                t => return self.fail(format!("expected `none` or `force`, found `{t}`")),
            });
        }

        if flags.is_empty() {
            return self.fail("Expected argument to be valid window write flags");
        }

        quote! { #(#flags)|* }
    }

    fn parse_window_target<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::WindowTarget::All, w, rest)
            },
            [ActionToken::Word("all-but"), rest @ ..] => {
                let fc = self.parse_focus_change(rest);
                quote! { ::editor_types::prelude::WindowTarget::AllBut(#fc) }
            },
            [ActionToken::Word("single"), rest @ ..] => {
                let fc = self.parse_focus_change(rest);
                quote! { ::editor_types::prelude::WindowTarget::Single(#fc) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::WindowTarget, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `all`, `all-but` or `single`, found `{w}`"))
            },
            _ => self.fail("Expected a valid window target argument"),
        }
    }

    fn parse_tab_target<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::TabTarget::All, w, rest)
            },
            [ActionToken::Word("all-but"), rest @ ..] => {
                let fc = self.parse_focus_change(rest);
                quote! { ::editor_types::prelude::TabTarget::AllBut(#fc) }
            },
            [ActionToken::Word("single"), rest @ ..] => {
                let fc = self.parse_focus_change(rest);
                quote! { ::editor_types::prelude::TabTarget::Single(#fc) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::TabTarget, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `all`, `all-but` or `single`, found `{w}`"))
            },
            _ => self.fail("Expected a valid tab target argument"),
        }
    }

    fn parse_open_target<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "alternate"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::OpenTarget::Alternate, w, rest)
            },
            [ActionToken::Word(w @ "current"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::OpenTarget::Current, w, rest)
            },
            [ActionToken::Word(w @ "selection"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::OpenTarget::Selection, w, rest)
            },
            [ActionToken::Word(w @ "unnamed"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::OpenTarget::Unnamed, w, rest)
            },
            [ActionToken::Word(w @ "cursor"), rest @ ..] => {
                let style = parse_single_flag(Flag::Style, rest)
                    .map(|s| self.parse_word_style(s))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::prelude::OpenTarget::Cursor(#style) }
            },
            [ActionToken::Word(w @ "list"), rest @ ..] => {
                let count = self.parse_single_count(w, rest);
                quote! { ::editor_types::prelude::OpenTarget::List(#count) }
            },
            [ActionToken::Word(w @ "name"), rest @ ..] => {
                let name = parse_single_flag(Flag::Input, rest)
                    .map(|s| self.parse_string(s))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::prelude::OpenTarget::Name(#name) }
            },
            [ActionToken::Word(w @ "offset"), rest @ ..] => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => {
                        let dir = self.parse_dir1d(dir);
                        let count = self.parse_count(count);
                        quote! { ::editor_types::prelude::OpenTarget::Offset(#dir, #count) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::OpenTarget, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "open target"),
            _ => self.fail("Expected a valid open target argument"),
        }
    }

    fn parse_axis<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("h" | "horizontal")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Axis::Horizontal, w, rest)
            },
            [ActionToken::Word(w @ ("v" | "vertical")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Axis::Vertical, w, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "axis"),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Axis, rest)
            },
            _ => self.fail("expected a valid axis argument"),
        }
    }

    fn parse_move_position<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::MovePosition::Beginning,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ ("m" | "middle")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MovePosition::Middle, w, rest)
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MovePosition::End, w, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "move position"),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MovePosition, rest)
            },
            _ => self.fail("expected a valid move position"),
        }
    }

    fn parse_move_terminus<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::MoveTerminus::Beginning,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::MoveTerminus::End, w, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "move terminus"),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveTerminus, rest)
            },
            _ => self.fail("expected a valid move terminus"),
        }
    }

    fn parse_scroll_size<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "cell"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::ScrollSize::Cell, w, rest)
            },
            [ActionToken::Word(w @ "half-page"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::ScrollSize::HalfPage, w, rest)
            },
            [ActionToken::Word(w @ "page"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::ScrollSize::Page, w, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "scroll size"),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::ScrollSize, rest)
            },
            _ => self.fail("expected a valid scroll size"),
        }
    }

    fn parse_size_change<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        match input {
            [ActionToken::Word("dec" | "decrease"), rest @ ..] => {
                let count = self.parse_count(rest);
                quote! { ::editor_types::prelude::SizeChange::Decrease(#count) }
            },
            [ActionToken::Word("inc" | "increase"), rest @ ..] => {
                let count = self.parse_count(rest);
                quote! { ::editor_types::prelude::SizeChange::Increase(#count) }
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let count = self.parse_count(rest);
                quote! { ::editor_types::prelude::SizeChange::Exact(#count) }
            },
            [ActionToken::Word(w @ ("eq" | "equal")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::SizeChange::Equal, w, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "size change"),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SizeChange, rest)
            },
            _ => self.fail("expected a valid size change"),
        }
    }

    fn parse_cursor_merge_style(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "union"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorMergeStyle::Union,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "intersect"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorMergeStyle::Intersect,
                    w,
                    rest
                )
            },
            [ActionToken::Word("select-cursor"), rest @ ..] => {
                let dir = self.parse_single_dir1d("select-cursor", rest);
                quote! { ::editor_types::prelude::CursorMergeStyle::SelectCursor(#dir) }
            },
            [ActionToken::Word(w @ "select-short"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorMergeStyle::SelectShort,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "select-long"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorMergeStyle::SelectLong,
                    w,
                    rest
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CursorMergeStyle, rest)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("`merge {w}` is not a valid merge style"))
            },
            _ => self.fail("expected a valid merge style for combining cursor groups"),
        }
    }

    fn parse_cursor_group_combine(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "append"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorGroupCombineStyle::Append,
                    w,
                    rest
                )
            },
            [ActionToken::Word("merge"), rest @ ..] => {
                let style = self.parse_cursor_merge_style(rest);
                quote! { ::editor_types::prelude::CursorGroupCombineStyle::Merge(#style) }
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorGroupCombineStyle::Replace,
                    w,
                    rest
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CursorGroupCombineStyle, rest)
            },
            [ActionToken::Word(w), ..] => {
                bad_word_match_branch!(self, w, "cursor group combining style")
            },
            _ => self.fail("expected a valid style for combining cursor groups"),
        }
    }

    fn parse_cursor_close_target(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "leader"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorCloseTarget::Leader,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "followers"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::CursorCloseTarget::Followers,
                    w,
                    rest
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CursorCloseTarget, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "cursor target"),
            _ => self.fail("expected a valid cursor target"),
        }
    }

    fn parse_word_style(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("alphanum" | "alpha-num")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::WordStyle::AlphaNum, w, rest)
            },
            [ActionToken::Word(w @ "big"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::WordStyle::Big, w, rest)
            },
            [ActionToken::Word(w @ ("filename" | "file-name")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::WordStyle::FileName, w, rest)
            },
            [ActionToken::Word(w @ ("filepath" | "file-path")), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::WordStyle::FilePath, w, rest)
            },
            [ActionToken::Word(w @ "little"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::WordStyle::Little, w, rest)
            },
            [
                ActionToken::Word(
                    w @ ("non-alphanum" | "nonalphanum" | "non-alphanumeric" | "nonalphanumeric"),
                ),
                rest @ ..,
            ] => {
                enum_no_args_branch!(self, ::editor_types::prelude::WordStyle::NonAlphaNum, w, rest)
            },
            [ActionToken::Word("radix"), rest @ ..] => {
                let radix = self.parse_radix(rest);
                quote! { ::editor_types::prelude::WordStyle::Number(#radix) }
            },
            [ActionToken::Word(w @ "whitespace"), rest @ ..] => {
                let wrap = parse_single_flag(Flag::Wrap, rest)
                    .map(|s| self.parse_bool(s))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::prelude::WordStyle::Whitespace(#wrap) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::WordStyle, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "word style"),
            _ => self.fail("expected a valid word style"),
        }
    }

    fn parse_keyword_target(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "selection"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::KeywordTarget::Selection,
                    w,
                    rest
                )
            },
            [ActionToken::Word("word"), rest @ ..] => {
                let style = self.parse_word_style(rest);
                quote! { ::editor_types::prelude::KeywordTarget::Word(#style) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::KeywordTarget, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "keyword target"),
            _ => self.fail("expected a valid keyword target"),
        }
    }

    fn parse_paste_style(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "cursor"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::PasteStyle::Cursor, w, rest)
            },
            [ActionToken::Word("side"), rest @ ..] => {
                let dir = self.parse_single_dir1d("side", rest);
                quote! { ::editor_types::prelude::PasteStyle::Side(#dir) }
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::PasteStyle::Replace, w, rest)
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::PasteStyle, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "paste style"),
            _ => self.fail("expected a valid paste style"),
        }
    }

    fn parse_repeat_style(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "edit-sequence"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::RepeatType::EditSequence,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "last-action"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::RepeatType::LastAction, w, rest)
            },
            [ActionToken::Word(w @ "last-selection"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::RepeatType::LastSelection,
                    w,
                    rest
                )
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::RepeatType, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "repeat style"),
            _ => self.fail("expected a valid repetition type"),
        }
    }

    fn parse_scroll_style(&mut self, input: &[ActionToken]) -> TokenStream {
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
                        let dir = self.parse_dir2d(dir);
                        let size = self.parse_scroll_size(size);
                        let count = self.parse_count(count);

                        quote! { ::editor_types::prelude::ScrollStyle::Direction2D(#dir, #size, #count) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Word(w @ "cursor-pos"), rest @ ..] => {
                match parse_flags([(Flag::Position, None), (Flag::Short('x'), None)], rest) {
                    Ok([pos, axis]) => {
                        let pos = self.parse_move_position(pos);
                        let axis = self.parse_axis(axis);

                        quote! { ::editor_types::prelude::ScrollStyle::CursorPos(#pos, #axis) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
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
                        let pos = self.parse_move_position(pos);
                        let count = self.parse_count(count);

                        quote! { ::editor_types::prelude::ScrollStyle::LinePos(#pos, #count) }
                    },
                    Err(e) => self.fail_cmd_flag(w, e),
                }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::ScrollStyle, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "scroll style"),
            _ => self.fail("expected a valid scroll style"),
        }
    }

    fn parse_mark(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "buffer-last-exited"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::BufferLastExited, w, rest)
            },
            [ActionToken::Word("buffer-named"), rest @ ..] => {
                let c = self.parse_std_char(rest);
                quote! { ::editor_types::prelude::Mark::BufferNamed(#c) }
            },
            [ActionToken::Word("global-last-exited"), rest @ ..] => {
                let n = self.parse_num(rest);
                quote! { ::editor_types::prelude::Mark::GlobalLastExited(#n) }
            },
            [ActionToken::Word("global-named"), rest @ ..] => {
                let c = self.parse_std_char(rest);
                quote! { ::editor_types::prelude::Mark::GlobalNamed(#c) }
            },
            [ActionToken::Word(w @ "last-changed"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::LastChanged, w, rest)
            },
            [ActionToken::Word(w @ "last-inserted"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::LastInserted, w, rest)
            },
            [ActionToken::Word(w @ "last-jump"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::LastJump, w, rest)
            },
            [ActionToken::Word(w @ "visual-begin"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::VisualBegin, w, rest)
            },
            [ActionToken::Word(w @ "visual-end"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::VisualEnd, w, rest)
            },
            [ActionToken::Word(w @ "last-yanked-begin"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::LastYankedBegin, w, rest)
            },
            [ActionToken::Word(w @ "last-yanked-end"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Mark::LastYankedEnd, w, rest)
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Mark, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "mark"),
            _ => self.fail("expected a valid mark"),
        }
    }

    fn parse_specifier_mark(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Specifier::Contextual, w, rest)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let mark = self.parse_mark(rest);
                quote! { ::editor_types::prelude::Specifier::Exact(#mark) }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Specifier, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "mark"),
            _ => self.fail("expected a valid mark specifier"),
        }
    }

    fn parse_char(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "copy-line"), rest @ ..] => {
                let dir = parse_single_flag(Flag::Dir, rest)
                    .map(|d| self.parse_dir1d(d))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::prelude::Char::CopyLine(#dir) }
            },
            [ActionToken::Word(w @ "ctrl-seq"), rest @ ..] => {
                let input = parse_single_flag(Flag::Input, rest)
                    .map(|d| self.parse_string(d))
                    .unwrap_or_else(|e| self.fail_cmd_flag(w, e));

                quote! { ::editor_types::prelude::Char::CtrlSeq(#input) }
            },
            [ActionToken::Word("digraph"), rest @ ..] => {
                let (c1, rest) = match rest {
                    [ActionToken::Char(c1), rest @ ..] => (quote! { #c1 }, rest),
                    [ActionToken::Id(id), rest @ ..] => {
                        (id_match_branch!(self, id, char, &rest[..0]), rest)
                    },
                    _ => return self.fail("`digraph` expects exactly two characters"),
                };

                let c2 = match rest {
                    [ActionToken::Char(c2)] => quote! { #c2 },
                    [ActionToken::Id(id), rest @ ..] => {
                        id_match_branch!(self, id, char, rest)
                    },
                    _ => return self.fail("`digraph` expects exactly two characters"),
                };

                quote! { ::editor_types::prelude::Char::Digraph(#c1, #c2) }
            },
            [ActionToken::Char(c), rest @ ..] => {
                if rest.is_empty() {
                    quote! { ::editor_types::prelude::Char::Single(#c) }
                } else {
                    self.fail("characters should not take any arguments")
                }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Char, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "character"),
            _ => self.fail("expected a digraph, character, or identifier"),
        }
    }

    fn parse_std_char(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Char(c), rest @ ..] => {
                if rest.is_empty() {
                    quote! { #c }
                } else {
                    self.fail("characters should not take any arguments")
                }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, char, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "character"),
            _ => self.fail("expected a character"),
        }
    }

    fn parse_specifier_char(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::Specifier::Contextual, w, rest)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let c = self.parse_char(rest);
                quote! { ::editor_types::prelude::Specifier::Exact(#c) }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Specifier, rest)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(self, w, "char"),
            _ => self.fail("expected a valid char"),
        }
    }

    fn parse_selection_cursor_change(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionCursorChange::Beginning,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionCursorChange::End,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "swap-anchor"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionCursorChange::SwapAnchor,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "swap-side"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionCursorChange::SwapSide,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!(
                    "expected `beginning`, `end`, `swap-anchor` or `swap-side`, found `{w}`"
                ))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SelectionCursorChange, rest)
            },
            _ => self.fail("Expected a valid selection cursor change"),
        }
    }

    fn parse_selection_resize_style(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "extend"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionResizeStyle::Extend,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "object"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionResizeStyle::Object,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "restart"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionResizeStyle::Restart,
                    w,
                    rest
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SelectionResizeStyle, rest)
            },
            _ => self.fail("Expected a valid selection resize argument"),
        }
    }

    fn parse_selection_split_style(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "anchor"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionSplitStyle::Anchor,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w @ "lines"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionSplitStyle::Lines,
                    w,
                    rest
                )
            },
            [ActionToken::Word("regex"), rest @ ..] => {
                let act = self.parse_match_action(rest);
                quote! { ::editor_types::prelude::SelectionSplitStyle::Regex(#act) }
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `anchor`, `object` or `regex`, found `{w}`"))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SelectionSplitStyle, rest)
            },
            _ => self.fail("Expected a valid selection split argument"),
        }
    }

    fn parse_selection_boundary(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "line"), rest @ ..] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionBoundary::Line,
                    w,
                    rest
                )
            },
            [
                ActionToken::Word(w @ ("non-ws" | "non-whitespace")),
                rest @ ..,
            ] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::SelectionBoundary::NonWhitespace,
                    w,
                    rest
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SelectionBoundary, rest)
            },
            _ => self.fail("expected a valid selection boundary argument"),
        }
    }

    fn parse_recall_filter(&mut self, input: &[ActionToken]) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(self, ::editor_types::prelude::RecallFilter::All, w, rest)
            },
            [
                ActionToken::Word(w @ ("prefix" | "prefix-match")),
                rest @ ..,
            ] => {
                enum_no_args_branch!(
                    self,
                    ::editor_types::prelude::RecallFilter::PrefixMatch,
                    w,
                    rest
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `all` or `prefix-match`, found `{w}`"))
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::RecallFilter, rest)
            },
            _ => self.fail("expected a valid prompt recall filter"),
        }
    }

    fn parse_target_shape_filter<'a>(&mut self, input: &'a [ActionToken<'a>]) -> TokenStream {
        if let [ActionToken::Id(i), rest @ ..] = input {
            return id_match_branch!(self, i, ::editor_types::prelude::TargetShapeFilter, rest);
        }

        let mut flags = vec![];

        for token in input {
            flags.push(match token {
                ActionToken::Word("all") => {
                    quote! { ::editor_types::prelude::TargetShapeFilter::ALL }
                },
                ActionToken::Word("none") => {
                    quote! { ::editor_types::prelude::TargetShapeFilter::NONE }
                },
                ActionToken::Word("char" | "charwise") => {
                    quote! { ::editor_types::prelude::TargetShapeFilter::CHAR }
                },
                ActionToken::Word("line" | "linewise") => {
                    quote! { ::editor_types::prelude::TargetShapeFilter::LINE }
                },
                ActionToken::Word("block" | "blockwise") => {
                    quote! { ::editor_types::prelude::TargetShapeFilter::BLOCK }
                },
                t => {
                    let msg = format!("expected a valid target shape filter, not `{t}`");
                    return self.fail(msg);
                },
            });
        }

        if flags.is_empty() {
            return self.fail("expected a valid target shape filter");
        }

        quote! { #(#flags)|* }
    }
}
