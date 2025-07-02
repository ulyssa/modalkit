extern crate proc_macro;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::parse::{Error as ParseError, Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Expr, Ident, LitStr, Token};

use editor_types_parser::{
    parse_flags,
    parse_single_flag,
    tokenize,
    ActionParser,
    ActionParserExt,
    ActionToken,
    ArgError,
    Flag,
    DEFAULT_COUNT,
};

#[macro_use]
mod macros;

#[inline]
fn fail<T: std::fmt::Display>(msg: T, span: Span) -> TokenStream {
    ParseError::new(span, msg).to_compile_error()
}

fn fail_cmd_flag(cmd: &str, err: ArgError, span: Span) -> TokenStream {
    match err {
        ArgError::ExpectedFlag(None) => fail(format!("`{cmd}` expects a flag argument"), span),
        ArgError::ExpectedFlag(Some(f)) => fail(format!("`{cmd}` requires a `{f}` argument"), span),
        ArgError::MissingArg(f) => {
            fail(format!("`{cmd}` expects a argument following `{f}`"), span)
        },
        ArgError::UnexpectedFlag(f) => fail(format!("`{cmd}` does not take `{f}`"), span),
        ArgError::DuplicateFlag(f) => fail(format!("`{cmd}` only takes one `{f}`"), span),
    }
}

struct ActionMacroParser {
    params: Vec<Ident>,
    pos: usize,
}

impl ActionMacroParser {
    fn advance(&mut self) -> Option<Ident> {
        if self.pos < self.params.len() {
            let res = self.params.get(self.pos).cloned();
            self.pos += 1;
            res
        } else {
            None
        }
    }

    fn parse_single_dir1d<'a>(
        &mut self,
        cmd: &str,
        input: &'a [ActionToken<'a>],
        span: Span,
    ) -> TokenStream {
        match parse_single_flag(Flag::Dir, input) {
            Ok(c) => self.parse_dir1d(c, span),
            Err(e) => fail_cmd_flag(cmd, e, span),
        }
    }

    fn parse_single_count<'a>(
        &mut self,
        cmd: &str,
        input: &'a [ActionToken<'a>],
        span: Span,
    ) -> TokenStream {
        match editor_types_parser::parse_single_count(input) {
            Ok(c) => self.parse_count(c, span),
            Err(e) => fail_cmd_flag(cmd, e, span),
        }
    }

    fn parse_single_wrap<'a>(
        &mut self,
        cmd: &str,
        input: &'a [ActionToken<'a>],
        span: Span,
    ) -> TokenStream {
        match parse_single_flag(Flag::Wrap, input) {
            Ok(c) => self.parse_bool(c, span),
            Err(e) => fail_cmd_flag(cmd, e, span),
        }
    }

    fn parse_bool<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Bool(b), rest @ ..] => {
                if rest.is_empty() {
                    quote! { #b }
                } else {
                    fail(format!("the boolean `{}` takes no arguments", b), span)
                }
            },
            [ActionToken::Id(i), rest @ ..] => id_match_branch!(self, i, ::bool, rest, span),
            _ => self.fail("expected a valid boolean argument", span),
        }
    }

    fn parse_num<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Number(n), rest @ ..] => {
                if rest.is_empty() {
                    quote! { #n }
                } else {
                    fail(format!("the number `{}` takes no arguments", n), span)
                }
            },
            [ActionToken::Id(i), rest @ ..] => id_match_branch!(self, i, ::usize, rest, span),
            _ => self.fail("expected a valid number argument", span),
        }
    }

    fn parse_edit_action<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Specifier::Contextual, w, rest, span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Specifier, rest, span)
            },
            _ => self.fail("expected a valid edit action argument", span),
        }
    }

    fn parse_edit_target<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::EditTarget, rest, span)
            },
            _ => self.fail("expected a valid edit target argument", span),
        }
    }

    fn parse_command_type<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "command"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CommandType::Command, w, rest, span)
            },
            [ActionToken::Word(w @ "search"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CommandType::Search, w, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `command` or `search`, found `{w}`"), span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CommandType, rest, span)
            },
            _ => self.fail("expected a valid command type", span),
        }
    }

    fn parse_completion_scope<'a>(
        &mut self,
        cmd: &str,
        input: &'a [ActionToken<'a>],
        span: Span,
    ) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "buffer"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CompletionScope::Buffer,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "global"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CompletionScope::Global,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `buffer or `global` after `{cmd}`, found `{w}`"), span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionScope, rest, span)
            },
            _ => self.fail(format!("expected `buffer` or `global` after `{cmd}`"), span),
        }
    }

    fn parse_completion_style<'a>(
        &mut self,
        input: &'a [ActionToken<'a>],
        span: Span,
    ) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CompletionStyle::None, w, rest, span)
            },
            [ActionToken::Word(w @ "prefix"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CompletionStyle::Prefix,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "single"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CompletionStyle::Single,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word("list"), rest @ ..] => {
                let dir = self.parse_dir1d(rest, span);
                quote! { ::editor_types::prelude::CompletionStyle::List(#dir) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionStyle, rest, span)
            },
            _ => self.fail("expected a valid completion selection", span),
        }
    }

    fn parse_completion_type<'a>(
        &mut self,
        input: &'a [ActionToken<'a>],
        span: Span,
    ) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "auto"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CompletionType::Auto, w, rest, span)
            },
            [ActionToken::Word(w @ "file"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CompletionType::File, w, rest, span)
            },
            [ActionToken::Word(w @ "line"), rest @ ..] => {
                let scope = self.parse_completion_scope(w, rest, span);
                quote! { ::editor_types::prelude::CompletionType::Line(#scope) }
            },
            [ActionToken::Word(w @ "word"), rest @ ..] => {
                let scope = self.parse_completion_scope(w, rest, span);
                quote! { ::editor_types::prelude::CompletionType::Word(#scope) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionType, rest, span)
            },
            _ => self.fail("expected a valid completion type", span),
        }
    }

    fn parse_completion_display<'a>(
        &mut self,
        input: &'a [ActionToken<'a>],
        span: Span,
    ) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CompletionDisplay::None,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "bar"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CompletionDisplay::Bar, w, rest, span)
            },
            [ActionToken::Word(w @ "list"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CompletionDisplay::List,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `none`, `bar` or `list`, found `{w}`"), span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CompletionDisplay, rest, span)
            },
            _ => self.fail("expected a valid completion display", span),
        }
    }

    fn parse_count<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Count::Contextual, w, rest, span)
            },
            [ActionToken::Word(w @ "ctx-sub-one"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Count::MinusOne, w, rest, span)
            },
            [ActionToken::Number(n), rest @ ..] => {
                if rest.is_empty() {
                    quote! { ::editor_types::prelude::Count::Exact(#n) }
                } else {
                    self.fail("numbers cannot have arguments", span)
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Count, rest, span)
            },
            _ => self.fail("expected a valid count argument", span),
        }
    }

    fn parse_position_list<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "jump-list"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::PositionList::JumpList, w, rest, span)
            },
            [ActionToken::Word(w @ "change-list"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::PositionList::ChangeList,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `jump-list` or `change-list`, found `{w}`"), span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::PositionList, rest, span)
            },
            _ => self.fail("expected a valid position list", span),
        }
    }

    fn parse_match_action<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "keep"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MatchAction::Keep, w, rest, span)
            },
            [ActionToken::Word(w @ "drop"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MatchAction::Drop, w, rest, span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MatchAction, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `drop` or `keep`, found `{w}`"), span)
            },
            _ => self.fail("expected a valid match action (`drop` or `keep`)", span),
        }
    }

    fn parse_radix<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Number(2), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Binary, "2", rest, span)
            },
            [ActionToken::Number(8), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Octal, "8", rest, span)
            },
            [ActionToken::Number(10), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Decimal, "10", rest, span)
            },
            [ActionToken::Number(16), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Hexadecimal, "16", rest, span)
            },
            [ActionToken::Word(w @ ("bin" | "binary")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Binary, w, rest, span)
            },
            [ActionToken::Word(w @ ("oct" | "octal")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Octal, w, rest, span)
            },
            [ActionToken::Word(w @ ("dec" | "decimal")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Decimal, w, rest, span)
            },
            [ActionToken::Word(w @ ("hex" | "hexadecimal")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Radix::Hexadecimal, w, rest, span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Radix, rest, span)
            },
            _ => self.fail("expected a valid count argument", span),
        }
    }

    fn parse_string<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Str(s), rest @ ..] => {
                if rest.is_empty() {
                    quote! { ::std::string::String::from(#s) }
                } else {
                    self.fail("strings cannot have arguments", span)
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::std::string::String, rest, span)
            },
            _ => self.fail("expected a string argument", span),
        }
    }

    fn parse_target_shape<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("char" | "charwise")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::TargetShape::CharWise, w, rest, span)
            },
            [ActionToken::Word(w @ ("line" | "linewise")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::TargetShape::LineWise, w, rest, span)
            },
            [ActionToken::Word(w @ ("block" | "blockwise")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::TargetShape::BlockWise, w, rest, span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::TargetShape, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(
                    format!("expected `charwise`, `linewise`, or `blockwise`, found `{w}`"),
                    span,
                )
            },
            _ => self.fail("expected a valid target shape", span),
        }
    }

    fn parse_dir1d<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "next"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MoveDir1D::Next, w, rest, span)
            },
            [ActionToken::Word(w @ ("prev" | "previous")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MoveDir1D::Previous, w, rest, span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveDir1D, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `next` or `prev`, found `{w}`"), span)
            },
            _ => self.fail("expected one of the directions `next` or `prev`", span),
        }
    }

    fn parse_dir2d<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word("up")] => quote! { ::editor_types::prelude::MoveDir2D::Up },
            [ActionToken::Word("down")] => quote! { ::editor_types::prelude::MoveDir2D::Down },
            [ActionToken::Word("left")] => quote! { ::editor_types::prelude::MoveDir2D::Left },
            [ActionToken::Word("right")] => quote! { ::editor_types::prelude::MoveDir2D::Right },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveDir2D, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `up`, `down`, `left`, or `right`, found `{w}`"), span)
            },
            _ => self.fail("expected one of the directions `up`, `down`, `left`, or `right`", span),
        }
    }

    fn parse_move_dir_mod<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "same"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MoveDirMod::Same, w, rest, span)
            },
            [ActionToken::Word(w @ "flip"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MoveDirMod::Flip, w, rest, span)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let dir1d = self.parse_dir1d(rest, span);
                quote! { ::editor_types::prelude::MoveDirMod::Exact(#dir1d) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MoveDirMod, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `same`, `flip`, or `exact`, found `{w}`"), span)
            },
            _ => self.fail(
                "expected one of the directions `same`, `flip`, `(exact prev)` or `(exact next)`",
                span,
            ),
        }
    }

    fn parse_focus_change<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "current"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::FocusChange::Current, w, rest, span)
            },
            [ActionToken::Word(w @ ("prev" | "previous" | "previously-focused")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::FocusChange::PreviouslyFocused,
                    w,
                    rest,
                    span
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
                    Ok([dir, clamp_last]) => {
                        let count = self.parse_count(dir, span);
                        let clamp_last = self.parse_bool(clamp_last, span);
                        quote! { ::editor_types::prelude::FocusChange::Offset(#count, #clamp_last) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
                }
            },
            [ActionToken::Word(w @ ("pos" | "position")), rest @ ..] => {
                match parse_single_flag(Flag::Short('p'), rest) {
                    Ok(pos) => {
                        let pos = self.parse_move_position(pos, span);

                        quote! { ::editor_types::prelude::FocusChange::Position(#pos) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
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
                        let dir = self.parse_dir1d(dir, span);
                        let count = self.parse_count(count, span);
                        let wrap = self.parse_bool(wrap, span);
                        quote! { ::editor_types::prelude::FocusChange::Direction1D(#dir, #count, #wrap) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
                }
            },
            [ActionToken::Word(w @ "dir2d"), rest @ ..] => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => {
                        let dir = self.parse_dir2d(dir, span);
                        let count = self.parse_count(count, span);
                        quote! { ::editor_types::prelude::FocusChange::Direction2D(#dir, #count) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::FocusChange, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(
                    format!(
                    "expected `current`, `dir1d, `dir2d`, `offset`, `pos` or `prev`, found `{w}`"
                ),
                    span,
                )
            },
            _ => self.fail("Expected a valid focus change argument", span),
        }
    }

    fn parse_close_flags<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CloseFlags::NONE, w, rest, span)
            },
            [ActionToken::Word(w @ "force"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CloseFlags::FORCE, w, rest, span)
            },
            [ActionToken::Word(w @ "quit"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CloseFlags::QUIT, w, rest, span)
            },
            [ActionToken::Word(w @ "write"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::CloseFlags::WRITE, w, rest, span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CloseFlags, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `none`, `force` or `quit`, found `{w}`"), span)
            },
            _ => self.fail("Expected argument to be valid window closing flags", span),
        }
    }

    fn parse_write_flags<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WriteFlags::NONE, w, rest, span)
            },
            [ActionToken::Word(w @ "force"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WriteFlags::FORCE, w, rest, span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::WriteFlags, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `none` or `force`, found `{w}`"), span)
            },
            _ => self.fail("Expected argument to be valid window write flags", span),
        }
    }

    fn parse_window_target<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WindowTarget::All, w, rest, span)
            },
            [ActionToken::Word("all-but"), rest @ ..] => {
                let fc = self.parse_focus_change(rest, span);
                quote! { ::editor_types::prelude::WindowTarget::AllBut(#fc) }
            },
            [ActionToken::Word("single"), rest @ ..] => {
                let fc = self.parse_focus_change(rest, span);
                quote! { ::editor_types::prelude::WindowTarget::Single(#fc) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::WindowTarget, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `all`, `all-but` or `single`, found `{w}`"), span)
            },
            _ => self.fail("Expected a valid window target argument", span),
        }
    }

    fn parse_tab_target<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::TabTarget::All, w, rest, span)
            },
            [ActionToken::Word("all-but"), rest @ ..] => {
                let fc = self.parse_focus_change(rest, span);
                quote! { ::editor_types::prelude::TabTarget::AllBut(#fc) }
            },
            [ActionToken::Word("single"), rest @ ..] => {
                let fc = self.parse_focus_change(rest, span);
                quote! { ::editor_types::prelude::TabTarget::Single(#fc) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::TabTarget, rest, span)
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `all`, `all-but` or `single`, found `{w}`"), span)
            },
            _ => self.fail("Expected a valid tab target argument", span),
        }
    }

    fn parse_open_target<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "alternate"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::OpenTarget::Alternate, w, rest, span)
            },
            [ActionToken::Word(w @ "current"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::OpenTarget::Current, w, rest, span)
            },
            [ActionToken::Word(w @ "selection"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::OpenTarget::Selection, w, rest, span)
            },
            [ActionToken::Word(w @ "unnamed"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::OpenTarget::Unnamed, w, rest, span)
            },
            [ActionToken::Word(w @ "cursor"), rest @ ..] => {
                let style = parse_single_flag(Flag::Style, rest)
                    .map(|s| self.parse_word_style(s, span))
                    .unwrap_or_else(|e| fail_cmd_flag(w, e, span));

                quote! { ::editor_types::prelude::OpenTarget::Cursor(#style) }
            },
            [ActionToken::Word(w @ "list"), rest @ ..] => {
                let count = self.parse_single_count(w, rest, span);
                quote! { ::editor_types::prelude::OpenTarget::List(#count) }
            },
            [ActionToken::Word(w @ "name"), rest @ ..] => {
                let name = parse_single_flag(Flag::Input, rest)
                    .map(|s| self.parse_string(s, span))
                    .unwrap_or_else(|e| fail_cmd_flag(w, e, span));

                quote! { ::editor_types::prelude::OpenTarget::Name(#name) }
            },
            [ActionToken::Word(w @ "offset"), rest @ ..] => {
                match parse_flags(
                    [(Flag::Dir, None), (Flag::Count, Some(&DEFAULT_COUNT[..]))],
                    rest,
                ) {
                    Ok([dir, count]) => {
                        let dir = self.parse_dir1d(dir, span);
                        let count = self.parse_count(count, span);
                        quote! { ::editor_types::prelude::OpenTarget::Offset(#dir, #count) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
                }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::OpenTarget, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "open target", span),
            _ => self.fail("Expected a valid open target argument", span),
        }
    }

    fn parse_axis<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("h" | "horizontal")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Axis::Horizontal, w, rest, span)
            },
            [ActionToken::Word(w @ ("v" | "vertical")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Axis::Vertical, w, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "axis", span),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::Axis, rest, span)
            },
            _ => self.fail("expected a valid axis argument", span),
        }
    }

    fn parse_move_position<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::MovePosition::Beginning,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ ("m" | "middle")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MovePosition::Middle, w, rest, span)
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::MovePosition::End, w, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "move position", span),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::MovePosition, rest, span)
            },
            _ => self.fail("expected a valid move position", span),
        }
    }

    fn parse_scroll_size<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "cell"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::ScrollSize::Cell, w, rest, span)
            },
            [ActionToken::Word(w @ "half-page"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::ScrollSize::HalfPage, w, rest, span)
            },
            [ActionToken::Word(w @ "page"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::ScrollSize::Page, w, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "scroll size", span),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::ScrollSize, rest, span)
            },
            _ => self.fail("expected a valid scroll size", span),
        }
    }

    fn parse_size_change<'a>(&mut self, input: &'a [ActionToken<'a>], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word("dec" | "decrease"), rest @ ..] => {
                let count = self.parse_count(rest, span);
                quote! { ::editor_types::prelude::SizeChange::Decrease(#count) }
            },
            [ActionToken::Word("inc" | "increase"), rest @ ..] => {
                let count = self.parse_count(rest, span);
                quote! { ::editor_types::prelude::SizeChange::Increase(#count) }
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let count = self.parse_count(rest, span);
                quote! { ::editor_types::prelude::SizeChange::Exact(#count) }
            },
            [ActionToken::Word(w @ ("eq" | "equal")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::SizeChange::Equal, w, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "size change", span),
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SizeChange, rest, span)
            },
            _ => self.fail("expected a valid size change", span),
        }
    }

    fn parse_cursor_merge_style(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "union"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorMergeStyle::Union,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "intersect"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorMergeStyle::Intersect,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word("select-cursor"), rest @ ..] => {
                let dir = self.parse_single_dir1d("select-cursor", rest, span);
                quote! { ::editor_types::prelude::CursorMergeStyle::SelectCursor(#dir) }
            },
            [ActionToken::Word(w @ "select-short"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorMergeStyle::SelectShort,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "select-long"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorMergeStyle::SelectLong,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("`merge {w}` is not a valid merge style"), span)
            },
            _ => self.fail("expected a valid merge style for combining cursor groups", span),
        }
    }

    fn parse_cursor_group_combine(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "append"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorGroupCombineStyle::Append,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word("merge"), rest @ ..] => {
                let style = self.parse_cursor_merge_style(rest, span);
                quote! { ::editor_types::prelude::CursorGroupCombineStyle::Merge(#style) }
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorGroupCombineStyle::Replace,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(
                    self,
                    i,
                    ::editor_types::prelude::CursorGroupCombineStyle,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w), ..] => {
                bad_word_match_branch!(w, "cursor group combining style", span)
            },
            _ => self.fail("expected a valid style for combining cursor groups", span),
        }
    }

    fn parse_cursor_close_target(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "leader"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorCloseTarget::Leader,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "followers"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::CursorCloseTarget::Followers,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::CursorCloseTarget, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "cursor target", span),
            _ => self.fail("expected a valid cursor target", span),
        }
    }

    fn parse_word_style(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("alphanum" | "alpha-num")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WordStyle::AlphaNum, w, rest, span)
            },
            [ActionToken::Word(w @ "big"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WordStyle::Big, w, rest, span)
            },
            [ActionToken::Word(w @ ("filename" | "file-name")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WordStyle::FileName, w, rest, span)
            },
            [ActionToken::Word(w @ ("filepath" | "file-path")), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WordStyle::FilePath, w, rest, span)
            },
            [ActionToken::Word(w @ "little"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WordStyle::Little, w, rest, span)
            },
            [ActionToken::Word(
                w @ ("non-alphanum" | "nonalphanum" | "non-alphanumeric" | "nonalphanumeric"),
            ), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::WordStyle::NonAlphaNum, w, rest, span)
            },
            [ActionToken::Word("radix"), rest @ ..] => {
                let radix = self.parse_radix(rest, span);
                quote! { ::editor_types::prelude::WordStyle::Number(#radix) }
            },
            [ActionToken::Word("whitespace"), rest @ ..] => {
                let wrap = self.parse_single_wrap("whitespace", rest, span);
                quote! { ::editor_types::prelude::WordStyle::Whitespace(#wrap) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::WordStyle, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "word style", span),
            _ => self.fail("expected a valid word style", span),
        }
    }

    fn parse_keyword_target(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "selection"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::KeywordTarget::Selection,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word("word"), rest @ ..] => {
                let style = self.parse_word_style(rest, span);
                quote! { ::editor_types::prelude::KeywordTarget::Word(#style) }
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::KeywordTarget, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "keyword target", span),
            _ => self.fail("expected a valid keyword target", span),
        }
    }

    fn parse_paste_style(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "cursor"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::PasteStyle::Cursor, w, rest, span)
            },
            [ActionToken::Word("side"), rest @ ..] => {
                let dir = self.parse_single_dir1d("side", rest, span);
                quote! { ::editor_types::prelude::PasteStyle::Side(#dir) }
            },
            [ActionToken::Word(w @ "replace"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::PasteStyle::Replace, w, rest, span)
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::PasteStyle, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "paste style", span),
            _ => self.fail("expected a valid paste style", span),
        }
    }

    fn parse_repeat_style(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "edit-sequence"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::RepeatType::EditSequence,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "last-action"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::RepeatType::LastAction, w, rest, span)
            },
            [ActionToken::Word(w @ "last-selection"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::RepeatType::LastSelection,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::RepeatType, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "repeat style", span),
            _ => self.fail("expected a valid repetition type", span),
        }
    }

    fn parse_scroll_style(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
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
                        let dir = self.parse_dir2d(dir, span);
                        let size = self.parse_scroll_size(size, span);
                        let count = self.parse_count(count, span);

                        quote! { ::editor_types::prelude::ScrollStyle::Direction2D(#dir, #size, #count) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
                }
            },
            [ActionToken::Word(w @ "cursor-pos"), rest @ ..] => {
                match parse_flags([(Flag::Short('p'), None), (Flag::Short('x'), None)], rest) {
                    Ok([pos, axis]) => {
                        let pos = self.parse_move_position(pos, span);
                        let axis = self.parse_axis(axis, span);

                        quote! { ::editor_types::prelude::ScrollStyle::CursorPos(#pos, #axis) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
                }
            },
            [ActionToken::Word(w @ "line-pos"), rest @ ..] => {
                match parse_flags(
                    [
                        (Flag::Short('p'), None),
                        (Flag::Count, Some(&DEFAULT_COUNT[..])),
                    ],
                    rest,
                ) {
                    Ok([pos, count]) => {
                        let pos = self.parse_move_position(pos, span);
                        let count = self.parse_count(count, span);

                        quote! { ::editor_types::prelude::ScrollStyle::LinePos(#pos, #count) }
                    },
                    Err(e) => fail_cmd_flag(w, e, span),
                }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::ScrollStyle, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "scroll style", span),
            _ => self.fail("expected a valid scroll style", span),
        }
    }

    fn parse_mark(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "buffer-last-exited"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::BufferLastExited, w, rest, span)
            },
            [ActionToken::Word("buffer-named"), rest @ ..] => {
                let c = self.parse_std_char(rest, span);
                quote! { ::editor_types::prelude::Mark::BufferNamed(#c) }
            },
            [ActionToken::Word("global-last-exited"), rest @ ..] => {
                let n = self.parse_num(rest, span);
                quote! { ::editor_types::prelude::Mark::GlobalLastExited(#n) }
            },
            [ActionToken::Word("global-named"), rest @ ..] => {
                let c = self.parse_std_char(rest, span);
                quote! { ::editor_types::prelude::Mark::GlobalNamed(#c) }
            },
            [ActionToken::Word(w @ "last-changed"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::LastChanged, w, rest, span)
            },
            [ActionToken::Word(w @ "last-inserted"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::LastInserted, w, rest, span)
            },
            [ActionToken::Word(w @ "last-jump"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::LastJump, w, rest, span)
            },
            [ActionToken::Word(w @ "visual-begin"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::VisualBegin, w, rest, span)
            },
            [ActionToken::Word(w @ "visual-end"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::VisualEnd, w, rest, span)
            },
            [ActionToken::Word(w @ "last-yanked-begin"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::LastYankedBegin, w, rest, span)
            },
            [ActionToken::Word(w @ "last-yanked-end"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Mark::LastYankedEnd, w, rest, span)
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Mark, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "mark", span),
            _ => self.fail("expected a valid mark", span),
        }
    }

    fn parse_specifier_mark(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Specifier::Contextual, w, rest, span)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let mark = self.parse_mark(rest, span);
                quote! { ::editor_types::prelude::Specifier::Exact(#mark) }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Specifier, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "mark", span),
            _ => self.fail("expected a valid mark specifier", span),
        }
    }

    fn parse_char(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word("digraph"), rest @ ..] => {
                let (c1, rest) = match rest {
                    [ActionToken::Char(c1), rest @ ..] => (quote! { #c1 }, rest),
                    [ActionToken::Id(id), rest @ ..] => {
                        (id_match_branch!(self, id, ::char, &rest[..0], span), rest)
                    },
                    _ => return self.fail("`digraph` expects exactly two characters", span),
                };

                let c2 = match rest {
                    [ActionToken::Char(c2)] => quote! { #c2 },
                    [ActionToken::Id(id), rest @ ..] => {
                        id_match_branch!(self, id, ::char, rest, span)
                    },
                    _ => return self.fail("`digraph` expects exactly two characters", span),
                };

                quote! { ::editor_types::prelude::Char::Digraph(#c1, #c2) }
            },
            [ActionToken::Char(c), rest @ ..] => {
                if rest.is_empty() {
                    quote! { ::editor_types::prelude::Char::Single(#c) }
                } else {
                    self.fail("characters should not take any arguments", span)
                }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Char, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "character", span),
            _ => self.fail("expected a digraph, character, or identifier", span),
        }
    }

    fn parse_std_char(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Char(c), rest @ ..] => {
                if rest.is_empty() {
                    quote! { #c }
                } else {
                    self.fail("characters should not take any arguments", span)
                }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::char, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "character", span),
            _ => self.fail("expected a character", span),
        }
    }

    fn parse_specifier_char(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "ctx"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::Specifier::Contextual, w, rest, span)
            },
            [ActionToken::Word("exact"), rest @ ..] => {
                let c = self.parse_char(rest, span);
                quote! { ::editor_types::prelude::Specifier::Exact(#c) }
            },
            [ActionToken::Id(id), rest @ ..] => {
                id_match_branch!(self, id, ::editor_types::prelude::Specifier, rest, span)
            },
            [ActionToken::Word(w), ..] => bad_word_match_branch!(w, "char", span),
            _ => self.fail("expected a valid char", span),
        }
    }

    fn parse_selection_cursor_change(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ ("b" | "beginning")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionCursorChange::Beginning,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ ("e" | "end")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionCursorChange::End,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "swap-anchor"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionCursorChange::SwapAnchor,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "swap-side"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionCursorChange::SwapSide,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(
                    format!(
                        "expected `beginning`, `end`, `swap-anchor` or `swap-side`, found `{w}`"
                    ),
                    span,
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(
                    self,
                    i,
                    ::editor_types::prelude::SelectionCursorChange,
                    rest,
                    span
                )
            },
            _ => self.fail("Expected a valid selection cursor change", span),
        }
    }

    fn parse_selection_resize_style(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "extend"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionResizeStyle::Extend,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "object"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionResizeStyle::Object,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "restart"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionResizeStyle::Restart,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SelectionResizeStyle, rest, span)
            },
            _ => self.fail("Expected a valid selection resize argument", span),
        }
    }

    fn parse_selection_split_style(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "anchor"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionSplitStyle::Anchor,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "lines"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionSplitStyle::Lines,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word("regex"), rest @ ..] => {
                let act = self.parse_match_action(rest, span);
                quote! { ::editor_types::prelude::SelectionSplitStyle::Regex(#act) }
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `anchor`, `object` or `regex`, found `{w}`"), span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SelectionSplitStyle, rest, span)
            },
            _ => self.fail("Expected a valid selection split argument", span),
        }
    }

    fn parse_selection_boundary(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "line"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionBoundary::Line,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ ("non-ws" | "non-whitespace")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::SelectionBoundary::NonWhitespace,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::SelectionBoundary, rest, span)
            },
            _ => self.fail("expected a valid selection boundary argument", span),
        }
    }

    fn parse_recall_filter(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::RecallFilter::All, w, rest, span)
            },
            [ActionToken::Word(w @ ("prefix" | "prefix-match")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::RecallFilter::PrefixMatch,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w), ..] => {
                self.fail(format!("expected `all` or `prefix-match`, found `{w}`"), span)
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::RecallFilter, rest, span)
            },
            _ => self.fail("expected a valid prompt recall filter", span),
        }
    }

    fn parse_target_shape_filter(&mut self, input: &[ActionToken], span: Span) -> TokenStream {
        match input {
            [ActionToken::Word(w @ "all"), rest @ ..] => {
                enum_no_args_branch!(::editor_types::prelude::TargetShapeFilter::ALL, w, rest, span)
            },
            [ActionToken::Word(w @ ("char" | "charwise")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::TargetShapeFilter::CHAR,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ ("line" | "linewise")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::TargetShapeFilter::LINE,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ ("block" | "blockwise")), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::TargetShapeFilter::BLOCK,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Word(w @ "none"), rest @ ..] => {
                enum_no_args_branch!(
                    ::editor_types::prelude::TargetShapeFilter::NONE,
                    w,
                    rest,
                    span
                )
            },
            [ActionToken::Id(i), rest @ ..] => {
                id_match_branch!(self, i, ::editor_types::prelude::TargetShapeFilter, rest, span)
            },
            _ => self.fail("expected a valid target shape filter", span),
        }
    }
}

impl ActionParser for ActionMacroParser {
    type Output = TokenStream;
    type Span = Span;

    fn fail<T: std::fmt::Display>(&self, msg: T, span: Self::Span) -> Self::Output {
        ParseError::new(span, msg).to_compile_error()
    }

    fn visit_keyword_lookup(&mut self, target: &[ActionToken], span: Self::Span) -> Self::Output {
        let target = self.parse_keyword_target(target, span);
        quote! { ::editor_types::Action::KeywordLookup(#target) }
    }

    fn visit_noop(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::Action::NoOp }
    }

    fn visit_redraw_screen(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::Action::RedrawScreen }
    }

    fn visit_suspend(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::Action::Suspend }
    }

    fn visit_cmdbar_focus(
        &mut self,
        prompt: &[ActionToken],
        cmdtype: &[ActionToken],
        action: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let prompt = self.parse_string(prompt, span);
        let cmdtype = self.parse_command_type(cmdtype, span);
        let action = self.parse_action(action, span);

        quote! {
            ::editor_types::Action::CommandBar(
                ::editor_types::CommandBarAction::Focus(#prompt, #cmdtype, Box::new(#action))
            )
        }
    }

    fn visit_cmdbar_unfocus(&mut self, _: Self::Span) -> Self::Output {
        quote! {
            ::editor_types::Action::CommandBar(
                ::editor_types::CommandBarAction::Unfocus
            )
        }
    }

    fn visit_command_execute(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output {
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Command(
                ::editor_types::CommandAction::Execute(#count)
            )
        }
    }

    fn visit_command_run(&mut self, input: &[ActionToken], span: Self::Span) -> Self::Output {
        let input = self.parse_string(input, span);

        quote! {
            ::editor_types::Action::Command(
                ::editor_types::CommandAction::Run(#input)
            )
        }
    }

    fn visit_complete(
        &mut self,
        style: &[ActionToken],
        comptype: &[ActionToken],
        display: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let comptype = self.parse_completion_type(comptype, span);
        let style = self.parse_completion_style(style, span);
        let display = self.parse_completion_display(display, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Complete(#style, #comptype, #display)
            )
        }
    }

    fn visit_history_checkpoint(&mut self, _: Self::Span) -> Self::Output {
        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::History(
                    ::editor_types::HistoryAction::Checkpoint
                )
            )
        }
    }

    fn visit_history_undo(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output {
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::History(
                    ::editor_types::HistoryAction::Undo(#count)
                )
            )
        }
    }

    fn visit_edit(
        &mut self,
        action: &[ActionToken],
        target: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let action = self.parse_edit_action(action, span);
        let target = self.parse_edit_target(target, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Edit(#action, #target)
            )
        }
    }

    fn visit_history_redo(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output {
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::History(
                    ::editor_types::HistoryAction::Redo(#count)
                )
            )
        }
    }

    fn visit_macro_execute(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output {
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::Execute(#count)
            )
        }
    }

    fn visit_macro_run(
        &mut self,
        input: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let input = self.parse_string(input, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::Run(#input, #count)
            )
        }
    }

    fn visit_macro_repeat(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output {
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::Repeat(#count)
            )
        }
    }

    fn visit_macro_toggle_recording(&mut self, _: Self::Span) -> Self::Output {
        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::ToggleRecording
            )
        }
    }

    fn visit_prompt_abort(&mut self, _: Self::Span) -> Self::Output {
        quote! {
            ::editor_types::Action::Prompt(
                ::editor_types::PromptAction::Abort(false)
            )
        }
    }

    fn visit_prompt_recall(
        &mut self,
        filter: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let filter = self.parse_recall_filter(filter, span);
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Prompt(
                ::editor_types::PromptAction::Recall(#filter, #dir, #count)
            )
        }
    }

    fn visit_prompt_submit(&mut self, _: Self::Span) -> Self::Output {
        quote! {
            ::editor_types::Action::Prompt(
                ::editor_types::PromptAction::Submit
            )
        }
    }

    fn visit_mark(&mut self, mark: &[ActionToken], span: Self::Span) -> Self::Output {
        let mark = self.parse_specifier_mark(mark, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Mark(#mark)
            )
        }
    }

    fn visit_tab_close(
        &mut self,
        target: &[ActionToken],
        flags: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let target = self.parse_tab_target(target, span);
        let flags = self.parse_close_flags(flags, span);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Close(#target, #flags)
            )
        }
    }

    fn visit_tab_extract(
        &mut self,
        fc: &[ActionToken],
        dir: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let fc = self.parse_focus_change(fc, span);
        let dir = self.parse_dir1d(dir, span);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Extract(#fc, #dir)
            )
        }
    }

    fn visit_tab_open(
        &mut self,
        target: &[ActionToken],
        fc: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let target = self.parse_open_target(target, span);
        let fc = self.parse_focus_change(fc, span);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Open(#target, #fc)
            )
        }
    }

    fn visit_tab_focus(&mut self, fc: &[ActionToken], span: Span) -> Self::Output {
        let fc = self.parse_focus_change(fc, span);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Focus(#fc)
            )
        }
    }

    fn visit_tab_move(&mut self, fc: &[ActionToken], span: Span) -> Self::Output {
        let fc = self.parse_focus_change(fc, span);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Move(#fc)
            )
        }
    }

    fn visit_cursor_close(&mut self, target: &[ActionToken], span: Self::Span) -> Self::Output {
        let target = self.parse_cursor_close_target(target, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                     ::editor_types::CursorAction::Close(#target)
                )
            )
        }
    }

    fn visit_cursor_restore(&mut self, style: &[ActionToken], span: Self::Span) -> Self::Output {
        let style = self.parse_cursor_group_combine(style, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                    ::editor_types::CursorAction::Restore(#style)
                )
            )
        }
    }

    fn visit_cursor_rotate(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                    ::editor_types::CursorAction::Rotate(#dir, #count)
                )
            )
        }
    }

    fn visit_cursor_save(&mut self, style: &[ActionToken], span: Self::Span) -> Self::Output {
        let style = self.parse_cursor_group_combine(style, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                    ::editor_types::CursorAction::Save(#style)
                )
            )
        }
    }

    fn visit_cursor_split(&mut self, count: &[ActionToken], span: Self::Span) -> Self::Output {
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                    ::editor_types::CursorAction::Split(#count)
                )
            )
        }
    }

    fn visit_window_close(
        &mut self,
        target: &[ActionToken],
        flags: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let target = self.parse_window_target(target, span);
        let flags = self.parse_close_flags(flags, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Close(#target, #flags)
            )
        }
    }

    fn visit_window_open(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let target = self.parse_open_target(target, span);
        let axis = self.parse_axis(axis, span);
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Open(#target, #axis, #dir, #count)
            )
        }
    }

    fn visit_window_resize(
        &mut self,
        fc: &[ActionToken],
        axis: &[ActionToken],
        size: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let fc = self.parse_focus_change(fc, span);
        let axis = self.parse_axis(axis, span);
        let size = self.parse_size_change(size, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Resize(#fc, #axis, #size)
            )
        }
    }

    fn visit_window_split(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let target = self.parse_open_target(target, span);
        let axis = self.parse_axis(axis, span);
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Split(#target, #axis, #dir, #count)
            )
        }
    }

    fn visit_window_switch(&mut self, target: &[ActionToken], span: Span) -> Self::Output {
        let target = self.parse_open_target(target, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Switch(#target)
            )
        }
    }

    fn visit_window_write(
        &mut self,
        target: &[ActionToken],
        flags: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let target = self.parse_window_target(target, span);
        let flags = self.parse_write_flags(flags, span);
        let name = quote! { ::std::option::Option::None };

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Write(#target, #name, #flags)
            )
        }
    }

    fn visit_window_exchange(&mut self, fc: &[ActionToken], span: Self::Span) -> Self::Output {
        let fc = self.parse_focus_change(fc, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Exchange(#fc)
            )
        }
    }

    fn visit_window_focus(&mut self, fc: &[ActionToken], span: Self::Span) -> Self::Output {
        let fc = self.parse_focus_change(fc, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Focus(#fc)
            )
        }
    }

    fn visit_window_move_side(&mut self, dir: &[ActionToken], span: Span) -> Self::Output {
        let dir = self.parse_dir2d(dir, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::MoveSide(#dir)
            )
        }
    }

    fn visit_window_rotate(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Rotate(#dir)
            )
        }
    }

    fn visit_window_clear_sizes(&mut self, _: Self::Span) -> Self::Output {
        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::ClearSizes
            )
        }
    }

    fn visit_window_zoom_toggle(&mut self, _: Self::Span) -> Self::Output {
        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::ZoomToggle
            )
        }
    }

    fn visit_insert_open_line(
        &mut self,
        shape: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let shape = self.parse_target_shape(shape, span);
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::InsertText(
                    ::editor_types::InsertTextAction::OpenLine(#shape, #dir, #count)
                )
            )
        }
    }

    fn visit_insert_transcribe(
        &mut self,
        input: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let input = self.parse_string(input, span);
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::InsertText(
                    ::editor_types::InsertTextAction::Transcribe(#input, #dir, #count)
                )
            )
        }
    }

    fn visit_insert_type(
        &mut self,
        c: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let c = self.parse_specifier_char(c, span);
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::InsertText(
                    ::editor_types::InsertTextAction::Type(#c, #dir, #count)
                )
            )
        }
    }

    fn visit_insert_paste(
        &mut self,
        style: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let style = self.parse_paste_style(style, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::InsertText(
                    ::editor_types::InsertTextAction::Paste(#style, #count)
                )
            )
        }
    }

    fn visit_jump(
        &mut self,
        list: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let list = self.parse_position_list(list, span);
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Jump(#list, #dir, #count)
        }
    }

    fn visit_repeat(&mut self, style: &[ActionToken], span: Span) -> Self::Output {
        let style = self.parse_repeat_style(style, span);

        quote! { ::editor_types::Action::Repeat(#style) }
    }

    fn visit_scroll(&mut self, style: &[ActionToken], span: Span) -> Self::Output {
        let style = self.parse_scroll_style(style, span);

        quote! { ::editor_types::Action::Scroll(#style) }
    }

    fn visit_search(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let dir = self.parse_move_dir_mod(dir, span);
        let count = self.parse_count(count, span);

        quote! { ::editor_types::Action::Search(#dir, #count) }
    }

    fn visit_selection_duplicate(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        let count = self.parse_count(count, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                    ::editor_types::SelectionAction::Duplicate(#dir, #count)
                )
            )
        }
    }

    fn visit_selection_cursor_set(&mut self, change: &[ActionToken], span: Span) -> Self::Output {
        let change = self.parse_selection_cursor_change(change, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                    ::editor_types::SelectionAction::CursorSet(#change)
                )
            )
        }
    }

    fn visit_selection_expand(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let boundary = self.parse_selection_boundary(boundary, span);
        let target = self.parse_target_shape_filter(target, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                    ::editor_types::SelectionAction::Expand(#boundary, #target)
                )
            )
        }
    }

    fn visit_selection_filter(&mut self, act: &[ActionToken], span: Span) -> Self::Output {
        let act = self.parse_match_action(act, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                    ::editor_types::SelectionAction::Filter(#act)
                )
            )
        }
    }

    fn visit_selection_resize(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let style = self.parse_selection_resize_style(style, span);
        let target = self.parse_edit_target(target, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                     ::editor_types::SelectionAction::Resize(#style, #target)
                )
            )
        }
    }

    fn visit_selection_split(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let style = self.parse_selection_split_style(style, span);
        let target = self.parse_target_shape_filter(target, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                     ::editor_types::SelectionAction::Split(#style, #target)
                )
            )
        }
    }

    fn visit_selection_join(&mut self, _: Span) -> Self::Output {
        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                     ::editor_types::SelectionAction::Join
                )
            )
        }
    }

    fn visit_selection_trim(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
        span: Span,
    ) -> Self::Output {
        let boundary = self.parse_selection_boundary(boundary, span);
        let target = self.parse_target_shape_filter(target, span);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                     ::editor_types::SelectionAction::Trim(#boundary, #target)
                )
            )
        }
    }
}

struct ActionMacroInput {
    args: Vec<(Ident, Expr)>,
    acts: TokenStream,
}

impl ActionMacroInput {
    fn into_stream(self) -> TokenStream {
        let mut act = TokenStream::new();

        for (ident, expr) in self.args {
            act.extend(quote! { let #ident = { #expr }; });
        }

        act.extend(self.acts);
        quote! { { #act } }
    }
}

impl Parse for ActionMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fmt = input.parse::<LitStr>()?;
        let fmt_str = fmt.value();
        let tokens = tokenize(&fmt_str).expect("Action expression should be valid");
        let arg_exprs = if input.parse::<Token![,]>().is_ok() {
            Punctuated::<Expr, Token![,]>::parse_separated_nonempty(input)?
        } else {
            Punctuated::new()
        };
        let mut idents = vec![];
        let mut args = vec![];

        for (i, arg) in arg_exprs.into_iter().enumerate() {
            let ident = format_ident!("arg{i}");
            idents.push(ident.clone());
            args.push((ident, arg));
        }

        let mut parser = ActionMacroParser { params: idents, pos: 0 };
        let acts = parser.parse_action(tokens.as_slice(), fmt.span());
        let generator = Self { args, acts };

        Ok(generator)
    }
}

#[proc_macro]
pub fn action(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(stream as ActionMacroInput);
    input.into_stream().into()
}
