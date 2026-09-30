use super::*;

impl MotionParser for ActionMacroParser {
    type Output = TokenStream;
    type Span = Span;

    /// Output an error for the current parse.
    fn motion_invalid<T: std::fmt::Display>(&self, msg: T, span: Self::Span) -> Self::Output {
        ParseError::new(span, msg).to_compile_error()
    }

    fn visit_buffer_pos(&mut self, position: &[ActionToken], span: Self::Span) -> Self::Output {
        let pos = self.parse_move_position(position, span);
        quote! { ::editor_types::prelude::MoveType::BufferPos(#pos) }
    }

    fn visit_buffer_byte_offset(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::BufferByteOffset }
    }

    fn visit_buffer_line_offset(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::BufferLineOffset }
    }

    fn visit_buffer_line_percent(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::BufferLinePercent }
    }

    fn visit_column(
        &mut self,
        dir: &[ActionToken],
        multiline: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        let multiline = self.parse_bool(multiline, span);
        quote! { ::editor_types::prelude::MoveType::Column(#dir, #multiline) }
    }

    fn visit_final_non_blank(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::FinalNonBlank(#dir) }
    }

    fn visit_first_word(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::FirstWord(#dir) }
    }

    fn visit_item_match(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::ItemMatch }
    }

    fn visit_line(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::Line(#dir) }
    }

    fn visit_line_column_offset(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::LineColumnOffset }
    }

    fn visit_line_percent(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::LinePercent }
    }

    fn visit_line_pos(&mut self, position: &[ActionToken], span: Self::Span) -> Self::Output {
        let pos = self.parse_move_position(position, span);
        quote! { ::editor_types::prelude::MoveType::LinePos(#pos) }
    }

    fn visit_word_begin(
        &mut self,
        style: &[ActionToken],
        dir: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let style = self.parse_word_style(style, span);
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::WordBegin(#style, #dir) }
    }

    fn visit_word_end(
        &mut self,
        style: &[ActionToken],
        dir: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let style = self.parse_word_style(style, span);
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::WordEnd(#style, #dir) }
    }

    fn visit_paragraph_begin(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::ParagraphBegin(#dir) }
    }

    fn visit_sentence_begin(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::SentenceBegin(#dir) }
    }

    fn visit_section_begin(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::SectionBegin(#dir) }
    }

    fn visit_section_end(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::SectionEnd(#dir) }
    }

    fn visit_screen_first_word(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::ScreenFirstWord(#dir) }
    }

    fn visit_screen_line(&mut self, dir: &[ActionToken], span: Self::Span) -> Self::Output {
        let dir = self.parse_dir1d(dir, span);
        quote! { ::editor_types::prelude::MoveType::ScreenLine(#dir) }
    }

    fn visit_screen_line_pos(
        &mut self,
        position: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let pos = self.parse_move_position(position, span);
        quote! { ::editor_types::prelude::MoveType::ScreenLinePos(#pos) }
    }

    fn visit_viewport_pos(&mut self, position: &[ActionToken], span: Self::Span) -> Self::Output {
        let pos = self.parse_move_position(position, span);
        quote! { ::editor_types::prelude::MoveType::ViewportPos(#pos) }
    }
}

pub struct MotionMacroInput {
    args: Vec<(Ident, Expr)>,
    acts: TokenStream,
}

impl MotionMacroInput {
    pub fn into_stream(self) -> TokenStream {
        let mut act = TokenStream::new();

        for (ident, expr) in self.args {
            act.extend(quote! { let #ident = { #expr }; });
        }

        act.extend(self.acts);
        quote! { { #act } }
    }
}

impl Parse for MotionMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fmt = input.parse::<LitStr>()?;
        let fmt_str = fmt.value();
        let tokens = tokenize(&fmt_str).expect("Motion expression should be valid");
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
        let acts = MotionParserExt::parse_tokens(&mut parser, tokens.as_slice(), fmt.span());
        let generator = Self { args, acts };

        Ok(generator)
    }
}
