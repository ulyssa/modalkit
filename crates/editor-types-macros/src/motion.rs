use super::*;

impl MotionParser for ActionMacroParser {
    type Output = TokenStream;

    /// Output an error for the current parse.
    fn motion_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output {
        ParseError::new(self.span, msg).to_compile_error()
    }

    fn visit_buffer_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = self.parse_move_position(position);
        quote! { ::editor_types::prelude::MoveType::BufferPos(#pos) }
    }

    fn visit_buffer_byte_offset(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::BufferByteOffset }
    }

    fn visit_buffer_line_offset(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::BufferLineOffset }
    }

    fn visit_buffer_line_percent(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::BufferLinePercent }
    }

    fn visit_column(&mut self, dir: &[ActionToken], multiline: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        let multiline = self.parse_bool(multiline);
        quote! { ::editor_types::prelude::MoveType::Column(#dir, #multiline) }
    }

    fn visit_final_non_blank(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::FinalNonBlank(#dir) }
    }

    fn visit_first_word(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::FirstWord(#dir) }
    }

    fn visit_item_match(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::ItemMatch }
    }

    fn visit_line(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::Line(#dir) }
    }

    fn visit_line_column_offset(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::LineColumnOffset }
    }

    fn visit_line_percent(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::MoveType::LinePercent }
    }

    fn visit_line_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = self.parse_move_position(position);
        quote! { ::editor_types::prelude::MoveType::LinePos(#pos) }
    }

    fn visit_word_begin(&mut self, style: &[ActionToken], dir: &[ActionToken]) -> Self::Output {
        let style = self.parse_word_style(style);
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::WordBegin(#style, #dir) }
    }

    fn visit_word_end(&mut self, style: &[ActionToken], dir: &[ActionToken]) -> Self::Output {
        let style = self.parse_word_style(style);
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::WordEnd(#style, #dir) }
    }

    fn visit_paragraph_begin(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::ParagraphBegin(#dir) }
    }

    fn visit_sentence_begin(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::SentenceBegin(#dir) }
    }

    fn visit_section_begin(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::SectionBegin(#dir) }
    }

    fn visit_section_end(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::SectionEnd(#dir) }
    }

    fn visit_screen_first_word(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::ScreenFirstWord(#dir) }
    }

    fn visit_screen_line(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        quote! { ::editor_types::prelude::MoveType::ScreenLine(#dir) }
    }

    fn visit_screen_line_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = self.parse_move_position(position);
        quote! { ::editor_types::prelude::MoveType::ScreenLinePos(#pos) }
    }

    fn visit_viewport_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = self.parse_move_position(position);
        quote! { ::editor_types::prelude::MoveType::ViewportPos(#pos) }
    }
}

pub struct MotionMacroInput {
    args: Vec<(Ident, Expr)>,
    acts: TokenStream,
}

impl MotionMacroInput {
    pub fn into_stream(self) -> TokenStream {
        let mut act = bind_args(self.args);

        act.extend(self.acts);
        quote! { { #act } }
    }
}

impl Parse for MotionMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fmt = input.parse::<LitStr>()?;
        let fmt_str = fmt.value();
        let mut tokens = tokenize_fmt("motion!", &fmt_str, fmt.span())?;
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

        let mut parser = ActionMacroParser { params: idents, pos: 0, span: fmt.span() };
        parser.bind_positional(tokens.as_mut_slice())?;

        let acts = MotionParserExt::parse_tokens(&mut parser, tokens.as_slice());
        let generator = Self { args, acts };

        Ok(generator)
    }
}
