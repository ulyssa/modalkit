use super::*;

impl RangeParser for ActionMacroParser {
    type Output = TokenStream;
    type Span = Span;

    fn range_invalid<T: std::fmt::Display>(&self, msg: T, span: Self::Span) -> Self::Output {
        ParseError::new(span, msg).to_compile_error()
    }

    fn visit_word(&mut self, style: &[ActionToken], span: Self::Span) -> Self::Output {
        let style = self.parse_word_style(style, span);
        quote! { ::editor_types::prelude::RangeType::Word(#style) }
    }

    fn visit_buffer(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::RangeType::Buffer }
    }

    fn visit_paragraph(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::RangeType::Paragraph }
    }

    fn visit_sentence(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::RangeType::Sentence }
    }

    fn visit_line(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::RangeType::Line }
    }

    fn visit_bracketed(
        &mut self,
        left: &[ActionToken],
        right: &[ActionToken],
        span: Self::Span,
    ) -> Self::Output {
        let left = self.parse_std_char(left, span);
        let right = self.parse_std_char(right, span);
        quote! { ::editor_types::prelude::RangeType::Bracketed(#left, #right) }
    }

    fn visit_item(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::RangeType::Item }
    }

    fn visit_quote(&mut self, surround: &[ActionToken], span: Self::Span) -> Self::Output {
        let c = self.parse_std_char(surround, span);
        quote! { ::editor_types::prelude::RangeType::Quote(#c) }
    }

    fn visit_xml_tag(&mut self, _: Self::Span) -> Self::Output {
        quote! { ::editor_types::prelude::RangeType::XmlTag }
    }
}

pub struct RangeMacroInput {
    args: Vec<(Ident, Expr)>,
    acts: TokenStream,
}

impl RangeMacroInput {
    pub fn into_stream(self) -> TokenStream {
        let mut act = TokenStream::new();

        for (ident, expr) in self.args {
            act.extend(quote! { let #ident = { #expr }; });
        }

        act.extend(self.acts);
        quote! { { #act } }
    }
}

impl Parse for RangeMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fmt = input.parse::<LitStr>()?;
        let fmt_str = fmt.value();
        let tokens = tokenize(&fmt_str).expect("Range expression should be valid");
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
        let acts = RangeParserExt::parse_tokens(&mut parser, tokens.as_slice(), fmt.span());
        let generator = Self { args, acts };

        Ok(generator)
    }
}
