use super::*;

impl EditTargetParser for ActionMacroParser {
    type Output = TokenStream;

    fn edit_target_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output {
        ParseError::new(self.span, msg).to_compile_error()
    }

    fn visit_boundary(
        &mut self,
        range: &[ActionToken],
        inclusive: &[ActionToken],
        terminus: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let range = self.parse_range_type(range);
        let inclusive = self.parse_bool(inclusive);
        let terminus = self.parse_move_terminus(terminus);
        let count = self.parse_count(count);
        quote! { ::editor_types::prelude::EditTarget::Boundary(#range, #inclusive, #terminus, #count) }
    }

    fn visit_current_position(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::EditTarget::CurrentPosition }
    }

    fn visit_char_jump(&mut self, mark: &[ActionToken]) -> Self::Output {
        let mark = self.parse_specifier_mark(mark);
        quote! { ::editor_types::prelude::EditTarget::CharJump(#mark) }
    }

    fn visit_line_jump(&mut self, mark: &[ActionToken]) -> Self::Output {
        let mark = self.parse_specifier_mark(mark);
        quote! { ::editor_types::prelude::EditTarget::LineJump(#mark) }
    }

    fn visit_motion(&mut self, motion: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let motion = self.parse_motion_type(motion);
        let count = self.parse_count(count);
        quote! { ::editor_types::prelude::EditTarget::Motion(#motion, #count) }
    }

    fn visit_range(
        &mut self,
        range: &[ActionToken],
        inclusive: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let range = self.parse_range_type(range);
        let inclusive = self.parse_bool(inclusive);
        let count = self.parse_count(count);
        quote! { ::editor_types::prelude::EditTarget::Range(#range, #inclusive, #count) }
    }

    fn visit_search(
        &mut self,
        search: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let search = self.parse_search_type(search);
        let dir = self.parse_move_dir_mod(dir);
        let count = self.parse_count(count);
        quote! { ::editor_types::prelude::EditTarget::Search(#search, #dir, #count) }
    }

    fn visit_selection(&mut self) -> Self::Output {
        quote! { ::editor_types::prelude::EditTarget::Selection }
    }
}

pub struct EditTargetMacroInput {
    args: Vec<(Ident, Expr)>,
    acts: TokenStream,
}

impl EditTargetMacroInput {
    pub fn into_stream(self) -> TokenStream {
        let mut act = TokenStream::new();

        for (ident, expr) in self.args {
            act.extend(quote! { let #ident = { #expr }; });
        }

        act.extend(self.acts);
        quote! { { #act } }
    }
}

impl Parse for EditTargetMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fmt = input.parse::<LitStr>()?;
        let fmt_str = fmt.value();
        let tokens = tokenize(&fmt_str).expect("EditTarget expression should be valid");
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
        let acts = EditTargetParserExt::parse_tokens(&mut parser, tokens.as_slice());
        let generator = Self { args, acts };

        Ok(generator)
    }
}
