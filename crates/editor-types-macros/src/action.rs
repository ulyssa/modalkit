use super::*;

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
        let action = self.parse_specifier_edit_action(action, span);
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

pub struct ActionMacroInput {
    args: Vec<(Ident, Expr)>,
    acts: TokenStream,
}

impl ActionMacroInput {
    pub fn into_stream(self) -> TokenStream {
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
        let acts = parser.parse_action(tokens.as_slice(), fmt.span());
        let generator = Self { args, acts };

        Ok(generator)
    }
}
