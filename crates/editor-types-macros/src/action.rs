use super::*;

impl ActionParser for ActionMacroParser {
    type Output = TokenStream;

    fn fail<T: std::fmt::Display>(&self, msg: T) -> Self::Output {
        ParseError::new(self.span, msg).to_compile_error()
    }

    fn visit_keyword_lookup(&mut self, target: &[ActionToken]) -> Self::Output {
        let target = self.parse_keyword_target(target);
        quote! { ::editor_types::Action::KeywordLookup(#target) }
    }

    fn visit_noop(&mut self) -> Self::Output {
        quote! { ::editor_types::Action::NoOp }
    }

    fn visit_redraw_screen(&mut self) -> Self::Output {
        quote! { ::editor_types::Action::RedrawScreen }
    }

    fn visit_suspend(&mut self) -> Self::Output {
        quote! { ::editor_types::Action::Suspend }
    }

    fn visit_cmdbar_focus(
        &mut self,
        prompt: &[ActionToken],
        cmdtype: &[ActionToken],
        action: &[ActionToken],
    ) -> Self::Output {
        let prompt = self.parse_string(prompt);
        let cmdtype = self.parse_command_type(cmdtype);
        let action = self.parse_action(action);

        quote! {
            ::editor_types::Action::CommandBar(
                ::editor_types::CommandBarAction::Focus(#prompt, #cmdtype, Box::new(#action))
            )
        }
    }

    fn visit_cmdbar_unfocus(&mut self) -> Self::Output {
        quote! {
            ::editor_types::Action::CommandBar(
                ::editor_types::CommandBarAction::Unfocus
            )
        }
    }

    fn visit_command_execute(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Command(
                ::editor_types::CommandAction::Execute(#count)
            )
        }
    }

    fn visit_command_run(&mut self, input: &[ActionToken]) -> Self::Output {
        let input = self.parse_string(input);

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
    ) -> Self::Output {
        let style = self.parse_completion_style(style);
        let comptype = self.parse_completion_type(comptype);
        let display = self.parse_completion_display(display);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Complete(#style, #comptype, #display)
            )
        }
    }

    fn visit_history_checkpoint(&mut self) -> Self::Output {
        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::History(
                    ::editor_types::HistoryAction::Checkpoint
                )
            )
        }
    }

    fn visit_history_undo(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::History(
                    ::editor_types::HistoryAction::Undo(#count)
                )
            )
        }
    }

    fn visit_edit(&mut self, action: &[ActionToken], target: &[ActionToken]) -> Self::Output {
        let action = self.parse_specifier_edit_action(action);
        let target = self.parse_edit_target(target);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Edit(#action, #target)
            )
        }
    }

    fn visit_history_redo(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::History(
                    ::editor_types::HistoryAction::Redo(#count)
                )
            )
        }
    }

    fn visit_macro_execute(&mut self, reg: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let reg = self.parse_specifier_register(reg);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::Execute(#reg, #count)
            )
        }
    }

    fn visit_macro_run(&mut self, input: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let input = self.parse_string(input);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::Run(#input, #count)
            )
        }
    }

    fn visit_macro_repeat(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::Repeat(#count)
            )
        }
    }

    fn visit_macro_toggle_recording(
        &mut self,
        reg: &[ActionToken],
        style: &[ActionToken],
    ) -> Self::Output {
        let reg = self.parse_specifier_register(reg);
        let style = self.parse_specifier_register_update_style(style);

        quote! {
            ::editor_types::Action::Macro(
                ::editor_types::MacroAction::ToggleRecording(#reg, #style)
            )
        }
    }

    fn visit_prompt_abort(&mut self, empty: &[ActionToken]) -> Self::Output {
        let empty = self.parse_bool(empty);

        quote! {
            ::editor_types::Action::Prompt(
                ::editor_types::PromptAction::Abort(#empty)
            )
        }
    }

    fn visit_prompt_recall(
        &mut self,
        filter: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let filter = self.parse_recall_filter(filter);
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Prompt(
                ::editor_types::PromptAction::Recall(#filter, #dir, #count)
            )
        }
    }

    fn visit_prompt_submit(&mut self) -> Self::Output {
        quote! {
            ::editor_types::Action::Prompt(
                ::editor_types::PromptAction::Submit
            )
        }
    }

    fn visit_mark(&mut self, mark: &[ActionToken]) -> Self::Output {
        let mark = self.parse_specifier_mark(mark);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Mark(#mark)
            )
        }
    }

    fn visit_tab_close(&mut self, target: &[ActionToken], flags: &[ActionToken]) -> Self::Output {
        let target = self.parse_tab_target(target);
        let flags = self.parse_close_flags(flags);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Close(#target, #flags)
            )
        }
    }

    fn visit_tab_extract(&mut self, fc: &[ActionToken], dir: &[ActionToken]) -> Self::Output {
        let fc = self.parse_focus_change(fc);
        let dir = self.parse_dir1d(dir);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Extract(#fc, #dir)
            )
        }
    }

    fn visit_tab_open(&mut self, target: &[ActionToken], fc: &[ActionToken]) -> Self::Output {
        let target = self.parse_open_target(target);
        let fc = self.parse_focus_change(fc);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Open(#target, #fc)
            )
        }
    }

    fn visit_tab_focus(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = self.parse_focus_change(fc);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Focus(#fc)
            )
        }
    }

    fn visit_tab_move(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = self.parse_focus_change(fc);

        quote! {
            ::editor_types::Action::Tab(
                ::editor_types::TabAction::Move(#fc)
            )
        }
    }

    fn visit_cursor_close(&mut self, target: &[ActionToken]) -> Self::Output {
        let target = self.parse_cursor_close_target(target);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                     ::editor_types::CursorAction::Close(#target)
                )
            )
        }
    }

    fn visit_cursor_restore(&mut self, reg: &[ActionToken], style: &[ActionToken]) -> Self::Output {
        let reg = self.parse_specifier_register(reg);
        let style = self.parse_cursor_group_combine(style);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                    ::editor_types::CursorAction::Restore(#reg, #style)
                )
            )
        }
    }

    fn visit_cursor_rotate(&mut self, dir: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                    ::editor_types::CursorAction::Rotate(#dir, #count)
                )
            )
        }
    }

    fn visit_cursor_save(&mut self, reg: &[ActionToken], style: &[ActionToken]) -> Self::Output {
        let reg = self.parse_specifier_register(reg);
        let style = self.parse_cursor_group_combine(style);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Cursor(
                    ::editor_types::CursorAction::Save(#reg, #style)
                )
            )
        }
    }

    fn visit_cursor_split(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = self.parse_count(count);

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
    ) -> Self::Output {
        let target = self.parse_window_target(target);
        let flags = self.parse_close_flags(flags);

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
    ) -> Self::Output {
        let target = self.parse_open_target(target);
        let axis = self.parse_axis(axis);
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

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
    ) -> Self::Output {
        let fc = self.parse_focus_change(fc);
        let axis = self.parse_axis(axis);
        let size = self.parse_size_change(size);

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
    ) -> Self::Output {
        let target = self.parse_open_target(target);
        let axis = self.parse_axis(axis);
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Split(#target, #axis, #dir, #count)
            )
        }
    }

    fn visit_window_switch(&mut self, target: &[ActionToken]) -> Self::Output {
        let target = self.parse_open_target(target);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Switch(#target)
            )
        }
    }

    fn visit_window_write(
        &mut self,
        target: &[ActionToken],
        name: &[ActionToken],
        flags: &[ActionToken],
    ) -> Self::Output {
        let target = self.parse_window_target(target);
        let name = if name.is_empty() {
            quote! { ::std::option::Option::None }
        } else {
            let name = self.parse_string(name);
            quote! { ::std::option::Option::Some(#name) }
        };
        let flags = self.parse_write_flags(flags);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Write(#target, #name, #flags)
            )
        }
    }

    fn visit_window_exchange(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = self.parse_focus_change(fc);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Exchange(#fc)
            )
        }
    }

    fn visit_window_focus(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = self.parse_focus_change(fc);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Focus(#fc)
            )
        }
    }

    fn visit_window_move_side(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir2d(dir);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::MoveSide(#dir)
            )
        }
    }

    fn visit_window_rotate(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = self.parse_dir1d(dir);

        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::Rotate(#dir)
            )
        }
    }

    fn visit_window_clear_sizes(&mut self) -> Self::Output {
        quote! {
            ::editor_types::Action::Window(
                ::editor_types::WindowAction::ClearSizes
            )
        }
    }

    fn visit_window_zoom_toggle(&mut self) -> Self::Output {
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
    ) -> Self::Output {
        let shape = self.parse_target_shape(shape);
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

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
    ) -> Self::Output {
        let input = self.parse_string(input);
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

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
    ) -> Self::Output {
        let c = self.parse_specifier_char(c);
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

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
        reg: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let style = self.parse_paste_style(style);
        let reg = self.parse_specifier_register(reg);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::InsertText(
                    ::editor_types::InsertTextAction::Paste(#style, #reg, #count)
                )
            )
        }
    }

    fn visit_jump(
        &mut self,
        list: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let list = self.parse_position_list(list);
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Jump(#list, #dir, #count)
        }
    }

    fn visit_repeat(&mut self, style: &[ActionToken]) -> Self::Output {
        let style = self.parse_repeat_style(style);

        quote! { ::editor_types::Action::Repeat(#style) }
    }

    fn visit_scroll(&mut self, style: &[ActionToken]) -> Self::Output {
        let style = self.parse_scroll_style(style);

        quote! { ::editor_types::Action::Scroll(#style) }
    }

    fn visit_search(&mut self, dir: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let dir = self.parse_move_dir_mod(dir);
        let count = self.parse_count(count);

        quote! { ::editor_types::Action::Search(#dir, #count) }
    }

    fn visit_selection_duplicate(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let dir = self.parse_dir1d(dir);
        let count = self.parse_count(count);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                    ::editor_types::SelectionAction::Duplicate(#dir, #count)
                )
            )
        }
    }

    fn visit_selection_cursor_set(&mut self, change: &[ActionToken]) -> Self::Output {
        let change = self.parse_selection_cursor_change(change);

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
    ) -> Self::Output {
        let boundary = self.parse_selection_boundary(boundary);
        let target = self.parse_target_shape_filter(target);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                    ::editor_types::SelectionAction::Expand(#boundary, #target)
                )
            )
        }
    }

    fn visit_selection_filter(&mut self, act: &[ActionToken]) -> Self::Output {
        let act = self.parse_match_action(act);

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
    ) -> Self::Output {
        let style = self.parse_selection_resize_style(style);
        let target = self.parse_edit_target(target);

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
    ) -> Self::Output {
        let style = self.parse_selection_split_style(style);
        let target = self.parse_target_shape_filter(target);

        quote! {
            ::editor_types::Action::Editor(
                ::editor_types::EditorAction::Selection(
                     ::editor_types::SelectionAction::Split(#style, #target)
                )
            )
        }
    }

    fn visit_selection_join(&mut self) -> Self::Output {
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
    ) -> Self::Output {
        let boundary = self.parse_selection_boundary(boundary);
        let target = self.parse_target_shape_filter(target);

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
        let mut act = bind_args(self.args);

        act.extend(self.acts);
        quote! { { #act } }
    }
}

impl Parse for ActionMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fmt = input.parse::<LitStr>()?;
        let fmt_str = fmt.value();
        let mut tokens = tokenize_fmt("action!", &fmt_str, fmt.span())?;
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

        let acts = parser.parse_action(tokens.as_slice());
        let generator = Self { args, acts };

        Ok(generator)
    }
}
