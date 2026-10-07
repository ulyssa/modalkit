use super::*;

impl<I> ActionParser for ActionReader<I>
where
    I: ApplicationInfo,
{
    type Output = anyhow::Result<Action<I>>;

    fn fail<T: std::fmt::Display>(&self, msg: T) -> Self::Output {
        bail!("{msg}")
    }

    fn visit_keyword_lookup(&mut self, target: &[ActionToken]) -> Self::Output {
        let target = KeywordTarget::try_from(target)?;
        Ok(Action::KeywordLookup(target))
    }

    fn visit_noop(&mut self) -> Self::Output {
        Ok(Action::NoOp)
    }

    fn visit_redraw_screen(&mut self) -> Self::Output {
        Ok(Action::RedrawScreen)
    }

    fn visit_suspend(&mut self) -> Self::Output {
        Ok(Action::Suspend)
    }

    fn visit_cmdbar_focus(
        &mut self,
        prompt: &[ActionToken],
        cmdtype: &[ActionToken],
        action: &[ActionToken],
    ) -> Self::Output {
        let prompt = parse_std_string(prompt)?;
        let cmdtype = CommandType::try_from(cmdtype)?;
        let action = <Self as ActionParserExt>::parse_action(self, action)?;

        Ok(Action::CommandBar(CommandBarAction::Focus(prompt, cmdtype, Box::new(action))))
    }

    fn visit_cmdbar_unfocus(&mut self) -> Self::Output {
        Ok(Action::CommandBar(CommandBarAction::Unfocus))
    }

    fn visit_command_execute(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = Count::try_from(count)?;
        Ok(Action::Command(CommandAction::Execute(count)))
    }

    fn visit_command_run(&mut self, input: &[ActionToken]) -> Self::Output {
        let input = parse_std_string(input)?;
        Ok(Action::Command(CommandAction::Run(input)))
    }

    fn visit_complete(
        &mut self,
        style: &[ActionToken],
        comptype: &[ActionToken],
        display: &[ActionToken],
    ) -> Self::Output {
        let comptype = CompletionType::try_from(comptype)?;
        let style = CompletionStyle::try_from(style)?;
        let display = CompletionDisplay::try_from(display)?;
        Ok(Action::Editor(EditorAction::Complete(style, comptype, display)))
    }

    fn visit_history_checkpoint(&mut self) -> Self::Output {
        Ok(Action::Editor(EditorAction::History(HistoryAction::Checkpoint)))
    }

    fn visit_history_undo(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = Count::try_from(count)?;

        Ok(Action::Editor(EditorAction::History(HistoryAction::Undo(count))))
    }

    fn visit_edit(&mut self, action: &[ActionToken], target: &[ActionToken]) -> Self::Output {
        let action = parse_specifier::<EditAction>(action)?;
        let target = <Self as EditTargetParserExt>::parse_tokens(self, target)?;

        Ok(Action::Editor(EditorAction::Edit(action, target)))
    }

    fn visit_history_redo(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = Count::try_from(count)?;

        Ok(Action::Editor(EditorAction::History(HistoryAction::Redo(count))))
    }

    fn visit_macro_execute(&mut self, reg: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let reg = parse_specifier::<Register>(reg)?;
        let count = Count::try_from(count)?;

        Ok(Action::Macro(MacroAction::Execute(reg, count)))
    }

    fn visit_macro_run(&mut self, input: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let input = parse_std_string(input)?;
        let count = Count::try_from(count)?;

        Ok(Action::Macro(MacroAction::Run(input, count)))
    }

    fn visit_macro_repeat(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = Count::try_from(count)?;

        Ok(Action::Macro(MacroAction::Repeat(count)))
    }

    fn visit_macro_toggle_recording(
        &mut self,
        reg: &[ActionToken],
        style: &[ActionToken],
    ) -> Self::Output {
        let reg = parse_specifier::<Register>(reg)?;
        let style = parse_specifier::<RegisterUpdateStyle>(style)?;

        Ok(Action::Macro(MacroAction::ToggleRecording(reg, style)))
    }

    fn visit_prompt_abort(&mut self, empty: &[ActionToken]) -> Self::Output {
        let empty = parse_std_bool(empty)?;
        Ok(Action::Prompt(PromptAction::Abort(empty)))
    }

    fn visit_prompt_recall(
        &mut self,
        filter: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let filter = RecallFilter::try_from(filter)?;
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;
        Ok(Action::Prompt(PromptAction::Recall(filter, dir, count)))
    }

    fn visit_prompt_submit(&mut self) -> Self::Output {
        Ok(Action::Prompt(PromptAction::Submit))
    }

    fn visit_mark(&mut self, mark: &[ActionToken]) -> Self::Output {
        let mark = parse_specifier::<Mark>(mark)?;
        Ok(Action::Editor(EditorAction::Mark(mark)))
    }

    fn visit_tab_close(&mut self, target: &[ActionToken], flags: &[ActionToken]) -> Self::Output {
        let target = TabTarget::try_from(target)?;
        let flags = CloseFlags::try_from(flags)?;
        Ok(Action::Tab(TabAction::Close(target, flags)))
    }

    fn visit_tab_extract(&mut self, fc: &[ActionToken], dir: &[ActionToken]) -> Self::Output {
        let fc = FocusChange::try_from(fc)?;
        let dir = MoveDir1D::try_from(dir)?;
        Ok(Action::Tab(TabAction::Extract(fc, dir)))
    }

    fn visit_tab_open(&mut self, target: &[ActionToken], fc: &[ActionToken]) -> Self::Output {
        let target = OpenTarget::try_from(target)?;
        let fc = FocusChange::try_from(fc)?;
        Ok(Action::Tab(TabAction::Open(target, fc)))
    }

    fn visit_tab_focus(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = FocusChange::try_from(fc)?;
        Ok(Action::Tab(TabAction::Focus(fc)))
    }

    fn visit_tab_move(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = FocusChange::try_from(fc)?;
        Ok(Action::Tab(TabAction::Move(fc)))
    }

    fn visit_cursor_close(&mut self, target: &[ActionToken]) -> Self::Output {
        let target = CursorCloseTarget::try_from(target)?;
        Ok(Action::Editor(EditorAction::Cursor(CursorAction::Close(target))))
    }

    fn visit_cursor_restore(&mut self, reg: &[ActionToken], style: &[ActionToken]) -> Self::Output {
        let reg = parse_specifier::<Register>(reg)?;
        let style = CursorGroupCombineStyle::try_from(style)?;
        Ok(Action::Editor(EditorAction::Cursor(CursorAction::Restore(reg, style))))
    }

    fn visit_cursor_rotate(&mut self, dir: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;
        Ok(Action::Editor(EditorAction::Cursor(CursorAction::Rotate(dir, count))))
    }

    fn visit_cursor_save(&mut self, reg: &[ActionToken], style: &[ActionToken]) -> Self::Output {
        let reg = parse_specifier::<Register>(reg)?;
        let style = CursorGroupCombineStyle::try_from(style)?;
        Ok(Action::Editor(EditorAction::Cursor(CursorAction::Save(reg, style))))
    }

    fn visit_cursor_split(&mut self, count: &[ActionToken]) -> Self::Output {
        let count = Count::try_from(count)?;
        Ok(Action::Editor(EditorAction::Cursor(CursorAction::Split(count))))
    }

    fn visit_window_close(
        &mut self,
        target: &[ActionToken],
        flags: &[ActionToken],
    ) -> Self::Output {
        let target = WindowTarget::try_from(target)?;
        let flags = CloseFlags::try_from(flags)?;
        Ok(Action::Window(WindowAction::Close(target, flags)))
    }

    fn visit_window_open(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let target = OpenTarget::try_from(target)?;
        let axis = Axis::try_from(axis)?;
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;
        Ok(Action::Window(WindowAction::Open(target, axis, dir, count)))
    }

    fn visit_window_resize(
        &mut self,
        fc: &[ActionToken],
        axis: &[ActionToken],
        size: &[ActionToken],
    ) -> Self::Output {
        let fc = FocusChange::try_from(fc)?;
        let axis = Axis::try_from(axis)?;
        let size = SizeChange::try_from(size)?;
        Ok(Action::Window(WindowAction::Resize(fc, axis, size)))
    }

    fn visit_window_split(
        &mut self,
        target: &[ActionToken],
        axis: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let target = OpenTarget::try_from(target)?;
        let axis = Axis::try_from(axis)?;
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;
        Ok(Action::Window(WindowAction::Split(target, axis, dir, count)))
    }

    fn visit_window_switch(&mut self, target: &[ActionToken]) -> Self::Output {
        let target = OpenTarget::try_from(target)?;
        Ok(Action::Window(WindowAction::Switch(target)))
    }

    fn visit_window_write(
        &mut self,
        target: &[ActionToken],
        name: &[ActionToken],
        flags: &[ActionToken],
    ) -> Self::Output {
        let target = WindowTarget::try_from(target)?;
        let name = if name.is_empty() {
            None
        } else {
            Some(parse_std_string(name)?)
        };
        let flags = WriteFlags::try_from(flags)?;
        Ok(Action::Window(WindowAction::Write(target, name, flags)))
    }

    fn visit_window_exchange(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = FocusChange::try_from(fc)?;
        Ok(Action::Window(WindowAction::Exchange(fc)))
    }

    fn visit_window_focus(&mut self, fc: &[ActionToken]) -> Self::Output {
        let fc = FocusChange::try_from(fc)?;
        Ok(Action::Window(WindowAction::Focus(fc)))
    }

    fn visit_window_move_side(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir2D::try_from(dir)?;
        Ok(Action::Window(WindowAction::MoveSide(dir)))
    }

    fn visit_window_rotate(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(Action::Window(WindowAction::Rotate(dir)))
    }

    fn visit_window_clear_sizes(&mut self) -> Self::Output {
        Ok(Action::Window(WindowAction::ClearSizes))
    }

    fn visit_window_zoom_toggle(&mut self) -> Self::Output {
        Ok(Action::Window(WindowAction::ZoomToggle))
    }

    fn visit_insert_open_line(
        &mut self,
        shape: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let shape = TargetShape::try_from(shape)?;
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;
        Ok(Action::Editor(EditorAction::InsertText(InsertTextAction::OpenLine(shape, dir, count))))
    }

    fn visit_insert_transcribe(
        &mut self,
        input: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let input = parse_std_string(input)?;
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;
        Ok(Action::Editor(EditorAction::InsertText(InsertTextAction::Transcribe(
            input, dir, count,
        ))))
    }

    fn visit_insert_type(
        &mut self,
        c: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let c = parse_specifier::<Char>(c)?;
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;

        Ok(Action::Editor(EditorAction::InsertText(InsertTextAction::Type(c, dir, count))))
    }

    fn visit_insert_paste(
        &mut self,
        style: &[ActionToken],
        reg: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let style = PasteStyle::try_from(style)?;
        let reg = parse_specifier::<Register>(reg)?;
        let count = Count::try_from(count)?;

        Ok(Action::Editor(EditorAction::InsertText(InsertTextAction::Paste(style, reg, count))))
    }

    fn visit_jump(
        &mut self,
        list: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let list = PositionList::try_from(list)?;
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;

        Ok(Action::Jump(list, dir, count))
    }

    fn visit_repeat(&mut self, style: &[ActionToken]) -> Self::Output {
        let style = RepeatType::try_from(style)?;

        Ok(Action::Repeat(style))
    }

    fn visit_scroll(&mut self, style: &[ActionToken]) -> Self::Output {
        let style = ScrollStyle::try_from(style)?;

        Ok(Action::Scroll(style))
    }

    fn visit_search(&mut self, dir: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let dir = MoveDirMod::try_from(dir)?;
        let count = Count::try_from(count)?;

        Ok(Action::Search(dir, count))
    }

    fn visit_selection_duplicate(
        &mut self,
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        let count = Count::try_from(count)?;

        Ok(Action::Editor(EditorAction::Selection(SelectionAction::Duplicate(dir, count))))
    }

    fn visit_selection_cursor_set(&mut self, change: &[ActionToken]) -> Self::Output {
        let change = SelectionCursorChange::try_from(change)?;

        Ok(Action::Editor(EditorAction::Selection(SelectionAction::CursorSet(change))))
    }

    fn visit_selection_expand(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output {
        let boundary = SelectionBoundary::try_from(boundary)?;
        let target = TargetShapeFilter::try_from(target)?;

        Ok(Action::Editor(EditorAction::Selection(SelectionAction::Expand(boundary, target))))
    }

    fn visit_selection_filter(&mut self, act: &[ActionToken]) -> Self::Output {
        let act = MatchAction::try_from(act)?;
        Ok(Action::Editor(EditorAction::Selection(SelectionAction::Filter(act))))
    }

    fn visit_selection_resize(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output {
        let style = SelectionResizeStyle::try_from(style)?;
        let target = <Self as EditTargetParserExt>::parse_tokens(self, target)?;
        Ok(Action::Editor(EditorAction::Selection(SelectionAction::Resize(style, target))))
    }

    fn visit_selection_split(
        &mut self,
        style: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output {
        let style = SelectionSplitStyle::try_from(style)?;
        let target = TargetShapeFilter::try_from(target)?;
        Ok(Action::Editor(EditorAction::Selection(SelectionAction::Split(style, target))))
    }

    fn visit_selection_join(&mut self) -> Self::Output {
        Ok(Action::Editor(EditorAction::Selection(SelectionAction::Join)))
    }

    fn visit_selection_trim(
        &mut self,
        boundary: &[ActionToken],
        target: &[ActionToken],
    ) -> Self::Output {
        let boundary = SelectionBoundary::try_from(boundary)?;
        let target = TargetShapeFilter::try_from(target)?;

        Ok(Action::Editor(EditorAction::Selection(SelectionAction::Trim(boundary, target))))
    }
}
