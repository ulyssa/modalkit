//! # Tabbed window layout
//!
//! ## Overview
//!
//! This widget can be used by consumers to create a tabbed window layout containing horizontal and
//! vertical splits. It builds on top of [CommandBarState] and [WindowLayoutState] to accomplish
//! this, both of which can also be used on their own if something different is needed.
use std::marker::PhantomData;

use tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier as StyleModifier, Style},
    text::{Span, Spans},
    widgets::{StatefulWidget, Tabs, Widget},
};

use crate::{
    editing::buffer::Editable,
    editing::store::SharedStore,
    input::InputContext,
    util::idx_offset,
};

use super::{
    cmdbar::{CommandBar, CommandBarState},
    util::{rect_down, rect_zero_height},
    windows::{WindowActions, WindowLayout, WindowLayoutState},
    Focusable,
    Submitable,
    TabContainer,
    TerminalCursor,
    Window,
    WindowContainer,
};

use crate::editing::base::{
    Action,
    Application,
    Axis,
    Char,
    CloseFlags,
    CloseTarget,
    CommandType,
    Count,
    CursorAction,
    EditAction,
    EditContext,
    EditError,
    EditResult,
    EditTarget,
    FocusChange,
    HistoryAction,
    Mark,
    MoveDir1D,
    MoveDir2D,
    MovePosition,
    ScrollStyle,
    SelectionCursorChange,
    TabAction,
    TargetShapeFilter,
    WindowAction,
};

trait TabActions<C> {
    /// Close one or more tabs, and all of their [Windows](Window).
    fn tab_close(&mut self, target: &CloseTarget, flags: CloseFlags, ctx: &C) -> EditResult;

    /// Switch focus to another tab.
    fn tab_focus(&mut self, change: &FocusChange, ctx: &C) -> EditResult;
}

fn bold<'a>(s: String) -> Span<'a> {
    Span::styled(s, Style::default().add_modifier(StyleModifier::BOLD))
}

/// Controls which part of the [ScreenState] is currently receiving user input.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CurrentFocus {
    /// Focus on the [CommandBarState].
    Command,

    /// Focus on the [WindowLayoutState].
    Window,
}

/// Persistent state for [Screen].
pub struct ScreenState<W: Window, C: EditContext + InputContext, P: Application> {
    focused: CurrentFocus,
    cmdbar: CommandBarState<C, P>,
    tabs: Vec<WindowLayoutState<W>>,
    tabidx: usize,
    tabidx_last: usize,

    messages: Vec<(String, Style)>,
    last_message: bool,
}

impl<W, C, P> ScreenState<W, C, P>
where
    W: Window,
    C: EditContext + InputContext,
    P: Application,
{
    /// Create a new instance.
    pub fn new(win: W, store: SharedStore<C, P>) -> Self {
        let cmdbar = CommandBarState::new(store.clone());
        let tab = WindowLayoutState::new(win);

        ScreenState {
            focused: CurrentFocus::Window,
            cmdbar,
            tabs: vec![tab],
            tabidx: 0,
            tabidx_last: 0,

            messages: vec![],
            last_message: false,
        }
    }

    pub fn push_message<T: Into<String>>(&mut self, msg: T, style: Style) {
        self.messages.push((msg.into(), style));
        self.last_message = true;
    }

    pub fn clear_message(&mut self) {
        self.last_message = false;
    }

    pub fn focus_command(&mut self, ct: CommandType) -> EditResult {
        self.focused = CurrentFocus::Command;
        self.cmdbar.set_type(ct);

        Ok(None)
    }

    pub fn focus_window(&mut self) -> EditResult {
        self.focused = CurrentFocus::Window;
        self.cmdbar.reset();

        Ok(None)
    }

    fn _focus_tab(&mut self, idx: usize) {
        if idx != self.tabidx {
            self.tabidx_last = self.tabidx;
            self.tabidx = idx;
        }
    }

    /// Get a reference to the window layout for the current tab.
    pub fn current_tab(&self) -> &WindowLayoutState<W> {
        self.tabs.get(self.tabidx).unwrap()
    }

    /// Get a mutable reference to the window layout for the current tab.
    pub fn current_tab_mut(&mut self) -> &mut WindowLayoutState<W> {
        self.tabs.get_mut(self.tabidx).unwrap()
    }

    /// Get a reference to the currently focused window.
    pub fn current_window(&self) -> Option<&W> {
        self.current_tab().get()
    }

    /// Get a mutable reference to the currently focused window.
    pub fn current_window_mut(&mut self) -> Option<&mut W> {
        self.current_tab_mut().get_mut()
    }

    fn _max_idx(&self) -> usize {
        self.tabs.len().saturating_sub(1)
    }

    fn _tabnr(&self, count: &Count, ctx: &C) -> usize {
        ctx.resolve(count).saturating_sub(1)
    }

    fn _target(&self, target: &FocusChange, ctx: &C) -> Option<usize> {
        let target = match target {
            FocusChange::Current => self.tabidx,
            FocusChange::Offset(count, false) => {
                let tabnr = self._tabnr(count, ctx);

                if tabnr >= self.tabs.len() {
                    // Invalid tab index; do nothing.
                    return None;
                }

                tabnr
            },
            FocusChange::Offset(count, true) => self._tabnr(count, ctx).min(self._max_idx()),
            FocusChange::Direction1D(dir, count, wrap) => {
                let ntabs = self.tabs.len();
                let count = ctx.resolve(count);

                return idx_offset(self.tabidx, count, dir, ntabs, *wrap);
            },
            FocusChange::Direction2D(MoveDir2D::Left, count) => {
                let ntabs = self.tabs.len();
                let count = ctx.resolve(count) % ntabs;

                (ntabs + self.tabidx - count) % ntabs
            },
            FocusChange::Direction2D(MoveDir2D::Right, count) => {
                let ntabs = self.tabs.len();
                let count = ctx.resolve(count) % ntabs;

                (self.tabidx + count) % ntabs
            },
            FocusChange::Direction2D(MoveDir2D::Up | MoveDir2D::Down, _) => {
                return None;
            },
            FocusChange::Position(MovePosition::Beginning) => 0,
            FocusChange::Position(MovePosition::Middle) => self.tabs.len() / 2,
            FocusChange::Position(MovePosition::End) => self.tabs.len().saturating_sub(1),
            FocusChange::PreviouslyFocused => self.tabidx_last,
        };

        Some(target)
    }
}

impl<W, C, P> TabActions<C> for ScreenState<W, C, P>
where
    W: Window,
    C: EditContext + InputContext,
    P: Application,
{
    fn tab_close(&mut self, target: &CloseTarget, flags: CloseFlags, ctx: &C) -> EditResult {
        match target {
            CloseTarget::All => {
                let mut old = vec![];
                let oldidx = self.tabidx;

                std::mem::swap(&mut self.tabs, &mut old);

                self.tabidx = 0;

                for (i, mut tab) in old.into_iter().enumerate() {
                    let _ = tab.window_close(CloseTarget::All, flags, ctx)?;

                    if WindowContainer::<W, C>::windows(&tab) > 0 {
                        if i == oldidx {
                            self.tabidx = self.tabs.len();
                        }

                        self.tabs.push(tab);
                    }
                }

                if self.tabs.len() == 0 {
                    return Ok(None);
                } else {
                    return Err(EditError::Failure(
                        "unable to close all windows in some tabs".into(),
                    ));
                }
            },
            CloseTarget::AllBut(fc) => {
                if let Some(idx) = self._target(fc, ctx) {
                    let mut old = vec![];

                    std::mem::swap(&mut self.tabs, &mut old);

                    for (i, mut tab) in old.into_iter().enumerate() {
                        if i == idx {
                            self.tabidx = self.tabs.len();
                            self.tabs.push(tab);
                            continue;
                        }

                        let _ = tab.window_close(CloseTarget::All, flags, ctx)?;

                        if WindowContainer::<W, C>::windows(&tab) > 0 {
                            self.tabs.push(tab);
                        }
                    }
                }
            },
            CloseTarget::Single(fc) => {
                if let Some(idx) = self._target(fc, ctx) {
                    let _ = self.tabs[idx].window_close(CloseTarget::All, flags, ctx)?;

                    if WindowContainer::<W, C>::windows(&self.tabs[idx]) > 0 {
                        return Err(EditError::Failure(
                            "unable to close all windows in tab".into(),
                        ));
                    }

                    let _ = self.tabs.remove(idx);

                    if idx < self.tabidx {
                        self.tabidx = self.tabidx.saturating_sub(1);
                    }
                }
            },
        }

        return Ok(None);
    }

    fn tab_focus(&mut self, change: &FocusChange, ctx: &C) -> EditResult {
        if let Some(target) = self._target(change, ctx) {
            self._focus_tab(target);
        }

        Ok(None)
    }
}

impl<W, C, P> TabContainer<C> for ScreenState<W, C, P>
where
    W: Window,
    C: EditContext + InputContext,
    P: Application,
{
    fn tabs(&self) -> usize {
        self.tabs.len()
    }

    fn tab_command(&mut self, act: TabAction, ctx: &C) -> EditResult {
        match act {
            TabAction::Close(target, flags) => self.tab_close(&target, flags, ctx),
            TabAction::Focus(change) => self.tab_focus(&change, ctx),
        }
    }
}

impl<W, C, P> WindowContainer<W, C> for ScreenState<W, C, P>
where
    W: Window,
    C: EditContext + InputContext,
    P: Application,
{
    fn windows(&self) -> usize {
        WindowContainer::<W, C>::windows(self.current_tab())
    }

    fn window_open(
        &mut self,
        window: W,
        axis: Axis,
        rel: MoveDir1D,
        count: Option<Count>,
        ctx: &C,
    ) -> EditResult {
        self.current_tab_mut().window_open(window, axis, rel, count, ctx)
    }

    fn window_command(&mut self, act: WindowAction, ctx: &C) -> EditResult {
        self.current_tab_mut().window_command(act, ctx)
    }
}

impl<W, C, P> Editable<C> for ScreenState<W, C, P>
where
    W: Window + Editable<C>,
    C: EditContext + InputContext,
    P: Application,
{
    fn edit(&mut self, action: &EditAction, target: &EditTarget, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.edit(action, target, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.edit(action, target, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn type_char(&mut self, ch: Char, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.type_char(ch, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.type_char(ch, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn open_line(&mut self, dir: MoveDir1D, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.open_line(dir, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.open_line(dir, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn cursor_command(&mut self, act: CursorAction, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.cursor_command(act, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.cursor_command(act, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn selection_split_lines(&mut self, filter: TargetShapeFilter, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.selection_split_lines(filter, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.selection_split_lines(filter, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn selcursor_set(&mut self, change: &SelectionCursorChange, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.selcursor_set(change, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.selcursor_set(change, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn paste(&mut self, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.paste(dir, count, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.paste(dir, count, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn mark(&mut self, name: Mark, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.mark(name, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.mark(name, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }

    fn history_command(&mut self, act: HistoryAction, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.history_command(act, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.history_command(act, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }
}

impl<W, C, P> TerminalCursor for ScreenState<W, C, P>
where
    W: Window + TerminalCursor,
    C: EditContext + InputContext,
    P: Application,
{
    fn get_term_cursor(&self) -> (u16, u16) {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.get_term_cursor(),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window() {
                    w.get_term_cursor()
                } else {
                    // XXX: make get_term_cursor() return an Option?
                    (0, 0)
                }
            },
        }
    }
}

impl<W, C, P> Focusable<C> for ScreenState<W, C, P>
where
    W: Window + Focusable<C>,
    C: EditContext + InputContext,
    P: Application,
{
    fn scroll(&mut self, style: &ScrollStyle, ctx: &C) -> EditResult {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.scroll(style, ctx),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window_mut() {
                    w.scroll(style, ctx)
                } else {
                    Ok(None)
                }
            },
        }
    }
}

impl<W, C, P> Submitable<Action<P>, C> for ScreenState<W, C, P>
where
    W: Window + Submitable<Action<P>, C>,
    C: EditContext + InputContext,
    P: Application,
{
    fn submit(&mut self, ctx: &mut C) -> Option<Action<P>> {
        match self.focused {
            CurrentFocus::Command => {
                let res = self.cmdbar.submit(ctx);
                let _ = self.focus_window();
                res
            },
            CurrentFocus::Window => {
                let w = self.current_window_mut()?;
                w.submit(ctx)
            },
        }
    }
}

/// Widget for displaying a tabbed window layout with a command bar.
pub struct Screen<'a, W: Window, C: EditContext + InputContext, P: Application> {
    showmode: Option<Span<'a>>,
    _p: PhantomData<(W, C, P)>,
}

impl<'a, W, C, P> Screen<'a, W, C, P>
where
    W: Window,
    C: EditContext + InputContext,
    P: Application,
{
    pub fn new() -> Self {
        Screen { showmode: None, _p: PhantomData }
    }

    pub fn showmode(mut self, mode: Option<String>) -> Self {
        self.showmode = mode.map(bold);
        self
    }
}

impl<'a, W, C, P> Default for Screen<'a, W, C, P>
where
    W: Window,
    C: EditContext + InputContext,
    P: Application,
{
    fn default() -> Self {
        Screen::new()
    }
}

impl<'a, W, C, P> StatefulWidget for Screen<'a, W, C, P>
where
    W: Window,
    C: EditContext + InputContext,
    P: Application,
{
    type State = ScreenState<W, C, P>;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if area.height == 0 {
            return;
        }

        let focused = state.focused;
        let ntabs = state.tabs.len();

        let tabh = if ntabs > 1 { 1 } else { 0 };

        let winh = area.height - tabh - 1;

        let init = rect_zero_height(area);
        let tabarea = rect_down(init, tabh);
        let winarea = rect_down(tabarea, winh);
        let cmdarea = rect_down(winarea, 1);

        let titles = state
            .tabs
            .iter()
            .enumerate()
            .map(|(i, _)| Spans::from(format!("Tab {:}", i + 1)))
            .collect();

        Tabs::new(titles)
            .style(Style::default().fg(Color::White))
            .highlight_style(Style::default().fg(Color::Yellow))
            .divider("|")
            .select(state.tabidx)
            .render(tabarea, buf);

        WindowLayout::new().focus(focused == CurrentFocus::Window).render(
            winarea,
            buf,
            state.current_tab_mut(),
        );

        let status = if self.showmode.is_some() || !state.last_message {
            state.last_message = false;
            self.showmode
        } else if let Some((s, style)) = state.messages.last() {
            Some(Span::styled(s, *style))
        } else {
            None
        };

        CommandBar::new()
            .focus(focused == CurrentFocus::Command)
            .status(status)
            .render(cmdarea, buf, &mut state.cmdbar);
    }
}
