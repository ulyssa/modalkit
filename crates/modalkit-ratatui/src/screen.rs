//! # Tabbed window layout
//!
//! ## Overview
//!
//! This widget can be used by consumers to create a tabbed window layout containing horizontal and
//! vertical splits. It builds on top of [CommandBarState] and [WindowLayoutState] to accomplish
//! this, both of which can also be used on their own if something different is needed.
use std::borrow::Cow;
use std::iter::Iterator;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier as StyleModifier, Style},
    text::{Line, Span},
    widgets::{BorderType, StatefulWidget, Tabs, Widget},
};

use super::{
    cmdbar::{CommandBar, CommandBarState},
    util::{rect_down, rect_zero_height},
    windows::{WindowActions, WindowLayout, WindowLayoutRoot, WindowLayoutState},
    TerminalCursor,
    Window,
    WindowOps,
};

use modalkit::actions::*;
use modalkit::errors::{EditResult, UIError, UIResult};
use modalkit::prelude::*;
use modalkit::ui::FocusList;

use modalkit::editing::{
    application::{ApplicationInfo, EmptyInfo},
    completion::CompletionList,
    context::EditContext,
    store::Store,
};

const MAX_COMPL_BARH: usize = 10;
const GAP_COMPL_COL: usize = 2;

/// Pop-up hover menu displaying completions.
#[derive(Default)]
struct CompletionMenu {
    cursor: (u16, u16),
}

impl CompletionMenu {
    fn new(cursor: (u16, u16)) -> Self {
        CompletionMenu { cursor }
    }
}

impl StatefulWidget for CompletionMenu {
    type State = CompletionList;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut CompletionList) {
        if area.height <= 1 {
            // Not enough space to render a menu above or below the cursor.
            return;
        }

        let len = state.candidates.len();

        let top = area.top();
        let bot = area.bottom();
        let (cx, cy) = self.cursor;

        let above = cy.saturating_sub(top) as usize;
        let below = bot.saturating_sub(cy).saturating_sub(1) as usize;

        let right = area.right();
        let space = right.saturating_sub(cx).saturating_sub(1) as usize;
        let maxw = state.candidates.iter().map(|s| s.len()).max().unwrap_or(0).min(space);
        let style = Style::reset().add_modifier(StyleModifier::REVERSED);
        let style_sel = style.bg(Color::Yellow).fg(Color::Black);

        let x = if state.start.y == state.cursor.y {
            let diff = state.cursor.x.saturating_sub(state.start.x);
            cx.saturating_sub(diff as u16)
        } else {
            cx
        };

        let mut draw = |y: u16, idx: usize, s: &str| {
            let sel = matches!(state.selected, Some(i) if i == idx);
            let style = if sel { style_sel } else { style };
            let slen = s.len();

            let (x, _) = buffer.set_stringn(x, y, s, space, style);
            let start = (maxw - slen) as u16;

            for off in 0..start {
                buffer.set_stringn(x + off, y, " ", 1, style);
            }
        };

        let candidates = state.candidates.iter().enumerate();

        if len <= below || below >= above {
            // We bias towards a drop-down menu.
            let height = len.min(below);
            let page = if let Some(selected) = state.selected {
                selected / height
            } else {
                0
            };

            for (y, (idx, s)) in candidates.skip(page * height).take(height).enumerate() {
                let y = cy + y as u16 + 1;
                draw(y, idx, s);
            }
        } else {
            // Draw a menu above the line.
            let height = len.min(above);
            let page = if let Some(selected) = state.selected {
                selected / height
            } else {
                0
            };

            let n = (len - page * height).min(height);

            for (y, (idx, s)) in candidates.skip(page * height).take(height).enumerate() {
                let y = cy.saturating_sub((n - y) as u16);
                draw(y, idx, s);
            }
        }
    }
}

/// Row and columns information for the completion bar.
#[derive(Default)]
struct CompletionBar {
    colw: u16,
    cols: u16,
    rows: u16,
}

impl CompletionBar {
    fn new(list: &CompletionList, width: u16) -> Self {
        match list.display {
            CompletionDisplay::None => CompletionBar::default(),
            CompletionDisplay::List => CompletionBar::default(),
            CompletionDisplay::Bar => {
                let len = list.candidates.len();
                let width = width as usize;

                if len == 0 {
                    return CompletionBar::default();
                }

                let cmax = list.candidates.iter().map(|s| s.len()).max().unwrap_or(0);
                let cmax = cmax.clamp(1, width);
                let colw = (cmax + GAP_COMPL_COL).min(width);
                let cols = (width / colw).max(1);
                let rows = (len / cols).clamp(1, MAX_COMPL_BARH);

                CompletionBar {
                    colw: colw as u16,
                    cols: cols as u16,
                    rows: rows as u16,
                }
            },
        }
    }
}

impl StatefulWidget for CompletionBar {
    type State = CompletionList;

    fn render(self, area: Rect, buffer: &mut Buffer, state: &mut CompletionList) {
        if area.height == 0 {
            return;
        }

        let mut iter = state.candidates.iter();
        let maxw = (self.colw as usize).saturating_sub(GAP_COMPL_COL);
        let style = Style::default();

        for x in 0..self.cols {
            for y in 0..self.rows {
                let item = match iter.next() {
                    Some(item) => item.as_str(),
                    None => return,
                };

                let x = area.x + x * self.colw;
                let y = area.y + y;
                buffer.set_stringn(x, y, item, maxw, style);
            }
        }
    }
}

trait TabActions<C, S, I>
where
    I: ApplicationInfo,
{
    /// Close one or more tabs, and all of their [Windows](Window).
    fn tab_close(
        &mut self,
        target: &TabTarget,
        flags: CloseFlags,
        ctx: &C,
        store: &mut S,
    ) -> UIResult<EditInfo, I>;

    /// Extract the currently focused [Window] from the currently focused tab, and place it in a
    /// new tab.
    fn tab_extract(
        &mut self,
        change: &FocusChange,
        side: &MoveDir1D,
        ctx: &C,
        store: &mut S,
    ) -> UIResult<EditInfo, I>;

    /// Switch focus to another tab.
    fn tab_focus(&mut self, change: &FocusChange, ctx: &C, store: &mut S) -> UIResult<EditInfo, I>;

    /// Move the current tab to another position.
    fn tab_move(&mut self, change: &FocusChange, ctx: &C, store: &mut S) -> UIResult<EditInfo, I>;

    /// Open a new tab after the tab targeted by [FocusChange].
    fn tab_open(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        change: &FocusChange,
        ctx: &C,
        store: &mut S,
    ) -> UIResult<EditInfo, I>;
}

fn bold<'a>(s: String) -> Span<'a> {
    Span::styled(s, Style::default().add_modifier(StyleModifier::BOLD))
}

/// A description of open tabs.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(bound(deserialize = "I::WindowId: Deserialize<'de>"))]
#[serde(bound(serialize = "I::WindowId: Serialize"))]
pub struct TabbedLayoutDescription<I: ApplicationInfo> {
    /// The description of the window layout for each tab.
    pub tabs: Vec<WindowLayoutRoot<I>>,
    /// The index of the last focused tab
    pub focused: usize,
}

impl<I: ApplicationInfo> TabbedLayoutDescription<I> {
    /// Create a new collection of tabs from this description.
    pub fn to_layout<W: Window<I>>(
        self,
        area: Option<Rect>,
        store: &mut Store<I>,
    ) -> UIResult<FocusList<WindowLayoutState<W, I>>, I> {
        let mut tabs = self
            .tabs
            .into_iter()
            .map(|desc| desc.to_layout(area, store))
            .collect::<UIResult<Vec<_>, I>>()
            .map(FocusList::new)?;

        // Count starts at 1
        let change = FocusChange::Offset(Count::Exact(self.focused + 1), true);
        let ctx = EditContext::default();
        tabs.focus(&change, &ctx);

        Ok(tabs)
    }
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
pub struct ScreenState<W, I = EmptyInfo>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    focused: CurrentFocus,
    cmdbar: CommandBarState<I>,
    tabs: FocusList<WindowLayoutState<W, I>>,

    messages: Vec<(String, Style)>,
    last_message: bool,
}

impl<W, I> ScreenState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    /// Create state for a [Screen] widget.
    pub fn new(win: W, cmdbar: CommandBarState<I>) -> Self {
        let tab = WindowLayoutState::new(win);
        let tabs = FocusList::from(tab);

        Self::from_list(tabs, cmdbar)
    }

    /// Create state for a [Screen] widget from an existing set of tabs.
    pub fn from_list(tabs: FocusList<WindowLayoutState<W, I>>, cmdbar: CommandBarState<I>) -> Self {
        ScreenState {
            focused: CurrentFocus::Window,
            cmdbar,
            tabs,

            messages: vec![],
            last_message: false,
        }
    }

    /// Get a description of the open tabs and their window layouts.
    pub fn as_description(&self) -> TabbedLayoutDescription<I> {
        TabbedLayoutDescription {
            tabs: self.tabs.iter().map(WindowLayoutState::as_description).collect(),
            focused: self.tabs.pos(),
        }
    }

    /// Push a new error or status message.
    pub fn push_message<T: ToString>(&mut self, msg: T, style: Style) {
        self.messages.push((msg.to_string(), style));
        self.last_message = true;
    }

    /// Push an error message with a red foreground.
    pub fn push_error<T: ToString>(&mut self, msg: T) {
        let style = Style::default().fg(Color::Red);

        self.push_message(msg, style);
    }

    /// Push an info message with a default [Style].
    pub fn push_info<T: Into<String>>(&mut self, msg: T) {
        let style = Style::default();

        self.push_message(msg.into(), style);
    }

    /// Clear the displayed error or status message.
    pub fn clear_message(&mut self) {
        self.last_message = false;
    }

    fn focus_command(
        &mut self,
        prompt: &str,
        ct: CommandType,
        act: &Action<I>,
        ctx: &EditContext,
    ) -> EditResult<EditInfo, I> {
        self.focused = CurrentFocus::Command;
        self.cmdbar.reset();
        self.cmdbar.set_type(prompt, ct, act, ctx);
        self.clear_message();

        Ok(None)
    }

    fn focus_window(&mut self) -> EditResult<EditInfo, I> {
        self.focused = CurrentFocus::Window;
        self.cmdbar.reset();

        Ok(None)
    }

    /// Perform a command bar action.
    pub fn command_bar(
        &mut self,
        act: &CommandBarAction<I>,
        ctx: &EditContext,
    ) -> EditResult<EditInfo, I> {
        match act {
            CommandBarAction::Focus(s, ct, act) => self.focus_command(s, *ct, act, ctx),
            CommandBarAction::Unfocus => self.focus_window(),
        }
    }

    /// Get a reference to the window layout for the current tab.
    pub fn current_tab(&self) -> UIResult<&WindowLayoutState<W, I>, I> {
        self.tabs.get().ok_or(UIError::NoTab)
    }

    /// Get a mutable reference to the window layout for the current tab.
    pub fn current_tab_mut(&mut self) -> UIResult<&mut WindowLayoutState<W, I>, I> {
        self.tabs.get_mut().ok_or(UIError::NoTab)
    }

    /// Get a reference to the currently focused window.
    pub fn current_window(&self) -> Option<&W> {
        self.tabs.get().and_then(WindowLayoutState::get)
    }

    /// Get a mutable reference to the currently focused window.
    pub fn current_window_mut(&mut self) -> UIResult<&mut W, I> {
        self.current_tab_mut()?.get_mut().ok_or(UIError::NoWindow)
    }

    /// Get the maximum valid tab index.
    fn _max_idx(&self) -> usize {
        self.tabs.len().saturating_sub(1)
    }
}

impl<W, I> TabActions<EditContext, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn tab_close(
        &mut self,
        target: &TabTarget,
        flags: CloseFlags,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let mut filter = |tab: &mut WindowLayoutState<W, I>| -> UIResult<(), I> {
            let _ = tab.window_close(&WindowTarget::All, flags, ctx, store);

            if tab.windows() == 0 {
                return Ok(());
            }

            let msg = "Could not close all windows in tab";
            let err = UIError::Failure(msg.into());

            return Err(err);
        };

        self.tabs.try_close(target, &mut filter, ctx)?;

        Ok(None)
    }

    fn tab_extract(
        &mut self,
        change: &FocusChange,
        side: &MoveDir1D,
        ctx: &EditContext,
        _: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        if self.windows() <= 1 {
            return Ok(Some(InfoMessage::from("Already one window")));
        }

        let tab = self.current_tab_mut()?.extract();

        let (idx, side) = if let Some((idx, _)) = self.tabs.target(change, ctx) {
            (idx, *side)
        } else {
            (self._max_idx(), MoveDir1D::Next)
        };

        self.tabs.insert(idx, side, tab);

        return Ok(None);
    }

    fn tab_focus(
        &mut self,
        change: &FocusChange,
        ctx: &EditContext,
        _: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        self.tabs.focus(change, ctx);

        Ok(None)
    }

    fn tab_move(
        &mut self,
        change: &FocusChange,
        ctx: &EditContext,
        _: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        self.tabs.transfer(change, ctx);

        return Ok(None);
    }

    fn tab_open(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        change: &FocusChange,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let (idx, side) =
            self.tabs.target(change, ctx).unwrap_or((self.tabs.pos(), MoveDir1D::Next));
        let tab = self.current_tab_mut()?.from_target(target, ctx, store)?;

        self.tabs.insert(idx, side, tab);

        return Ok(None);
    }
}

impl<W, I> TabCount for ScreenState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn tabs(&self) -> usize {
        self.tabs.len()
    }
}

impl<W, I> TabContainer<EditContext, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn tab_command(
        &mut self,
        act: &TabAction<I>,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        match act {
            TabAction::Close(target, flags) => self.tab_close(target, *flags, ctx, store),
            TabAction::Extract(target, side) => self.tab_extract(target, side, ctx, store),
            TabAction::Focus(change) => self.tab_focus(change, ctx, store),
            TabAction::Move(change) => self.tab_move(change, ctx, store),
            TabAction::Open(target, change) => self.tab_open(target, change, ctx, store),
            act => {
                let msg = format!("unknown tab action: {act:?}");
                return Err(UIError::Unimplemented(msg));
            },
        }
    }
}

impl<W, I> WindowCount for ScreenState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn windows(&self) -> usize {
        self.tabs.get().map(WindowCount::windows).unwrap_or(0)
    }
}

impl<W, I> WindowContainer<EditContext, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    fn window_command(
        &mut self,
        act: &WindowAction<I>,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        let tab = self.current_tab_mut()?;
        let ret = tab.window_command(act, ctx, store);

        if tab.windows() == 0 {
            self.tabs.remove_current();
        }

        ret
    }
}

macro_rules! delegate_focus {
    ($s: expr, $id: ident => $invoke: expr) => {
        match $s.focused {
            CurrentFocus::Command => {
                let $id = &mut $s.cmdbar;
                $invoke
            },
            CurrentFocus::Window => {
                if let Ok($id) = $s.current_window_mut() {
                    $invoke
                } else {
                    Ok(Default::default())
                }
            },
        }
    };
}

impl<W, I> Editable<EditContext, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Editable<EditContext, Store<I>, I>,
    I: ApplicationInfo,
{
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        delegate_focus!(self, f => f.editor_command(act, ctx, store))
    }
}

impl<W, I> TerminalCursor for ScreenState<W, I>
where
    W: Window<I> + TerminalCursor,
    I: ApplicationInfo,
{
    fn get_term_cursor(&self) -> Option<(u16, u16)> {
        match self.focused {
            CurrentFocus::Command => self.cmdbar.get_term_cursor(),
            CurrentFocus::Window => {
                if let Some(w) = self.current_window() {
                    w.get_term_cursor()
                } else {
                    None
                }
            },
        }
    }
}

impl<W, C, I> Jumpable<C, I> for ScreenState<W, I>
where
    W: Window<I> + Jumpable<C, I>,
    I: ApplicationInfo,
{
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &C,
    ) -> UIResult<usize, I> {
        self.current_tab_mut()?.jump(list, dir, count, ctx)
    }
}

impl<W, I> Promptable<EditContext, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Promptable<EditContext, Store<I>, I>,
    I: ApplicationInfo,
{
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, EditContext)>, I> {
        delegate_focus!(self, f => f.prompt(act, ctx, store))
    }
}

impl<W, I> Scrollable<EditContext, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Scrollable<EditContext, Store<I>, I>,
    I: ApplicationInfo,
{
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        delegate_focus!(self, f => f.scroll(style, ctx, store))
    }
}

impl<W, C, I> Searchable<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Searchable<C, Store<I>, I>,
    I: ApplicationInfo,
{
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        self.current_window_mut()?.search(dir, count, ctx, store)
    }
}

/// Widget for displaying a tabbed window layout with a command bar.
pub struct Screen<'a, W, I = EmptyInfo>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    store: &'a mut Store<I>,
    showdialog: Vec<Span<'a>>,
    showmode: Option<Span<'a>>,

    borders: bool,
    border_style: Style,
    border_style_focused: Style,
    border_type: BorderType,
    cmdbar_style: Style,
    cmdbar_prompt_style: Option<Style>,
    tab_style: Style,
    tab_style_focused: Style,
    divider: Span<'a>,
    focused: bool,

    _p: PhantomData<(W, I)>,
}

impl<'a, W, I> Screen<'a, W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    /// Create a new widget.
    pub fn new(store: &'a mut Store<I>) -> Self {
        Screen {
            store,
            showdialog: Vec::new(),
            showmode: None,
            borders: false,
            border_style: Style::default(),
            border_style_focused: Style::default(),
            border_type: BorderType::Plain,
            cmdbar_style: Style::default(),
            cmdbar_prompt_style: None,
            tab_style: Style::default(),
            tab_style_focused: Style::default(),
            divider: Span::raw("|"),
            focused: true,
            _p: PhantomData,
        }
    }

    /// What [Style] should be used when drawing borders.
    pub fn border_style(mut self, style: Style) -> Self {
        self.border_style = style;
        self
    }

    /// What [Style] should be used when drawing the border of the selected window.
    pub fn border_style_focused(mut self, style: Style) -> Self {
        self.border_style_focused = style;
        self
    }

    /// What characters should be used when drawing borders.
    pub fn border_type(mut self, border_type: BorderType) -> Self {
        self.border_type = border_type;
        self
    }

    /// Indicate whether to draw borders around windows.
    pub fn borders(mut self, borders: bool) -> Self {
        self.borders = borders;
        self
    }

    /// What [Style] should be used when drawing borders.
    pub fn cmdbar_style(mut self, style: Style) -> Self {
        self.cmdbar_style = style;
        self
    }

    /// What [Style] should be used when drawing the border of the selected window.
    pub fn cmdbar_prompt_style(mut self, style: Style) -> Self {
        self.cmdbar_prompt_style = Some(style);
        self
    }

    /// What [Style] should be used for tab names.
    pub fn tab_style(mut self, style: Style) -> Self {
        self.tab_style = style;
        self
    }

    /// What [Style] should be used for the focused tab name.
    pub fn tab_style_focused(mut self, style: Style) -> Self {
        self.tab_style_focused = style;
        self
    }

    /// Set the divider [Span] to place in between tab names.
    ///
    /// This defaults to an unstyled "|".
    pub fn divider(mut self, divider: impl Into<Span<'a>>) -> Self {
        self.divider = divider.into();
        self
    }

    /// Indicates whether the terminal window is currently focused.
    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }

    /// Show the message from an interactive dialog.
    pub fn show_dialog(mut self, dialog: Vec<Cow<'a, str>>) -> Self {
        self.showdialog = dialog.into_iter().map(Span::raw).collect();
        self
    }

    /// Set the mode string to display.
    pub fn show_mode(mut self, mode: Option<String>) -> Self {
        self.showmode = mode.map(bold);
        self
    }
}

impl<'a, W, I> StatefulWidget for Screen<'a, W, I>
where
    W: Window<I>,
    I: ApplicationInfo,
{
    type State = ScreenState<W, I>;

    fn render(mut self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if area.height == 0 {
            return;
        }

        let focused = state.focused;

        let mut compls = match focused {
            CurrentFocus::Command => state.cmdbar.get_completions(),
            CurrentFocus::Window => state.current_window().and_then(WindowOps::get_completions),
        };

        // Determine whether we need to show the tab bar.
        let ntabs = state.tabs.len();
        let tabh = if ntabs > 1 && area.height > 2 { 1 } else { 0 };

        // Determine whether we need to show the completion bar, and how big it should be.
        let cbar = compls
            .as_ref()
            .map(|l| CompletionBar::new(l, area.width))
            .unwrap_or_default();
        let mut barh = cbar.rows;

        // Calculate height for dialog message.
        let dialog = std::mem::take(&mut self.showdialog);
        let cmdh = match dialog.len() {
            0 => 1,
            n => {
                // If we have a dialog message, we'll skip showing a completion bar.
                barh = 0;
                (n as u16).clamp(1, area.height)
            },
        };

        // The rest of the space goes to showing the open windows and the command bar.
        let winh = area.height.saturating_sub(tabh).saturating_sub(barh).saturating_sub(cmdh);

        let init = rect_zero_height(area);
        let tabarea = rect_down(init, tabh);
        let winarea = rect_down(tabarea, winh);
        let bararea = rect_down(winarea, barh);
        let cmdarea = rect_down(bararea, cmdh);

        let titles: Vec<Line> = state
            .tabs
            .iter()
            .map(|tab| {
                let mut spans = vec![];
                let n = tab.windows();

                if n > 1 {
                    spans.push(Span::from(format!("{n} ")));
                }

                if let Some(w) = tab.get() {
                    let mut title = w.get_tab_title(self.store).spans;

                    spans.append(&mut title);
                } else {
                    spans.push(Span::from("[No Name]"));
                }

                Line::from(spans)
            })
            .collect();

        Tabs::new(titles)
            .style(self.tab_style)
            .highlight_style(self.tab_style_focused)
            .divider(self.divider)
            .select(state.tabs.pos())
            .render(tabarea, buf);

        if let Ok(tab) = state.current_tab_mut() {
            WindowLayout::new(self.store)
                .focus(self.focused && focused == CurrentFocus::Window)
                .border_style(self.border_style)
                .border_style_focused(self.border_style_focused)
                .border_type(self.border_type)
                .borders(self.borders)
                .render(winarea, buf, tab);
        }

        if !dialog.is_empty() {
            let iter = dialog.into_iter().take(cmdarea.height as usize);

            for (i, line) in iter.enumerate() {
                let y = cmdarea.y + i as u16;
                buf.set_span(0, y, &line, cmdarea.width);
            }

            return;
        }

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
            .style(self.cmdbar_style)
            .prompt_style(self.cmdbar_prompt_style.unwrap_or(self.cmdbar_style))
            .render(cmdarea, buf, &mut state.cmdbar);

        // Render completion list last so it's drawn on top of the windows.
        if let Some(ref mut completions) = compls {
            match completions.display {
                CompletionDisplay::None => {},
                CompletionDisplay::Bar => {
                    cbar.render(bararea, buf, completions);
                },
                CompletionDisplay::List => {
                    if let Some(cursor) = state.get_term_cursor() {
                        CompletionMenu::new(cursor).render(winarea, buf, completions);
                    }
                },
            }
        }
    }
}
