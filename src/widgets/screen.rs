//! # Tabbed window layout
//!
//! ## Overview
//!
//! This widget can be used by consumers to create a tabbed window layout containing horizontal and
//! vertical splits. It builds on top of [CommandBarState] and [WindowLayoutState] to accomplish
//! this, both of which can also be used on their own if something different is needed.
use std::borrow::Cow;
use std::cmp::Ordering;
use std::iter::Iterator;
use std::marker::PhantomData;
use std::ops::Index;

use serde::{Deserialize, Serialize};

use tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier as StyleModifier, Style},
    text::{Line, Span},
    widgets::{BorderType, StatefulWidget, Tabs, Widget},
};

use super::{
    cmdbar::{CommandBar, CommandBarState},
    util::{rect_down, rect_zero_height},
    windows::{WindowActions, WindowLayout, WindowLayoutDescription, WindowLayoutState},
    TerminalCursor,
    Window,
    WindowOps,
};

use crate::editing::{
    action::{
        Action,
        CommandBarAction,
        EditInfo,
        EditResult,
        Editable,
        EditorAction,
        InfoMessage,
        Jumpable,
        PromptAction,
        Promptable,
        Scrollable,
        Searchable,
        TabAction,
        TabContainer,
        TabCount,
        UIError,
        UIResult,
        WindowAction,
        WindowContainer,
        WindowCount,
    },
    application::{ApplicationInfo, EmptyInfo},
    base::{
        CloseFlags,
        CommandType,
        CompletionDisplay,
        Count,
        FocusChange,
        MoveDir1D,
        MoveDir2D,
        MoveDirMod,
        MovePosition,
        OpenTarget,
        PositionList,
        ScrollStyle,
        TabTarget,
        WindowTarget,
    },
    completion::CompletionList,
    context::EditContext,
    store::Store,
};

use crate::util::{idx_move, idx_offset};

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

/// List of items that tracks a current and previous focus.
pub struct FocusList<T> {
    /// The list of items.
    items: Vec<T>,

    /// Previously focused position.
    idx_last: usize,

    /// Currently focused position.
    idx_curr: usize,
}

impl<T> FocusList<T> {
    /// Create a new [FocusList] given an original set of items.
    pub fn new(items: Vec<T>) -> Self {
        FocusList { items, idx_last: 0, idx_curr: 0 }
    }

    /// Returns how many items are in this list.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Indicates whether this list is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Get a reference to the currently focused item.
    pub fn get(&self) -> Option<&T> {
        self.items.get(self.idx_curr)
    }

    /// Get a mutable reference to the currently focused item.
    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.items.get_mut(self.idx_curr)
    }

    /// Get the currently focused position in this list.
    pub fn pos(&self) -> usize {
        self.idx_curr
    }

    /// Iterate over mutable references to the items in this list.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.items.iter_mut()
    }

    /// Iterate over references to the items in this list.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items.iter()
    }

    /// Try closing targeted items using `f` until one fails or all targets are closed.
    ///
    /// If the target cannot be closed, focus will be moved to it.
    pub fn try_close<E, F, C>(&mut self, target: &TabTarget, mut f: F, ctx: &C) -> Result<(), E>
    where
        F: FnMut(&mut T) -> Result<(), E>,
        C: EditContext,
    {
        let mut result = Ok(());

        match target {
            TabTarget::All => {
                let old = std::mem::take(&mut self.items);

                // The first item we fail to close will be at the front.
                self.idx_curr = 0;
                self.idx_last = 0;

                for mut item in old.into_iter() {
                    if result.is_ok() {
                        if let e @ Err(_) = f(&mut item) {
                            result = e;
                        } else {
                            continue;
                        }
                    }

                    self.items.push(item);
                }
            },
            TabTarget::AllBut(fc) => {
                if let Some((idx, _)) = self.target(fc, ctx) {
                    let old = std::mem::take(&mut self.items);

                    // Focus will end on the remaining item or the error.
                    self.idx_curr = 0;
                    self.idx_last = 0;

                    for (i, mut tab) in old.into_iter().enumerate() {
                        if i == idx {
                            self.items.push(tab);
                            continue;
                        }

                        if result.is_ok() {
                            if let e @ Err(_) = f(&mut tab) {
                                result = e;

                                // Focus on the error.
                                self.set_focus(self.len());
                            } else {
                                continue;
                            }
                        }

                        self.items.push(tab);
                    }
                }
            },
            TabTarget::Single(fc) => {
                if let Some((idx, _)) = self.target(fc, ctx) {
                    if let e @ Err(_) = f(&mut self.items[idx]) {
                        result = e;

                        // Focus on the error.
                        self.set_focus(idx);
                    } else {
                        self.remove(idx);
                    }
                }
            },
        }

        return result;
    }

    fn _max_idx(&self) -> usize {
        self.len().saturating_sub(1)
    }

    fn _idx<C: EditContext>(&self, count: &Count, ctx: &C) -> Option<usize> {
        ctx.resolve(count).checked_sub(1)
    }

    /// Determine the index of the item targeted by a given [FocusChange].
    ///
    /// The [MoveDir1D] value returned indicates which side of the index to insert at.
    pub fn target<C>(&self, change: &FocusChange, ctx: &C) -> Option<(usize, MoveDir1D)>
    where
        C: EditContext,
    {
        let target = match change {
            FocusChange::Current => self.idx_curr,
            FocusChange::Offset(count, false) => {
                let idx = if let Some(n) = self._idx(count, ctx) {
                    n
                } else {
                    return Some((0, MoveDir1D::Previous));
                };

                if idx >= self.len() {
                    // Invalid tab index; do nothing.
                    return None;
                }

                idx
            },
            FocusChange::Offset(count, true) => {
                if let Some(n) = self._idx(count, ctx) {
                    n.min(self._max_idx())
                } else {
                    return Some((0, MoveDir1D::Previous));
                }
            },
            FocusChange::Direction1D(dir, count, wrap) => {
                let ntabs = self.len();
                let count = ctx.resolve(count);

                return idx_offset(self.idx_curr, count, dir, ntabs, *wrap).map(|i| (i, *dir));
            },
            FocusChange::Direction2D(MoveDir2D::Left, count) => {
                let ntabs = self.len();
                let count = ctx.resolve(count) % ntabs;

                (ntabs + self.idx_curr - count) % ntabs
            },
            FocusChange::Direction2D(MoveDir2D::Right, count) => {
                let ntabs = self.len();
                let count = ctx.resolve(count) % ntabs;

                (self.idx_curr + count) % ntabs
            },
            FocusChange::Direction2D(MoveDir2D::Up | MoveDir2D::Down, _) => {
                return None;
            },
            FocusChange::Position(MovePosition::Beginning) => {
                return Some((0, MoveDir1D::Previous));
            },
            FocusChange::Position(MovePosition::Middle) => self.len() / 2,
            FocusChange::Position(MovePosition::End) => self.len().saturating_sub(1),
            FocusChange::PreviouslyFocused => self.idx_last,
        };

        Some((target, MoveDir1D::Next))
    }

    /// Insert an item before or after a given index.
    pub fn insert(&mut self, idx: usize, side: MoveDir1D, item: T) {
        let idx = match side {
            MoveDir1D::Previous => idx,
            MoveDir1D::Next => idx + 1,
        };

        self.items.insert(idx, item);

        if self.idx_curr >= idx {
            self.idx_last = self.idx_curr + 1;
        } else {
            self.idx_last = self.idx_curr;
        }

        self.idx_curr = idx;
    }

    /// Remove the item located at a given index.
    pub fn remove(&mut self, idx: usize) {
        if idx >= self.len() {
            return;
        }

        let _ = self.items.remove(idx);
        let max = self._max_idx();

        self.idx_last = match idx.cmp(&self.idx_last) {
            Ordering::Less => self.idx_last.saturating_sub(1).min(max),
            Ordering::Equal => 0,
            Ordering::Greater => self.idx_last.min(max),
        };

        self.idx_curr = match idx.cmp(&self.idx_curr) {
            Ordering::Less => self.idx_curr.saturating_sub(1).min(max),
            Ordering::Equal => self.idx_last,
            Ordering::Greater => self.idx_curr.min(max),
        };
    }

    /// Remove the currently focused item.
    pub fn remove_current(&mut self) {
        self.remove(self.idx_curr);
    }

    fn set_focus(&mut self, idx: usize) {
        if idx == self.idx_curr {
            return;
        }

        self.idx_last = self.idx_curr;
        self.idx_curr = idx;
    }

    /// Switch focus to a new item.
    pub fn focus<C: EditContext>(&mut self, change: &FocusChange, ctx: &C) {
        if let Some((idx, _)) = self.target(change, ctx) {
            self.set_focus(idx);
        }
    }

    /// Move the currently focused item to a new position.
    pub fn transfer<C: EditContext>(&mut self, change: &FocusChange, ctx: &C) {
        if let Some((idx, side)) = self.target(change, ctx) {
            idx_move(&mut self.items, &mut self.idx_curr, idx, &mut self.idx_last, side);
        }
    }

    /// Push a new item onto the end of this list.
    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }
}

impl<T> Default for FocusList<T> {
    fn default() -> Self {
        FocusList::new(Vec::new())
    }
}

impl<T> Index<usize> for FocusList<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        self.items.index(index)
    }
}

impl<T> AsRef<[T]> for FocusList<T> {
    fn as_ref(&self) -> &[T] {
        self.items.as_ref()
    }
}

impl<T> From<T> for FocusList<T> {
    fn from(item: T) -> Self {
        FocusList::new(vec![item])
    }
}

/// A description of open tabs.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(bound(deserialize = "I::WindowId: Deserialize<'de>"))]
#[serde(bound(serialize = "I::WindowId: Serialize"))]
pub struct TabLayoutDescription<I: ApplicationInfo> {
    /// The description of the window layout for each tab.
    pub tabs: Vec<WindowLayoutDescription<I>>,
}

impl<I: ApplicationInfo> TabLayoutDescription<I> {
    /// Create a new collection of tabs from this description.
    pub fn to_layout<W: Window<I>>(
        self,
        area: Option<Rect>,
        store: &mut Store<I>,
    ) -> UIResult<FocusList<WindowLayoutState<W, I>>, I> {
        self.tabs
            .into_iter()
            .map(|desc| desc.to_layout(area, store))
            .collect::<UIResult<Vec<_>, I>>()
            .map(FocusList::new)
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
    pub fn as_description(&self) -> TabLayoutDescription<I> {
        TabLayoutDescription {
            tabs: self.tabs.iter().map(WindowLayoutState::as_description).collect(),
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

    fn focus_command(&mut self, ct: CommandType, dir: MoveDir1D) -> EditResult<EditInfo, I> {
        self.focused = CurrentFocus::Command;
        self.cmdbar.reset();
        self.cmdbar.set_type(ct, dir);
        self.clear_message();

        Ok(None)
    }

    fn focus_window(&mut self) -> EditResult<EditInfo, I> {
        self.focused = CurrentFocus::Window;
        self.cmdbar.reset();

        Ok(None)
    }

    /// Perform a command bar action.
    pub fn command_bar<C: EditContext>(
        &mut self,
        act: &CommandBarAction,
        ctx: &C,
    ) -> EditResult<EditInfo, I> {
        match act {
            CommandBarAction::Focus(ct) => self.focus_command(*ct, ctx.get_search_regex_dir()),
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

impl<W, C, I> TabActions<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I>,
    C: EditContext,
    I: ApplicationInfo,
{
    fn tab_close(
        &mut self,
        target: &TabTarget,
        flags: CloseFlags,
        ctx: &C,
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
        ctx: &C,
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
        ctx: &C,
        _: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        self.tabs.focus(change, ctx);

        Ok(None)
    }

    fn tab_move(
        &mut self,
        change: &FocusChange,
        ctx: &C,
        _: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        self.tabs.transfer(change, ctx);

        return Ok(None);
    }

    fn tab_open(
        &mut self,
        target: &OpenTarget<I::WindowId>,
        change: &FocusChange,
        ctx: &C,
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

impl<W, C, I> TabContainer<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I>,
    C: EditContext,
    I: ApplicationInfo,
{
    fn tab_command(
        &mut self,
        act: &TabAction<I>,
        ctx: &C,
        store: &mut Store<I>,
    ) -> UIResult<EditInfo, I> {
        match act {
            TabAction::Close(target, flags) => self.tab_close(target, *flags, ctx, store),
            TabAction::Extract(target, side) => self.tab_extract(target, side, ctx, store),
            TabAction::Focus(change) => self.tab_focus(change, ctx, store),
            TabAction::Move(change) => self.tab_move(change, ctx, store),
            TabAction::Open(target, change) => self.tab_open(target, change, ctx, store),
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

impl<W, C, I> WindowContainer<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I>,
    C: EditContext,
    I: ApplicationInfo,
{
    fn window_command(
        &mut self,
        act: &WindowAction<I>,
        ctx: &C,
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

impl<W, C, I> Editable<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Editable<C, Store<I>, I>,
    C: EditContext,
    I: ApplicationInfo,
{
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &C,
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

impl<W, C, I> Promptable<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Promptable<C, Store<I>, I>,
    C: EditContext,
    I: ApplicationInfo,
{
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, C)>, I> {
        delegate_focus!(self, f => f.prompt(act, ctx, store))
    }
}

impl<W, C, I> Scrollable<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Scrollable<C, Store<I>, I>,
    C: EditContext,
    I: ApplicationInfo,
{
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        delegate_focus!(self, f => f.scroll(style, ctx, store))
    }
}

impl<W, C, I> Searchable<C, Store<I>, I> for ScreenState<W, I>
where
    W: Window<I> + Searchable<C, Store<I>, I>,
    C: EditContext,
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
    border_type: BorderType,
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
            border_type: BorderType::Plain,
            focused: true,
            _p: PhantomData,
        }
    }

    /// What [Style] should be used when drawing borders.
    pub fn border_style(mut self, style: Style) -> Self {
        self.border_style = style;
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

        let titles = state
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
            .style(Style::default().fg(Color::White))
            .highlight_style(Style::default().fg(Color::Yellow))
            .divider("|")
            .select(state.tabs.pos())
            .render(tabarea, buf);

        if let Ok(tab) = state.current_tab_mut() {
            WindowLayout::new(self.store)
                .focus(self.focused && focused == CurrentFocus::Window)
                .border_style(self.border_style)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::application::EmptyInfo;
    use crate::editing::base::MoveDir1D::{Next, Previous as Prev};
    use crate::env::vim::VimContext;

    fn close1(v: &mut char) -> Result<(), char> {
        if *v == 'c' {
            Err(*v)
        } else {
            Ok(())
        }
    }

    fn close2(_: &mut char) -> Result<(), bool> {
        Ok(())
    }

    #[test]
    fn test_insert() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        list.insert(2, Next, 'q');
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'q', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 0);

        list.insert(2, Prev, 'r');
        assert_eq!(list.as_ref(), &['a', 'b', 'r', 'c', 'q', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 4);

        // Insert at the end of the list.
        list.insert(8, Next, 's');
        assert_eq!(list.as_ref(), &['a', 'b', 'r', 'c', 'q', 'd', 'e', 'f', 'g', 's']);
        assert_eq!(list.idx_curr, 9);
        assert_eq!(list.idx_last, 2);

        // Insert at the beginning of the list.
        list.insert(0, Prev, 't');
        assert_eq!(list.as_ref(), &['t', 'a', 'b', 'r', 'c', 'q', 'd', 'e', 'f', 'g', 's']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 10);
    }

    #[test]
    fn test_remove() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);

        list.idx_curr = 1;
        list.idx_last = 4;

        // Remove an index after idx_curr and idx_last.
        list.remove(6);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'd', 'e', 'f']);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 4);

        // Remove an index in between idx_curr and idx_last.
        list.remove(2);
        assert_eq!(list.as_ref(), &['a', 'b', 'd', 'e', 'f']);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 3);

        // Removing idx_curr goes to idx_last.
        list.remove(1);
        assert_eq!(list.as_ref(), &['a', 'd', 'e', 'f']);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 2);

        // Removing idx_curr goes to 0 when it's also equal to idx_last.
        list.remove(2);
        assert_eq!(list.as_ref(), &['a', 'd', 'f']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);
    }

    #[test]
    fn test_focus_dir() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Move two forward.
        list.focus(&FocusChange::Direction1D(Next, 2.into(), true), &ctx);
        assert_eq!(list.idx_curr, 2);

        // Move three forward.
        list.focus(&FocusChange::Direction1D(Next, 3.into(), true), &ctx);
        assert_eq!(list.idx_curr, 5);
        assert_eq!(list.idx_last, 2);

        // Move six forward and wrap around.
        list.focus(&FocusChange::Direction1D(Next, 6.into(), true), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 5);

        // Move six backwards, wrapping around to where we had been.
        list.focus(&FocusChange::Direction1D(Prev, 6.into(), true), &ctx);
        assert_eq!(list.idx_curr, 5);
        assert_eq!(list.idx_last, 4);

        // Move two backwards.
        list.focus(&FocusChange::Direction1D(Prev, 2.into(), true), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 5);

        // Check that we can prevent wrapping around at the end.
        list.focus(&FocusChange::Direction1D(Next, 5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 5);

        // Check that we can prevent wrapping around at the beginning.
        list.focus(&FocusChange::Direction1D(Prev, 5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 5);

        // We should be able to move backwards within the boundaries.
        list.focus(&FocusChange::Direction1D(Prev, 1.into(), false), &ctx);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 3);

        // We should be able to move forwards within the boundaries.
        list.focus(&FocusChange::Direction1D(Next, 2.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 2);
    }

    #[test]
    fn test_focus_offset() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Move to the 2nd item.
        list.focus(&FocusChange::Offset(2.into(), false), &ctx);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 0);

        // Move to the 5th item.
        list.focus(&FocusChange::Offset(5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 1);

        // Trying to move past the end does nothing.
        list.focus(&FocusChange::Offset(9.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 1);

        // Trying to move past the end now succeeds, and goes to the last item.
        list.focus(&FocusChange::Offset(9.into(), true), &ctx);
        assert_eq!(list.idx_curr, 6);
        assert_eq!(list.idx_last, 4);

        // Move to the 1st item.
        list.focus(&FocusChange::Offset(1.into(), true), &ctx);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 6);
    }

    #[test]
    fn test_focus_position() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Move to the end.
        list.focus(&FocusChange::Position(MovePosition::End), &ctx);
        assert_eq!(list.idx_curr, 6);
        assert_eq!(list.idx_last, 0);

        // Move to the middle.
        list.focus(&FocusChange::Position(MovePosition::Middle), &ctx);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 6);

        // Move to the beginning.
        list.focus(&FocusChange::Position(MovePosition::Beginning), &ctx);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 3);
    }

    #[test]
    fn test_focus_previously_focused() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // First, move to the 5th item.
        list.focus(&FocusChange::Offset(5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 0);

        // Going to the 5th item again doesn't touch idx_last.
        list.focus(&FocusChange::Offset(5.into(), false), &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 0);

        // Jump to the previously focused item.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 4);

        // Jump back again.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 0);

        // First, move to the 2nd item.
        list.focus(&FocusChange::Offset(2.into(), false), &ctx);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 4);

        // Jump to the previously focused item.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 4);
        assert_eq!(list.idx_last, 1);

        // Jump once more.
        list.focus(&FocusChange::PreviouslyFocused, &ctx);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 4);
    }

    #[test]
    fn test_move_item() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        list.idx_curr = 1;
        list.idx_last = 3;

        // Move 'b' to the end.
        list.transfer(&FocusChange::Position(MovePosition::End), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'd', 'e', 'f', 'g', 'b']);
        assert_eq!(list.idx_curr, 6);
        assert_eq!(list.idx_last, 2);

        // Move 'b' to the beginning.
        list.transfer(&FocusChange::Position(MovePosition::Beginning), &ctx);
        assert_eq!(list.as_ref(), &['b', 'a', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 3);

        // Move 'b' to after the fourth item, 'd'.
        list.transfer(&FocusChange::Offset(4.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'd', 'b', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 2);

        // Move 'b' to after the first item, 'a'.
        list.transfer(&FocusChange::Offset(1.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 1);
        assert_eq!(list.idx_last, 3);

        // Move 'b' to after the zeroth item.
        list.transfer(&FocusChange::Offset(0.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['b', 'a', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 3);

        // Move 'b' forwards three times.
        list.transfer(&FocusChange::Direction1D(Next, 3.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'd', 'b', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 2);

        // Move 'b' backwards once.
        list.transfer(&FocusChange::Direction1D(Prev, 1.into(), true), &ctx);
        assert_eq!(list.as_ref(), &['a', 'c', 'b', 'd', 'e', 'f', 'g']);
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 3);
    }

    #[test]
    fn test_try_close_all() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        list.idx_curr = 4;
        list.idx_last = 5;

        // Close the first several items before failing.
        let res = list.try_close(&TabTarget::All, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Trying again makes no progress.
        let res = list.try_close(&TabTarget::All, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Using close2() we make it all the way through.
        let res = list.try_close(&TabTarget::All, close2, &ctx);
        assert_eq!(list.is_empty(), true);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);
    }

    #[test]
    fn test_try_close_all_but() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        list.idx_curr = 4;
        list.idx_last = 4;

        // Try to close everything but 'e', but stop at 'c'.
        let target = TabTarget::AllBut(FocusChange::Current);
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Trying again makes no progress.
        let target = TabTarget::AllBut(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);

        // Using close2() we make it all the way through.
        let target = TabTarget::AllBut(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close2, &ctx);
        assert_eq!(list.as_ref(), &['e']);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 0);
        assert_eq!(list.idx_last, 0);
    }

    #[test]
    fn test_try_close_single() {
        let mut list = FocusList::new(vec!['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let ctx = VimContext::<EmptyInfo>::default();

        list.idx_curr = 5;
        list.idx_last = 0;

        // Cannot close 'c' using close1(); focus moves to 'c'.
        let target = TabTarget::Single(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(res, Err('c'));
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 5);

        // But we can close 'd' just fine.
        let target = TabTarget::Single(FocusChange::Offset(4.into(), true));
        let res = list.try_close(&target, close1, &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'c', 'e', 'f', 'g']);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 2);
        assert_eq!(list.idx_last, 4);

        // We can close 'c' using close2().
        let target = TabTarget::Single(FocusChange::Offset(3.into(), true));
        let res = list.try_close(&target, close2, &ctx);
        assert_eq!(list.as_ref(), &['a', 'b', 'e', 'f', 'g']);
        assert_eq!(res, Ok(()));
        assert_eq!(list.idx_curr, 3);
        assert_eq!(list.idx_last, 3);
    }
}
