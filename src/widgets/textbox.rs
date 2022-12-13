//! # Text box
//!
//! ## Overview
//!
//! This text box provides a view of a shared editing buffer, and, on top of passing operations
//! through to an [EditBuffer], is capable of doing the following:
//!
//! - Toggling wrapped and non-wrapped views of the buffer's
//! - Scrolling through the buffer's contents
//! - Rendering line annotations in left and right gutters
//!
//! [EditBuffer]: crate::editing::buffer::EditBuffer
//!
//! ## Example
//!
//! ```
//! use modalkit::{
//!     editing::application::EmptyInfo,
//!     editing::store::Store,
//!     widgets::textbox::TextBoxState,
//! };
//!
//! use tui::layout::Rect;
//!
//! fn main() {
//!     let mut store = Store::<EmptyInfo>::default();
//!     let buffer = store.load_buffer(String::from("*scratch*"));
//!     let mut tbox = TextBoxState::new(buffer);
//!
//!     tbox.set_term_info(Rect::new(0, 0, 6, 4));
//!
//!     tbox.set_text("a\nb\nc\nd\ne\nf\n");
//!     assert_eq!(tbox.get_text(), "a\nb\nc\nd\ne\nf\n");
//!     assert_eq!(tbox.get_lines(), 6);
//!     assert_eq!(tbox.has_lines(4), 4);
//!     assert_eq!(tbox.has_lines(6), 6);
//!     assert_eq!(tbox.has_lines(8), 6);
//! }
//! ```
use std::convert::TryInto;
use std::iter::Iterator;
use std::marker::PhantomData;

use tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Modifier, Style},
    widgets::{Block, StatefulWidget, Widget},
};

use crate::editing::{
    action::{
        Action,
        EditError,
        EditInfo,
        EditResult,
        Editable,
        EditorAction,
        Jumpable,
        PromptAction,
        Promptable,
        Scrollable,
        Searchable,
        UIResult,
    },
    application::{ApplicationInfo, EmptyInfo},
    base::{
        Axis,
        CloseFlags,
        Count,
        MoveDir1D,
        MoveDir2D,
        MoveDirMod,
        MovePosition,
        PositionList,
        ScrollSize,
        ScrollStyle,
        TargetShape,
        ViewportContext,
        WordStyle,
        Wrappable,
    },
    buffer::{CursorGroupId, FollowersInfo, HighlightInfo},
    context::EditContext,
    cursor::Cursor,
    rope::EditRope,
    store::{SharedBuffer, Store},
};

use super::{ScrollActions, TerminalCursor, WindowOps};

/// Line annotation shown in the left gutter.
pub struct LeftGutterInfo {
    text: String,
    style: Style,
}

impl LeftGutterInfo {
    /// Create a new instance.
    pub fn new(text: String, style: Style) -> Self {
        LeftGutterInfo { text, style }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let _ = buf.set_stringn(area.x, area.y, &self.text, area.width as usize, self.style);
    }
}

/// Line annotation shown in the right gutter.
pub struct RightGutterInfo {
    text: String,
    style: Style,
}

impl RightGutterInfo {
    /// Create a new instance.
    pub fn new(text: String, style: Style) -> Self {
        RightGutterInfo { text, style }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let _ = buf.set_stringn(area.x, area.y, &self.text, area.width as usize, self.style);
    }
}

/// Persistent state for [TextBox].
pub struct TextBoxState<I: ApplicationInfo = EmptyInfo> {
    buffer: SharedBuffer<I>,
    group_id: CursorGroupId,
    readonly: bool,

    viewctx: ViewportContext<Cursor>,
    term_area: Rect,
    term_cursor: (u16, u16),
}

/// Widget for rendering a multi-line text box.
pub struct TextBox<'a, I: ApplicationInfo = EmptyInfo> {
    block: Option<Block<'a>>,
    prompt: &'a str,

    lgutter_width: u16,
    rgutter_width: u16,

    _pc: PhantomData<I>,
}

/*
 * If the cursor has moved outside of the viewport, update the corner of the viewport so that the
 * cursor is visible onscreen again.
 */
fn shift_corner_nowrap(cursor: &Cursor, corner: &mut Cursor, width: usize, height: usize) {
    if cursor.y < corner.y {
        corner.set_y(cursor.y);
    } else if cursor.y >= corner.y + height {
        corner.set_y(cursor.y - height + 1);
    }

    if cursor.x < corner.x {
        corner.set_x(cursor.x);
    } else if cursor.x >= corner.x + width {
        corner.set_x(cursor.x - width + 1);
    }
}

fn shift_corner_wrap(cursor: &Cursor, corner: &mut Cursor, height: usize) {
    if cursor.y < corner.y {
        corner.set_y(cursor.y);
        corner.set_x(0);
    } else if cursor.y >= corner.y + height {
        corner.set_y(cursor.y - height + 1);
        corner.set_x(0);
    } else if cursor.y == corner.y && cursor.x < corner.x {
        corner.set_x(0);
    }
}

fn shift_corner(
    viewctx: &mut ViewportContext<Cursor>,
    cursor: &Cursor,
    width: usize,
    height: usize,
) {
    if viewctx.wrap {
        shift_corner_wrap(cursor, &mut viewctx.corner, height);
    } else {
        shift_corner_nowrap(cursor, &mut viewctx.corner, width, height);
    }
}

/*
 * If the cursor has moved outside of the viewport, move the cursor back within the boundaries of
 * the viewport, so it is visible onscreen again.
 */
fn shift_cursor(cursor: &mut Cursor, corner: &Cursor, width: usize, height: usize) {
    if cursor.y < corner.y {
        cursor.set_y(corner.y);
    } else if cursor.y >= corner.y + height {
        cursor.set_y(corner.y + height - 1);
    }

    if cursor.x < corner.x {
        cursor.set_x(corner.x);
    } else if cursor.x >= corner.x + width {
        cursor.set_x(corner.x + width - 1);
    }
}

impl<I> TextBoxState<I>
where
    I: ApplicationInfo,
{
    /// Create state for a new text box.
    pub fn new(buffer: SharedBuffer<I>) -> Self {
        let mut viewctx = ViewportContext::default();
        let group_id = buffer.write().unwrap().create_group();

        viewctx.set_wrap(true);

        TextBoxState {
            buffer,
            group_id,
            readonly: false,

            viewctx,
            term_area: Rect::default(),
            term_cursor: (0, 0),
        }
    }

    /// Get a reference to the shared buffer used by this text box.
    pub fn buffer(&self) -> SharedBuffer<I> {
        self.buffer.clone()
    }

    /// Set whether the buffer contents are modifiable through the [Editable] trait.
    pub fn set_readonly(&mut self, readonly: bool) {
        self.readonly = readonly;
    }

    /// Get the contents of the underlying buffer as an [EditRope].
    pub fn get(&self) -> EditRope {
        self.buffer.read().unwrap().get().clone()
    }

    /// Get the contents of the underlying buffer as a [String].
    pub fn get_text(&self) -> String {
        self.buffer.read().unwrap().get_text()
    }

    /// Replace the contents of the text box's underlying buffer.
    pub fn set_text<T: Into<EditRope>>(&mut self, t: T) {
        self.buffer.write().unwrap().set_text(t)
    }

    /// Clear the text box's underlying buffer of its content, and return it.
    pub fn reset(&mut self) -> EditRope {
        self.buffer.write().unwrap().reset()
    }

    /// Clear the text box's underlying buffer of its content, and return it as a [String].
    pub fn reset_text(&mut self) -> String {
        self.buffer.write().unwrap().reset_text()
    }

    /// Create or update a line annotation for the left gutter.
    pub fn set_left_gutter<'a>(&mut self, line: usize, s: String, style: Option<Style>) {
        let style = style.unwrap_or_default();
        let info = LeftGutterInfo::new(s, style);

        self.buffer.write().unwrap().set_line_info(line, info);
    }

    /// Create or update a line annotation for the right gutter.
    pub fn set_right_gutter<'a>(&mut self, line: usize, s: String, style: Option<Style>) {
        let style = style.unwrap_or_default();
        let info = RightGutterInfo::new(s, style);

        self.buffer.write().unwrap().set_line_info(line, info);
    }

    /// Control whether the text box should wrap long lines when displaying them.
    pub fn set_wrap(&mut self, wrap: bool) {
        self.viewctx.set_wrap(wrap);
    }

    /// Inform the text box what its dimensions and placement on the terminal window is.
    pub fn set_term_info(&mut self, area: Rect) {
        self.viewctx.dimensions = (area.width as usize, area.height as usize);
        self.term_area = area;
    }

    /// Get the leader cursor for this text box's cursor group.
    pub fn get_cursor(&mut self) -> Cursor {
        self.buffer.write().unwrap().get_leader(self.group_id)
    }

    /// Calculate how many lines are in this text box.
    pub fn get_lines(&self) -> usize {
        self.buffer.read().unwrap().get_lines()
    }

    /// Check whether this text box is capable of displaying `max` lines.
    ///
    /// If there are fewer lines available than `max`, this returns the same value as
    /// [get_lines()](TextBoxState::get_lines).
    /// Otherwise, this returns `max`.
    ///
    /// This method is useful for building additional widgets that want to create a [TextBox] with a
    /// flexible height up to `max` lines.
    pub fn has_lines(&self, max: usize) -> usize {
        if self.viewctx.wrap {
            let width = self.viewctx.get_width();
            let mut count = 0;

            if width == 0 {
                return count;
            }

            for line in self.buffer.read().unwrap().lines(0) {
                count += 1;
                count += line.len_chars().saturating_sub(1) / width;

                if count >= max {
                    return max;
                }
            }

            return count;
        } else {
            self.buffer.read().unwrap().get_lines().min(max)
        }
    }
}

macro_rules! c2cgi {
    ($s: expr, $ctx: expr) => {
        &mut ($s.group_id, &$s.viewctx, $ctx)
    };
}

impl<'a, C, I> Editable<C, Store<I>, I> for TextBoxState<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        if self.readonly && !act.is_readonly(ctx) {
            Err(EditError::ReadOnly)
        } else {
            self.buffer.editor_command(act, c2cgi!(self, ctx), store)
        }
    }
}

impl<'a, C, I> Jumpable<C, I> for TextBoxState<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &C,
    ) -> UIResult<usize, I> {
        self.buffer.jump(list, dir, count, c2cgi!(self, ctx))
    }
}

impl<C, I> Promptable<C, Store<I>, I> for TextBoxState<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn prompt(
        &mut self,
        _: &PromptAction,
        _: &C,
        _: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, C)>, I> {
        Err(EditError::Failure("Not at a prompt".to_string()))
    }
}

impl<'a, C, I> Searchable<C, Store<I>, I> for TextBoxState<I>
where
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
        self.buffer.search(dir, count, c2cgi!(self, ctx), store)
    }
}

impl<'a, C, I> ScrollActions<C, Store<I>, I> for TextBoxState<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn dirscroll(
        &mut self,
        dir: MoveDir2D,
        size: ScrollSize,
        count: &Count,
        ctx: &C,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let count = ctx.resolve(count);

        let height = self.viewctx.dimensions.1;
        let rows = match size {
            ScrollSize::Cell => count,
            ScrollSize::HalfPage => count.saturating_mul(height) / 2,
            ScrollSize::Page => count.saturating_mul(height),
        };

        let width = self.viewctx.dimensions.0;
        let cols = match size {
            ScrollSize::Cell => count,
            ScrollSize::HalfPage => count.saturating_mul(width) / 2,
            ScrollSize::Page => count.saturating_mul(width),
        };

        match (dir, self.viewctx.wrap) {
            (MoveDir2D::Up, _) => self.viewctx.corner.up(rows),
            (MoveDir2D::Down, _) => self.viewctx.corner.down(rows),
            (MoveDir2D::Left, false) => self.viewctx.corner.left(cols),
            (MoveDir2D::Right, false) => self.viewctx.corner.right(cols),
            (MoveDir2D::Left | MoveDir2D::Right, true) => (),
        };

        /*
         * We do a quick dance here: moving the viewport should move the cursor so that it stays
         * visible on the screen. The cursor should not be shifted past the last line or last
         * column, though, so we clamp it after shifting it. Since the cursor should never
         * be off-screen, this also sets a boundary of how far we can move the viewport.
         */
        let mut cursor = self.get_cursor();
        let mut buffer = self.buffer.write().unwrap();
        shift_cursor(&mut cursor, &self.viewctx.corner, width, height);
        buffer.clamp(&mut cursor, c2cgi!(self, ctx));
        shift_corner(&mut self.viewctx, &cursor, width, height);
        buffer.set_leader(self.group_id, cursor);

        Ok(None)
    }

    fn cursorpos(
        &mut self,
        pos: MovePosition,
        axis: Axis,
        _: &C,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        if axis == Axis::Horizontal && self.viewctx.wrap {
            return Ok(None);
        }

        let (width, height) = self.viewctx.dimensions;
        let (width, height) = (width as usize, height as usize);
        let cursor = self.get_cursor();
        shift_corner(&mut self.viewctx, &cursor, width, height);

        match (axis, pos) {
            (Axis::Horizontal, MovePosition::Beginning) => {
                self.viewctx.corner.set_x(cursor.x);
            },
            (Axis::Horizontal, MovePosition::Middle) => {
                let off = cursor.x.saturating_add(1).saturating_sub(width / 2);

                self.viewctx.corner.set_x(off);
            },
            (Axis::Horizontal, MovePosition::End) => {
                let off = cursor.x.saturating_add(1).saturating_sub(width);

                self.viewctx.corner.set_x(off);
            },
            (Axis::Vertical, MovePosition::Beginning) => {
                self.viewctx.corner.set_y(cursor.y);
            },
            (Axis::Vertical, MovePosition::Middle) => {
                let off = cursor.y.saturating_add(1).saturating_sub(height / 2);

                self.viewctx.corner.set_y(off);
            },
            (Axis::Vertical, MovePosition::End) => {
                let off = cursor.y.saturating_add(1).saturating_sub(height);

                self.viewctx.corner.set_y(off);
            },
        }

        Ok(None)
    }

    fn linepos(
        &mut self,
        pos: MovePosition,
        count: &Count,
        ctx: &C,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let mut buffer = self.buffer.write().unwrap();
        let max = buffer.get_lines();
        let line = ctx.resolve(count).min(max).saturating_sub(1);

        let height = self.viewctx.dimensions.1 as usize;

        buffer.set_leader(self.group_id, Cursor::new(line, 0));

        match pos {
            MovePosition::Beginning => {
                self.viewctx.corner.set_y(line);
            },
            MovePosition::Middle => {
                let off = line.saturating_add(1).saturating_sub(height / 2);

                self.viewctx.corner.set_y(off);
            },
            MovePosition::End => {
                let off = line.saturating_add(1).saturating_sub(height);

                self.viewctx.corner.set_y(off);
            },
        }

        Ok(None)
    }
}

impl<'a, C, I> Scrollable<C, Store<I>, I> for TextBoxState<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        match style {
            ScrollStyle::Direction2D(dir, size, count) => {
                return self.dirscroll(*dir, *size, count, ctx, store);
            },
            ScrollStyle::CursorPos(pos, axis) => {
                return self.cursorpos(*pos, *axis, ctx, store);
            },
            ScrollStyle::LinePos(pos, count) => {
                return self.linepos(*pos, count, ctx, store);
            },
        }
    }
}

impl<I> TerminalCursor for TextBoxState<I>
where
    I: ApplicationInfo,
{
    fn get_term_cursor(&self) -> Option<(u16, u16)> {
        self.term_cursor.into()
    }
}

impl<I> WindowOps<I> for TextBoxState<I>
where
    I: ApplicationInfo,
{
    fn dup(&self, _: &mut Store<I>) -> Self {
        let buffer = self.buffer.clone();
        let group_id = buffer.write().unwrap().create_group();

        TextBoxState {
            buffer,
            group_id,
            readonly: self.readonly,

            viewctx: self.viewctx.clone(),
            term_area: Rect::default(),
            term_cursor: (0, 0),
        }
    }

    fn close(&mut self, _: CloseFlags, _: &mut Store<I>) -> bool {
        true
    }

    fn draw(&mut self, area: Rect, buf: &mut Buffer, _: bool, _: &mut Store<I>) {
        TextBox::new().render(area, buf, self);
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        self.buffer.read().unwrap().get_cursor_word(self.group_id, style)
    }

    fn get_selected_word(&self) -> Option<String> {
        self.buffer.read().unwrap().get_selected_word(self.group_id)
    }
}

impl<'a, I> TextBox<'a, I>
where
    I: ApplicationInfo,
{
    /// Create a new widget.
    pub fn new() -> Self {
        TextBox {
            block: None,
            prompt: "",

            lgutter_width: 0,
            rgutter_width: 0,

            _pc: PhantomData,
        }
    }

    /// Wrap this text box in a [Block].
    pub fn block(mut self, block: Block<'a>) -> Self {
        self.block = Some(block);
        self
    }

    /// Display a prompt in the top left of the text box when focused.
    pub fn prompt(mut self, prompt: &'a str) -> Self {
        self.prompt = prompt;
        self
    }

    /// Set the width of the left gutter.
    pub fn left_gutter(mut self, lw: u16) -> Self {
        self.lgutter_width = lw;
        self
    }

    /// Set the width of the right gutter.
    pub fn right_gutter(mut self, rw: u16) -> Self {
        self.rgutter_width = rw;
        self
    }

    #[inline]
    fn _highlight_followers(
        &self,
        line: usize,
        start: usize,
        end: usize,
        (x, y): (u16, u16),
        followers: &FollowersInfo,
        buf: &mut Buffer,
    ) {
        let hlstyled = Style::default().add_modifier(Modifier::REVERSED);
        let cs = (line, start);
        let ce = (line, end);

        for follower in followers.query(cs..ce) {
            let fx = x + (follower.value.x - start) as u16;
            let fa = Rect::new(fx, y, 1, 1);
            buf.set_style(fa, hlstyled);
        }
    }

    #[inline]
    fn _set_style(&self, start: usize, h1: usize, h2: usize, (x, y): (u16, u16), buf: &mut Buffer) {
        let tx: u16 = x + (h1 - start) as u16;
        let selwidth: u16 = (h2 - h1 + 1).try_into().unwrap();

        let hlstyled = Style::default().add_modifier(Modifier::REVERSED);
        let selarea = Rect::new(tx, y, selwidth, 1);

        buf.set_style(selarea, hlstyled);
    }

    #[inline]
    fn _highlight_line(
        &self,
        line: usize,
        start: usize,
        end: usize,
        (x, y): (u16, u16),
        hls: &HighlightInfo,
        buf: &mut Buffer,
    ) {
        for selection in hls.query_point(line) {
            let (sb, se, shape) = &selection.value;

            let maxcol = end.saturating_sub(1);
            let range = start..end;

            match shape {
                TargetShape::CharWise => {
                    let x1 = if line == sb.y { sb.x.max(start) } else { start };
                    let x2 = if line == se.y {
                        se.x.min(maxcol)
                    } else {
                        maxcol
                    };

                    if range.contains(&x1) && range.contains(&x2) {
                        self._set_style(start, x1, x2, (x, y), buf);
                    }
                },
                TargetShape::LineWise => {
                    let hlstyled = Style::default().add_modifier(Modifier::REVERSED);
                    let selwidth: u16 = (end - start).try_into().unwrap();
                    let selarea = Rect::new(x, y, selwidth, 1);

                    buf.set_style(selarea, hlstyled);
                },
                TargetShape::BlockWise => {
                    let lx = sb.x.min(se.x);
                    let rx = sb.x.max(se.x);

                    let x1 = lx.max(start);
                    let x2 = rx.min(maxcol);

                    if range.contains(&x1) && range.contains(&x2) {
                        self._set_style(start, x1, x2, (x, y), buf);
                    }
                },
            }
        }
    }

    fn _render_lines_wrap(
        &mut self,
        area: Rect,
        gutters: (Rect, Rect),
        buf: &mut Buffer,
        hinfo: HighlightInfo,
        finfo: FollowersInfo,
        state: &mut TextBoxState<I>,
    ) {
        let bot = area.bottom();
        let x = area.left();
        let mut y = area.top();

        let height = area.height as usize;
        let width = area.width as usize;

        /*
         * If the cursor has moved off-screen, update the viewport corner.
         *
         * There might be several long wrapped lines between the new corner and the cursor
         * afterwards, though, so we update the corner again after handling wrapping if needed.
         */
        let cursor = state.get_cursor();
        shift_corner_wrap(&cursor, &mut state.viewctx.corner, height);

        let cby = state.viewctx.corner.y;
        let cbx = state.viewctx.corner.x;

        let unstyled = Style::default();

        let text = state.buffer.read().unwrap();
        let mut line = cby;
        let mut lines = text.lines_at(line, cbx);

        let mut wrapped = Vec::new();
        let mut sawcursor = false;

        while let Some(s) = lines.next() {
            if wrapped.len() >= height && sawcursor {
                break;
            }

            let mut first = true;
            let mut off = 0;
            let slen = s.len_chars();

            while off < slen && (wrapped.len() < height || !sawcursor) {
                let start = off;
                let end = (start + width).min(slen);
                let swrapped = s.slice(start..end).to_string();

                let cursor_line = line == cursor.y && (start..=end).contains(&cursor.x);

                wrapped.push((line, start, end, swrapped, cursor_line, first));

                if cursor_line {
                    sawcursor = true;
                }

                off = end;
                first = false;
            }

            if slen == 0 {
                wrapped.push((line, 0, 0, s.to_string(), line == cursor.y, true));
            }

            line += 1;
        }

        if wrapped.len() > height {
            let n = wrapped.len() - height;
            let _ = wrapped.drain(..n);
            let (line, start, _, _, _, _) = wrapped.first().unwrap();
            state.viewctx.corner.set_y(*line);
            state.viewctx.corner.set_x(*start);
        }

        for (line, start, end, s, cursor_line, first) in wrapped.into_iter() {
            if y >= bot {
                break;
            }

            if first {
                let lgutter = text.get_line_info::<LeftGutterInfo>(line);
                let rgutter = text.get_line_info::<RightGutterInfo>(line);

                if let Some(ref lgi) = lgutter {
                    let lga = Rect::new(gutters.0.x, y, gutters.0.width, 0);
                    lgi.render(lga, buf);
                }

                if let Some(ref rgi) = rgutter {
                    let rga = Rect::new(gutters.1.x, y, gutters.1.width, 0);
                    rgi.render(rga, buf);
                }
            }

            let _ = buf.set_stringn(x, y, s, width, unstyled);

            if cursor_line {
                let coff = (cursor.x - start) as u16;
                state.term_cursor = (x + coff, y);
            }

            self._highlight_followers(line, start, end, (x, y), &finfo, buf);
            self._highlight_line(line, start, end, (x, y), &hinfo, buf);

            y += 1;
        }
    }

    fn _render_lines_nowrap(
        &mut self,
        area: Rect,
        gutters: (Rect, Rect),
        buf: &mut Buffer,
        hinfo: HighlightInfo,
        finfo: FollowersInfo,
        state: &mut TextBoxState<I>,
    ) {
        let bot = area.bottom();
        let x = area.left();
        let mut y = area.top();

        let height = area.height as usize;
        let width = area.width as usize;

        // If the cursor has moved off-screen, update the viewport corner.
        let cursor = state.get_cursor();
        shift_corner_nowrap(&cursor, &mut state.viewctx.corner, width, height);

        let cby = state.viewctx.corner.y;
        let cbx = state.viewctx.corner.x;

        let unstyled = Style::default();

        let text = state.buffer.read().unwrap();
        let mut line = cby;
        let mut lines = text.lines(line);

        while y < bot {
            if let Some(s) = lines.next() {
                let lgutter = text.get_line_info::<LeftGutterInfo>(line);
                let rgutter = text.get_line_info::<RightGutterInfo>(line);

                let slen = s.len_chars();
                let start = cbx;
                let end = slen;

                if let Some(ref lgi) = lgutter {
                    let lga = Rect::new(gutters.0.x, y, gutters.0.width, 0);
                    lgi.render(lga, buf);
                }

                if cbx < slen {
                    let _ = buf.set_stringn(x, y, s.slice(start..end).to_string(), width, unstyled);
                }

                if let Some(ref rgi) = rgutter {
                    let rga = Rect::new(gutters.1.x, y, gutters.1.width, 0);
                    rgi.render(rga, buf);
                }

                if line == cursor.y && (start..=end).contains(&cursor.x) {
                    let coff = (cursor.x - start) as u16;
                    state.term_cursor = (x + coff, y);
                }

                self._highlight_followers(line, start, end, (x, y), &finfo, buf);
                self._highlight_line(line, cbx, slen, (x, y), &hinfo, buf);

                y += 1;
                line += 1;
            } else {
                break;
            }
        }
    }

    #[inline]
    fn _selection_intervals(&self, state: &mut TextBoxState<I>) -> HighlightInfo {
        state.buffer.write().unwrap()._selection_intervals(state.group_id)
    }

    #[inline]
    fn _follower_intervals(&self, state: &mut TextBoxState<I>) -> FollowersInfo {
        state.buffer.write().unwrap()._follower_intervals(state.group_id)
    }

    fn _render_lines(&mut self, area: Rect, buf: &mut Buffer, state: &mut TextBoxState<I>) {
        let hinfo = self._selection_intervals(state);
        let finfo = self._follower_intervals(state);

        let (lgw, rgw) = if area.width <= self.lgutter_width + self.rgutter_width {
            (0, 0)
        } else {
            (self.lgutter_width, self.rgutter_width)
        };
        let textw = area.width - lgw - rgw;
        let lga = Rect::new(area.x, area.y, lgw, area.height);
        let texta = Rect::new(area.x + lgw, area.y, textw, area.height);
        let rga = Rect::new(area.x + lgw + textw, area.y, rgw, area.height);
        let gutters = (lga, rga);

        state.set_term_info(area);

        if state.viewctx.wrap {
            self._render_lines_wrap(texta, gutters, buf, hinfo, finfo, state);
        } else {
            self._render_lines_nowrap(texta, gutters, buf, hinfo, finfo, state);
        }
    }
}

impl<'a, I> StatefulWidget for TextBox<'a, I>
where
    I: ApplicationInfo,
{
    type State = TextBoxState<I>;

    fn render(mut self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let area = match self.block.take() {
            Some(block) => {
                let inner_area = block.inner(area);
                block.render(area, buf);
                inner_area
            },
            None => area,
        };

        let plen = self.prompt.len() as u16;
        let gutter = Rect::new(area.x, area.y, plen, area.height);

        let text_area =
            Rect::new(area.x + plen, area.y, area.width.saturating_sub(plen), area.height);

        if text_area.width == 0 || text_area.height == 0 {
            return;
        }

        // First, draw the prompt in the gutter.
        let _ = buf.set_stringn(
            gutter.left(),
            gutter.top(),
            &self.prompt,
            gutter.width as usize,
            Style::default(),
        );

        // Now draw the text.
        self._render_lines(text_area, buf, state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::action::{EditAction, HistoryAction};
    use crate::editing::base::{EditTarget, MoveDir1D, MoveType};
    use crate::editing::store::Store;
    use crate::env::vim::VimContext;

    macro_rules! mv {
        ($mt: expr) => {
            EditTarget::Motion($mt, Count::Contextual)
        };
        ($mt: expr, $c: expr) => {
            EditTarget::Motion($mt, Count::Exact($c))
        };
    }

    macro_rules! dirscroll {
        ($tbox: expr, $d: expr, $s: expr, $c: expr, $ctx: expr, $store: expr) => {
            $tbox
                .scroll(&ScrollStyle::Direction2D($d, $s, $c), $ctx, &mut $store)
                .unwrap()
        };
    }

    macro_rules! cursorpos {
        ($tbox: expr, $pos: expr, $axis: expr, $ctx: expr, $store: expr) => {
            $tbox
                .scroll(&ScrollStyle::CursorPos($pos, $axis), $ctx, &mut $store)
                .unwrap()
        };
    }

    macro_rules! linepos {
        ($tbox: expr, $pos: expr, $c: expr, $ctx: expr, $store: expr) => {
            $tbox.scroll(&ScrollStyle::LinePos($pos, $c), $ctx, &mut $store).unwrap()
        };
    }

    fn mkbox() -> (TextBoxState, Store<EmptyInfo>) {
        let mut store = Store::default();
        let buffer = store.load_buffer("".to_string());

        (TextBoxState::new(buffer), store)
    }

    fn mkboxstr(s: &str) -> (TextBoxState, VimContext, Store<EmptyInfo>) {
        let (mut b, mut store) = mkbox();
        let ctx = VimContext::default();

        b.set_text(s);
        b.editor_command(&HistoryAction::Checkpoint.into(), &ctx, &mut store)
            .unwrap();

        return (b, ctx, store);
    }

    #[test]
    fn test_scroll_dir1d() {
        let (mut tbox, mut ctx, mut store) = mkboxstr(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n\
            uvwxyz,.<>\n\
            -_=+[{]}\\|\n\
            !@#$%^&*()\n\
            1234567890\n",
        );

        tbox.set_wrap(false);
        tbox.set_term_info(Rect::new(0, 0, 6, 4));

        // Scroll by terminal cells
        ctx.action.count = Some(4);
        dirscroll!(tbox, MoveDir2D::Down, ScrollSize::Cell, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));

        ctx.action.count = Some(2);
        dirscroll!(tbox, MoveDir2D::Up, ScrollSize::Cell, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));

        ctx.action.count = Some(6);
        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Cell, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 6));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        ctx.action.count = Some(2);
        dirscroll!(tbox, MoveDir2D::Left, ScrollSize::Cell, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        ctx.action.count = None;

        // Scroll by half page
        dirscroll!(tbox, MoveDir2D::Down, ScrollSize::HalfPage, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        dirscroll!(tbox, MoveDir2D::Up, ScrollSize::HalfPage, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::HalfPage, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 7));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 7));

        dirscroll!(tbox, MoveDir2D::Left, ScrollSize::HalfPage, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 7));

        // Scroll by page
        dirscroll!(tbox, MoveDir2D::Down, ScrollSize::Page, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(6, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(6, 7));

        dirscroll!(tbox, MoveDir2D::Up, ScrollSize::Page, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 7));

        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Page, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 9));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 9));

        dirscroll!(tbox, MoveDir2D::Left, ScrollSize::Page, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 3));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 8));

        // Cannot scroll cursor and viewport past the end of the line.
        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Page, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 9));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 9));

        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Page, Count::Contextual, &ctx, store);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 9));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 9));
    }

    #[test]
    fn test_scroll_cursorpos() {
        let (mut tbox, ctx, mut store) = mkboxstr(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n\
            uvwxyz,.<>\n\
            -_=+[{]}\\|\n\
            !@#$%^&*()\n\
            1234567890\n",
        );

        tbox.set_wrap(false);
        tbox.set_term_info(Rect::new(0, 0, 4, 4));

        // When the cursor is at the top-left corner, these actions are effectively no-ops.
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::Beginning, Axis::Vertical, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::Middle, Axis::Vertical, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::End, Axis::Vertical, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::Beginning, Axis::Horizontal, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::Middle, Axis::Horizontal, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::End, Axis::Horizontal, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // Move the cursor to the second column of the fifth line, and vertically position cursor.
        let mov = mv!(MoveType::BufferLineOffset, 5);
        let act = EditorAction::Edit(EditAction::Motion.into(), mov);
        tbox.editor_command(&act, &ctx, &mut store).unwrap();
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));

        let mov = mv!(MoveType::LineColumnOffset, 2);
        let act = EditorAction::Edit(EditAction::Motion.into(), mov);
        tbox.editor_command(&act, &ctx, &mut store).unwrap();
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));

        cursorpos!(tbox, MovePosition::Beginning, Axis::Vertical, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 0));

        cursorpos!(tbox, MovePosition::End, Axis::Vertical, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));
        assert_eq!(tbox.viewctx.corner, Cursor::new(1, 0));

        cursorpos!(tbox, MovePosition::Middle, Axis::Vertical, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 0));

        // Move the cursor to the fifth column, and horizontally position cursor.
        let mov = mv!(MoveType::LineColumnOffset, 5);
        let act = EditorAction::Edit(EditAction::Motion.into(), mov);
        tbox.editor_command(&act, &ctx, &mut store).unwrap();
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));

        cursorpos!(tbox, MovePosition::Beginning, Axis::Horizontal, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 4));

        cursorpos!(tbox, MovePosition::End, Axis::Horizontal, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 1));

        cursorpos!(tbox, MovePosition::Middle, Axis::Horizontal, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 3));

        // Vertically positioning the cursor after a FirstWord.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        let act = EditorAction::Edit(EditAction::Motion.into(), mv!(mov, 0));
        tbox.editor_command(&act, &ctx, &mut store).unwrap();
        cursorpos!(tbox, MovePosition::Beginning, Axis::Vertical, &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 0));
    }

    #[test]
    fn test_scroll_linepos() {
        let (mut tbox, ctx, mut store) = mkboxstr(
            "1234567890\n\
            abcdefghij\n\
            klmnopqrst\n\
            uvwxyz,.<>\n\
            -_=+[{]}\\|\n\
            !@#$%^&*()\n\
            1234567890\n",
        );

        tbox.set_wrap(false);
        tbox.set_term_info(Rect::new(0, 0, 4, 4));

        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // Scroll so that the 3rd line at the top of the screen.
        linepos!(tbox, MovePosition::Beginning, Count::Exact(3), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(2, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 0));

        // Scroll so that the 7th line is in the middle of the screen.
        linepos!(tbox, MovePosition::Middle, Count::Exact(7), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(6, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(5, 0));

        // The 1st line cannot be in the middle of the screen.
        linepos!(tbox, MovePosition::Middle, Count::Exact(1), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 1st line cannot be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(1), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 2nd line cannot be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(2), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(1, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 3nd line cannot be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(3), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(2, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 4th line can be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(4), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(3, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 5th line can be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(5), &ctx, store);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(1, 0));
    }

    #[test]
    fn test_reset_text() {
        let (mut tbox, ctx, mut store) = mkboxstr("foo\nbar\nbaz");

        let mov = mv!(MoveType::BufferLineOffset, 3);
        let act = EditorAction::Edit(EditAction::Motion.into(), mov);
        tbox.editor_command(&act, &ctx, &mut store).unwrap();

        assert_eq!(tbox.get_text(), "foo\nbar\nbaz\n");
        assert_eq!(tbox.get_cursor(), Cursor::new(2, 0));

        assert_eq!(tbox.reset_text(), "foo\nbar\nbaz\n");
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));

        assert_eq!(tbox.get_text(), "\n");
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
    }

    #[test]
    fn test_render_nowrap() {
        let (mut tbox, ctx, mut store) = mkboxstr("foo\nbar\nbaz\nquux 1 2 3 4 5");

        tbox.set_wrap(false);

        let mut buffer = Buffer::empty(Rect::new(0, 0, 10, 10));
        let area = Rect::new(0, 8, 10, 2);

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.get_term_cursor(), (2, 8).into());

        // Move the cursor to the fourth line, thereby moving corner.
        let mov = mv!(MoveType::BufferLineOffset, 4);
        let act = EditorAction::Edit(EditAction::Motion.into(), mov);
        tbox.editor_command(&act, &ctx, &mut store).unwrap();

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(3, 0));
        assert_eq!(tbox.get_term_cursor(), (2, 9).into());

        // Move the cursor to the end of the fourth line, again moving corner.
        let mov = mv!(MoveType::LineColumnOffset, 14);
        let act = EditorAction::Edit(EditAction::Motion.into(), mov);
        tbox.editor_command(&act, &ctx, &mut store).unwrap();

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 6));
        assert_eq!(tbox.get_cursor(), Cursor::new(3, 13));
        assert_eq!(tbox.get_term_cursor(), (9, 9).into());

        // Now move back to the top-left corner.
        let mov = mv!(MoveType::BufferByteOffset, 0);
        let act = EditorAction::Edit(EditAction::Motion.into(), mov);
        tbox.editor_command(&act, &ctx, &mut store).unwrap();

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.get_term_cursor(), (2, 8).into());
    }
}
