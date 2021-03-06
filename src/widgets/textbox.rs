use std::convert::TryInto;
use std::iter::Iterator;
use std::marker::PhantomData;

use intervaltree::IntervalTree;

use tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Modifier, Style},
    widgets::{Block, StatefulWidget, Widget},
};

use crate::editing::base::{
    Application,
    Axis,
    Char,
    CloseFlags,
    Count,
    CursorAction,
    EditAction,
    EditContext,
    EditResult,
    EditTarget,
    HistoryAction,
    Mark,
    MoveDir1D,
    MoveDir2D,
    MovePosition,
    ScrollSize,
    ScrollStyle,
    SelectionCursorChange,
    TargetShape,
    TargetShapeFilter,
    ViewportContext,
    Wrappable,
};

use crate::editing::{
    buffer::{CursorGroupId, Editable},
    cursor::Cursor,
    store::SharedBuffer,
};

use super::{Focusable, TerminalCursor, Window};

pub struct LeftGutterInfo {
    text: String,
    style: Style,
}

impl LeftGutterInfo {
    pub fn new(text: String, style: Style) -> Self {
        LeftGutterInfo { text, style }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let _ = buf.set_stringn(area.x, area.y, &self.text, area.width as usize, self.style);
    }
}

pub struct RightGutterInfo {
    text: String,
    style: Style,
}

impl RightGutterInfo {
    pub fn new(text: String, style: Style) -> Self {
        RightGutterInfo { text, style }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let _ = buf.set_stringn(area.x, area.y, &self.text, area.width as usize, self.style);
    }
}

pub struct TextBoxState<C: EditContext, P: Application> {
    buffer: SharedBuffer<C, P>,
    group_id: CursorGroupId,

    viewctx: ViewportContext<Cursor>,
    term_area: Rect,
    term_cursor: (u16, u16),
}

pub struct TextBox<'a, C: EditContext, P: Application> {
    block: Option<Block<'a>>,
    prompt: &'a str,

    lgutter_width: u16,
    rgutter_width: u16,

    _pc: PhantomData<(C, P)>,
}

type HighlightInfo = IntervalTree<usize, (Cursor, Cursor, TargetShape)>;
type FollowersInfo = IntervalTree<(usize, usize), Cursor>;

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

impl<C, P> TextBoxState<C, P>
where
    C: EditContext,
    P: Application,
{
    pub fn new(buffer: SharedBuffer<C, P>) -> Self {
        let group_id = buffer.try_write().unwrap().create_group();
        let mut viewctx = ViewportContext::default();

        viewctx.set_wrap(true);

        TextBoxState {
            buffer,
            group_id,

            viewctx,
            term_area: Rect::default(),
            term_cursor: (0, 0),
        }
    }

    pub fn get_text(&self) -> String {
        self.buffer.try_read().unwrap().get_text()
    }

    pub fn set_text<T: Into<String>>(&mut self, t: T) {
        self.buffer.try_write().unwrap().set_text(t)
    }

    pub fn reset_text(&mut self) -> String {
        self.buffer.try_write().unwrap().reset_text()
    }

    pub fn set_left_gutter<'a>(&mut self, line: usize, s: String, style: Option<Style>) {
        let style = style.unwrap_or_default();
        let info = LeftGutterInfo::new(s, style);

        self.buffer.write().unwrap().set_line_info(line, info);
    }

    pub fn set_right_gutter<'a>(&mut self, line: usize, s: String, style: Option<Style>) {
        let style = style.unwrap_or_default();
        let info = RightGutterInfo::new(s, style);

        self.buffer.write().unwrap().set_line_info(line, info);
    }

    pub fn set_wrap(&mut self, wrap: bool) {
        self.viewctx.set_wrap(wrap);
    }

    pub fn set_term_info(&mut self, area: Rect) {
        self.viewctx.dimensions = (area.width as usize, area.height as usize);
        self.term_area = area;
    }

    pub fn get_cursor(&self) -> Cursor {
        self.buffer.try_read().unwrap().get_leader(self.group_id)
    }

    pub fn get_lines(&self) -> usize {
        self.buffer.try_read().unwrap().get_lines()
    }

    pub fn has_lines(&self, max: usize) -> usize {
        if self.viewctx.wrap {
            let width = self.viewctx.get_width();
            let mut count = 0;

            if width == 0 {
                return count;
            }

            for line in self.buffer.try_read().unwrap().lines(0) {
                count += 1;
                count += line.len().saturating_sub(1) / width;

                if count >= max {
                    return max;
                }
            }

            return count;
        } else {
            self.buffer.try_read().unwrap().get_lines().min(max)
        }
    }

    fn dirscroll(
        &mut self,
        dir: MoveDir2D,
        size: ScrollSize,
        count: &Count,
        ctx: &C,
    ) -> EditResult {
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
        let mut buffer = self.buffer.try_write().unwrap();
        shift_cursor(&mut cursor, &self.viewctx.corner, width, height);
        buffer.clamp(&mut cursor, &(self.group_id, &self.viewctx, &ctx));
        shift_corner(&mut self.viewctx, &cursor, width, height);
        buffer.set_leader(self.group_id, cursor);

        Ok(None)
    }

    fn cursorpos(&mut self, pos: MovePosition, axis: Axis, _: &C) -> EditResult {
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

    fn linepos(&mut self, pos: MovePosition, count: &Count, ctx: &C) -> EditResult {
        let mut buffer = self.buffer.try_write().unwrap();
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

impl<C, P> Editable<C> for TextBoxState<C, P>
where
    C: EditContext,
    P: Application,
{
    fn edit(&mut self, operation: &EditAction, motion: &EditTarget, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.edit(operation, motion, &ctx)
    }

    fn type_char(&mut self, ch: Char, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.type_char(ch, &ctx)
    }

    fn selcursor_set(&mut self, change: &SelectionCursorChange, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.selcursor_set(change, &ctx)
    }

    fn selection_split_lines(&mut self, filter: TargetShapeFilter, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.selection_split_lines(filter, &ctx)
    }

    fn paste(&mut self, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.paste(dir, count, &ctx)
    }

    fn open_line(&mut self, dir: MoveDir1D, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.open_line(dir, &ctx)
    }

    fn mark(&mut self, name: Mark, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.mark(name, &ctx)
    }

    fn history_command(&mut self, act: HistoryAction, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.history_command(act, &ctx)
    }

    fn cursor_command(&mut self, act: CursorAction, ctx: &C) -> EditResult {
        let ctx = (self.group_id, &self.viewctx, ctx);

        self.buffer.cursor_command(act, &ctx)
    }
}

impl<C, P> Focusable<C> for TextBoxState<C, P>
where
    C: EditContext,
    P: Application,
{
    fn scroll(&mut self, style: &ScrollStyle, ctx: &C) -> EditResult {
        match style {
            ScrollStyle::Direction2D(dir, size, count) => {
                return self.dirscroll(*dir, *size, count, ctx);
            },
            ScrollStyle::CursorPos(pos, axis) => {
                return self.cursorpos(*pos, *axis, ctx);
            },
            ScrollStyle::LinePos(pos, count) => {
                return self.linepos(*pos, count, ctx);
            },
        }
    }
}

impl<C, P> TerminalCursor for TextBoxState<C, P>
where
    C: EditContext,
    P: Application,
{
    fn get_term_cursor(&self) -> (u16, u16) {
        self.term_cursor
    }
}

impl<C, P> Window for TextBoxState<C, P>
where
    C: EditContext,
    P: Application,
{
    fn draw(&mut self, area: Rect, buf: &mut Buffer) {
        TextBox::new().render(area, buf, self);
    }

    fn dup(&self) -> Self {
        let buffer = self.buffer.clone();
        let group_id = buffer.try_write().unwrap().create_group();

        TextBoxState {
            buffer,
            group_id,

            viewctx: self.viewctx.clone(),
            term_area: Rect::default(),
            term_cursor: (0, 0),
        }
    }

    fn close(&mut self, _: CloseFlags) -> bool {
        true
    }
}

impl<'a, C, P> TextBox<'a, C, P>
where
    C: EditContext,
    P: Application,
{
    pub fn new() -> Self {
        TextBox {
            block: None,
            prompt: "",

            lgutter_width: 0,
            rgutter_width: 0,

            _pc: PhantomData,
        }
    }

    pub fn block(mut self, block: Block<'a>) -> Self {
        self.block = Some(block);
        self
    }

    pub fn prompt(mut self, prompt: &'a str) -> Self {
        self.prompt = prompt;
        self
    }

    pub fn left_gutter(mut self, lw: u16) -> Self {
        self.lgutter_width = lw;
        self
    }

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

            let hlstyled = Style::default().add_modifier(Modifier::REVERSED);
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
                        let tx: u16 = x + (x1 - start) as u16;
                        let selwidth: u16 = (x2 - x1 + 1).try_into().unwrap();

                        let selarea = Rect::new(tx, y, selwidth, 1);

                        buf.set_style(selarea, hlstyled);
                    }
                },
                TargetShape::LineWise => {
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
                        let tx: u16 = x + (x1 - start) as u16;
                        let selwidth: u16 = (x2 - x1 + 1).try_into().unwrap();

                        let selarea = Rect::new(tx, y, selwidth, 1);

                        buf.set_style(selarea, hlstyled);
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
        state: &mut TextBoxState<C, P>,
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

        let text = state.buffer.try_read().unwrap();
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
            let slen = s.len();

            while off < slen && y < bot && (wrapped.len() < height || !sawcursor) {
                let start = off;
                let end = (start + width).min(slen);
                let swrapped = s[start..end].to_string();

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
        state: &mut TextBoxState<C, P>,
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

        let text = state.buffer.try_read().unwrap();
        let mut line = cby;
        let mut lines = text.lines(line);

        while y < bot {
            if let Some(s) = lines.next() {
                let lgutter = text.get_line_info::<LeftGutterInfo>(line);
                let rgutter = text.get_line_info::<RightGutterInfo>(line);

                let slen = s.len();
                let start = cbx;
                let end = slen;

                if let Some(ref lgi) = lgutter {
                    let lga = Rect::new(gutters.0.x, y, gutters.0.width, 0);
                    lgi.render(lga, buf);
                }

                if cbx < slen {
                    let _ = buf.set_stringn(x, y, &s[start..end], width, unstyled);
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
    fn _selection_intervals(&self, state: &mut TextBoxState<C, P>) -> HighlightInfo {
        state
            .buffer
            .try_read()
            .unwrap()
            .get_group_selections(state.group_id)
            .into_iter()
            .flatten()
            .map(|s| (s.0.y..s.1.y.saturating_add(1), s))
            .collect()
    }

    #[inline]
    fn _follower_intervals(&self, state: &mut TextBoxState<C, P>) -> FollowersInfo {
        state
            .buffer
            .try_read()
            .unwrap()
            .get_followers(state.group_id)
            .into_iter()
            .map(|c| ((c.y, c.x)..(c.y, c.x + 1), c))
            .collect()
    }

    fn _render_lines(&mut self, area: Rect, buf: &mut Buffer, state: &mut TextBoxState<C, P>) {
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

impl<'a, C, P> StatefulWidget for TextBox<'a, C, P>
where
    C: EditContext,
    P: Application,
{
    type State = TextBoxState<C, P>;

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
    use crate::editing::base::MoveType;
    use crate::editing::store::Store;
    use crate::vim::VimContext;

    macro_rules! mv {
        ($mt: expr) => {
            EditTarget::Motion($mt, Count::Contextual)
        };
        ($mt: expr, $c: expr) => {
            EditTarget::Motion($mt, Count::Exact($c))
        };
    }

    macro_rules! dirscroll {
        ($tbox: expr, $d: expr, $s: expr, $c: expr, $ctx: expr) => {
            $tbox.scroll(&ScrollStyle::Direction2D($d, $s, $c), $ctx).unwrap()
        };
    }

    macro_rules! cursorpos {
        ($tbox: expr, $pos: expr, $axis: expr, $ctx: expr) => {
            $tbox.scroll(&ScrollStyle::CursorPos($pos, $axis), $ctx).unwrap()
        };
    }

    macro_rules! linepos {
        ($tbox: expr, $pos: expr, $c: expr, $ctx: expr) => {
            $tbox.scroll(&ScrollStyle::LinePos($pos, $c), $ctx).unwrap()
        };
    }

    fn mkbox() -> TextBoxState<VimContext, ()> {
        let store = Store::new();
        let buffer = Store::new_buffer(&store);

        TextBoxState::new(buffer)
    }

    fn mkboxstr(s: &str) -> (TextBoxState<VimContext, ()>, VimContext) {
        let mut b = mkbox();
        let ctx = VimContext::default();

        b.set_text(s);
        b.history_command(HistoryAction::Checkpoint, &ctx).unwrap();

        return (b, ctx);
    }

    #[test]
    fn test_scroll_dir1d() {
        let (mut tbox, mut ctx) = mkboxstr(
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
        dirscroll!(tbox, MoveDir2D::Down, ScrollSize::Cell, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));

        ctx.action.count = Some(2);
        dirscroll!(tbox, MoveDir2D::Up, ScrollSize::Cell, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));

        ctx.action.count = Some(6);
        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Cell, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 6));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        ctx.action.count = Some(2);
        dirscroll!(tbox, MoveDir2D::Left, ScrollSize::Cell, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        ctx.action.count = None;

        // Scroll by half page
        dirscroll!(tbox, MoveDir2D::Down, ScrollSize::HalfPage, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        dirscroll!(tbox, MoveDir2D::Up, ScrollSize::HalfPage, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 6));

        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::HalfPage, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 7));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 7));

        dirscroll!(tbox, MoveDir2D::Left, ScrollSize::HalfPage, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 7));

        // Scroll by page
        dirscroll!(tbox, MoveDir2D::Down, ScrollSize::Page, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(6, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(6, 7));

        dirscroll!(tbox, MoveDir2D::Up, ScrollSize::Page, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 4));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 7));

        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Page, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 9));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 9));

        dirscroll!(tbox, MoveDir2D::Left, ScrollSize::Page, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 3));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 8));

        // Cannot scroll cursor and viewport past the end of the line.
        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Page, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 9));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 9));

        dirscroll!(tbox, MoveDir2D::Right, ScrollSize::Page, Count::Contextual, &ctx);
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 9));
        assert_eq!(tbox.get_cursor(), Cursor::new(5, 9));
    }

    #[test]
    fn test_scroll_cursorpos() {
        let (mut tbox, ctx) = mkboxstr(
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

        cursorpos!(tbox, MovePosition::Beginning, Axis::Vertical, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::Middle, Axis::Vertical, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::End, Axis::Vertical, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::Beginning, Axis::Horizontal, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::Middle, Axis::Horizontal, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        cursorpos!(tbox, MovePosition::End, Axis::Horizontal, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // Move the cursor to the second column of the fifth line, and vertically position cursor.
        let mov = mv!(MoveType::BufferLineOffset, 5);
        tbox.edit(&EditAction::Motion, &mov, &ctx).unwrap();
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));

        let mov = mv!(MoveType::LineColumnOffset, 2);
        tbox.edit(&EditAction::Motion, &mov, &ctx).unwrap();
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));

        cursorpos!(tbox, MovePosition::Beginning, Axis::Vertical, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 0));

        cursorpos!(tbox, MovePosition::End, Axis::Vertical, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));
        assert_eq!(tbox.viewctx.corner, Cursor::new(1, 0));

        cursorpos!(tbox, MovePosition::Middle, Axis::Vertical, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 1));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 0));

        // Move the cursor to the fifth column, and horizontally position cursor.
        let mov = mv!(MoveType::LineColumnOffset, 5);
        tbox.edit(&EditAction::Motion, &mov, &ctx).unwrap();
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));

        cursorpos!(tbox, MovePosition::Beginning, Axis::Horizontal, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 4));

        cursorpos!(tbox, MovePosition::End, Axis::Horizontal, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 1));

        cursorpos!(tbox, MovePosition::Middle, Axis::Horizontal, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 4));
        assert_eq!(tbox.viewctx.corner, Cursor::new(3, 3));

        // Vertically positioning the cursor after a FirstWord.
        let mov = MoveType::FirstWord(MoveDir1D::Next);
        tbox.edit(&EditAction::Motion, &mv!(mov, 0), &ctx).unwrap();
        cursorpos!(tbox, MovePosition::Beginning, Axis::Vertical, &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(4, 0));
    }

    #[test]
    fn test_scroll_linepos() {
        let (mut tbox, ctx) = mkboxstr(
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
        linepos!(tbox, MovePosition::Beginning, Count::Exact(3), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(2, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 0));

        // Scroll so that the 7th line is in the middle of the screen.
        linepos!(tbox, MovePosition::Middle, Count::Exact(7), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(6, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(5, 0));

        // The 1st line cannot be in the middle of the screen.
        linepos!(tbox, MovePosition::Middle, Count::Exact(1), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 1st line cannot be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(1), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 2nd line cannot be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(2), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(1, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 3nd line cannot be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(3), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(2, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 4th line can be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(4), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(3, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));

        // The 5th line can be at the bottom of the screen.
        linepos!(tbox, MovePosition::End, Count::Exact(5), &ctx);
        assert_eq!(tbox.get_cursor(), Cursor::new(4, 0));
        assert_eq!(tbox.viewctx.corner, Cursor::new(1, 0));
    }

    #[test]
    fn test_reset_text() {
        let (mut tbox, ctx) = mkboxstr("foo\nbar\nbaz");

        let mov = mv!(MoveType::BufferLineOffset, 3);
        tbox.edit(&EditAction::Motion, &mov, &ctx).unwrap();

        assert_eq!(tbox.get_text(), "foo\nbar\nbaz\n");
        assert_eq!(tbox.get_cursor(), Cursor::new(2, 0));

        assert_eq!(tbox.reset_text(), "foo\nbar\nbaz\n");
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));

        assert_eq!(tbox.get_text(), "\n");
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
    }

    #[test]
    fn test_render_nowrap() {
        let (mut tbox, ctx) = mkboxstr("foo\nbar\nbaz\nquux 1 2 3 4 5");

        tbox.set_wrap(false);

        let mut buffer = Buffer::empty(Rect::new(0, 0, 10, 10));
        let area = Rect::new(0, 8, 10, 2);

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.get_term_cursor(), (2, 8));

        // Move the cursor to the fourth line, thereby moving corner.
        let mov = mv!(MoveType::BufferLineOffset, 4);
        tbox.edit(&EditAction::Motion, &mov, &ctx).unwrap();

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(3, 0));
        assert_eq!(tbox.get_term_cursor(), (2, 9));

        // Move the cursor to the end of the fourth line, again moving corner.
        let mov = mv!(MoveType::LineColumnOffset, 14);
        tbox.edit(&EditAction::Motion, &mov, &ctx).unwrap();

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(2, 6));
        assert_eq!(tbox.get_cursor(), Cursor::new(3, 13));
        assert_eq!(tbox.get_term_cursor(), (9, 9));

        // Now move back to the top-left corner.
        let mov = mv!(MoveType::BufferByteOffset, 0);
        tbox.edit(&EditAction::Motion, &mov, &ctx).unwrap();

        TextBox::new().prompt("> ").render(area, &mut buffer, &mut tbox);

        assert_eq!(tbox.viewctx.corner, Cursor::new(0, 0));
        assert_eq!(tbox.get_cursor(), Cursor::new(0, 0));
        assert_eq!(tbox.get_term_cursor(), (2, 8));
    }
}
