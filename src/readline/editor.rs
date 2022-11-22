use std::cmp::Ordering;
use std::io::{stdout, BufWriter, Stdout};
use std::ops::{Deref, DerefMut, RangeInclusive};

use regex::Regex;

use crossterm::{
    cursor::MoveTo,
    style::{Print, PrintStyledContent, Stylize},
    terminal::ScrollUp,
    QueueableCommand,
};

use crate::util::is_newline;

use crate::editing::{
    action::{
        CursorAction,
        EditAction,
        EditResult,
        Editable,
        HistoryAction,
        InsertTextAction,
        Jumpable,
        SelectionAction,
        UIResult,
    },
    application::ApplicationInfo,
    base::{EditTarget, Mark, MoveDir1D, PositionList, TargetShape, ViewportContext, Wrappable},
    buffer::{CursorGroupId, EditBuffer},
    context::EditContext,
    cursor::Cursor,
    history::{HistoryList, ScrollbackState},
    rope::EditRope,
    store::{BufferId, Store},
};

pub struct EditorContext {
    pub stdout: BufWriter<Stdout>,
    pub top: u16,
}

impl Default for EditorContext {
    fn default() -> Self {
        let stdout = BufWriter::new(stdout());
        let top = 0;

        EditorContext { stdout, top }
    }
}

pub struct Editor<I>
where
    I: ApplicationInfo,
{
    buffer: EditBuffer<I>,
    scrollback: ScrollbackState,

    viewctx: ViewportContext<Cursor>,
    gid: CursorGroupId,
}

impl<I> Editor<I>
where
    I: ApplicationInfo,
{
    pub fn new() -> Self {
        let id = BufferId(0);
        let mut buffer = EditBuffer::new(id);
        let mut viewctx = ViewportContext::default();
        viewctx.set_wrap(true);

        let gid = buffer.create_group();

        Editor {
            buffer,
            scrollback: ScrollbackState::Pending,
            viewctx,
            gid,
        }
    }

    pub fn resize(&mut self, width: u16, height: u16) {
        self.viewctx.dimensions = (width.into(), height.into());
    }

    pub fn is_blank(&self) -> bool {
        self.buffer.is_blank()
    }

    fn _highlight_ranges(
        &self,
        line: usize,
        start: usize,
        end: usize,
    ) -> Vec<RangeInclusive<usize>> {
        let hinfo = self.buffer._selection_intervals(self.gid);
        let mut ranges = vec![];

        for selection in hinfo.query_point(line) {
            let (sb, se, shape) = &selection.value;

            let maxcol = end.saturating_sub(1);
            let range = start..end;

            match shape {
                TargetShape::LineWise => {
                    ranges.push(start..=maxcol);
                    break;
                },
                TargetShape::CharWise => {
                    let x1 = if line == sb.y { sb.x.max(start) } else { start };
                    let x2 = if line == se.y {
                        se.x.min(maxcol)
                    } else {
                        maxcol
                    };

                    if range.contains(&x1) && range.contains(&x2) {
                        ranges.push(x1..=x2);
                    }
                },
                TargetShape::BlockWise => {
                    let lx = sb.x.min(se.x);
                    let rx = sb.x.max(se.x);

                    let x1 = lx.max(start);
                    let x2 = rx.min(maxcol);

                    if range.contains(&x1) && range.contains(&x2) {
                        ranges.push(x1..=x2);
                    }
                },
            }
        }

        ranges.sort_by(|a, b| {
            let res = a.start().cmp(b.start());

            if res != Ordering::Equal {
                return res;
            }

            return a.end().cmp(b.end());
        });

        return ranges;
    }

    fn _redraw_wrap(
        &mut self,
        prompt: &Option<String>,
        off: u16,
        context: &mut EditorContext,
    ) -> crossterm::Result<u16> {
        let width = self.viewctx.dimensions.0;
        let height = self.viewctx.dimensions.1;

        let cursor = self.buffer.get_leader(self.gid);

        let cby = self.viewctx.corner.y;
        let cbx = self.viewctx.corner.x;

        let mut line = cby;
        let mut lines = self.buffer.lines_at(line, cbx);

        let mut wrapped = Vec::new();
        let mut sawcursor = false;

        while let Some(s) = lines.next() {
            if wrapped.len() >= height && sawcursor {
                break;
            }

            let mut off = 0;
            let slen = s.len();

            while off < slen && (wrapped.len() < height || !sawcursor) {
                let start = off;
                let end = (start + width).min(slen);
                let swrapped = s[start..end].to_string();

                let cursor_line = line == cursor.y && (start..=end).contains(&cursor.x);

                wrapped.push((line, start, end, swrapped, cursor_line));

                if cursor_line {
                    sawcursor = true;
                }

                off = end;
            }

            if slen == 0 {
                wrapped.push((line, 0, 0, s.to_string(), line == cursor.y));
            }

            line += 1;
        }

        if wrapped.len() > height {
            let n = wrapped.len() - height;
            let _ = wrapped.drain(..n);
            let (line, start, _, _, _) = wrapped.first().unwrap();
            self.viewctx.corner.set_y(*line);
            self.viewctx.corner.set_x(*start);
        }

        let mut x = 0;
        let mut y = context.top + off;

        let avail = height.saturating_sub(y as usize);
        let lines = wrapped.len();

        if avail < lines {
            let amt = lines.saturating_sub(avail) as u16;

            context.stdout.queue(ScrollUp(amt))?;
            context.top = context.top.saturating_sub(amt);
            y = y.saturating_sub(amt);
        }

        let bot = self.viewctx.dimensions.1 as u16;
        let mut term_cursor = (0, 0);

        context.stdout.queue(MoveTo(0, y))?;

        if let Some(ref prompt) = prompt {
            context.stdout.queue(Print(prompt))?;
            x = prompt.len() as u16;
        }

        for (line, start, end, s, cursor_line) in wrapped.into_iter() {
            if y >= bot {
                break;
            }

            if cursor_line {
                let coff = (cursor.x - start) as u16;
                term_cursor = (x + coff, y);
            }

            let ranges = self._highlight_ranges(line, start, end);

            // XXX: need to highlight followers, too.
            // let finfo = self.buffer._follower_intervals(self.gid);

            let mut prev = 0;

            context.stdout.queue(MoveTo(x, y))?;

            for range in ranges {
                let rs = prev.max(*range.start());
                let re = *range.end();

                context.stdout.queue(Print(&s[prev..rs]))?;

                let neg = s[(rs - start)..=(re - start)].negative();

                prev = re.saturating_add(1);

                context.stdout.queue(PrintStyledContent(neg))?;
            }

            context.stdout.queue(Print(&s[prev..]))?;

            y += 1;
        }

        context.stdout.queue(MoveTo(term_cursor.0 as u16, term_cursor.1 as u16))?;

        Ok(lines as u16)
    }

    pub fn get_trim(&self) -> EditRope {
        self.buffer.get().trim_end_matches(is_newline)
    }

    fn _redraw_nowrap(
        &mut self,
        _: &Option<String>,
        _: u16,
        _: &mut EditorContext,
    ) -> crossterm::Result<u16> {
        Ok(0)
    }

    pub fn set_text<T: Into<EditRope>>(&mut self, text: T) {
        self.buffer.set_text(text)
    }

    pub fn redraw(
        &mut self,
        prompt: &Option<String>,
        off: u16,
        context: &mut EditorContext,
    ) -> crossterm::Result<u16> {
        if self.viewctx.wrap {
            self._redraw_wrap(prompt, off, context)
        } else {
            self._redraw_nowrap(prompt, off, context)
        }
    }

    pub fn reset(&mut self) -> EditRope {
        self.scrollback = ScrollbackState::Pending;
        self.buffer.reset()
    }

    pub fn find(
        &mut self,
        history: &mut HistoryList<EditRope>,
        needle: &Regex,
        dir: MoveDir1D,
        inc: bool,
    ) -> Option<EditRope> {
        if self.scrollback == ScrollbackState::Pending {
            let rope = self.get_trim();

            if rope.len() > 0 {
                self.scrollback = ScrollbackState::Typed;

                history.append(rope);
            } else {
                self.scrollback = ScrollbackState::Empty;
            }
        }

        history.find(needle, dir, inc).map(Clone::clone)
    }

    pub fn recall(
        &mut self,
        history: &mut HistoryList<EditRope>,
        dir: MoveDir1D,
        count: usize,
    ) -> Option<EditRope> {
        history.recall(self.buffer.get(), &mut self.scrollback, dir, count)
    }

    pub fn line_leftover(&mut self, dir: MoveDir1D, count: usize) -> usize {
        self.buffer.line_leftover(dir, count, self.gid)
    }
}

impl<I> Deref for Editor<I>
where
    I: ApplicationInfo,
{
    type Target = EditBuffer<I>;

    fn deref(&self) -> &Self::Target {
        &self.buffer
    }
}

impl<I> DerefMut for Editor<I>
where
    I: ApplicationInfo,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buffer
    }
}

impl<C, I> Editable<C, I> for Editor<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn edit(
        &mut self,
        operation: &EditAction,
        motion: &EditTarget,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult {
        let ctx = (self.gid, &self.viewctx, ctx);

        self.buffer.edit(operation, motion, &ctx, store)
    }

    fn mark(&mut self, name: Mark, ctx: &C, store: &mut Store<I>) -> EditResult {
        let ctx = (self.gid, &self.viewctx, ctx);

        self.buffer.mark(name, &ctx, store)
    }

    fn insert_text(&mut self, act: InsertTextAction, ctx: &C, store: &mut Store<I>) -> EditResult {
        let ctx = (self.gid, &self.viewctx, ctx);

        self.buffer.insert_text(act, &ctx, store)
    }

    fn selection_command(
        &mut self,
        act: SelectionAction,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult {
        let ctx = (self.gid, &self.viewctx, ctx);

        self.buffer.selection_command(act, &ctx, store)
    }

    fn history_command(&mut self, act: HistoryAction, ctx: &C, store: &mut Store<I>) -> EditResult {
        let ctx = (self.gid, &self.viewctx, ctx);

        self.buffer.history_command(act, &ctx, store)
    }

    fn cursor_command(&mut self, act: &CursorAction, ctx: &C, store: &mut Store<I>) -> EditResult {
        let ctx = (self.gid, &self.viewctx, ctx);

        self.buffer.cursor_command(act, &ctx, store)
    }
}

impl<C, I> Jumpable<C> for Editor<I>
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
    ) -> UIResult<usize> {
        let ctx = (self.gid, &self.viewctx, ctx);

        self.buffer.jump(list, dir, count, &ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editing::application::EmptyInfo;

    fn mked() -> Editor<EmptyInfo> {
        Editor::new()
    }

    fn mkedstr(s: &str) -> Editor<EmptyInfo> {
        let mut ed = mked();
        ed.set_text(s);

        ed
    }

    #[test]
    fn test_line_leftover() {
        let mut ed = mked();

        ed.set_text("a\nb\nc\n");
        assert_eq!(ed.buffer.get_leader(ed.gid), Cursor::new(0, 0));
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 1), 1);
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 2), 2);
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 3), 3);
        assert_eq!(ed.line_leftover(MoveDir1D::Next, 1), 0);
        assert_eq!(ed.line_leftover(MoveDir1D::Next, 2), 0);
        assert_eq!(ed.line_leftover(MoveDir1D::Next, 3), 1);

        ed.buffer.set_leader(ed.gid, Cursor::new(1, 0));
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 1), 0);
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 2), 1);
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 3), 2);
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 1), 0);
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 2), 1);
        assert_eq!(ed.line_leftover(MoveDir1D::Previous, 3), 2);
    }

    #[test]
    fn test_find_empty() {
        let mut ed = mkedstr("\n");
        let mut history = HistoryList::new(EditRope::from("hello world"), 100);
        let needle = Regex::new("he").unwrap();

        history.push("foo".into());
        history.push("help me".into());
        history.push("bar".into());
        history.push("writhe".into());
        history.push("baz".into());

        let v = vec!["hello world", "foo", "help me", "bar", "writhe", "baz"];

        assert_eq!(ed.scrollback, ScrollbackState::Pending);
        assert_eq!(history.strs(), v);

        let res = ed.find(&mut history, &needle, MoveDir1D::Previous, false).unwrap();
        assert_eq!(res, EditRope::from("writhe"));
        assert_eq!(ed.scrollback, ScrollbackState::Empty);
        assert_eq!(history.strs(), v);

        let res = ed.find(&mut history, &needle, MoveDir1D::Previous, false).unwrap();
        assert_eq!(res, EditRope::from("help me"));
        assert_eq!(ed.scrollback, ScrollbackState::Empty);
        assert_eq!(history.strs(), v);

        let res = ed.find(&mut history, &needle, MoveDir1D::Next, false).unwrap();
        assert_eq!(res, EditRope::from("writhe"));
        assert_eq!(ed.scrollback, ScrollbackState::Empty);
        assert_eq!(history.strs(), v);
    }

    #[test]
    fn test_find_typed() {
        let mut ed = mkedstr("quux\n");
        let mut history = HistoryList::new(EditRope::from("hello world"), 100);
        let needle = Regex::new("he").unwrap();

        history.push("foo".into());
        history.push("help me".into());
        history.push("bar".into());
        history.push("writhe".into());
        history.push("baz".into());

        let v = vec!["hello world", "foo", "help me", "bar", "writhe", "baz"];

        assert_eq!(ed.scrollback, ScrollbackState::Pending);
        assert_eq!(history.strs(), v);

        let v = vec![
            "hello world",
            "foo",
            "help me",
            "bar",
            "writhe",
            "baz",
            "quux",
        ];

        let res = ed.find(&mut history, &needle, MoveDir1D::Previous, false).unwrap();
        assert_eq!(res, EditRope::from("writhe"));
        assert_eq!(ed.scrollback, ScrollbackState::Typed);
        assert_eq!(history.strs(), v);

        let res = ed.find(&mut history, &needle, MoveDir1D::Previous, false).unwrap();
        assert_eq!(res, EditRope::from("help me"));
        assert_eq!(ed.scrollback, ScrollbackState::Typed);
        assert_eq!(history.strs(), v);

        let res = ed.find(&mut history, &needle, MoveDir1D::Next, false).unwrap();
        assert_eq!(res, EditRope::from("writhe"));
        assert_eq!(ed.scrollback, ScrollbackState::Typed);
        assert_eq!(history.strs(), v);
    }

    #[test]
    fn test_recall_empty() {
        let mut ed = mkedstr("\n");
        let mut history = HistoryList::new(EditRope::from("hello world"), 100);

        history.push("foo".into());
        history.push("help me".into());
        history.push("bar".into());
        history.push("writhe".into());
        history.push("baz".into());

        let v = vec!["hello world", "foo", "help me", "bar", "writhe", "baz"];

        assert_eq!(ed.scrollback, ScrollbackState::Pending);
        assert_eq!(history.strs(), v);

        let res = ed.recall(&mut history, MoveDir1D::Previous, 3).unwrap();
        assert_eq!(res, EditRope::from("bar"));
        assert_eq!(ed.scrollback, ScrollbackState::Empty);
        assert_eq!(history.strs(), v);

        let res = ed.recall(&mut history, MoveDir1D::Next, 1).unwrap();
        assert_eq!(res, EditRope::from("writhe"));
        assert_eq!(ed.scrollback, ScrollbackState::Empty);
        assert_eq!(history.strs(), v);

        let res = ed.recall(&mut history, MoveDir1D::Next, 2).unwrap();
        assert_eq!(res, EditRope::from(""));
        assert_eq!(ed.scrollback, ScrollbackState::Pending);
        assert_eq!(history.strs(), v);
    }

    #[test]
    fn test_recall_typed() {
        let mut ed = mkedstr("quux\n");
        let mut history = HistoryList::new(EditRope::from("hello world"), 100);

        history.push("foo".into());
        history.push("help me".into());
        history.push("bar".into());
        history.push("writhe".into());
        history.push("baz".into());

        let v = vec!["hello world", "foo", "help me", "bar", "writhe", "baz"];

        assert_eq!(ed.scrollback, ScrollbackState::Pending);
        assert_eq!(history.strs(), v);

        let v = vec![
            "hello world",
            "foo",
            "help me",
            "bar",
            "writhe",
            "baz",
            "quux",
        ];

        let res = ed.recall(&mut history, MoveDir1D::Previous, 3).unwrap();
        assert_eq!(res, EditRope::from("bar"));
        assert_eq!(ed.scrollback, ScrollbackState::Typed);
        assert_eq!(history.strs(), v);

        let res = ed.recall(&mut history, MoveDir1D::Next, 1).unwrap();
        assert_eq!(res, EditRope::from("writhe"));
        assert_eq!(ed.scrollback, ScrollbackState::Typed);
        assert_eq!(history.strs(), v);

        let res = ed.recall(&mut history, MoveDir1D::Next, 2).unwrap();
        assert_eq!(res, EditRope::from("quux"));
        assert_eq!(ed.scrollback, ScrollbackState::Typed);
        assert_eq!(history.strs(), v);
    }
}
