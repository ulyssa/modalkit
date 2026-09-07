use crate::editing::{
    application::ApplicationInfo,
    buffer::{CursorGroupIdContext, EditBuffer},
    context::Resolve,
    cursor::{Adjustable, CursorAdjustment, CursorChoice, CursorState},
    rope::EditRope,
    store::Store,
};
use crate::errors::EditResult;
use crate::prelude::*;

pub trait InsertTextActions<C, I>
where
    I: ApplicationInfo,
{
    /// Open a new blank line before or after the cursor.
    fn open_line(
        &mut self,
        shape: TargetShape,
        dir: MoveDir1D,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Paste text into the buffer.
    fn paste(
        &mut self,
        style: &PasteStyle,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Enter text at the cursor position.
    fn transcribe(
        &mut self,
        s: &str,
        dir: MoveDir1D,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;

    /// Enter a new character at the cursor position.
    fn type_char(
        &mut self,
        ch: Char,
        dir: MoveDir1D,
        count: &Count,
        ctx: &C,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I>;
}

impl<I> EditBuffer<I>
where
    I: ApplicationInfo,
{
    fn cursor_insert<'a, F>(
        &mut self,
        clamp: bool,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
        mut callback: F,
    ) -> EditResult<(), I>
    where
        F: FnMut(
            &mut CursorState,
            &mut EditBuffer<I>,
            &mut Store<I>,
        ) -> EditResult<(CursorChoice, Vec<CursorAdjustment>), I>,
    {
        let gid = ctx.0;
        let mut group = self.get_group(gid);
        let change_start = group.clone();
        let end = ctx.2.get_cursor_end();

        let mut typed: Vec<&mut CursorState> = vec![];
        let mut adjs = vec![];

        for state in group.iter_mut().rev() {
            let (choice, mut adj) = callback(state, self, store)?;

            typed.adjust(adj.as_slice());
            adjs.append(&mut adj);

            if let Some(cursor) = choice.resolve(end) {
                state.set(cursor);
                if clamp {
                    self.clamp_state(state, ctx);
                }
            }

            typed.push(state);
        }

        self._adjust_all(adjs, store);
        self.push_change(change_start);
        self.set_group(gid, group);

        Ok(())
    }
}

impl<'a, I> InsertTextActions<CursorGroupIdContext<'a>, I> for EditBuffer<I>
where
    I: ApplicationInfo,
{
    fn paste(
        &mut self,
        style: &PasteStyle,
        count: &Count,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let count = ctx.2.resolve(count);
        let insty = ctx.2.get_insert_style();
        let register = ctx
            .2
            .get_register()
            .unwrap_or_else(|| store.registers.get_default_register());
        let cell = store.registers.get(&register)?;
        let text = cell.value.repeat(cell.shape, count);

        self.cursor_insert(true, ctx, store, |state, buf, _| {
            let (choice, adj) = match style {
                PasteStyle::Cursor => {
                    let cursor = state.cursor();
                    let dir = MoveDir1D::Previous;

                    if let Some(style) = insty {
                        buf.text.insert(cursor, dir, text.clone(), style)
                    } else {
                        buf.text.paste(cursor, dir, text.clone(), cell.shape)
                    }
                },
                PasteStyle::Side(dir) => {
                    let cursor = match dir {
                        MoveDir1D::Previous => state.start(),
                        MoveDir1D::Next => state.end(),
                    };

                    if let Some(style) = insty {
                        buf.text.insert(cursor, *dir, text.clone(), style)
                    } else {
                        buf.text.paste(cursor, *dir, text.clone(), cell.shape)
                    }
                },
                PasteStyle::Replace => {
                    let start = buf.text.cursor_to_offset(state.start());
                    let end = buf.text.cursor_to_offset(state.end());

                    buf.text.replace(start..=end, text.clone())
                },
            };

            Ok((choice, adj))
        })?;

        Ok(None)
    }

    fn open_line(
        &mut self,
        shape: TargetShape,
        dir: MoveDir1D,
        count: &Count,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let count = ctx.2.resolve(count);
        let text = EditRope::from("\n").repeat(TargetShape::CharWise, count);

        self.cursor_insert(true, ctx, store, |state, buf, _| {
            Ok(buf.text.paste(state.cursor(), dir, text.clone(), shape))
        })?;

        Ok(None)
    }

    fn transcribe(
        &mut self,
        s: &str,
        dir: MoveDir1D,
        count: &Count,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let style = ctx.2.get_insert_style().unwrap_or(InsertStyle::Insert);
        let count = ctx.2.resolve(count);
        let text = EditRope::from(s).repeat(TargetShape::CharWise, count);

        self.cursor_insert(true, ctx, store, |state, buf, _| {
            Ok(buf.text.insert(state.cursor(), dir, text.clone(), style))
        })?;

        Ok(None)
    }

    fn type_char(
        &mut self,
        ch: Char,
        dir: MoveDir1D,
        count: &Count,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let style = ctx.2.get_insert_style().unwrap_or(InsertStyle::Insert);
        let count = ctx.2.resolve(count);

        self.cursor_insert(false, ctx, store, |state, buf, store| {
            let mut choice = CursorChoice::Single(state.cursor().clone());
            let mut adjs = vec![];

            for _ in 0..count {
                if let Some(cursor) = choice.get(CursorEnd::Auto) {
                    let s = buf._str(ch.clone(), cursor, &store.digraphs)?;
                    let text = EditRope::from(s.as_str());

                    let mut res = buf.text.insert(cursor, dir, text, style);
                    choice = res.0;
                    adjs.append(&mut res.1);
                }
            }

            Ok((choice, adjs))
        })?;

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    macro_rules! type_digraph {
        ($ebuf: expr, $d1: expr, $d2: expr, $ctx: expr, $store: expr) => {
            $ebuf
                .type_char(
                    Char::Digraph($d1, $d2).into(),
                    MoveDir1D::Previous,
                    &1.into(),
                    $ctx,
                    &mut $store,
                )
                .unwrap()
        };
    }

    macro_rules! type_copy_line {
        ($ebuf: expr, $dir: expr, $c: expr, $ctx: expr, $store: expr) => {
            $ebuf
                .type_char($dir.clone(), MoveDir1D::Previous, &$c, $ctx, &mut $store)
                .unwrap()
        };
    }

    macro_rules! open_line {
        ($ebuf: expr, $shape: expr, $dir: expr, $ctx: expr, $store: expr) => {
            $ebuf
                .open_line($shape, $dir, &Count::Contextual, $ctx, &mut $store)
                .unwrap()
        };
    }

    #[test]
    fn test_typing_insert_char() {
        let (mut ebuf, gid, vwctx, vctx, mut store) = mkfive();

        type_char!(ebuf, 'h', gid, vwctx, vctx, store);
        type_char!(ebuf, 'e', gid, vwctx, vctx, store);
        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        type_char!(ebuf, 'o', gid, vwctx, vctx, store);

        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 5));

        type_char!(ebuf, ' ', gid, vwctx, vctx, store);
        type_char!(ebuf, 'w', gid, vwctx, vctx, store);
        type_char!(ebuf, 'o', gid, vwctx, vctx, store);
        type_char!(ebuf, 'r', gid, vwctx, vctx, store);
        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        type_char!(ebuf, 'd', gid, vwctx, vctx, store);

        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 11));

        type_char!(ebuf, '\n', gid, vwctx, vctx, store);
        type_char!(ebuf, '1', gid, vwctx, vctx, store);

        assert_eq!(ebuf.get_text(), "hello world\n1\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 1));
    }

    #[test]
    fn test_typing_insert_digraph() {
        let (mut ebuf, gid, vwctx, vctx, mut store) = mkfive();

        type_digraph!(ebuf, '>', '>', ctx!(gid, vwctx, vctx), store);

        assert_eq!(ebuf.get_text(), "\u{00BB}\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));

        type_digraph!(ebuf, '<', '<', ctx!(gid, vwctx, vctx), store);

        assert_eq!(ebuf.get_text(), "\u{00BB}\u{00AB}\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 2));
    }

    #[test]
    fn test_typing_insert_copy_line() {
        let (mut ebuf, gid, vwctx, vctx, mut store) = mkfivestr("abcde\n_\n123456\n");

        let above = Char::CopyLine(MoveDir1D::Previous);
        let below = Char::CopyLine(MoveDir1D::Next);

        // Set cursor to (1, 1).
        ebuf.set_leader(gid, Cursor::new(1, 1));

        // Copy character above cursor ("^Y").
        type_copy_line!(ebuf, above, 1.into(), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 2));
        assert_eq!(ebuf.get_text(), "abcde\n_b\n123456\n");

        // Copy character below cursor ("^E").
        type_copy_line!(ebuf, below, 1.into(), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 3));
        assert_eq!(ebuf.get_text(), "abcde\n_b3\n123456\n");

        // Copy two characters above cursor.
        type_copy_line!(ebuf, above, 2.into(), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 5));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de\n123456\n");

        // There are no more characters above the cursor to copy ("^Y").
        let res = ebuf.type_char(
            above.clone(),
            MoveDir1D::Previous,
            &1.into(),
            ctx!(gid, vwctx, vctx),
            &mut store,
        );
        assert!(res.is_err());
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 5));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de\n123456\n");

        // There is still a character below though ("^E").
        type_copy_line!(ebuf, below, 1.into(), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 6));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de6\n123456\n");

        // And now there's nothing below to copy ("^E").
        let res = ebuf.type_char(
            below.clone(),
            MoveDir1D::Previous,
            &1.into(),
            ctx!(gid, vwctx, vctx),
            &mut store,
        );
        assert!(res.is_err());
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 6));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de6\n123456\n");
    }

    #[test]
    fn test_typing_replace() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr("hello");

        vctx.persist.insert = Some(InsertStyle::Replace);

        type_char!(ebuf, 'c', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "cello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));

        type_char!(ebuf, 'a', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "callo\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 2));

        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "callo\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 3));

        type_char!(ebuf, 'y', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyo\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));

        type_char!(ebuf, 'x', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyx\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 5));

        type_char!(ebuf, 'e', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxe\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 6));

        type_char!(ebuf, 's', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 7));

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));
    }

    #[test]
    fn test_typing_replace_multiline() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr("hello\nworld");

        vctx.persist.insert = Some(InsertStyle::Replace);

        type_char!(ebuf, 'c', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "cello\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));

        type_char!(ebuf, 'a', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "callo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 2));

        type_char!(ebuf, 'l', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "callo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 3));

        type_char!(ebuf, 'y', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));

        type_char!(ebuf, 'x', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyx\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 5));

        type_char!(ebuf, 'e', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxe\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 6));

        type_char!(ebuf, 's', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 7));

        type_char!(ebuf, '\n', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\n\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        type_char!(ebuf, 'a', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\na\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 1));

        type_char!(ebuf, 'b', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nab\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 2));

        type_char!(ebuf, 'c', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabc\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 3));

        type_char!(ebuf, 'd', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabcd\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 4));

        type_char!(ebuf, 'e', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabcde\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 5));

        type_char!(ebuf, 'f', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabcdef\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 6));

        type_char!(ebuf, 'g', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabcdefg\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 7));

        type_char!(ebuf, 'h', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabcdefgh\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 8));

        type_char!(ebuf, 'i', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabcdefghi\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 9));

        type_char!(ebuf, 'j', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "calyxes\nabcdefghij\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 10));

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "calyxes\n\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        let mov = MoveType::Column(MoveDir1D::Previous, true);

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "calyxes\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 7));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "calyxe\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 6));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "calyx\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 5));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "calyo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "callo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 3));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "callo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 2));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "cello\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));
    }

    #[test]
    fn test_open_line() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr("hello world\nhello world\n");

        // Start out at (0, 6).
        ebuf.set_leader(gid, Cursor::new(0, 6));

        // Insert newline before cursor.
        open_line!(ebuf, TargetShape::CharWise, MoveDir1D::Previous, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello \nworld\nhello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 5));

        // Move to (2, 6).
        ebuf.set_leader(gid, Cursor::new(2, 6));

        // If there's an InsertStyle, cursor is left on the newline.
        vctx.persist.insert = Some(InsertStyle::Insert);
        open_line!(ebuf, TargetShape::CharWise, MoveDir1D::Previous, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello \nworld\nhello \nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 6));

        // Move to (1, 2).
        ebuf.set_leader(gid, Cursor::new(1, 2));
        vctx.persist.insert = None;

        // Insert newline above this line.
        open_line!(ebuf, TargetShape::LineWise, MoveDir1D::Previous, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello \n\nworld\nhello \nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));
    }

    #[test]
    fn test_open_line_cursor_group() {
        let (mut ebuf, gid, vwctx, vctx, mut store) = mkfivestr("hello world\nhello world\n");

        // Start out at (0, 6).
        ebuf.set_leader(gid, Cursor::new(0, 6));

        // First split the cursor into five cursors.
        ebuf.cursor_split(&Count::Exact(4), ctx!(gid, vwctx, vctx), &mut store)
            .unwrap();

        // Insert multiple newlines below current line:
        open_line!(ebuf, TargetShape::LineWise, MoveDir1D::Next, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\n\n\n\n\n\nhello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));
        assert_eq!(ebuf.get_followers(gid), vec![
            Cursor::new(2, 0),
            Cursor::new(3, 0),
            Cursor::new(4, 0),
            Cursor::new(5, 0),
        ]);

        // Text should be inserted on each of the new lines:
        type_char!(ebuf, 'q', gid, vwctx, vctx, store);
        type_char!(ebuf, '1', gid, vwctx, vctx, store);
        assert_eq!(ebuf.get_text(), "hello world\nq1\nq1\nq1\nq1\nq1\nhello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 2));
        assert_eq!(ebuf.get_followers(gid), vec![
            Cursor::new(2, 2),
            Cursor::new(3, 2),
            Cursor::new(4, 2),
            Cursor::new(5, 2),
        ]);
    }

    #[test]
    fn test_paste() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::CharWise, "hello");
        set_named_reg!(store, 'b', TargetShape::CharWise, " world");
        set_named_reg!(store, 'c', TargetShape::LineWise, "foo bar\n");
        set_named_reg!(store, 'd', TargetShape::LineWise, "three\nregister\nlines\n");
        set_named_reg!(store, 'e', TargetShape::BlockWise, "abcde\n12345");
        set_named_reg!(store, 'f', TargetShape::BlockWise, "1\n2\n3\n4\n5\n6\n7");

        // Start with an empty buffer
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        // place "a ("hello") into the buffer
        vctx.action.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));

        // place "b (" world") into the buffer
        vctx.action.register = Some(Register::Named('b'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 10));

        // place "c ("foo bar\n") on the line below
        vctx.action.register = Some(Register::Named('c'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nfoo bar\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        // place "d ("three\nregister\nlines\n") on the line above
        vctx.action.register = Some(Register::Named('d'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        // place "c ("foo bar\n") on the line below, breaking up the "d text.
        vctx.action.register = Some(Register::Named('c'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\nfoo bar\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 0));

        // place "e ("abcde\n12345") twice before the next several lines.
        vctx.action.register = Some(Register::Named('e'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(gid, vwctx, vctx), store);
        assert_eq!(
            ebuf.get_text(),
            "hello world\nthree\nabcdeabcdefoo bar\n1234512345register\nlines\nfoo bar\n"
        );
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 0));

        // place "f ("1\n2\n3\n4\n5\n6\n7") on the next several lines, adding new lines as needed.
        vctx.action.register = Some(Register::Named('f'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar\n12234512345register\nl3ines\nf4oo bar\n5\n6\n7\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 1));

        // Move to the end of the line, and repeat pasting "f.
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Motion, mv!(mov, 0), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 17).goal(usize::MAX));

        vctx.action.register = Some(Register::Named('f'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar1\n12234512345registe2r\nl3ines3\nf4oo bar4\n55\n66\n77\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 18));
    }

    #[test]
    fn test_paste_empty_charwise_next() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::CharWise, "hello");

        vctx.action.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
    }

    #[test]
    fn test_paste_empty_linewise_next() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::LineWise, "hello\n");

        vctx.action.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "\nhello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));
    }

    #[test]
    fn test_paste_empty_blockwise_next() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::BlockWise, "hello\nworld");

        vctx.action.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));
    }

    #[test]
    fn test_paste_insert() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr("hello world\n");

        set_named_reg!(store, 'a', TargetShape::LineWise, "foo\n");
        set_named_reg!(store, 'b', TargetShape::BlockWise, "a\nb\nc");

        vctx.persist.insert = Some(InsertStyle::Insert);

        // Start out at (0, 6).
        ebuf.set_leader(gid, Cursor::new(0, 6));

        // place "a ("foo\n") into the buffer as if it were CharWise.
        vctx.action.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello foo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        // place "b ("a\nb\nc") into the buffer as if it were CharWise.
        vctx.action.register = Some(Register::Named('b'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello foo\na\nb\ncworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(3, 1));
    }

    #[test]
    fn test_paste_repeat() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::CharWise, "hello");
        set_named_reg!(store, 'b', TargetShape::LineWise, "1 2 3\n");
        set_named_reg!(store, 'c', TargetShape::BlockWise, "a\nb\nc");

        // Paste "hello" from "a 5 times.
        vctx.action.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Exact(5), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 24));

        // Paste "1 2 3\n" from "b 2 times.
        vctx.action.register = Some(Register::Named('b'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "1 2 3\n1 2 3\nhellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        // Paste "a\nb\nc" from "c 4 times.
        vctx.action.register = Some(Register::Named('c'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Exact(4), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "1aaaa 2 3\n1bbbb 2 3\nhccccellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));
    }
}
