use crate::editing::{
    action::{EditInfo, EditResult},
    application::ApplicationInfo,
    base::{Char, Count, CursorEnd, InsertStyle, MoveDir1D, PasteStyle, Register, TargetShape},
    buffer::{CursorGroupIdContext, EditBuffer},
    context::Resolve,
    cursor::{Adjustable, CursorChoice, CursorState},
    rope::EditRope,
    store::Store,
};

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
        let cell = store.registers.get(&ctx.2.get_register().unwrap_or(Register::Unnamed))?;
        let text = cell.value.repeat(cell.shape, count);
        let end = ctx.2.get_cursor_end();

        let gid = ctx.0;
        let mut group = self.get_group(gid);

        self.push_change(&group);

        for state in group.iter_mut() {
            let (choice, adjs) = match style {
                PasteStyle::Cursor => {
                    let cursor = state.cursor();
                    let dir = MoveDir1D::Previous;

                    if let Some(style) = insty {
                        self.text.insert(cursor, dir, text.clone(), style)
                    } else {
                        self.text.paste(cursor, dir, text.clone(), cell.shape)
                    }
                },
                PasteStyle::Side(dir) => {
                    let cursor = match dir {
                        MoveDir1D::Previous => state.start(),
                        MoveDir1D::Next => state.end(),
                    };

                    if let Some(style) = insty {
                        self.text.insert(cursor, *dir, text.clone(), style)
                    } else {
                        self.text.paste(cursor, *dir, text.clone(), cell.shape)
                    }
                },
                PasteStyle::Replace => {
                    let start = self.text.cursor_to_offset(state.start());
                    let end = self.text.cursor_to_offset(state.end());

                    self.text.replace(start, end, true, text.clone())
                },
            };

            self._adjust_all(adjs, store);

            if let Some(cursor) = choice.resolve(end) {
                state.set(cursor);
                self.clamp_state(state, ctx);
            }
        }

        self.set_group(gid, group);

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
        let end = ctx.2.get_cursor_end();

        let gid = ctx.0;
        let mut group = self.get_group(gid);

        self.push_change(&group);

        for state in group.iter_mut() {
            let (choice, adjs) = self.text.paste(state.cursor(), dir, text.clone(), shape);

            self._adjust_all(adjs, store);

            if let Some(cursor) = choice.resolve(end) {
                state.set(cursor);
                self.clamp_state(state, ctx);
            }
        }

        self.set_group(gid, group);

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
        let end = ctx.2.get_cursor_end();

        let gid = ctx.0;
        let mut group = self.get_group(gid);
        let mut adjs = vec![];

        let text = EditRope::from(s).repeat(TargetShape::CharWise, count);

        self.push_change(&group);

        for state in group.iter_mut() {
            state.adjust(adjs.as_slice());

            let (choice, mut adj) = self.text.insert(state.cursor(), dir, text.clone(), style);

            if let Some(cursor) = choice.resolve(end) {
                state.set(cursor);
                self.clamp_state(state, ctx);
            } else {
                state.adjust(adj.as_slice());
            }

            adjs.append(&mut adj);
        }

        self._adjust_all(adjs, store);
        self.set_group(gid, group);

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
        let end = ctx.2.get_cursor_end();

        let gid = ctx.0;
        let mut group = self.get_group(gid);

        self.push_change(&group);

        let mut typed: Vec<&mut CursorState> = vec![];
        let mut adjs = vec![];

        for state in group.iter_mut().rev() {
            let mut choice = CursorChoice::Single(state.cursor().clone());

            for _ in 0..count {
                if let Some(cursor) = choice.get(CursorEnd::Auto) {
                    let s = self._str(ch.clone(), cursor, &store.digraphs)?;
                    let text = EditRope::from(s.as_str());

                    let mut res = self.text.insert(cursor, dir, text, style);
                    choice = res.0;

                    for typed in typed.iter_mut() {
                        typed.adjust(res.1.as_slice());
                    }

                    adjs.append(&mut res.1);
                }
            }

            if let Some(cursor) = choice.resolve(end) {
                state.set(cursor);
            }

            typed.push(state);
        }

        self._adjust_all(adjs, store);
        self.set_group(gid, group);

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

        vctx.insert_style = Some(InsertStyle::Replace);
        vctx.last_column = true;

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

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\n");
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
        vctx.insert_style = Some(InsertStyle::Insert);
        vctx.last_column = true;
        open_line!(ebuf, TargetShape::CharWise, MoveDir1D::Previous, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello \nworld\nhello \nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 6));

        // Move to (1, 2).
        ebuf.set_leader(gid, Cursor::new(1, 2));
        vctx.insert_style = None;
        vctx.last_column = true;

        // Insert newline above this line.
        open_line!(ebuf, TargetShape::LineWise, MoveDir1D::Previous, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello \n\nworld\nhello \nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));
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
        vctx.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));

        // place "b (" world") into the buffer
        vctx.register = Some(Register::Named('b'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 10));

        // place "c ("foo bar\n") on the line below
        vctx.register = Some(Register::Named('c'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nfoo bar\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        // place "d ("three\nregister\nlines\n") on the line above
        vctx.register = Some(Register::Named('d'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        // place "c ("foo bar\n") on the line below, breaking up the "d text.
        vctx.register = Some(Register::Named('c'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\nfoo bar\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 0));

        // place "e ("abcde\n12345") twice before the next several lines.
        vctx.register = Some(Register::Named('e'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(gid, vwctx, vctx), store);
        assert_eq!(
            ebuf.get_text(),
            "hello world\nthree\nabcdeabcdefoo bar\n1234512345register\nlines\nfoo bar\n"
        );
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 0));

        // place "f ("1\n2\n3\n4\n5\n6\n7") on the next several lines, adding new lines as needed.
        vctx.register = Some(Register::Named('f'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar\n12234512345register\nl3ines\nf4oo bar\n5\n6\n7\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 1));

        // Move to the end of the line, and repeat pasting "f.
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Motion, mv!(mov, 0), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 17).goal(usize::MAX));

        vctx.register = Some(Register::Named('f'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar1\n12234512345registe2r\nl3ines3\nf4oo bar4\n55\n66\n77\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(2, 18));
    }

    #[test]
    fn test_paste_empty_charwise_next() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::CharWise, "hello");

        vctx.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 4));
    }

    #[test]
    fn test_paste_empty_linewise_next() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::LineWise, "hello\n");

        vctx.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "\nhello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));
    }

    #[test]
    fn test_paste_empty_blockwise_next() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfive();

        set_named_reg!(store, 'a', TargetShape::BlockWise, "hello\nworld");

        vctx.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));
    }

    #[test]
    fn test_paste_insert() {
        let (mut ebuf, gid, vwctx, mut vctx, mut store) = mkfivestr("hello world\n");

        set_named_reg!(store, 'a', TargetShape::LineWise, "foo\n");
        set_named_reg!(store, 'b', TargetShape::BlockWise, "a\nb\nc");

        vctx.insert_style = Some(InsertStyle::Insert);
        vctx.last_column = true;

        // Start out at (0, 6).
        ebuf.set_leader(gid, Cursor::new(0, 6));

        // place "a ("foo\n") into the buffer as if it were CharWise.
        vctx.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hello foo\nworld\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(1, 0));

        // place "b ("a\nb\nc") into the buffer as if it were CharWise.
        vctx.register = Some(Register::Named('b'));
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
        vctx.register = Some(Register::Named('a'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Exact(5), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "hellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 24));

        // Paste "1 2 3\n" from "b 2 times.
        vctx.register = Some(Register::Named('b'));
        paste_dir!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "1 2 3\n1 2 3\nhellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 0));

        // Paste "a\nb\nc" from "c 4 times.
        vctx.register = Some(Register::Named('c'));
        paste_dir!(ebuf, MoveDir1D::Next, Count::Exact(4), ctx!(gid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "1aaaa 2 3\n1bbbb 2 3\nhccccellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(gid), Cursor::new(0, 1));
    }
}
