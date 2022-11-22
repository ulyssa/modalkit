use crate::editing::{
    action::EditResult,
    base::{
        Application,
        Char,
        Count,
        CursorEnd,
        EditContext,
        InsertStyle,
        MoveDir1D,
        Register,
        TargetShape,
    },
    buffer::{CursorGroupIdContext, EditBuffer},
    cursor::{Adjustable, CursorChoice, CursorState},
    rope::EditRope,
};

pub trait InsertTextActions<C> {
    /// Open a new blank line before or after the cursor.
    fn open_line(
        &mut self,
        shape: TargetShape,
        dir: MoveDir1D,
        count: Count,
        ctx: &C,
    ) -> EditResult;

    /// Paste text into the buffer.
    fn paste(&mut self, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult;

    /// Enter text at the cursor position.
    fn transcribe(&mut self, s: String, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult;

    /// Enter a new character at the cursor position.
    fn type_char(&mut self, ch: Char, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult;
}

impl<'a, 'b, C, P> InsertTextActions<CursorGroupIdContext<'a, 'b, C>> for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn paste(
        &mut self,
        dir: MoveDir1D,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let count = ctx.2.resolve(&count);
        let style = ctx.2.get_insert_style();
        let cell = self.get_register(&ctx.2.get_register().unwrap_or(Register::Unnamed));
        let text = cell.value.repeat(cell.shape, count);
        let end = ctx.2.get_cursor_end();

        let gid = ctx.0;
        let mut group = self.get_group(gid);

        self.push_change(&group);

        for state in group.iter_mut() {
            let (choice, adjs) = if let Some(style) = style {
                self.text.insert(state.cursor(), dir, text.clone(), style)
            } else {
                self.text.paste(state.cursor(), dir, text.clone(), cell.shape)
            };

            self._adjust_all(adjs);

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
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let count = ctx.2.resolve(&count);
        let text = EditRope::from("\n").repeat(TargetShape::CharWise, count);
        let end = ctx.2.get_cursor_end();

        let gid = ctx.0;
        let mut group = self.get_group(gid);

        self.push_change(&group);

        for state in group.iter_mut() {
            let (choice, adjs) = self.text.paste(state.cursor(), dir, text.clone(), shape);

            self._adjust_all(adjs);

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
        s: String,
        dir: MoveDir1D,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let style = ctx.2.get_insert_style().unwrap_or(InsertStyle::Insert);
        let count = ctx.2.resolve(&count);
        let end = ctx.2.get_cursor_end();

        let gid = ctx.0;
        let mut group = self.get_group(gid);
        let mut adjs = vec![];

        let text = EditRope::from(s.as_str()).repeat(TargetShape::CharWise, count);

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

        self._adjust_all(adjs);
        self.set_group(gid, group);

        Ok(None)
    }

    fn type_char(
        &mut self,
        ch: Char,
        dir: MoveDir1D,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let style = ctx.2.get_insert_style().unwrap_or(InsertStyle::Insert);
        let count = ctx.2.resolve(&count);
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
                    let s = self._str(ch.clone(), cursor)?;
                    let text = EditRope::from(s.as_str());

                    let mut res = self.text.insert(&cursor, dir, text, style);
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

        self._adjust_all(adjs);
        self.set_group(gid, group);

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    macro_rules! type_digraph {
        ($ebuf: expr, $d1: expr, $d2: expr, $ctx: expr) => {
            $ebuf
                .type_char(Char::Digraph($d1, $d2).into(), MoveDir1D::Previous, 1.into(), $ctx)
                .unwrap()
        };
    }

    macro_rules! type_copy_line {
        ($ebuf: expr, $dir: expr, $c: expr, $ctx: expr) => {
            $ebuf.type_char($dir.clone(), MoveDir1D::Previous, $c, $ctx).unwrap()
        };
    }

    macro_rules! open_line {
        ($ebuf: expr, $shape: expr, $dir: expr, $ctx: expr) => {
            $ebuf.open_line($shape, $dir, Count::Contextual, $ctx).unwrap()
        };
    }

    #[test]
    fn test_typing_insert_char() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        type_char!(ebuf, 'h', curid, vwctx, vctx);
        type_char!(ebuf, 'e', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        type_char!(ebuf, ' ', curid, vwctx, vctx);
        type_char!(ebuf, 'w', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);
        type_char!(ebuf, 'r', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'd', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));

        type_char!(ebuf, '\n', curid, vwctx, vctx);
        type_char!(ebuf, '1', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello world\n1\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 1));
    }

    #[test]
    fn test_typing_insert_digraph() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        type_digraph!(ebuf, '>', '>', ctx!(curid, vwctx, vctx));

        assert_eq!(ebuf.get_text(), "\u{00BB}\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 1));

        type_digraph!(ebuf, '<', '<', ctx!(curid, vwctx, vctx));

        assert_eq!(ebuf.get_text(), "\u{00BB}\u{00AB}\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
    }

    #[test]
    fn test_typing_insert_copy_line() {
        let mut ebuf = mkbufstr("abcde\n_\n123456\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        let above = Char::CopyLine(MoveDir1D::Previous);
        let below = Char::CopyLine(MoveDir1D::Next);

        // Set cursor to (1, 1).
        ebuf.set_leader(curid, Cursor::new(1, 1));

        // Copy character above cursor ("^Y").
        type_copy_line!(ebuf, above, 1.into(), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 2));
        assert_eq!(ebuf.get_text(), "abcde\n_b\n123456\n");

        // Copy character below cursor ("^E").
        type_copy_line!(ebuf, below, 1.into(), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 3));
        assert_eq!(ebuf.get_text(), "abcde\n_b3\n123456\n");

        // Copy two characters above cursor.
        type_copy_line!(ebuf, above, 2.into(), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 5));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de\n123456\n");

        // There are no more characters above the cursor to copy ("^Y").
        let res =
            ebuf.type_char(above.clone(), MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx));
        assert!(res.is_err());
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 5));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de\n123456\n");

        // There is still a character below though ("^E").
        type_copy_line!(ebuf, below, 1.into(), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de6\n123456\n");

        // And now there's nothing below to copy ("^E").
        let res =
            ebuf.type_char(below.clone(), MoveDir1D::Previous, 1.into(), ctx!(curid, vwctx, vctx));
        assert!(res.is_err());
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));
        assert_eq!(ebuf.get_text(), "abcde\n_b3de6\n123456\n");
    }

    #[test]
    fn test_typing_replace() {
        let mut ebuf = mkbufstr("hello");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        vctx.persist.insert = Some(InsertStyle::Replace);

        type_char!(ebuf, 'c', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "cello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 1));

        type_char!(ebuf, 'a', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "callo\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));

        type_char!(ebuf, 'l', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "callo\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        type_char!(ebuf, 'y', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "calyo\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        type_char!(ebuf, 'x', curid, vwctx, vctx);
        assert_eq!(ebuf.get_text(), "calyx\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);

        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
    }

    #[test]
    fn test_open_line() {
        let mut ebuf = mkbufstr("hello world\nhello world\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        // Start out at (0, 6).
        ebuf.set_leader(curid, Cursor::new(0, 6));

        // Insert newline before cursor.
        open_line!(ebuf, TargetShape::CharWise, MoveDir1D::Previous, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello \nworld\nhello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        // Move to (2, 6).
        ebuf.set_leader(curid, Cursor::new(2, 6));

        // If there's an InsertStyle, cursor is left on the newline.
        vctx.persist.insert = Some(InsertStyle::Insert);
        open_line!(ebuf, TargetShape::CharWise, MoveDir1D::Previous, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello \nworld\nhello \nworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 6));

        // Move to (1, 2).
        ebuf.set_leader(curid, Cursor::new(1, 2));
        vctx.persist.insert = None;

        // Insert newline above this line.
        open_line!(ebuf, TargetShape::LineWise, MoveDir1D::Previous, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello \n\nworld\nhello \nworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_paste() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::CharWise, "hello");
        set_named_reg!(ebuf, 'b', TargetShape::CharWise, " world");
        set_named_reg!(ebuf, 'c', TargetShape::LineWise, "foo bar\n");
        set_named_reg!(ebuf, 'd', TargetShape::LineWise, "three\nregister\nlines\n");
        set_named_reg!(ebuf, 'e', TargetShape::BlockWise, "abcde\n12345");
        set_named_reg!(ebuf, 'f', TargetShape::BlockWise, "1\n2\n3\n4\n5\n6\n7");

        // Start with an empty buffer
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // place "a ("hello") into the buffer
        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // place "b (" world") into the buffer
        vctx.action.register = Some(Register::Named('b'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));

        // place "c ("foo bar\n") on the line below
        vctx.action.register = Some(Register::Named('c'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nfoo bar\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // place "d ("three\nregister\nlines\n") on the line above
        vctx.action.register = Some(Register::Named('d'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // place "c ("foo bar\n") on the line below, breaking up the "d text.
        vctx.action.register = Some(Register::Named('c'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\nfoo bar\nregister\nlines\nfoo bar\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));

        // place "e ("abcde\n12345") twice before the next several lines.
        vctx.action.register = Some(Register::Named('e'));
        paste!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(curid, vwctx, vctx));
        assert_eq!(
            ebuf.get_text(),
            "hello world\nthree\nabcdeabcdefoo bar\n1234512345register\nlines\nfoo bar\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));

        // place "f ("1\n2\n3\n4\n5\n6\n7") on the next several lines, adding new lines as needed.
        vctx.action.register = Some(Register::Named('f'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar\n12234512345register\nl3ines\nf4oo bar\n5\n6\n7\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 1));

        // Move to the end of the line, and repeat pasting "f.
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Motion, mv!(mov, 0), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 17).goal(usize::MAX));

        vctx.action.register = Some(Register::Named('f'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello world\nthree\na1bcdeabcdefoo bar1\n12234512345registe2r\nl3ines3\nf4oo bar4\n55\n66\n77\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 18));
    }

    #[test]
    fn test_paste_empty_charwise_next() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::CharWise, "hello");

        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
    }

    #[test]
    fn test_paste_empty_linewise_next() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::LineWise, "hello\n");

        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "\nhello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }

    #[test]
    fn test_paste_empty_blockwise_next() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::BlockWise, "hello\nworld");

        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello\nworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
    }

    #[test]
    fn test_paste_insert() {
        let mut ebuf = mkbufstr("hello world\n");
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::LineWise, "foo\n");
        set_named_reg!(ebuf, 'b', TargetShape::BlockWise, "a\nb\nc");

        vctx.persist.insert = Some(InsertStyle::Insert);

        // Start out at (0, 6).
        ebuf.set_leader(curid, Cursor::new(0, 6));

        // place "a ("foo\n") into the buffer as if it were CharWise.
        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello foo\nworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));

        // place "b ("a\nb\nc") into the buffer as if it were CharWise.
        vctx.action.register = Some(Register::Named('b'));
        paste!(ebuf, MoveDir1D::Previous, Count::Contextual, ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hello foo\na\nb\ncworld\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 1));
    }

    #[test]
    fn test_paste_repeat() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let mut vctx = VimContext::default();

        set_named_reg!(ebuf, 'a', TargetShape::CharWise, "hello");
        set_named_reg!(ebuf, 'b', TargetShape::LineWise, "1 2 3\n");
        set_named_reg!(ebuf, 'c', TargetShape::BlockWise, "a\nb\nc");

        // Paste "hello" from "a 5 times.
        vctx.action.register = Some(Register::Named('a'));
        paste!(ebuf, MoveDir1D::Next, Count::Exact(5), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "hellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 24));

        // Paste "1 2 3\n" from "b 2 times.
        vctx.action.register = Some(Register::Named('b'));
        paste!(ebuf, MoveDir1D::Previous, Count::Exact(2), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "1 2 3\n1 2 3\nhellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Paste "a\nb\nc" from "c 4 times.
        vctx.action.register = Some(Register::Named('c'));
        paste!(ebuf, MoveDir1D::Next, Count::Exact(4), ctx!(curid, vwctx, vctx));
        assert_eq!(ebuf.get_text(), "1aaaa 2 3\n1bbbb 2 3\nhccccellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 1));
    }
}
