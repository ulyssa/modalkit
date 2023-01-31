use std::borrow::Cow;

use crate::editing::{
    application::ApplicationInfo,
    base::{
        Case,
        CursorEnd,
        CursorMovements,
        CursorMovementsContext,
        IndentChange,
        InsertStyle,
        JoinStyle,
        NumberChange,
        Radix,
        Register,
        TargetShape,
        WordStyle,
    },
    context::EditContext,
    cursor::{Adjustable, Cursor, CursorChoice},
    rope::EditRope,
    store::{RegisterCell, RegisterPutFlags, Store},
};

use super::{CursorRange, EditBuffer};

pub trait EditActions<C, I>
where
    I: ApplicationInfo,
{
    fn delete(&mut self, range: &CursorRange, ctx: &C, store: &mut Store<I>) -> CursorChoice;
    fn yank(&mut self, range: &CursorRange, ctx: &C, store: &mut Store<I>) -> CursorChoice;
    fn replace(
        &mut self,
        c: char,
        virt: bool,
        range: &CursorRange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> CursorChoice;
    fn changecase(
        &mut self,
        case: &Case,
        range: &CursorRange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> CursorChoice;
    fn format(&mut self, range: &CursorRange, ctx: &C, store: &mut Store<I>) -> CursorChoice;
    fn changenum(
        &mut self,
        change: &NumberChange,
        mul: bool,
        range: &CursorRange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> CursorChoice;
    fn join(
        &mut self,
        spaces: JoinStyle,
        range: &CursorRange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> CursorChoice;
    fn indent(
        &mut self,
        change: &IndentChange,
        range: &CursorRange,
        ctx: &C,
        store: &mut Store<I>,
    ) -> CursorChoice;
}

impl<'a, 'b, 'c, C, I> EditActions<CursorMovementsContext<'a, 'b, 'c, Cursor, C>, I>
    for EditBuffer<I>
where
    C: EditContext,
    I: ApplicationInfo,
{
    fn delete(
        &mut self,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        store: &mut Store<I>,
    ) -> CursorChoice {
        let style = ctx.context.get_insert_style().unwrap_or(InsertStyle::Insert);
        let (shape, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut deleted = EditRope::from("");
        let mut first = true;
        let mut coff = self.text.cursor_to_offset(&range.start);

        for (start, end, inclusive) in ranges.into_iter().rev() {
            if first {
                first = false;
            } else {
                deleted = EditRope::from("\n") + deleted;
            }

            let (prefix, text, suffix) = self.text.split(start, end, inclusive);

            let tlines = text.get_lines();
            let tlen = text.len() as isize;
            let lstart = self.text.line_of_offset(start);

            deleted = text + deleted;

            match style {
                InsertStyle::Insert => {
                    self.text = prefix + suffix;
                },
                InsertStyle::Replace => {
                    let current = self.history.current();
                    let restore = current.slice(start, end, inclusive);

                    self.text = prefix + restore + suffix;
                },
            }

            if tlines == 0 {
                let cstart = self.text.offset_to_cursor(start);
                self._adjust_columns(cstart.y, cstart.x, 0, -tlen, store);
            } else {
                let lend = lstart.saturating_add(tlines - 1);
                self._adjust_lines(lstart, lend, isize::MAX, -(tlines as isize), store);
            }

            coff = start;
        }

        self.text.trailing_newline();

        let cell = RegisterCell::new(shape, deleted);
        let register = ctx.context.get_register().unwrap_or(Register::Unnamed);
        let mut flags = RegisterPutFlags::DELETE;

        if ctx.context.get_register_append() {
            flags |= RegisterPutFlags::APPEND
        }

        store.registers.put(&register, cell, flags);

        let cursor = self.text.offset_to_cursor(coff);

        return CursorChoice::Single(cursor);
    }

    fn yank(
        &mut self,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        store: &mut Store<I>,
    ) -> CursorChoice {
        let (shape, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut yanked = EditRope::from("");
        let mut first = true;
        let mut cursors = None;

        for (start, end, inclusive) in ranges.into_iter() {
            if first {
                first = false;
            } else {
                yanked += EditRope::from('\n');
            }

            if let Some((_, ref mut eoff)) = cursors {
                *eoff = end;
            } else {
                cursors = Some((start, end));
            }

            yanked += self.text.slice(start, end, inclusive);
        }

        let cell = RegisterCell::new(shape, yanked);
        let register = ctx.context.get_register().unwrap_or(Register::Unnamed);
        let mut flags = RegisterPutFlags::NONE;

        if ctx.context.get_register_append() {
            flags |= RegisterPutFlags::APPEND;
        }

        store.registers.put(&register, cell, flags);

        cursors
            .map(|(start, end)| {
                let start = self.text.offset_to_cursor(start);
                let end = self.text.offset_to_cursor(end);

                match shape {
                    TargetShape::CharWise | TargetShape::LineWise => {
                        // CharWise and LineWise yanks place the cursor at the range start.
                        CursorChoice::Range(start, end, range.start.clone())
                    },
                    TargetShape::BlockWise => {
                        // BlockWise yanks place the cursor at the upper left.
                        CursorChoice::Range(start.clone(), end, start)
                    },
                }
            })
            .unwrap_or_default()
    }

    fn replace(
        &mut self,
        c: char,
        _virt: bool,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        _: &mut Store<I>,
    ) -> CursorChoice {
        let (_, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut cursor = None;

        // XXX: if this is a blockwise replace, then whitespace needs to be split into individual
        // spaces first, and then replaced.

        for (start, end, inclusive) in ranges.into_iter().rev() {
            self.text = self.text.transform(start, end, inclusive, |r| {
                let s: String = r.to_string();
                let n: String =
                    s.chars().map(|i| if i == '\n' || i == '\r' { i } else { c }).collect();
                return EditRope::from(n);
            });

            /*
             * Unlike most operations, character replacement puts the cursor on the final character
             * in the affected range, and not immediately after it. This allows the cursor to stay
             * in place when doing a single character replacement (e.g. "ra").
             */
            let _ = if inclusive || end == 0.into() {
                cursor.get_or_insert(end)
            } else {
                cursor.get_or_insert(end - 1.into())
            };
        }

        return cursor
            .map(|off| self.text.offset_to_cursor(off).into())
            .unwrap_or_default();
    }

    fn changecase(
        &mut self,
        case: &Case,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        _: &mut Store<I>,
    ) -> CursorChoice {
        let (shape, ranges) = self._effective(range, ctx.context.get_target_shape());
        let mut cursors = None;

        for (start, end, inclusive) in ranges.into_iter().rev() {
            self.text = self.text.transform(start, end, inclusive, |r| r.changecase(case));

            if let Some((ref mut soff, _)) = cursors {
                *soff = start;
            } else {
                cursors = Some((start, end));
            }
        }

        cursors
            .map(|(start, end)| {
                let start = self.text.offset_to_cursor(start);
                let end = self.text.offset_to_cursor(end);

                match shape {
                    TargetShape::CharWise | TargetShape::BlockWise => {
                        CursorChoice::Range(start.clone(), end, start)
                    },
                    TargetShape::LineWise => {
                        let default = if range.start.y == range.end.y {
                            self.text.first_word(&range.start, ctx)
                        } else {
                            range.start.clone()
                        };

                        CursorChoice::Range(start, end, default)
                    },
                }
            })
            .unwrap_or_default()
    }

    fn indent(
        &mut self,
        _: &IndentChange,
        _: &CursorRange,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        _: &mut Store<I>,
    ) -> CursorChoice {
        // XXX: implement (:help <, :help >, :help v_b_<, :help v_b_>)

        return CursorChoice::Empty;
    }

    fn format(
        &mut self,
        _: &CursorRange,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        _: &mut Store<I>,
    ) -> CursorChoice {
        /*
         * Automatically formatting lines requires a whole lot of logic that just doesn't exist
         * in this codebase yet. At some point, if some kind of filetype detection is added, then
         * this function can be made to do something useful.
         */
        return CursorChoice::Empty;
    }

    fn changenum(
        &mut self,
        change: &NumberChange,
        mul: bool,
        range: &CursorRange,
        ctx: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        store: &mut Store<I>,
    ) -> CursorChoice {
        let mut diff = match change {
            NumberChange::Decrease(count) => -(ctx.context.resolve(count) as isize),
            NumberChange::Increase(count) => ctx.context.resolve(count) as isize,
        };
        let off = if mul { diff } else { 0 };

        let mut cursor = range.start.clone();
        let mut res = CursorChoice::Empty;

        while cursor.y <= range.end.y {
            let style = WordStyle::Number(Radix::Decimal);
            let number = match self.text.get_cursor_word_mut(&mut cursor, &style) {
                Some(n) => n,
                None => return CursorChoice::Empty,
            };

            if cursor > range.end {
                break;
            }

            let nlen = number.len();

            let n = Cow::from(&number);
            let n = match n.as_ref().parse::<isize>() {
                Ok(n) => EditRope::from((n + diff).to_string()),
                Err(_) => return CursorChoice::Empty,
            };

            let mut nend = cursor.clone();
            nend.x += nlen;
            let start = self.text.cursor_to_offset(&cursor);
            let end = self.text.cursor_to_offset(&nend);

            let (choice, adjs) = self.text.replace(start, end, false, n);
            self._adjust_all(adjs, store);

            res = if let CursorChoice::Range(s, e, _) = choice {
                CursorChoice::Range(s, e.clone(), e)
            } else {
                choice
            };

            diff += off;

            cursor.y += 1;
            cursor.x = 0;
        }

        return res;
    }

    fn join(
        &mut self,
        spaces: JoinStyle,
        range: &CursorRange,
        _: &CursorMovementsContext<'a, 'b, 'c, Cursor, C>,
        store: &mut Store<I>,
    ) -> CursorChoice {
        // Joining is always forced into a LineWise movement.
        let (_, ranges) = self._effective(range, Some(TargetShape::LineWise));
        let mut cursor = None;

        for (start, end, inclusive) in ranges.into_iter().rev() {
            let mut nls = Vec::new();

            for nl in self.text.newlines(start) {
                if nl > end || (!inclusive && nl == end) {
                    break;
                }

                nls.push(nl);
            }

            if nls.len() > 1 {
                /*
                 * Normally we ignore the final newline in the range since it's the lines *inside*
                 * the range that are being joined together, but since the minimum number of lines
                 * joined is 2, we don't remove the last offset if it's the only offset.
                 */
                let _ = nls.pop();
            }

            for nl in nls.into_iter().rev() {
                // Leave the buffer's final newline alone.
                if nl == self.text.last_offset() {
                    continue;
                }

                let (choice, adjs) = match spaces {
                    JoinStyle::OneSpace => {
                        let mut iter = self.text.chars(nl + 1.into());
                        let mut blank = false;

                        for c in iter.by_ref() {
                            if c == '\n' {
                                blank = true;
                                break;
                            } else if c.is_ascii_whitespace() {
                                continue;
                            } else {
                                break;
                            }
                        }

                        let jtxt = if blank { "" } else { " " };

                        let stop = iter.pos();

                        self.text.replace(nl, stop, false, jtxt.into())
                    },
                    JoinStyle::NewSpace => self.text.replace(nl, nl, true, " ".into()),
                    JoinStyle::NoChange => self.text.replace(nl, nl, true, "".into()),
                };

                cursor
                    .get_or_insert_with(|| choice.get(CursorEnd::Auto).cloned().unwrap_or_default())
                    .adjust(&adjs);

                self._adjust_all(adjs, store);
            }
        }

        return cursor.map(CursorChoice::Single).unwrap_or_default();
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    use crate::editing::base::CursorEnd;

    macro_rules! get_reg {
        ($store: expr, $reg: expr) => {
            $store.registers.get(&$reg)
        };
    }

    macro_rules! get_named_reg {
        ($store: expr, $reg: expr) => {
            get_reg!($store, Register::Named($reg))
        };
    }

    macro_rules! get_recent_del_reg {
        ($store: expr, $n: expr) => {
            get_reg!($store, Register::RecentlyDeleted($n))
        };
    }

    #[test]
    fn test_replace() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello world\na b c d e\nfoo bar baz");

        // 3r!
        let mov = MoveType::Column(MoveDir1D::Next, false);
        vctx.action.replace = Some('!'.into());
        edit!(ebuf, EditAction::Replace(false), mv!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "!!!lo world\na b c d e\nfoo bar baz\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));

        // replace three words ("!", "lo", "world") w/ "Q"
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        vctx.action.replace = Some('Q'.into());
        edit!(ebuf, EditAction::Replace(false), mv!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "!!QQQQQQQQQ\na b c d e\nfoo bar baz\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 10));

        // replace two lines w/ ":", leaving newlines intact.
        let mov = RangeType::Line;
        vctx.action.replace = Some(':'.into());
        edit!(ebuf, EditAction::Replace(false), range!(mov, 2), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), ":::::::::::\n:::::::::\nfoo bar baz\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 8));
    }

    #[test]
    fn test_yank() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello world\na b c d e\nfoo bar baz");

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);

        // Move forward to "world"
        edit!(ebuf, EditAction::Motion, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Test that we use the unnamed register ("") by default.
        edit!(ebuf, EditAction::Yank, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Both "" and "0 should now be updated.
        assert_eq!(get_reg!(store, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(CharWise, "world"));

        // Test using the named 'a' register ("a).
        vctx.action.count = Some(3);
        vctx.action.register = Some(Register::Named('a'));
        edit!(ebuf, EditAction::Yank, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Both "" and "a should now be updated, and "0 untouched.
        assert_eq!(get_reg!(store, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(CharWise, "world\na b "));
        assert_eq!(get_named_reg!(store, 'a'), cell!(CharWise, "world\na b "));

        // Append a line to the 'a' register ("A).
        vctx.action.count = None;
        vctx.action.register = Some(Register::Named('a'));
        vctx.action.register_append = true;
        edit!(ebuf, EditAction::Yank, range!(RangeType::Line), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // Both "" and "a should contain appended text, and "0 be untouched.
        assert_eq!(get_reg!(store, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(
            get_reg!(store, Register::Unnamed),
            cell!(LineWise, "world\na b \nhello world\n")
        );
        assert_eq!(get_named_reg!(store, 'a'), cell!(LineWise, "world\na b \nhello world\n"));

        // The blackhole register ("_) discards the yanked text.
        vctx.action.count = None;
        vctx.action.register = Some(Register::Blackhole);
        vctx.action.register_append = false;
        edit!(ebuf, EditAction::Yank, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 6));

        // All registers should be untouched, and "_ should not return the word "world".
        assert_eq!(get_reg!(store, Register::LastYanked), cell!(CharWise, "world"));
        assert_eq!(
            get_reg!(store, Register::Unnamed),
            cell!(LineWise, "world\na b \nhello world\n")
        );
        assert_eq!(get_named_reg!(store, 'a'), cell!(LineWise, "world\na b \nhello world\n"));
        assert_eq!(get_reg!(store, Register::Blackhole), cell!(CharWise, ""));
    }

    #[test]
    fn test_delete() {
        let (mut ebuf, curid, vwctx, vctx, mut store) =
            mkfivestr("hello world\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Test deleting a word.
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "world\na b c d e f\n\n\n1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(get_reg!(store, Register::LastYanked), RegisterCell::default());

        // Less than a line was deleted, so this goes into "-, not "1.
        assert_eq!(get_reg!(store, Register::SmallDelete), cell!(CharWise, "hello "));
        assert_eq!(get_recent_del_reg!(store, 0), RegisterCell::default());

        // Test that deleting multiple words crosses lines.
        edit!(ebuf, EditAction::Delete, mv!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "c d e f\n\n\n1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // More than a line was deleted, so this goes into "1 and "- is untouched.
        assert_eq!(get_reg!(store, Register::SmallDelete), cell!(CharWise, "hello "));
        assert_eq!(get_recent_del_reg!(store, 0), cell!(CharWise, "world\na b "));

        // Test that the behaviour changes if the last word is at the end of a line.
        edit!(ebuf, EditAction::Delete, mv!(mov, 4), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "\n\n\n1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Less than a line was deleted, so this goes into "- and "1 is untouched.
        assert_eq!(get_reg!(store, Register::SmallDelete), cell!(CharWise, "c d e f"));
        assert_eq!(get_recent_del_reg!(store, 0), cell!(CharWise, "world\na b "));

        // Test deleting blank lines.
        edit!(
            ebuf,
            EditAction::Delete,
            range!(RangeType::Line, 3),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // More than a line was deleted, so "1 shifts to "2, this goes into "1 and "- is untouched.
        assert_eq!(get_reg!(store, Register::SmallDelete), cell!(CharWise, "c d e f"));
        assert_eq!(get_recent_del_reg!(store, 0), cell!(LineWise, "\n\n\n"));
        assert_eq!(get_recent_del_reg!(store, 1), cell!(CharWise, "world\na b "));

        // Move forward two words and delete text in middle of line.
        edit!(ebuf, EditAction::Motion, mv!(mov, 2), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "1 2 3 4 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Test deleting in middle of string.
        edit!(ebuf, EditAction::Delete, mv!(mov, 2), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "1 2 5 6\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));

        // Less than a line was deleted, so "- is updated, other registers remain the same.
        assert_eq!(get_reg!(store, Register::SmallDelete), cell!(CharWise, "3 4 "));
        assert_eq!(get_recent_del_reg!(store, 0), cell!(LineWise, "\n\n\n"));
        assert_eq!(get_recent_del_reg!(store, 1), cell!(CharWise, "world\na b "));

        // Test that deleting more lines than exists deletes whole string.
        edit!(
            ebuf,
            EditAction::Delete,
            range!(RangeType::Line, 3),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // "1 and "2 get shifted, "0 set, "- untouched.
        assert_eq!(get_reg!(store, Register::SmallDelete), cell!(CharWise, "3 4 "));
        assert_eq!(get_recent_del_reg!(store, 0), cell!(LineWise, "1 2 5 6\n"));
        assert_eq!(get_recent_del_reg!(store, 1), cell!(LineWise, "\n\n\n"));
        assert_eq!(get_recent_del_reg!(store, 2), cell!(CharWise, "world\na b "));
    }

    #[test]
    fn test_delete_blockwise() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello world\n1 2 3 4 5 6\n  a b c d e f\n");

        // Set cursor to (0, 7).
        ebuf.set_leader(curid, Cursor::new(0, 7));

        // Do a blockwise delete from here to the first word of the third line.
        vctx.persist.shape = Some(TargetShape::BlockWise);
        edit!(
            ebuf,
            EditAction::Delete,
            range!(RangeType::Line, 3),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "herld\n1 5 6\n  d e f\n");

        // Check that the deleted text went into "" and "1. "0 and "- should be untouched.
        assert_eq!(get_recent_del_reg!(store, 0), cell!(BlockWise, "llo wo\n2 3 4 \na b c "));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(BlockWise, "llo wo\n2 3 4 \na b c "));
        assert_eq!(get_reg!(store, Register::LastYanked), cell!(CharWise, ""));
        assert_eq!(get_reg!(store, Register::SmallDelete), cell!(CharWise, ""));
    }

    #[test]
    fn test_delete_eol() {
        let (mut ebuf, curid, vwctx, vctx, mut store) =
            mkfivestr("hello world\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Set cursor to (0, 3).
        ebuf.set_leader(curid, Cursor::new(0, 3));

        // Delete from cursor to the end of the line ("d$").
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Delete, mv!(mov, 0), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(ebuf.get_text(), "hel\na b c d e f\n\n\n1 2 3 4 5 6\n");
    }

    #[test]
    fn test_change() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello world\na b c d e f\n\n\n1 2 3 4 5 6\n");

        vctx.persist.insert = Some(InsertStyle::Insert);

        // Start out at (0, 3).
        ebuf.set_leader(curid, Cursor::new(0, 3));

        // Delete from cursor to the end of the line ("c$").
        let mov = MoveType::LinePos(MovePosition::End);
        edit!(ebuf, EditAction::Delete, mv!(mov, 0), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));
        assert_eq!(ebuf.get_text(), "hel\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Delete previous character ("<BS>").
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(ebuf.get_text(), "he\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Delete previous word ("^W").
        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Previous);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(ebuf.get_text(), "\na b c d e f\n\n\n1 2 3 4 5 6\n");

        // Delete next character ("<Del>").
        let mov = MoveType::Column(MoveDir1D::Next, true);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(ebuf.get_text(), "a b c d e f\n\n\n1 2 3 4 5 6\n");

        // Move to (0, 3).
        ebuf.set_leader(curid, Cursor::new(3, 0));

        // Delete previous newline character ("<BS>").
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(2, 0));
        assert_eq!(ebuf.get_text(), "a b c d e f\n\n1 2 3 4 5 6\n");

        // Delete two previous newline characters ("<BS>").
        let mov = MoveType::Column(MoveDir1D::Previous, true);
        vctx.action.count = Some(2);
        edit!(ebuf, EditAction::Delete, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));
        assert_eq!(ebuf.get_text(), "a b c d e f1 2 3 4 5 6\n");
    }

    #[test]
    fn test_changecase() {
        let (mut ebuf, curid, vwctx, vctx, mut store) =
            mkfivestr("thiS iS An eXaMpLE of mIxed cASE\n");

        let mov = MoveType::WordBegin(WordStyle::Little, MoveDir1D::Next);

        // Test Case::Toggle operations
        let operation = EditAction::ChangeCase(Case::Toggle);

        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "THIs iS An eXaMpLE of mIxed cASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Running a second time toggles again
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "thiS Is aN ExAmPle OF MiXED Case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Test Case::Upper operations
        let operation = EditAction::ChangeCase(Case::Upper);

        // Make first word uppercase
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "THIS Is aN ExAmPle OF MiXED Case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Uppercasing is idempotent
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "THIS Is aN ExAmPle OF MiXED Case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Make whole line uppercase
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "THIS IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Uppercasing is idempotent
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "THIS IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Test Case::Lower operations
        let operation = EditAction::ChangeCase(Case::Lower);

        // Make first word lowercase
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "this IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Operation is idempotent
        edit!(ebuf, operation, mv!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "this IS AN EXAMPLE OF MIXED CASE\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Make whole line lowercase
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "this is an example of mixed case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // Operation is idempotent
        edit!(ebuf, operation, range!(RangeType::Line), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "this is an example of mixed case\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        // XXX: cursor should move to first word after g~~/gUU/guu
    }

    #[test]
    fn test_changecase_tilde() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("thiS iS An eXaMpLE of mIxed cASE\nfoo bar\n");

        let operation = EditAction::ChangeCase(Case::Toggle);
        let mov = MoveType::Column(MoveDir1D::Next, false);

        vctx.action.cursor_end = Some(CursorEnd::End);

        // Start out at (0, 11), at the start of "eXaMpLE".
        ebuf.set_leader(curid, Cursor::new(0, 11));

        // Toggle case to the end of the line ("100~").
        edit!(ebuf, operation, mv!(mov, 100), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "thiS iS An ExAmPle OF MiXED Case\nfoo bar\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 31));
    }

    #[test]
    fn test_forced_motion_char() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello\nworld\na b c d e\n    word\n");

        let op = EditAction::Yank;
        vctx.persist.shape = Some(TargetShape::CharWise);

        // Move to (0, 2) to begin.
        ebuf.set_leader(curid, Cursor::new(0, 2));

        // Forced linewise into charwise motion (2yvj)
        let mov = MoveType::Line(MoveDir1D::Next);
        edit!(ebuf, op, mv!(mov, 2), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(CharWise, "llo\nworld\na "));

        // Forced linewise into charwise motion (4yvG)
        let mov = MoveType::BufferLineOffset;
        edit!(ebuf, op, mv!(mov, 4), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(
            get_reg!(store, Register::Unnamed),
            cell!(CharWise, "llo\nworld\na b c d e\n    ")
        );

        // Forced linewise into charwise motion (yv'a)
        ebuf.set_leader(curid, Cursor::new(3, 6));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 2));

        edit_line_mark!(ebuf, op, 'a', curid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(
            get_reg!(store, Register::Unnamed),
            cell!(CharWise, "llo\nworld\na b c d e\n    ")
        );
    }

    #[test]
    fn test_forced_motion_line() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello\nworld\na b c d e\n1 2 3 4 5 6");

        let op = EditAction::Yank;
        vctx.persist.shape = Some(TargetShape::LineWise);

        // Move to (0, 2) to begin.
        ebuf.set_leader(curid, Cursor::new(0, 2));

        // Force charwise into linewise motion (100yVl)
        let mov = MoveType::Column(MoveDir1D::Next, false);
        edit!(ebuf, op, mv!(mov, 100), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(LineWise, "hello\n"));

        // Force charwise into linewise motion (2yVgj)
        let mov = MoveType::ScreenLine(MoveDir1D::Next);
        edit!(ebuf, op, mv!(mov, 2), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(
            get_reg!(store, Register::Unnamed),
            cell!(LineWise, "hello\nworld\na b c d e\n")
        );

        // Force charwise into linewise motion (yV`a)
        ebuf.set_leader(curid, Cursor::new(1, 2));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 2));

        edit_char_mark!(ebuf, op, 'a', curid, vwctx, vctx, store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(LineWise, "hello\nworld\n"));
    }

    #[test]
    fn test_forced_motion_block() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("hello\nworld\na b c d e\n1 2 3 4 5 6");

        vctx.persist.shape = Some(TargetShape::BlockWise);

        let mov = MoveType::Line(MoveDir1D::Next);

        // Forced linewise into blockwise motion ("1y<C-V>j")
        edit!(ebuf, EditAction::Yank, mv!(mov, 1), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(BlockWise, "h\nw"));

        // Forced linewise into blockwise motion ("3y<C-V>j").
        edit!(ebuf, EditAction::Yank, mv!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(BlockWise, "h\nw\na\n1"));

        // Move down and test from a position that skips chars.
        ebuf.set_leader(curid, Cursor::new(3, 6));

        let mov = MoveType::Line(MoveDir1D::Previous);

        // Force linewise into blockwise motion ("3y<C-V>k").
        edit!(ebuf, EditAction::Yank, mv!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 4));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(BlockWise, "o\nd\nc d\n3 4"));

        // Mark (3, 6), and move to (0, 2) so we can make a wider block.
        ebuf.set_leader(curid, Cursor::new(3, 6));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 2));

        // Force charwise into blockwise motion ("y<C-V>`a").
        let target = EditTarget::CharJump(Specifier::Exact(mark!('a')));
        edit!(ebuf, EditAction::Yank, target, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));
        assert_eq!(get_reg!(store, Register::Unnamed), cell!(BlockWise, "llo\nrld\nb c d\n2 3 4"));

        // Mark (3, 0), and move to (0, 4) so we can make a wider block.
        ebuf.set_leader(curid, Cursor::new(3, 0));
        ebuf.mark(mark!('a'), ctx!(curid, vwctx, vctx), &mut store).unwrap();
        ebuf.set_leader(curid, Cursor::new(0, 4));

        // Test with a bottom cursor w/ a column that comes before the top cursor's column.
        let target = EditTarget::CharJump(Specifier::Exact(mark!('a')));
        edit!(ebuf, EditAction::Yank, target, ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));
        assert_eq!(
            get_reg!(store, Register::Unnamed),
            cell!(BlockWise, "hello\nworld\na b c\n1 2 3")
        );
    }

    #[test]
    fn test_join_spaces() {
        let (mut ebuf, curid, vwctx, vctx, mut store) =
            mkfivestr("foo\n       hello world\n  a b c\n   d e\n    first\n");

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let operation = EditAction::Join(JoinStyle::OneSpace);

        // Joining with a count of 1 should still do something.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 1), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo hello world\n  a b c\n   d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        // Join with a count of 2 joins the current and next line.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo hello world a b c d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 21));

        // Try to join with four following lines, hitting the end of the buffer.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 5), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo hello world a b c d e first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 25));

        // Joining when there's only one line, should do nothing.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo hello world a b c d e first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 25));
    }

    #[test]
    fn test_join_nospaces() {
        let (mut ebuf, curid, vwctx, vctx, mut store) =
            mkfivestr("foo\n       hello world\n  a b c\n   d e\n    first\n");

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let operation = EditAction::Join(JoinStyle::NoChange);

        // Join with a count of 1 still joins 2 lines together.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 1), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo       hello world\n  a b c\n   d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        // Join the current line with the following two lines.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo       hello world  a b c   d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 28));

        // Try to join the next four lines, hitting the end of the buffer.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 4), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo       hello world  a b c   d e    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 34));

        // Joining when there's only one line, should do nothing.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo       hello world  a b c   d e    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 34));
    }

    #[test]
    fn test_join_new_spaces() {
        let (mut ebuf, curid, vwctx, vctx, mut store) =
            mkfivestr("foo\n       hello world\n  a b c\n   d e\n    first\n");

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let operation = EditAction::Join(JoinStyle::NewSpace);

        // Join with a count of 1 still joins 2 lines together.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 1), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo        hello world\n  a b c\n   d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        // Join the current line with the following two lines.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo        hello world   a b c    d e\n    first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 30));

        // Try to join the next four lines, hitting the end of the buffer.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov, 4), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo        hello world   a b c    d e     first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 37));

        // Joining when there's only one line, should do nothing.
        let mov = RangeType::Line;
        edit!(ebuf, operation, range!(mov), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo        hello world   a b c    d e     first\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 37));
    }

    #[test]
    fn test_join_blanks() {
        let (mut ebuf, curid, vwctx, vctx, mut store) = mkfivestr("foo\n\n\n     \n    first\n");

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let operation = EditAction::Join(JoinStyle::OneSpace);

        // Join with the next empty line, adding no space.
        edit!(ebuf, operation, range!(RangeType::Line, 2), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo\n\n     \n    first\n");

        // Join with the blank line, and the all spaces line.
        edit!(ebuf, operation, range!(RangeType::Line, 3), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo\n    first\n");

        // Join with final line.
        edit!(ebuf, operation, range!(RangeType::Line, 2), ctx!(curid, vwctx, vctx), store);
        assert_eq!(ebuf.get_text(), "foo first\n");
    }

    #[test]
    fn test_changenum() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) = mkfivestr("a 1 b 2 c\nd 3 e 4 f\n");

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let opdec = EditAction::ChangeNumber(NumberChange::Decrease(Count::Contextual), false);
        let opinc = EditAction::ChangeNumber(NumberChange::Increase(Count::Contextual), false);

        vctx.action.count = Some(3);
        edit!(
            ebuf,
            opinc,
            mv!(MoveType::LinePos(MovePosition::End), 0),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "a 4 b 2 c\nd 3 e 4 f\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));

        vctx.action.count = Some(9);
        edit!(
            ebuf,
            opdec,
            mv!(MoveType::LinePos(MovePosition::End), 0),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "a -5 b 2 c\nd 3 e 4 f\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        vctx.action.count = Some(1);
        edit!(
            ebuf,
            opdec,
            mv!(MoveType::LinePos(MovePosition::End), 0),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "a -6 b 2 c\nd 3 e 4 f\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 3));

        vctx.action.count = Some(7);
        edit!(
            ebuf,
            opinc,
            mv!(MoveType::LinePos(MovePosition::End), 0),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "a 1 b 2 c\nd 3 e 4 f\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 2));

        ebuf.set_leader(curid, Cursor::new(1, 4));

        vctx.action.count = Some(2);
        edit!(
            ebuf,
            opinc,
            mv!(MoveType::LinePos(MovePosition::End), 0),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "a 1 b 2 c\nd 3 e 6 f\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));

        vctx.action.count = Some(3);
        edit!(
            ebuf,
            opdec,
            mv!(MoveType::LinePos(MovePosition::End), 0),
            ctx!(curid, vwctx, vctx),
            store
        );
        assert_eq!(ebuf.get_text(), "a 1 b 2 c\nd 3 e 3 f\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 6));
    }

    #[test]
    fn test_changenum_mul() {
        let (mut ebuf, curid, vwctx, mut vctx, mut store) =
            mkfivestr("a 1 b 2 c\nd 3 e 4 f\ng 5 h 6 i\nj 7 k 8 l\n");

        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 0));

        let opdec = EditAction::ChangeNumber(NumberChange::Decrease(Count::Contextual), true);
        let opinc = EditAction::ChangeNumber(NumberChange::Increase(Count::Contextual), true);

        vctx.action.count = Some(5);
        edit!(ebuf, opinc, &range!(RangeType::Buffer), ctx!(curid, vwctx, vctx), &mut store);
        assert_eq!(ebuf.get_text(), "a 6 b 2 c\nd 13 e 4 f\ng 20 h 6 i\nj 27 k 8 l\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 3));

        vctx.action.count = Some(2);
        edit!(ebuf, opdec, &range!(RangeType::Buffer), ctx!(curid, vwctx, vctx), &mut store);
        assert_eq!(ebuf.get_text(), "a 4 b 2 c\nd 9 e 4 f\ng 14 h 6 i\nj 19 k 8 l\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(3, 3));
    }
}
