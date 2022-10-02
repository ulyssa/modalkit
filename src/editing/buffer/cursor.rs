use crate::editing::action::EditResult;

use crate::editing::base::{Application, Count, CursorCloseTarget, EditContext, MoveDir1D};

use crate::util::idx_offset;

use super::{CursorGroupIdContext, EditBuffer};

pub trait CursorActions<C> {
    fn cursor_split(&mut self, count: Count, ctx: &C) -> EditResult;
    fn cursor_close(&mut self, target: &CursorCloseTarget, ctx: &C) -> EditResult;
    fn cursor_rotate(&mut self, dir: MoveDir1D, count: Count, ctx: &C) -> EditResult;
}

impl<'a, 'b, C, P> CursorActions<CursorGroupIdContext<'a, 'b, C>> for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn cursor_rotate(
        &mut self,
        dir: MoveDir1D,
        count: Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let EditBuffer { cursors, leaders, members, .. } = self;
        let off = ctx.2.resolve(&count);
        let gid = ctx.0;

        if let Some(members) = members.get_mut(&gid) {
            if members.len() == 0 {
                return Ok(None);
            }

            // Make the leader one of the members.
            let leader = leaders.get(&gid);
            let leader = leader.expect("no current group leader");
            members.push(*leader);

            // Sort all of the members by their cursor positions.
            members.sort_by(|a, b| {
                let a = cursors.get(*a);
                let b = cursors.get(*b);

                return a.cmp(&b);
            });

            // Calculate the relative offset of the new leader from the old leader.
            let idx = members
                .iter()
                .position(|id| id == leader)
                .map(|idx| idx_offset(idx, off, &dir, members.len(), true).unwrap_or(idx))
                .unwrap_or(0);

            // Promote the new leader.
            let leader = members.remove(idx);
            leaders.insert(gid, leader);
        }

        return Ok(None);
    }

    fn cursor_close(
        &mut self,
        target: &CursorCloseTarget,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let gid = ctx.0;

        match target {
            CursorCloseTarget::Leader => {
                self.delete_leader(gid);
            },
            CursorCloseTarget::Followers => {
                if let Some(members) = self.members.get_mut(&gid) {
                    let closed = members.split_off(0);

                    for member in closed.into_iter() {
                        self.delete_cursor(member);
                    }
                }
            },
        }

        Ok(None)
    }

    fn cursor_split(&mut self, count: Count, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let count = ctx.2.resolve(&count);
        let group = ctx.0;

        if count == 1 {
            return Ok(None);
        }

        for (_, cursor) in self.get_group_cursors(group) {
            for _ in 1..count {
                let _ = self.create_cursor_from(group, &cursor);
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    #[test]
    fn test_cursor_group() {
        let mut ebuf = mkbuf();
        let curid = ebuf.create_group();
        let vwctx = ViewportContext::default();
        let vctx = VimContext::default();

        // First split the cursor into five cursors.
        ebuf.cursor_split(Count::Exact(5), ctx!(curid, vwctx, vctx)).unwrap();

        // Now type with all of them.
        type_char!(ebuf, 'h', curid, vwctx, vctx);
        type_char!(ebuf, 'e', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        type_char!(ebuf, ' ', curid, vwctx, vctx);
        type_char!(ebuf, 'w', curid, vwctx, vctx);
        type_char!(ebuf, 'o', curid, vwctx, vctx);
        type_char!(ebuf, 'r', curid, vwctx, vctx);
        type_char!(ebuf, 'l', curid, vwctx, vctx);
        type_char!(ebuf, 'd', curid, vwctx, vctx);

        assert_eq!(ebuf.get_text(), "hello worldhello worldhello worldhello worldhello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));

        // And insert some newlines.
        type_char!(ebuf, '\n', curid, vwctx, vctx);
        assert_eq!(
            ebuf.get_text(),
            "hello world\nhello world\nhello world\nhello world\nhello world\n\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }
}
