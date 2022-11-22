use crate::{
    editing::action::EditResult,
    editing::base::{
        Application,
        Count,
        CursorCloseTarget,
        CursorGroupCombineStyle,
        EditContext,
        MoveDir1D,
        Register,
    },
};

use super::{CursorGroupIdContext, EditBuffer};

pub trait CursorActions<C> {
    fn cursor_split(&mut self, count: &Count, ctx: &C) -> EditResult;
    fn cursor_close(&mut self, target: &CursorCloseTarget, ctx: &C) -> EditResult;
    fn cursor_restore(&mut self, style: &CursorGroupCombineStyle, ctx: &C) -> EditResult;
    fn cursor_rotate(&mut self, dir: MoveDir1D, count: &Count, ctx: &C) -> EditResult;
    fn cursor_save(&mut self, style: &CursorGroupCombineStyle, ctx: &C) -> EditResult;
}

impl<'a, 'b, C, P> CursorActions<CursorGroupIdContext<'a, 'b, C>> for EditBuffer<C, P>
where
    C: EditContext,
    P: Application,
{
    fn cursor_rotate(
        &mut self,
        dir: MoveDir1D,
        count: &Count,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let off = ctx.2.resolve(count);
        let gid = ctx.0;

        self.get_group_mut(gid).rotate(dir, off);

        return Ok(None);
    }

    fn cursor_close(
        &mut self,
        target: &CursorCloseTarget,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let gid = ctx.0;

        self.get_group_mut(gid).close(target);

        Ok(None)
    }

    fn cursor_restore(
        &mut self,
        style: &CursorGroupCombineStyle,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let reg = ctx.2.get_register().unwrap_or(Register::UnnamedCursorGroup);
        let gid = ctx.0;

        let locked = self.store.read().unwrap();

        // Get saved group.
        let ngroup = locked.cursors.get_group(self.id, &reg)?;

        // Combine with current group.
        let ogroup = self.cursors.entry(gid).or_default();
        let merged = ogroup.combine(&ngroup, style, &self.text)?;
        *ogroup = merged;

        Ok(None)
    }

    fn cursor_save(
        &mut self,
        style: &CursorGroupCombineStyle,
        ctx: &CursorGroupIdContext<'a, 'b, C>,
    ) -> EditResult {
        let reg = ctx.2.get_register().unwrap_or(Register::UnnamedCursorGroup);
        let gid = ctx.0;

        let mut locked = self.store.write().unwrap();

        // Get currently saved group.
        let ogroup = locked.cursors.get_group(self.id, &reg)?;

        // Combine with current group.
        let cgroup = self.cursors.entry(gid).or_default();
        let merged = ogroup.combine(&cgroup, style, &self.text)?;

        locked.cursors.set_group(self.id, reg, merged)?;

        Ok(None)
    }

    fn cursor_split(&mut self, count: &Count, ctx: &CursorGroupIdContext<'a, 'b, C>) -> EditResult {
        let count = ctx.2.resolve(count);

        self.get_group_mut(ctx.0).split(count);

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
        ebuf.cursor_split(&Count::Exact(4), ctx!(curid, vwctx, vctx)).unwrap();

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
