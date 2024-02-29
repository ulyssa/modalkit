use crate::editing::{application::ApplicationInfo, context::Resolve, store::Store};

use super::{CursorGroupIdContext, EditBuffer};
use crate::errors::EditResult;
use crate::prelude::*;

pub trait CursorActions<C, S, I>
where
    I: ApplicationInfo,
{
    fn cursor_split(&mut self, count: &Count, ctx: &C, store: &mut S) -> EditResult<EditInfo, I>;

    fn cursor_close(
        &mut self,
        target: &CursorCloseTarget,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    fn cursor_restore(
        &mut self,
        style: &CursorGroupCombineStyle,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    fn cursor_rotate(
        &mut self,
        dir: MoveDir1D,
        count: &Count,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    fn cursor_save(
        &mut self,
        style: &CursorGroupCombineStyle,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;
}

impl<'a, I> CursorActions<CursorGroupIdContext<'a>, Store<I>, I> for EditBuffer<I>
where
    I: ApplicationInfo,
{
    fn cursor_rotate(
        &mut self,
        dir: MoveDir1D,
        count: &Count,
        ctx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let off = ctx.2.resolve(count);
        let gid = ctx.0;

        self.get_group_mut(gid).rotate(dir, off);

        return Ok(None);
    }

    fn cursor_close(
        &mut self,
        target: &CursorCloseTarget,
        ctx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let gid = ctx.0;

        self.get_group_mut(gid).close(target);

        Ok(None)
    }

    fn cursor_restore(
        &mut self,
        style: &CursorGroupCombineStyle,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let reg = ctx.2.get_register().unwrap_or(Register::UnnamedCursorGroup);
        let gid = ctx.0;

        // Get saved group.
        let ngroup = store.cursors.get_group(self.id.clone(), &reg)?;

        // Combine with current group.
        let ogroup = self.cursors.entry(gid).or_default();
        let merged = ogroup.combine(&ngroup, style, &self.text)?;
        *ogroup = merged;

        Ok(None)
    }

    fn cursor_save(
        &mut self,
        style: &CursorGroupCombineStyle,
        ctx: &CursorGroupIdContext<'a>,
        store: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
        let reg = ctx.2.get_register().unwrap_or(Register::UnnamedCursorGroup);
        let gid = ctx.0;

        // Get currently saved group.
        let ogroup = store.cursors.get_group(self.id.clone(), &reg)?;

        // Combine with current group.
        let cgroup = self.cursors.entry(gid).or_default();
        let merged = ogroup.combine(cgroup, style, &self.text)?;

        store.cursors.set_group(self.id.clone(), reg, merged)?;

        Ok(None)
    }

    fn cursor_split(
        &mut self,
        count: &Count,
        ctx: &CursorGroupIdContext<'a>,
        _: &mut Store<I>,
    ) -> EditResult<EditInfo, I> {
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
        let (mut ebuf, curid, vwctx, vctx, mut store) = mkfive();

        // First split the cursor into five cursors.
        ebuf.cursor_split(&Count::Exact(4), ctx!(curid, vwctx, vctx), &mut store)
            .unwrap();

        // Now type with all of them.
        type_char!(ebuf, 'h', curid, vwctx, vctx, store);
        type_char!(ebuf, 'e', curid, vwctx, vctx, store);
        type_char!(ebuf, 'l', curid, vwctx, vctx, store);
        type_char!(ebuf, 'l', curid, vwctx, vctx, store);
        type_char!(ebuf, 'o', curid, vwctx, vctx, store);

        assert_eq!(ebuf.get_text(), "hellohellohellohellohello\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 5));

        type_char!(ebuf, ' ', curid, vwctx, vctx, store);
        type_char!(ebuf, 'w', curid, vwctx, vctx, store);
        type_char!(ebuf, 'o', curid, vwctx, vctx, store);
        type_char!(ebuf, 'r', curid, vwctx, vctx, store);
        type_char!(ebuf, 'l', curid, vwctx, vctx, store);
        type_char!(ebuf, 'd', curid, vwctx, vctx, store);

        assert_eq!(ebuf.get_text(), "hello worldhello worldhello worldhello worldhello world\n");
        assert_eq!(ebuf.get_leader(curid), Cursor::new(0, 11));

        // And insert some newlines.
        type_char!(ebuf, '\n', curid, vwctx, vctx, store);
        assert_eq!(
            ebuf.get_text(),
            "hello world\nhello world\nhello world\nhello world\nhello world\n\n"
        );
        assert_eq!(ebuf.get_leader(curid), Cursor::new(1, 0));
    }
}
