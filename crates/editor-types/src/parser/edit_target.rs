use super::*;

impl<I> EditTargetParser for ActionReader<I> {
    type Output = anyhow::Result<EditTarget>;

    fn edit_target_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output {
        bail!("{msg}")
    }

    fn visit_boundary(
        &mut self,
        range: &[ActionToken],
        inclusive: &[ActionToken],
        terminus: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let range = RangeParserExt::parse_tokens(self, range)?;
        let inclusive = parse_std_bool(inclusive)?;
        let terminus = MoveTerminus::try_from(terminus)?;
        let count = Count::try_from(count)?;
        Ok(EditTarget::Boundary(range, inclusive, terminus, count))
    }

    fn visit_current_position(&mut self) -> Self::Output {
        Ok(EditTarget::CurrentPosition)
    }

    fn visit_char_jump(&mut self, mark: &[ActionToken]) -> Self::Output {
        let mark = parse_specifier::<Mark>(mark)?;
        Ok(EditTarget::CharJump(mark))
    }

    fn visit_line_jump(&mut self, mark: &[ActionToken]) -> Self::Output {
        let mark = parse_specifier::<Mark>(mark)?;
        Ok(EditTarget::LineJump(mark))
    }

    fn visit_motion(&mut self, motion: &[ActionToken], count: &[ActionToken]) -> Self::Output {
        let motion = MotionParserExt::parse_tokens(self, motion)?;
        let count = Count::try_from(count)?;
        Ok(EditTarget::Motion(motion, count))
    }

    fn visit_range(
        &mut self,
        range: &[ActionToken],
        inclusive: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let range = RangeParserExt::parse_tokens(self, range)?;
        let inclusive = parse_std_bool(inclusive)?;
        let count = Count::try_from(count)?;
        Ok(EditTarget::Range(range, inclusive, count))
    }

    fn visit_search(
        &mut self,
        search: &[ActionToken],
        dir: &[ActionToken],
        count: &[ActionToken],
    ) -> Self::Output {
        let search = SearchType::try_from(search)?;
        let dir = MoveDirMod::try_from(dir)?;
        let count = Count::try_from(count)?;
        Ok(EditTarget::Search(search, dir, count))
    }

    fn visit_selection(&mut self) -> Self::Output {
        Ok(EditTarget::Selection)
    }
}
