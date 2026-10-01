use super::*;

impl<I> MotionParser for ActionReader<I> {
    type Output = anyhow::Result<MoveType>;

    /// Output an error for the current parse.
    fn motion_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output {
        bail!("{msg}")
    }

    fn visit_buffer_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = MovePosition::try_from(position)?;
        Ok(MoveType::BufferPos(pos))
    }

    fn visit_buffer_byte_offset(&mut self) -> Self::Output {
        Ok(MoveType::BufferByteOffset)
    }

    fn visit_buffer_line_offset(&mut self) -> Self::Output {
        Ok(MoveType::BufferLineOffset)
    }

    fn visit_buffer_line_percent(&mut self) -> Self::Output {
        Ok(MoveType::BufferLinePercent)
    }

    fn visit_column(&mut self, dir: &[ActionToken], multiline: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        let multiline = parse_std_bool(multiline)?;
        Ok(MoveType::Column(dir, multiline))
    }

    fn visit_final_non_blank(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::FinalNonBlank(dir))
    }

    fn visit_first_word(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::FirstWord(dir))
    }

    fn visit_item_match(&mut self) -> Self::Output {
        Ok(MoveType::ItemMatch)
    }

    fn visit_line(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::Line(dir))
    }

    fn visit_line_column_offset(&mut self) -> Self::Output {
        Ok(MoveType::LineColumnOffset)
    }

    fn visit_line_percent(&mut self) -> Self::Output {
        Ok(MoveType::LinePercent)
    }

    fn visit_line_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = MovePosition::try_from(position)?;
        Ok(MoveType::LinePos(pos))
    }

    fn visit_word_begin(&mut self, style: &[ActionToken], dir: &[ActionToken]) -> Self::Output {
        let style = WordStyle::try_from(style)?;
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::WordBegin(style, dir))
    }

    fn visit_word_end(&mut self, style: &[ActionToken], dir: &[ActionToken]) -> Self::Output {
        let style = WordStyle::try_from(style)?;
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::WordEnd(style, dir))
    }

    fn visit_paragraph_begin(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::ParagraphBegin(dir))
    }

    fn visit_sentence_begin(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::SentenceBegin(dir))
    }

    fn visit_section_begin(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::SectionBegin(dir))
    }

    fn visit_section_end(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::SectionEnd(dir))
    }

    fn visit_screen_first_word(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::ScreenFirstWord(dir))
    }

    fn visit_screen_line(&mut self, dir: &[ActionToken]) -> Self::Output {
        let dir = MoveDir1D::try_from(dir)?;
        Ok(MoveType::ScreenLine(dir))
    }

    fn visit_screen_line_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = MovePosition::try_from(position)?;
        Ok(MoveType::ScreenLinePos(pos))
    }

    fn visit_viewport_pos(&mut self, position: &[ActionToken]) -> Self::Output {
        let pos = MovePosition::try_from(position)?;
        Ok(MoveType::ViewportPos(pos))
    }
}
