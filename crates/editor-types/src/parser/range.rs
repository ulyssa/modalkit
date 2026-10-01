use super::*;

impl<I> RangeParser for ActionReader<I> {
    type Output = anyhow::Result<RangeType>;

    fn range_invalid<T: std::fmt::Display>(&self, msg: T) -> Self::Output {
        Err(anyhow!("{msg}"))
    }

    fn visit_word(&mut self, style: &[ActionToken<'_>]) -> Self::Output {
        let style = WordStyle::try_from(style)?;
        Ok(RangeType::Word(style))
    }

    fn visit_buffer(&mut self) -> Self::Output {
        Ok(RangeType::Buffer)
    }

    fn visit_paragraph(&mut self) -> Self::Output {
        Ok(RangeType::Paragraph)
    }

    fn visit_sentence(&mut self) -> Self::Output {
        Ok(RangeType::Sentence)
    }

    fn visit_line(&mut self) -> Self::Output {
        Ok(RangeType::Line)
    }

    fn visit_bracketed(
        &mut self,
        left: &[ActionToken<'_>],
        right: &[ActionToken<'_>],
    ) -> Self::Output {
        let left = parse_std_char(left)?;
        let right = parse_std_char(right)?;
        Ok(RangeType::Bracketed(left, right))
    }

    fn visit_item(&mut self) -> Self::Output {
        Ok(RangeType::Item)
    }

    fn visit_quote(&mut self, surround: &[ActionToken<'_>]) -> Self::Output {
        let c = parse_std_char(surround)?;
        Ok(RangeType::Quote(c))
    }

    fn visit_xml_tag(&mut self) -> Self::Output {
        Ok(RangeType::XmlTag)
    }
}
