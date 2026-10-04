use super::*;

impl From<&RangeType> for ActionToken<'_> {
    fn from(range: &RangeType) -> Self {
        match range {
            RangeType::Buffer => ActionToken::Word("buffer"),
            RangeType::Item => ActionToken::Word("item"),
            RangeType::Line => ActionToken::Word("line"),
            RangeType::Paragraph => ActionToken::Word("paragraph"),
            RangeType::Sentence => ActionToken::Word("sentence"),
            RangeType::XmlTag => ActionToken::Word("xml-tag"),

            RangeType::Bracketed(left, right) => {
                ActionToken::Group(vec![
                    ActionToken::Word("bracketed"),
                    ActionToken::Flag(Flag::Long("left".into())),
                    ActionToken::Char(*left),
                    ActionToken::Flag(Flag::Long("right".into())),
                    ActionToken::Char(*right),
                ])
            },
            RangeType::Quote(c) => prefixed("quote", ActionToken::Char(*c)),
            RangeType::Word(style) => {
                ActionToken::Group(vec![
                    ActionToken::Word("word"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                ])
            },
        }
    }
}
