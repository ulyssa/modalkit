use super::*;

impl From<&MoveType> for ActionToken<'_> {
    fn from(motion: &MoveType) -> Self {
        match motion {
            MoveType::BufferByteOffset => ActionToken::Word("buffer-byte-offset"),
            MoveType::BufferLineOffset => ActionToken::Word("buffer-line-offset"),
            MoveType::BufferLinePercent => ActionToken::Word("buffer-line-percent"),
            MoveType::ItemMatch => ActionToken::Word("item-match"),
            MoveType::LineColumnOffset => ActionToken::Word("line-column-offset"),
            MoveType::LinePercent => ActionToken::Word("line-percent"),

            MoveType::BufferPos(pos) => {
                ActionToken::Group(vec![
                    ActionToken::Word("buffer-pos"),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(pos),
                ])
            },
            MoveType::Column(dir, multiline) => {
                ActionToken::Group(vec![
                    ActionToken::Word("column"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Long("multiline".into())),
                    ActionToken::Bool(*multiline),
                ])
            },
            MoveType::FinalNonBlank(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("final-non-blank"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::FirstWord(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("first-word"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::Line(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("line"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::LinePos(pos) => {
                ActionToken::Group(vec![
                    ActionToken::Word("line-pos"),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(pos),
                ])
            },
            MoveType::WordBegin(style, dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("word-begin"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::WordEnd(style, dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("word-end"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::ParagraphBegin(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("paragraph-begin"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::SentenceBegin(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("sentence-begin"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::SectionBegin(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("section-begin"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::SectionEnd(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("section-end"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::ScreenFirstWord(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("screen-first-word"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::ScreenLine(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("screen-line"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            MoveType::ScreenLinePos(pos) => {
                ActionToken::Group(vec![
                    ActionToken::Word("screen-line-pos"),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(pos),
                ])
            },
            MoveType::ViewportPos(pos) => {
                ActionToken::Group(vec![
                    ActionToken::Word("viewport-pos"),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(pos),
                ])
            },
        }
    }
}
