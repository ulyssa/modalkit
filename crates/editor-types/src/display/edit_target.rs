use super::*;

impl From<&EditTarget> for ActionToken<'_> {
    fn from(target: &EditTarget) -> Self {
        match target {
            EditTarget::CurrentPosition => ActionToken::Word("current-position"),
            EditTarget::Selection => ActionToken::Word("selection"),

            EditTarget::Boundary(range, inclusive, terminus, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("boundary"),
                    ActionToken::Flag(Flag::Short('T')),
                    ActionToken::from(range),
                    ActionToken::Flag(Flag::Long("inclusive".into())),
                    ActionToken::Bool(*inclusive),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(terminus),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
            EditTarget::CharJump(mark) => {
                ActionToken::Group(vec![
                    ActionToken::Word("char-jump"),
                    ActionToken::Flag(Flag::Mark),
                    specifier(mark),
                ])
            },
            EditTarget::LineJump(mark) => {
                ActionToken::Group(vec![
                    ActionToken::Word("line-jump"),
                    ActionToken::Flag(Flag::Mark),
                    specifier(mark),
                ])
            },
            EditTarget::Motion(motion, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("motion"),
                    ActionToken::Flag(Flag::Short('T')),
                    ActionToken::from(motion),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
            EditTarget::Range(range, inclusive, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("range"),
                    ActionToken::Flag(Flag::Short('T')),
                    ActionToken::from(range),
                    ActionToken::Flag(Flag::Long("inclusive".into())),
                    ActionToken::Bool(*inclusive),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
            EditTarget::Search(search, dir, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("search"),
                    ActionToken::Flag(Flag::Short('T')),
                    ActionToken::from(search),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
        }
    }
}
