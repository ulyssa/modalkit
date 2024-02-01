use crate::editing::cursor::{Cursor, CursorState};
use crate::prelude::{CursorEnd, TargetShape};

/// Result type for functions that provide the option of leaving the cursor in different places.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum CursorChoice {
    /// One choice of cursor placement.
    Single(Cursor),

    /// The start and end positions of an affected range, and a default choice that may or may not
    /// be inside the range.
    Range(Cursor, Cursor, Cursor),

    /// No choices for cursor placement.
    #[default]
    Empty,
}

impl CursorChoice {
    /// Get a reference to the cursor that would be used in the [CursorState] returned by
    /// [CursorChoice::resolve].
    pub fn get(&self, placement: CursorEnd) -> Option<&Cursor> {
        match (self, placement) {
            (CursorChoice::Empty, _) => None,
            (CursorChoice::Single(_), CursorEnd::Keep) => None,
            (CursorChoice::Single(c), CursorEnd::Start) => Some(c),
            (CursorChoice::Single(c), CursorEnd::End) => Some(c),
            (CursorChoice::Single(c), CursorEnd::Selection) => Some(c),
            (CursorChoice::Single(c), CursorEnd::Auto) => Some(c),
            (CursorChoice::Range(_, _, _), CursorEnd::Keep) => None,
            (CursorChoice::Range(s, _, _), CursorEnd::Start) => Some(s),
            (CursorChoice::Range(_, e, _), CursorEnd::End) => Some(e),
            (CursorChoice::Range(_, e, _), CursorEnd::Selection) => Some(e),
            (CursorChoice::Range(_, _, d), CursorEnd::Auto) => Some(d),
        }
    }

    /// Choose the cursor placement as indicated.
    pub fn resolve(self, placement: CursorEnd) -> Option<CursorState> {
        match (self, placement) {
            (CursorChoice::Empty, _) => None,
            (CursorChoice::Single(_), CursorEnd::Keep) => None,
            (CursorChoice::Single(c), CursorEnd::Start) => Some(c.into()),
            (CursorChoice::Single(c), CursorEnd::End) => Some(c.into()),
            (CursorChoice::Single(c), CursorEnd::Selection) => {
                Some(CursorState::Selection(c.clone(), c, TargetShape::CharWise))
            },
            (CursorChoice::Single(c), CursorEnd::Auto) => Some(c.into()),
            (CursorChoice::Range(_, _, _), CursorEnd::Keep) => None,
            (CursorChoice::Range(s, _, _), CursorEnd::Start) => Some(s.into()),
            (CursorChoice::Range(_, e, _), CursorEnd::End) => Some(e.into()),
            (CursorChoice::Range(s, e, _), CursorEnd::Selection) => {
                Some(CursorState::Selection(e, s, TargetShape::CharWise))
            },
            (CursorChoice::Range(_, _, d), CursorEnd::Auto) => Some(d.into()),
        }
    }
}

impl From<Cursor> for CursorChoice {
    fn from(cursor: Cursor) -> Self {
        CursorChoice::Single(cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_choice_get() {
        let c2 = Cursor::new(2, 5);
        let c4 = Cursor::new(4, 3);
        let c5 = Cursor::new(5, 0);

        let choice = CursorChoice::Single(c5.clone());
        assert_eq!(choice.get(CursorEnd::Keep), None);
        assert_eq!(choice.get(CursorEnd::Start), Some(&c5));
        assert_eq!(choice.get(CursorEnd::End), Some(&c5));
        assert_eq!(choice.get(CursorEnd::Selection), Some(&c5));
        assert_eq!(choice.get(CursorEnd::Auto), Some(&c5));

        let choice = CursorChoice::Range(c2.clone(), c4.clone(), c5.clone());
        assert_eq!(choice.get(CursorEnd::Keep), None);
        assert_eq!(choice.get(CursorEnd::Start), Some(&c2));
        assert_eq!(choice.get(CursorEnd::End), Some(&c4));
        assert_eq!(choice.get(CursorEnd::Selection), Some(&c4));
        assert_eq!(choice.get(CursorEnd::Auto), Some(&c5));
    }

    #[test]
    fn test_choice_keep() {
        // CursorEnd::Keep always resolves to none.
        let c0 = Cursor::new(0, 0);

        let choice = CursorChoice::Empty;
        assert_eq!(choice.resolve(CursorEnd::Keep), None);

        let choice = CursorChoice::Single(c0.clone());
        assert_eq!(choice.resolve(CursorEnd::Keep), None);

        let choice = CursorChoice::Range(c0.clone(), c0.clone(), c0);
        assert_eq!(choice.resolve(CursorEnd::Keep), None);
    }

    #[test]
    fn test_choice_empty() {
        let ends = vec![
            CursorEnd::Start,
            CursorEnd::End,
            CursorEnd::Selection,
            CursorEnd::Auto,
        ];

        for end in ends.into_iter() {
            // CursorChoice::Empty always resolves to None.
            assert_eq!(CursorChoice::Empty.clone().resolve(end), None);
        }
    }

    #[test]
    fn test_choice_single() {
        let c0 = Cursor::new(0, 1);
        let c3 = Cursor::new(3, 5);
        let cw = TargetShape::CharWise;

        for end in vec![CursorEnd::Start, CursorEnd::End, CursorEnd::Auto] {
            let choice = CursorChoice::Single(c0.clone());
            assert_eq!(choice.resolve(end).unwrap(), CursorState::Location(Cursor::new(0, 1)));

            let choice = CursorChoice::Single(c3.clone());
            assert_eq!(choice.resolve(end).unwrap(), CursorState::Location(Cursor::new(3, 5)));
        }

        let choice = CursorChoice::Single(c0.clone());
        let state = CursorState::Selection(c0.clone(), c0.clone(), cw);
        assert_eq!(choice.resolve(CursorEnd::Selection).unwrap(), state);

        let choice = CursorChoice::Single(c3.clone());
        let state = CursorState::Selection(c3.clone(), c3.clone(), cw);
        assert_eq!(choice.resolve(CursorEnd::Selection).unwrap(), state);
    }

    #[test]
    fn test_choice_range() {
        let c2 = Cursor::new(2, 5);
        let c4 = Cursor::new(4, 3);
        let c5 = Cursor::new(5, 0);
        let cw = TargetShape::CharWise;

        let choice = CursorChoice::Range(c2.clone(), c4.clone(), c5.clone());

        let state = Some(CursorState::Location(c5.clone()));
        assert_eq!(choice.clone().resolve(CursorEnd::Auto), state);

        let state = Some(CursorState::Location(c2.clone()));
        assert_eq!(choice.clone().resolve(CursorEnd::Start), state);

        let state = Some(CursorState::Location(c4.clone()));
        assert_eq!(choice.clone().resolve(CursorEnd::End), state);

        let state = Some(CursorState::Selection(c4, c2, cw));
        assert_eq!(choice.clone().resolve(CursorEnd::Selection), state);
    }
}
