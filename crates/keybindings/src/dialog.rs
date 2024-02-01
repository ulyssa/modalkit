//! # Interactive dialog prompts
//!
//! ## Overview
//!
//! Dialogs allow interacting with the user outside of the normal keybindings. For example, if the
//! user has tried to take an action that requires confirmation, then you can use [PromptYesNo] to
//! show them a message and fetch the `y` or `Y` keypresses.
use std::borrow::Cow;
use std::fmt::Debug;

use textwrap::wrap;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

/// An interactive dialog at the bottom of the screen.
pub trait Dialog<A>: Debug + Send {
    /// Render the lines to show to the user within the available area.
    fn render(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>>;

    /// The user's response to this interactive dialog. The user will be repeatedly
    /// prompted until this returns Some.
    fn input(&mut self, c: char) -> Option<Vec<A>>;
}

/// Interactively prompt the user for "y" or "n"
#[derive(Clone, Debug)]
pub struct PromptYesNo<A: Clone + Debug> {
    res: Vec<A>,
    msg: Cow<'static, str>,
}

impl<A: Debug> PromptYesNo<A>
where
    A: Clone + Debug,
{
    /// Create a new prompt with the given message and resulting actions.
    pub fn new<T>(prompt: T, actions: Vec<A>) -> Self
    where
        T: Into<Cow<'static, str>>,
    {
        PromptYesNo { res: actions, msg: prompt.into() }
    }
}

impl<A> Dialog<A> for PromptYesNo<A>
where
    A: Clone + Debug + Send + 'static,
{
    fn render(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>> {
        if max_rows == 0 {
            return vec![];
        }

        let mut lines = wrap(self.msg.as_ref(), max_cols);

        if let Some(last) = lines.last_mut() {
            last.to_mut().push_str(" (y/N)");
        }

        return lines;
    }

    fn input(&mut self, c: char) -> Option<Vec<A>> {
        match c {
            'y' | 'Y' => Some(self.res.clone()),
            _ => Some(vec![]),
        }
    }
}

fn find_end(s: &str, start: usize, mut rows: usize, width: usize) -> usize {
    let mut idx = 0;
    let mut full = true;
    let mut cols = width;

    if rows == 0 || width == 0 {
        // No room to show the string in, so just show it all as one page that
        // the user can press space to escape from.
        return s.len();
    }

    for (i, grapheme) in UnicodeSegmentation::grapheme_indices(&s[start..], false) {
        idx = i;

        if rows == 0 {
            full = false;
            break;
        }

        if let "\n" | "\r" | "\r\n" = grapheme {
            cols = width;
            rows = rows.saturating_sub(1);
            continue;
        }

        if cols == 0 {
            cols = width;
            rows = rows.saturating_sub(1);
        }

        cols -= UnicodeWidthStr::width(grapheme);
    }

    if full {
        return s.len();
    } else {
        return start + idx;
    }
}

/// Allow the user to interactively page through some text.
#[derive(Clone, Debug)]
pub struct Pager<A: Clone + Debug> {
    text: Cow<'static, str>,
    idx_start: usize,
    idx_end: usize,
    area: (usize, usize),
    res: Vec<A>,
}

impl<A> Pager<A>
where
    A: Clone + Debug,
{
    /// Create a new pager to display within the given boundaries.
    pub fn new<T>(text: T, actions: Vec<A>) -> Self
    where
        T: Into<Cow<'static, str>>,
    {
        let text = text.into();
        let idx_start = 0;
        let idx_end = text.len();

        Pager {
            text,
            idx_start,
            idx_end,
            area: (0, 0),
            res: actions,
        }
    }

    fn next_page(&mut self) -> bool {
        self.idx_start = self.idx_end;
        self.idx_end = self.text.len();

        return self.idx_start == self.idx_end;
    }
}

impl<A> Dialog<A> for Pager<A>
where
    A: Clone + Debug + Send + 'static,
{
    fn render(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>> {
        if max_rows == 0 {
            return vec![];
        }

        let max_rows = max_rows.saturating_sub(1);

        if (max_rows, max_cols) != self.area {
            self.idx_end = find_end(self.text.as_ref(), self.idx_start, max_rows, max_cols);
            self.area = (max_rows, max_cols);
        }

        let s = &self.text[self.idx_start..self.idx_end];
        let options = textwrap::Options::new(max_cols).break_words(true);
        let mut lines = wrap(s.trim_end(), options);
        lines.push("--- Press Space To Continue ---".into());
        lines
    }

    fn input(&mut self, c: char) -> Option<Vec<A>> {
        if c == ' ' {
            if self.next_page() {
                return Some(self.res.clone());
            } else {
                return None;
            }
        } else {
            return None;
        }
    }
}

/// One of the choices for a [MultiChoice] prompt.
#[derive(Clone, Debug)]
pub struct MultiChoiceItem<A: Clone + Debug> {
    choice: char,
    text: Cow<'static, str>,
    actions: Vec<A>,
}

impl<A> MultiChoiceItem<A>
where
    A: Clone + Debug,
{
    /// Create a new choice.
    pub fn new<T>(choice: char, text: T, actions: Vec<A>) -> Self
    where
        T: Into<Cow<'static, str>>,
    {
        let text = text.into();

        MultiChoiceItem { text, choice, actions }
    }
}

/// A prompt that has multiple choices resulting in different actions.
#[derive(Clone, Debug)]
pub struct MultiChoice<A: Clone + Debug> {
    choices: Vec<MultiChoiceItem<A>>,
    idx_start: usize,
    idx_end: usize,
    area: (usize, usize),
}

impl<A> MultiChoice<A>
where
    A: Clone + Debug,
{
    /// Create a new prompt with multiple choices.
    pub fn new(choices: Vec<MultiChoiceItem<A>>) -> Self {
        MultiChoice {
            idx_start: 0,
            idx_end: choices.len(),
            area: (0, 0),
            choices,
        }
    }

    fn next_page(&mut self) -> bool {
        self.idx_start = self.idx_end;
        self.idx_end = self.choices.len();

        return self.idx_start == self.idx_end;
    }
}

impl<A> Dialog<A> for MultiChoice<A>
where
    A: Clone + Debug + Send + 'static,
{
    fn render(&mut self, max_rows: usize, max_cols: usize) -> Vec<Cow<'_, str>> {
        if max_rows == 0 {
            return vec![];
        }

        let max_rows = max_rows.saturating_sub(1);

        if (max_rows, max_cols) != self.area {
            self.idx_end = self.choices.len().min(self.idx_start + max_rows);
            self.area = (max_rows, max_cols);
        }

        let mut lines = self.choices[self.idx_start..self.idx_end]
            .iter()
            .map(|c| format!("({}) {}", c.choice, c.text))
            .map(Cow::Owned)
            .collect::<Vec<_>>();
        lines.push("--- Select A Choice Or Press Space To Continue ---".into());

        return lines;
    }

    fn input(&mut self, c: char) -> Option<Vec<A>> {
        if c == ' ' {
            if self.next_page() {
                return Some(vec![]);
            } else {
                return None;
            }
        }

        for item in self.choices.iter() {
            if item.choice == c {
                return Some(item.actions.clone());
            }
        }

        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yes_no() {
        let mut dialog = PromptYesNo::new("Are you sure?", vec![1, 2, 3]);

        let lines = dialog.render(1, 100);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].as_ref(), "Are you sure? (y/N)");

        // Lower- and uppercase 'y' return actions.
        assert_eq!(dialog.input('y'), Some(vec![1, 2, 3]));
        assert_eq!(dialog.input('Y'), Some(vec![1, 2, 3]));

        // Lower- and uppercase 'n' don't return actions.
        assert_eq!(dialog.input('n'), Some(vec![]));
        assert_eq!(dialog.input('N'), Some(vec![]));

        // Defaults to not returning an action.
        assert_eq!(dialog.input('q'), Some(vec![]));
    }

    #[test]
    fn test_pager() {
        let mut dialog =
            Pager::new("This is Line 1\nThis is Line 2\nThis is Line 3\nThis is Line 4", vec![5]);

        // Only returns lines that we have room for.
        let lines = dialog.render(3, 15);
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].as_ref(), "This is Line 1");
        assert_eq!(lines[1].as_ref(), "This is Line 2");
        assert_eq!(lines[2].as_ref(), "--- Press Space To Continue ---");

        // Wraps the original text (but not press space message).
        let lines = dialog.render(3, 10);
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].as_ref(), "This is");
        assert_eq!(lines[1].as_ref(), "Line 1");
        assert_eq!(lines[2].as_ref(), "--- Press Space To Continue ---");

        // We can give the pager more space, and it resizes.
        let lines = dialog.render(5, 10);
        assert_eq!(lines.len(), 5);
        assert_eq!(lines[0].as_ref(), "This is");
        assert_eq!(lines[1].as_ref(), "Line 1");
        assert_eq!(lines[2].as_ref(), "This is");
        assert_eq!(lines[3].as_ref(), "Line 2");
        assert_eq!(lines[4].as_ref(), "--- Press Space To Continue ---");

        // Press space bar.
        let res = dialog.input(' ');
        assert_eq!(res, None);

        // Now renders the second page.
        let lines = dialog.render(5, 10);
        assert_eq!(lines.len(), 5);
        assert_eq!(lines[0].as_ref(), "This is");
        assert_eq!(lines[1].as_ref(), "Line 3");
        assert_eq!(lines[2].as_ref(), "This is");
        assert_eq!(lines[3].as_ref(), "Line 4");
        assert_eq!(lines[4].as_ref(), "--- Press Space To Continue ---");

        // Press space bar again, and we're done.
        let res = dialog.input(' ');
        assert_eq!(res, Some(vec![5]));
    }

    #[test]
    fn test_multi_choice() {
        let choice1 = MultiChoiceItem::new('a', "Choice A", vec![0, 1]);
        let choice2 = MultiChoiceItem::new('q', "Choice Q", vec![2]);
        let choice3 = MultiChoiceItem::new('5', "Choice 5", vec![3]);
        let choices = vec![choice1, choice2, choice3];
        let mut dialog = MultiChoice::new(choices.clone());

        // Only returns lines that we have room for.
        let lines = dialog.render(2, 15);
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].as_ref(), "(a) Choice A");
        assert_eq!(lines[1].as_ref(), "--- Select A Choice Or Press Space To Continue ---");

        // Increase visible lines.
        let lines = dialog.render(3, 15);
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].as_ref(), "(a) Choice A");
        assert_eq!(lines[1].as_ref(), "(q) Choice Q");
        assert_eq!(lines[2].as_ref(), "--- Select A Choice Or Press Space To Continue ---");

        // Press Space.
        assert_eq!(dialog.input(' '), None);

        // Next page renders.
        let lines = dialog.render(3, 15);
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].as_ref(), "(5) Choice 5");
        assert_eq!(lines[1].as_ref(), "--- Select A Choice Or Press Space To Continue ---");

        // Select choice from previous page.
        assert_eq!(dialog.input('q'), Some(vec![2]));

        // Restart and select a different choice.
        let mut dialog = MultiChoice::new(choices.clone());
        assert_eq!(dialog.input('a'), Some(vec![0, 1]));

        // Restart and select a different choice.
        let mut dialog = MultiChoice::new(choices.clone());
        assert_eq!(dialog.input('5'), Some(vec![3]));
    }
}
