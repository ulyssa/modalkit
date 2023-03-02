use crate::editing::completion::LineCompleter;

/// Storage for text completion.
#[derive(Default)]
pub struct CompletionStore {
    /// Global collection of word completion candidates.
    pub lines: LineCompleter,
}

impl CompletionStore {
    /// Complete the given line.
    pub fn complete_line(&self, prefix: &str) -> Vec<String> {
        self.lines.complete_line(prefix)
    }

    /// Complete the given word.
    pub fn complete_word(&self, prefix: &str) -> Vec<String> {
        self.lines.complete_word(prefix)
    }
}
