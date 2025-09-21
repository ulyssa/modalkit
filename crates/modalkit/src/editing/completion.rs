//! # Text completion
//!
//! ## Overview
//!
//! This module contains the types and code needed for completing text.
use std::borrow::{Borrow, Cow};
use std::ffi::OsStr;
use std::fs::DirEntry;
use std::path::{Path, MAIN_SEPARATOR};
use std::sync::Arc;

use radix_trie::{Trie, TrieCommon, TrieKey};
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    editing::{
        application::ApplicationInfo,
        cursor::{Adjustable, Cursor, CursorAdjustment},
        rope::EditRope,
    },
    prelude::*,
    util::{common_prefix, completion_keys, idx_offset, MAX_COMPLETIONS},
};

/// A trait for implementing custom completers for an application.
pub trait Completer<I: ApplicationInfo>: Send + Sync {
    /// Complete the word just before [Cursor] inside the [EditRope].
    ///
    /// When this method returns, the [Cursor] should be updated to point at the
    /// beginning of the word, so that the resulting range may be replaced by the
    /// possible completions returned.
    ///
    /// The [ApplicationInfo::ContentId] will be the application's identifier for
    /// the buffer where the completion is happening.
    fn complete(
        &mut self,
        text: &EditRope,
        cursor: &mut Cursor,
        content: &I::ContentId,
        store: &mut I::Store,
    ) -> Vec<String>;
}

/// A basic implementation of [Completer] that never returns anything.
#[derive(Default)]
pub struct EmptyCompleter;

impl<I: ApplicationInfo> Completer<I> for EmptyCompleter {
    fn complete(
        &mut self,
        _: &EditRope,
        _: &mut Cursor,
        _: &I::ContentId,
        _: &mut I::Store,
    ) -> Vec<String> {
        vec![]
    }
}

/// List of text completion candidates to show to the user.
#[derive(Clone)]
pub struct CompletionList {
    /// The original prefix being completed.
    pub prefix: String,

    /// Candidates for text completion.
    pub candidates: Arc<Vec<String>>,

    /// Currently selected index in `candidates`.
    pub selected: Option<usize>,

    /// How to display the list of candidates.
    pub display: CompletionDisplay,

    /// The cursor's current position after the word.
    pub cursor: Cursor,

    /// The start of the text being completed.
    pub start: Cursor,
}

impl CompletionList {
    pub(crate) fn replace(&self, rope: &mut EditRope, val: String) -> Vec<CursorAdjustment> {
        let so = rope.cursor_to_offset(&self.start);
        let eo = rope.cursor_to_offset(&self.cursor);
        rope.replace(so..eo, EditRope::from(val)).1
    }

    /// Select a different candidate from the completion list.
    pub fn select(&mut self, selection: &CompletionStyle) -> Option<String> {
        match selection {
            CompletionStyle::List(dir) => {
                let len = self.candidates.len();
                let max = len.saturating_sub(1);
                let idx = match (self.selected, dir) {
                    (None, MoveDir1D::Next) => max,
                    (None, MoveDir1D::Previous) => 0,
                    (Some(idx), MoveDir1D::Next) if idx == max => {
                        self.selected = None;
                        return Some(self.prefix.clone());
                    },
                    (Some(0), MoveDir1D::Previous) => {
                        self.selected = None;
                        return Some(self.prefix.clone());
                    },
                    (Some(idx), _) => idx,
                };

                self.selected = idx_offset(idx, 1, dir, len, true);
                self.selected.map(|i| self.candidates[i].clone())
            },
            CompletionStyle::None => {
                return None;
            },
            CompletionStyle::Prefix => {
                let mut prefix = None;

                for candidate in self.candidates.iter() {
                    if let Some(p) = prefix {
                        let common = common_prefix(p, candidate.as_str());

                        if common.is_empty() {
                            return None;
                        } else {
                            prefix = Some(common);
                        }
                    } else {
                        prefix = Some(candidate.as_str());
                    }
                }

                return prefix.map(ToString::to_string);
            },
            CompletionStyle::Single => {
                if self.candidates.len() == 1 {
                    return Some(self.candidates[0].clone());
                } else {
                    return None;
                }
            },
        }
    }
}

impl Adjustable for CompletionList {
    fn adjust(&mut self, adjs: &[CursorAdjustment]) {
        self.cursor.adjust(adjs);
    }

    fn zero(&mut self) {
        self.start.zero();
        self.cursor.zero();
    }
}

#[derive(Clone, Eq, PartialEq)]
struct StrLike<K: AsRef<str> + Clone + Eq>(K);

impl<K> Borrow<str> for StrLike<K>
where
    K: AsRef<str> + Clone + Eq,
{
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl<K> TrieKey for StrLike<K>
where
    K: AsRef<str> + Clone + Eq,
{
    fn encode_bytes(&self) -> Vec<u8> {
        str::encode_bytes(self.borrow())
    }
}

/// Maps keys onto values with support for listing keys by prefix.
pub struct CompletionMap<K, V>
where
    K: AsRef<str> + Clone + Eq,
{
    trie: Trie<StrLike<K>, V>,
}

impl<K, V> CompletionMap<K, V>
where
    K: AsRef<str> + Clone + Eq,
{
    /// Whether this completer contains zero completions.
    pub fn is_empty(&self) -> bool {
        self.trie.is_empty()
    }

    /// Map the given key onto the given value.
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.trie.insert(StrLike(k), v)
    }

    /// Unmap the given key, returning its previous value if one existed.
    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        Q: AsRef<str> + ?Sized,
    {
        self.trie.remove(k.as_ref())
    }

    /// Get an immutable reference to a value if the key exists.
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        Q: AsRef<str> + ?Sized,
    {
        self.trie.get(k.as_ref())
    }

    /// Get a mutable reference to a value if the key exists.
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        Q: AsRef<str> + ?Sized,
    {
        self.trie.get_mut(k.as_ref())
    }

    /// Get a mutable reference to a value for the given key.
    ///
    /// If the key is unmapped, then this will insert a [Default] value first.
    pub fn get_or_default(&mut self, k: K) -> &mut V
    where
        V: Default,
    {
        if self.trie.get(k.as_ref()).is_none() {
            self.trie.insert(StrLike(k.clone()), V::default());
        }

        self.trie
            .get_mut(k.as_ref())
            .expect("default value should have been inserted")
    }

    /// Get a list of keys that share a common prefix.
    pub fn complete(&self, prefix: &str) -> Vec<K> {
        completion_keys(&self.trie, prefix).into_iter().map(|k| k.0).collect()
    }

    /// Returns an iterator over the entries
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.trie.iter().map(|(k, v)| (&k.0, v))
    }
}

impl<K, V> Default for CompletionMap<K, V>
where
    K: AsRef<str> + Clone + Eq,
{
    fn default() -> Self {
        CompletionMap { trie: Trie::new() }
    }
}

/// Tracks words and handles completions for them.
///
/// Words are reference-counted to make it easy to forget them once every occurrence has been
/// removed from a source.
#[derive(Default)]
pub struct WordCompleter {
    map: CompletionMap<String, usize>,
}

impl WordCompleter {
    /// Whether this completer contains zero completions.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Decrement the reference count for a `word`.
    pub fn word_decr(&mut self, word: &str) {
        if let Some(count) = self.map.get_mut(word) {
            *count -= 1;

            if *count == 0 {
                self.map.remove(word);
            }
        }
    }

    /// Increment the reference count for a `word`.
    pub fn word_incr(&mut self, word: &str) {
        if let Some(count) = self.map.get_mut(word) {
            *count += 1;
        } else {
            self.map.insert(word.to_string(), 1);
        }
    }

    /// Get the completions for the given word prefix.
    pub fn complete_word(&self, prefix: &str) -> Vec<String> {
        self.map.complete(prefix)
    }
}

/// Tracks lines and handles completions for them and their contained words.
///
/// Lines are reference-counted to make it easy to forget them once every occurrence has been
/// removed from a source.
#[derive(Default)]
pub struct LineCompleter {
    map: CompletionMap<String, usize>,
    words: WordCompleter,
}

impl LineCompleter {
    /// Whether this completer contains zero completions.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Decrement the reference count for a given `line` of text.
    pub fn line_decr(&mut self, line: &str) {
        if let Some(count) = self.map.get_mut(line) {
            *count -= 1;

            if *count == 0 {
                self.map.remove(line);
            }

            for word in UnicodeSegmentation::unicode_words(line) {
                self.words.word_decr(word);
            }
        }
    }

    /// Increment the reference count for a given `line` of text.
    pub fn line_incr(&mut self, line: &str) {
        if line.is_empty() {
            // Don't complete empty lines.
            return;
        }

        if let Some(count) = self.map.get_mut(line) {
            *count += 1;
        } else {
            self.map.insert(line.to_string(), 1);
        }

        for word in UnicodeSegmentation::unicode_words(line) {
            self.words.word_incr(word);
        }
    }

    /// Get the completions for the given line prefix.
    pub fn complete_line(&self, prefix: &str) -> Vec<String> {
        self.map.complete(prefix)
    }

    /// Get the completions for the given word prefix.
    pub fn complete_word(&self, prefix: &str) -> Vec<String> {
        self.words.complete_word(prefix)
    }
}

/// Complete filenames within a path leading up to the cursor.
pub fn complete_path(input: &EditRope, cursor: &mut Cursor) -> Vec<String> {
    let filepath = input.get_prefix_word(cursor, &WordStyle::FilePath);
    let filepath = filepath.unwrap_or_else(EditRope::empty);
    let filepath = Cow::from(&filepath);

    let mut res = Vec::<String>::with_capacity(MAX_COMPLETIONS);
    let path = Path::new(filepath.as_ref());

    if filepath.as_ref().ends_with(MAIN_SEPARATOR) {
        if let Ok(dir) = path.read_dir() {
            let filter = |entry: DirEntry| {
                let name = entry.file_name();
                let name = name.to_string_lossy();

                if name.starts_with('.') {
                    return None;
                } else {
                    return Some(name.to_string());
                }
            };

            res.extend(dir.flatten().flat_map(filter).take(MAX_COMPLETIONS));
        }
    } else if filepath.as_ref() == "." || filepath.as_ref().ends_with([MAIN_SEPARATOR, '.']) {
        // The .parent() and .file_name() methods treat . especially, so we
        // have to special-case completion of hidden files here.
        let _ = input.get_prefix_word_mut(cursor, &WordStyle::FileName);

        if let Ok(dir) = path.read_dir() {
            let filter = |entry: DirEntry| {
                let name = entry.file_name();
                let name = name.to_string_lossy();

                if name.starts_with('.') {
                    return Some(name.to_string());
                } else {
                    return None;
                }
            };

            res.extend(dir.flatten().flat_map(filter).take(MAX_COMPLETIONS));
        }
    } else {
        let prefix = path.file_name().map(OsStr::to_string_lossy).unwrap_or(Cow::Borrowed(""));
        let _ = input.get_prefix_word_mut(cursor, &WordStyle::FileName);

        let dir = path
            .parent()
            .filter(|p| !p.as_os_str().is_empty())
            .and_then(|p| p.read_dir().ok())
            .or_else(|| std::env::current_dir().and_then(|cwd| cwd.read_dir()).ok());

        if let Some(dir) = dir {
            let filter = |entry: DirEntry| {
                let name = entry.file_name();
                let name = name.to_string_lossy();

                if prefix.is_empty() && name.starts_with('.') {
                    return None;
                } else if name.starts_with(prefix.as_ref()) {
                    return Some(name.to_string());
                } else {
                    return None;
                }
            };

            res.extend(dir.flatten().flat_map(filter).take(MAX_COMPLETIONS));
        }
    }

    res.sort();

    return res;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mkmap() -> CompletionMap<String, usize> {
        let mut map = CompletionMap::default();

        map.insert("press".into(), 1);
        map.insert("pressed".into(), 1);
        map.insert("presses".into(), 1);
        map.insert("pressing".into(), 1);
        map.insert("pressings".into(), 1);
        map.insert("pressman".into(), 1);
        map.insert("pressmen".into(), 1);
        map.insert("pressure".into(), 1);
        map.insert("pressured".into(), 1);
        map.insert("pressures".into(), 1);
        map.insert("pressuring".into(), 1);
        map.insert("pressurization".into(), 1);
        map.insert("pressurize".into(), 1);
        map.insert("pressurized".into(), 1);
        map.insert("pressurizes".into(), 1);
        map.insert("pressurizing".into(), 1);

        return map;
    }

    #[test]
    fn test_map_complete() {
        let map = mkmap();

        let res = map.complete("");
        assert_eq!(res, strs![
            "press",
            "pressed",
            "presses",
            "pressing",
            "pressings",
            "pressman",
            "pressmen",
            "pressure",
            "pressured",
            "pressures",
            "pressuring",
            "pressurization",
            "pressurize",
            "pressurized",
            "pressurizes",
            "pressurizing"
        ]);

        let res = map.complete("presse");
        assert_eq!(res, strs!["pressed", "presses"]);

        let res = map.complete("pressur");
        assert_eq!(res, strs![
            "pressure",
            "pressured",
            "pressures",
            "pressuring",
            "pressurization",
            "pressurize",
            "pressurized",
            "pressurizes",
            "pressurizing"
        ]);
    }

    #[test]
    fn test_line_incr_decr() {
        let mut completer = LineCompleter::default();

        // Increment and decrement two different lines, so that only one is left.
        completer.line_incr("foo bar baz");
        completer.line_incr("foo bar baz");
        completer.line_incr("foo bar quux");
        completer.line_decr("foo bar baz");
        completer.line_incr("foo bar baz");
        completer.line_decr("foo bar quux");
        completer.line_decr("foo bar baz");

        let res = completer.complete_line("foo bar");
        assert_eq!(res, vec!["foo bar baz"]);

        let res = completer.complete_line("foo bar q");
        assert_eq!(res, Vec::<String>::new());

        let res = completer.complete_word("f");
        assert_eq!(res, vec!["foo"]);

        let res = completer.complete_word("b");
        assert_eq!(res, vec!["bar", "baz"]);

        let res = completer.complete_word("q");
        assert_eq!(res, Vec::<String>::new());
    }

    #[test]
    fn test_line_complete() {
        let mut completer = LineCompleter::default();

        completer.line_incr("foo bar baz");
        completer.line_incr("foo bar quux");
        completer.line_incr("foo baz");

        let res = completer.complete_line("f");
        assert_eq!(res, vec!["foo bar baz", "foo bar quux", "foo baz"]);

        let res = completer.complete_line("foo bar");
        assert_eq!(res, vec!["foo bar baz", "foo bar quux"]);

        let res = completer.complete_line("foo bar q");
        assert_eq!(res, vec!["foo bar quux"]);
    }
}
