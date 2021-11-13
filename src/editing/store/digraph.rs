use std::collections::HashMap;
use std::iter::FromIterator;

macro_rules! digraphs {
    { $( $c: literal <- $d1: literal + $d2: literal ),* } => {
        [ $( (($d1, $d2), $c), )* ]
    };
}

const RFC1345_DIGRAPHS: [((char, char), char); 1300] = include!("rfc1345.digraphs");

const VIM_DIGRAPHS: [((char, char), char); 62] = include!("vim.digraphs");

pub struct DigraphStore {
    mappings: HashMap<(char, char), char>,
}

impl DigraphStore {
    pub fn new() -> Self {
        DigraphStore { mappings: HashMap::new() }
    }

    /// Create a new instance initialized with the digraphs specified in
    /// [RFC1345](https://datatracker.ietf.org/doc/html/rfc1345).
    pub fn rfc1345() -> Self {
        RFC1345_DIGRAPHS.iter().collect()
    }

    /// Create a new instance initialized with the digraphs used in Vim, which
    /// uses the [RFC1345](https://datatracker.ietf.org/doc/html/rfc1345) digraphs plus some of its
    /// own.
    pub fn vim() -> Self {
        let mut store = DigraphStore::rfc1345();

        for ((d1, d2), c) in VIM_DIGRAPHS {
            store.put((d1, d2), c);
        }

        return store;
    }

    pub fn put(&mut self, digraph: (char, char), c: char) {
        self.mappings.insert(digraph, c);
    }

    pub fn get(&self, digraph: (char, char)) -> Option<char> {
        self.mappings.get(&digraph).copied()
    }
}

impl FromIterator<((char, char), char)> for DigraphStore {
    fn from_iter<T: IntoIterator<Item = ((char, char), char)>>(digraphs: T) -> Self {
        let mut store = DigraphStore::new();

        for (digraph, c) in digraphs {
            store.put(digraph, c);
        }

        return store;
    }
}

impl<'a> FromIterator<&'a ((char, char), char)> for DigraphStore {
    fn from_iter<T: IntoIterator<Item = &'a ((char, char), char)>>(digraphs: T) -> Self {
        let mut store = DigraphStore::new();

        for ((d1, d2), c) in digraphs {
            store.put((*d1, *d2), *c);
        }

        return store;
    }
}

impl Default for DigraphStore {
    fn default() -> Self {
        Self::vim()
    }
}
