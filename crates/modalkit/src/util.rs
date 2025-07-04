use std::borrow::Borrow;
use std::cmp::Ordering;
use std::ops::Bound;

use radix_trie::{SubTrie, Trie, TrieCommon, TrieKey};
use unicode_segmentation::UnicodeSegmentation;

use crate::{key::TerminalKey, prelude::MoveDir1D};

#[allow(unused)]
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

#[allow(unused_macros)]
macro_rules! key_event {
    ($ch: literal) => {
        KeyEvent::new(KeyCode::Char($ch), match $ch.is_ascii_uppercase() {
            true => crossterm::event::KeyModifiers::SHIFT,
            false => crossterm::event::KeyModifiers::NONE,
        })
    };
    ($kc: expr) => {
        KeyEvent::new($kc, crossterm::event::KeyModifiers::NONE)
    };
    ($kc: literal, $km: expr) => {
        KeyEvent::new(KeyCode::Char($kc), $km)
    };
    ($kc: expr, $km: expr) => {
        KeyEvent::new($kc, $km)
    };
}

#[allow(unused_macros)]
macro_rules! key {
    ($ch: literal) => {
        TerminalKey::from(key_event!($ch))
    };
    ($kc: expr) => {
        TerminalKey::from(key_event!($kc))
    };
    ($kc: literal, $km: expr) => {
        TerminalKey::from(key_event!($kc, $km))
    };
    ($kc: expr, $km: expr) => {
        TerminalKey::from(key_event!($kc, $km))
    };
}

#[allow(unused_macros)]
macro_rules! ctl {
    ($ch: literal) => {
        key!(KeyCode::Char($ch.to_ascii_lowercase()), KeyModifiers::CONTROL)
    };
}

#[allow(unused_macros)]
macro_rules! assert_pop1 {
    ($mm: expr, $act: expr, $ctx: expr) => {
        assert_eq!($mm.pop(), Some(($act.clone(), EditContext::from($ctx.clone()))));
    };
}

#[allow(unused_macros)]
macro_rules! assert_pop2 {
    ($mm: expr, $act: expr, $ctx: expr) => {
        assert_pop1!($mm, $act, $ctx);
        assert_eq!($mm.pop(), None);
    };
}

macro_rules! strs {
    ( $( $ss: expr ),* ) => {
        vec![ $( String::from($ss), )* ]
    };
}

#[derive(Debug, Default)]
pub(crate) struct IdGenerator {
    next_id: u64,
}

impl IdGenerator {
    pub fn next(&mut self) -> u64 {
        let id = self.next_id;

        if self.next_id == u64::MAX {
            panic!("no more node IDs available!");
        }

        self.next_id += 1;

        return id;
    }
}

/// Internal upper limit on number of completions to return.
pub(crate) const MAX_COMPLETIONS: usize = 500;

#[inline]
pub(crate) fn subtrie_keys<K, V>(subtrie: SubTrie<K, V>) -> Vec<K>
where
    K: Clone + TrieKey,
{
    subtrie.keys().take(MAX_COMPLETIONS).cloned().collect()
}

#[inline]
pub(crate) fn completion_keys<K, V>(trie: &Trie<K, V>, prefix: &str) -> Vec<K>
where
    K: Borrow<str> + Clone + TrieKey,
{
    trie.get_raw_descendant(prefix).map(subtrie_keys).unwrap_or_default()
}

pub(crate) fn common_prefix<'a>(a: &'a str, b: &str) -> &'a str {
    if a == b {
        return a;
    }

    let mut idx = 0;
    let itera = UnicodeSegmentation::grapheme_indices(a, false);
    let iterb = UnicodeSegmentation::graphemes(b, false);

    for ((i, ga), gb) in itera.zip(iterb) {
        idx = i;

        if ga != gb {
            break;
        }
    }

    return &a[..idx];
}

/// Utility function for calculating bounded and possibly wrapped offsets.
pub fn idx_offset(
    index: usize,
    offset: usize,
    dir: &MoveDir1D,
    modulus: usize,
    wrap: bool,
) -> Option<usize> {
    if modulus == 0 {
        return None;
    }

    match (dir, wrap) {
        (MoveDir1D::Next, true) => {
            let offset = offset % modulus;
            let new = (index + offset) % modulus;

            Some(new)
        },
        (MoveDir1D::Previous, true) => {
            let offset = offset % modulus;
            let new = (modulus + index - offset) % modulus;

            Some(new)
        },
        (MoveDir1D::Next, false) => {
            let new = index.saturating_add(offset);

            if new >= modulus {
                None
            } else {
                Some(new)
            }
        },
        (MoveDir1D::Previous, false) => {
            if offset > index {
                None
            } else {
                Some(index - offset)
            }
        },
    }
}

/// Move the element currently located at index `fidx` to be located before or after the element
/// currently located at index `tidx`, as determined by `side`.
///
/// `fidx` will be updated with the new index of the element after it has been moved.
///
/// `idx_last` represents an index to another tracked element, and will be updated to make sure
/// that it continues to point at the same element.
pub fn idx_move<T>(
    els: &mut Vec<T>,
    fidx: &mut usize,
    tidx: usize,
    idx_last: &mut usize,
    side: MoveDir1D,
) {
    match side {
        MoveDir1D::Next => idx_move_after(els, fidx, tidx, idx_last),
        MoveDir1D::Previous => idx_move_before(els, fidx, tidx, idx_last),
    }
}

fn idx_move_after<T>(els: &mut Vec<T>, fidx: &mut usize, tidx: usize, idx_last: &mut usize) {
    idx_move_before(els, fidx, tidx + 1, idx_last)
}

fn idx_move_before<T>(els: &mut Vec<T>, fidx: &mut usize, tidx: usize, idx_last: &mut usize) {
    let nidx = match tidx.cmp(fidx) {
        Ordering::Less => {
            let tab = els.remove(*fidx);
            els.insert(tidx, tab);

            tidx
        },
        Ordering::Equal => {
            // Do nothing.
            return;
        },
        Ordering::Greater => {
            let tab = els.remove(*fidx);
            let tidx = tidx - 1;
            els.insert(tidx, tab);

            tidx
        },
    };

    match idx_last.cmp(&fidx) {
        Ordering::Less => {
            if *idx_last >= tidx {
                // tabidx moved from after idx_last to before it.
                *idx_last += 1;
            }
        },
        Ordering::Equal => {
            // fidx and idx_last are the same, and should remain so.
            *idx_last = nidx;
        },
        Ordering::Greater => {
            if *idx_last < tidx {
                // fidx move from before idx_last to after it.
                *idx_last -= 1;
            }
        },
    }

    *fidx = nidx;
}

#[inline]
pub fn keycode_to_num(ke: &TerminalKey, radix: u32) -> Option<u32> {
    if let Some((c, _)) = ke.get_char_mods() {
        c.to_digit(radix)
    } else {
        None
    }
}

#[inline]
pub fn option_muladd_u32(opt: &Option<u32>, mul: u32, add: u32) -> u32 {
    opt.unwrap_or(0).saturating_mul(mul).saturating_add(add)
}

#[inline]
pub fn option_muladd_usize(opt: &Option<usize>, mul: usize, add: usize) -> usize {
    opt.unwrap_or(0).saturating_mul(mul).saturating_add(add)
}

#[inline]
pub fn sort2<T>(a: T, b: T) -> (T, T)
where
    T: Ord,
{
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}

pub fn into_range<T>(start: T, end: T, inclusive: bool) -> (Bound<T>, Bound<T>) {
    if inclusive {
        (Bound::Included(start), Bound::Included(end))
    } else {
        (Bound::Included(start), Bound::Excluded(end))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_idx_move_before_equal() {
        let mut idx;
        let mut idxlast;
        let mut v = vec!['a', 'b', 'c', 'd', 'e'];

        // Move element to later in the list when idx == idxlast.
        idx = 0;
        idxlast = 0;
        idx_move_before(&mut v, &mut idx, 3, &mut idxlast);

        assert_eq!(idx, 2);
        assert_eq!(idxlast, 2);
        assert_eq!(v, vec!['b', 'c', 'a', 'd', 'e']);

        // Move element to earlier in the list when idx == idxlast.
        idx = 3;
        idxlast = 3;
        idx_move_before(&mut v, &mut idx, 0, &mut idxlast);

        assert_eq!(idx, 0);
        assert_eq!(idxlast, 0);
        assert_eq!(v, vec!['d', 'b', 'c', 'a', 'e']);
    }

    #[test]
    fn test_idx_move_before_cross() {
        let mut idx;
        let mut idxlast;
        let mut v = vec!['a', 'b', 'c', 'd', 'e'];

        // Move element to later place in the list, crossing over idxlast.
        idx = 0;
        idxlast = 1;
        idx_move_before(&mut v, &mut idx, 3, &mut idxlast);

        assert_eq!(idx, 2);
        assert_eq!(idxlast, 0);
        assert_eq!(v, vec!['b', 'c', 'a', 'd', 'e']);

        // Move element to earlier place in the list, crossing over idxlast.
        idx = 3;
        idxlast = 1;
        idx_move_before(&mut v, &mut idx, 0, &mut idxlast);

        assert_eq!(idx, 0);
        assert_eq!(idxlast, 2);
        assert_eq!(v, vec!['d', 'b', 'c', 'a', 'e']);

        // Move element to earlier place in the list, so that it's just before idxlast.
        idx = 3;
        idxlast = 1;
        idx_move_before(&mut v, &mut idx, 1, &mut idxlast);

        assert_eq!(idx, 1);
        assert_eq!(idxlast, 2);
        assert_eq!(v, vec!['d', 'a', 'b', 'c', 'e']);
    }

    #[test]
    fn test_idx_move_before_no_cross() {
        let mut idx;
        let mut idxlast;
        let mut v = vec!['a', 'b', 'c', 'd', 'e'];

        // Move element to later place in the list, when idxlast is just before affected range.
        idx = 2;
        idxlast = 1;
        idx_move_before(&mut v, &mut idx, 4, &mut idxlast);

        assert_eq!(idx, 3);
        assert_eq!(idxlast, 1);
        assert_eq!(v, vec!['a', 'b', 'd', 'c', 'e']);

        // Move element to earlier place in the list, when idxlast is just before affected range.
        idx = 3;
        idxlast = 1;
        idx_move_before(&mut v, &mut idx, 2, &mut idxlast);

        assert_eq!(idx, 2);
        assert_eq!(idxlast, 1);
        assert_eq!(v, vec!['a', 'b', 'c', 'd', 'e']);

        // Move element to later in the list, so that it's before idxlast.
        idx = 0;
        idxlast = 3;
        idx_move_before(&mut v, &mut idx, 3, &mut idxlast);

        assert_eq!(idx, 2);
        assert_eq!(idxlast, 3);
        assert_eq!(v, vec!['b', 'c', 'a', 'd', 'e']);
    }
}
