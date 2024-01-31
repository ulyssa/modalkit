//! This code is adapated to work w/ ropey from the xi_rope library:
//!
//!   <https://github.com/xi-editor/xi-editor/blob/v0.3.0/rust/rope/src/diff.rs>
//!
//! The xi_rope code is licensed under Apache-2.0, the same as modalkit:
//!
//!   <https://github.com/xi-editor/xi-editor/blob/v0.3.0/LICENSE>
//!
use std::borrow::Cow;
use std::collections::HashMap;

use ropey::{Rope, RopeSlice};

use super::trimnl;

/// The minimum length of non-whitespace characters in a line before
/// we consider it for diffing purposes.
const MIN_SIZE: usize = 32;

#[derive(Clone, Debug)]
pub enum DeltaElement<'a> {
    /// Represents a range of text in the base document. Includes beginning, excludes end.
    Copy(usize, usize), // note: for now, we lose open/closed info at interval endpoints
    Insert(RopeSlice<'a>),
}

#[derive(Clone, Debug)]
pub struct Delta<'a> {
    pub els: Vec<DeltaElement<'a>>,
    pub base_len: usize,
}

fn find_ne_char_back(
    base: &Rope,
    base_off: usize,
    target: &Rope,
    targ_off: usize,
    stop: Option<usize>,
) -> usize {
    let stop = stop.unwrap_or(usize::MAX);
    let mut start = 0;

    while base_off >= start && targ_off >= start {
        let base_idx = base_off - start;
        let targ_idx = targ_off - start;

        if start >= stop {
            return start;
        }

        let base_char = base.get_char(base_idx);
        let targ_char = target.get_char(targ_idx);

        if let (Some(c1), Some(c2)) = (base_char, targ_char) {
            if c1 == c2 {
                start += 1;
                continue;
            }
        }

        break;
    }

    return start;
}

fn find_ne_char(base: &Rope, base_off: usize, target: &Rope, targ_off: usize) -> usize {
    let mut end = 0;

    loop {
        let base_idx = base_off + end;
        let targ_idx = targ_off + end;

        if let (Some(c1), Some(c2)) = (base.get_char(base_idx), target.get_char(targ_idx)) {
            if c1 == c2 {
                end += 1;
                continue;
            }
        }

        break;
    }

    return end;
}

fn find_min_diff_range(base: &Rope, target: &Rope) -> (usize, usize) {
    let b_end = base.len_chars();
    let t_end = target.len_chars();
    let start = find_ne_char(base, 0, target, 0);

    let unscanned = b_end.min(t_end) - start;

    let end = match unscanned {
        0 => 0,
        n => {
            let b_idx = b_end.saturating_sub(1);
            let t_idx = t_end.saturating_sub(1);

            find_ne_char_back(base, b_idx, target, t_idx, Some(n))
        },
    };

    return (start, end);
}

/// A line-oriented, hash based diff algorithm.
///
/// This works by taking a hash of each line in either document that
/// has a length, ignoring leading whitespace, above some threshold.
///
/// Lines in the target document are matched against lines in the
/// base document. When a match is found, it is extended forwards
/// and backwards as far as possible.
///
/// This runs in O(n+m) in the lengths of the two ropes, and produces
/// results on a variety of workloads that are comparable in quality
/// (measured in terms of serialized diff size) with the results from
/// using a suffix array, while being an order of magnitude faster.
pub fn compute_delta<'a>(base: &Rope, target: &'a Rope) -> Delta<'a> {
    let mut builder = DiffBuilder::default();

    // before doing anything, scan top down and bottom up for like-ness.
    let (start_offset, diff_end) = find_min_diff_range(base, target);
    let target_end = target.len_chars() - diff_end;

    if start_offset > 0 {
        builder.copy(0, 0, start_offset);
    }

    // if our preliminary scan finds no differences we're done
    if start_offset == base.len_chars() && target.len_chars() == base.len_chars() {
        return builder.into_delta(base, target);
    }

    let line_hashes = make_line_hashes(base, MIN_SIZE);

    let line_count = target.len_lines();
    let mut matches = Vec::with_capacity(line_count);

    let mut targ_line_offset = 0;
    let mut prev_base = 0;

    let mut needs_subseq = false;
    for line in target.slice(start_offset..target_end).lines().map(trimnl) {
        let line = Cow::from(line);
        let non_ws = non_ws_offset(&line);

        if line.len() - non_ws >= MIN_SIZE {
            if let Some(base_off) = line_hashes.get(&line[non_ws..]) {
                let targ_off = targ_line_offset + non_ws;
                matches.push((start_offset + targ_off, *base_off));
                if *base_off < prev_base {
                    needs_subseq = true;
                }
                prev_base = *base_off;
            }
        }

        targ_line_offset += line.len();
    }

    // we now have an ordered list of matches and their positions.
    // to ensure that our delta only copies non-decreasing base regions,
    // we take the longest increasing subsequence.
    // TODO: a possible optimization here would be to expand matches
    // to adjacent lines first? this would be at best a small win though..

    let longest_subseq = if needs_subseq {
        longest_increasing_region_set(&matches)
    } else {
        matches
    };

    // for each matching region, we extend it forwards and backwards.
    // we keep track of how far forward we extend it each time, to avoid
    // having a subsequent scan extend backwards over the same region.
    let mut prev_end = start_offset;

    for (targ_off, base_off) in longest_subseq {
        if targ_off <= prev_end {
            continue;
        }
        let (left_dist, mut right_dist) = expand_match(base, target, base_off, targ_off, prev_end);

        // don't let last match expand past target_end
        right_dist = right_dist.min(target_end - targ_off);

        let targ_start = targ_off - left_dist;
        let base_start = base_off - left_dist;
        let len = left_dist + right_dist;
        prev_end = targ_start + len;

        builder.copy(base_start, targ_start, len);
    }

    if diff_end > 0 {
        builder.copy(base.len_chars() - diff_end, target.len_chars() - diff_end, diff_end);
    }

    builder.into_delta(base, target)
}

/// Given two ropes and the offsets of two equal bytes, finds the largest
/// identical substring shared between the two ropes which contains the offset.
///
/// The return value is a pair of offsets, each of which represents an absolute
/// distance. That is to say, the position of the start and end boundaries
/// relative to the input offset.
fn expand_match(
    base: &Rope,
    target: &Rope,
    base_off: usize,
    targ_off: usize,
    prev_match_targ_end: usize,
) -> (usize, usize) {
    let max_left = targ_off - prev_match_targ_end;
    let start = find_ne_char_back(base, base_off, target, targ_off, Some(max_left));
    let end = find_ne_char(base, base_off, target, targ_off);

    (start.min(max_left), end)
}

/// Finds the longest increasing subset of copyable regions. This is essentially
/// the longest increasing subsequence problem. This implementation is adapted
/// from https://codereview.stackexchange.com/questions/187337/longest-increasing-subsequence-algorithm
fn longest_increasing_region_set(items: &[(usize, usize)]) -> Vec<(usize, usize)> {
    let mut result = vec![0];
    let mut prev_chain = vec![0; items.len()];

    for i in 1..items.len() {
        // If the next item is greater than the last item of the current longest
        // subsequence, push its index at the end of the result and continue.
        let last_idx = *result.last().unwrap();
        if items[last_idx].1 < items[i].1 {
            prev_chain[i] = last_idx;
            result.push(i);
            continue;
        }

        let next_idx = match result.binary_search_by(|&j| items[j].1.cmp(&items[i].1)) {
            Ok(_) => continue, // we ignore duplicates
            Err(idx) => idx,
        };

        if items[i].1 < items[result[next_idx]].1 {
            if next_idx > 0 {
                prev_chain[i] = result[next_idx - 1];
            }
            result[next_idx] = i;
        }
    }

    // walk backwards from the last item in result to build the final sequence
    let mut u = result.len();
    let mut v = *result.last().unwrap();
    while u != 0 {
        u -= 1;
        result[u] = v;
        v = prev_chain[v];
    }
    result.iter().map(|i| items[*i]).collect()
}

#[inline]
fn non_ws_offset(s: &str) -> usize {
    s.as_bytes().iter().take_while(|b| **b == b' ' || **b == b'\t').count()
}

/// Represents copying `len` bytes from base to target.
#[derive(Debug, Clone, Copy)]
struct DiffOp {
    target_idx: usize,
    base_idx: usize,
    len: usize,
}

/// Keeps track of copy ops during diff construction.
#[derive(Debug, Clone, Default)]
pub struct DiffBuilder {
    ops: Vec<DiffOp>,
}

impl DiffBuilder {
    fn copy(&mut self, base: usize, target: usize, len: usize) {
        if let Some(prev) = self.ops.last_mut() {
            let prev_end = prev.target_idx + prev.len;
            let base_end = prev.base_idx + prev.len;
            assert!(prev_end <= target, "{} <= {} prev {:?}", prev_end, target, &prev);
            if prev_end == target && base_end == base {
                prev.len += len;
                return;
            }
        }
        self.ops.push(DiffOp { target_idx: target, base_idx: base, len })
    }

    fn into_delta<'a>(self, base: &Rope, target: &'a Rope) -> Delta<'a> {
        let mut els = Vec::with_capacity(self.ops.len() * 2);
        let mut targ_pos = 0;
        for DiffOp { base_idx, target_idx, len } in self.ops {
            if target_idx > targ_pos {
                let slice = target.slice(targ_pos..target_idx);
                els.push(DeltaElement::Insert(slice));
            }
            els.push(DeltaElement::Copy(base_idx, base_idx + len));
            targ_pos = target_idx + len;
        }

        if targ_pos < target.len_chars() {
            let slice = target.slice(targ_pos..target.len_chars());
            els.push(DeltaElement::Insert(slice));
        }

        Delta { els, base_len: base.len_chars() }
    }
}

/// Creates a map of lines to offsets, ignoring trailing whitespace, and only for those lines
/// where line.len() >= min_size. Offsets refer to the first non-whitespace byte in the line.
fn make_line_hashes(base: &Rope, min_size: usize) -> HashMap<Cow<'_, str>, usize> {
    let mut offset = 0;
    let mut line_hashes = HashMap::with_capacity(base.len_chars() / 60);

    for line in base.lines().map(trimnl) {
        let line = Cow::from(line);
        let non_ws = non_ws_offset(&line);

        if line.len() - non_ws >= min_size {
            let cow = match line {
                Cow::Owned(ref s) => Cow::Owned(s[non_ws..].to_string()),
                Cow::Borrowed(s) => Cow::Borrowed(&s[non_ws..]),
            };
            line_hashes.insert(cow, offset + non_ws);
        }

        offset += line.len();
    }

    line_hashes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ne_char() {
        let r1 = Rope::from("foobar123");
        let r2 = Rope::from("foobar456");
        let r3 = Rope::from("afoobar123");

        // Compare r1 with each string from the start.
        let res = find_ne_char(&r1, 0, &r1, 0);
        assert_eq!(res, 9);

        let res = find_ne_char(&r1, 0, &r2, 0);
        assert_eq!(res, 6);

        let res = find_ne_char(&r1, 0, &r3, 0);
        assert_eq!(res, 0);

        // r1 is the same as r2 when offset by 1.
        let res = find_ne_char(&r1, 0, &r3, 1);
        assert_eq!(res, 9);

        // the first six letters of r2 are the same as r3 when offset by 1.
        let res = find_ne_char(&r2, 0, &r3, 1);
        assert_eq!(res, 6);
    }

    #[test]
    fn test_ne_char_back() {
        let r1 = Rope::from("foobar123");
        let r2 = Rope::from("foobar456");
        let r3 = Rope::from("afoobar123");

        // Compare r1 with each string from the start.
        let res = find_ne_char_back(&r1, 8, &r1, 8, None);
        assert_eq!(res, 9);

        let res = find_ne_char_back(&r1, 8, &r2, 8, None);
        assert_eq!(res, 0);

        let res = find_ne_char_back(&r1, 8, &r3, 9, None);
        assert_eq!(res, 9);

        // r1 is the same as r2 when starting at index 5 ('r').
        let res = find_ne_char_back(&r1, 5, &r2, 5, None);
        assert_eq!(res, 6);

        // the last nine letters of r1 are the same as the end of r3.
        let res = find_ne_char_back(&r1, 8, &r3, 9, None);
        assert_eq!(res, 9);

        // the first six letter of r2 are contained in r3.
        let res = find_ne_char_back(&r2, 5, &r3, 6, None);
        assert_eq!(res, 6);
    }

    #[test]
    fn test_min_diff_range() {
        let r1 = Rope::from("hello world\nhello world\nhello\n");
        let r2 = Rope::from("hello world\nhello world\nhello world\n");

        let res = find_min_diff_range(&r1, &r1);
        assert_eq!(res, (30, 0));

        let res = find_min_diff_range(&r1, &r2);
        assert_eq!(res, (29, 1));

        let r1 = Rope::from("abcdef\n1234 5678\nghijkl\n");
        let r2 = Rope::from("def\n1234 5678\nghijkl\n");

        let res = find_min_diff_range(&r2, &r1);
        assert_eq!(res, (0, 21));
    }
}
