#![doc(hidden)]
use std::path::MAIN_SEPARATOR;

pub fn is_horizontal_space(c: char) -> bool {
    return c == ' ' || c == '\t';
}

pub fn is_space_char(c: char) -> bool {
    return c.is_ascii_whitespace();
}

pub fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

pub fn is_word_char(c: char) -> bool {
    return c.is_alphanumeric() || c == '_';
}

pub fn is_keyword(c: char) -> bool {
    return c >= '!' && c <= '/' || c >= '[' && c <= '^' || c >= '{' && c <= '~' || c == '`';
}

pub fn is_filepath_char(c: char) -> bool {
    return c == MAIN_SEPARATOR || is_filename_char(c);
}

pub fn is_filename_char(c: char) -> bool {
    return is_word_char(c) || "@.-_+,#$%~=:".contains(c) || c > '\u{007F}';
}

#[inline]
pub(crate) fn sort2<T>(a: T, b: T) -> (T, T)
where
    T: Ord,
{
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}
