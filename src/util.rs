use crate::editing::base::MoveDir1D;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

macro_rules! key {
    ($ch: literal) => {
        KeyEvent {
            code: KeyCode::Char($ch),
            modifiers: match $ch.is_ascii_uppercase() {
                true => KeyModifiers::SHIFT,
                false => KeyModifiers::NONE,
            },
        }
    };
    ($kc: expr) => {
        KeyEvent { code: $kc, modifiers: KeyModifiers::NONE }
    };
    ($kc: expr, $km: expr) => {
        KeyEvent { code: $kc, modifiers: $km }
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
        assert_eq!($mm.pop(), Some(($act.clone(), $ctx.clone())));
    };
}

#[allow(unused_macros)]
macro_rules! assert_pop2 {
    ($mm: expr, $act: expr, $ctx: expr) => {
        assert_pop1!($mm, $act, $ctx);
        assert_eq!($mm.pop(), None);
    };
}

#[derive(Debug)]
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

impl Default for IdGenerator {
    fn default() -> IdGenerator {
        IdGenerator { next_id: 0 }
    }
}

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

#[inline]
pub fn get_char(ke: &KeyEvent) -> Option<char> {
    if let KeyCode::Char(c) = ke.code {
        if (ke.modifiers - KeyModifiers::SHIFT).is_empty() {
            return Some(c);
        }
    }

    None
}

pub fn get_literal_char(ke: &KeyEvent) -> Option<char> {
    match ke.code {
        KeyCode::Char(c) => {
            if (ke.modifiers - KeyModifiers::SHIFT).is_empty() {
                return Some(c);
            }

            if ke.modifiers == KeyModifiers::CONTROL {
                let cp = match c {
                    'a'..='z' => c as u32 - b'a' as u32 + 0x01,
                    ' ' | '@' => 0x0,
                    '4'..='7' => c as u32 - b'4' as u32 + 0x1C,
                    _ => {
                        panic!("unknown control key")
                    },
                };

                return char::from_u32(cp);
            }

            return None;
        },
        KeyCode::Tab if ke.modifiers.is_empty() => {
            return Some('\u{09}');
        },
        KeyCode::Enter => {
            return Some('\u{0D}');
        },
        KeyCode::Esc => {
            return Some('\u{1B}');
        },
        KeyCode::Backspace => {
            return Some('\u{7F}');
        },
        _ => {
            return None;
        },
    }
}

#[inline]
pub fn keycode_to_num(ke: &KeyEvent, radix: u32) -> Option<u32> {
    if let Some(c) = get_char(ke) {
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
