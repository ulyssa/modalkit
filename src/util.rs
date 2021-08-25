#[cfg(test)]
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
        KeyEvent {
            code: $kc,
            modifiers: KeyModifiers::NONE,
        }
    };
    ($kc: expr, $km: expr) => {
        KeyEvent {
            code: $kc,
            modifiers: $km,
        }
    };
}

#[allow(unused_macros)]
macro_rules! ctl {
    ($ch: literal) => {
        key!(
            KeyCode::Char($ch.to_ascii_lowercase()),
            KeyModifiers::CONTROL
        )
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

#[cfg(test)]
#[inline]
pub fn get_char(ke: &KeyEvent) -> Option<char> {
    if let KeyCode::Char(c) = ke.code {
        if (ke.modifiers - KeyModifiers::SHIFT).is_empty() {
            return Some(c);
        }
    }

    None
}

#[cfg(test)]
#[inline]
pub fn keycode_to_num(ke: &KeyEvent, radix: u32) -> Option<u32> {
    if let Some(c) = get_char(ke) {
        c.to_digit(radix)
    } else {
        None
    }
}
