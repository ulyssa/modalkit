//! # Input keys
//!
//! ## Overview
//!
//! This module contains code for representing and processing keys.
//!
use std::hash::Hash;
use std::str::FromStr;

use self::parse::{parse_key_str, parse_macro_str};
use crate::keybindings::InputKey;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers, MediaKeyCode};

pub(crate) mod parse;

/// Errors that occur while trying to use macros.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum MacroError {
    /// Failure to interpret macro string.
    #[error("Invalid macro string: {0:?}")]
    InvalidMacro(String),

    /// Empty macro string.
    #[error("Empty macro string")]
    EmptyMacro,

    /// A macro that seems to be looping.
    #[error("Ending suspected macro loop; macro run {0} times w/o keyboard input")]
    LoopingMacro(usize),
}

/// A key pressed in a terminal.
///
/// It's important to remember when setting up keybindings and building a TUI that some terminals
/// represent several keypresses the same way by default, while others are capable of precisely
/// representing what keys the user actually pressed, such as when using something like
/// [crossterm::event::PushKeyboardEnhancementFlags] or when running in Windows terminal.
///
/// For example, if you're using [crossterm::event::KeyCode] then you'll likely run into the
/// following in a Unix terminal:
///
/// - `<C-M>` will sometimes be represented as `<Enter>`
/// - `<C-I>` will sometimes be represented as `<Tab>`
/// - `<C-?>` will sometimes be represented as `<BackSpace>`
/// - `<C-@>` will sometimes be represented as `<C-Space>`
/// - `<C-[>` will sometimes be represented as `<Esc>`
/// - `<C-\>` will sometimes be represented as `<C-4>`
/// - `<C-]>` will sometimes be represented as `<C-5>`
/// - `<C-^>` will sometimes be represented as `<C-6>`
/// - `<C-_>` will sometimes be represented as `<C-7>`
///
/// You can avoid getting bit by differences between terminals by mapping all of these to the same
/// action for consistency.
///
/// Additionally, some modifier keys can't be expressed by default: `<S-Enter>` or `<C-Enter>`
/// cannot always be represented. If you map those, make sure you also provide alternatives!
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TerminalKey {
    code: KeyCode,
    modifiers: KeyModifiers,
}

impl TerminalKey {
    /// We internally represent terminal keys using the crossterm types, but we do a little bit of
    /// normalization first, since how keys end up getting represented varies based on terminal.
    pub(crate) fn new(mut code: KeyCode, mut modifiers: KeyModifiers) -> Self {
        if let KeyCode::Char(ref mut c) = code {
            if modifiers.intersects(KeyModifiers::SHIFT | KeyModifiers::CONTROL) {
                // If Shift or Control are pressed, represent the character as uppercase.
                *c = c.to_ascii_uppercase();
            } else if c.is_ascii_uppercase() {
                // If the character is uppercase, imply Shift.
                modifiers.insert(KeyModifiers::SHIFT);
            }

            if modifiers == KeyModifiers::SHIFT && *c != ' ' {
                // SHIFT is included for things like ':' and '?' on Windows, but not on *nix systems,
                // so remove it for character keypresses that *only* contain Shift, so that it
                // doesn't break hashing and comparisons.
                //
                // The exception here is <S-Space>, which can be parsed on both Windows and *nix
                // systems when using the Kitty enhanced key protocol/modifyOtherKeys.
                modifiers -= KeyModifiers::SHIFT;
            }
        }

        Self { code, modifiers }
    }

    pub(crate) fn get_char_mods(&self) -> Option<(char, KeyModifiers)> {
        if let KeyCode::Char(c) = self.code {
            return Some((c, self.modifiers));
        }

        None
    }

    /// Return this key's representation as a single codepoint, if it exists.
    pub fn get_literal_char(&self) -> Option<char> {
        match self.code {
            KeyCode::Char(c) => {
                if (self.modifiers - KeyModifiers::SHIFT).is_empty() {
                    return Some(c);
                }

                if self.modifiers == KeyModifiers::CONTROL {
                    let cp = match c {
                        'A'..='Z' => c as u32 - b'A' as u32 + 0x01,
                        ' ' | '@' => 0x0,
                        '4'..='7' => c as u32 - b'4' as u32 + 0x1C,
                        _ => {
                            panic!("unknown control key: {:?}", c)
                        },
                    };

                    return char::from_u32(cp);
                }

                return None;
            },
            KeyCode::Tab if self.modifiers.is_empty() => {
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
}

impl FromStr for TerminalKey {
    type Err = MacroError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        if input.is_empty() {
            return Err(MacroError::EmptyMacro);
        } else if let Ok((_, key)) = parse_key_str(input) {
            return Ok(key);
        } else {
            return Err(MacroError::InvalidMacro(input.to_string()));
        }
    }
}

impl ToString for TerminalKey {
    fn to_string(&self) -> String {
        let mut res = String::new();

        let push_mods = |res: &mut String, mods: KeyModifiers| {
            if mods.contains(KeyModifiers::CONTROL) {
                res.push_str("C-");
            }

            if mods.contains(KeyModifiers::ALT) {
                res.push_str("A-");
            }

            if mods.contains(KeyModifiers::SHIFT) {
                res.push_str("S-");
            }
        };

        let push_named_mods = |res: &mut String, name: &str, mods| {
            res.push('<');
            push_mods(res, mods);
            res.push_str(name);
            res.push('>');
        };

        let push_named = |res: &mut String, name: &str| push_named_mods(res, name, self.modifiers);

        match self.code {
            KeyCode::Left => push_named(&mut res, "Left"),
            KeyCode::Right => push_named(&mut res, "Right"),
            KeyCode::Up => push_named(&mut res, "Up"),
            KeyCode::Down => push_named(&mut res, "Down"),
            KeyCode::Backspace => push_named(&mut res, "BS"),
            KeyCode::Enter => push_named(&mut res, "Enter"),
            KeyCode::Home => push_named(&mut res, "Home"),
            KeyCode::End => push_named(&mut res, "End"),
            KeyCode::PageUp => push_named(&mut res, "PageUp"),
            KeyCode::PageDown => push_named(&mut res, "PageDown"),
            KeyCode::Null => push_named(&mut res, "Nul"),
            KeyCode::Esc => push_named(&mut res, "Esc"),
            KeyCode::Delete => push_named(&mut res, "Del"),
            KeyCode::Insert => push_named(&mut res, "Insert"),
            KeyCode::CapsLock => push_named(&mut res, "CapsLock"),
            KeyCode::ScrollLock => push_named(&mut res, "ScrollLock"),
            KeyCode::NumLock => push_named(&mut res, "NumLock"),
            KeyCode::PrintScreen => push_named(&mut res, "PrintScreen"),
            KeyCode::Pause => push_named(&mut res, "Pause"),
            KeyCode::Menu => push_named(&mut res, "Menu"),
            KeyCode::Tab => push_named(&mut res, "Tab"),
            KeyCode::BackTab => {
                push_named_mods(&mut res, "Tab", self.modifiers | KeyModifiers::SHIFT)
            },
            KeyCode::F(n) => {
                let n = n.to_string();

                push_named(&mut res, n.as_str());
            },
            KeyCode::Char(c) => {
                if c == ' ' {
                    if self.modifiers.is_empty() {
                        res.push(c);
                    } else {
                        push_named(&mut res, "Space");
                    }
                } else if c == '<' {
                    push_named(&mut res, "lt");
                } else if (self.modifiers - KeyModifiers::SHIFT).is_empty() {
                    res.push(c);
                } else {
                    let c =
                        if self.modifiers.intersects(KeyModifiers::CONTROL | KeyModifiers::SHIFT) {
                            c.to_uppercase().to_string()
                        } else {
                            c.to_string()
                        };

                    push_named(&mut res, c.as_str());
                }
            },
            KeyCode::Media(mc) => {
                let name = match mc {
                    MediaKeyCode::PlayPause => "MediaPlayPause",
                    MediaKeyCode::Play => "MediaPlay",
                    MediaKeyCode::Pause => "MediaPause",
                    MediaKeyCode::Reverse => "MediaReverse",
                    MediaKeyCode::Stop => "MediaStop",
                    MediaKeyCode::FastForward => "MediaFastForward",
                    MediaKeyCode::Rewind => "MediaRewind",
                    MediaKeyCode::TrackNext => "MediaTrackNext",
                    MediaKeyCode::TrackPrevious => "MediaTrackPrevious",
                    MediaKeyCode::Record => "MediaRecord",
                    MediaKeyCode::LowerVolume => "MediaVolumeUp",
                    MediaKeyCode::RaiseVolume => "MediaVolumeDown",
                    MediaKeyCode::MuteVolume => "MediaVolumeMute",
                };

                push_named(&mut res, name);
            },
            KeyCode::Modifier(_) | KeyCode::KeypadBegin => {
                // Do nothing with these for now.
            },
        }

        return res;
    }
}

impl InputKey for TerminalKey {
    type Error = MacroError;

    fn decompose(&mut self) -> Option<Self> {
        if let KeyCode::Char(_) = self.code {
            if self.modifiers.contains(KeyModifiers::ALT) {
                self.modifiers -= KeyModifiers::ALT;

                return Some(Self::from(KeyCode::Esc));
            }
        }

        return None;
    }

    fn from_macro_str(input: &str) -> Result<Vec<Self>, MacroError> {
        if input.is_empty() {
            return Err(MacroError::EmptyMacro);
        } else if let Ok((_, keys)) = parse_macro_str(input) {
            return Ok(keys);
        } else {
            return Err(MacroError::InvalidMacro(input.to_string()));
        }
    }

    fn get_char(&self) -> Option<char> {
        if let KeyCode::Char(c) = self.code {
            if (self.modifiers - KeyModifiers::SHIFT).is_empty() {
                return Some(c);
            }
        }

        None
    }
}

impl From<KeyCode> for TerminalKey {
    fn from(code: KeyCode) -> Self {
        TerminalKey::new(code, KeyModifiers::NONE)
    }
}

impl From<KeyEvent> for TerminalKey {
    fn from(ke: KeyEvent) -> Self {
        TerminalKey::new(ke.code, ke.modifiers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roundtrip(s: &str) {
        let key = TerminalKey::from_str(s).expect("can parse");
        assert_eq!(s, key.to_string());
    }

    #[test]
    fn test_roundtrips() {
        // Normal characters and their shifted variants on a QWERTY keyboard.
        roundtrip("s");
        roundtrip("S");
        roundtrip("5");
        roundtrip("^");
        roundtrip(":");
        roundtrip(";");

        // '<' is special.
        roundtrip("<lt>");

        // Space is ' ' until a modifier is added.
        roundtrip(" ");
        roundtrip("<C-Space>");
        roundtrip("<S-Space>");

        // Ctrl-Alt-Delete
        roundtrip("<C-A-Del>");

        // Named keys and modified variants.
        roundtrip("<Left>");
        roundtrip("<S-Left>");
        roundtrip("<A-Left>");
        roundtrip("<C-Left>");
        roundtrip("<Enter>");
        roundtrip("<S-Enter>");
        roundtrip("<A-Enter>");
        roundtrip("<C-Enter>");
        roundtrip("<BS>");
        roundtrip("<S-BS>");
        roundtrip("<A-BS>");
        roundtrip("<C-BS>");

        // Make sure Tab/BackTab survive a roundtrip.
        roundtrip("<A-Tab>");
        roundtrip("<A-S-Tab>");
        roundtrip("<C-A-S-Tab>");
        roundtrip("<C-A-Tab>");

        // Alt-j and Alt-J
        roundtrip("<A-S-J>");
        roundtrip("<A-j>");

        // <C-?> is sometimes sent as <BS>, but we should be able to represent both.
        roundtrip("<BS>");
        roundtrip("<C-?>");

        // <C-\> is sometimes sent as <C-4>, but we should be able to represent both.
        roundtrip("<C-\\>");
        roundtrip("<C-4>");

        // <C-]> is sometimes sent as <C-5>, but we should be able to represent both.
        roundtrip("<C-]>");
        roundtrip("<C-5>");

        // <C-[> is sometimes sent as <Esc>, but we should be able to represent both.
        roundtrip("<C-[>");
        roundtrip("<Esc>");
    }
}
