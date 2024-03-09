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
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TerminalKey {
    code: KeyCode,
    modifiers: KeyModifiers,
}

impl TerminalKey {
    pub(crate) fn new(code: KeyCode, mut modifiers: KeyModifiers) -> Self {
        if let KeyCode::Char(_) = code {
            // SHIFT is included for things like ':' and '?' on Windows, but not on *nix systems,
            // so remove it for characters, so that it doesn't break hashing and comparisons.
            modifiers -= KeyModifiers::SHIFT;
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

        let push_mods = |res: &mut String| {
            if self.modifiers.contains(KeyModifiers::CONTROL) {
                res.push_str("C-");
            }

            if self.modifiers.contains(KeyModifiers::SHIFT) {
                res.push_str("S-");
            }

            if self.modifiers.contains(KeyModifiers::ALT) {
                res.push_str("A-");
            }
        };

        let push_named = |res: &mut String, name: &str| {
            res.push('<');
            push_mods(res);
            res.push_str(name);
            res.push('>');
        };

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
            KeyCode::BackTab => res.push_str("<S-Tab>"),
            KeyCode::F(n) => {
                let n = n.to_string();

                push_named(&mut res, n.as_str());
            },
            KeyCode::Char(c) => {
                if (self.modifiers - KeyModifiers::SHIFT).is_empty() {
                    if c == '<' {
                        res.push_str("<lt>");
                    } else {
                        res.push(c);
                    }
                } else if self.modifiers.contains(KeyModifiers::CONTROL) {
                    match c {
                        '4' => {
                            push_named(&mut res, "\\");
                        },
                        '5' => {
                            push_named(&mut res, "]");
                        },
                        '6' => {
                            push_named(&mut res, "^");
                        },
                        '7' => {
                            push_named(&mut res, "_");
                        },
                        ' ' => {
                            push_named(&mut res, "Space");
                        },
                        '<' => {
                            push_named(&mut res, "lt");
                        },
                        _ => {
                            let c = c.to_uppercase().to_string();

                            push_named(&mut res, c.as_str());
                        },
                    }
                } else {
                    let c = c.to_string();

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
