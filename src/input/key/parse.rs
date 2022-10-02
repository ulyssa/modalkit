use std::ops::BitOr;

use crossterm::event::{KeyCode, KeyModifiers, MediaKeyCode};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char, digit1},
    combinator::{eof, map_res, value},
    multi::{many0, many1},
    IResult,
};

use super::TerminalKey;

fn parse_modifier(input: &str) -> IResult<&str, KeyModifiers> {
    /*
     * Parse the modifier prefixes in things like <C-...>, <S-...>, <A-...>, and <M-...>.
     *
     * Vim also has <D-...> for Apple's Command key, but terminals don't
     * understand that, so it's not meaningful to parse it here (unless
     * we were to map it to be the same as something else like <C-...>).
     */
    alt((
        value(KeyModifiers::ALT, tag("A-")),
        value(KeyModifiers::ALT, tag("M-")),
        value(KeyModifiers::CONTROL, tag("C-")),
        value(KeyModifiers::SHIFT, tag("S-")),
    ))(input)
}

fn parse_lock(input: &str) -> IResult<&str, KeyCode> {
    alt((
        value(KeyCode::CapsLock, tag("CapsLock")),
        value(KeyCode::ScrollLock, tag("ScrollLock")),
        value(KeyCode::NumLock, tag("NumLock")),
    ))(input)
}

fn parse_media_name(input: &str) -> IResult<&str, MediaKeyCode> {
    alt((
        value(MediaKeyCode::PlayPause, tag("MediaPlayPause")),
        value(MediaKeyCode::Play, tag("MediaPlay")),
        value(MediaKeyCode::Pause, tag("MediaPause")),
        value(MediaKeyCode::Reverse, tag("MediaReverse")),
        value(MediaKeyCode::Stop, tag("MediaStop")),
        value(MediaKeyCode::FastForward, tag("MediaFastForward")),
        value(MediaKeyCode::Rewind, tag("MediaRewind")),
        value(MediaKeyCode::TrackNext, tag("MediaTrackNext")),
        value(MediaKeyCode::TrackPrevious, tag("MediaTrackPrevious")),
        value(MediaKeyCode::Record, tag("MediaRecord")),
        value(MediaKeyCode::LowerVolume, tag("MediaVolumeUp")),
        value(MediaKeyCode::RaiseVolume, tag("MediaVolumeDown")),
        value(MediaKeyCode::MuteVolume, tag("MediaVolumeMute")),
    ))(input)
}

fn parse_media(input: &str) -> IResult<&str, KeyCode> {
    let (input, k) = parse_media_name(input)?;

    Ok((input, KeyCode::Media(k)))
}

fn parse_arrow(input: &str) -> IResult<&str, KeyCode> {
    alt((
        value(KeyCode::Left, tag("Left")),
        value(KeyCode::Right, tag("Right")),
        value(KeyCode::Up, tag("Up")),
        value(KeyCode::Down, tag("Down")),
    ))(input)
}

fn parse_ps(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = alt((tag("PS"), tag("PrintScreen"), tag("SysRq")))(input)?;
    Ok((input, KeyCode::PrintScreen))
}

fn parse_page_up(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("PageUp")(input)?;
    Ok((input, KeyCode::PageUp))
}

fn parse_page_down(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("PageDown")(input)?;
    Ok((input, KeyCode::PageDown))
}

fn parse_home(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Home")(input)?;
    Ok((input, KeyCode::Home))
}

fn parse_end(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("End")(input)?;
    Ok((input, KeyCode::End))
}

fn parse_insert(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = alt((tag("Insert"), tag("Ins")))(input)?;
    Ok((input, KeyCode::Insert))
}

fn parse_esc(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Esc")(input)?;
    Ok((input, KeyCode::Esc))
}

fn parse_tab(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Tab")(input)?;
    Ok((input, KeyCode::Tab))
}

fn parse_bs(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = alt((tag("BS"), tag("BackSpace")))(input)?;
    Ok((input, KeyCode::Backspace))
}

fn parse_nl(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = alt((tag("NL"), tag("NewLine"), tag("LineFeed"), tag("LF")))(input)?;
    Ok((input, KeyCode::Char('\n')))
}

fn parse_cr(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = alt((tag("CR"), tag("Return"), tag("Enter")))(input)?;
    Ok((input, KeyCode::Enter))
}

fn parse_del(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = alt((tag("Delete"), tag("Del")))(input)?;
    Ok((input, KeyCode::Delete))
}

fn parse_nul(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Nul")(input)?;
    Ok((input, KeyCode::Null))
}

fn parse_pause(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Pause")(input)?;
    Ok((input, KeyCode::Pause))
}

fn parse_undo(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Undo")(input)?;
    Ok((input, KeyCode::F(14)))
}

fn parse_help(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Help")(input)?;
    Ok((input, KeyCode::F(15)))
}

fn parse_space(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Space")(input)?;
    Ok((input, KeyCode::Char(' ')))
}

fn parse_bar(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Bar")(input)?;
    Ok((input, KeyCode::Char('|')))
}

fn parse_bslash(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Bslash")(input)?;
    Ok((input, KeyCode::Char('\\')))
}

fn parse_lt(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("lt")(input)?;
    Ok((input, KeyCode::Char('<')))
}

fn parse_named_ascii(input: &str) -> IResult<&str, KeyCode> {
    alt((parse_space, parse_bar, parse_bslash, parse_lt))(input)
}

fn parse_named_ctl(input: &str) -> IResult<&str, KeyCode> {
    alt((parse_esc, parse_tab, parse_bs, parse_nl, parse_cr, parse_nul))(input)
}

fn parse_keyname(input: &str) -> IResult<&str, KeyCode> {
    alt((
        parse_arrow,
        parse_named_ascii,
        parse_named_ctl,
        parse_page_up,
        parse_page_down,
        parse_home,
        parse_end,
        parse_insert,
        parse_del,
        parse_pause,
        parse_ps,
        parse_undo,
        parse_help,
        parse_lock,
        parse_media,
    ))(input)
}

fn parse_base10_u8(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 10)
}

fn parse_function(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = char('F')(input)?;
    let (input, n) = map_res(digit1, parse_base10_u8)(input)?;

    Ok((input, KeyCode::F(n)))
}

fn parse_anychar(input: &str) -> IResult<&str, KeyCode> {
    let (input, c) = anychar(input)?;

    Ok((input, KeyCode::Char(c)))
}

pub fn parse_simple(input: &str) -> IResult<&str, TerminalKey> {
    let (input, c) = anychar(input)?;
    let kc = KeyCode::Char(c);
    let km = if c.is_ascii_uppercase() {
        KeyModifiers::SHIFT
    } else {
        KeyModifiers::NONE
    };

    let key = TerminalKey::new(kc, km);

    Ok((input, key))
}

pub fn parse_special(input: &str) -> IResult<&str, TerminalKey> {
    let (input, _) = char('<')(input)?;
    let (input, m) = many0(parse_modifier)(input)?;
    let (input, mut k) = alt((parse_keyname, parse_function, parse_anychar))(input)?;
    let (input, _) = char('>')(input)?;

    let mut m = m.into_iter().fold(KeyModifiers::NONE, BitOr::bitor);

    if let KeyCode::Char(c) = k {
        if m.contains(KeyModifiers::CONTROL) {
            m -= KeyModifiers::SHIFT;

            let k = match c.to_ascii_lowercase() {
                'i' => TerminalKey::from(KeyCode::Tab),
                'j' => TerminalKey::from(KeyCode::Char('\n')),
                'm' => TerminalKey::from(KeyCode::Enter),
                '[' => TerminalKey::from(KeyCode::Esc),
                '?' => TerminalKey::from(KeyCode::Backspace),
                '\\' => TerminalKey::new(KeyCode::Char('4'), m),
                ']' => TerminalKey::new(KeyCode::Char('5'), m),
                '^' => TerminalKey::new(KeyCode::Char('6'), m),
                '_' => TerminalKey::new(KeyCode::Char('7'), m),
                '@' => TerminalKey::new(KeyCode::Char(' '), m),
                c => TerminalKey::new(KeyCode::Char(c), m),
            };

            return Ok((input, k));
        }

        if m.contains(KeyModifiers::SHIFT) {
            k = KeyCode::Char(c.to_ascii_uppercase());
        }

        if m.contains(KeyModifiers::ALT) && c.is_uppercase() {
            m |= KeyModifiers::SHIFT;
        }
    } else if let KeyCode::Tab = k {
        if m == KeyModifiers::SHIFT {
            let key = TerminalKey::from(KeyCode::BackTab);

            return Ok((input, key));
        }
    }

    let key = TerminalKey::new(k, m);

    return Ok((input, key));
}

pub fn parse_key_str(input: &str) -> IResult<&str, TerminalKey> {
    let (input, res) = alt((parse_special, parse_simple))(input)?;
    let (input, _) = eof(input)?;

    Ok((input, res))
}

pub fn parse_macro_str(input: &str) -> IResult<&str, Vec<TerminalKey>> {
    let (input, res) = many1(alt((parse_special, parse_simple)))(input)?;
    let (input, _) = eof(input)?;

    Ok((input, res))
}
