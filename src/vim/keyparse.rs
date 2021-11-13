use std::ops::BitOr;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char, digit1, satisfy},
    combinator::{eof, map_res, opt, value},
    multi::{many0, many1},
    IResult,
};

use super::{VimEdgeEvent, VimEdgePath, VimEdgePathPart, VimKeyClass};

use crate::input::bindings::{EdgeEvent, EdgeRepeat};

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

fn parse_left(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Left")(input)?;
    Ok((input, KeyCode::Left))
}

fn parse_right(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Right")(input)?;
    Ok((input, KeyCode::Right))
}

fn parse_up(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Up")(input)?;
    Ok((input, KeyCode::Up))
}

fn parse_down(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = tag("Down")(input)?;
    Ok((input, KeyCode::Down))
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

fn parse_arrow(input: &str) -> IResult<&str, KeyCode> {
    alt((parse_left, parse_right, parse_up, parse_down))(input)
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
        parse_undo,
        parse_help,
    ))(input)
}

fn parse_base10_u8(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 10)
}

fn parse_base10_usize(input: &str) -> Result<usize, std::num::ParseIntError> {
    usize::from_str_radix(input, 10)
}

fn parse_function(input: &str) -> IResult<&str, KeyCode> {
    let (input, _) = char('F')(input)?;
    let (input, n) = map_res(digit1, parse_base10_u8)(input)?;

    Ok((input, KeyCode::F(n)))
}

fn parse_control(input: &str) -> IResult<&str, KeyCode> {
    let (input, c) = alt((
        char('@'),
        satisfy(|c| c.is_ascii_alphanumeric()),
        char('['),
        char('\\'),
        char(']'),
        char('^'),
        char('_'),
        char('?'),
    ))(input)?;

    Ok((input, KeyCode::Char(c)))
}

fn parse_special(input: &str) -> IResult<&str, VimEdgePathPart> {
    let (input, _) = char('<')(input)?;
    let (input, m) = many0(parse_modifier)(input)?;
    let (input, mut k) = alt((parse_keyname, parse_function, parse_control))(input)?;
    let (input, _) = char('>')(input)?;

    let mut m = m.into_iter().fold(KeyModifiers::NONE, BitOr::bitor);
    let rep = EdgeRepeat::Once;

    if let KeyCode::Char(c) = k {
        if m == KeyModifiers::CONTROL {
            let k = match c.to_ascii_lowercase() {
                'i' => key!(KeyCode::Tab),
                'j' => key!(KeyCode::Char('\n')),
                'm' => key!(KeyCode::Enter),
                '[' => key!(KeyCode::Esc),
                '?' => key!(KeyCode::Backspace),
                '\\' => key!(KeyCode::Char('4'), m),
                ']' => key!(KeyCode::Char('5'), m),
                '^' => key!(KeyCode::Char('6'), m),
                '_' => key!(KeyCode::Char('7'), m),
                '@' => key!(KeyCode::Char(' '), m),
                c => key!(KeyCode::Char(c), m),
            };

            let key = EdgeEvent::Key(k);
            let ret = (rep, key);

            return Ok((input, ret));
        }

        if m.contains(KeyModifiers::SHIFT) {
            k = KeyCode::Char(c.to_ascii_uppercase());
        }

        if m.contains(KeyModifiers::ALT) && c.is_uppercase() {
            m |= KeyModifiers::SHIFT;
        }
    } else if let KeyCode::Tab = k {
        if m == KeyModifiers::SHIFT {
            let key = EdgeEvent::Key(key!(KeyCode::BackTab));
            let ret = (rep, key);

            return Ok((input, ret));
        }
    }

    let key = EdgeEvent::Key(key!(k, m));
    let ret = (rep, key);

    return Ok((input, ret));
}

fn parse_count(input: &str) -> IResult<&str, VimEdgePathPart> {
    let (input, _) = tag("{count}")(input)?;

    let rep = EdgeRepeat::Min(1);
    let evt = EdgeEvent::Class(VimKeyClass::Count);

    Ok((input, (rep, evt)))
}

fn parse_repetition_min(input: &str) -> IResult<&str, EdgeRepeat> {
    let (input, _) = tag(">=")(input)?;
    let (input, n) = map_res(digit1, parse_base10_usize)(input)?;

    Ok((input, EdgeRepeat::Min(n)))
}

fn parse_repetition_max(input: &str) -> IResult<&str, EdgeRepeat> {
    let (input, _) = tag("<=")(input)?;
    let (input, n) = map_res(digit1, parse_base10_usize)(input)?;

    Ok((input, EdgeRepeat::Max(n)))
}

fn parse_repetition(input: &str) -> IResult<&str, EdgeRepeat> {
    alt((
        value(EdgeRepeat::Min(0), tag("*")),
        value(EdgeRepeat::Min(1), tag("+")),
        parse_repetition_min,
        parse_repetition_max,
    ))(input)
}

fn parse_edgename(input: &str) -> IResult<&str, VimEdgePathPart> {
    let (input, _) = char('{')(input)?;
    let (input, e) = alt((
        value(EdgeEvent::Any, tag("any")),
        value(EdgeEvent::Class(VimKeyClass::Register), tag("register")),
        value(EdgeEvent::Class(VimKeyClass::Mark), tag("mark")),
        value(EdgeEvent::Class(VimKeyClass::Octal), tag("oct")),
        value(EdgeEvent::Class(VimKeyClass::Decimal), tag("dec")),
        value(EdgeEvent::Class(VimKeyClass::Hexadecimal), tag("hex")),
        value(EdgeEvent::Class(VimKeyClass::Digraph1), tag("digraph1")),
        value(EdgeEvent::Class(VimKeyClass::Digraph2), tag("digraph2")),
    ))(input)?;
    let (input, rep) = opt(parse_repetition)(input)?;
    let (input, _) = char('}')(input)?;

    let rep = rep.unwrap_or(EdgeRepeat::Once);

    Ok((input, (rep, e)))
}

fn parse_key_simple(input: &str) -> IResult<&str, VimEdgePathPart> {
    let (input, c) = anychar(input)?;
    let kc = KeyCode::Char(c);
    let km = if c.is_ascii_uppercase() {
        KeyModifiers::SHIFT
    } else {
        KeyModifiers::NONE
    };

    let rep = EdgeRepeat::Once;
    let key = EdgeEvent::Key(key!(kc, km));

    Ok((input, (rep, key)))
}

fn parse_key(input: &str) -> IResult<&str, (EdgeRepeat, VimEdgeEvent)> {
    alt((parse_special, parse_count, parse_edgename, parse_key_simple))(input)
}

pub fn parse(input: &str) -> IResult<&str, VimEdgePath> {
    let (input, res) = many1(parse_key)(input)?;
    let (input, _) = eof(input)?;

    Ok((input, res))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! once {
        ($ev: expr) => {
            (EdgeRepeat::Once, $ev)
        };
    }

    macro_rules! evkey {
        ($k: expr) => {
            once!(EdgeEvent::Key(key!($k)))
        };
        ($k: expr, $km: expr) => {
            once!(EdgeEvent::Key(key!($k, $km)))
        };
    }

    macro_rules! evctl {
        ($c: literal) => {
            once!(EdgeEvent::Key(ctl!($c)))
        };
    }

    macro_rules! evclass {
        ($k: expr) => {
            once!(EdgeEvent::Class($k))
        };
    }

    macro_rules! count {
        () => {
            (EdgeRepeat::Min(1), EdgeEvent::Class(VimKeyClass::Count))
        };
    }

    macro_rules! res {
        ( $( $x: expr ),* ) => {
            Ok(("", vec![ $( $x, )* ]))
        };
    }

    #[test]
    fn test_ctl_uppercase() {
        assert_eq!(parse("<C-A>"), res![evctl!('a')]);
        assert_eq!(parse("<C-B>"), res![evctl!('b')]);
    }

    #[test]
    fn test_ctl_lowercase() {
        assert_eq!(parse("<C-a>"), res![evctl!('a')]);
        assert_eq!(parse("<C-b>"), res![evctl!('b')]);
    }

    #[test]
    fn test_shift_lowercase() {
        let mods = KeyModifiers::SHIFT;

        assert_eq!(parse("<S-a>"), res![evkey!(KeyCode::Char('A'), mods)]);
        assert_eq!(parse("<S-q>"), res![evkey!(KeyCode::Char('Q'), mods)]);
        assert_eq!(parse("<S-1>"), res![evkey!(KeyCode::Char('1'), mods)]);
        assert_eq!(parse("<S-^>"), res![evkey!(KeyCode::Char('^'), mods)]);
    }

    #[test]
    fn test_shift_uppercase() {
        let mods = KeyModifiers::SHIFT;

        assert_eq!(parse("<S-A>"), res![evkey!(KeyCode::Char('A'), mods)]);
        assert_eq!(parse("<S-Q>"), res![evkey!(KeyCode::Char('Q'), mods)]);
        assert_eq!(parse("<S-1>"), res![evkey!(KeyCode::Char('1'), mods)]);
        assert_eq!(parse("<S-^>"), res![evkey!(KeyCode::Char('^'), mods)]);
    }

    #[test]
    fn test_shift_alt() {
        let mods = KeyModifiers::ALT | KeyModifiers::SHIFT;

        assert_eq!(parse("<S-A-a>"), res![evkey!(KeyCode::Char('A'), mods)]);
        assert_eq!(parse("<S-A-A>"), res![evkey!(KeyCode::Char('A'), mods)]);
        assert_eq!(parse("<S-A-q>"), res![evkey!(KeyCode::Char('Q'), mods)]);
        assert_eq!(parse("<S-A-Q>"), res![evkey!(KeyCode::Char('Q'), mods)]);
    }

    #[test]
    fn test_alt_lowercase() {
        let mods = KeyModifiers::ALT;

        assert_eq!(parse("<A-a>"), res![evkey!(KeyCode::Char('a'), mods)]);
        assert_eq!(parse("<A-b>"), res![evkey!(KeyCode::Char('b'), mods)]);
        assert_eq!(parse("<M-a>"), res![evkey!(KeyCode::Char('a'), mods)]);
        assert_eq!(parse("<M-b>"), res![evkey!(KeyCode::Char('b'), mods)]);
    }

    #[test]
    fn test_alt_uppercase() {
        let mods = KeyModifiers::ALT | KeyModifiers::SHIFT;

        assert_eq!(parse("<A-A>"), res![evkey!(KeyCode::Char('A'), mods)]);
        assert_eq!(parse("<A-B>"), res![evkey!(KeyCode::Char('B'), mods)]);
        assert_eq!(parse("<M-A>"), res![evkey!(KeyCode::Char('A'), mods)]);
        assert_eq!(parse("<M-B>"), res![evkey!(KeyCode::Char('B'), mods)]);
    }

    #[test]
    fn test_ctl_rename() {
        assert_eq!(parse("<C-?>"), res![evkey!(KeyCode::Backspace)]);
        assert_eq!(parse("<C-I>"), res![evkey!(KeyCode::Tab)]);
        assert_eq!(parse("<C-J>"), res![evkey!('\n')]);
        assert_eq!(parse("<C-M>"), res![evkey!(KeyCode::Enter)]);
        assert_eq!(parse("<C-@>"), res![evctl!(' ')]);
        assert_eq!(parse("<C-[>"), res![evkey!(KeyCode::Esc)]);
    }

    #[test]
    fn test_named_ascii() {
        assert_eq!(parse("<Space>"), res![evkey!(' ')]);
        assert_eq!(parse("<Bar>"), res![evkey!('|')]);
        assert_eq!(parse("<Bslash>"), res![evkey!('\\')]);
        assert_eq!(parse("<lt>"), res![evkey!('<')]);
    }

    #[test]
    fn test_arrow_key() {
        assert_eq!(parse("<Left>"), res![evkey!(KeyCode::Left)]);
        assert_eq!(parse("<Right>"), res![evkey!(KeyCode::Right)]);
        assert_eq!(parse("<Up>"), res![evkey!(KeyCode::Up)]);
        assert_eq!(parse("<Down>"), res![evkey!(KeyCode::Down)]);
    }

    #[test]
    fn test_named_ctl() {
        // <C-?>
        assert_eq!(parse("<BS>"), res![evkey!(KeyCode::Backspace)]);
        assert_eq!(parse("<BackSpace>"), res![evkey!(KeyCode::Backspace)]);

        // <C-J>
        assert_eq!(parse("<NL>"), res![evkey!('\n')]);
        assert_eq!(parse("<NewLine>"), res![evkey!('\n')]);
        assert_eq!(parse("<LineFeed>"), res![evkey!('\n')]);
        assert_eq!(parse("<LF>"), res![evkey!('\n')]);

        // <C-M>
        assert_eq!(parse("<CR>"), res![evkey!(KeyCode::Enter)]);
        assert_eq!(parse("<Return>"), res![evkey!(KeyCode::Enter)]);
        assert_eq!(parse("<Enter>"), res![evkey!(KeyCode::Enter)]);

        assert_eq!(parse("<Esc>"), res![evkey!(KeyCode::Esc)]);
        assert_eq!(parse("<Tab>"), res![evkey!(KeyCode::Tab)]);
        assert_eq!(parse("<Nul>"), res![evkey!(KeyCode::Null)]);
    }

    #[test]
    fn test_function_key() {
        assert_eq!(parse("<F1>"), res![evkey!(KeyCode::F(1))]);
        assert_eq!(parse("<F2>"), res![evkey!(KeyCode::F(2))]);
        assert_eq!(parse("<F10>"), res![evkey!(KeyCode::F(10))]);
    }

    #[test]
    fn test_special_key() {
        assert_eq!(parse("<PageUp>"), res![evkey!(KeyCode::PageUp)]);
        assert_eq!(parse("<PageDown>"), res![evkey!(KeyCode::PageDown)]);
        assert_eq!(parse("<Home>"), res![evkey!(KeyCode::Home)]);
        assert_eq!(parse("<End>"), res![evkey!(KeyCode::End)]);
        assert_eq!(parse("<Insert>"), res![evkey!(KeyCode::Insert)]);
        assert_eq!(parse("<Del>"), res![evkey!(KeyCode::Delete)]);
        assert_eq!(parse("<Delete>"), res![evkey!(KeyCode::Delete)]);
        assert_eq!(parse("<Undo>"), res![evkey!(KeyCode::F(14))]);
        assert_eq!(parse("<Help>"), res![evkey!(KeyCode::F(15))]);
        assert_eq!(parse("<S-Tab>"), res![evkey!(KeyCode::BackTab)]);
    }

    #[test]
    fn test_edges() {
        assert_eq!(parse("{any}"), res![once!(EdgeEvent::Any)]);
        assert_eq!(parse("{count}"), res![count!()]);
        assert_eq!(parse("{mark}"), res![evclass!(VimKeyClass::Mark)]);
        assert_eq!(parse("{register}"), res![evclass!(VimKeyClass::Register)]);
        assert_eq!(parse("{digraph1}"), res![evclass!(VimKeyClass::Digraph1)]);
        assert_eq!(parse("{digraph2}"), res![evclass!(VimKeyClass::Digraph2)]);
    }

    #[test]
    fn test_sequence() {
        assert_eq!(parse("\"{register}"), res![evkey!('"'), evclass!(VimKeyClass::Register)]);
        assert_eq!(parse("<C-R>{register}"), res![evctl!('r'), evclass!(VimKeyClass::Register)]);
        assert_eq!(parse("r{any}"), res![evkey!('r'), once!(EdgeEvent::Any)]);
        assert_eq!(parse("gwgw"), res![evkey!('g'), evkey!('w'), evkey!('g'), evkey!('w')]);
        assert_eq!(parse("<C-K>{digraph1}{digraph2}"), res![
            evctl!('k'),
            evclass!(VimKeyClass::Digraph1),
            evclass!(VimKeyClass::Digraph2)
        ]);
    }
}
