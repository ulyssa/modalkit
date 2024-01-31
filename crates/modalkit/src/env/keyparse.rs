use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1},
    combinator::{eof, map_res, opt, value},
    multi::many1,
    IResult,
};

use super::{CommonEdgeEvent, CommonEdgePath, CommonEdgePathPart, CommonKeyClass};

use crate::keybindings::{EdgeEvent, EdgeRepeat};

fn parse_base10_usize(input: &str) -> Result<usize, std::num::ParseIntError> {
    input.parse::<usize>()
}

fn parse_special(input: &str) -> IResult<&str, CommonEdgePathPart> {
    let (input, k) = crate::key::parse::parse_special(input)?;
    let ret = (EdgeRepeat::Once, EdgeEvent::Key(k));

    return Ok((input, ret));
}

fn parse_count(input: &str) -> IResult<&str, CommonEdgePathPart> {
    let (input, _) = tag("{count}")(input)?;

    let rep = EdgeRepeat::Min(1);
    let evt = EdgeEvent::Class(CommonKeyClass::Count);

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

fn parse_edgename(input: &str) -> IResult<&str, CommonEdgePathPart> {
    let (input, _) = char('{')(input)?;
    let (input, e) = alt((
        value(EdgeEvent::Any, tag("any")),
        value(EdgeEvent::Class(CommonKeyClass::Count), tag("count")),
        value(EdgeEvent::Class(CommonKeyClass::Register), tag("register")),
        value(EdgeEvent::Class(CommonKeyClass::Mark), tag("mark")),
        value(EdgeEvent::Class(CommonKeyClass::Octal), tag("oct")),
        value(EdgeEvent::Class(CommonKeyClass::Decimal), tag("dec")),
        value(EdgeEvent::Class(CommonKeyClass::Hexadecimal), tag("hex")),
        value(EdgeEvent::Class(CommonKeyClass::Digraph1), tag("digraph1")),
        value(EdgeEvent::Class(CommonKeyClass::Digraph2), tag("digraph2")),
    ))(input)?;
    let (input, rep) = opt(parse_repetition)(input)?;
    let (input, _) = char('}')(input)?;

    let rep = rep.unwrap_or(EdgeRepeat::Once);

    Ok((input, (rep, e)))
}

fn parse_key_simple(input: &str) -> IResult<&str, CommonEdgePathPart> {
    let (input, k) = crate::key::parse::parse_simple(input)?;
    let rep = EdgeRepeat::Once;
    let key = EdgeEvent::Key(k);

    Ok((input, (rep, key)))
}

fn parse_key(input: &str) -> IResult<&str, (EdgeRepeat, CommonEdgeEvent)> {
    alt((parse_special, parse_count, parse_edgename, parse_key_simple))(input)
}

pub fn parse(input: &str) -> IResult<&str, CommonEdgePath> {
    let (input, res) = many1(parse_key)(input)?;
    let (input, _) = eof(input)?;

    Ok((input, res))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key::TerminalKey;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

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

    macro_rules! evalt {
        ($c: literal) => {
            once!(EdgeEvent::Key(key!($c, KeyModifiers::ALT)))
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
            (EdgeRepeat::Min(1), EdgeEvent::Class(CommonKeyClass::Count))
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
        assert_eq!(parse("{mark}"), res![evclass!(CommonKeyClass::Mark)]);
        assert_eq!(parse("{register}"), res![evclass!(CommonKeyClass::Register)]);
        assert_eq!(parse("{digraph1}"), res![evclass!(CommonKeyClass::Digraph1)]);
        assert_eq!(parse("{digraph2}"), res![evclass!(CommonKeyClass::Digraph2)]);
    }

    #[test]
    fn test_sequence() {
        assert_eq!(parse("\"{register}"), res![evkey!('"'), evclass!(CommonKeyClass::Register)]);
        assert_eq!(parse("<C-R>{register}"), res![evctl!('r'), evclass!(CommonKeyClass::Register)]);
        assert_eq!(parse("r{any}"), res![evkey!('r'), once!(EdgeEvent::Any)]);
        assert_eq!(parse("gwgw"), res![evkey!('g'), evkey!('w'), evkey!('g'), evkey!('w')]);
        assert_eq!(parse("<C-K>{digraph1}{digraph2}"), res![
            evctl!('k'),
            evclass!(CommonKeyClass::Digraph1),
            evclass!(CommonKeyClass::Digraph2)
        ]);
    }

    #[test]
    fn test_multiple_modifiers() {
        assert_eq!(parse("<C-M-X>"), res![evkey!('x', KeyModifiers::CONTROL | KeyModifiers::ALT)]);
    }

    #[test]
    fn test_angle_bracket() {
        assert_eq!(parse("<C-X>>"), res![evctl!('x'), evkey!('>')]);
        assert_eq!(parse("<C-X><"), res![evctl!('x'), evkey!('<')]);
        assert_eq!(parse("<M->>"), res![evalt!('>')]);
        assert_eq!(parse("<M-<>"), res![evalt!('<')]);
    }
}
