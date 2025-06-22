use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_while1},
    character::complete::{alphanumeric1, anychar, char, satisfy, space0, space1, u32 as num},
    combinator::{cut, eof, opt, value},
    error::{ErrorKind, ParseError},
    multi::separated_list0,
    IResult,
};

use super::*;

fn parse_id(input: &str) -> IResult<&str, ActionToken<'_>> {
    let (input, _) = char('{')(input)?;
    let (input, i) = opt(alphanumeric1)(input)?;
    let (input, _) = cut(char('}'))(input)?;
    Ok((input, ActionToken::Id(i)))
}

fn parse_flag_long(input: &str) -> IResult<&str, ActionToken<'_>> {
    let (input, _) = tag("--")(input)?;
    let (input, c) = cut(take_while1(|c: char| c.is_ascii_alphabetic()))(input)?;

    let token = match c {
        "count" => ActionToken::Flag(Flag::Count),
        "dir" => ActionToken::Flag(Flag::Dir),
        "focus" => ActionToken::Flag(Flag::Focus),
        "input" => ActionToken::Flag(Flag::Input),
        "mark" => ActionToken::Flag(Flag::Mark),
        "style" => ActionToken::Flag(Flag::Style),
        "target" => ActionToken::Flag(Flag::Target),
        "wrap" => ActionToken::Flag(Flag::Wrap),
        flag => ActionToken::Flag(Flag::Long(flag.to_owned())),
    };

    Ok((input, token))
}

fn parse_flag_short(input: &str) -> IResult<&str, ActionToken<'_>> {
    let (input, _) = char('-')(input)?;
    let (input, c) = cut(satisfy(|c| c.is_ascii_alphabetic()))(input)?;

    let token = match c {
        'c' => ActionToken::Flag(Flag::Count),
        'd' => ActionToken::Flag(Flag::Dir),
        'f' => ActionToken::Flag(Flag::Focus),
        'i' => ActionToken::Flag(Flag::Input),
        'm' => ActionToken::Flag(Flag::Mark),
        's' => ActionToken::Flag(Flag::Style),
        't' => ActionToken::Flag(Flag::Target),
        'w' => ActionToken::Flag(Flag::Wrap),
        flag => ActionToken::Flag(Flag::Short(flag)),
    };

    Ok((input, token))
}

fn parse_num(input: &str) -> IResult<&str, ActionToken<'_>> {
    let (input, n) = num(input)?;
    Ok((input, ActionToken::Number(n as usize)))
}

fn parse_bool(input: &str) -> IResult<&str, ActionToken<'_>> {
    let (input, b) = alt((value(true, tag("true")), value(false, tag("false"))))(input)?;
    Ok((input, ActionToken::Bool(b)))
}

fn parse_word(input: &str) -> IResult<&str, ActionToken<'_>> {
    let (input, w) = take_while1(|c: char| c.is_alphanumeric() || c == '-')(input)?;
    Ok((input, ActionToken::Word(w)))
}

fn parse_group(input: &str) -> IResult<&str, ActionToken<'_>> {
    let (input, _) = char('(')(input)?;
    let (input, tokens) = parse_tokens(input)?;
    let (input, _) = cut(char(')'))(input)?;
    Ok((input, ActionToken::Group(tokens)))
}

fn parse_token(input: &str) -> IResult<&str, ActionToken<'_>> {
    alt((
        parse_group,
        parse_flag_long,
        parse_flag_short,
        parse_num,
        parse_bool,
        parse_id,
        parse_word,
        parse_double_quote,
        parse_single_quote,
    ))(input)
}

fn parse_double_quote(input: &str) -> IResult<&str, ActionToken<'_>> {
    if input.is_empty() {
        let err = ParseError::from_error_kind(input, ErrorKind::Eof);
        let err = nom::Err::Error(err);
        return Err(err);
    }

    let (input, _) = char('\"')(input)?;
    let (input, text) = cut(escaped_transform(
        is_not("\t\n\\\""),
        '\\',
        alt((
            value("\t", tag("t")),
            value("\r", tag("r")),
            value("\n", tag("n")),
            value("\\", tag("\\")),
            value("\"", tag("\"")),
        )),
    ))(input)?;
    let (input, _) = cut(char('\"'))(input)?;

    Ok((input, ActionToken::Str(text)))
}

fn parse_single_quote(input: &str) -> IResult<&str, ActionToken<'_>> {
    if input.is_empty() {
        let err = ParseError::from_error_kind(input, ErrorKind::Eof);
        let err = nom::Err::Error(err);
        return Err(err);
    }

    let (input, _) = char('\'')(input)?;
    let (input, c) = cut(anychar)(input)?;

    let (input, c) = if c == '\\' {
        cut(alt((
            value('\t', tag("t")),
            value('\r', tag("r")),
            value('\n', tag("n")),
            value('\\', tag("\\")),
            value('\"', tag("\"")),
        )))(input)?
    } else {
        (input, c)
    };
    let (input, _) = cut(char('\''))(input)?;

    Ok((input, ActionToken::Char(c)))
}

pub fn parse_tokens(input: &str) -> IResult<&str, Vec<ActionToken<'_>>> {
    let (input, _) = space0(input)?;
    let (input, args) = separated_list0(space1, parse_token)(input)?;
    let (input, _) = space0(input)?;

    Ok((input, args))
}

pub fn tokenize(input: &str) -> Result<Vec<ActionToken<'_>>, nom::Err<nom::error::Error<&str>>> {
    let (input, tokens) = parse_tokens(input)?;
    let (_, _) = eof(input)?;

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_cmd() {
        let tokens = tokenize("window close").unwrap();
        assert_eq!(tokens, vec![ActionToken::Word("window"), ActionToken::Word("close"),]);

        let tokens = tokenize("window zoom-toggle").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("window"),
            ActionToken::Word("zoom-toggle"),
        ]);
    }

    #[test]
    fn test_tokenize_ignore_space() {
        let exp = vec![ActionToken::Word("window"), ActionToken::Word("close")];

        // ignore space before
        let tokens = tokenize("    window close").unwrap();
        assert_eq!(tokens, exp);

        // ignore space after
        let tokens = tokenize("window close    ").unwrap();
        assert_eq!(tokens, exp);

        // ignore space in between
        let tokens = tokenize("window     close").unwrap();
        assert_eq!(tokens, exp);
    }

    #[test]
    fn test_tokenize_flags() {
        let tokens = tokenize("window close -c 5").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("window"),
            ActionToken::Word("close"),
            ActionToken::Flag(Flag::Count),
            ActionToken::Number(5),
        ]);

        let tokens = tokenize("scroll -d left").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("scroll"),
            ActionToken::Flag(Flag::Dir),
            ActionToken::Word("left"),
        ]);

        let tokens = tokenize("window focus -f current").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("window"),
            ActionToken::Word("focus"),
            ActionToken::Flag(Flag::Focus),
            ActionToken::Word("current"),
        ]);

        let tokens = tokenize("insert paste -s cursor").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("insert"),
            ActionToken::Word("paste"),
            ActionToken::Flag(Flag::Style),
            ActionToken::Word("cursor"),
        ]);

        let tokens = tokenize("cursor close -t leader").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("cursor"),
            ActionToken::Word("close"),
            ActionToken::Flag(Flag::Target),
            ActionToken::Word("leader"),
        ]);

        let tokens = tokenize("cursor close -F").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("cursor"),
            ActionToken::Word("close"),
            ActionToken::Flag(Flag::Short('F')),
        ]);
    }

    #[test]
    fn test_tokenize_id() {
        let tokens = tokenize("window close -c {count}").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("window"),
            ActionToken::Word("close"),
            ActionToken::Flag(Flag::Count),
            ActionToken::Id(Some("count")),
        ]);

        let tokens = tokenize("window close -c {}").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("window"),
            ActionToken::Word("close"),
            ActionToken::Flag(Flag::Count),
            ActionToken::Id(None),
        ]);
    }

    #[test]
    fn test_tokenize_group() {
        let tokens = tokenize("insert paste -s (side -d next)").unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("insert"),
            ActionToken::Word("paste"),
            ActionToken::Flag(Flag::Style),
            ActionToken::Group(vec![
                ActionToken::Word("side"),
                ActionToken::Flag(Flag::Dir),
                ActionToken::Word("next"),
            ]),
        ]);
    }

    #[test]
    fn test_tokenize_quote() {
        let tokens = tokenize(r#"command run -i "quitall" "#).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("command"),
            ActionToken::Word("run"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Str("quitall".into()),
        ]);

        let tokens = tokenize(r#"command run -i "foo\nbar" "#).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("command"),
            ActionToken::Word("run"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Str("foo\nbar".into()),
        ]);

        let tokens = tokenize(r#"insert type -i 'q'"#).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("insert"),
            ActionToken::Word("type"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Char('q'),
        ]);

        let tokens = tokenize(r#"insert type -i '\n'"#).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("insert"),
            ActionToken::Word("type"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Char('\n'),
        ]);
    }
}
