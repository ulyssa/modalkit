use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_while, take_while1},
    character::complete::{char, digit0, digit1, one_of, space0, space1},
    combinator::{eof, opt, peek, value},
    error::{context, ErrorKind, ParseError},
    multi::{many0, separated_list0},
    IResult,
};

use crate::{
    commands::{CommandError, ParsedCommand},
    editing::application::ApplicationWindowId,
    prelude::*,
};

fn parse_failed(err: nom::Err<nom::error::Error<&str>>) -> CommandError {
    CommandError::ParseFailed(err.to_string())
}

/// Options provided to a command and parsed by [CommandArgument::options].
#[derive(Debug, Eq, PartialEq)]
pub enum OptionType {
    /// A flag with an optional value, such as `++key=value`.
    Flag(String, Option<String>),

    /// A positional argument.
    Positional(String),
}

/// Argument text following a command name.
#[derive(Debug, Eq, PartialEq)]
pub struct CommandArgument {
    /// Original text of the command argument.
    pub untrimmed: String,

    /// Argument to the command with leading spaces stripped.
    pub text: String,
}

/// Result of parsing command text.
#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct CommandDescription {
    /// An optionally specified range of lines before the command.
    ///
    /// Some Vim commands use this as a way not to specify lines, but to instead provide an integer
    /// argument (e.g., `3q` to quit window 3).
    pub range: Option<RangeSpec>,

    /// Name by which the command was invoked.
    pub command: String,

    /// Whether the command name was followed by a `!` (bang).
    pub bang: bool,

    /// Argument text to the command.
    pub arg: CommandArgument,
}

impl CommandArgument {
    /// Interpret the argument text as a collection of filenames.
    ///
    /// This is similar to [CommandArgument::strings], but in addition to processing escaped spaces
    /// and quoted strings, this will:
    ///
    /// - turn lone `#` characters into [OpenTarget::Alternate]
    /// - turn lone `%` characters into [OpenTarget::Current]
    ///
    /// These values can be escaped. For example, `\#` will become `OpenTarget::Name("#")`.
    pub fn filenames<W>(&self) -> Result<Vec<OpenTarget<W>>, CommandError>
    where
        W: ApplicationWindowId,
    {
        match parse_filenames(self.text.as_str()) {
            Ok((_, args)) => Ok(args),
            Err(e) => Err(CommandError::ParseFailed(e.to_string())),
        }
    }

    /// Interpret the argument text as a collection of strings.
    ///
    /// Values containing spaces can either be quoted, or escaped with a backslash. (For example,
    /// `My\ File.txt` or `"My File.txt")
    pub fn strings(&self) -> Result<Vec<String>, CommandError> {
        match parse_strings(self.text.as_str()) {
            Ok((_, args)) => Ok(args),
            Err(e) => Err(CommandError::ParseFailed(e.to_string())),
        }
    }

    /// Interpret the argument text as a series of positional arguments and flags starting with
    /// `++`.
    pub fn options(&self) -> Result<Vec<OptionType>, CommandError> {
        let res = self
            .strings()?
            .into_iter()
            .map(|mut option| {
                if !option.starts_with("++") {
                    return OptionType::Positional(option);
                }

                let mut option = option.split_off(2);
                let value = option.find('=').map(|idx| {
                    let mut flag = option.split_off(idx);
                    flag.split_off(1)
                });

                OptionType::Flag(option, value)
            })
            .collect();

        Ok(res)
    }

    /// Interpret the argument text as a range specification.
    ///
    /// This can be used to create commands that take a range specification either before or after
    /// the command name.
    pub fn range(&self) -> Result<RangeSpec, CommandError> {
        let (input, spec) = parse_range(self.text.as_str()).map_err(parse_failed)?;
        let _ = eof(input).map_err(parse_failed)?;

        Ok(spec)
    }
}

fn is_not_newline(chr: char) -> bool {
    chr != '\n'
}

fn is_cmd_char(chr: char) -> bool {
    chr.is_ascii_alphabetic() || "#&*<=>@~".contains(chr)
}

fn parse_quote(input: &str) -> IResult<&str, String> {
    if input.is_empty() {
        let err = ParseError::from_error_kind(input, ErrorKind::Eof);
        let err = nom::Err::Error(err);
        return Err(err);
    }

    let (input, _) = tag("\"")(input)?;
    let (input, text) = escaped_transform(
        is_not("\t\n\\\""),
        '\\',
        alt((
            value("t", tag("\t")),
            value("r", tag("\r")),
            value("n", tag("\n")),
            value("\\", tag("\\")),
            value("\"", tag("\"")),
        )),
    )(input)?;
    let (input, _) = tag("\"")(input)?;

    Ok((input, text))
}

fn parse_filename_quote<W>(input: &str) -> IResult<&str, OpenTarget<W>>
where
    W: ApplicationWindowId,
{
    let (input, text) = parse_quote(input)?;

    Ok((input, OpenTarget::Name(text)))
}

fn parse_text(input: &str) -> IResult<&str, String> {
    if input.is_empty() {
        let err = ParseError::from_error_kind(input, ErrorKind::Eof);
        let err = nom::Err::Error(err);
        return Err(err);
    }

    escaped_transform(
        is_not("\t\n\\ |\""),
        '\\',
        alt((
            value("\\", tag("\\")),
            value(" ", tag(" ")),
            value("#", tag("#")),
            value("%", tag("%")),
            value("|", tag("|")),
            value("\"", tag("\"")),
        )),
    )(input)
}

fn parse_filename_text<W>(input: &str) -> IResult<&str, OpenTarget<W>>
where
    W: ApplicationWindowId,
{
    let (input, text) = parse_text(input)?;

    Ok((input, OpenTarget::Name(text)))
}

fn parse_filename_special<W>(input: &str) -> IResult<&str, OpenTarget<W>>
where
    W: ApplicationWindowId,
{
    let (input, v) =
        alt((value(OpenTarget::Alternate, tag("#")), value(OpenTarget::Current, tag("%"))))(input)?;
    let (input, _) = peek(alt((space1, eof)))(input)?;

    Ok((input, v))
}

fn parse_filename<W>(input: &str) -> IResult<&str, OpenTarget<W>>
where
    W: ApplicationWindowId,
{
    alt((parse_filename_special, parse_filename_quote, parse_filename_text))(input)
}

fn parse_filenames<W>(input: &str) -> IResult<&str, Vec<OpenTarget<W>>>
where
    W: ApplicationWindowId,
{
    let (input, args) = separated_list0(space1, parse_filename)(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = eof(input)?;

    Ok((input, args))
}

fn parse_string(input: &str) -> IResult<&str, String> {
    alt((parse_quote, parse_text))(input)
}

fn parse_strings(input: &str) -> IResult<&str, Vec<String>> {
    let (input, args) = separated_list0(space1, parse_string)(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = eof(input)?;

    Ok((input, args))
}

fn parse_range_offset(input: &str) -> IResult<&str, MoveDir1D> {
    alt((value(MoveDir1D::Next, tag("+")), value(MoveDir1D::Previous, tag("-"))))(input)
}

fn parse_range_modifier(input: &str) -> IResult<&str, RangeEndingModifier> {
    let (input, sign) = parse_range_offset(input)?;
    let (input, n) = digit0(input)?;
    let n = if n.is_empty() {
        1
    } else {
        n.parse::<usize>().unwrap()
    };

    Ok((input, RangeEndingModifier::Offset(sign, Count::Exact(n))))
}

fn parse_range_number(input: &str) -> IResult<&str, RangeEndingType> {
    let (input, n) = digit1(input)?;
    let n = n.parse::<usize>().unwrap();

    Ok((input, RangeEndingType::Absolute(Count::Exact(n))))
}

fn parse_range_mark_lc(input: &str) -> IResult<&str, Mark> {
    let (input, c) = one_of("abcdefghijklmnopqrstuvwxyz")(input)?;

    Ok((input, Mark::BufferNamed(c)))
}

fn parse_range_mark_uc(input: &str) -> IResult<&str, Mark> {
    let (input, c) = one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)?;

    Ok((input, Mark::GlobalNamed(c)))
}

fn parse_range_mark_number(input: &str) -> IResult<&str, Mark> {
    let (input, n) = one_of("0123456789")(input)?;
    let n: usize = n.to_digit(10).unwrap() as usize;

    Ok((input, Mark::GlobalLastExited(n)))
}

fn parse_range_mark(input: &str) -> IResult<&str, Mark> {
    context(
        "invalid mark character",
        alt((
            parse_range_mark_lc,
            parse_range_mark_uc,
            parse_range_mark_number,
            value(Mark::VisualBegin, tag("<")),
            value(Mark::VisualEnd, tag(">")),
            value(Mark::LastYankedBegin, tag("[")),
            value(Mark::LastYankedEnd, tag("]")),
            value(Mark::LastJump, one_of("\'`")),
            value(Mark::LastInserted, tag("^")),
            value(Mark::LastChanged, tag(".")),
            value(Mark::BufferLastExited, tag("\"")),
        )),
    )(input)
}

fn parse_range_tick_mark(input: &str) -> IResult<&str, RangeEndingType> {
    let (input, _) = char('\'')(input)?;
    let (input, m) = parse_range_mark(input)?;

    Ok((input, RangeEndingType::Mark(Specifier::Exact(m))))
}

fn parse_range_atom(input: &str) -> IResult<&str, RangeEndingType> {
    alt((
        parse_range_number,
        value(RangeEndingType::Current, tag(".")),
        value(RangeEndingType::All, tag("%")),
        value(RangeEndingType::Last, tag("$")),
        value(RangeEndingType::Search(MoveDir1D::Next), tag("\\/")),
        value(RangeEndingType::Search(MoveDir1D::Previous), tag("\\?")),
        value(RangeEndingType::SubPatSearch(MoveDir1D::Next), tag("\\&")),
        parse_range_tick_mark,
    ))(input)
}

fn parse_range_sep(input: &str) -> IResult<&str, RangeSearchInit> {
    alt((value(RangeSearchInit::Cursor, tag(",")), value(RangeSearchInit::Start, tag(";"))))(input)
}

fn parse_range(original: &str) -> IResult<&str, RangeSpec> {
    let (input, ltype) = opt(parse_range_atom)(original)?;
    let (input, lmods) = many0(parse_range_modifier)(input)?;
    let (input, sep) = opt(parse_range_sep)(input)?;

    match (ltype, sep) {
        (None, None) => {
            if lmods.is_empty() {
                let err = ParseError::from_error_kind(original, ErrorKind::Alt);
                let err = nom::Err::Error(err);
                Err(err)
            } else {
                let ltype = RangeEndingType::Unspecified;
                let left = RangeEnding(ltype, lmods);

                Ok((input, RangeSpec::Single(left)))
            }
        },
        (Some(ltype), None) => {
            let left = RangeEnding(ltype, lmods);

            Ok((input, RangeSpec::Single(left)))
        },
        (ltype, Some(sep)) => {
            let (input, rtype) = opt(parse_range_atom)(input)?;
            let (input, rmods) = many0(parse_range_modifier)(input)?;

            let ltype = ltype.unwrap_or(RangeEndingType::Unspecified);
            let left = RangeEnding(ltype, lmods);

            let rtype = rtype.unwrap_or(RangeEndingType::Unspecified);
            let right = RangeEnding(rtype, rmods);

            Ok((input, RangeSpec::Double(left, right, sep)))
        },
    }
}

fn parse_bang(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('!')(input)?;

    Ok((input, "!"))
}

fn parse_cmd_empty(input: &str) -> IResult<&str, &str> {
    let (input, _) = alt((eof, tag("\n")))(input)?;

    Ok((input, ""))
}

fn parse_cmd_string(input: &str) -> IResult<&str, &str> {
    take_while1(is_cmd_char)(input)
}

fn parse_cmd_name(input: &str) -> IResult<&str, String> {
    let (input, name) = alt((parse_bang, parse_cmd_string, parse_cmd_empty))(input)?;

    Ok((input, name.to_string()))
}

fn parse_cmd_argument(input: &str) -> IResult<&str, CommandArgument> {
    let (trimmed, spaces) = space0(input)?;
    let (input, arg) = take_while(is_not_newline)(trimmed)?;

    let untrimmed = spaces.to_string() + arg;
    let text = arg.to_string();

    Ok((input, CommandArgument { untrimmed, text }))
}

fn parse_cmd_descr(input: &str) -> IResult<&str, CommandDescription> {
    let (input, _) = space0(input)?;
    let (input, _) = many0(char(':'))(input)?;
    let (input, _) = space0(input)?;
    let (input, range) = opt(parse_range)(input)?;
    let (input, command) = parse_cmd_name(input)?;
    let (input, bang) = opt(parse_bang)(input)?;
    let (input, arg) = parse_cmd_argument(input)?;
    let (input, _) = opt(char('\n'))(input)?;

    let cmd = CommandDescription { range, command, bang: bang.is_some(), arg };

    Ok((input, cmd))
}

fn parse(input: &str) -> IResult<&str, CommandDescription> {
    let (input, descr) = parse_cmd_descr(input)?;
    let (input, _) = eof(input)?;

    Ok((input, descr))
}

impl ParsedCommand for CommandDescription {
    fn name(&self) -> String {
        return self.command.clone();
    }
}

impl FromStr for CommandDescription {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse(s) {
            Ok((_, cmd)) => Ok(cmd),
            Err(e) => Err(e.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! arg {
        ($arg: expr) => {
            CommandArgument {
                untrimmed: $arg.to_string(),
                text: $arg.trim_start().to_string(),
            }
        };
    }

    macro_rules! desc {
        ($range: expr, $command: expr, $bang: expr, $arg: expr) => {
            CommandDescription {
                range: $range,
                command: $command.into(),
                bang: $bang,
                arg: arg!($arg),
            }
        };
    }

    macro_rules! names {
        ( $( $mods: expr ),* ) => {
            vec![ $( OpenTarget::<()>::Name($mods.into()), )* ]
        };
    }

    macro_rules! r {
        ($r: expr) => {
            RangeSpec::Single($r.clone())
        };
        ($r1: expr, $r2: expr) => {
            RangeSpec::Double($r1.clone(), $r2.clone(), RangeSearchInit::Cursor)
        };
        ($r1: expr, $r2: expr, $i: expr) => {
            RangeSpec::Double($r1.clone(), $r2.clone(), $i)
        };
    }

    macro_rules! e {
        ($r: expr  $( , $mods: expr ),* ) => {
            RangeEnding($r, vec![ $( $mods, )* ])
        };
    }

    macro_rules! res {
        ($command: expr) => {
            Ok(("", desc!(None, $command, false, "")))
        };
        ($command: expr, $bang: expr) => {
            Ok(("", desc!(None, $command, $bang, "")))
        };
        ($command: expr, $bang: expr, $arg: expr) => {
            Ok(("", desc!(None, $command, $bang, $arg)))
        };
        ($range: expr, $command: expr, $bang: expr, $arg: expr) => {
            Ok(("", desc!(Some($range.clone()), $command, $bang, $arg)))
        };
    }

    #[test]
    fn test_arg_split_hash() {
        let arg = arg!("#");
        assert_eq!(arg.filenames::<()>().unwrap(), vec![OpenTarget::Alternate]);

        let arg = arg!("\\#");
        assert_eq!(arg.filenames::<()>().unwrap(), vec![OpenTarget::Name("#".into())]);

        let arg = arg!("# file#");
        let split = vec![OpenTarget::Alternate, OpenTarget::Name("file#".into())];
        assert_eq!(arg.filenames::<()>().unwrap(), split);

        let arg = arg!("#file#");
        assert_eq!(arg.filenames::<()>().unwrap(), names!["#file#"]);
    }

    #[test]
    fn test_arg_split_perc() {
        let arg = arg!("%");
        assert_eq!(arg.filenames::<()>().unwrap(), vec![OpenTarget::Current]);

        let arg = arg!("\\%");
        assert_eq!(arg.filenames::<()>().unwrap(), vec![OpenTarget::Name("%".into())]);

        let arg = arg!("% file%");
        let split = vec![OpenTarget::Current, OpenTarget::Name("file%".into())];
        assert_eq!(arg.filenames::<()>().unwrap(), split);

        let arg = arg!("%file%");
        assert_eq!(arg.filenames::<()>().unwrap(), names!["%file%"]);
    }

    #[test]
    fn test_arg_split_names_unquoted() {
        let arg = arg!("");
        assert_eq!(arg.filenames::<()>().unwrap(), vec![]);

        let arg = arg!("file");
        assert_eq!(arg.filenames().unwrap(), names!["file"]);

        let arg = arg!(" file1.txt");
        assert_eq!(arg.filenames().unwrap(), names!["file1.txt"]);

        let arg = arg!(" file1.txt ");
        assert_eq!(arg.filenames().unwrap(), names!["file1.txt"]);

        let arg = arg!(" My\\ Documents/foo\\ file.txt file2.txt file3.txt");
        let split = names!["My Documents/foo file.txt", "file2.txt", "file3.txt"];
        assert_eq!(arg.filenames().unwrap(), split);
    }

    #[test]
    fn test_arg_split_names_quoted() {
        let arg = arg!("");
        assert_eq!(arg.filenames::<()>().unwrap(), vec![]);

        let arg = arg!("\"file\"");
        assert_eq!(arg.filenames().unwrap(), names!["file"]);

        let arg = arg!(" \"file1.txt\"");
        assert_eq!(arg.filenames().unwrap(), names!["file1.txt"]);

        let arg = arg!(" \"file1.txt\" ");
        assert_eq!(arg.filenames().unwrap(), names!["file1.txt"]);

        let arg = arg!(" \"My Documents/foo file.txt\" \"file2.txt\" \"file3.txt\"");
        let split = names!["My Documents/foo file.txt", "file2.txt", "file3.txt"];
        assert_eq!(arg.filenames().unwrap(), split);
    }

    #[test]
    fn test_arg_split_kitchen_sink() {
        let arg = arg!(" \"My Documents/\\\"foo file\\\".txt\" file\\ foo.txt # % #foo# %bar%");
        let split = vec![
            OpenTarget::Name("My Documents/\"foo file\".txt".into()),
            OpenTarget::Name("file foo.txt".into()),
            OpenTarget::Alternate,
            OpenTarget::Current,
            OpenTarget::Name("#foo#".into()),
            OpenTarget::Name("%bar%".into()),
        ];
        assert_eq!(arg.filenames::<()>().unwrap(), split);
    }

    #[test]
    fn test_arg_options() {
        let arg = arg!("++flag ++foo=bar pos1 ++baz=quux pos2 ++novalue=");
        let split = vec![
            OptionType::Flag("flag".into(), None),
            OptionType::Flag("foo".into(), Some("bar".into())),
            OptionType::Positional("pos1".into()),
            OptionType::Flag("baz".into(), Some("quux".into())),
            OptionType::Positional("pos2".into()),
            OptionType::Flag("novalue".into(), Some("".into())),
        ];
        assert_eq!(arg.options().unwrap(), split);
    }

    #[test]
    fn test_arg_strings() {
        let arg = arg!(" \"My Documents/\\\"foo file\\\".txt\" file\\ foo.txt # % #foo# %bar%");
        let split = vec![
            "My Documents/\"foo file\".txt",
            "file foo.txt",
            "#",
            "%",
            "#foo#",
            "%bar%",
        ];
        assert_eq!(arg.strings().unwrap(), split);
    }

    #[test]
    fn test_cmd_name() {
        assert_eq!(parse("!"), res!("!", false));
        assert_eq!(parse("!!"), res!("!", true));
        assert_eq!(parse("!echo hello"), res!("!", false, "echo hello"));
        assert_eq!(parse("!!echo hello"), res!("!", true, "echo hello"));
        assert_eq!(parse("!! hello"), res!("!", true, " hello"));
        assert_eq!(parse("q!"), res!("q", true));
        assert_eq!(parse("qa"), res!("qa"));
        assert_eq!(parse("sp"), res!("sp"));
        assert_eq!(parse("split"), res!("split"));
        assert_eq!(parse("vertical split"), res!("vertical", false, " split"));
    }

    #[test]
    fn test_cmd_empty() {
        assert_eq!(parse(""), res!(""));
        assert_eq!(parse(":"), res!(""));
        assert_eq!(parse(" "), res!(""));
        assert_eq!(parse("\t"), res!(""));

        let range = r!(e!(RangeEndingType::Absolute(Count::Exact(7))));
        assert_eq!(parse("7"), res!(range, "", false, ""));

        let range = r!(e!(
            RangeEndingType::Current,
            RangeEndingModifier::Offset(MoveDir1D::Next, Count::Exact(100))
        ));
        assert_eq!(parse(".+100"), res!(range, "", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::LastChanged.into())));
        assert_eq!(parse("'."), res!(range, "", false, ""));
    }

    #[test]
    fn test_ranges_single() {
        let range = r!(e!(RangeEndingType::Current));
        assert_eq!(parse(".d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Last));
        assert_eq!(parse("$d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::All));
        assert_eq!(parse("%d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Absolute(Count::Exact(5))));
        assert_eq!(parse("5d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Absolute(Count::Exact(500))));
        assert_eq!(parse("500d"), res!(range, "d", false, ""));

        let range = r!(e!(
            RangeEndingType::Unspecified,
            RangeEndingModifier::Offset(MoveDir1D::Next, Count::Exact(30))
        ));
        assert_eq!(parse("+30d"), res!(range, "d", false, ""));

        let range = r!(e!(
            RangeEndingType::Current,
            RangeEndingModifier::Offset(MoveDir1D::Next, Count::Exact(30))
        ));
        assert_eq!(parse(".+30d"), res!(range, "d", false, ""));

        let range = r!(e!(
            RangeEndingType::Unspecified,
            RangeEndingModifier::Offset(MoveDir1D::Previous, Count::Exact(30))
        ));
        assert_eq!(parse("-30d"), res!(range, "d", false, ""));

        let range = r!(e!(
            RangeEndingType::Current,
            RangeEndingModifier::Offset(MoveDir1D::Previous, Count::Exact(30))
        ));
        assert_eq!(parse(".-30d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::LastJump.into())));
        assert_eq!(parse("''d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::LastInserted.into())));
        assert_eq!(parse("'^d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::BufferLastExited.into())));
        assert_eq!(parse("'\"d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::BufferNamed('z').into())));
        assert_eq!(parse("'zd"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::GlobalNamed('A').into())));
        assert_eq!(parse("'Ad"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::GlobalLastExited(0).into())));
        assert_eq!(parse("'0d"), res!(range, "d", false, ""));

        let range = r!(e!(RangeEndingType::Mark(Mark::GlobalLastExited(9).into())));
        assert_eq!(parse("'9d"), res!(range, "d", false, ""));
    }

    #[test]
    fn test_ranges_double_comma() {
        let dot = e!(RangeEndingType::Current);
        let unspec = e!(RangeEndingType::Unspecified);

        let dotdot = r!(dot, unspec);
        assert_eq!(parse(".,d"), res!(dotdot, "d", false, ""));

        let dotdot = r!(unspec, dot);
        assert_eq!(parse(",.d"), res!(dotdot, "d", false, ""));

        let dotdot = r!(dot, dot);
        assert_eq!(parse(".,.d"), res!(dotdot, "d", false, ""));

        let lt = e!(RangeEndingType::Mark(Mark::VisualBegin.into()));
        let gt = e!(RangeEndingType::Mark(Mark::VisualEnd.into()));
        let range = r!(lt, gt);
        assert_eq!(parse("'<,'>d"), res!(range, "d", false, ""));

        let lb = e!(RangeEndingType::Mark(Mark::LastYankedBegin.into()));
        let rb = e!(RangeEndingType::Mark(Mark::LastYankedEnd.into()));
        let range = r!(lb, rb);
        assert_eq!(parse("'[,']d"), res!(range, "d", false, ""));
    }

    #[test]
    fn test_ranges_double_semicolon() {
        let dot = e!(RangeEndingType::Current);
        let unspec = e!(RangeEndingType::Unspecified);

        let dotdot = r!(dot, unspec, RangeSearchInit::Start);
        assert_eq!(parse(".;d"), res!(dotdot, "d", false, ""));

        let dotdot = r!(unspec, dot, RangeSearchInit::Start);
        assert_eq!(parse(";.d"), res!(dotdot, "d", false, ""));

        let dotdot = r!(dot, dot, RangeSearchInit::Start);
        assert_eq!(parse(".;.d"), res!(dotdot, "d", false, ""));

        let r = e!(RangeEndingType::Search(MoveDir1D::Next));
        let range = r!(dot, r, RangeSearchInit::Start);
        assert_eq!(parse(".;\\/d"), res!(range, "d", false, ""));

        let l = e!(
            RangeEndingType::Current,
            RangeEndingModifier::Offset(MoveDir1D::Previous, Count::Exact(5))
        );
        let r = e!(RangeEndingType::Search(MoveDir1D::Previous));
        let range = r!(l, r, RangeSearchInit::Start);
        assert_eq!(parse(".-5;\\?d"), res!(range, "d", false, ""));

        let l = e!(
            RangeEndingType::Current,
            RangeEndingModifier::Offset(MoveDir1D::Next, Count::Exact(60))
        );
        let r = e!(RangeEndingType::SubPatSearch(MoveDir1D::Next));
        let range = r!(l, r, RangeSearchInit::Start);
        assert_eq!(parse(".+60;\\&d"), res!(range, "d", false, ""));
    }

    #[test]
    fn test_extra_spaces() {
        assert_eq!(parse("w    file"), res!("w", false, "    file"));
        assert_eq!(parse("w\t\tfile"), res!("w", false, "\t\tfile"));
        assert_eq!(parse("   w file"), res!("w", false, " file"));
        assert_eq!(parse("w  file\n"), res!("w", false, "  file"));
        assert_eq!(parse(":  w file"), res!("w", false, " file"));
        assert_eq!(parse(" : w file"), res!("w", false, " file"));
        assert_eq!(parse("  :w file"), res!("w", false, " file"));
    }

    #[test]
    fn test_extra_colons() {
        assert_eq!(parse("w"), res!("w"));
        assert_eq!(parse(":w"), res!("w"));
        assert_eq!(parse("::::w"), res!("w"));
        assert_eq!(parse("::::w!"), res!("w", true));
        assert_eq!(parse("::::w! filename"), res!("w", true, " filename"));

        let range = r!(e!(RangeEndingType::All));
        assert_eq!(parse("::::%w! filename"), res!(range, "w", true, " filename"));

        let l = e!(RangeEndingType::Current);
        let r = e!(
            RangeEndingType::Unspecified,
            RangeEndingModifier::Offset(MoveDir1D::Next, Count::Exact(10))
        );
        let range = r!(l, r);
        assert_eq!(parse("::::.,+10w! filename"), res!(range, "w", true, " filename"));
    }
}
