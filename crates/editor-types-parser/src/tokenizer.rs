use std::borrow::Cow;

use logos::Logos;

use crate::error::*;
use crate::{ActionToken, Flag};

impl LexingError {
    fn from_lexer<'a>(lex: &mut logos::Lexer<'a, LexToken<'a>>) -> Self {
        let offset = lex.span().start;
        let remaining = lex.slice();
        if remaining.len() > 10 {
            let idx = remaining.char_indices().nth(10).map(|i| i.0).unwrap_or(0);
            LexingError::UnexpectedSequence(offset, format!("{}...", &remaining[..idx]))
        } else {
            LexingError::UnexpectedSequence(offset, remaining.to_string())
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(error(LexingError, LexingError::from_lexer))]
enum LexToken<'a> {
    // Explicit booleans have higher priority than Words
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),

    #[regex(r"[0-9][0-9a-zA-Z]*", parse_num)]
    Number(usize),

    // Matches {id} or {}
    #[regex(r"\{[a-zA-Z_][a-zA-Z0-9_]*\}|\{\}", parse_id)]
    Id(Option<Cow<'a, str>>),

    // Matches quoted strings with basic escape sequence support
    #[regex(r#""(?:[^"\\]|\\.|\\u\{[0-9a-fA-F]{1,6}\})*""#, parse_string)]
    Str(Cow<'a, str>),

    // Matches single-quoted chars with basic escape sequence support
    #[regex(r#"'(?:[^'\\]|\\.|\\u\{[0-9a-fA-F]{1,6}\})'"#, parse_char)]
    Char(char),

    // Command-line style flags
    #[regex(r"--[a-zA-Z0-9_-]+", parse_long_flag)]
    #[regex(r"-[a-zA-Z]", parse_short_flag)]
    Flag(Flag),

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    // Matches bare words (identifiers). Placed at the bottom, Logos will
    // fall back to this for alphabetic strings that aren't "true" or "false".
    #[regex(r"[a-zA-Z_][-a-zA-Z0-9_]*", |lex| lex.slice())]
    Word(&'a str),
}

fn parse_num<'a>(lex: &mut logos::Lexer<'a, LexToken<'a>>) -> Result<usize, LexingError> {
    let input = lex.slice();

    let res = if let Some(rest) = input.strip_prefix("0x") {
        usize::from_str_radix(rest, 16)?
    } else if let Some(rest) = input.strip_prefix("0o") {
        usize::from_str_radix(rest, 8)?
    } else if let Some(rest) = input.strip_prefix("0b") {
        usize::from_str_radix(rest, 2)?
    } else {
        input.parse()?
    };

    Ok(res)
}

fn parse_id<'a>(lex: &mut logos::Lexer<'a, LexToken<'a>>) -> Option<Cow<'a, str>> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    if inner.is_empty() {
        None
    } else {
        Some(Cow::Borrowed(inner))
    }
}

fn unescape<'a>(s: &'a str) -> Result<Cow<'a, str>, InvalidEscapeError> {
    unescape_zero_copy::unescape_default(s).map_err(InvalidEscapeError)
}

fn parse_string<'a>(lex: &mut logos::Lexer<'a, LexToken<'a>>) -> Result<Cow<'a, str>, LexingError> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    Ok(unescape(inner)?)
}

fn parse_char<'a>(lex: &mut logos::Lexer<'a, LexToken<'a>>) -> Result<char, LexingError> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    let s = unescape(inner)?;
    s.chars().next().ok_or(LexingError::BadInput)
}

fn parse_long_flag<'a>(lex: &mut logos::Lexer<'a, LexToken<'a>>) -> Flag {
    let slice = lex.slice();
    match slice {
        "--count" => Flag::Count,
        "--dir" => Flag::Dir,
        "--focus" => Flag::Focus,
        "--input" => Flag::Input,
        "--mark" => Flag::Mark,
        "--position" => Flag::Position,
        "--style" => Flag::Style,
        "--target" => Flag::Target,
        "--wrap" => Flag::Wrap,
        _ => Flag::Long(slice[2..].to_string()),
    }
}

fn parse_short_flag<'a>(lex: &mut logos::Lexer<'a, LexToken<'a>>) -> Flag {
    let slice = lex.slice();
    let c = slice.chars().nth(1).unwrap();
    match c {
        'c' => Flag::Count,
        'd' => Flag::Dir,
        'f' => Flag::Focus,
        'i' => Flag::Input,
        'm' => Flag::Mark,
        'p' => Flag::Position,
        's' => Flag::Style,
        't' => Flag::Target,
        'w' => Flag::Wrap,
        _ => Flag::Short(c),
    }
}

pomelo::pomelo! {
    %module parser;
    %include {
        use super::{ActionToken, Cow, Flag, TokenTreeError};
    }

    %error TokenTreeError;
    %parse_fail { TokenTreeError::ParseFailed }
    %stack_overflow { TokenTreeError::TooDeeplyNested }
    %syntax_error {
        let wanted = expected.map(|t| t.name.to_string()).collect();
        Err(TokenTreeError::SyntaxError(wanted))
    }

    %stack_size 100;

    // The Pomelo-generated token type for the grammar:
    %token #[derive(Debug)] pub enum Token<'a> {};

    %type Word &'a str;
    %type Flag Flag;
    %type Str Cow<'a, str>;
    %type Id Option<Cow<'a, str>>;
    %type Bool bool;
    %type Number usize;
    %type Char char;

    // AST bindings
    %type input Vec<ActionToken<'a>>;
    %type exprs Vec<ActionToken<'a>>;
    %type expr ActionToken<'a>;

    // Start symbol rule:
    input ::= exprs(E) { E };

    // Top-level and group list accumulation:
    exprs ::= exprs(mut E) expr(X) { E.push(X); E };
    exprs ::= { Vec::with_capacity(32) };

    // Base DSL types:
    expr ::= Word(W) { ActionToken::Word(W) }
    expr ::= Flag(F) { ActionToken::Flag(F) }
    expr ::= Str(S) { ActionToken::Str(S) }
    expr ::= Id(I) { ActionToken::Id(I) }
    expr ::= Bool(B) { ActionToken::Bool(B) }
    expr ::= Number(N) { ActionToken::Number(N) }
    expr ::= Char(C) { ActionToken::Char(C) }

    // Nested groups within parentheses:
    expr ::= LParen exprs(E) RParen { ActionToken::Group(E) };
}

/// Parse the DSL input into a series of `ActionToken` values.
pub fn tokenize(input: &str) -> Result<Vec<ActionToken<'_>>, Error> {
    let mut p = parser::Parser::new();
    let lex = LexToken::lexer(input);

    for res in lex {
        let token = match res? {
            LexToken::Word(w) => parser::Token::Word(w),
            LexToken::Flag(f) => parser::Token::Flag(f),
            LexToken::Str(s) => parser::Token::Str(s),
            LexToken::Id(id) => parser::Token::Id(id),
            LexToken::Bool(b) => parser::Token::Bool(b),
            LexToken::Number(n) => parser::Token::Number(n),
            LexToken::Char(c) => parser::Token::Char(c),
            LexToken::LParen => parser::Token::LParen,
            LexToken::RParen => parser::Token::RParen,
        };

        p.parse(token)?;
    }

    Ok(p.end_of_input()?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::Strategy;

    #[derive(Clone, Debug)]
    pub enum ArbitraryToken {
        Bool(bool),
        Char(char),
        Flag(Flag),
        Id(Option<String>),
        Number(usize),
        Str(String),
        Word(String),
        Group(Vec<ArbitraryToken>),
    }

    fn arbitrary_token() -> impl Strategy<Value = ArbitraryToken> {
        use proptest::prelude::*;
        use proptest::string::string_regex;

        let leaf = prop_oneof![
            any::<bool>().prop_map(ArbitraryToken::Bool),
            any::<char>().prop_map(ArbitraryToken::Char),
            any::<Flag>().prop_map(ArbitraryToken::Flag),
            Just(ArbitraryToken::Id(None)),
            string_regex("[a-zA-Z_][a-zA-Z0-9_]*")
                .unwrap()
                .prop_map(|s| ArbitraryToken::Id(Some(s))),
            any::<usize>().prop_map(ArbitraryToken::Number),
            any::<String>().prop_map(ArbitraryToken::Str),
            string_regex("[a-zA-Z_][-a-zA-Z0-9_]*")
                .unwrap()
                .prop_map(ArbitraryToken::Word),
        ];

        leaf.prop_recursive(
            8,
            256,
            10,
            |inner| prop_oneof![
            prop::collection::vec(inner.clone(), 0..10).prop_map(ArbitraryToken::Group),
        ])
    }

    impl<'a> From<&'a ArbitraryToken> for ActionToken<'a> {
        fn from(token: &'a ArbitraryToken) -> Self {
            match token {
                ArbitraryToken::Word(s) => ActionToken::Word(s.as_str()),
                ArbitraryToken::Flag(f) => Self::Flag(f.clone()),
                ArbitraryToken::Str(s) => Self::Str(Cow::Borrowed(s.as_str())),
                ArbitraryToken::Id(id) => Self::Id(id.as_deref().map(Cow::Borrowed)),
                ArbitraryToken::Bool(b) => Self::Bool(*b),
                ArbitraryToken::Number(n) => Self::Number(*n),
                ArbitraryToken::Char(c) => Self::Char(*c),
                ArbitraryToken::Group(tokens) => {
                    Self::Group(tokens.iter().map(ActionToken::from).collect())
                },
            }
        }
    }

    #[test]
    fn test_roundtrips() {
        proptest::proptest!(|(sample in arbitrary_token())| {
            let token = ActionToken::from(&sample);
            let s = token.to_string();
            let parsed = tokenize(&s).unwrap();
            proptest::prop_assert_eq!(&parsed[0], &token, "failed to parse {:?} into original input", s);
            proptest::prop_assert_eq!(parsed.len(), 1);
        });
    }

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
            ActionToken::Id(Some(Cow::Borrowed("count"))),
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

    #[test]
    fn test_tokenize_missing_quote_escape() {
        let err = tokenize(r#"insert type -i '''"#).unwrap_err();
        assert_eq!(err, Error::Lexing(LexingError::UnexpectedSequence(15, "\'".into())));
    }

    #[test]
    fn test_tokenize_missing_end_quote() {
        let err = tokenize(r#"insert type -i 'a"#).unwrap_err();
        assert_eq!(err, Error::Lexing(LexingError::UnexpectedSequence(15, "'a".into())));
    }

    #[test]
    fn test_tokenize_char_escapes() {
        let exp = vec![
            ActionToken::Word("insert"),
            ActionToken::Word("type"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Char('\''),
        ];

        assert_eq!(tokenize(r#"insert type -i '\''"#).unwrap(), exp);

        let exp = vec![
            ActionToken::Word("insert"),
            ActionToken::Word("type"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Char('"'),
        ];

        assert_eq!(tokenize(r#"insert type -i '"'"#).unwrap(), exp);
        assert_eq!(tokenize(r#"insert type -i '\"'"#).unwrap(), exp);
    }

    #[test]
    fn test_tokenize_str_escapes() {
        let tokens = tokenize(r#"command run -i "it\'s \"quoted\"""#).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("command"),
            ActionToken::Word("run"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Str("it's \"quoted\"".into()),
        ]);
    }

    #[test]
    fn test_tokenize_empty_string() {
        let tokens = tokenize(r#"command run -i """#).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("command"),
            ActionToken::Word("run"),
            ActionToken::Flag(Flag::Input),
            ActionToken::Str(Cow::Borrowed("")),
        ]);

        let tokens = tokenize(r#"cmdbar focus -P "" -s command -a nop"#).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("cmdbar"),
            ActionToken::Word("focus"),
            ActionToken::Flag(Flag::Short('P')),
            ActionToken::Str(Cow::Borrowed("")),
            ActionToken::Flag(Flag::Style),
            ActionToken::Word("command"),
            ActionToken::Flag(Flag::Short('a')),
            ActionToken::Word("nop"),
        ]);
    }

    #[test]
    fn test_tokenize_id_ident() {
        // Rust identifiers can contain underscores and numbers in the right position:
        for id in ["my_mark", "_mark", "mark2", "m_", "m"] {
            let input = format!("mark -m {{{id}}}");
            let tokens = tokenize(&input).unwrap();
            assert_eq!(tokens, vec![
                ActionToken::Word("mark"),
                ActionToken::Flag(Flag::Mark),
                ActionToken::Id(Some(Cow::Borrowed(id))),
            ]);
        }

        // But they still have to be valid Rust identifiers:
        for id in ["1mark", "my-mark", "my mark"] {
            assert!(tokenize(&format!("mark -m {{{id}}}")).is_err(), "{id:?} should be invalid");
        }
    }

    #[test]
    fn test_tokenize_bool() {
        let tokens = tokenize("column -d next --multiline false").unwrap();
        assert_eq!(tokens.last(), Some(&ActionToken::Bool(false)));

        let tokens = tokenize("edit -t (range -T buffer --inclusive true)").unwrap();
        let ActionToken::Group(group) = &tokens[2] else {
            panic!("expected a group, got {:?}", tokens[2]);
        };
        assert_eq!(group.last(), Some(&ActionToken::Bool(true)));
    }

    #[test]
    fn test_tokenize_bool_prefixed_word() {
        for w in ["truecolor", "true-color", "falsey"] {
            let input = format!("edit -t (range -T (word -s {w}) --inclusive true)");
            let tokens = tokenize(&input).unwrap();
            assert_eq!(tokens, vec![
                ActionToken::Word("edit"),
                ActionToken::Flag(Flag::Target),
                ActionToken::Group(vec![
                    ActionToken::Word("range"),
                    ActionToken::Flag(Flag::Short('T')),
                    ActionToken::Group(vec![
                        ActionToken::Word("word"),
                        ActionToken::Flag(Flag::Style),
                        ActionToken::Word(w),
                    ]),
                    ActionToken::Flag(Flag::Long("inclusive".into())),
                    ActionToken::Bool(true),
                ]),
            ]);
        }
    }

    #[test]
    fn test_tokenize_prefixed_numbers() {
        for (input, n) in [
            ("0x0", 0),
            ("0o0", 0),
            ("0b0", 0),
            ("0x1", 1),
            ("0o1", 1),
            ("0b1", 1),
            ("0x10", 16),
            ("0o10", 8),
            ("0b10", 2),
            ("0x1f", 31),
            ("0o17", 15),
            ("0b11", 3),
        ] {
            let input = format!("window close -c {input}");
            let tokens = tokenize(&input).unwrap();
            assert_eq!(
                tokens,
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("close"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::Number(n),
                ],
                "failed to parse {input:?} to {n}"
            );
        }
    }

    #[test]
    fn test_tokenize_large_number() {
        let n = u32::MAX as usize + 1;
        let input = format!("window close -c {n}");
        let tokens = tokenize(&input).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("window"),
            ActionToken::Word("close"),
            ActionToken::Flag(Flag::Count),
            ActionToken::Number(n),
        ]);

        let n = usize::MAX;
        let input = format!("window close -c {n}");
        let tokens = tokenize(&input).unwrap();
        assert_eq!(tokens, vec![
            ActionToken::Word("window"),
            ActionToken::Word("close"),
            ActionToken::Flag(Flag::Count),
            ActionToken::Number(n),
        ]);
    }

    #[test]
    fn test_tokenize_invalid_numbers() {
        let huge = "9".repeat(40);
        let err = tokenize(&format!("window close -c {huge}")).unwrap_err();
        assert!(matches!(err, Error::Lexing(LexingError::InvalidNumber(..))));

        let err = tokenize("window close -c 5x").unwrap_err();
        assert!(matches!(err, Error::Lexing(LexingError::InvalidNumber(..))));
    }

    #[test]
    fn test_tokenize_error_display() {
        let err = tokenize("mark -m {my mark}").unwrap_err();
        assert_eq!(err, Error::Lexing(LexingError::UnexpectedSequence(8, "{my".into())));

        let err = tokenize("command run -i \"unterminated").unwrap_err();
        assert_eq!(
            err,
            Error::Lexing(LexingError::UnexpectedSequence(15, "\"untermina...".into()))
        );
    }

    #[test]
    fn test_tokenize_group_depth() {
        const MAX_GROUP_DEPTH: usize = 48;

        fn nest(n: usize, inner: &str) -> String {
            format!("{}{inner}{}", "(".repeat(n), ")".repeat(n))
        }

        // Verify the current nesting limit:
        let input = nest(MAX_GROUP_DEPTH, "buffer");
        let tokens = tokenize(&input).unwrap();

        let mut token = &tokens[0];
        let mut depth = 0;

        while let ActionToken::Group(group) = token {
            depth += 1;
            token = &group[0];
        }

        assert_eq!(depth, MAX_GROUP_DEPTH);
        assert_eq!(token, &ActionToken::Word("buffer"));

        // And then verify that going past that returns an error instead of crashing:
        let input = nest(MAX_GROUP_DEPTH + 1, "buffer");
        let err = tokenize(&input).unwrap_err();
        assert_eq!(err, Error::TokenTree(TokenTreeError::TooDeeplyNested));

        // And then pass a really large group to show that the input doesn't break anything:
        let input = nest(100_000, "buffer");
        assert_eq!(
            tokenize(&input).unwrap_err(),
            Error::TokenTree(TokenTreeError::TooDeeplyNested)
        );

        // An unbalanced run of parentheses also results in `TooDeeplyNested` before realizing
        // that there are missing `)` characters:
        let input = "(".repeat(100_000);
        assert_eq!(
            tokenize(&input).unwrap_err(),
            Error::TokenTree(TokenTreeError::TooDeeplyNested)
        );

        // The %stack_size limit is about nesting level, and overall ActionToken::Groups:
        let group = nest(MAX_GROUP_DEPTH, "buffer");
        let input = vec![group.as_str(); 50].join(" ");
        assert_eq!(tokenize(&input).unwrap().len(), 50);
    }
}
