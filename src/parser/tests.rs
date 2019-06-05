use super::*;
// TODO error positions are all wrong

// errors
#[test]
fn empty_regex() {
    for input in &["", "()", "(?:)"] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::EmptyRegex);
    }
}

#[test]
fn misplaced_bol() {
    let err = Parser::parse("a^").unwrap_err();
    assert_eq!(err.kind(), ErrorKind::BolPosition);
}

#[test]
fn misplaced_eol() {
    let err = Parser::parse("$a").unwrap_err();
    assert_eq!(err.kind(), ErrorKind::EolPosition);
}

#[test]
fn unknown_group_flag() {
    let err = Parser::parse("(?Q)").unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnknownGroupFlag);
}

#[test]
fn unknown_escape() {
    for input in &["\\n", "\\t", "\\^", "\\$"] {
        Parser::parse(input).unwrap();
    }
    let err = Parser::parse("\\?").unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnknownEscape);
}

#[test]
fn unfinished_name() {
    let err = Parser::parse("(?P<asdf)").unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnfinishedName);
}

#[test]
fn unmatched_paren() {
    for input in &["(a|b", "((a|b)", "a|b)", ")", "("] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::UnmatchedParen);
    }
}

#[test]
fn unterminated_char_set() {
    for input in &["[a-z", "[a-z][0-9", "[a\\", "[]"] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::UnterminatedCharSet);
    }
}

#[test]
fn invalid_character_range() {
    for input in &["[a-b-c]", "[z-a]"] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidCharacterRange);
    }
}

#[test]
fn invalid_encoding() {
    for input in &["[あ]", "[ㅆ]"] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidEncoding);
    }
}

#[test]
fn invalid_repetition() {
    for input in &["a{", "a{1b", "a{1,b", "a{5,3}"] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidRepetition);
    }
}

#[test]
fn nothing_to_repeat() {
    for input in &["a{}", "a{}?", "a{0}", "a{0}?", "a{0,0}", "a{0,0}?"] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::NothingToRepeat);
    }
}

#[test]
fn missing_alternation() {
    for input in &["a|", "|a"] {
        let err = Parser::parse(input).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::MissingAlternation);
    }
}

#[test]
fn metacharacter() {
    for (input, expected) in &[
        (r"\d", {
            let mut cs = CharSet::new();
            for n in b'0'..=b'9' {
                cs.add(n as char);
            }
            cs
        }),
        (r"\D", {
            let mut cs = CharSet::new();
            for n in b'0'..=b'9' {
                cs.add(n as char);
            }
            cs.complement();
            cs
        }),
        (r"\w", {
            let mut cs = CharSet::new();
            for n in (b'A'..=b'Z').chain(b'a'..=b'z').chain(b'0'..=b'9') {
                cs.add(n as char);
            }
            cs.add(b'_' as char);
            cs
        }),
        (r"\W", {
            let mut cs = CharSet::new();
            for n in (b'A'..=b'Z').chain(b'a'..=b'z').chain(b'0'..=b'9') {
                cs.add(n as char);
            }
            cs.add(b'_' as char);
            cs.complement();
            cs
        }),
        (r"\s", {
            let mut cs = CharSet::new();
            cs.add(b' ' as char);
            for n in b'\t'..=b'\r' {
                cs.add(n as char);
            }
            cs
        }),
        (r"\S", {
            let mut cs = CharSet::new();
            cs.add(b' ' as char);
            for n in b'\t'..=b'\r' {
                cs.add(n as char);
            }
            cs.complement();
            cs
        }),
        (r"\l", {
            let mut cs = CharSet::new();
            for n in b'a'..=b'z' {
                cs.add(n as char);
            }
            cs
        }),
        (r"\L", {
            let mut cs = CharSet::new();
            for n in b'a'..=b'z' {
                cs.add(n as char);
            }
            cs.complement();
            cs
        }),
        (r"\u", {
            let mut cs = CharSet::new();
            for n in b'A'..=b'Z' {
                cs.add(n as char);
            }
            cs
        }),
        (r"\U", {
            let mut cs = CharSet::new();
            for n in b'A'..=b'Z' {
                cs.add(n as char);
            }
            cs.complement();
            cs
        }),
    ] {
        assert_eq!(
            Parser::parse(input).unwrap().expr,
            Expr::CharSet(*expected),
            "failure for: {}",
            input
        );
    }
}

#[test]
fn parsed() {
    use self::Expr::*;
    use self::Repetition::*;
    use self::CharSet as CS;

    let table = &[
        (r"a", Char('a')),
        (r"\n", Char('\n')),
        (r"ab", Expr::Concatenation(vec![Char('a'), Char('b')])),
        (
            r"a+",
            Expr::Concatenation(vec![
                Char('a'),
                Expr::Repetition(Box::new(Char('a')), Star, true),
            ]),
        ),
        (r"a*?", Expr::Repetition(Box::new(Char('a')), Star, false)),
        (
            r"a+?",
            Expr::Concatenation(vec![
                Char('a'),
                Expr::Repetition(Box::new(Char('a')), Star, false),
            ]),
        ),
        (r"a??", Repetition(Box::new(Char('a')), Question, false)),
        (
            r"a+b*",
            Concatenation(vec![
                Concatenation(vec![Char('a'), Repetition(Box::new(Char('a')), Star, true)]),
                Repetition(Box::new(Char('b')), Star, true),
            ]),
        ),
        (
            r"ab*",
            Concatenation(vec![Char('a'), Repetition(Box::new(Char('b')), Star, true)]),
        ),
        (
            r"a?b",
            Concatenation(vec![
                Repetition(Box::new(Char('a')), Question, true),
                Char('b'),
            ]),
        ),
        (
            r"a+|b",
            Alternation(vec![
                Concatenation(vec![Char('a'), Repetition(Box::new(Char('a')), Star, true)]),
                Char('b'),
            ]),
        ),
        (
            r"a+|b*",
            Alternation(vec![
                Concatenation(vec![Char('a'), Repetition(Box::new(Char('a')), Star, true)]),
                Repetition(Box::new(Char('b')), Star, true),
            ]),
        ),
        (r".", Any),
        (r"a..b", Concatenation(vec![Char('a'), Any, Any, Char('b')])),
        (r"(a)", CaptureGroup(1, None, Box::new(Char('a')))),
        (
            r"(ab)",
            CaptureGroup(1, None, Box::new(Concatenation(vec![Char('a'), Char('b')]))),
        ),
        (
            r"(ab)+",
            Concatenation(vec![
                CaptureGroup(1, None, Box::new(Concatenation(vec![Char('a'), Char('b')]))),
                Repetition(
                    Box::new(CaptureGroup(
                        1,
                        None,
                        Box::new(Concatenation(vec![Char('a'), Char('b')])),
                    )),
                    Star,
                    true,
                ),
            ]),
        ),
        (
            r"(a(bc)?)+",
            Concatenation(vec![
                CaptureGroup(
                    1,
                    None,
                    Box::new(Concatenation(vec![
                        Char('a'),
                        Repetition(
                            Box::new(CaptureGroup(
                                2,
                                None,
                                Box::new(Concatenation(vec![Char('b'), Char('c')])),
                            )),
                            Question,
                            true,
                        ),
                    ])),
                ),
                Repetition(
                    Box::new(CaptureGroup(
                        1,
                        None,
                        Box::new(Concatenation(vec![
                            Char('a'),
                            Repetition(
                                Box::new(CaptureGroup(
                                    2,
                                    None,
                                    Box::new(Concatenation(vec![Char('b'), Char('c')])),
                                )),
                                Question,
                                true,
                            ),
                        ])),
                    )),
                    Star,
                    true,
                ),
            ]),
        ),
        (
            r"(a+|b*|cd?)",
            CaptureGroup(
                1,
                None,
                Box::new(Alternation(vec![
                    Concatenation(vec![Char('a'), Repetition(Box::new(Char('a')), Star, true)]),
                    Repetition(Box::new(Char('b')), Star, true),
                    Concatenation(vec![
                        Char('c'),
                        Repetition(Box::new(Char('d')), Question, true),
                    ]),
                ])),
            ),
        ),
        (
            r"(a)(b)",
            Concatenation(vec![
                CaptureGroup(1, None, Box::new(Char('a'))),
                CaptureGroup(1, None, Box::new(Char('b'))),
            ]),
        ),
        (
            r"(a|b)(c|d)",
            Concatenation(vec![
                CaptureGroup(1, None, Box::new(Alternation(vec![Char('a'), Char('b')]))),
                CaptureGroup(1, None, Box::new(Alternation(vec![Char('c'), Char('d')]))),
            ]),
        ),
        (
            r"(a)|(b)",
            Alternation(vec![
                CaptureGroup(1, None, Box::new(Char('a'))),
                CaptureGroup(1, None, Box::new(Char('b'))),
            ]),
        ),
        (
            r"(((test?)))",
            CaptureGroup(
                1,
                None,
                Box::new(CaptureGroup(
                    2,
                    None,
                    Box::new(CaptureGroup(
                        3,
                        None,
                        Box::new(Concatenation(vec![
                            Char('t'),
                            Char('e'),
                            Char('s'),
                            Repetition(Box::new(Char('t')), Question, true),
                        ])),
                    )),
                )),
            ),
        ),
        (
            r"(?P<a>a)",
            CaptureGroup(1, Some("a".into()), Box::new(Char('a'))),
        ),
        (
            r"(?P<a>a|b)",
            CaptureGroup(
                1,
                Some("a".into()),
                Box::new(Alternation(vec![Char('a'), Char('b')])),
            ),
        ),
        (
            r"(?P<a>a|b)",
            CaptureGroup(
                1,
                Some("a".into()),
                Box::new(Alternation(vec![Char('a'), Char('b')])),
            ),
        ),
        (
            r"(?P<a>a|(b))",
            CaptureGroup(
                1,
                Some("a".into()),
                Box::new(Alternation(vec![
                    Char('a'),
                    CaptureGroup(2, None, Box::new(Char('b'))),
                ])),
            ),
        ),
        (
            r"(?P<a>a|(?P<b>b))",
            CaptureGroup(
                1,
                Some("a".into()),
                Box::new(Alternation(vec![
                    Char('a'),
                    CaptureGroup(2, Some("b".into()), Box::new(Char('b'))),
                ])),
            ),
        ),
        (
            r"(?:a(?P<a>b))",
            Group(Box::new(Concatenation(vec![
                Char('a'),
                CaptureGroup(2, Some("a".into()), Box::new(Char('b'))),
            ]))),
        ),
        (
            r"^abc",
            Concatenation(vec![Char('a'), Char('b'), Char('c')]),
        ),
        (
            r"abc$",
            Concatenation(vec![Char('a'), Char('b'), Char('c')]),
        ),
        (
            r"^abc$",
            Concatenation(vec![Char('a'), Char('b'), Char('c')]),
        ),
        (
            r"(:?abc)",
            CaptureGroup(
                1,
                None,
                Box::new(Concatenation(vec![
                    Repetition(Box::new(Char(':')), Question, true),
                    Char('a'),
                    Char('b'),
                    Char('c'),
                ])),
            ),
        ),
        (
            r"(:?ab(c))",
            CaptureGroup(
                1,
                None,
                Box::new(Concatenation(vec![
                    Repetition(Box::new(Char(':')), Question, true),
                    Char('a'),
                    Char('b'),
                    CaptureGroup(2, None, Box::new(Char('c'))),
                ])),
            ),
        ),
        (
            r"[abc]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('b');
                cs.add('c');
                cs
            }),
        ),
        (
            r"[^abc]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('b');
                cs.add('c');
                cs.complement();
                cs
            }),
        ),
        (
            r"[a-c]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('b');
                cs.add('c');
                cs
            }),
        ),
        (
            r"[a-ca-e]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('b');
                cs.add('c');

                cs.add('a');
                cs.add('b');
                cs.add('c');
                cs.add('d');
                cs.add('e');
                cs
            }),
        ),
        (
            r"[a-cx-z]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('b');
                cs.add('c');

                cs.add('x');
                cs.add('y');
                cs.add('z');
                cs
            }),
        ),
        (
            r"[a-c123x-z]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('b');
                cs.add('c');

                cs.add('1');
                cs.add('2');
                cs.add('3');

                cs.add('x');
                cs.add('y');
                cs.add('z');
                cs
            }),
        ),
        (
            r"[]a]",
            CharSet({
                let mut cs = CS::new();
                cs.add(']');
                cs.add('a');
                cs
            }),
        ),
        (
            r"[a\]b]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add(']');
                cs.add('b');
                cs
            }),
        ),
        (
            r"[a^b]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('^');
                cs.add('b');
                cs
            }),
        ),
        (
            r"[-ab]",
            CharSet({
                let mut cs = CS::new();
                cs.add('-');
                cs.add('a');
                cs.add('b');
                cs
            }),
        ),
        (
            r"[a\-b]",
            CharSet({
                let mut cs = CS::new();
                cs.add('a');
                cs.add('-');
                cs.add('b');
                cs
            }),
        ),
        (r"a{,}", Repetition(Box::new(Char('a')), Star, true)),
        (r"a{,}?", Repetition(Box::new(Char('a')), Star, false)),
        (
            r"a{3}",
            Concatenation(vec![Char('a'), Char('a'), Char('a')]),
        ),
        (
            r"a{3}?",
            Concatenation(vec![Char('a'), Char('a'), Char('a')]),
        ),
        (
            r"a{3,}?",
            Concatenation(vec![
                Char('a'),
                Char('a'),
                Char('a'),
                Repetition(Box::new(Char('a')), Star, false),
            ]),
        ),
        (
            r"a{,3}",
            Repetition(
                Box::new(Concatenation(vec![
                    Char('a'),
                    Repetition(
                        Box::new(Concatenation(vec![
                            Char('a'),
                            Repetition(Box::new(Char('a')), Question, true),
                        ])),
                        Question,
                        true,
                    ),
                ])),
                Question,
                true,
            ),
        ),
        (
            r"a{,3}?",
            Repetition(
                Box::new(Concatenation(vec![
                    Char('a'),
                    Repetition(
                        Box::new(Concatenation(vec![
                            Char('a'),
                            Repetition(Box::new(Char('a')), Question, false),
                        ])),
                        Question,
                        false,
                    ),
                ])),
                Question,
                false,
            ),
        ),
        (
            r"a{1,3}",
            Concatenation(vec![
                Char('a'),
                Repetition(
                    Box::new(Concatenation(vec![
                        Char('a'),
                        Repetition(Box::new(Char('a')), Question, true),
                    ])),
                    Question,
                    true,
                ),
            ]),
        ),
        (
            r"a{1,3}?",
            Concatenation(vec![
                Char('a'),
                Repetition(
                    Box::new(Concatenation(vec![
                        Char('a'),
                        Repetition(Box::new(Char('a')), Question, false),
                    ])),
                    Question,
                    false,
                ),
            ]),
        ),
    ];

    macro_rules! parsed {
        ($input:expr, $ast:expr) => {
            let expr = Parser::parse($input).unwrap();
            assert_eq!(expr.expr, $ast, "failure for: {}", $input)
        };
    }

    for (input, expected) in table.iter() {
        eprintln!("parsing: {}", input);
        parsed!(input, *expected);
    }
}
