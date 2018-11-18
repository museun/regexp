use std::borrow::Cow;
use std::fmt;
use std::{iter::Peekable, str::CharIndices};

mod ast;
mod charset;
mod error;

pub use self::ast::{Expr, Expression, Repetition};
pub use self::charset::CharSet;
pub use self::error::{Error, ErrorKind};

type Result<T> = std::result::Result<T, Error>;

pub struct Parser<'a> {
    stack: Vec<Expr>,

    input: Cow<'a, str>,
    iter: Peekable<CharIndices<'a>>,
    current: Option<char>,
    pos: usize,

    paren: u8, // open parenthesis count

    bol: bool, // ^
    eol: bool, // $
}

impl<'a> fmt::Debug for Parser<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Parser")
            .field("input", &self.input)
            .field("stack", &self.stack)
            .field("current", &self.current)
            .field("paren", &self.paren)
            .field("bol", &self.bol)
            .field("eol", &self.eol)
            .finish()
    }
}

impl<'a> Parser<'a> {
    pub fn parse(input: &'a str) -> Result<Expression> {
        let mut this = Self {
            stack: vec![],

            input: input.into(),
            iter: input.char_indices().peekable(),
            current: None,
            pos: 0,

            paren: 0,

            bol: false,
            eol: false,
        };

        if this.advance().is_none() {
            return this.error(ErrorKind::EmptyRegex);
        }

        'exit: loop {
            let c = match this.current {
                Some(c) => c,
                None => break 'exit,
            };

            match c {
                '(' => this.parenthesis()?,
                ')' => this.group()?,
                '^' => {
                    if this.pos != 1 {
                        return this.error(ErrorKind::BolPosition);
                    }
                    this.bol = true;
                    this.advance();
                }
                '$' => {
                    if this.pos != input.len() {
                        return this.error(ErrorKind::EolPosition);
                    }
                    this.eol = true;
                    this.advance();
                }
                '[' => this.charset()?,
                '?' | '*' | '+' | '{' => this.repetition()?,
                '|' => this.alternation()?,
                '.' => {
                    this.stack.push(Expr::Any);
                    this.advance();
                }
                '\\' => this.escape()?,
                e => {
                    this.stack.push(Expr::Char(e));
                    this.advance();
                }
            }
        }

        this.end(false)?;

        if this.paren > 0 {
            return this.error(ErrorKind::UnmatchedParen);
        }

        Ok(Expression {
            expr: this.stack.pop().unwrap(),
            bol: this.bol,
            eol: this.eol,
        })
    }

    fn escape(&mut self) -> Result<()> {
        self.advance(); // \
        let esc = match self.current {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('^') => '^',
            Some('$') => '$',

            Some('d') | Some('w') | Some('s') | Some('l') | Some('u') => {
                return self.metacharacter()
            }

            Some('D') | Some('W') | Some('S') | Some('L') | Some('U') => {
                return self.metacharacter()
            }

            Some(_c) => self.error(ErrorKind::UnknownEscape)?,
            _ => unreachable!(),
        };
        self.stack.push(Expr::Char(esc));
        self.advance();
        Ok(())
    }

    fn metacharacter(&mut self) -> Result<()> {
        // \d -> [0-9]
        // \D -> [^0-9]
        // \w -> [A-Za-z0-9_]
        // \W -> [^A-Za-z0-9_]
        // \s -> [ \t\n\r\f]
        // \S -> [^ \t\n\r\f]
        // \l -> [a-z]
        // \L -> [^a-z]
        // \u -> [A-Z]
        // \U -> [^A-Z]

        let mut cs = CharSet::new();
        match self.current {
            Some(c @ 'd') | Some(c @ 'D') => {
                for n in b'0'..=b'9' {
                    cs.add(n as char);
                }
                if c == 'D' {
                    cs.complement();
                }
            }
            Some(c @ 'w') | Some(c @ 'W') => {
                for n in (b'A'..=b'Z').chain(b'a'..=b'z').chain(b'0'..=b'9') {
                    cs.add(n as char);
                }
                cs.add(b'_' as char);
                if c == 'W' {
                    cs.complement();
                }
            }
            Some(c @ 's') | Some(c @ 'S') => {
                cs.add(b' ' as char);
                for n in b'\t'..=b'\r' {
                    cs.add(n as char);
                }
                if c == 'S' {
                    cs.complement();
                }
            }
            Some(c @ 'l') | Some(c @ 'L') => {
                for n in b'a'..=b'z' {
                    cs.add(n as char);
                }
                if c == 'L' {
                    cs.complement();
                }
            }
            Some(c @ 'u') | Some(c @ 'U') => {
                for n in b'A'..=b'Z' {
                    cs.add(n as char);
                }
                if c == 'U' {
                    cs.complement();
                }
            }
            _ => unreachable!(),
        };

        self.advance();
        self.stack.push(Expr::CharSet(cs));
        Ok(())
    }

    fn parenthesis(&mut self) -> Result<()> {
        if let Some((_, '?')) = self.iter.peek() {
            self.advance(); // (
            match self.iter.peek() {
                Some((_, ':')) => {
                    self.paren += 1;
                    self.stack.push(Expr::LParen);
                    self.advance(); // ?
                    self.advance(); // :
                }
                Some((_, 'P')) => {
                    self.advance(); // P
                    if let Some('<') = self.advance() {
                        let mut name = String::new();
                        'named: loop {
                            if let Some(ch) = self.advance() {
                                if ch == '>' {
                                    break 'named;
                                }
                                if !ch.is_alphabetic() {
                                    return self.error(ErrorKind::UnfinishedName); // TODO end
                                }
                                name.push(ch)
                            } else {
                                return self.error(ErrorKind::UnfinishedName); // TODO end
                            }
                        }
                        self.paren += 1;
                        self.advance(); // >
                        self.stack.push(Expr::CaptureLParen(self.paren, Some(name)));
                    } else {
                        return self.error(ErrorKind::UnfinishedName); // TODO begin
                    }
                }
                _ => return self.error(ErrorKind::UnknownGroupFlag),
            }
        } else {
            self.paren += 1;
            self.stack.push(Expr::CaptureLParen(self.paren, None));
            self.advance();
        }

        Ok(())
    }

    fn group(&mut self) -> Result<()> {
        if self.paren == 0 {
            return self.error(ErrorKind::UnmatchedParen);
        }

        self.advance();
        self.end(true)?;

        let group = self.stack.pop().unwrap();
        match self.stack.pop() {
            Some(Expr::CaptureLParen(p, name)) => {
                let expr = Expr::CaptureGroup(p, name, Box::new(group));
                self.stack.push(expr)
            }
            Some(Expr::LParen) => self.stack.push(Expr::Group(Box::new(group))),
            _ => return self.error(ErrorKind::UnmatchedParen),
        }

        self.paren = self.paren.saturating_sub(1);
        Ok(())
    }

    fn charset(&mut self) -> Result<()> {
        self.advance();

        let mut chars = CharSet::new();
        let complement = if let Some('^') = self.current {
            self.advance();
            true
        } else {
            false
        };

        let mut prev = match self.current {
            Some(ch @ '-') | Some(ch @ ']') => {
                chars.add(ch);
                self.advance();
                Some(ch)
            }
            _ => None,
        };

        let (mut escape, mut range) = (false, false);
        loop {
            match self.current {
                None => return self.error(ErrorKind::UnterminatedCharSet),
                Some(']') if !escape => break,
                Some('-') if !escape => {
                    if let Some((_, ']')) = self.iter.peek() {
                        chars.add('-');
                        self.advance();
                        continue;
                    }
                    if prev.is_none() {
                        return self.error(ErrorKind::InvalidCharacterRange);
                    }
                    range = true;
                    self.advance();
                }
                Some('\\') if !escape => {
                    escape = true;
                    self.advance();
                }
                Some(ch) => {
                    if !chars.add(ch) {
                        return self.error(ErrorKind::InvalidEncoding);
                    }
                    if range {
                        let start = prev.unwrap();
                        if start as u8 > ch as u8 {
                            return self.error(ErrorKind::InvalidCharacterRange);
                        }
                        for b in start as u8 + 1..ch as u8 {
                            chars.add(b as char);
                        }
                        range = false;
                        prev = None;
                    } else {
                        prev = Some(ch)
                    }
                    escape = false;
                    self.advance();
                }
            }
        }

        if complement {
            chars.complement();
        }

        self.advance();
        self.stack.push(Expr::CharSet(chars));
        Ok(())
    }

    fn repetition(&mut self) -> Result<()> {
        let term = match self.stack.pop() {
            Some(t @ Expr::Char(..))
            | Some(t @ Expr::Any)
            | Some(t @ Expr::CaptureGroup(..))
            | Some(t @ Expr::CharSet(..)) => t,

            None => return self.error(ErrorKind::NothingToRepeat),
            _e => return self.error(ErrorKind::CannotRepeat),
        };

        let (min, mut max) = match self.current {
            Some(c) if c != '{' => {
                self.advance();
                match c {
                    '*' => (0, None),
                    '+' => (1, None),
                    _ => (0, Some(1)),
                }
            }
            // { } repetition
            _ => {
                self.advance();
                let min = self.integer().unwrap_or_default();
                let comma = match self.current {
                    Some(',') => {
                        self.advance();
                        true
                    }
                    _ => false,
                };

                let max = match self.integer() {
                    None if !comma => Some(min),
                    Some(max) if min > max => return self.error(ErrorKind::InvalidRepetition),
                    max => max,
                };

                match self.current {
                    Some('}') => self.advance(),
                    _ => return self.error(ErrorKind::InvalidRepetition),
                };

                (min, max)
            }
        };

        if let (0, Some(0)) = (min, max) {
            return self.error(ErrorKind::NothingToRepeat);
        }

        let greedy = if let Some('?') = self.current {
            self.advance();
            false
        } else {
            true
        };

        let mut concat = if min > 0 {
            if let Some(bound) = max {
                max = Some(bound - min)
            }
            Some(vec![term.clone(); min])
        } else {
            None
        };

        match max {
            None => {
                let rep = Expr::Repetition(Box::new(term), Repetition::Star, greedy);
                match concat {
                    None => self.stack.push(rep),
                    Some(ref mut list) => list.push(rep),
                }
            }
            // deterministic
            Some(0) => (),
            Some(bound) => {
                let mut rep =
                    Expr::Repetition(Box::new(term.clone()), Repetition::Question, greedy);
                for _ in 0..bound - 1 {
                    rep = Expr::Repetition(
                        Box::new(Expr::Concatenation(vec![term.clone(), rep])),
                        Repetition::Question,
                        greedy,
                    );
                }
                match concat {
                    None => self.stack.push(rep),
                    Some(ref mut list) => list.push(rep),
                }
            }
        };

        if let Some(concat) = concat {
            self.stack.push(Expr::Concatenation(concat));
        }
        Ok(())
    }

    fn alternation(&mut self) -> Result<()> {
        let mut terms = vec![];
        loop {
            match self.stack.pop() {
                None => {
                    self.push_alternation(terms, None)?;
                    break;
                }
                Some(p @ Expr::LParen) | Some(p @ Expr::CaptureLParen(..)) => {
                    self.stack.push(p);
                    self.push_alternation(terms, None)?;
                    break;
                }
                Some(Expr::Alternation(list)) => {
                    self.push_alternation(terms, Some(list))?;
                    break;
                }
                Some(t) => terms.push(t),
            }
        }

        self.advance();
        Ok(())
    }

    fn push_alternation(&mut self, mut terms: Vec<Expr>, exprs: Option<Vec<Expr>>) -> Result<()> {
        if terms.is_empty() {
            return self.error(ErrorKind::MissingAlternation);
        }

        terms.reverse();
        let concat = match terms.len() {
            1 => terms.pop().unwrap(),
            _ => Expr::Concatenation(terms),
        };

        match exprs {
            None => self.stack.push(Expr::Alternation(vec![concat])),
            Some(mut list) => {
                list.push(concat);
                self.stack.push(Expr::Alternation(list));
            }
        };

        Ok(())
    }

    fn end(&mut self, sub: bool) -> Result<()> {
        let mut terms = vec![];
        loop {
            match self.stack.pop() {
                None => {
                    if sub {
                        return self.error(ErrorKind::UnmatchedParen);
                    }
                    return self.push_end(terms, None);
                }
                Some(p @ Expr::CaptureLParen(..)) | Some(p @ Expr::LParen) => {
                    if !sub {
                        return self.error(ErrorKind::UnmatchedParen);
                    }
                    self.stack.push(p);
                    return self.push_end(terms, None);
                }
                Some(Expr::Alternation(list)) => {
                    return self.push_end(terms, Some(list));
                }
                Some(t) => terms.push(t),
            };
        }
    }

    fn push_end(&mut self, mut terms: Vec<Expr>, exprs: Option<Vec<Expr>>) -> Result<()> {
        if terms.is_empty() {
            return match exprs {
                Some(..) => self.error(ErrorKind::MissingAlternation)?,
                None => self.error(ErrorKind::EmptyRegex)?,
            };
        }

        terms.reverse();
        let concat = match terms.len() {
            1 => terms.pop().unwrap(),
            _ => Expr::Concatenation(terms),
        };

        match exprs {
            None => self.stack.push(concat),
            Some(mut list) => {
                list.push(concat);
                self.stack.push(Expr::Alternation(list))
            }
        }
        Ok(())
    }

    fn advance(&mut self) -> Option<char> {
        // println!("< {:?}", self.current);
        self.current = self.iter.next().map(|(_, s)| s);
        self.pos += 1;
        // println!("> {:?}", self.current);
        self.current
    }

    fn integer(&mut self) -> Option<usize> {
        match self.current {
            Some(t) if !t.is_ascii_digit() => return None,
            None => return None,
            _ => {}
        };

        let mut n = 0;
        while let Some(t) = self.current {
            if !t.is_ascii_digit() {
                break;
            }
            n = n * 10 + t.to_digit(10).unwrap();
            self.advance();
        }
        Some(n as usize)
    }

    // TODO fix the positions
    fn error<T>(&self, kind: ErrorKind) -> Result<T> {
        Err(Error {
            pos: self.pos,
            kind,
        })
    }
}

#[cfg(test)]
mod tests {
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

        macro_rules! parsed {
            ($input:expr, $ast:expr) => {
                let expr = Parser::parse($input).unwrap();
                assert_eq!(expr.expr, $ast)
            };
        }

        macro_rules! concat {
            ($($e:expr),*) => {
                Expr::Concatenation(vec![
                    $($e),*
                ])
            };
        }

        macro_rules! rep {
            ($e:expr, $r:expr) => {
                Expr::Repetition(
                    Box::new($e),
                    match $r {
                        '*' => self::Repetition::Star,
                        '?' => self::Repetition::Question,
                        _ => panic!("unknown"),
                    },
                    false,
                )
            };

            ($e:expr, $r:expr, $greedy:expr) => {
                Expr::Repetition(
                    Box::new($e),
                    match $r {
                        '*' => self::Repetition::Star,
                        '?' => self::Repetition::Question,
                        _ => panic!("unknown"),
                    },
                    $greedy,
                )
            };
        }

        parsed!(r"a", Char('a'));
        parsed!(r"\n", Char('\n'));
        parsed!(r"ab", concat!(Char('a'), Char('b')));
        parsed!(r"a+", concat!(Char('a'), rep!(Char('a'), '*', true)));
        parsed!(r"a*?", rep!(Char('a'), '*'));
        parsed!(r"a+?", concat!(Char('a'), rep!(Char('a'), '*', false)));

        use self::CharSet as CS;
        use self::Repetition::*;

        // beware: dragons beyond this point
        let table = &[
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

        for (input, expected) in table.iter() {
            assert_eq!(
                Parser::parse(input).unwrap().expr,
                *expected,
                "failure for: {}",
                input
            );
        }
    }
}
