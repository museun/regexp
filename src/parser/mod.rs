use std::borrow::Cow;
use std::fmt;
use std::{iter::Peekable, str::Chars};

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
    iter: Peekable<Chars<'a>>,
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
            iter: input.chars().peekable(),
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
            Some('.') => '.',

            Some('d') | Some('w') | Some('s') | Some('l') | Some('u') => {
                return self.metacharacter();
            }

            Some('D') | Some('W') | Some('S') | Some('L') | Some('U') => {
                return self.metacharacter();
            }

            Some(_c) => self.error(ErrorKind::UnknownEscape)?,
            _ => unreachable!(),
        };
        self.stack.push(Expr::Char(esc));
        self.advance();
        Ok(())
    }

    fn metacharset(&mut self, cs: &mut CharSet) {
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
        self.metacharset(&mut cs);
        self.advance();
        self.stack.push(Expr::CharSet(cs));
        Ok(())
    }

    fn parenthesis(&mut self) -> Result<()> {
        if let Some('?') = self.iter.peek() {
            self.advance(); // (
            match self.iter.peek() {
                Some(':') => {
                    self.paren += 1;
                    self.stack.push(Expr::LParen);
                    self.advance(); // ?
                    self.advance(); // :
                }
                Some('P') => {
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

        const META_CHARS: &[char] = &['d', 'w', 's', 'l', 'u', 'D', 'W', 'S', 'L', 'U'];

        let (mut escape, mut range) = (false, false);
        loop {
            match self.current {
                None => return self.error(ErrorKind::UnterminatedCharSet),
                Some(']') if !escape => break,
                Some('-') if !escape => {
                    if let Some(']') = self.iter.peek() {
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

                Some(ch) if escape && META_CHARS.contains(&ch) => {
                    self.metacharset(&mut chars);
                    self.advance();
                    escape = false;
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
            | Some(t @ Expr::Group(..))
            | Some(t @ Expr::CharSet(..)) => t,

            None => return self.error(ErrorKind::NothingToRepeat),
            _ => return self.error(ErrorKind::CannotRepeat),
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
        let concat = if terms.len() == 1 {
            terms.pop().unwrap()
        } else {
            Expr::Concatenation(terms)
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
        let concat = if terms.len() == 1 {
            terms.pop().unwrap()
        } else {
            Expr::Concatenation(terms)
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
        self.current = self.iter.next();
        self.pos += 1;
        self.current
    }

    fn integer(&mut self) -> Option<usize> {
        if !self.current?.is_ascii_digit() {
            return None;
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
mod tests;
