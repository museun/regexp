use crate::{Error, Result};

use std::borrow::Cow;
use std::fmt;
use std::{iter::Peekable, str::CharIndices};

mod ast;
mod charset;

pub use self::ast::{Expr, Expression, Repetition};
pub use self::charset::CharSet;

pub struct Parser<'a> {
    stack: Vec<Expr>,

    input: Cow<'a, str>,
    iter: Peekable<CharIndices<'a>>,
    current: Option<char>,
    pos: usize,

    paren: u8,

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
            return Err(Error::EmptyRegex);
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
                        return Err(Error::BolPosition);
                    }
                    this.bol = true;
                    this.advance();
                }
                '$' => {
                    if this.pos != input.len() {
                        return Err(Error::EolPosition);
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
        Ok(Expression {
            expr: this.stack.pop().unwrap(),
            bol: this.bol,
            eol: this.eol,
        })
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

    fn escape(&mut self) -> Result<()> {
        self.advance(); // \
        let esc = match self.current {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('^') => '^',
            Some('$') => '$',
            // TODO meta characters
            Some(c) => unimplemented!("{}", c),
            _ => unreachable!(),
        };
        self.stack.push(Expr::Char(esc));
        self.advance();
        Ok(())
    }

    fn parenthesis(&mut self) -> Result<()> {
        if let Some((_, '?')) = self.iter.peek() {
            self.advance(); // (
            match self.iter.peek() {
                Some((_, ':')) => {
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
                                name.push(ch)
                            } else {
                                return Err(Error::UnfinishedName); // TODO end
                            }
                        }
                        self.paren += 1;
                        self.advance(); // >
                        self.stack.push(Expr::CaptureLParen(self.paren, Some(name)));
                    } else {
                        return Err(Error::UnfinishedName); // TODO begin
                    }
                }
                _ => return Err(Error::UnknownGroupFlag),
            }
        } else {
            self.paren += 1;
            self.stack.push(Expr::CaptureLParen(self.paren, None));
            self.advance();
        }

        Ok(())
    }

    fn group(&mut self) -> Result<()> {
        self.advance();
        self.end(true)?;

        // this is wrong, it needs to pop twice
        let group = self.stack.pop().unwrap();
        match self.stack.pop() {
            Some(Expr::CaptureLParen(p, name)) => {
                self.stack
                    .push(Expr::CaptureGroup(p, name, Box::new(group)))
            }
            Some(Expr::LParen) => self.stack.push(Expr::Group(Box::new(group))),
            _ => return Err(Error::UnmatchedParen),
        }

        Ok(())
    }

    fn charset(&mut self) -> Result<()> {
        self.advance();
        let mut chars = CharSet::new();
        let complement = if let Some('^') = self.current {
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

        let mut escape = false;
        let mut range = false;
        loop {
            match self.current {
                None => return Err(Error::UnterminatedCharSet),
                Some(']') if !escape => break,
                Some('-') if !escape => {
                    if let Some((_, ']')) = self.iter.peek() {
                        chars.add('-');
                        self.advance();
                        continue;
                    }
                    if prev.is_none() {
                        return Err(Error::InvalidCharacterRange);
                    }
                    range = true;
                    self.advance();
                }
                Some('\\') if !escape => {
                    escape = true;
                    self.advance();
                }
                Some(c) => {
                    if !chars.add(c) {
                        return Err(Error::InvalidEncoding);
                    }
                    if range {
                        let s = prev.unwrap();
                        if s as u8 > c as u8 {
                            return Err(Error::InvalidCharacterRange);
                        }
                        for b in s as u8 + 1..c as u8 {
                            chars.add(b as char);
                        }
                        range = false;
                        prev = None;
                    } else {
                        prev = Some(c)
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
            None => return Err(Error::NothingToRepeat),
            _e => {
                return Err(Error::CannotRepeat);
            }
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
                    Some(max) if min > max => return Err(Error::InvalidRepetition),
                    max => max,
                };

                match self.current {
                    Some('}') => self.advance(),
                    _ => return Err(Error::InvalidRepetition),
                };

                (min, max)
            }
        };

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
        fn alternation(
            this: &mut Parser<'_>,
            mut terms: Vec<Expr>,
            exprs: Option<Vec<Expr>>,
        ) -> Result<()> {
            //
            if terms.is_empty() {
                return Err(Error::MissingAlternation);
            }

            terms.reverse();
            let concat = match terms.len() {
                1 => terms.pop().unwrap(),
                _ => Expr::Concatenation(terms),
            };

            match exprs {
                None => this.stack.push(Expr::Alternation(vec![concat])),
                Some(mut list) => {
                    list.push(concat);
                    this.stack.push(Expr::Alternation(list));
                }
            };

            Ok(())
        }

        let mut terms = vec![];
        loop {
            match self.stack.pop() {
                None => {
                    alternation(self, terms, None)?;
                    break;
                }
                Some(p @ Expr::LParen) | Some(p @ Expr::CaptureLParen(..)) => {
                    self.stack.push(p);
                    alternation(self, terms, None)?;
                    break;
                }
                Some(Expr::Alternation(list)) => {
                    alternation(self, terms, Some(list))?;
                    break;
                }
                Some(t) => terms.push(t),
            }
        }

        self.advance();
        Ok(())
    }

    fn end(&mut self, sub: bool) -> Result<()> {
        fn end(
            stack: &mut Vec<Expr>,
            mut terms: Vec<Expr>,
            exprs: Option<Vec<Expr>>,
        ) -> Result<()> {
            //
            if terms.is_empty() {
                return match exprs {
                    Some(_) => Err(Error::MissingAlternation),
                    None => Err(Error::EmptyRegex),
                };
            }

            terms.reverse();
            let concat = match terms.len() {
                1 => terms.pop().unwrap(),
                _ => Expr::Concatenation(terms),
            };

            match exprs {
                None => stack.push(concat),
                Some(mut list) => {
                    list.push(concat);
                    stack.push(Expr::Alternation(list))
                }
            }
            Ok(())
        }

        let mut terms = vec![];
        loop {
            match self.stack.pop() {
                None => {
                    if sub {
                        return Err(Error::UnmatchedParen);
                    }
                    end(&mut self.stack, terms, None)?;
                    break;
                }
                Some(p @ Expr::CaptureLParen(..)) | Some(p @ Expr::LParen) => {
                    if !sub {
                        return Err(Error::UnmatchedParen);
                    }
                    self.stack.push(p);
                    end(&mut self.stack, terms, None)?;
                    break;
                }
                Some(Expr::Alternation(list)) => {
                    end(&mut self.stack, terms, Some(list))?;
                    break;
                }
                Some(t) => terms.push(t),
            };
        }

        Ok(())
    }
}
