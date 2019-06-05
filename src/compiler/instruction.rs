use super::{count_digits, Char, Operation};
use crate::parser;

use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Match,
    Char(Char),
    CharSet(parser::CharSet),
    Split(u32, u32),
    Jump(u32),
    Start(u32),
    End(u32),
    Bol,
    Eol,
}

impl<'a> From<&'a Operation> for Instruction {
    fn from(inst: &'a Operation) -> Self {
        use self::Operation::*;
        match inst {
            Match => Instruction::Match,
            Char(ch) => Instruction::Char(*ch),
            CharSet(cs) => Instruction::CharSet(*cs),
            Split(Some(x), Some(y)) => Instruction::Split(*x, *y),
            Jump(Some(x)) => Instruction::Jump(*x),
            Start(n) => Instruction::Start(*n),
            End(n) => Instruction::End(*n),
            Bol => Instruction::Bol,
            Eol => Instruction::Eol,
            _ => unreachable!(),
        }
    }
}

impl Instruction {
    pub(crate) fn columns(&self) -> [usize; 3] {
        use self::Instruction::*;

        match self {
            Match => [5, 0, 0],
            Split(x, y) => [5, count_digits(*x), count_digits(*y)],
            Jump(x) => [4, count_digits(*x), 0],
            Start(n) => [5, count_digits(*n), 0],
            End(n) => [3, count_digits(*n), 0],
            Char(super::Char::Char(ch)) => [4, ch.len_utf8(), 0],
            Char(_) // any
            | CharSet(_) // set
            | Bol
            | Eol => [3, 0, 0],
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Instruction::*;

        match self {
            Match => write!(f, "match"),
            Char(super::Char::Any) => write!(f, "any"),
            Char(_) => write!(f, "char"),
            CharSet(_) => write!(f, "set"),
            Split(_, _) => write!(f, "split"),
            Jump(_) => write!(f, "jump"),
            Start(_) => write!(f, "start"),
            End(_) => write!(f, "end"),
            Bol => write!(f, "bol"),
            Eol => write!(f, "eol"),
        }
    }
}
