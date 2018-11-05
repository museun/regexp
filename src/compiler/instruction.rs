use super::{digits, Char, Operation};
use crate::parser;

use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Match,
    Char(Char),
    CharSet(parser::CharSet),
    Split(u32, u32),
    Jump(u32),
    Save(u32),
    Bol,
    Eol,
}

impl<'a> From<&'a Operation> for Instruction {
    fn from(inst: &'a Operation) -> Self {
        match inst {
            Operation::Match => Instruction::Match,
            Operation::Char(ch) => Instruction::Char(*ch),
            Operation::CharSet(cs) => Instruction::CharSet(*cs),
            Operation::Split(Some(x), Some(y)) => Instruction::Split(*x, *y),
            Operation::Jump(Some(x)) => Instruction::Jump(*x),
            Operation::Save(n) => Instruction::Save(*n),
            Operation::Bol => Instruction::Bol,
            Operation::Eol => Instruction::Eol,
            _ => unreachable!(),
        }
    }
}

impl Instruction {
    pub(crate) fn columns(&self) -> [usize; 3] {
        match self {
            Instruction::Match => [5, 0, 0],
            Instruction::Split(x, y) => [5, digits(*x), digits(*y)],
            Instruction::Jump(x) => [4, digits(*x), 0],
            Instruction::Save(n) => [4, digits(*n), 0],
            Instruction::Char(Char::Char(ch)) => [4, ch.len_utf8(), 0],
            Instruction::Char(_) // any
            | Instruction::CharSet(_) // set
            | Instruction::Bol
            | Instruction::Eol => [3, 0, 0],
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Match => write!(f, "match"),
            Instruction::Char(Char::Any) => write!(f, "any"),
            Instruction::Char(_) => write!(f, "char"),
            Instruction::CharSet(_) => write!(f, "set"),
            Instruction::Split(_, _) => write!(f, "split"),
            Instruction::Jump(_) => write!(f, "jump"),
            Instruction::Save(_) => write!(f, "save"),
            Instruction::Bol => write!(f, "bol"),
            Instruction::Eol => write!(f, "eol"),
        }
    }
}
