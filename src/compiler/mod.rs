use std::collections::VecDeque;
type Result<T> = std::result::Result<T, Error>;

mod error;
mod instruction;
mod operation;
mod program;

pub use self::error::{Error, ErrorKind};
pub use self::instruction::Instruction;
pub use self::program::Program;

use crate::parser;
use self::operation::Operation;

#[derive(Debug, Copy, Clone)]
pub enum Char {
    Char(char),
    Any,
}

// bad name
#[derive(Debug)]
pub struct State {
    entry: u32,
    branches: Vec<u32>,
}

impl State {
    pub fn new(entry: u32) -> Self {
        Self {
            entry,
            branches: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub enum GroupType {
    Unnamed(u32),
    Named(u32, String),
}

impl PartialEq for GroupType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (GroupType::Named(.., ln), GroupType::Named(.., rn)) => ln.eq(rn),
            (GroupType::Named(l, ..), GroupType::Unnamed(r))
            | (GroupType::Unnamed(l), GroupType::Named(r, ..))
            | (GroupType::Unnamed(l), GroupType::Unnamed(r)) => l.eq(r),
        }
    }
}

pub struct Compiler {
    ops: Vec<Operation>,
    names: VecDeque<GroupType>, // need to keep these in the right order
    group: u32,
}

impl Compiler {
    pub fn compile(regex: &parser::Expression) -> Result<Program> {
        let mut compiler = Compiler {
            ops: vec![],
            names: VecDeque::new(),
            group: 1,
        };

        compiler.names.push_back(GroupType::Unnamed(0)); // for 0th group

        if regex.bol {
            compiler.emit(Operation::Bol);
        } else {
            // add .*? at the beginning if we don't have a ^
            compiler.emit(Operation::Split(Some(3), Some(1)));
            compiler.emit(Operation::Char(Char::Any));
            compiler.emit(Operation::Jump(Some(0)));
        }

        compiler.emit(Operation::Start(0));
        let res = compiler.visit(&regex.expr)?;

        let pc = if regex.eol {
            let pc = compiler.emit(Operation::Eol);
            compiler.emit(Operation::End(0));
            pc
        } else {
            compiler.emit(Operation::End(0))
        };

        for branch in res.branches {
            compiler.kill(branch, pc)
        }

        compiler.emit(Operation::Match);

        let program = Program {
            names: compiler.names.into_iter().collect(),
            insts: compiler.ops.iter().map(Instruction::from).collect(),
        };

        Ok(program)
    }

    fn visit(&mut self, expr: &parser::Expr) -> Result<State> {
        use self::parser::{Expr, Repetition};

        match expr {
            Expr::Char(ch) => self.visit_char(Char::Char(*ch)),
            Expr::Any => self.visit_char(Char::Any),
            Expr::CharSet(ref cs) => self.visit_charset(cs),
            Expr::Repetition(expr, rep, greedy) => match rep {
                Repetition::Star => self.visit_star(expr, *greedy),
                Repetition::Question => self.visit_question(expr, *greedy),
            },
            Expr::Alternation(ref exprs) => self.visit_alternation(exprs),
            Expr::Concatenation(ref exprs) => self.visit_concatenation(exprs),
            Expr::CaptureGroup(n, name, ref expr) => self.visit_group(Some(*n), name.clone(), expr),
            Expr::Group(ref expr) => self.visit_group(None, None, expr),
            e => unimplemented!("{:?}", e),
        }
    }

    fn visit_group(
        &mut self,
        n: Option<u8>,
        name: Option<String>,
        expr: &parser::Expr,
    ) -> Result<State> {
        if n.is_some() {
            if let Some(name) = name {
                let opt = GroupType::Named(self.group, name);
                if self.names.contains(&opt) {
                    return Err(ErrorKind::GroupAlreadyDefined.into());
                }
                self.names.push_back(opt);
            } else {
                self.names.push_back(GroupType::Unnamed(self.group));
            }
            self.group += 1;
        }

        if n.is_none() {
            return self.visit(expr);
        };

        let group = self.group;
        let begin = self.emit(Operation::Start(group));
        self.group += 1;

        let state = self.visit(expr)?;
        let end = self.emit(Operation::Start(group));

        for branch in state.branches {
            self.kill(branch, end)
        }

        Ok(State {
            entry: begin,
            branches: vec![],
        })
    }

    fn visit_char(&mut self, ch: Char) -> Result<State> {
        let pc = self.emit(Operation::Char(ch));
        Ok(State::new(pc))
    }

    fn visit_charset(&mut self, cs: &parser::CharSet) -> Result<State> {
        let pc = self.emit(Operation::CharSet(*cs));
        Ok(State::new(pc))
    }

    fn visit_star(&mut self, expr: &parser::Expr, greedy: bool) -> Result<State> {
        let inst = if greedy {
            Operation::Split(Some(self.pc() + 1), None)
        } else {
            Operation::Split(None, Some(self.pc() + 1))
        };

        let split = self.emit(inst);
        let res = self.visit(expr)?;
        let jmp = self.emit(Operation::Jump(Some(split)));

        for branch in res.branches {
            self.kill(branch, jmp);
        }

        Ok(State {
            entry: split,
            branches: vec![split],
        })
    }

    fn visit_question(&mut self, expr: &parser::Expr, greedy: bool) -> Result<State> {
        let inst = if greedy {
            Operation::Split(Some(self.pc() + 1), None)
        } else {
            Operation::Split(None, Some(self.pc() + 1))
        };

        let split = self.emit(inst);
        let mut res = self.visit(&expr)?;
        res.entry = split;
        res.branches.push(split);

        Ok(res)
    }

    fn visit_alternation(&mut self, list: &[parser::Expr]) -> Result<State> {
        let mut entry = None;
        let mut branches = vec![];
        let mut last = None;

        let (head, tail) = list.split_at(list.len() - 1);
        for expr in head.iter() {
            let inst = Operation::Split(Some(self.pc() + 1), None);
            let split = self.emit(inst);
            if entry.is_none() {
                entry = Some(split);
            }
            if last.is_some() {
                self.kill(last.unwrap(), split);
            }
            last = Some(split);

            let res = self.visit(&expr)?;
            let jmp = self.emit(Operation::Jump(None));

            branches.push(jmp);
            for branch in res.branches {
                self.kill(branch, jmp)
            }
        }

        let mut res = self.visit(&tail[0])?;
        self.kill(last.unwrap(), res.entry);
        branches.append(&mut res.branches);

        Ok(State {
            entry: entry.unwrap(),
            branches,
        })
    }

    fn visit_concatenation(&mut self, list: &[parser::Expr]) -> Result<State> {
        let mut entry = None;
        let mut last = State {
            entry: 0,
            branches: vec![],
        };

        for expr in list {
            let res = self.visit(expr)?;
            if entry.is_none() {
                entry = Some(res.entry)
            }
            for branch in last.branches {
                self.kill(branch, res.entry);
            }
            last = res;
        }
        last.entry = entry.unwrap();
        Ok(last)
    }

    fn kill(&mut self, branch: u32, pc: u32) {
        match self.ops[branch as usize] {
            Operation::Split(ref mut x, _) if *x == None => *x = Some(pc),
            Operation::Split(_, ref mut y) if *y == None => *y = Some(pc),
            Operation::Jump(ref mut x) => *x = Some(pc),
            _ => {}
        }
    }

    fn emit(&mut self, inst: Operation) -> u32 {
        let pc = self.pc();
        self.ops.push(inst);
        pc
    }

    fn pc(&self) -> u32 {
        self.ops.len() as u32
    }
}

#[inline]
pub(crate) fn count_digits(n: u32) -> usize {
    if n == 0 {
        return 1;
    }

    let (mut n, mut x) = (n, 0);
    while n > 0 {
        n /= 10;
        x += 1;
    }
    x
}
