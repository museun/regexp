use crate::parser;

use std::cmp::max;
use std::collections::VecDeque;
use std::io::Write;

mod error;
mod instruction;
mod operation;

pub use self::error::{Error, ErrorKind};
pub use self::instruction::Instruction;
use self::operation::Operation;

type Result<T> = std::result::Result<T, Error>;

const MAX_GROUPS: usize = 10 * 2;

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

#[derive(Debug, Default)]
pub struct Program {
    pub insts: Vec<Instruction>,
    pub names: Vec<Option<String>>, // None for unnamed groups, so indices are correct
}

impl Program {
    pub fn dump<W: Write>(&self, w: &mut W) {
        let mut labels = vec![];
        for inst in self.insts.iter() {
            match *inst {
                Instruction::Split(x, y) => labels.extend_from_slice(&[x, y]),
                Instruction::Jump(n) => labels.push(n),
                _ => continue,
            }
        }
        labels.sort_unstable();
        labels.dedup_by(|a, b| *a > 0 && a == b);

        let max = self
            .insts
            .iter()
            .map(|i| i.columns())
            .fold(vec![0; 3], |mut a, b| {
                for (i, n) in b.iter().enumerate() {
                    a[i] = max(a[i], *n)
                }
                a
            });

        let label_max = digits(*labels.iter().max_by(|x, y| x.cmp(y)).unwrap()) + 1; // for the colon
        let label_pad = std::iter::repeat(" ").take(label_max).collect();

        for (i, inst) in self.insts.iter().enumerate() {
            let label = if labels.iter().any(|&n| n == i as u32) {
                Some(format!("{:^1$}", format!("{}:", i), label_max))
            } else {
                None
            };
            let name = format!("{: >1$}", format!("{}", inst), max[0]);
            let args = match inst {
                Instruction::Split(x, y) => format!("{:<2$} or {:>3$}", x, y, max[1], max[2]),
                Instruction::Jump(n) => format!("{}", n),
                Instruction::Save(n) => format!("#{}", n),
                Instruction::Char(Char::Char(ch)) => format!("{}", ch),
                Instruction::CharSet(cs) => format!("{}", cs),
                _ => "".into(),
            };
            writeln!(
                w,
                "{} {} | {}",
                label.as_ref().unwrap_or_else(|| &label_pad),
                name,
                args,
            )
            .unwrap()
        }
    }
}

pub struct Compiler {
    ops: Vec<Operation>,
    names: VecDeque<Option<String>>, // need to keep these in the right order
}

impl Compiler {
    pub fn compile(regex: &parser::Expression) -> Result<Program> {
        let mut compiler = Compiler {
            ops: vec![],
            names: VecDeque::new(),
        };

        compiler.names.push_back(None); // for 0th group

        if regex.bol {
            compiler.emit(Operation::Bol);
        } else {
            // add .*? at the beginning if we don't have a ^
            compiler.emit(Operation::Split(Some(3), Some(1)));
            compiler.emit(Operation::Char(Char::Any));
            compiler.emit(Operation::Jump(Some(0)));
        }

        compiler.emit(Operation::Save(0));
        let res = compiler.visit(&regex.expr)?;

        let pc = if regex.eol {
            let pc = compiler.emit(Operation::Eol);
            compiler.emit(Operation::Save(1));
            pc
        } else {
            compiler.emit(Operation::Save(1))
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
                let opt = Some(name);
                if self.names.contains(&opt) {
                    return Err(ErrorKind::GroupAlreadyDefined.into());
                }
                self.names.push_back(opt);
            } else {
                self.names.push_back(None);
            }
        }

        let i = match n {
            None => return self.visit(expr),
            Some(i) if i >= MAX_GROUPS as u8 => return self.visit(expr),
            Some(i) => u32::from(i),
        };

        let begin = self.emit(Operation::Save(i * 2)); // probably won't collide
        let state = self.visit(expr)?;
        let end = self.emit(Operation::Save(i * 2 + 1));

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
pub(crate) fn digits(n: u32) -> usize {
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
