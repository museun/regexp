use crate::{compiler, compiler::Instruction, compiler::GroupType};

mod action;
mod locations;
mod matches;
mod thread;

use self::{
    action::Action,
    locations::Locations,
    matches::Match,
    matches::Matches,
    thread::Thread,
};

#[derive(Debug)]
pub struct Machine {
    program: Vec<Instruction>,
    visited: Vec<usize>,
    stack: Vec<Action>,
    matched: bool,

    names: Vec<GroupType>,
    locations: Locations,

    group: u32,
}

impl Machine {
    pub fn new(program: compiler::Program) -> Self {
        let len = program.insts.len();
        Self {
            program: program.insts,
            visited: vec![0; len],
            stack: vec![],
            matched: false,

            names: program.names,
            locations: Locations::default(),

            group: 1,
        }
    }

    // TODO rewrite this
    pub fn matches(&mut self, input: impl AsRef<str>) -> Vec<Matches> {
        let input = input.as_ref();

        let mut matches = vec![];
        let mut needle = 0;
        let mut initial = false;

        while needle <= input.len() {
            let (ok, n) = self.find_match(input, needle);
            needle = n;

            if !ok {
                matches.push(Matches(vec![None]));
                continue;
            }

            let mut matched = vec![];
            for (i, loc) in self.locations.chunks(2).enumerate() {
                let match_ = match (loc[0], loc[1]) {
                    (Some(start), Some(end)) => Some(Match {
                        name: self.names[i].clone(),
                        start,
                        end,
                    }),
                    _ => break,
                };
                // ugly
                if !initial {
                    matches.push(Matches(vec![match_]));
                    initial = true;
                    continue;
                }
                matched.push(match_);
            }
            matches.push(Matches(matched));
        }

        matches
    }

    pub fn find_match(&mut self, input: impl AsRef<str>, pos: usize) -> (bool, usize) {
        let len = self.program.len();
        let (mut current, mut next) = (Vec::with_capacity(len), Vec::with_capacity(len));
        self.visited = vec![0; len];

        let chars = input.as_ref().chars().collect::<Vec<_>>();

        let len = chars.len();
        let mut pos = pos;

        // clear the other locations, so the new ones are always at the front
        // TODO probably use deque here and drain it for the return..
        self.locations = Locations::default();

        let mut thread = Thread::default();
        self.epsilon(&mut thread, 0, len, &mut current);

        while !current.is_empty() {
            'thread: for mut thread in current.iter_mut() {
                if pos >= chars.len() {
                    break 'thread;
                }

                if !self.evaluate(pos, len, &chars, &mut thread, &mut next) {
                    break 'thread;
                }
            }

            current.clear();
            pos += 1;
            std::mem::swap(&mut current, &mut next);
        }

        (self.matched, pos)
    }

    fn evaluate(
        &mut self,
        pos: usize,
        len: usize,
        chars: &[char],
        thread: &mut Thread,
        next: &mut Vec<Thread>,
    ) -> bool {
        let mut pos = pos;
        match self.program[thread.pc as usize] {
            Instruction::Char(compiler::Char::Char(ch)) if chars[pos] == ch => {
                thread.pc += 1;
                pos += 1;
            }
            Instruction::Char(compiler::Char::Any) => {
                thread.pc += 1;
                pos += 1;
            }
            Instruction::CharSet(cs) if cs.has(chars[pos]) => {
                thread.pc += 1;
                pos += 1;
            }
            _ => {}
        }
        !self.epsilon(thread, pos, len, next)
    }

    fn epsilon(&mut self, t: &mut Thread, pos: usize, len: usize, list: &mut Vec<Thread>) -> bool {
        self.stack.clear();
        self.stack.push(Action::Jump(t.pc));

        loop {
            match self.stack.pop() {
                None => break,
                Some(Action::Jump(pc)) => {
                    t.pc = pc;
                    if self.follow(t, pos, len, list) {
                        return true;
                    }
                }
                Some(Action::Return(n, entry)) => t.loc[n as usize] = entry,
            }
        }

        false
    }

    fn follow(&mut self, t: &mut Thread, pos: usize, len: usize, list: &mut Vec<Thread>) -> bool {
        loop {
            if pos < self.visited[t.pc as usize] {
                break;
            }
            self.visited[t.pc as usize] = pos + 1;
            match self.program[t.pc as usize] {
                Instruction::Match => {
                    self.locations = t.loc;
                    self.matched = true;
                    return true;
                }
                Instruction::Bol => {
                    if pos != 0 {
                        break;
                    }
                    t.pc += 1;
                }
                Instruction::Eol => {
                    if pos != len {
                        break;
                    }
                    t.pc += 1;
                }
                Instruction::Split(x, y) => {
                    self.stack.push(Action::Jump(y));
                    t.pc = x
                }
                Instruction::Jump(pc) => t.pc = pc,
                Instruction::Start(n) => {
                    let start = t.loc[n as usize];
                    self.stack.push(Action::Return(n, start));
                    t.loc[n as usize] = Some(pos);
                    t.pc += 1;
                }

                Instruction::End(n) => {
                    let start = t.loc[n as usize];
                    self.stack.push(Action::Return(n, start));
                    t.pc += 1;
                }
                _ => {
                    list.push(t.clone());
                    break;
                }
            }
        }

        false
    }
}
