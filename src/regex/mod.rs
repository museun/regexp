use crate::compiler::{Compiler,GroupType};
use crate::machine::Machine;
use crate::parser::Parser;
use crate::Error;

mod matches;
pub use self::matches::{Match, Matches};

use std::ops::Range;

pub struct Regex {
    machine: Machine,
}

impl Regex {
    pub fn compile(pattern: &str) -> Result<Regex, Error> {
        let ast = Parser::parse(pattern).map_err(Error::ParserError)?;
        let prog = Compiler::compile(&ast).map_err(Error::CompilerError)?;
        let machine = Machine::new(prog);
        Ok(Self { machine })
    }

    pub fn find(&mut self, input: &str) -> bool {
        let (ok, _) = self.machine.find_match(input, 0);
        ok
    }

    pub fn matches<'a, 'b: 'a>(&'b mut self, input: &'a str) -> Matches<'a> {
        use std::cmp;
        fn range<T>((start, end): (T, T)) -> Range<T> {
            Range { start, end }
        }

        let mut results = vec![];
        let (mut min, mut max) = (0, 0);
        for matches in self
            .machine
            .matches(input)
            .into_iter()
            .filter(|m| !m.0.is_empty())
        {
            for m in matches.0.iter().cloned().filter_map(|s| s) {
                min = cmp::min(min, m.start);
                max = cmp::max(max, m.end);
                results.push((m.name, range((m.start, m.end))))
            }
        }

        // total match group
        results.insert(0, (GroupType::Unnamed(0), range((min, max))));
        Matches::new(&input, results)
    }
}
