use crate::compiler::Compiler;
use crate::machine::Machine;
use crate::parser::Parser;
use crate::Error;

mod matches;

pub use self::matches::{Match, MatchIndex, Matches};

pub struct Regex {
    machine: Machine,
}

impl Regex {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        let ast = Parser::parse(pattern).map_err(Error::ParserError)?;
        let prog = Compiler::compile(&ast).map_err(Error::CompilerError)?;

        Ok(Self {
            machine: Machine::new(prog),
        })
    }

    pub fn find(&mut self, input: &str) -> bool {
        let (ok, _) = self.machine.find_match(input, 0);
        ok
    }

    pub fn matches(&mut self, input: &str) -> Matches {
        let mut results = Matches { matches: vec![] };

        let (mut min, mut max) = (0, 0);
        for matches in self
            .machine
            .matches(input)
            .into_iter()
            .filter(|m| !m.0.is_empty())
            // this isn't the zeroth group
            .skip(1)
        {
            for match_ in matches.0.iter().cloned().filter_map(|s| s) {
                min = std::cmp::min(min, match_.start);
                max = std::cmp::max(max, match_.end);

                results.matches.push((
                    match_.name,
                    Match {
                        start: match_.start,
                        end: match_.end,
                    },
                ));
            }
        }

        // put the zeroth group in. I don't know where we lost it
        results.matches.insert(
            0,
            (
                None,
                Match {
                    start: min,
                    end: max,
                },
            ),
        );

        results
    }
}
