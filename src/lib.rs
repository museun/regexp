pub mod compiler;
pub mod machine;
pub mod parser;
mod regex;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    NoThread,
    OutOfBounds,
    MissingAlternation,
    EmptyRegex,
    NothingToRepeat,
    CannotRepeat,
    InvalidRepetition,
    BolPosition,
    EolPosition,
    UnterminatedCharSet,
    InvalidCharacterRange,
    InvalidEncoding,
    UnmatchedParen,
    UnknownGroupFlag,
    UnfinishedName,
    GroupAlreadyDefined,
}

pub mod prelude {
    pub use crate::regex::*;
}

pub fn new<S>(input: S) -> Result<regex::Regex>
where
    S: AsRef<str>,
{
    regex::Regex::new(input.as_ref())
}

pub fn find<S>(pattern: S, input: S) -> Result<bool>
where
    S: AsRef<str>,
{
    let ast = parser::Parser::parse(pattern.as_ref())?;
    let prog = compiler::Compiler::compile(&ast)?;
    let mut machine = machine::Machine::new(prog);
    let (ok, _) = machine.find_match(input.as_ref(), 0);
    Ok(ok)
}
