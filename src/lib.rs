pub mod compiler;
pub mod machine;
pub mod parser;
mod regex;

#[derive(Debug, PartialEq)]
pub enum Error {
    ParserError(parser::Error),
    CompilerError(compiler::Error),
}

pub mod prelude {
    pub use crate::regex::*;
}

pub fn new<S>(input: S) -> Result<regex::Regex, Error>
where
    S: AsRef<str>,
{
    regex::Regex::new(input.as_ref())
}

pub fn find<S>(pattern: S, input: S) -> Result<bool, Error>
where
    S: AsRef<str>,
{
    let ast = parser::Parser::parse(pattern.as_ref()).map_err(Error::ParserError)?;
    let prog = compiler::Compiler::compile(&ast).map_err(Error::CompilerError)?;
    let mut machine = machine::Machine::new(prog);
    let (ok, _) = machine.find_match(input.as_ref(), 0);
    Ok(ok)
}
