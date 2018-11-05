pub(crate) mod compiler;
pub(crate) mod machine;
pub(crate) mod parser;

pub type Result<T> = std::result::Result<T, Error>;

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

mod regex;

pub use self::regex::*;
