#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Error {
    pub(crate) pos: usize,
    pub(crate) kind: ErrorKind,
}

impl Error {
    pub fn kind(self) -> ErrorKind {
        self.kind
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ErrorKind {
    EmptyRegex,

    BolPosition,
    EolPosition,

    UnknownGroupFlag,

    UnfinishedName,
    UnmatchedParen,

    UnterminatedCharSet,
    InvalidCharacterRange,
    InvalidEncoding,

    InvalidRepetition,
    NothingToRepeat,
    CannotRepeat,

    MissingAlternation,
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error { pos: 0, kind: kind }
    }
}
