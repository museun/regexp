#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn kind(self) -> ErrorKind {
        self.kind
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ErrorKind {
    GroupAlreadyDefined,
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error { kind: kind }
    }
}
