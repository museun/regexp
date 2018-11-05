use super::CharSet;

#[derive(Clone, Debug)]
pub struct Expression {
    pub expr: Expr,
    pub bol: bool, // ^
    pub eol: bool, // $
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Char(char),
    Any,
    CharSet(CharSet),
    Repetition(Box<Expr>, Repetition, bool),
    Alternation(Vec<Expr>),
    Concatenation(Vec<Expr>),
    CaptureGroup(u8, Option<String>, Box<Expr>),
    Group(Box<Expr>),
    CaptureLParen(u8, Option<String>),
    LParen,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Repetition {
    Star,
    Question,
}
