use super::Char;
use crate::parser;

#[derive(Debug, Copy, Clone)]
pub(crate) enum Operation {
    Match,
    Char(Char),
    CharSet(parser::CharSet),
    Split(Option<u32>, Option<u32>),
    Jump(Option<u32>),
    Start(u32),
    End(u32),
    Bol,
    Eol,
}
