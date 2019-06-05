use crate::compiler::GroupType;

#[derive(Debug, Clone)]
pub struct Matches(pub Vec<Option<Match>>);

#[derive(Debug, Clone)]
pub struct Match {
    pub start: usize,
    pub end: usize,
    pub name: GroupType,
}
