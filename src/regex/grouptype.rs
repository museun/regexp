#[derive(Debug)]
pub enum GroupType<'a> {
    Index(usize),
    Named(&'a str),
}

impl<'a> From<usize> for GroupType<'a> {
    fn from(s: usize) -> Self {
        GroupType::Index(s)
    }
}

impl<'a> From<&'a str> for GroupType<'a> {
    fn from(s: &'a str) -> Self {
        GroupType::Named(s)
    }
}
