use super::GroupType;

use std::ops::{Bound, Index, RangeBounds};

#[derive(Debug)]
pub struct Matches {
    pub(crate) matches: Vec<(Option<String>, Match)>,
}

impl Matches {
    pub fn get<'a>(&'a self, g: impl Into<GroupType<'a>>) -> Option<&Match> {
        match g.into() {
            GroupType::Index(n) => self.matches.get(n).map(|(_, m)| m),
            GroupType::Named(q) => {
                let n = self.matches.iter().position(|(s, _)| match s {
                    Some(n) if *n == q => true,
                    _ => false,
                })?;
                Some(&self.matches[n].1)
            }
        }
    }

    pub fn len(&self) -> usize {
        self.matches.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn group_names(&self) -> Vec<&String> {
        self.matches
            .iter()
            .filter_map(|(m, _)| m.as_ref())
            .collect()
    }
}

impl Index<usize> for Matches {
    type Output = Match;
    fn index(&self, index: usize) -> &Self::Output {
        &self.matches[index].1
    }
}

impl<'a> Index<&'a str> for Matches {
    type Output = Match;
    fn index(&self, index: &'a str) -> &Self::Output {
        for (n, m) in &self.matches {
            match n {
                Some(n) if n == index => return &m,
                _ => {}
            }
        }

        panic!("{} not found", index)
    }
}

#[derive(Debug, PartialEq)]
pub struct Match {
    pub start: usize,
    pub end: usize,
}

impl RangeBounds<usize> for Match {
    fn start_bound(&self) -> Bound<&usize> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&usize> {
        Bound::Excluded(&self.end)
    }
}
