use crate::compiler::GroupType;

use std::ops::{Bound, RangeBounds,Range};

#[derive(Debug, Clone)]
pub struct Matches<'a> {
    source: &'a str,
    matches: Vec<(GroupType, Range<usize>)>,
}

impl<'a> Matches<'a> {
    pub(crate) fn new(source: &'a str, matches: Vec<(GroupType, Range<usize>)>) -> Self {
        Self { source, matches }
    }

    pub fn success(&self) -> bool {
        !self.is_empty()
    }

    pub fn len(&self) -> usize {
        self.matches.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn groups(&self) -> Vec<&str> {
        self.matches
            .iter()
            .filter_map(|(m, _)| match m {
                GroupType::Unnamed(..) => None,
                GroupType::Named(.., name) => Some(name.as_ref()),
            })
            .collect()
    }

    pub fn iter(&self) -> MatchesIter<'a> {
        self.clone().into_iter()
    }
}

impl<'a> IntoIterator for Matches<'a> {
    type Item = Match;
    type IntoIter = MatchesIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        MatchesIter {
            matches: self,
            pos: 0,
        }
    }
}

pub struct MatchesIter<'a> {
    matches: Matches<'a>,
    pos: usize,
}

impl<'a> Iterator for MatchesIter<'a> {
    type Item = Match;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos == self.matches.len() {
            return None;
        }
        let tmp = &self.matches.matches[self.pos];
        self.pos += 1;
        Some(Match {
            group: tmp.0.clone(),
            value: self.matches.source[tmp.1.clone()].to_string(),
            range: tmp.1.clone(),
        })
    }
}

#[derive(Debug)]
pub struct Match {
    group: GroupType,
    value: String, // pre sliced
    range: Range<usize>,
}

impl Match {
    pub fn group(&self) -> &GroupType {
        &self.group
    }

    pub fn range(&self) -> &Range<usize> {
        &self.range
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl RangeBounds<usize> for Match {
    fn start_bound(&self) -> Bound<&usize> {
        Bound::Included(&self.range.start)
    }

    fn end_bound(&self) -> Bound<&usize> {
        Bound::Excluded(&self.range.end)
    }
}
