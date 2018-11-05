use std::fmt;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Copy)]
pub struct Locations([Option<usize>; 64]);

impl Default for Locations {
    fn default() -> Self {
        Self { 0: [None; 64] }
    }
}

impl fmt::Debug for Locations {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self
            .0
            .iter()
            .enumerate()
            .fold(String::new(), |mut a, (i, c)| {
                if c.is_none() {
                    return a;
                }
                if i > 0 {
                    a.push_str(", ");
                }
                // whatever
                a.push_str(&format!("{}", c.unwrap()));
                a
            });

        write!(f, "[{}]", s)
    }
}

impl Deref for Locations {
    type Target = [Option<usize>; 64];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Locations {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
