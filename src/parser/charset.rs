use std::fmt;

#[derive(Clone, Copy, PartialEq)]
pub struct CharSet(pub u128);

impl fmt::Display for CharSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = String::new();

        for n in 0x20..=0x7E {
            if (self.0 >> n) & 1 == 1 {
                out.push((((self.0 >> n) & 1) as u8 + n) as char)
            }
        }

        write!(f, "[{}]", out)
    }
}

impl fmt::Debug for CharSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0b{:0128b}", self.0)
    }
}

impl Default for CharSet {
    fn default() -> Self {
        CharSet(0u128)
    }
}

impl CharSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, ch: char) -> bool {
        if !ch.is_ascii() || ch.len_utf8() != 1 {
            return false;
        }

        // this could be done in 1 mask
        if !self.check(ch as usize) {
            self.toggle(ch as usize)
        }
        true
    }

    pub fn has(&self, ch: char) -> bool {
        if !ch.is_ascii() || ch.len_utf8() != 1 {
            return false;
        }
        self.check(ch as usize) // ugly
    }

    pub fn complement(&mut self) {
        self.0 = !self.0;
    }

    #[inline]
    fn toggle(&mut self, n: usize) {
        self.0 ^= 1 << n.saturating_sub(1);
    }

    #[inline]
    fn check(&self, n: usize) -> bool {
        (self.0 >> n.saturating_sub(1)) & 1 == 1
    }
}
