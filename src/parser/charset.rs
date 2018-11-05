use std::fmt;

#[derive(Clone, Copy)]
pub struct CharSet(pub [bool; 128]);

impl CharSet {
    pub fn new() -> Self {
        CharSet([false; 128])
    }

    pub fn add(&mut self, ch: char) -> bool {
        if !ch.is_ascii() || ch.len_utf8() != 1 {
            return false;
        }
        self.0[ch as usize] = true;
        true
    }

    pub fn has(&self, ch: char) -> bool {
        if !ch.is_ascii() || ch.len_utf8() != 1 {
            return false;
        }
        self.0[ch as usize]
    }

    // for ^
    pub fn complement(&mut self) {
        for ch in self.0.iter_mut() {
            *ch = !*ch
        }
    }
}

impl fmt::Debug for CharSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for c in 0x21u8..0x7Eu8 {
            // TODO compress this output
            if self.0[c as usize] {
                write!(f, "{}", c as char)?;
            }
        }
        write!(f, "]")
    }
}

impl PartialEq for CharSet {
    fn eq(&self, other: &CharSet) -> bool {
        for i in 0..self.0.len() {
            if self.0[i] != other.0[i] {
                return false;
            }
        }
        true
    }
}
