use super::Locations;

#[derive(Debug, Clone, Copy, Default)]
pub struct Thread {
    pub pc: u32,
    pub loc: Locations,
}
