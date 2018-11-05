#[derive(Debug)]
pub enum Action {
    Jump(u32),
    Return(u32, Option<usize>),
}
