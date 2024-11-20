#[derive(Debug, Default)]
pub struct Scanner {
    input: String,
}

impl Scanner {
    pub fn new(input: String) -> Self {
        Self { input }
    }
}
