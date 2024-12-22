#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub line: usize,
    pub description: String,
}

impl DebugInfo {
    pub fn new(line: usize, description: String) -> Self {
        Self { line, description }
    }

    pub fn add_description(&mut self, description: String) {
        self.description = format!("({}) {}", self.description, description);
    }

    pub fn with_more_info(mut self, description: String) -> Self {
        self.add_description(description);
        self
    }

    pub fn more_info(&self, description: String) -> Self {
        let mut self_clone = self.clone();
        self_clone.add_description(description);
        self_clone
    }
}

impl std::fmt::Display for DebugInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {} :: {}", self.line, self.description)
    }
}
