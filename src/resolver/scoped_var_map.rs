use super::MapEntry;
use std::collections::HashMap;

pub type VarMap = HashMap<String, MapEntry>;

#[derive(Debug)]
pub struct ScopedVarMap {
    scope_stack: Vec<VarMap>,
}

impl ScopedVarMap {
    pub fn new() -> ScopedVarMap {
        ScopedVarMap {
            scope_stack: Vec::new(),
        }
    }

    pub fn insert_scope(&mut self) {
        self.scope_stack.push(VarMap::new());
    }

    pub fn unwind(&mut self) -> Option<VarMap> {
        self.scope_stack.pop()
    }

    pub fn current_scope(&self) -> Option<&VarMap> {
        self.scope_stack.last()
    }

    pub fn current_scope_mut(&mut self) -> Option<&mut VarMap> {
        self.scope_stack.last_mut()
    }

    pub fn parent(&self, n: usize) -> Option<&VarMap> {
        self.scope_stack.get(self.scope_stack.len() - n - 2)
    }

    pub fn lookup(&self, name: &str) -> Option<&MapEntry> {
        self.current_scope().and_then(|scope| {
            scope.get(name).or_else(|| {
                (0..self.scope_stack.len() - 1)
                    .rev()
                    .find_map(|i| self.parent(i).and_then(|p| p.get(name)))
            })
        })
    }
}

impl std::ops::Deref for ScopedVarMap {
    type Target = HashMap<String, MapEntry>;

    fn deref(&self) -> &Self::Target {
        self.current_scope().expect("scope stack is empty")
    }
}

impl std::ops::DerefMut for ScopedVarMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.current_scope_mut().expect("scope stack is empty")
    }
}
