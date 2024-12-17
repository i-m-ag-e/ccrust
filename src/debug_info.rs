use std::ops::Deref;
use std::ops::DerefMut;

#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub line: usize,
    pub description: String,
}

impl DebugInfo {
    pub fn map_description(self, f: impl FnOnce(String) -> String) -> Self {
        Self {
            line: self.line,
            description: f(self.description),
        }
    }

    pub fn map_description_ref(&self, f: impl FnOnce(&String) -> String) -> Self {
        Self {
            line: self.line,
            description: f(&self.description),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WithDebugInfo<T> {
    pub value: T,
    pub debug_info: DebugInfo,
}

impl<T> WithDebugInfo<T> {
    pub fn new(value: T, debug_info: DebugInfo) -> Self {
        Self { value, debug_info }
    }

    pub fn unwrap(self) -> T {
        self.value
    }

    pub fn get(&self) -> &T {
        &self.value
    }

    pub fn get_debug_info(&self) -> &DebugInfo {
        &self.debug_info
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithDebugInfo<U> {
        WithDebugInfo {
            value: f(self.value),
            debug_info: self.debug_info,
        }
    }
}

impl<T> Deref for WithDebugInfo<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for WithDebugInfo<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
