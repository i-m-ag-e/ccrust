use std::collections::HashMap;

use crate::lexer::token::Token;

use super::r#type::Type;

#[derive(Debug)]
pub struct SymbolTableEntry {
    pub ty: Type,
    pub token: Token,
    pub defined: bool,
}

pub type SymbolTable = HashMap<String, SymbolTableEntry>;
