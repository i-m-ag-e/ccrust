use crate::lexer::token::Token;

#[derive(Debug)]
pub struct Program(pub Vec<FunctionDef>);

#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Return { ret_value: Expr },
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
}

#[derive(Debug)]
pub enum Literal {
    Integer(i64),
    Float(f64),
}
