use std::fmt::Display;

use crate::lexer::token::Token;

use super::ast::WithToken;

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(Assign),
    Binary(Binary),
    Comma(Comma),
    Conditional(Conditional),
    FunctionCall(FunctionCall),
    Literal(WithToken<Literal>),
    Unary(Unary),
    Var(WithToken<String>),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub eq_sign: Token,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: WithToken<BinaryOp>,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Comma(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: WithToken<UnaryOp>,
    pub expr: Box<Expr>,
    pub postfix: bool,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
    pub qmark: Token,
    pub colon: Token,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: Box<Expr>,
    pub args: Vec<WithToken<Expr>>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Not,
    BitNOT,
    Increment,
    Decrement,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            UnaryOp::Minus => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNOT => "~",
            UnaryOp::Increment => "++",
            UnaryOp::Decrement => "--",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,

    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    LShift,
    RShift,

    Eq,
    NotEq,
    Greater,
    GreaterEq,
    Lesser,
    LesserEq,

    And,
    Or,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            BinaryOp::Plus => "+",
            BinaryOp::Minus => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",

            BinaryOp::BitwiseAND => "&",
            BinaryOp::BitwiseOR => "|",
            BinaryOp::BitwiseXOR => "^",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",

            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::Lesser => "<",
            BinaryOp::LesserEq => "<=",

            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Integral {
    Integer(i64),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Literal {
    Integral(Integral),
    Float(f64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integral(Integral::Integer(i)) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
        }
    }
}

pub trait ExprRefVisitor<R> {
    fn visit_assign(&mut self, expr: &Assign) -> R;
    fn visit_binary(&mut self, expr: &Binary) -> R;
    fn visit_comma(&mut self, expr: &Comma) -> R;
    fn visit_conditional(&mut self, expr: &Conditional) -> R;
    fn visit_function_call(&mut self, call: &FunctionCall) -> R;
    fn visit_literal(&mut self, literal: &WithToken<Literal>) -> R;
    fn visit_unary(&mut self, expr: &Unary) -> R;
    fn visit_var(&mut self, name: &WithToken<String>) -> R;
}

pub trait ExprVisitor<R> {
    fn visit_assign(&mut self, expr: Assign) -> R;
    fn visit_binary(&mut self, expr: Binary) -> R;
    fn visit_comma(&mut self, expr: Comma) -> R;
    fn visit_conditional(&mut self, expr: Conditional) -> R;
    fn visit_function_call(&mut self, call: FunctionCall) -> R;
    fn visit_literal(&mut self, literal: WithToken<Literal>) -> R;
    fn visit_unary(&mut self, expr: Unary) -> R;
    fn visit_var(&mut self, name: WithToken<String>) -> R;
}
