pub use super::expr::*;
pub use super::stmt::*;
use crate::lexer::token::Token;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct WithToken<T>(pub T, pub Token);

impl<T> WithToken<T> {
    pub fn get_token(&self) -> &Token {
        &self.1
    }

    pub fn unwrap(self) -> T {
        self.0
    }

    pub fn replace(&mut self, new_val: T) -> T {
        std::mem::replace(&mut self.0, new_val)
    }
}

impl<T> Deref for WithToken<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for WithToken<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct Program(pub Vec<FunctionDef>);

#[derive(Debug)]
pub struct FunctionDef {
    pub name: WithToken<String>,
    pub body: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Stmt(Stmt),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: WithToken<String>,
    pub init: Option<Expr>,
}

pub trait ASTRefVisitor:
    StmtRefVisitor<Self::StmtResult> + ExprRefVisitor<Self::ExprResult>
{
    type ProgramResult;
    type FuncDefResult;
    type BlockItemResult;
    type VarDeclResult;
    type StmtResult;
    type ExprResult;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, function_def: &FunctionDef) -> Self::FuncDefResult;

    fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItemResult;

    fn visit_var_decl(&mut self, var_decl: &VarDecl) -> Self::VarDeclResult;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtResult {
        match stmt {
            Stmt::Expression(expr) => self.visit_expression(expr),
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::Null => self.visit_null(),
            Stmt::Return { ret_value } => self.visit_return(ret_value),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match expr {
            Expr::Assign(assign) => self.visit_assign(assign),
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Conditional(cond) => self.visit_conditional(cond),
            Expr::Literal(literal) => self.visit_literal(literal),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Var(name) => self.visit_var(name),
        }
    }
}

pub trait ASTVisitor: StmtVisitor<Self::StmtResult> + ExprVisitor<Self::ExprResult> {
    type ProgramResult;
    type FuncDefResult;
    type BlockItemResult;
    type VarDeclResult;
    type StmtResult;
    type ExprResult;

    fn visit_program(&mut self, program: Program) -> Self::ProgramResult;
    fn visit_function_def(&mut self, function_def: FunctionDef) -> Self::FuncDefResult;

    fn visit_block_item(&mut self, block_item: BlockItem) -> Self::BlockItemResult;

    fn visit_var_decl(&mut self, var_decl: VarDecl) -> Self::VarDeclResult;

    fn visit_stmt(&mut self, stmt: Stmt) -> Self::StmtResult {
        match stmt {
            Stmt::Expression(expr) => self.visit_expression(expr),
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::Null => self.visit_null(),
            Stmt::Return { ret_value } => self.visit_return(ret_value),
        }
    }

    fn visit_expr(&mut self, expr: Expr) -> Self::ExprResult {
        match expr {
            Expr::Assign(assign) => self.visit_assign(assign),
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Conditional(cond) => self.visit_conditional(cond),
            Expr::Literal(literal) => self.visit_literal(literal),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Var(name) => self.visit_var(name),
        }
    }
}
