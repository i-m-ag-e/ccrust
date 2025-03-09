pub use super::expr::*;
pub use super::stmt::*;
use crate::lexer::token::Token;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone, PartialEq)]
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

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithToken<U> {
        WithToken(f(self.0), self.1)
    }
}

impl<T, E> WithToken<Result<T, E>> {
    pub fn map_inner<U>(self, f: impl FnOnce(T) -> U) -> WithToken<Result<U, E>> {
        self.map(|res| res.map(f))
    }

    pub fn transpose(self) -> Result<WithToken<T>, E> {
        self.0.map(|t| WithToken(t, self.1))
    }
}

impl<T, E> WithToken<Option<Result<T, E>>> {
    pub fn raise_result(self) -> Result<WithToken<Option<T>>, E> {
        self.map(|op| op.transpose()).transpose()
    }
}

impl<T> WithToken<Option<T>> {
    pub fn map_inner<U>(self, f: impl FnOnce(T) -> U) -> WithToken<Option<U>> {
        self.map(|res| res.map(f))
    }

    pub fn transpose(self) -> Option<WithToken<T>> {
        self.0.map(|t| WithToken(t, self.1))
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
    pub body: CompoundStmt,
}

#[derive(Debug)]
pub enum BlockItem {
    Stmt(Stmt),
    VarDecl(Vec<VarDecl>),
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
            Stmt::Break(wt) => self.visit_break(wt),
            Stmt::Continue(wt) => self.visit_continue(wt),
            Stmt::Compound(block) => self.visit_compound(block),
            Stmt::Expression(expr) => self.visit_expression(expr),
            Stmt::For(for_stmt) => self.visit_for(for_stmt),
            Stmt::Goto(label) => self.visit_goto(label),
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::Label(label) => self.visit_label(label),
            Stmt::Null => self.visit_null(),
            Stmt::Return { ret_value } => self.visit_return(ret_value),
            Stmt::Switch(switch_stmt) => self.visit_switch(switch_stmt),
            Stmt::While(while_stmt) => self.visit_while(while_stmt),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match expr {
            Expr::Assign(assign) => self.visit_assign(assign),
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Comma(comma) => self.visit_comma(comma),
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
            Stmt::Break(wt) => self.visit_break(wt),
            Stmt::Continue(wt) => self.visit_continue(wt),
            Stmt::Compound(block) => self.visit_compound(block),
            Stmt::Expression(expr) => self.visit_expression(expr),
            Stmt::For(for_stmt) => self.visit_for(for_stmt),
            Stmt::Goto(label) => self.visit_goto(label),
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::Label(label) => self.visit_label(label),
            Stmt::Null => self.visit_null(),
            Stmt::Return { ret_value } => self.visit_return(ret_value),
            Stmt::Switch(switch_stmt) => self.visit_switch(switch_stmt),
            Stmt::While(while_stmt) => self.visit_while(while_stmt),
        }
    }

    fn visit_expr(&mut self, expr: Expr) -> Self::ExprResult {
        match expr {
            Expr::Assign(assign) => self.visit_assign(assign),
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Comma(comma) => self.visit_comma(comma),
            Expr::Conditional(cond) => self.visit_conditional(cond),
            Expr::Literal(literal) => self.visit_literal(literal),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Var(name) => self.visit_var(name),
        }
    }
}
