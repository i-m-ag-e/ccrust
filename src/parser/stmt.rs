use super::{
    ast::{Literal, VarDecl},
    expr::Expr,
    BlockItem, WithToken,
};

#[derive(Debug)]
pub struct Block(pub Vec<BlockItem>);

#[derive(Debug)]
pub struct CompoundStmt {
    pub block: Block,
    pub introduce_scope: bool,
}

#[derive(Debug)]
pub struct BreakStmt {
    pub loop_or_switch: bool,
    pub id: WithToken<LoopID>,
}

#[derive(Debug)]
pub enum ForStmtInitializer {
    VarDecl(Vec<VarDecl>),
    Expr(Expr),
}

type LoopID = i32;

#[derive(Debug)]
pub struct ForStmt {
    pub loop_id: LoopID,
    pub initializer: WithToken<Option<ForStmtInitializer>>,
    pub condition: WithToken<Option<Expr>>,
    pub step: WithToken<Option<Expr>>,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
pub struct IfStmt {
    pub cond: WithToken<Expr>,
    pub then: Box<Stmt>,
    pub else_clause: Option<Box<WithToken<Stmt>>>,
}

#[derive(Debug)]
pub struct Label {
    pub name: WithToken<String>,
    pub next_stmt: Box<Stmt>,
}

#[derive(Debug)]
pub struct Case {
    pub value: WithToken<Literal>,
    pub stmt: CompoundStmt,
}

#[derive(Debug)]
pub struct SwitchStmt {
    pub switch_id: LoopID,
    pub cond: WithToken<Expr>,
    pub cases: Vec<WithToken<Case>>,
    pub default: CompoundStmt,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub loop_id: LoopID,
    pub cond: WithToken<Expr>,
    pub body: Box<Stmt>,
    pub do_while: bool,
}

#[derive(Debug)]
pub enum Stmt {
    Break(BreakStmt),
    Continue(WithToken<LoopID>),
    Compound(CompoundStmt),
    Expression(Expr),
    For(ForStmt),
    Goto(WithToken<String>),
    If(IfStmt),
    Label(Label),
    Null,
    Return { ret_value: WithToken<Expr> },
    Switch(SwitchStmt),
    While(WhileStmt),
}

pub trait StmtRefVisitor<R> {
    fn visit_break(&mut self, break_stmt: &BreakStmt) -> R;
    fn visit_continue(&mut self, continue_stmt: &WithToken<i32>) -> R;
    fn visit_compound(&mut self, compound_stmt: &CompoundStmt) -> R;
    fn visit_expression(&mut self, expr: &Expr) -> R;
    fn visit_for(&mut self, for_stmt: &ForStmt) -> R;
    fn visit_goto(&mut self, label: &WithToken<String>) -> R;
    fn visit_if(&mut self, if_stmt: &IfStmt) -> R;
    fn visit_label(&mut self, label: &Label) -> R;
    fn visit_null(&mut self) -> R;
    fn visit_return(&mut self, ret_value: &WithToken<Expr>) -> R;
    fn visit_switch(&mut self, switch_stmt: &SwitchStmt) -> R;
    fn visit_while(&mut self, while_stmt: &WhileStmt) -> R;
}

pub trait StmtVisitor<R> {
    fn visit_break(&mut self, break_stmt: BreakStmt) -> R;
    fn visit_continue(&mut self, continue_stmt: WithToken<i32>) -> R;
    fn visit_compound(&mut self, compound_stmt: CompoundStmt) -> R;
    fn visit_expression(&mut self, expr: Expr) -> R;
    fn visit_for(&mut self, for_stmt: ForStmt) -> R;
    fn visit_goto(&mut self, label: WithToken<String>) -> R;
    fn visit_if(&mut self, if_stmt: IfStmt) -> R;
    fn visit_label(&mut self, label: Label) -> R;
    fn visit_null(&mut self) -> R;
    fn visit_return(&mut self, ret_value: WithToken<Expr>) -> R;
    fn visit_switch(&mut self, switch_stmt: SwitchStmt) -> R;
    fn visit_while(&mut self, while_stmt: WhileStmt) -> R;
}
