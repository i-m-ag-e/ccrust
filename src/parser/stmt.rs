use super::{expr::Expr, BlockItem, WithToken};

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
pub struct Block(pub Vec<BlockItem>);

#[derive(Debug)]
pub enum Stmt {
    Compound(Block),
    Expression(Expr),
    Goto(WithToken<String>),
    If(IfStmt),
    Label(Label),
    Null,
    Return { ret_value: WithToken<Expr> },
}

pub trait StmtRefVisitor<R> {
    fn visit_compound(&mut self, block: &Block) -> R;
    fn visit_expression(&mut self, expr: &Expr) -> R;
    fn visit_goto(&mut self, label: &WithToken<String>) -> R;
    fn visit_if(&mut self, if_stmt: &IfStmt) -> R;
    fn visit_label(&mut self, label: &Label) -> R;
    fn visit_null(&mut self) -> R;
    fn visit_return(&mut self, ret_value: &WithToken<Expr>) -> R;
}

pub trait StmtVisitor<R> {
    fn visit_compound(&mut self, block: Block) -> R;
    fn visit_expression(&mut self, expr: Expr) -> R;
    fn visit_goto(&mut self, label: WithToken<String>) -> R;
    fn visit_if(&mut self, if_stmt: IfStmt) -> R;
    fn visit_label(&mut self, label: Label) -> R;
    fn visit_null(&mut self) -> R;
    fn visit_return(&mut self, ret_value: WithToken<Expr>) -> R;
}
