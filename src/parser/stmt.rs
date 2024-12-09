use super::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Null,
    Return { ret_value: Expr },
}

pub trait StmtRefVisitor<R> {
    fn visit_expression(&mut self, expr: &Expr) -> R;
    fn visit_null(&mut self) -> R;
    fn visit_return(&mut self, ret_value: &Expr) -> R;
}

pub trait StmtVisitor<R> {
    fn visit_expression(&mut self, expr: Expr) -> R;
    fn visit_null(&mut self) -> R;
    fn visit_return(&mut self, ret_value: Expr) -> R;
}
