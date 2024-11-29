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
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Not,
    BitNOT,
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

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    Integer(i64),
    Float(f64),
}

pub trait ASTVisitor<R> {
    fn visit_program(&mut self, program: &Program) -> R;
    fn visit_function_def(&mut self, function_def: &FunctionDef) -> R;
    fn visit_stmt(&mut self, stmt: &Stmt) -> R;
    fn visit_expr(&mut self, expr: &Expr) -> R;
    fn visit_unary_op(&mut self, unary_op: &UnaryOp) -> R;
    fn visit_binary_op(&mut self, binary_op: &BinaryOp) -> R;
    fn visit_literal(&mut self, literal: &Literal) -> R;
}
