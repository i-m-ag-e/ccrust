use crate::parser::ast::{self, BinaryOp, Literal};

#[derive(Debug)]
pub struct Program(pub Vec<FunctionDef>);

#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub body: Vec<Instruction>,
}

pub type Target = String;

#[derive(Debug)]
pub enum Instruction {
    Return(Value),
    Unary(ast::UnaryOp, Value, Value),
    Binary(ast::BinaryOp, Value, Value, Value),
    Copy { src: Value, dst: Value },
    Jump(Target),
    JumpIfZero(Value, Target),
    JumpIfNotZero(Value, Target),
    Label(String),
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(ast::Literal),
    Var(String),
}

pub struct GenerateTacky {
    var_counter: i32,
    label_counter: i32,
}

impl GenerateTacky {
    pub fn new() -> Self {
        Self {
            var_counter: 0,
            label_counter: 0,
        }
    }

    pub fn generate(&mut self, program: &ast::Program) -> Program {
        let funs = program
            .0
            .iter()
            .map(|f| self.generate_function_def(f))
            .collect();
        Program(funs)
    }

    fn generate_function_def(&mut self, function_def: &ast::FunctionDef) -> FunctionDef {
        let mut body = Vec::new();
        function_def
            .body
            .iter()
            .for_each(|s| self.generate_stmt(s, &mut body));
        FunctionDef {
            name: function_def.name.clone(),
            body,
        }
    }

    fn generate_stmt(&mut self, stmt: &ast::Stmt, body: &mut Vec<Instruction>) {
        match stmt {
            ast::Stmt::Return { ret_value } => {
                let ret_value = self.generate_expr(ret_value, body);
                body.push(Instruction::Return(ret_value));
            }
        }
    }

    fn generate_expr(&mut self, expr: &ast::Expr, body: &mut Vec<Instruction>) -> Value {
        match expr {
            ast::Expr::Literal(lit) => Value::Literal(*lit),
            ast::Expr::Unary { op, expr } => {
                let expr = self.generate_expr(expr, body);
                let var = self.new_var();
                body.push(Instruction::Unary(*op, expr, var.clone()));
                var
            }
            ast::Expr::Binary {
                op: BinaryOp::And,
                lhs,
                rhs,
            } => {
                let dst = self.new_var();
                let false_label = self.new_label("and_false");
                let end_label = self.new_label("and_end");

                let vlhs = self.generate_expr(lhs, body);
                body.push(Instruction::JumpIfZero(vlhs, false_label.clone()));
                let vrhs = self.generate_expr(rhs, body);
                body.push(Instruction::JumpIfZero(vrhs, false_label.clone()));

                body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(1)),
                    dst: dst.clone(),
                });
                body.push(Instruction::Jump(end_label.clone()));

                body.push(Instruction::Label(false_label));
                body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(0)),
                    dst: dst.clone(),
                });
                body.push(Instruction::Label(end_label));

                dst
            }
            ast::Expr::Binary {
                op: BinaryOp::Or,
                lhs,
                rhs,
            } => {
                let dst = self.new_var();
                let true_label = self.new_label("or_true");
                let end_label = self.new_label("or_end");

                let vlhs = self.generate_expr(lhs, body);
                body.push(Instruction::JumpIfNotZero(vlhs, true_label.clone()));
                let vrhs = self.generate_expr(rhs, body);
                body.push(Instruction::JumpIfNotZero(vrhs, true_label.clone()));

                body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(0)),
                    dst: dst.clone(),
                });
                body.push(Instruction::Jump(end_label.clone()));

                body.push(Instruction::Label(true_label));
                body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(1)),
                    dst: dst.clone(),
                });
                body.push(Instruction::Label(end_label));

                dst
            }
            ast::Expr::Binary { op, lhs, rhs } => {
                let vlhs = self.generate_expr(lhs, body);
                let vrhs = self.generate_expr(rhs, body);
                let dst = self.new_var();
                body.push(Instruction::Binary(*op, vlhs, vrhs, dst.clone()));
                dst
            }
        }
    }

    fn new_var(&mut self) -> Value {
        let var = format!("tmp.{}", self.var_counter);
        self.var_counter += 1;
        Value::Var(var)
    }

    fn new_label(&mut self, desc: &str) -> String {
        self.label_counter += 1;
        format!("l.{}.{}", desc, self.label_counter - 1)
    }
}
