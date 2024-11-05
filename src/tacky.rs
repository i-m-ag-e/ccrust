use crate::parser::ast;

#[derive(Debug)]
pub struct Program(pub Vec<FunctionDef>);

#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Value),
    Unary(ast::UnaryOp, Value, Value),
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(ast::Literal),
    Var(String),
}

pub struct GenerateTacky {
    counter: i32,
}

impl GenerateTacky {
    pub fn new() -> Self {
        Self { counter: 0 }
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
        }
    }

    fn new_var(&mut self) -> Value {
        let var = format!("tmp.{}", self.counter);
        self.counter += 1;
        Value::Var(var)
    }
}
