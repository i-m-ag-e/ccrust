use crate::parser::ast::{
    self, ASTRefVisitor, BinaryOp, BlockItem, ExprRefVisitor, Literal, StmtRefVisitor, WithToken,
};

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
    current_body: Vec<Instruction>,
}

impl GenerateTacky {
    pub fn new() -> Self {
        Self {
            var_counter: 0,
            label_counter: 0,
            current_body: Vec::new(),
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

impl ExprRefVisitor<Value> for GenerateTacky {
    fn visit_assign(&mut self, expr: &ast::Assign) -> Value {
        let result = self.visit_expr(&expr.rhs);
        // lhs guarateed to be Var(_), so dst will be a Value::Var(_)
        let dest = self.visit_expr(&expr.lhs);
        self.current_body.push(Instruction::Copy {
            src: result,
            dst: dest.clone(),
        });
        dest
    }

    fn visit_binary(&mut self, expr: &ast::Binary) -> Value {
        let ast::Binary { op, lhs, rhs } = expr;
        match **op {
            BinaryOp::And => {
                let dst = self.new_var();
                let false_label = self.new_label("and_false");
                let end_label = self.new_label("and_end");

                let vlhs = self.visit_expr(lhs);
                self.current_body
                    .push(Instruction::JumpIfZero(vlhs, false_label.clone()));
                let vrhs = self.visit_expr(rhs);
                self.current_body
                    .push(Instruction::JumpIfZero(vrhs, false_label.clone()));

                self.current_body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(1)),
                    dst: dst.clone(),
                });
                self.current_body.push(Instruction::Jump(end_label.clone()));

                self.current_body.push(Instruction::Label(false_label));
                self.current_body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(0)),
                    dst: dst.clone(),
                });
                self.current_body.push(Instruction::Label(end_label));

                dst
            }
            BinaryOp::Or => {
                let dst = self.new_var();
                let true_label = self.new_label("or_true");
                let end_label = self.new_label("or_end");

                let vlhs = self.visit_expr(lhs);
                self.current_body
                    .push(Instruction::JumpIfNotZero(vlhs, true_label.clone()));
                let vrhs = self.visit_expr(rhs);
                self.current_body
                    .push(Instruction::JumpIfNotZero(vrhs, true_label.clone()));

                self.current_body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(0)),
                    dst: dst.clone(),
                });
                self.current_body.push(Instruction::Jump(end_label.clone()));

                self.current_body.push(Instruction::Label(true_label));
                self.current_body.push(Instruction::Copy {
                    src: Value::Literal(Literal::Integer(1)),
                    dst: dst.clone(),
                });
                self.current_body.push(Instruction::Label(end_label));

                dst
            }
            op => {
                let vlhs = self.visit_expr(lhs);
                let vrhs = self.visit_expr(rhs);
                let dst = self.new_var();
                self.current_body
                    .push(Instruction::Binary(op, vlhs, vrhs, dst.clone()));
                dst
            }
        }
    }

    fn visit_conditional(&mut self, expr: &ast::Conditional) -> Value {
        let else_label = self.new_label("else_condexpr");
        let end_label = self.new_label("end_condexpr");

        let result = self.visit_expr(&expr.cond);
        self.current_body
            .push(Instruction::JumpIfZero(result, else_label.clone()));

        let dst = self.new_var();

        let res_if = self.visit_expr(&expr.then_expr);
        self.current_body.push(Instruction::Copy {
            src: res_if,
            dst: dst.clone(),
        });
        self.current_body.push(Instruction::Jump(end_label.clone()));

        self.current_body.push(Instruction::Label(else_label));
        let res_else = self.visit_expr(&expr.else_expr);
        self.current_body.push(Instruction::Copy {
            src: res_else,
            dst: dst.clone(),
        });

        self.current_body.push(Instruction::Label(end_label));
        dst
    }

    fn visit_literal(&mut self, literal: &ast::WithToken<Literal>) -> Value {
        Value::Literal(**literal)
    }

    fn visit_unary(&mut self, expr: &ast::Unary) -> Value {
        let ast::Unary { op, expr, postfix } = expr;
        let expr_value = self.visit_expr(expr);
        let var = self.new_var();

        match **op {
            ast::UnaryOp::Increment => {
                if *postfix {
                    self.current_body.push(Instruction::Copy {
                        src: expr_value.clone(),
                        dst: var.clone(),
                    });
                    self.current_body.push(Instruction::Binary(
                        BinaryOp::Plus,
                        expr_value.clone(),
                        Value::Literal(Literal::Integer(1)),
                        expr_value,
                    ));
                } else {
                    self.current_body.push(Instruction::Binary(
                        BinaryOp::Plus,
                        expr_value.clone(),
                        Value::Literal(Literal::Integer(1)),
                        expr_value.clone(),
                    ));
                    self.current_body.push(Instruction::Copy {
                        src: expr_value,
                        dst: var.clone(),
                    });
                }
            }
            ast::UnaryOp::Decrement => {
                if *postfix {
                    self.current_body.push(Instruction::Copy {
                        src: expr_value.clone(),
                        dst: var.clone(),
                    });
                    self.current_body.push(Instruction::Binary(
                        BinaryOp::Minus,
                        expr_value.clone(),
                        Value::Literal(Literal::Integer(1)),
                        expr_value,
                    ));
                } else {
                    self.current_body.push(Instruction::Binary(
                        BinaryOp::Minus,
                        expr_value.clone(),
                        Value::Literal(Literal::Integer(1)),
                        expr_value.clone(),
                    ));
                    self.current_body.push(Instruction::Copy {
                        src: expr_value,
                        dst: var.clone(),
                    });
                }
            }
            _ => self
                .current_body
                .push(Instruction::Unary(**op, expr_value, var.clone())),
        }
        var
    }

    fn visit_var(&mut self, name: &ast::WithToken<String>) -> Value {
        Value::Var(name.0.clone())
    }
}

impl StmtRefVisitor<()> for GenerateTacky {
    fn visit_expression(&mut self, expr: &ast::Expr) -> () {
        self.visit_expr(expr);
    }

    fn visit_goto(&mut self, label: &WithToken<String>) -> () {
        self.current_body.push(Instruction::Jump((**label).clone()));
    }

    fn visit_if(&mut self, if_stmt: &ast::IfStmt) -> () {
        let (jump_not_true, end_label) = if if_stmt.else_clause.is_some() {
            (self.new_label("else"), self.new_label("end_if"))
        } else {
            (self.new_label("end_if"), self.new_label("end_if"))
        };

        let result = self.visit_expr(&if_stmt.cond);
        self.current_body
            .push(Instruction::JumpIfZero(result, jump_not_true.clone()));

        self.visit_stmt(&if_stmt.then);

        if let Some(ref else_clause) = if_stmt.else_clause {
            self.current_body.push(Instruction::Jump(end_label.clone()));
            self.current_body.push(Instruction::Label(jump_not_true));
            self.visit_stmt(&else_clause);
        }

        self.current_body.push(Instruction::Label(end_label));
    }

    fn visit_label(&mut self, label: &ast::Label) -> () {
        self.current_body
            .push(Instruction::Label((*label.name).clone()));
        self.visit_stmt(&label.next_stmt);
    }

    fn visit_null(&mut self) -> () {}

    fn visit_return(&mut self, ret_value: &ast::Expr) -> () {
        let ret_value = self.visit_expr(ret_value);
        self.current_body.push(Instruction::Return(ret_value));
    }
}

impl ASTRefVisitor for GenerateTacky {
    type BlockItemResult = ();
    type ExprResult = Value;
    type FuncDefResult = FunctionDef;
    type ProgramResult = Program;
    type StmtResult = ();
    type VarDeclResult = ();

    fn visit_block_item(&mut self, block_item: &ast::BlockItem) -> Self::BlockItemResult {
        match block_item {
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
            BlockItem::VarDecl(decl) => self.visit_var_decl(decl),
        };
    }

    fn visit_function_def(&mut self, function_def: &ast::FunctionDef) -> Self::FuncDefResult {
        function_def
            .body
            .iter()
            .for_each(|b| self.visit_block_item(b));
        self.current_body
            .push(Instruction::Return(Value::Literal(Literal::Integer(0))));
        let body = std::mem::replace(&mut self.current_body, Vec::new());
        FunctionDef {
            name: function_def.name.0.clone(),
            body,
        }
    }

    fn visit_program(&mut self, program: &ast::Program) -> Self::ProgramResult {
        let funs = program
            .0
            .iter()
            .map(|f| self.visit_function_def(f))
            .collect();
        Program(funs)
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDecl) -> Self::VarDeclResult {
        if let Some(ref expr) = var_decl.init {
            let dst = self.visit_var(&var_decl.name);
            let src = self.visit_expr(expr);
            self.current_body.push(Instruction::Copy { src, dst });
        }
    }
}
