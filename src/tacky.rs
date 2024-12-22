use std::fmt::Display;

use crate::{
    debug_info::DebugInfo,
    parser::ast::{
        self, ASTRefVisitor, BinaryOp, BlockItem, ExprRefVisitor, Literal, StmtRefVisitor,
        WithToken,
    },
};

macro_rules! tacky {
    ( $body: expr; $line: expr; $d_info_name: ident;
        $( $var: ident <- $e: expr; $( $inst: expr => $desc: expr ),* $(,)? );*
        $(;)?
    ) => {
        let line = $line;
        $(
            let $var = $e;
            $(
                let $d_info_name = DebugInfo::new(line, $desc);
                let inst = $inst;
                $body.push(inst);
            )*
        )*
    };

    ( $body: expr; $line: expr; $d_info_name: ident;
        $( $inst: expr => $desc: expr ),* $(,)?
    ) => {
        let line = $line;
        $(
            let $d_info_name = DebugInfo::new(line, $desc);
            let inst = $inst;
            $body.push(inst);
        )*
    };
}

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
    Return(Value, DebugInfo),
    Unary(ast::UnaryOp, Value, Value, DebugInfo),
    Binary(ast::BinaryOp, Value, Value, Value, DebugInfo),
    Copy {
        src: Value,
        dst: Value,
        debug_info: DebugInfo,
    },
    Jump(Target, DebugInfo),
    JumpIfZero(Value, Target, DebugInfo),
    JumpIfNotZero(Value, Target, DebugInfo),
    Label(String, DebugInfo),
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(ast::Literal),
    Var(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Literal(lit) => write!(f, "${}", lit),
            Value::Var(var) => write!(f, "{}", var),
        }
    }
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
            src: result.clone(),
            dst: dest.clone(),
            debug_info: DebugInfo::new(
                expr.eq_sign.line,
                format!(
                    "(assignment) `{}` ({}) = {} (result)",
                    if let Value::Var(ref name) = dest {
                        name
                    } else {
                        unreachable!()
                    },
                    dest,
                    result
                ),
            ),
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

                tacky! {
                    self.current_body;
                    expr.op.1.line;
                    debug_info;

                    vlhs <- self.visit_expr(lhs);
                    Instruction::JumpIfZero(vlhs, false_label.clone(), debug_info)
                        => format!("(&&) when lhs `{}` -> 0", vlhs);

                    vrhs <- self.visit_expr(rhs);
                    Instruction::JumpIfZero(vrhs, false_label.clone(), debug_info)
                        => format!("(&&) when rhs `{}` also -> 0", vrhs),
                    Instruction::Copy {
                        src: Value::Literal(Literal::Integer(1)),
                        dst: dst.clone(),
                        debug_info
                    } => format!("(&&) (both true) `{}` (result) = 1", dst),
                    Instruction::Jump(end_label.clone(), debug_info)
                        => format!("(&&) (both true) jump to end"),
                    Instruction::Label(false_label, debug_info) => "".to_string(),
                    Instruction::Copy {
                        src: Value::Literal(Literal::Integer(0)),
                        dst: dst.clone(),
                        debug_info
                    } => format!("(&&) (false) `{}` (result) = 0", dst),
                    Instruction::Label(end_label, debug_info) => "".to_string(),
                }

                dst
            }
            BinaryOp::Or => {
                let dst = self.new_var();
                let true_label = self.new_label("or_true");
                let end_label = self.new_label("or_end");

                tacky! {
                    self.current_body;
                    expr.op.1.line;
                    debug_info;

                    vlhs <- self.visit_expr(lhs);
                    Instruction::JumpIfNotZero(vlhs, true_label.clone(), debug_info)
                        => format!("(||) when lhs `{}` is true", vlhs);
                    vrhs <- self.visit_expr(rhs);
                    Instruction::JumpIfNotZero(vrhs, true_label.clone(), debug_info)
                        => format!("(||) when rhs `{}` is true", vrhs),
                    Instruction::Copy {
                        src: Value::Literal(Literal::Integer(0)),
                        dst: dst.clone(),
                        debug_info
                    } => format!("(||) (both false) `{}` (result) = 0", dst),
                    Instruction::Jump(end_label.clone(), debug_info)
                        => "(||) (both false) jump to end".to_string(),

                    Instruction::Label(true_label, debug_info) => "".to_string(),
                    Instruction::Copy {
                        src: Value::Literal(Literal::Integer(1)),
                        dst: dst.clone(),
                        debug_info
                    } => format!("(||) (true) `{}` (result) = 1", dst),
                    Instruction::Label(end_label, debug_info) => "".to_string(),
                }

                dst
            }
            op => {
                let vlhs = self.visit_expr(lhs);
                let vrhs = self.visit_expr(rhs);
                let dst = self.new_var();
                let description = format!("`{}` {} `{}`", vlhs, op, vrhs);
                self.current_body.push(Instruction::Binary(
                    op,
                    vlhs,
                    vrhs,
                    dst.clone(),
                    DebugInfo::new(expr.op.1.line, description),
                ));
                dst
            }
        }
    }

    fn visit_conditional(&mut self, expr: &ast::Conditional) -> Value {
        let else_label = self.new_label("else_condexpr");
        let end_label = self.new_label("end_condexpr");

        tacky! {
            self.current_body;
            expr.qmark.line;
            debug_info;

            result <- self.visit_expr(&expr.cond);
            Instruction::JumpIfZero(result, else_label.clone(), debug_info)
                => "(c?t:e) cond false, jump to end".to_string();

            dst <- self.new_var();;
            res_if <- self.visit_expr(&expr.then_expr);
            Instruction::Copy {
                src: res_if,
                dst: dst.clone(),
                debug_info
            } => format!("(c?t:e) `{}` (result) = `{}` (then)", dst, res_if),
            Instruction::Jump(end_label.clone(), debug_info)
                => "(c?t:e)".to_string(),
            Instruction::Label(else_label, debug_info) => "(c?t:e) e:".to_string();

            res_else <- self.visit_expr(&expr.else_expr);
            Instruction::Copy {
                src: res_else,
                dst: dst.clone(),
                debug_info
            } => format!("(c?t:e) `{}` (result) = `{}` (else)", dst, res_else),

            Instruction::Label(end_label, debug_info) => "(c?t:e) end".to_string()
        }

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
                    tacky! {
                        self.current_body;
                        op.1.line;
                        debug_info;

                        Instruction::Copy {
                            src: expr_value.clone(),
                            dst: var.clone(),
                            debug_info
                        } => "(p++) copy final result of ++".to_string(),
                        Instruction::Binary(
                            BinaryOp::Plus,
                            expr_value.clone(),
                            Value::Literal(Literal::Integer(1)),
                            expr_value,
                            debug_info
                        ) => format!("(p++) {} += 1", expr_value)
                    }
                } else {
                    tacky! {
                        self.current_body;
                        op.1.line;
                        debug_info;

                        Instruction::Binary(
                            BinaryOp::Plus,
                            expr_value.clone(),
                            Value::Literal(Literal::Integer(1)),
                            expr_value.clone(),
                            debug_info
                        ) => format!("(++) {} += 1", expr_value),
                        Instruction::Copy {
                            src: expr_value,
                            dst: var.clone(),
                            debug_info
                        } => "(++) copy final result of ++".to_string()
                    }
                }
            }
            ast::UnaryOp::Decrement => {
                if *postfix {
                    tacky! {
                        self.current_body;
                        op.1.line;
                        debug_info;

                        Instruction::Copy {
                            src: expr_value.clone(),
                            dst: var.clone(),
                            debug_info
                        } => "(p--) copy final result of --".to_string(),
                        Instruction::Binary(
                            BinaryOp::Minus,
                            expr_value.clone(),
                            Value::Literal(Literal::Integer(1)),
                            expr_value,
                            debug_info
                        ) => format!("(p--) {} -= 1", expr_value)
                    }
                } else {
                    tacky! {
                        self.current_body;
                        op.1.line;
                        debug_info;

                        Instruction::Binary(
                            BinaryOp::Minus,
                            expr_value.clone(),
                            Value::Literal(Literal::Integer(1)),
                            expr_value.clone(),
                            debug_info
                        ) => format!("(--) {} -= 1", expr_value),
                        Instruction::Copy {
                            src: expr_value,
                            dst: var.clone(),
                            debug_info
                        } => "(--) copy final result of --".to_string()
                    }
                }
            }
            _ => {
                let description = format!("(unary) {} {}", **op, expr_value.to_string());
                self.current_body.push(Instruction::Unary(
                    **op,
                    expr_value,
                    var.clone(),
                    DebugInfo::new(op.1.line, description),
                ));
            }
        }
        var
    }

    fn visit_var(&mut self, name: &ast::WithToken<String>) -> Value {
        Value::Var(name.0.clone())
    }
}

impl StmtRefVisitor<()> for GenerateTacky {
    fn visit_compound(&mut self, block: &ast::Block) -> () {
        block.0.iter().for_each(|b| self.visit_block_item(b));
    }

    fn visit_expression(&mut self, expr: &ast::Expr) -> () {
        self.visit_expr(expr);
    }

    fn visit_goto(&mut self, label: &WithToken<String>) -> () {
        self.current_body.push(Instruction::Jump(
            (**label).clone(),
            DebugInfo::new(label.1.line, "".to_string()),
        ));
    }

    fn visit_if(&mut self, if_stmt: &ast::IfStmt) -> () {
        let (jump_not_true, end_label) = if if_stmt.else_clause.is_some() {
            (self.new_label("else"), self.new_label("end_if"))
        } else {
            let new_label = self.new_label("end_if");
            (new_label.clone(), new_label)
        };

        tacky! {
            self.current_body;
            if_stmt.cond.1.line;
            debug_info;

            result <- self.visit_expr(&if_stmt.cond);
            Instruction::JumpIfZero(result, jump_not_true.clone(), debug_info)
                => "(if) cond false".to_string();
            _then <- self.visit_stmt(&if_stmt.then);
        }

        if let Some(ref else_clause) = if_stmt.else_clause {
            tacky! {
                self.current_body;
                else_clause.1.line;
                debug_info;

                Instruction::Jump(end_label.clone(), debug_info)
                    => "(if) condition true (skip else)".to_string(),
                Instruction::Label(jump_not_true, debug_info)
                    => "".to_string()
            }
            self.visit_stmt(&else_clause);
        }

        self.current_body.push(Instruction::Label(
            end_label,
            DebugInfo::new(if_stmt.cond.1.line, "".to_string()),
        ));
    }

    fn visit_label(&mut self, label: &ast::Label) -> () {
        self.current_body.push(Instruction::Label(
            (*label.name).clone(),
            DebugInfo::new(label.name.1.line, "".to_string()),
        ));
        self.visit_stmt(&label.next_stmt);
    }

    fn visit_null(&mut self) -> () {}

    fn visit_return(&mut self, ret_value: &WithToken<ast::Expr>) -> () {
        let line = ret_value.1.line;
        let ret_value = self.visit_expr(ret_value);
        self.current_body.push(Instruction::Return(
            ret_value,
            DebugInfo::new(line, "".to_string()),
        ));
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
        self.visit_compound(&function_def.body);
        // self.current_body
        //     .push(Instruction::Return(Value::Literal(Literal::Integer(0))));
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
            self.current_body.push(Instruction::Copy {
                src,
                dst,
                debug_info: DebugInfo::new(
                    var_decl.name.1.line,
                    format!("(decl) {} = <init>", &*var_decl.name),
                ),
            });
        }
    }
}
