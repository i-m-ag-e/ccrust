use super::ast::*;
use colored::Colorize;

// fn unary_op_to_str(op: &UnaryOp) -> &str {
//     match op {
//         UnaryOp::BitNOT => "~",
//         UnaryOp::Minus => "-",
//     }
// }

// fn binary_op_to_str(op: &BinaryOp) -> &str {
//     match op {
//         BinaryOp::Div => "/",
//         BinaryOp::Minus => "-",
//         BinaryOp::Mul => "*",
//         BinaryOp::Plus => "+",
//     }
// }

pub struct PrettyPrint {
    indent: usize,
}

impl PrettyPrint {
    const INDENT: &'static str = "    ";
    pub fn new() -> Self {
        Self { indent: 0 }
    }

    fn indent_now(&self) -> String {
        Self::INDENT.repeat(self.indent - 1)
    }

    fn indent_inside(&self) -> String {
        Self::INDENT.repeat(self.indent)
    }
}

impl ExprRefVisitor<String> for PrettyPrint {
    fn visit_assign(&mut self, expr: &Assign) -> String {
        self.indent += 1;
        let expr_str = format!(
            "({} {} {})",
            self.visit_expr(&expr.lhs),
            "=".yellow().to_string(),
            self.visit_expr(&expr.rhs)
        );
        self.indent -= 1;
        expr_str
    }

    fn visit_binary(&mut self, expr: &Binary) -> String {
        let Binary { op, lhs, rhs } = expr;
        self.indent += 1;

        let expr_str = format!(
            "{0}(\"{1}\",\n{4}{2},\n{4}{3}\n{5})",
            "BINARY".cyan().bold(),
            op.to_string().yellow(),
            self.visit_expr(lhs),
            self.visit_expr(rhs),
            Self::INDENT.repeat(self.indent),
            Self::INDENT.repeat(self.indent - 1)
        );

        self.indent -= 1;
        expr_str
    }

    fn visit_comma(&mut self, expr: &Comma) -> String {
        self.indent += 1;
        let expr_str = format!(
            "{0}(\"{}\",\n{3}{1},\n{3}{2}\n{4})",
            "COMMA".cyan().bold(),
            self.visit_expr(&expr.0),
            self.visit_expr(&expr.1),
            Self::INDENT.repeat(self.indent),
            Self::INDENT.repeat(self.indent - 1)
        );
        self.indent -= 1;
        expr_str
    }

    fn visit_conditional(&mut self, expr: &Conditional) -> String {
        self.indent += 1;

        let cond_str = self.visit_expr(&expr.cond);
        self.indent += 1;
        let then_str = self.visit_expr(&expr.then_expr);
        let else_str = self.visit_expr(&expr.else_expr);
        self.indent -= 1;

        let expr_str = format!(
            "{0}\n{6}{1}\n{6}{2}   {3}\n{6}{4}   {5}",
            "CONDITIONAL".cyan().bold(),
            cond_str,
            "?".yellow(),
            then_str,
            ":".yellow(),
            else_str,
            self.indent_inside()
        );

        self.indent -= 1;
        expr_str
    }

    fn visit_function_call(&mut self, call: &FunctionCall) -> String {
        let FunctionCall { name, args } = call;
        let Expr::Var(ref name) = **name else {
            unreachable!()
        };
        self.indent += 1;

        let call_str = format!(
            "({0}) {1}(\n{2}\n{3})",
            "CALL".red(),
            name.blue(),
            args.iter()
                .map(|arg| format!("{}{}", self.indent_inside(), self.visit_expr(arg)))
                .collect::<Vec<_>>()
                .join(",\n"),
            self.indent_now()
        );

        self.indent -= 1;
        call_str
    }

    fn visit_literal(&mut self, literal: &WithToken<Literal>) -> String {
        match &literal.0 {
            Literal::Float(f) => format!("{}({})", "FLOAT".green(), f.to_string().magenta()),
            Literal::Integral(Integral::Integer(i)) => {
                format!("{}({})", "INT".green(), i.to_string().magenta())
            }
        }
    }

    fn visit_unary(&mut self, expr: &Unary) -> String {
        let Unary { op, expr, postfix } = expr;
        self.indent += 1;
        let expr_str = format!(
            "{0}(\"{1}\"{5},\n{3}{2}\n{4})",
            "UNARY".cyan().bold(),
            op.to_string().yellow(),
            self.visit_expr(expr),
            Self::INDENT.repeat(self.indent),
            Self::INDENT.repeat(self.indent - 1),
            if *postfix {
                format!(", ({})", "POST".green())
            } else {
                "".to_string()
            }
        );
        self.indent -= 1;
        expr_str
    }

    fn visit_var(&mut self, name: &WithToken<String>) -> String {
        name.green().to_string()
    }
}

impl StmtRefVisitor<String> for PrettyPrint {
    fn visit_break(&mut self, break_stmt: &BreakStmt) -> String {
        format!(
            "{0}{1}({2} {3})",
            self.indent_inside(),
            "BREAK".red(),
            if break_stmt.loop_or_switch {
                "LOOP".yellow()
            } else {
                "SWITCH".yellow()
            },
            *break_stmt.id
        )
    }

    fn visit_continue(&mut self, id: &WithToken<i32>) -> String {
        format!("{0}{1}({2})", self.indent_inside(), "CONTINUE".red(), **id)
    }

    fn visit_compound(&mut self, stmt: &CompoundStmt) -> String {
        self.indent += 1;
        let block_str = format!(
            "{0}{1}\n{3}\n{0}{2}",
            self.indent_now(),
            "{".yellow().bold(),
            "}".yellow().bold(),
            stmt.block
                .0
                .iter()
                .map(|item| self.visit_block_item(item))
                .collect::<Vec<_>>()
                .join("\n")
        );
        self.indent -= 1;
        block_str
    }

    fn visit_expression(&mut self, expr: &Expr) -> String {
        self.indent += 1;
        let expr_str = format!(
            "{0}{1}\x0b{2}",
            Self::INDENT.repeat(self.indent - 1),
            "EXPR".red(),
            self.visit_expr(expr)
        );
        self.indent -= 1;
        expr_str
    }

    fn visit_for(&mut self, for_stmt: &ForStmt) -> String {
        self.indent += 1;

        let initializer_str = match &*for_stmt.initializer {
            Some(ForStmtInitializer::Expr(expr)) => self.visit_expr(expr),
            Some(ForStmtInitializer::VarDecl(decls)) => decls
                .iter()
                .map(|decl| self.visit_var_decl(decl))
                .collect::<Vec<_>>()
                .join("\n"),
            None => "".to_string(),
        };

        let condition_str = if let Some(cond) = &*for_stmt.condition {
            self.visit_expr(cond)
        } else {
            "".to_string()
        };

        let step_str = if let Some(step) = &*for_stmt.step {
            self.visit_expr(step)
        } else {
            "".to_string()
        };

        let for_str = format!(
            "{0}{1}({7}) (\n{0}  {2}{6}\n{0}  {3}{6}\n{0}  {4}\n{0})\n{5}",
            self.indent_now(),                // 0
            "FOR".red(),                      // 1
            initializer_str,                  // 2
            condition_str,                    // 3
            step_str,                         // 4
            self.visit_stmt(&*for_stmt.body), // 5
            ";".yellow(),                     // 6
            for_stmt.loop_id                  // 7
        );

        self.indent -= 1;
        for_str
    }

    fn visit_goto(&mut self, label: &WithToken<String>) -> String {
        format!(
            "{}{} {}",
            Self::INDENT.repeat(self.indent),
            "GOTO".red(),
            label.yellow().bold()
        )
    }

    fn visit_if(&mut self, if_stmt: &IfStmt) -> String {
        self.indent += 1;
        let cond_str = format!(
            "{0}{1}\x0b{2}\n{0}  {3}\n{4}{5}",
            self.indent_now(),
            "IF".red(),
            self.visit_expr(&if_stmt.cond),
            "THEN".magenta(),
            self.visit_stmt(&*if_stmt.then),
            if let Some(ref else_clause) = if_stmt.else_clause {
                format!(
                    "\n{0}  {1}\n{2}",
                    self.indent_now(),
                    "ELSE".magenta(),
                    self.visit_stmt(else_clause)
                )
            } else {
                "".to_string()
            }
        );
        self.indent -= 1;
        cond_str
    }

    fn visit_label(&mut self, label: &Label) -> String {
        self.indent += 1;
        let label_str = format!(
            "{}{} {}:\n{}",
            Self::INDENT.repeat(self.indent - 1),
            "LABEL".red(),
            label.name.yellow().bold(),
            self.visit_stmt(&*label.next_stmt)
        );
        self.indent -= 1;
        label_str
    }

    fn visit_null(&mut self) -> String {
        format!("{}{}", Self::INDENT.repeat(self.indent), "NULL".red())
    }

    fn visit_return(&mut self, ret_value: &WithToken<Expr>) -> String {
        self.indent += 1;
        let ret_str = format!(
            "{}{} \x0b{}",
            Self::INDENT.repeat(self.indent - 1),
            "RETURN".red(),
            self.visit_expr(ret_value)
        );
        self.indent -= 1;
        ret_str
    }

    fn visit_switch(&mut self, switch_stmt: &SwitchStmt) -> String {
        self.indent += 1;
        let switch_str = format!(
            "{0}{1}\n{2}\n{0}{3}:{4}",
            self.indent_now(),
            "SWITCH".red(),
            switch_stmt
                .cases
                .iter()
                .map(|case| format!(
                    "{}{} {:?}:\n{}",
                    self.indent_inside(),
                    "CASE".red(),
                    *case.value,
                    self.visit_compound(&case.stmt)
                ))
                .collect::<Vec<_>>()
                .join("\n"),
            "DEFAULT".yellow(),
            self.visit_compound(&switch_stmt.default)
        );
        self.indent -= 1;
        switch_str
    }

    fn visit_while(&mut self, while_stmt: &WhileStmt) -> String {
        self.indent += 1;
        let while_str = format!(
            "{0}{4}{1}({5})\x0b{2}\n{0} \n{3}",
            self.indent_now(),
            "WHILE".red(),
            self.visit_expr(&while_stmt.cond),
            self.visit_stmt(&*while_stmt.body),
            if while_stmt.do_while {
                "DO ".red().to_string()
            } else {
                "".to_string()
            },
            while_stmt.loop_id
        );
        self.indent -= 1;
        while_str
    }
}

impl ASTRefVisitor for PrettyPrint {
    type BlockItemResult = String;
    type ExprResult = String;
    type FuncDeclResult = String;
    type ProgramResult = String;
    type StmtResult = String;
    type VarDeclResult = String;

    fn visit_program(&mut self, program: &Program) -> String {
        program
            .0
            .iter()
            .map(|def| self.visit_function_decl(def))
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItemResult {
        match block_item {
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
            BlockItem::VarDecl(decls) => decls
                .iter()
                .map(|decl| self.visit_var_decl(decl))
                .collect::<Vec<_>>()
                .join("\n"),
            BlockItem::FunctionDecl(decl) => self.visit_function_decl(decl),
        }
    }

    fn visit_function_decl(&mut self, function_decl: &FunctionDecl) -> String {
        self.indent += 1;
        let body_str = function_decl
            .body
            .as_ref()
            .map(|body| {
                body.block
                    .0
                    .iter()
                    .map(|item| self.visit_block_item(item))
                    .collect::<Vec<_>>()
                    .join("\n")
            })
            .unwrap_or(format!("{};", self.indent_inside()));
        let args = function_decl
            .params
            .iter()
            .map(|i| i.yellow().to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let func_str = format!(
            "{}({}) {}({}):\n{}",
            self.indent_now(),
            if function_decl.body.is_some() {
                "DEF".red()
            } else {
                "DECL".red()
            },
            function_decl.name.blue(),
            args,
            body_str
        );
        self.indent -= 1;
        func_str
    }

    fn visit_var_decl(&mut self, var_decl: &VarDecl) -> String {
        self.indent += 1;
        let decl_str = format!(
            "{}{} {} {}",
            Self::INDENT.repeat(self.indent - 1),
            "VAR".red(),
            var_decl.name.green().bold(),
            if let Some(expr) = &var_decl.init {
                format!(" = {}", self.visit_expr(expr))
            } else {
                String::new()
            }
        );
        self.indent -= 1;
        decl_str
    }
}
