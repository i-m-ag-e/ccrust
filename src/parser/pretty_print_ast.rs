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

    fn visit_literal(&mut self, literal: &WithToken<Literal>) -> String {
        match &literal.0 {
            Literal::Float(f) => format!("{}({})", "FLOAT".green(), f.to_string().magenta()),
            Literal::Integer(i) => format!("{}({})", "INT".green(), i.to_string().magenta()),
        }
    }

    fn visit_unary(&mut self, expr: &Unary) -> String {
        let Unary { op, expr } = expr;
        self.indent += 1;
        let expr_str = format!(
            "{0}(\"{1}\",\n{3}{2}\n{4})",
            "UNARY".cyan().bold(),
            op.to_string().yellow(),
            self.visit_expr(expr),
            Self::INDENT.repeat(self.indent),
            Self::INDENT.repeat(self.indent - 1)
        );
        self.indent -= 1;
        expr_str
    }

    fn visit_var(&mut self, name: &WithToken<String>) -> String {
        name.green().to_string()
    }
}

impl StmtRefVisitor<String> for PrettyPrint {
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

    fn visit_null(&mut self) -> String {
        format!("{}{}", Self::INDENT.repeat(self.indent), "NULL".red())
    }

    fn visit_return(&mut self, ret_value: &Expr) -> String {
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
}

impl ASTRefVisitor for PrettyPrint {
    type BlockItemResult = String;
    type ExprResult = String;
    type FuncDefResult = String;
    type ProgramResult = String;
    type StmtResult = String;
    type VarDeclResult = String;

    fn visit_program(&mut self, program: &Program) -> String {
        program
            .0
            .iter()
            .map(|def| self.visit_function_def(def))
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItemResult {
        match block_item {
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
            BlockItem::VarDecl(decl) => self.visit_var_decl(decl),
        }
    }

    fn visit_function_def(&mut self, function_def: &FunctionDef) -> String {
        self.indent += 1;
        let body_str = function_def
            .body
            .iter()
            .map(|item| self.visit_block_item(item))
            .collect::<Vec<_>>()
            .join("\n");
        self.indent -= 1;
        format!("{}():\n{}", function_def.name.blue(), body_str)
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
