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

impl ASTVisitor<String> for PrettyPrint {
    fn visit_program(&mut self, program: &Program) -> String {
        program
            .0
            .iter()
            .map(|def| self.visit_function_def(def))
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    fn visit_function_def(&mut self, function_def: &FunctionDef) -> String {
        self.indent += 1;
        let body_str = function_def
            .body
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Vec<_>>()
            .join("\n");
        self.indent -= 1;
        format!("{}():\n{}", function_def.name.blue(), body_str)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        self.indent += 1;
        let stmt_str = match stmt {
            Stmt::Return { ret_value } => {
                format!("{}\n{}", "RETURN".red(), self.visit_expr(ret_value))
            }
        };
        self.indent -= 1;
        format!("{}{}", Self::INDENT.repeat(self.indent), stmt_str)
    }

    fn visit_expr(&mut self, expr: &Expr) -> String {
        self.indent += 1;
        let expr_str = match expr {
            Expr::Literal(lit) => format!("{}", self.visit_literal(lit)),
            Expr::Unary { op, expr } => format!(
                "{}(\"{}\",\n{}\n{})",
                "UNARY".cyan().bold(),
                self.visit_unary_op(op),
                self.visit_expr(expr),
                Self::INDENT.repeat(self.indent - 1)
            ),
            Expr::Binary { op, lhs, rhs } => format!(
                "{}(\"{}\",\n{},\n{}\n{})",
                "BINARY".cyan().bold(),
                self.visit_binary_op(op),
                self.visit_expr(lhs),
                self.visit_expr(rhs),
                Self::INDENT.repeat(self.indent - 1)
            ),
        };
        self.indent -= 1;
        format!("{}{}", Self::INDENT.repeat(self.indent), expr_str)
    }

    fn visit_unary_op(&mut self, unary_op: &UnaryOp) -> String {
        match unary_op {
            UnaryOp::BitNOT => "~",
            UnaryOp::Minus => "-",
            UnaryOp::Not => "!",
        }
        .yellow()
        .to_string()
    }

    fn visit_binary_op(&mut self, binary_op: &BinaryOp) -> String {
        match binary_op {
            BinaryOp::Div => "/",
            BinaryOp::Minus => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Plus => "+",
            BinaryOp::Mod => "%",

            BinaryOp::BitwiseAND => "&",
            BinaryOp::BitwiseOR => "|",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",
            BinaryOp::BitwiseXOR => "^",

            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::Lesser => "<",
            BinaryOp::LesserEq => "<=",

            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
        .yellow()
        .to_string()
    }

    fn visit_literal(&mut self, literal: &Literal) -> String {
        match literal {
            Literal::Float(f) => format!("{}({})", "FLOAT".green(), f.to_string().magenta()),
            Literal::Integer(i) => format!("{}({})", "INT".green(), i.to_string().magenta()),
        }
    }
}
