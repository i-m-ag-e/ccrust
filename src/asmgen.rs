use crate::tacky::{FunctionDef, Instruction, Program, Value};

const INDENT: &str = "    ";

pub trait AsmGen {
    fn gen(&self) -> String;
}

impl AsmGen for Program {
    fn gen(&self) -> String {
        format!(
            "{0}.section .text\n\n{1}\n{0}.section .note.GNU-stack,\"\",@progbits\n",
            INDENT,
            self.0
                .iter()
                .map(|f| f.gen())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl AsmGen for FunctionDef {
    fn gen(&self) -> String {
        format!(
            "{0}.globl {1}\n{1}:\n{0}{2}\n{0}ret\n",
            INDENT,
            self.name,
            self.body
                .iter()
                .map(|s| s.gen())
                .collect::<Vec<String>>()
                .join(&format!("\n{}", INDENT))
        )
    }
}

impl AsmGen for Stmt {
    fn gen(&self) -> String {
        match self {
            Stmt::Return { ret_value } => format!(
                "movq ${}, %rax",
                match ret_value {
                    Expr::Literal(Literal::Integer(i)) => i,
                    _ => unreachable!(),
                }
            ),
        }
    }
}
