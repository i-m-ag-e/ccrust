use std::collections::HashMap;
use std::fmt::Display;

use crate::parser::ast::{self, UnaryOp};
use crate::tacky;

const INDENT: &str = "    ";

macro_rules! replace_pseudo_operand {
    ($self_: expr, $operand: expr, $pseudo_map: expr) => {
        if let Operand::PseudoReg(name) = $operand {
            if let Some(offset) = $pseudo_map.get(name) {
                *$operand = Operand::Stack(*offset);
            } else {
                $self_.stack_offset += 8;
                $pseudo_map.insert(name.clone(), -$self_.stack_offset);
                *$operand = Operand::Stack(-$self_.stack_offset);
            }
        }
    };
}

#[derive(Debug)]
pub struct Program {
    defs: Vec<FunctionDef>,
    stack_offset: i32,
}

impl Program {
    pub fn new(defs: Vec<FunctionDef>) -> Self {
        Program {
            defs,
            stack_offset: 0,
        }
    }

    pub fn to_asm_string(&self) -> String {
        format!(
            "{0}.section .text\n\n{1}\n{0}.section .note.GNU-stack,\"\",@progbits\n",
            INDENT,
            self.defs
                .iter()
                .map(|f| f.to_asm_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    fn resolve_pseudo(&mut self) {
        for def in self.defs.iter_mut() {
            let mut pseudo_map = HashMap::new();
            for inst in def.body.iter_mut() {
                match inst {
                    Instruction::Mov { src, dest } => {
                        replace_pseudo_operand!(self, src, pseudo_map);
                        replace_pseudo_operand!(self, dest, pseudo_map);
                    }
                    Instruction::Unary(_, operand) => {
                        replace_pseudo_operand!(self, operand, pseudo_map);
                    }
                    _ => {}
                };
            }

            def.stack_size = self.stack_offset;
            self.stack_offset = 0;
        }
    }

    fn alloc_stack_and_resolve_stack_ops(&mut self) {
        for def in self.defs.iter_mut() {
            if def.stack_size > 0 {
                def.body
                    .insert(0, Instruction::AllocateStack(def.stack_size));
            }

            let mut index = 0;
            while index < def.body.len() {
                if let Instruction::Mov {
                    src: Operand::Stack(src),
                    dest: Operand::Stack(dest),
                } = def.body[index]
                {
                    def.body[index] = Instruction::Mov {
                        src: Operand::Stack(src),
                        dest: Operand::Reg(Register::R10),
                    };
                    def.body.insert(
                        index + 1,
                        Instruction::Mov {
                            src: Operand::Reg(Register::R10),
                            dest: Operand::Stack(dest),
                        },
                    );
                    index += 1;
                }
                index += 1;
            }
        }
    }
}

impl From<&tacky::Program> for Program {
    fn from(program: &tacky::Program) -> Self {
        let mut program = Program::new(program.0.iter().map(FunctionDef::from).collect());
        program.resolve_pseudo();
        program.alloc_stack_and_resolve_stack_ops();
        program
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    name: String,
    body: Vec<Instruction>,
    stack_size: i32,
}

impl FunctionDef {
    fn to_asm_string(&self) -> String {
        format!(
            "{0}.globl {1}\n{1}:\n{0}{2}\n{0}{3}\n",
            INDENT,
            self.name,
            indent(&["pushq\t%rbp", "movq\t%rsp, %rbp"]),
            self.body
                .iter()
                .map(|s| s.to_asm_string())
                .collect::<Vec<String>>()
                .join(&format!("\n{}", INDENT))
        )
    }
}

impl From<&tacky::FunctionDef> for FunctionDef {
    fn from(function_def: &tacky::FunctionDef) -> Self {
        let mut body = Vec::new();
        function_def
            .body
            .iter()
            .for_each(|inst| Instruction::generate(inst, &mut body));
        FunctionDef {
            name: function_def.name.clone(),
            body: body,
            stack_size: 0,
        }
    }
}

fn unary_op_to_asm(op: &UnaryOp) -> &str {
    match op {
        UnaryOp::BitNOT => "notq",
        UnaryOp::Minus => "negq",
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dest: Operand },
    Unary(ast::UnaryOp, Operand),
    AllocateStack(i32),
    Ret,
}

impl Instruction {
    fn to_asm_string(&self) -> String {
        match self {
            Instruction::AllocateStack(n) => format!("subq\t${}, %rsp", n),
            Instruction::Mov { src, dest } => {
                format!("movq\t{}, {}", src.to_asm_string(), dest.to_asm_string())
            }
            Instruction::Unary(op, operand) => {
                format!("{}\t{}", unary_op_to_asm(op), operand.to_asm_string())
            }
            Instruction::Ret => indent(&["movq\t%rbp, %rsp", "popq\t%rbp", "ret"]),
        }
    }

    fn generate(inst: &tacky::Instruction, body: &mut Vec<Instruction>) {
        match inst {
            tacky::Instruction::Return(value) => {
                let value = Operand::from(value);
                body.push(Instruction::Mov {
                    src: value,
                    dest: Operand::Reg(Register::RAX),
                });
                body.push(Instruction::Ret);
            }
            tacky::Instruction::Unary(op, src, dest) => {
                let src = Operand::from(src);
                let dest = Operand::from(dest);
                body.push(Instruction::Mov {
                    src,
                    dest: dest.clone(),
                });
                body.push(Instruction::Unary(*op, dest));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i64),
    Reg(Register),
    PseudoReg(String),
    Stack(i32),
}

impl Operand {
    fn to_asm_string(&self) -> String {
        match self {
            Operand::Imm(n) => format!("${}", n),
            Operand::Reg(reg) => format!("%{}", reg.to_string()),
            Operand::PseudoReg(_) => unreachable!(),
            Operand::Stack(n) => format!("{}(%rbp)", n),
        }
    }
}

impl From<&tacky::Value> for Operand {
    fn from(value: &tacky::Value) -> Self {
        match value {
            tacky::Value::Literal(lit) => match lit {
                ast::Literal::Integer(i) => Operand::Imm(*i),
                ast::Literal::Float(_) => unimplemented!(),
            },
            tacky::Value::Var(var) => Operand::PseudoReg(var.clone()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
    RAX,
    R10,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Register::RAX => "rax",
                Register::R10 => "r10",
            }
        )
    }
}

fn indent(lines: &[&str]) -> String {
    lines.join(&format!("\n{}", INDENT))
}
