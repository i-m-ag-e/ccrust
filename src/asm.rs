use std::collections::HashMap;
use std::fmt::Display;

use crate::parser::ast::{self, BinaryOp, UnaryOp};
use crate::tacky;

const INDENT: &str = "    ";
const REGULAR_BINARY_OPS: [BinaryOp; 11] = [
    BinaryOp::Plus,
    BinaryOp::Minus,
    BinaryOp::Div,
    BinaryOp::Mod,
    BinaryOp::BitwiseAND,
    BinaryOp::BitwiseOR,
    BinaryOp::BitwiseXOR,
    BinaryOp::LShift,
    BinaryOp::RShift,
    BinaryOp::And,
    BinaryOp::Or,
];

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

macro_rules! replace_instruction {
    ( $arr: expr; [$index: expr] => $first_inst: expr, $( $rest: expr ),*) => {{
        $arr[$index] = $first_inst;

        $(
            $index += 1;
            $arr.insert($index, $rest);
        )*
    }};
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
                    Instruction::Binary(_, lhs, rhs) => {
                        replace_pseudo_operand!(self, lhs, pseudo_map);
                        replace_pseudo_operand!(self, rhs, pseudo_map);
                    }
                    Instruction::Idiv(operand) => {
                        replace_pseudo_operand!(self, operand, pseudo_map);
                        replace_pseudo_operand!(self, operand, pseudo_map);
                    }
                    Instruction::Cmp(op1, op2) => {
                        replace_pseudo_operand!(self, op1, pseudo_map);
                        replace_pseudo_operand!(self, op2, pseudo_map);
                    }
                    Instruction::SetCC(_, op) => {
                        replace_pseudo_operand!(self, op, pseudo_map);
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
                match def.body[index].clone() {
                    Instruction::Mov {
                        src: Operand::Stack(src),
                        dest: Operand::Stack(dest),
                    } => {
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: Operand::Stack(src),
                                dest: Operand::Reg(Register::R10),
                            },
                            Instruction::Mov {
                                src: Operand::Reg(Register::R10),
                                dest: Operand::Stack(dest),
                            }
                        );
                    }
                    Instruction::Binary(op, Operand::Stack(val), Operand::Stack(src))
                        if REGULAR_BINARY_OPS.contains(&op) =>
                    {
                        // addq -m(%rbp), -n(%rbp)
                        // ---
                        // movq -m(%rbp), %r10
                        // addl %r10, -n(%rbp)
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: Operand::Stack(val),
                                dest: Operand::Reg(Register::R10),
                            },
                            Instruction::Binary(
                                op,
                                Operand::Reg(Register::R10),
                                Operand::Stack(src),
                            )
                        );
                    }
                    Instruction::Binary(BinaryOp::Mul, rhs, lhs @ Operand::Stack(_)) => {
                        // imulq $3, -4(%rbp)
                        // ---
                        // movq -4(%rbp), %r11
                        // imulq $3, %r11
                        // movq %r11, -4(%rbp)
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: lhs.clone(),
                                dest: Operand::Reg(Register::R11),
                            },
                            Instruction::Binary(BinaryOp::Mul, rhs, Operand::Reg(Register::R11)),
                            Instruction::Mov {
                                src: Operand::Reg(Register::R11),
                                dest: lhs,
                            }
                        );
                    }
                    Instruction::Cmp(op1 @ Operand::Stack(_), op2 @ Operand::Stack(_)) => {
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: op1,
                                dest: Operand::Reg(Register::R11),
                            },
                            Instruction::Cmp(Operand::Reg(Register::R11), op2)
                        );
                    }
                    Instruction::Cmp(op1, op2 @ Operand::Imm(_)) => {
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: op2,
                                dest: Operand::Reg(Register::R11),
                            },
                            Instruction::Cmp(op1, Operand::Reg(Register::R11))
                        );
                    }
                    _ => {}
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
            body,
            stack_size: 0,
        }
    }
}

fn unary_op_to_asm(op: &UnaryOp) -> &str {
    match op {
        UnaryOp::BitNOT => "notq",
        UnaryOp::Minus => "negq",
        UnaryOp::Not | UnaryOp::Increment | UnaryOp::Decrement => unreachable!(),
    }
}

fn binary_op_to_asm(op: &BinaryOp) -> &str {
    match op {
        BinaryOp::Plus => "addq",
        BinaryOp::Minus => "subq",
        BinaryOp::Mul => "imulq",
        BinaryOp::BitwiseAND => "andq",
        BinaryOp::BitwiseOR => "orq",
        BinaryOp::BitwiseXOR => "xorq",
        BinaryOp::LShift => "shlq",
        BinaryOp::RShift => "shrq",
        BinaryOp::And
        | BinaryOp::Div
        | BinaryOp::Mod
        | BinaryOp::Eq
        | BinaryOp::NotEq
        | BinaryOp::Greater
        | BinaryOp::GreaterEq
        | BinaryOp::Lesser
        | BinaryOp::LesserEq
        | BinaryOp::Or => unreachable!(),
    }
}

#[derive(Debug, Clone)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

impl Display for CondCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CondCode::E => "e",
                CondCode::NE => "ne",
                CondCode::G => "g",
                CondCode::GE => "ge",
                CondCode::L => "l",
                CondCode::LE => "le",
            }
        )
    }
}

impl From<&BinaryOp> for CondCode {
    fn from(value: &BinaryOp) -> Self {
        match value {
            BinaryOp::Eq => CondCode::E,
            BinaryOp::NotEq => CondCode::NE,
            BinaryOp::Greater => CondCode::G,
            BinaryOp::GreaterEq => CondCode::GE,
            BinaryOp::Lesser => CondCode::L,
            BinaryOp::LesserEq => CondCode::LE,
            _ => panic!("Invalid conversion of {:?} to CondCond", value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    AllocateStack(i32),
    Binary(BinaryOp, Operand, Operand),
    Cmp(Operand, Operand),
    Cqo,
    Idiv(Operand),
    Jmp(String),
    JmpCC(CondCode, String),
    Label(String),
    Mov { src: Operand, dest: Operand },
    Ret,
    SetCC(CondCode, Operand),
    Unary(UnaryOp, Operand),
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
            Instruction::Binary(op, val, src) => {
                format!(
                    "{:<4}\t{}, {}",
                    binary_op_to_asm(op),
                    val.to_asm_string(),
                    src.to_asm_string()
                )
            }
            Instruction::Cmp(lhs, rhs) => {
                format!("cmpq\t{}, {}", lhs.to_asm_string(), rhs.to_asm_string())
            }
            Instruction::Label(label) => format!("{}:", label),
            Instruction::Jmp(target) => format!("jmp\t\t{}", target),
            Instruction::JmpCC(op, target) => format!("j{}\t{}", op, target),
            Instruction::SetCC(op, dest) => format!("set{}\t{}", op, dest.to_asm_string()),
            Instruction::Cqo => "cqo".to_string(),
            Instruction::Idiv(operand) => format!("idivq\t{}", operand.to_asm_string()),
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
            tacky::Instruction::Copy { src, dst } => {
                body.push(Instruction::Mov {
                    src: Operand::from(src),
                    dest: Operand::from(dst),
                });
            }
            tacky::Instruction::Unary(op, src, dest) => {
                let src = Operand::from(src);
                let dest = Operand::from(dest);
                match op {
                    UnaryOp::Not => {
                        body.extend_from_slice(&[
                            Instruction::Cmp(Operand::Imm(0), src),
                            Instruction::Mov {
                                src: Operand::Imm(0),
                                dest: dest.clone(),
                            },
                            Instruction::SetCC(CondCode::E, dest),
                        ]);
                    }
                    _ => {
                        body.extend_from_slice(&[
                            Instruction::Mov {
                                src,
                                dest: dest.clone(),
                            },
                            Instruction::Unary(*op, dest),
                        ]);
                    }
                }
            }
            tacky::Instruction::Binary(op, vlhs, vrhs, dest) => {
                let vlhs = Operand::from(vlhs);
                let mut vrhs = Operand::from(vrhs);
                let dest = Operand::from(dest);
                match op {
                    BinaryOp::Mod | BinaryOp::Div => {
                        let result_reg = if let BinaryOp::Div = op {
                            Operand::Reg(Register::RAX)
                        } else {
                            Operand::Reg(Register::RDX)
                        };
                        body.extend_from_slice(&[
                            Instruction::Mov {
                                src: vlhs,
                                dest: Operand::Reg(Register::RAX),
                            },
                            Instruction::Cqo,
                        ]);

                        if let Operand::Imm(_) = vrhs {
                            body.push(Instruction::Mov {
                                src: vrhs,
                                dest: Operand::Reg(Register::R10),
                            });
                            vrhs = Operand::Reg(Register::R10);
                        }

                        body.extend_from_slice(&[
                            Instruction::Idiv(vrhs),
                            Instruction::Mov {
                                src: result_reg,
                                dest,
                            },
                        ]);
                    }
                    BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::LesserEq
                    | BinaryOp::Lesser
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEq => {
                        body.extend_from_slice(&[
                            Instruction::Cmp(vrhs, vlhs),
                            Instruction::Mov {
                                src: Operand::Imm(0),
                                dest: dest.clone(),
                            },
                            Instruction::SetCC(op.into(), dest),
                        ]);
                    }
                    _ => {
                        if vlhs != dest {
                            body.push(Instruction::Mov {
                                src: vlhs,
                                dest: dest.clone(),
                            });
                        }
                        body.push(Instruction::Binary(*op, vrhs, dest));
                    }
                }
            }
            tacky::Instruction::Jump(label) => {
                body.push(Instruction::Jmp(label.clone()));
            }

            tacky::Instruction::JumpIfZero(val, target) => {
                let val = Operand::from(val);
                body.extend_from_slice(&[
                    Instruction::Cmp(Operand::Imm(0), val),
                    Instruction::JmpCC(CondCode::E, target.clone()),
                ]);
            }

            tacky::Instruction::JumpIfNotZero(val, target) => {
                let val = Operand::from(val);
                body.extend_from_slice(&[
                    Instruction::Cmp(Operand::Imm(0), val),
                    Instruction::JmpCC(CondCode::NE, target.clone()),
                ]);
            }

            tacky::Instruction::Label(label) => body.push(Instruction::Label(label.clone())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    AL,
    RAX,
    RDX,
    R10,
    R11,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Register::AL => "al",
                Register::RAX => "rax",
                Register::RDX => "rdx",
                Register::R10 => "r10",
                Register::R11 => "r11",
            }
        )
    }
}

fn indent(lines: &[&str]) -> String {
    lines.join(&format!("\n{}", INDENT))
}
