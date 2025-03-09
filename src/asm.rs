use std::collections::HashMap;
use std::fmt::Display;

use crate::debug_info::DebugInfo;
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
                    Instruction::Mov { src, dest, .. } => {
                        replace_pseudo_operand!(self, src, pseudo_map);
                        replace_pseudo_operand!(self, dest, pseudo_map);
                    }
                    Instruction::Unary(_, operand, _) => {
                        replace_pseudo_operand!(self, operand, pseudo_map);
                    }
                    Instruction::Binary(_, lhs, rhs, _) => {
                        replace_pseudo_operand!(self, lhs, pseudo_map);
                        replace_pseudo_operand!(self, rhs, pseudo_map);
                    }
                    Instruction::Idiv(operand, _) => {
                        replace_pseudo_operand!(self, operand, pseudo_map);
                        replace_pseudo_operand!(self, operand, pseudo_map);
                    }
                    Instruction::Cmp(op1, op2, _) => {
                        replace_pseudo_operand!(self, op1, pseudo_map);
                        replace_pseudo_operand!(self, op2, pseudo_map);
                    }
                    Instruction::SetCC(_, op, _) => {
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
                def.body.insert(
                    0,
                    Instruction::AllocateStack(
                        def.stack_size,
                        DebugInfo::new(0, format!("stack size: {}", def.stack_size)),
                    ),
                );
            }

            let mut index = 0;
            while index < def.body.len() {
                match def.body[index].clone() {
                    Instruction::Mov {
                        src: Operand::Stack(src),
                        dest: Operand::Stack(dest),
                        debug_info,
                    } => {
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: Operand::Stack(src),
                                dest: Operand::Reg(Register::R10),
                                debug_info: debug_info.more_info(format!("(stack mov {0} -> {1}) {0} -> %r10 (temp)", src, dest)),
                            },
                            Instruction::Mov {
                                src: Operand::Reg(Register::R10),
                                dest: Operand::Stack(dest),
                                debug_info: debug_info.with_more_info(format!("(stack mov {0} -> {1}) %r10 -> {1}", src, dest)),
                            }
                        );
                    }
                    Instruction::Binary(
                        op,
                        Operand::Stack(val),
                        Operand::Stack(src),
                        debug_info,
                    ) if REGULAR_BINARY_OPS.contains(&op) => {
                        // addq -m(%rbp), -n(%rbp)
                        // ---
                        // movq -m(%rbp), %r10
                        // addl %r10, -n(%rbp)
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: Operand::Stack(val),
                                dest: Operand::Reg(Register::R10),
                                debug_info: debug_info.more_info(format!("(stack binary {1} {0} {2}) {2} -> %r10 (temp)", op, src, val)),
                            },
                            Instruction::Binary(
                                op,
                                Operand::Reg(Register::R10),
                                Operand::Stack(src),
                                debug_info.with_more_info(format!("(stack binary {1} {0} {2})", op, src, val)),
                            )
                        );
                    }
                    Instruction::Binary(
                        BinaryOp::Mul,
                        rhs,
                        lhs @ Operand::Stack(n),
                        debug_info,
                    ) => {
                        // imulq $3, -4(%rbp)
                        // ---
                        // movq -4(%rbp), %r11
                        // imulq $3, %r11
                        // movq %r11, -4(%rbp)
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: lhs.clone(),
                                dest: Operand::Reg(Register::R11),
                                debug_info: debug_info.more_info(format!("(mul on stack {0}) {0} -> %r11 (temp)", n)),
                            },
                            Instruction::Binary(BinaryOp::Mul, rhs, Operand::Reg(Register::R11), debug_info.more_info(format!("(mul on stack {})", n))),
                            Instruction::Mov {
                                src: Operand::Reg(Register::R11),
                                dest: lhs,
                                debug_info: debug_info.with_more_info(format!("(mul on stack {0}) %r11 -> {0}", n)),
                            }
                        );
                    }
                    Instruction::Cmp(
                        op1 @ Operand::Stack(n1),
                        op2 @ Operand::Stack(n2),
                        debug_info,
                    ) => {
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: op1,
                                dest: Operand::Reg(Register::R11),
                                debug_info: debug_info.more_info(format!("(stack cmp {0} {1}) {0} -> %r11 (temp)", n1, n2)),
                            },
                            Instruction::Cmp(
                                Operand::Reg(Register::R11), op2,
                                debug_info.with_more_info(format!("(stack cmp {0} {1})", n1, n2))
                            )
                        );
                    }
                    Instruction::Cmp(op1, op2 @ Operand::Imm(n), debug_info) => {
                        replace_instruction!(def.body; [index] =>
                            Instruction::Mov {
                                src: op2,
                                dest: Operand::Reg(Register::R11),
                                debug_info: debug_info.more_info(format!("(cmp with imm) ${} -> %r11 (temp)", n)),
                            },
                            Instruction::Cmp(op1, Operand::Reg(Register::R11), debug_info.with_more_info("cmp with imm".to_string()))
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
            indent_str(&["pushq\t%rbp", "movq\t%rsp, %rbp"]),
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
    AllocateStack(i32, DebugInfo),
    Binary(BinaryOp, Operand, Operand, DebugInfo),
    Cmp(Operand, Operand, DebugInfo),
    Cqo(DebugInfo),
    Idiv(Operand, DebugInfo),
    Jmp(String, DebugInfo),
    JmpCC(CondCode, String, DebugInfo),
    Label(String, DebugInfo),
    Mov {
        src: Operand,
        dest: Operand,
        debug_info: DebugInfo,
    },
    Ret(DebugInfo),
    SetCC(CondCode, Operand, DebugInfo),
    Unary(UnaryOp, Operand, DebugInfo),
}

impl Instruction {
    fn to_asm_string(&self) -> String {
        match self {
            Instruction::AllocateStack(n, info) => format!("subq\t${}, %rsp # {}", n, info),
            Instruction::Mov {
                src,
                dest,
                debug_info,
            } => {
                format!(
                    "movq\t{}, {}\t# {}",
                    src.to_asm_string(),
                    dest.to_asm_string(),
                    debug_info
                )
            }
            Instruction::Unary(op, operand, info) => {
                format!(
                    "{}\t{}\t# {}",
                    unary_op_to_asm(op),
                    operand.to_asm_string(),
                    info
                )
            }
            Instruction::Binary(op, val, src, info) => {
                format!(
                    "{:<4}\t{}, {}\t# {}",
                    binary_op_to_asm(op),
                    val.to_asm_string(),
                    src.to_asm_string(),
                    info
                )
            }
            Instruction::Cmp(lhs, rhs, info) => {
                format!(
                    "cmpq\t{}, {}\t# {}",
                    lhs.to_asm_string(),
                    rhs.to_asm_string(),
                    info
                )
            }
            Instruction::Label(label, info) => format!("\n{}:\t# {}", label, info),
            Instruction::Jmp(target, info) => format!("jmp\t\t{}\t# {}", target, info),
            Instruction::JmpCC(op, target, info) => format!("j{}\t{}\t# {}", op, target, info),
            Instruction::SetCC(op, dest, info) => {
                format!("set{}\t{}\t# {}", op, dest.to_asm_string(), info)
            }
            Instruction::Cqo(info) => format!("cqo\t# {}", info),
            Instruction::Idiv(operand, info) => {
                format!("idivq\t{}\t# {}", operand.to_asm_string(), info)
            }
            Instruction::Ret(info) => indent(&[
                format!("movq\t%rbp, %rsp\t# {}", info),
                format!("popq\t%rbp\t# {}", info),
                format!("ret\t# {}", info),
            ]),
        }
    }

    fn generate(inst: &tacky::Instruction, body: &mut Vec<Instruction>) {
        match inst {
            tacky::Instruction::Return(value, debug_info) => {
                let value = Operand::from(value);
                body.push(Instruction::Mov {
                    src: value,
                    dest: Operand::Reg(Register::RAX),
                    debug_info: debug_info.more_info("return value -> rax".to_string()),
                });
                body.push(Instruction::Ret(debug_info.clone()));
            }
            tacky::Instruction::Copy {
                src,
                dst,
                debug_info,
            } => {
                body.push(Instruction::Mov {
                    src: Operand::from(src),
                    dest: Operand::from(dst),
                    debug_info: debug_info.more_info(format!("{} -> {}", src, dst)),
                });
            }
            tacky::Instruction::Unary(op, src, dest, debug_info) => {
                let new_debug_info = debug_info.more_info(format!("unary {} {}", op, src));
                let src = Operand::from(src);
                let dest = Operand::from(dest);
                match op {
                    UnaryOp::Not => {
                        body.extend_from_slice(&[
                            Instruction::Cmp(Operand::Imm(0), src, new_debug_info.clone()),
                            Instruction::Mov {
                                src: Operand::Imm(0),
                                dest: dest.clone(),
                                debug_info: new_debug_info.clone(),
                            },
                            Instruction::SetCC(CondCode::E, dest, new_debug_info),
                        ]);
                    }
                    _ => {
                        body.extend_from_slice(&[
                            Instruction::Mov {
                                src,
                                dest: dest.clone(),
                                debug_info: new_debug_info.clone(),
                            },
                            Instruction::Unary(*op, dest, new_debug_info),
                        ]);
                    }
                }
            }
            tacky::Instruction::Binary(op, vlhs, vrhs, dest, debug_info) => {
                let new_debug_info =
                    debug_info.more_info(format!("binary ({}) {} {}", op, vlhs, vrhs));
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
                                debug_info: new_debug_info.clone(),
                            },
                            Instruction::Cqo(new_debug_info.clone()),
                        ]);

                        if let Operand::Imm(_) = vrhs {
                            body.push(Instruction::Mov {
                                src: vrhs,
                                dest: Operand::Reg(Register::R10),
                                debug_info: new_debug_info.clone(),
                            });
                            vrhs = Operand::Reg(Register::R10);
                        }

                        body.extend_from_slice(&[
                            Instruction::Idiv(vrhs, new_debug_info.clone()),
                            Instruction::Mov {
                                src: result_reg,
                                dest,
                                debug_info: new_debug_info,
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
                            Instruction::Cmp(vrhs, vlhs, new_debug_info.clone()),
                            Instruction::Mov {
                                src: Operand::Imm(0),
                                dest: dest.clone(),
                                debug_info: new_debug_info.clone(),
                            },
                            Instruction::SetCC(op.into(), dest, new_debug_info),
                        ]);
                    }
                    _ => {
                        if vlhs != dest {
                            body.push(Instruction::Mov {
                                src: vlhs,
                                dest: dest.clone(),
                                debug_info: new_debug_info.clone(),
                            });
                        }
                        body.push(Instruction::Binary(*op, vrhs, dest, new_debug_info));
                    }
                }
            }
            tacky::Instruction::Jump(label, debug_info) => {
                body.push(Instruction::Jmp(label.clone(), debug_info.clone()));
            }

            tacky::Instruction::JumpIfZero(val, target, debug_info) => {
                let val = Operand::from(val);
                body.extend_from_slice(&[
                    Instruction::Cmp(Operand::Imm(0), val, debug_info.clone()),
                    Instruction::JmpCC(CondCode::E, target.clone(), debug_info.clone()),
                ]);
            }

            tacky::Instruction::JumpIfNotZero(val, target, debug_info) => {
                let val = Operand::from(val);
                body.extend_from_slice(&[
                    Instruction::Cmp(Operand::Imm(0), val, debug_info.clone()),
                    Instruction::JmpCC(CondCode::NE, target.clone(), debug_info.clone()),
                ]);
            }

            tacky::Instruction::Label(label, debug_info) => {
                body.push(Instruction::Label(label.clone(), debug_info.clone()))
            }
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
                ast::Literal::Integral(i) => match i {
                    ast::Integral::Integer(i) => Operand::Imm(*i),
                },
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

fn indent_str(lines: &[&str]) -> String {
    lines.join(&format!("\n{}", INDENT))
}

fn indent(lines: &[String]) -> String {
    lines.join(&format!("\n{}", INDENT))
}
