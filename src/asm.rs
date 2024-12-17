use std::collections::HashMap;
use std::fmt::Display;

use crate::debug_info::{DebugInfo, WithDebugInfo};
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
                match &mut **inst {
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
                def.body.insert(
                    0,
                    WithDebugInfo::new(
                        Instruction::AllocateStack(def.stack_size),
                        DebugInfo {
                            line: 0,
                            description: "".to_string(),
                        },
                    ),
                );
            }

            let mut index = 0;
            while index < def.body.len() {
                match def.body[index].clone() {
                    WithDebugInfo {
                        value:
                            Instruction::Mov {
                                src: Operand::Stack(src),
                                dest: Operand::Stack(dest),
                            },
                        debug_info,
                    } => {
                        replace_instruction!(def.body; [index] =>
                            WithDebugInfo::new(Instruction::Mov {
                                src: Operand::Stack(src),
                                dest: Operand::Reg(Register::R10),
                            }, DebugInfo { line: debug_info.line, description: format!("({}) (stack mov {} -> {}) {} -> r10 (temp)", debug_info.description, src, dest, src) }),
                            WithDebugInfo::new(Instruction::Mov {
                                src: Operand::Reg(Register::R10),
                                dest: Operand::Stack(dest),
                            }, DebugInfo { line: debug_info.line, description: format!("({}) (stack mov {} -> {}) r10 -> {}", debug_info.description, src, dest, dest) })
                        );
                    }
                    WithDebugInfo {
                        value: Instruction::Binary(op, Operand::Stack(val), Operand::Stack(src)),
                        debug_info,
                    } if REGULAR_BINARY_OPS.contains(&op) => {
                        // addq -m(%rbp), -n(%rbp)
                        // ---
                        // movq -m(%rbp), %r10
                        // addl %r10, -n(%rbp)
                        replace_instruction!(def.body; [index] =>
                            WithDebugInfo::new(Instruction::Mov {
                                src: Operand::Stack(val),
                                dest: Operand::Reg(Register::R10),
                            }, DebugInfo{ line: debug_info.line, description: format!("({}) (stack binary {} {} {}) {} -> r10 (temp)", debug_info.description, src, op, val, val) }),
                            WithDebugInfo::new(Instruction::Binary(
                                op,
                                Operand::Reg(Register::R10),
                                Operand::Stack(src),
                            ), DebugInfo { line: debug_info.line, description: format!("({}) (stack binary {} {} {}) r10 -> {}", debug_info.description, src, op, val, src) })
                        );
                    }
                    WithDebugInfo {
                        value: Instruction::Binary(BinaryOp::Mul, rhs, lhs @ Operand::Stack(n)),
                        debug_info,
                    } => {
                        // imulq $3, -4(%rbp)
                        // ---
                        // movq -4(%rbp), %r11
                        // imulq $3, %r11
                        // movq %r11, -4(%rbp)
                        replace_instruction!(def.body; [index] =>
                            WithDebugInfo::new(Instruction::Mov {
                                src: lhs.clone(),
                                dest: Operand::Reg(Register::R11),
                            }, DebugInfo{ line: debug_info.line, description: format!("({}) (mul on stack {}) {} -> r11", debug_info.description, n, n) }),
                            WithDebugInfo::new(Instruction::Binary(BinaryOp::Mul, rhs, Operand::Reg(Register::R11)),  debug_info.clone()),
                            WithDebugInfo::new(Instruction::Mov {
                                src: Operand::Reg(Register::R11),
                                dest: lhs,
                            }, DebugInfo{ line: debug_info.line, description: format!("({}) (mul on stack {}) r11 -> {}", debug_info.description, n, n) })
                        );
                    }
                    WithDebugInfo {
                        value: Instruction::Cmp(op1 @ Operand::Stack(n1), op2 @ Operand::Stack(n2)),
                        debug_info,
                    } => {
                        replace_instruction!(def.body; [index] =>
                            WithDebugInfo::new(Instruction::Mov {
                                src: op1,
                                dest: Operand::Reg(Register::R11),
                            }, DebugInfo{ line: debug_info.line, description: format!("({}) (stack cmp {} {}) {} -> r11", debug_info.description, n1, n2, n1) }),
                            WithDebugInfo::new(Instruction::Cmp(Operand::Reg(Register::R11), op2), DebugInfo{ line: debug_info.line, description: format!("({}) (stack cmp {} {})", debug_info.description, n1, n2) })
                        );
                    }
                    WithDebugInfo {
                        value: Instruction::Cmp(op1, op2 @ Operand::Imm(n)),
                        debug_info,
                    } => {
                        replace_instruction!(def.body; [index] =>
                            WithDebugInfo::new(Instruction::Mov {
                                src: op2,
                                dest: Operand::Reg(Register::R11),
                            }, DebugInfo{ line: debug_info.line, description: format!("({}) (cmp with imm) ${} -> r11", debug_info.description, n) }),
                            WithDebugInfo::new(Instruction::Cmp(op1, Operand::Reg(Register::R11)), DebugInfo{ line: debug_info.line, description: format!("({}) (cmp with imm)", debug_info.description) })
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
    body: Vec<WithDebugInfo<Instruction>>,
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
                .map(|s| to_asm_string(s))
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
    // fn to_asm_string(&self) -> String {
    //     match self {
    //         Instruction::AllocateStack(n) => format!("subq\t${}, %rsp", n),
    //         Instruction::Mov { src, dest } => {
    //             format!("movq\t{}, {}", src.to_asm_string(), dest.to_asm_string())
    //         }
    //         Instruction::Unary(op, operand) => {
    //             format!("{}\t{}", unary_op_to_asm(op), operand.to_asm_string())
    //         }
    //         Instruction::Binary(op, val, src) => {
    //             format!(
    //                 "{:<4}\t{}, {}",
    //                 binary_op_to_asm(op),
    //                 val.to_asm_string(),
    //                 src.to_asm_string()
    //             )
    //         }
    //         Instruction::Cmp(lhs, rhs) => {
    //             format!("cmpq\t{}, {}", lhs.to_asm_string(), rhs.to_asm_string())
    //         }
    //         Instruction::Label(label) => format!("{}:", label),
    //         Instruction::Jmp(target) => format!("jmp\t\t{}", target),
    //         Instruction::JmpCC(op, target) => format!("j{}\t{}", op, target),
    //         Instruction::SetCC(op, dest) => format!("set{}\t{}", op, dest.to_asm_string()),
    //         Instruction::Cqo => "cqo".to_string(),
    //         Instruction::Idiv(operand) => format!("idivq\t{}", operand.to_asm_string()),
    //         Instruction::Ret => indent(&["movq\t%rbp, %rsp", "popq\t%rbp", "ret"]),
    //     }
    // }

    fn generate(
        inst: &WithDebugInfo<tacky::Instruction>,
        body: &mut Vec<WithDebugInfo<Instruction>>,
    ) {
        match inst {
            WithDebugInfo {
                value: tacky::Instruction::Return(val),
                debug_info,
            } => {
                let value = Operand::from(val);
                body.push(WithDebugInfo::new(
                    Instruction::Mov {
                        src: value,
                        dest: Operand::Reg(Register::RAX),
                    },
                    DebugInfo {
                        line: debug_info.line,
                        description: format!(
                            "({}) ret {}",
                            debug_info.description,
                            val.to_string()
                        ),
                    },
                ));
                body.push(WithDebugInfo::new(Instruction::Ret, debug_info.clone()));
            }
            WithDebugInfo {
                value: tacky::Instruction::Copy { src, dst },
                debug_info,
            } => {
                body.push(WithDebugInfo::new(
                    Instruction::Mov {
                        src: Operand::from(src),
                        dest: Operand::from(dst),
                    },
                    DebugInfo {
                        line: debug_info.line,
                        description: format!(
                            "({}) {} -> {}",
                            debug_info.description,
                            src.to_string(),
                            dst.to_string()
                        ),
                    },
                ));
            }
            WithDebugInfo {
                value: tacky::Instruction::Unary(op, src_, dest_),
                debug_info,
            } => {
                let src = Operand::from(src_);
                let dest = Operand::from(dest_);
                match op {
                    UnaryOp::Not => {
                        body.extend_from_slice(&[
                            WithDebugInfo::new(
                                Instruction::Cmp(Operand::Imm(0), src),
                                debug_info.map_description_ref(|d| {
                                    format!("({}) unary ! {}", d, src_.to_string())
                                }),
                            ),
                            WithDebugInfo::new(
                                Instruction::Mov {
                                    src: Operand::Imm(0),
                                    dest: dest.clone(),
                                },
                                debug_info.map_description_ref(|d| {
                                    format!("({}) unary ! {}", d, src_.to_string())
                                }),
                            ),
                            WithDebugInfo::new(
                                Instruction::SetCC(CondCode::E, dest),
                                debug_info.map_description_ref(|d| {
                                    format!("({}) unary ! {}", d, src_.to_string())
                                }),
                            ),
                        ]);
                    }
                    _ => {
                        body.extend_from_slice(&[
                            WithDebugInfo::new(
                                Instruction::Mov {
                                    src,
                                    dest: dest.clone(),
                                },
                                debug_info.map_description_ref(|d| {
                                    format!("({}) unary {} {}", d, op, src_.to_string())
                                }),
                            ),
                            WithDebugInfo::new(Instruction::Unary(*op, dest), debug_info.clone()),
                        ]);
                    }
                }
            }
            WithDebugInfo {
                value: tacky::Instruction::Binary(op, vlhs_, vrhs_, dest),
                debug_info,
            } => {
                let vlhs = Operand::from(vlhs_);
                let mut vrhs = Operand::from(vrhs_);
                let dest = Operand::from(dest);
                match op {
                    BinaryOp::Mod | BinaryOp::Div => {
                        let (result_reg, inst) = if let BinaryOp::Div = op {
                            (Operand::Reg(Register::RAX), "div")
                        } else {
                            (Operand::Reg(Register::RDX), "mod")
                        };
                        body.extend_from_slice(&[
                            WithDebugInfo::new(
                                Instruction::Mov {
                                    src: vlhs,
                                    dest: Operand::Reg(Register::RAX),
                                },
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        inst,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ),
                            WithDebugInfo::new(
                                Instruction::Cqo,
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ),
                        ]);

                        if let Operand::Imm(_) = vrhs {
                            body.push(WithDebugInfo::new(
                                Instruction::Mov {
                                    src: vrhs,
                                    dest: Operand::Reg(Register::R10),
                                },
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ));
                            vrhs = Operand::Reg(Register::R10);
                        }

                        body.extend_from_slice(&[
                            WithDebugInfo::new(
                                Instruction::Idiv(vrhs),
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ),
                            WithDebugInfo::new(
                                Instruction::Mov {
                                    src: result_reg,
                                    dest,
                                },
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ),
                        ]);
                    }
                    BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::LesserEq
                    | BinaryOp::Lesser
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEq => {
                        body.extend_from_slice(&[
                            WithDebugInfo::new(
                                Instruction::Cmp(vrhs, vlhs),
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ),
                            WithDebugInfo::new(
                                Instruction::Mov {
                                    src: Operand::Imm(0),
                                    dest: dest.clone(),
                                },
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ),
                            WithDebugInfo::new(
                                Instruction::SetCC(op.into(), dest),
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ),
                        ]);
                    }
                    _ => {
                        if vlhs != dest {
                            body.push(WithDebugInfo::new(
                                Instruction::Mov {
                                    src: vlhs,
                                    dest: dest.clone(),
                                },
                                debug_info.map_description_ref(|d| {
                                    format!(
                                        "({}) {} {} {}",
                                        d,
                                        op,
                                        vlhs_.to_string(),
                                        vrhs_.to_string()
                                    )
                                }),
                            ));
                        }
                        body.push(WithDebugInfo::new(
                            Instruction::Binary(*op, vrhs, dest),
                            debug_info.map_description_ref(|d| {
                                format!(
                                    "({}) {} {} {}",
                                    d,
                                    op,
                                    vlhs_.to_string(),
                                    vrhs_.to_string()
                                )
                            }),
                        ));
                    }
                }
            }
            WithDebugInfo {
                value: tacky::Instruction::Jump(label),
                debug_info,
            } => {
                body.push(WithDebugInfo::new(
                    Instruction::Jmp(label.clone()),
                    debug_info.clone(),
                ));
            }

            WithDebugInfo {
                value: tacky::Instruction::JumpIfZero(val, target),
                debug_info,
            } => {
                let val = Operand::from(val);
                body.extend_from_slice(&[
                    WithDebugInfo::new(Instruction::Cmp(Operand::Imm(0), val), debug_info.clone()),
                    WithDebugInfo::new(
                        Instruction::JmpCC(CondCode::E, target.clone()),
                        debug_info.clone(),
                    ),
                ]);
            }

            WithDebugInfo {
                value: tacky::Instruction::JumpIfNotZero(val, target),
                debug_info,
            } => {
                let val = Operand::from(val);
                body.extend_from_slice(&[
                    WithDebugInfo::new(Instruction::Cmp(Operand::Imm(0), val), debug_info.clone()),
                    WithDebugInfo::new(
                        Instruction::JmpCC(CondCode::NE, target.clone()),
                        debug_info.clone(),
                    ),
                ]);
            }

            WithDebugInfo {
                value: tacky::Instruction::Label(label),
                debug_info,
            } => body.push(WithDebugInfo::new(
                Instruction::Label(label.clone()),
                debug_info.clone(),
            )),
        }
    }
}

fn to_asm_string(inst: &WithDebugInfo<Instruction>) -> String {
    let comment = format!(
        "# line {} :: {}",
        inst.debug_info.line, inst.debug_info.description
    );
    let asm = match &inst.value {
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
    };
    format!("{}\t{}", asm, comment)
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
