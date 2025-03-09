use std::{
    os::unix::process::ExitStatusExt,
    path::Path,
    process::{Command, ExitStatus},
};

use parser::{
    ast::{ASTRefVisitor, ASTVisitor},
    pretty_print_ast::PrettyPrint,
    Parser,
};
use std::fs;

#[cfg(feature = "asm_gen")]
use tacky::GenerateTacky;

pub mod debug_info;
pub mod lexer;
pub mod parser;

#[cfg(feature = "resolve")]
pub mod resolver;
#[cfg(feature = "resolve")]
use resolver::Resolver;
#[cfg(feature = "asm_gen")]
pub mod asm;
#[cfg(feature = "asm_gen")]
pub mod tacky;

pub fn compile(input: &str, debug: bool) -> anyhow::Result<String> {
    let lexer = lexer::Lexer::new(input);
    let tokens = lexer.collect::<Result<Vec<_>, _>>()?;

    let mut parser = Parser::new(input, tokens);

    let program = parser.program()?;

    #[cfg(feature = "resolve")]
    let program = Resolver::new().visit_program(program)?;

    #[cfg(feature = "asm_gen")]
    {
        let tacky_prog = GenerateTacky::new().visit_program(&program);
        let asm = asm::Program::from(&tacky_prog);
        let asm_string = asm.to_asm_string();

        if debug {
            println!("Tacky: {tacky_prog:#?}");
            println!("ASM: {asm:#?}");
            println!("ASM:\n{}", asm_string);
        }

        return Ok(asm_string);
    }
    Ok(PrettyPrint::new().visit_program(&program))
}

#[allow(unused)]
pub fn assemble(
    file: &Path,
    asm: &str,
    output: &Option<String>,
    keep_asm: bool,
    debug: bool,
) -> std::io::Result<ExitStatus> {
    #[cfg(feature = "asm_gen")]
    {
        let asm_file_path = file.with_extension("s");
        let out_file = if let Some(out) = output {
            out.clone()
        } else {
            asm_file_path
                .with_extension("")
                .to_str()
                .unwrap()
                .to_string()
        };
        std::fs::write(&asm_file_path, asm)?;

        let mut command = Command::new("gcc");

        command
            .arg(
                asm_file_path
                    .to_str()
                    .expect("path is not valid Unicode")
                    .to_string(),
            )
            .arg("-g")
            .args(["-o", &out_file]);

        if debug {
            println!("Assembling with command {:?}", command)
        };
        let exit_status = command.spawn()?.wait()?;

        if exit_status.success() {
            println!("Compiled successfully");
            if !keep_asm {
                fs::remove_file(asm_file_path)?;
            }
        } else {
            eprintln!("Failed to compile");
        }

        Ok(exit_status)
    }

    #[cfg(not(feature = "asm_gen"))]
    {
        Ok(ExitStatus::from_raw(0))
    }
}
