use colored::Colorize;
use rustcc::asm::Program;
use rustcc::lexer::token::{Token, TokenType};
use rustcc::lexer::Lexer;
use rustcc::parser::ast::{ASTRefVisitor, ASTVisitor};
use rustcc::parser::pretty_print_ast::PrettyPrint;
use rustcc::parser::Parser;
use rustcc::resolver::Resolver;
use rustcc::tacky::GenerateTacky;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    println!("args: {:?}", args);

    let (text, path) = if let Some(path) = args.get(1) {
        (fs::read_to_string(path)?, Path::new(path))
    } else {
        panic!("Invalid no. of arguments");
    };

    let mut lexer = Lexer::new(&text);
    let mut tokens = Vec::new();
    let mut token = lexer.next_token();
    while let Ok(ref tok) = token {
        println!(
            "{:?} ({:?})",
            tok,
            if (tok.span.0 as usize) < text.len() {
                &text[tok.span.0 as usize..tok.span.1 as usize]
            } else {
                ""
            }
        );
        tokens.push(tok.clone());
        if let Token {
            tok_type: TokenType::EOF,
            ..
        } = tok
        {
            break;
        }
        token = lexer.next_token();
    }

    if let Err(ref err) = token {
        eprintln!(
            "Error {}:{} (at ({:?}):: {}",
            err.token.line,
            err.token.span.0,
            &text[err.token.span.0..err.token.span.1],
            err.error
        );
    }

    let mut parser = Parser::new(&text, tokens);
    let program = parser.program();

    match program {
        Ok(mut program) => {
            println!("{:#?}\n\n", program);
            println!("AST:\n{}\n", PrettyPrint::new().visit_program(&mut program));

            let mut resolver = Resolver::new();
            let res = resolver.visit_program(program);

            match res {
                Err(err) => {
                    println!("{:?}", err);
                    eprintln!(
                        "{}",
                        format!(
                            "Error {}:{} (at ({:?}):: {}",
                            err.token.line,
                            err.token.span.0,
                            &text[err.token.span.0..err.token.span.1],
                            err.error_type
                        )
                        .red()
                    );
                    std::process::exit(-1);
                }
                Ok(prog) => {
                    program = prog;
                    println!("AST:\n{}\n", PrettyPrint::new().visit_program(&program));
                }
            }

            let tacky = GenerateTacky::new().visit_program(&program);
            let asm = Program::from(&tacky);
            let asm_string = asm.to_asm_string();

            println!("Tacky: {:#?}\n", tacky);
            println!("ASM: {:#?}\n", asm);
            println!("ASM:\n{}", asm_string);
            let asm_file_path = path.with_extension("s");

            fs::write(&asm_file_path, &asm_string)?;

            let mut command = Command::new("gcc");
            command
                .arg(
                    asm_file_path
                        .to_str()
                        .expect("path is not valid Unicode")
                        .to_string(),
                )
                .args(["-o", asm_file_path.with_extension("").to_str().unwrap()]);
            println!("Assemblig with command: {:?}", command);
            let assemble_success = command
                .spawn()
                .expect("could not begin compilation using gcc")
                .wait()
                .expect("could not compile using gcc");

            println!("exit status: {}", assemble_success);

            if assemble_success.success() {
                println!("Compiled successfully");
                // fs::remove_file(asm_file_path)?;
            } else {
                eprintln!("Compile failure");
            }
        }
        Err(err) => {
            println!("{:?}", err);
            eprintln!(
                "{}",
                format!(
                    "Error {}:{} (at ({:?}):: {}",
                    err.token.line,
                    err.token.span.0,
                    &text[err.token.span.0..err.token.span.1],
                    err.error
                )
                .red()
            );
        }
    }

    Ok(())
}
