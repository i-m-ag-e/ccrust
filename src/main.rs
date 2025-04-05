use ccrust::assemble;
use ccrust::compile;
use ccrust::lexer::LexerError;
use ccrust::parser::ParseError;
#[cfg(feature = "resolve")]
use ccrust::resolver::ResolveError;
use ccrust::type_checker::TypeCheckerError;
use clap::Parser;
use colored::Colorize;
use std::fs;
use std::path::Path;
use std::process::Command;

#[derive(Parser)]
#[command(name = "ccrust")]
#[command(version, about, long_about = None)]
struct Cli {
    /// Path of input C file
    files: Vec<String>,

    /// Specify path of output executable
    #[arg(long, short)]
    output: Option<String>,

    /// Compile to object file
    #[arg(short)]
    compile_to_object: bool,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    // When multiple files are provided, force compilation to object files and ignore the output option
    let (force_object_compilation, adjusted_output) = if cli.files.len() > 1 {
        (true, None)
    } else {
        (cli.compile_to_object, cli.output.clone()) // Use provided options for a single file
    };

    let mut out_files = Vec::new();
    for file in cli.files.iter() {
        let path = Path::new(file);
        let text = fs::read_to_string(path)?;

        let compile_result = compile(&text, true);

        let Ok(asm_string) = compile_result else {
            let err = compile_result.unwrap_err();
            let err_msg = err.to_string();

            let res = err
                .downcast::<LexerError>()
                .map(|le| le.token)
                .or_else(|err| err.downcast::<ParseError>().map(|pe| pe.token));

            #[cfg(feature = "resolve")]
            let res = {
                res.or_else(|err| err.downcast::<ResolveError>().map(|re| re.token))
                    .or_else(|err| err.downcast::<TypeCheckerError>().map(|te| te.token))
            };

            res.map(|token| {
                eprintln!(
                    "{}",
                    format!(
                        "Error {}:{} (at ({:?}):: {}",
                        token.line,
                        token.span.0,
                        &text[token.span.0..token.span.1],
                        err_msg
                    )
                    .red()
                );
            })
            .unwrap();

            std::process::exit(-1);
        };

        #[cfg(not(feature = "asm_gen"))]
        println!("{}\n{}:\n{}\n", "-".repeat(20), file, asm_string);

        let (_status, out_file) = assemble(
            &path,
            &asm_string,
            &adjusted_output,
            force_object_compilation,
            true,
            true,
        )?;
        out_files.push(out_file);
    }

    if cli.files.len() > 1 && !cli.compile_to_object {
        let mut command = Command::new("gcc");

        command
            .args(&out_files)
            .args(["-o", cli.output.as_deref().unwrap_or("a.out")]);
        println!("Compiling with command {:?}", command);
        let status = command.spawn()?.wait()?;
        if status.success() {
            println!("Compiled successfully");
        } else {
            eprintln!("Failed to compile");
        }

        if !cli.compile_to_object {
            for out_file in out_files {
                fs::remove_file(out_file)?;
            }
        }
    }

    Ok(())
}
