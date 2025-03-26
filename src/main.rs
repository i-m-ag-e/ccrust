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

        let _status = assemble(
            &path,
            &asm_string,
            &cli.output,
            cli.compile_to_object,
            true,
            true,
        )?;
    }

    Ok(())
}
