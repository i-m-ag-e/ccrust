use ccrust::assemble;
use ccrust::compile;
use ccrust::lexer::LexerError;
use ccrust::parser::ParseError;
#[cfg(feature = "resolve")]
use ccrust::resolver::ResolveError;
use clap::Parser;
use colored::Colorize;
use std::fs;
use std::path::Path;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Path of input C file
    file: String,

    /// Specify path of output executable
    #[arg(long, short)]
    output: Option<String>,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    let path = Path::new(&cli.file);
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
        let res = res.or_else(|err| err.downcast::<ResolveError>().map(|re| re.token));

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
    println!("{}", asm_string);

    let _status = assemble(&path, &asm_string, &cli.output, true, true)?;

    Ok(())
}
