use rustcc::lexer::token::{Token, TokenType};
use rustcc::lexer::Lexer;
use std::env;
use std::fs;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    println!("args: {:?}", args);

    let text = if let Some(path) = args.get(1) {
        fs::read_to_string(path)?
    } else {
        panic!("Invalid no. of arguments");
    };

    let mut lexer = Lexer::new(&text);
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
    Ok(())
}
