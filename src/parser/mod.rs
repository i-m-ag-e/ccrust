pub mod ast;

use crate::lexer::token::{Token, TokenType};
use ast::*;
use multipeek::{multipeek, MultiPeek};
use thiserror::Error;

// <program>        ::= <function-decl>
// <function-decl>  ::= "int" <identifier> "(" "void" ")" "{" <function-body> "}"
// <function-body>  ::= (<statement> ";")*
// <statement>      ::= "return" <int>

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
    pub error: ParseErrorType,
}

#[derive(Error, Debug)]
pub enum ParseErrorType {
    #[error("expected `{exp}`, got {got:?}")]
    Expected { exp: &'static str, got: String },
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    input: &'a str,
    tokens: MultiPeek<<Vec<Token> as IntoIterator>::IntoIter>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            input,
            tokens: multipeek(tokens.into_iter()),
            errors: Vec::new(),
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().is_some()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_next(&mut self) -> Option<&Token> {
        self.tokens.peek_nth(1)
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn match_(&mut self, tok_type: TokenType) -> bool {
        match self.peek() {
            Some(Token {
                tok_type: ttype, ..
            }) if *ttype == tok_type => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn match_one_of(&mut self, tok_types: &[TokenType]) -> bool {
        match self.peek() {
            Some(Token {
                tok_type: ttype, ..
            }) if tok_types.contains(ttype) => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn consume(&mut self, tok_type: TokenType, exp: &'static str) -> ParseResult<Token> {
        assert!(!self.is_at_end());

        match self.peek() {
            Some(Token {
                tok_type: tt, span, ..
            }) => {
                if tok_type == *tt {
                    Ok(self.advance().unwrap())
                } else if tok_type == TokenType::EOF {
                    Err(self.error_at_current(ParseErrorType::Expected {
                        exp,
                        got: String::from("<eof>"),
                    }))
                } else {
                    Err(self.error_at_current(ParseErrorType::Expected {
                        exp,
                        got: self.input[span.0..span.1].to_string(),
                    }))
                }
            }
            None => unreachable!(),
        }
    }

    fn error_at_current(&mut self, error: ParseErrorType) -> ParseError {
        assert!(!self.is_at_end());
        let token = self.advance().unwrap();
        ParseError { token, error }
    }
}

impl<'a> Parser<'a> {
    pub fn program(&mut self) -> ParseResult<Program> {
        let mut funs = Vec::new();
        while !self.match_(TokenType::EOF) {
            funs.push(self.function_def()?);
        }
        Ok(Program(funs))
    }

    fn function_def(&mut self) -> ParseResult<FunctionDef> {
        Err(self.error_at_current(ParseErrorType::Expected {
            exp: "",
            got: "".to_owned(),
        }))
    }
}
