pub mod ast;
pub mod pretty_print_ast;

use crate::lexer::token::{self, Span, Token, TokenType};
use ast::*;
use multipeek::{multipeek, MultiPeek};
use thiserror::Error;

// <program>        ::= <function-decl>
// <function-decl>  ::= "int" <identifier> "(" "void" ")" "{" <function-body> "}"
// <function-body>  ::= (<statement> ";")*
// <statement>      ::= "return" <expr>
// <expr>           ::= <term>
// <term>           ::= <factor> ( ( "+" | "-" ) <factor> )?
// <factor>         ::= <unary> ( ( "*" | "/" | "%" ) <unary> )?
// <unary>          ::= <unary-op>? <primary>
// <primary>        ::= <constant> | "(" <expr> ")"
// <unary-op>       ::= "-" | "~"
// <constant>       ::= <integer>

macro_rules! parse_binary_expr {
    ( $self: ident, $ops: pat, $nextp: ident ) => {{
        let mut lhs = $self.$nextp()?;
        while let Some($ops) = $self.peek_token_type() {
            let op = binary_tt_to_op(&$self.advance().unwrap().tok_type);
            let rhs = Box::new($self.$nextp()?);
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs,
            }
        }
        Ok(lhs)
    }};
}

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
    // errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, tokens: Vec<Token>) -> Self {
        println!("input: {:?}, len: {}", input, input.len());
        Self {
            input,
            tokens: multipeek(tokens.into_iter()),
            // errors: Vec::new(),
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_token_type(&mut self) -> Option<&TokenType> {
        match self.peek() {
            Some(Token { tok_type, .. }) => Some(tok_type),
            None => None,
        }
    }

    // fn peek_next(&mut self) -> Option<&Token> {
    //     self.tokens.peek_nth(1)
    // }

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

    // fn match_one_of(&mut self, tok_types: &[TokenType]) -> bool {
    //     match self.peek() {
    //         Some(Token {
    //             tok_type: ttype, ..
    //         }) if tok_types.contains(ttype) => {
    //             self.advance();
    //             true
    //         }
    //         _ => false,
    //     }
    // }

    fn consume(&mut self, tok_type: TokenType, exp: &'static str) -> ParseResult<Token> {
        match self.peek() {
            Some(Token {
                tok_type: TokenType::EOF,
                ..
            })
            | None => Err(self.error_at_current(ParseErrorType::Expected {
                exp,
                got: String::from("<eof>"),
            })),
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
                    let span = *span;
                    let lexeme = self.input[span.0..span.1].to_string();
                    Err(self.error_at_current(ParseErrorType::Expected { exp, got: lexeme }))
                }
            }
        }
    }

    fn consume_if<F>(&mut self, pred: F, exp: &'static str) -> ParseResult<Token>
    where
        F: FnOnce(&TokenType) -> bool,
    {
        match self.peek() {
            Some(Token {
                tok_type: TokenType::EOF,
                ..
            })
            | None => Err(self.error_at_current(ParseErrorType::Expected {
                exp,
                got: String::from("<eof>"),
            })),
            Some(Token {
                tok_type: tt, span, ..
            }) => {
                if pred(tt) {
                    Ok(self.advance().unwrap())
                } else {
                    let span = *span;
                    let lexeme = self.input[span.0..span.1].to_string();
                    Err(self.error_at_current(ParseErrorType::Expected { exp, got: lexeme }))
                }
            }
        }
    }

    fn error_at_current(&mut self, error: ParseErrorType) -> ParseError {
        if let Some(token) = self.advance() {
            ParseError { token, error }
        } else {
            ParseError {
                token: Token {
                    span: Span(self.input.len() - 1, self.input.len() - 1),
                    line: 0,
                    tok_type: TokenType::EOF,
                },
                error,
            }
        }
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
        self.consume(TokenType::Identifier(String::from("int")), "int")?;

        let name = self.consume(TokenType::Identifier(String::from("main")), "main")?;
        let name = match name.tok_type {
            TokenType::Identifier(ident) => ident,
            _ => unreachable!(),
        };

        self.consume(TokenType::LParen, "(")?;
        self.consume(TokenType::Identifier(String::from("void")), "void")?;
        self.consume(TokenType::RParen, ")")?;

        self.consume(TokenType::LBrace, "{")?;
        self.consume(TokenType::KReturn, "return")?;

        let ret_value = self.expression()?;

        self.consume(TokenType::Semicolon, ";")?;
        self.consume(TokenType::RBrace, "}")?;

        Ok(FunctionDef {
            name,
            body: vec![Stmt::Return { ret_value }],
        })
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.or()
    }

    fn or(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(self, TokenType::Or, and)
    }

    fn and(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(self, TokenType::And, bitwise_or)
    }

    fn bitwise_or(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(self, TokenType::BitwiseOR, bitwise_xor)
    }

    fn bitwise_xor(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(self, TokenType::BitwiseXOR, bitwise_and)
    }

    fn bitwise_and(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(self, TokenType::BitwiseAND, equality)
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(self, TokenType::EqEqual | TokenType::BangEq, comparison)
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(
            self,
            TokenType::Greater | TokenType::GreaterEq | TokenType::Lesser | TokenType::LesserEq,
            shifts
        )
    }

    fn shifts(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(self, TokenType::LShift | TokenType::RShift, term)
    }

    fn term(&mut self) -> ParseResult<Expr> {
        // let lhs = self.factor()?;
        // if let Some(TokenType::Plus | TokenType::Minus) = self.peek_token_type() {
        //     let op = binary_tt_to_op(&self.advance().unwrap().tok_type);
        //     let rhs = Box::new(self.term()?);
        //     Ok(Expr::Binary {
        //         op,
        //         lhs: Box::new(lhs),
        //         rhs,
        //     })
        // } else {
        //     Ok(lhs)
        // }
        parse_binary_expr!(self, TokenType::Plus | TokenType::Minus, factor)
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        // let lhs = self.unary()?;
        // if let Some(TokenType::Star | TokenType::Slash | TokenType::Perc) = self.peek_token_type() {
        //     let op = binary_tt_to_op(&self.advance().unwrap().tok_type);
        //     let rhs = Box::new(self.factor()?);
        //     Ok(Expr::Binary {
        //         op,
        //         lhs: Box::new(lhs),
        //         rhs,
        //     })
        // } else {
        //     Ok(lhs)
        // }
        parse_binary_expr!(
            self,
            TokenType::Star | TokenType::Slash | TokenType::Perc,
            unary
        )
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        let tt = self.peek_token_type();
        match tt {
            Some(TokenType::Minus | TokenType::Tilde) => {
                let op = unary_tt_to_op(tt.unwrap());
                self.advance();
                let expr = self.unary()?;
                Ok(Expr::Unary {
                    op,
                    expr: Box::new(expr),
                })
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        let tt = self.peek_token_type();
        match tt {
            Some(TokenType::Literal(_)) => {
                let token = self.advance().unwrap();
                let value = match token.tok_type {
                    TokenType::Literal(lit) => match lit {
                        token::Literal::Integer(value) => Literal::Integer(value),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                Ok(Expr::Literal(value))
            }
            Some(TokenType::LParen) => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RParen, ")")?;
                Ok(expr)
            }
            _ => {
                let span = self.peek().unwrap().span;
                Err(self.error_at_current(ParseErrorType::Expected {
                    exp: "<expression>",
                    got: self.input[span.0..span.1].to_string(),
                }))
            }
        }
    }
}

fn unary_tt_to_op(tt: &TokenType) -> UnaryOp {
    match tt {
        TokenType::Minus => UnaryOp::Minus,
        TokenType::Tilde => UnaryOp::BitNOT,
        _ => unreachable!(),
    }
}

fn binary_tt_to_op(tt: &TokenType) -> BinaryOp {
    match tt {
        TokenType::Minus => BinaryOp::Minus,
        TokenType::Plus => BinaryOp::Plus,
        TokenType::Star => BinaryOp::Mul,
        TokenType::Slash => BinaryOp::Div,
        TokenType::Perc => BinaryOp::Mod,
        TokenType::BitwiseAND => BinaryOp::BitwiseAND,
        TokenType::BitwiseOR => BinaryOp::BitwiseOR,
        TokenType::BitwiseXOR => BinaryOp::BitwiseXOR,
        TokenType::LShift => BinaryOp::LShift,
        TokenType::RShift => BinaryOp::RShift,
        TokenType::EqEqual => BinaryOp::Eq,
        TokenType::BangEq => BinaryOp::NotEq,
        TokenType::And => BinaryOp::And,
        TokenType::Or => BinaryOp::Or,
        TokenType::Greater => BinaryOp::Greater,
        TokenType::GreaterEq => BinaryOp::GreaterEq,
        TokenType::Lesser => BinaryOp::Lesser,
        TokenType::LesserEq => BinaryOp::LesserEq,
        _ => unreachable!(),
    }
}
