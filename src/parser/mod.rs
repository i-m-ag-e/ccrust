pub mod ast;
mod expr;
pub mod pretty_print_ast;
mod stmt;

use crate::lexer::token::{self, Span, Token, TokenType};
use ast::*;
use multipeek::{multipeek, MultiPeek};
use thiserror::Error;

// <program>        ::= <function-decl>
// <function-decl>  ::= "int" <identifier> "(" "void" ")" "{" <block-tem>* "}"
// <block-item>     ::= <statment> | <declaration>
// <declaration>    ::= "int" <identifier> ( "=" <expr> )? ";"
// <statement>      ::= "return" <expr> ";" | <expr> ";" | ";"
// <expr>           ::= <assign>
// <assign>         ::= <term> "=" <term>
// <term>           ::= <factor> ( ( "+" | "-" ) <factor> )?
// <factor>         ::= <unary> ( ( "*" | "/" | "%" ) <unary> )?
// <unary>          ::= <unary-op>? <primary>
// <primary>        ::= <constant> | "(" <expr> ")" | <identifier>
// <unary-op>       ::= "-" | "~"
// <constant>       ::= <integer>

macro_rules! parse_binary_expr {
    ( $self: ident, $ops: pat, $nextp: ident ) => {{
        let mut lhs = $self.$nextp()?;
        while let Some($ops) = $self.peek_token_type() {
            let op_token = $self.advance().unwrap();
            let op = binary_tt_to_op(&op_token.tok_type);
            let rhs = Box::new($self.$nextp()?);
            lhs = Expr::Binary(Binary {
                op: WithToken(op, op_token),
                lhs: Box::new(lhs),
                rhs,
            })
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

    #[error("redifinition of label `{0}` (previous definition at {1:?}")]
    RedefinedLabel(String, Token),
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    input: &'a str,
    tokens: MultiPeek<<Vec<Token> as IntoIterator>::IntoIter>,
    current_function_labels: Vec<WithToken<String>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, tokens: Vec<Token>) -> Self {
        println!("input: {:?}, len: {}", input, input.len());
        Self {
            input,
            tokens: multipeek(tokens.into_iter()),
            current_function_labels: Vec::new(),
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
                }
                // else if tok_type == TokenType::EOF {
                //     Err(self.error_at_current(ParseErrorType::Expected {
                //         exp,
                //         got: String::from("<eof>"),
                //     }))
                // }
                else {
                    let span = *span;
                    let lexeme = self.input[span.0..span.1].to_string();
                    Err(self.error_at_current(ParseErrorType::Expected { exp, got: lexeme }))
                }
            }
        }
    }

    fn consume_identifier(&mut self) -> ParseResult<WithToken<String>> {
        let exp = "<identifier>";
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
                tok_type: TokenType::Identifier(ident),
                ..
            }) => Ok(WithToken(ident.clone(), self.advance().unwrap())),
            Some(Token { span, .. }) => {
                let span = *span;
                let lexeme = self.input[span.0..span.1].to_string();
                Err(self.error_at_current(ParseErrorType::Expected { exp, got: lexeme }))
            }
        }
    }

    #[allow(unused)]
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

        let name = self.consume_identifier()?;

        self.consume(TokenType::LParen, "(")?;
        self.consume(TokenType::Identifier(String::from("void")), "void")?;
        self.consume(TokenType::RParen, ")")?;

        self.consume(TokenType::LBrace, "{")?;

        let mut body = Vec::new();
        loop {
            match self.peek_token_type() {
                Some(TokenType::RBrace) => break,
                _ => body.push(self.block_item()?),
            };
        }

        self.consume(TokenType::RBrace, "}")?;
        self.current_function_labels.clear();

        Ok(FunctionDef { name, body })
    }

    fn block_item(&mut self) -> ParseResult<BlockItem> {
        match self.peek_token_type() {
            Some(TokenType::Identifier(name)) if name == "int" => self.declaration(),
            _ => Ok(BlockItem::Stmt(self.statement()?)),
        }
    }
}

impl<'a> Parser<'a> {
    // Methods for parsing expressions

    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let mut lhs = self.conditional_expr()?;
        if let Some(
            TokenType::Equal
            | TokenType::BitwiseANDEq
            | TokenType::BitwiseOREq
            | TokenType::BitwiseXOREq
            | TokenType::LShiftEq
            | TokenType::MinusEq
            | TokenType::PercEq
            | TokenType::PlusEq
            | TokenType::RShiftEq
            | TokenType::SlashEq
            | TokenType::StarEq,
        ) = self.peek_token_type()
        {
            let eq_sign = self.advance().unwrap();
            let mut rhs = self.assignment()?;

            if eq_sign.tok_type != TokenType::Equal {
                let op = match eq_sign.tok_type {
                    TokenType::BitwiseANDEq => TokenType::BitwiseAND,
                    TokenType::BitwiseOREq => TokenType::BitwiseOR,
                    TokenType::BitwiseXOREq => TokenType::BitwiseXOR,
                    TokenType::LShiftEq => TokenType::LShift,
                    TokenType::MinusEq => TokenType::Minus,
                    TokenType::PercEq => TokenType::Perc,
                    TokenType::PlusEq => TokenType::Plus,
                    TokenType::RShiftEq => TokenType::RShift,
                    TokenType::SlashEq => TokenType::Slash,
                    TokenType::StarEq => TokenType::Star,
                    _ => unreachable!(),
                };
                rhs = Expr::Binary(Binary {
                    op: WithToken(binary_tt_to_op(&op), eq_sign.clone()),
                    lhs: Box::new(lhs.clone()),
                    rhs: Box::new(rhs),
                });
            }

            lhs = Expr::Assign(Assign {
                eq_sign,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }
        Ok(lhs)
    }

    fn conditional_expr(&mut self) -> ParseResult<Expr> {
        let cond = self.or()?;
        if let Some(TokenType::QMark) = self.peek_token_type() {
            self.advance();
            let then_expr = Box::new(self.expression()?);
            self.consume(TokenType::Colon, ";")?;
            let else_expr = Box::new(self.conditional_expr()?);

            Ok(Expr::Conditional(Conditional {
                cond: Box::new(cond),
                then_expr,
                else_expr,
            }))
        } else {
            Ok(cond)
        }
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
        parse_binary_expr!(self, TokenType::Plus | TokenType::Minus, factor)
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        parse_binary_expr!(
            self,
            TokenType::Star | TokenType::Slash | TokenType::Perc,
            unary
        )
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        let tt = self.peek_token_type();
        match tt {
            Some(
                TokenType::Minus | TokenType::Tilde | TokenType::Increment | TokenType::Decrement,
            ) => {
                let op_token = self.advance().unwrap();
                let op = unary_tt_to_op(&op_token.tok_type);
                let expr = self.unary()?;
                Ok(Expr::Unary(Unary {
                    op: WithToken(op, op_token),
                    expr: Box::new(expr),
                    postfix: false,
                }))
            }
            _ => self.postfix(),
        }
    }

    fn postfix(&mut self) -> ParseResult<Expr> {
        let mut lhs = self.primary()?;
        while let Some(TokenType::Increment | TokenType::Decrement) = self.peek_token_type() {
            let op_token = self.advance().unwrap();
            let op = unary_tt_to_op(&op_token.tok_type);
            lhs = Expr::Unary(Unary {
                op: WithToken(op, op_token),
                expr: Box::new(lhs),
                postfix: true,
            });
        }
        Ok(lhs)
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        let tt = self.peek_token_type();
        match tt {
            Some(TokenType::Literal(_)) => {
                let token = self.advance().unwrap();
                let value = match &token.tok_type {
                    TokenType::Literal(lit) => match lit {
                        token::Literal::Integer(value) => Literal::Integer(*value),
                        _ => todo!(),
                    },
                    _ => todo!(),
                };
                Ok(Expr::Literal(WithToken(value, token)))
            }
            Some(TokenType::LParen) => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RParen, ")")?;
                Ok(expr)
            }
            Some(TokenType::Identifier(name)) => {
                let name = name.clone();
                let token = self.advance().unwrap();
                Ok(Expr::Var(WithToken(name, token)))
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

impl<'a> Parser<'a> {
    fn declaration(&mut self) -> ParseResult<BlockItem> {
        self.advance();

        let name = self.consume_identifier()?;

        let init = if let Some(TokenType::Equal) = self.peek_token_type() {
            self.advance();
            let rhs = self.expression()?;
            Some(rhs)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, ";")?;

        Ok(BlockItem::VarDecl(VarDecl { name, init }))
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        match self.peek_token_type() {
            Some(TokenType::KGoto) => self.goto_stmt(),
            Some(TokenType::KIf) => self.if_stmt(),
            Some(TokenType::KReturn) => self.return_statement(),
            Some(TokenType::Semicolon) => {
                self.advance();
                Ok(Stmt::Null)
            }
            Some(TokenType::Identifier(_)) => {
                if let Some(Token {
                    tok_type: TokenType::Colon,
                    ..
                }) = self.peek_next()
                {
                    self.label()
                } else {
                    self.expression_statement()
                }
            }
            _ => self.expression_statement(),
        }
    }

    fn goto_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let label = self.consume_identifier()?;
        self.consume(TokenType::Semicolon, ";")?;
        Ok(Stmt::Goto(label))
    }

    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();

        self.consume(TokenType::LParen, "(")?;
        let cond = self.expression()?;
        self.consume(TokenType::RParen, ")")?;

        let then = Box::new(self.statement()?);
        let else_clause = if let Some(TokenType::KElse) = self.peek_token_type() {
            self.advance();
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(IfStmt {
            cond,
            then,
            else_clause,
        }))
    }

    fn return_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let ret_value = self.expression()?;
        self.consume(TokenType::Semicolon, ";")?;
        Ok(Stmt::Return { ret_value })
    }

    fn label(&mut self) -> ParseResult<Stmt> {
        let name = self.consume_identifier()?;

        if let Some(prev_token) = self.current_function_labels.iter().find(|l| ***l == *name) {
            return Err(ParseError {
                token: name.1,
                error: ParseErrorType::RedefinedLabel(name.0, prev_token.1.clone()),
            });
        } else {
            self.current_function_labels.push(name.clone());
        }

        self.advance(); // consume the colon
        let next_stmt = Box::new(self.statement()?);
        Ok(Stmt::Label(Label { name, next_stmt }))
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, ";")?;
        Ok(Stmt::Expression(expr))
    }
}

fn unary_tt_to_op(tt: &TokenType) -> UnaryOp {
    match tt {
        TokenType::Minus => UnaryOp::Minus,
        TokenType::Tilde => UnaryOp::BitNOT,
        TokenType::Increment => UnaryOp::Increment,
        TokenType::Decrement => UnaryOp::Decrement,
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
