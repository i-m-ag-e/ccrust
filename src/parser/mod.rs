pub mod ast;
mod expr;
pub mod pretty_print_ast;
mod stmt;

use ordermap::OrderMap;

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

#[derive(Error, Debug)]
#[error("ParseError at {token:?}: {error}")]
pub struct ParseError {
    pub token: Token,
    #[source]
    pub error: ParseErrorType,
}

#[derive(Error, Debug)]
pub enum ParseErrorType {
    #[error("expected `{exp}`, got {got:?}")]
    Expected { exp: &'static str, got: String },

    #[error("redifinition of label `{0}` (previous definition at {1:?}")]
    RedefinedLabel(String, Token),

    #[error("cannot declare {decl_type} in {context}")]
    InvalidDeclaration {
        decl_type: &'static str,
        context: &'static str,
    },

    #[error("break or continue statement used outside loop")]
    BreakContinueOutsideLoop,

    #[error("case label used outside switch statement")]
    CaseOutsideSwitch,
    #[error("case value is not an integer constant")]
    CaseValueNotConstant,
    #[error("default label used outside switch statement")]
    DefaultOutsideSwitch,
    #[error("same case value used more than once (previous label at {prev:?})")]
    DuplicateCaseValue { prev: Token },
    #[error("duplicate default labels (previous one at {prev:?})")]
    DuplicateDefaultLabel { prev: Token },
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    input: &'a str,
    tokens: MultiPeek<<Vec<Token> as IntoIterator>::IntoIter>,
    current_function_labels: Vec<WithToken<String>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, tokens: Vec<Token>) -> Self {
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

    fn error_at(&mut self, token: Token, error: ParseErrorType) -> ParseError {
        ParseError { token, error }
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

    fn check_declaration(&mut self) -> bool {
        match self.peek_token_type() {
            Some(TokenType::KInt) => true,
            _ => false,
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
        self.consume(TokenType::KInt, "int")?;

        let name = self.consume_identifier()?;

        self.consume(TokenType::LParen, "(")?;
        self.consume(TokenType::Identifier(String::from("void")), "void")?;
        self.consume(TokenType::RParen, ")")?;

        let body = CompoundStmt {
            block: self.block("function")?,
            introduce_scope: true,
        };
        self.current_function_labels.clear();

        Ok(FunctionDef { name, body })
    }

    fn block_item(&mut self, context: &'static str) -> ParseResult<BlockItem> {
        match self.peek_token_type() {
            _ if self.check_declaration() => self.declaration().map(BlockItem::VarDecl),
            _ => Ok(BlockItem::Stmt(self.statement(context)?)),
        }
    }
}

impl<'a> Parser<'a> {
    // Methods for parsing expressions

    fn optional_expression(
        &mut self,
        followed_by: TokenType,
        followed_by_str: &'static str,
    ) -> ParseResult<(Option<Expr>, Token)> {
        match self.peek_token_type() {
            Some(tt) if *tt == followed_by => Ok((None, self.advance().unwrap())),
            _ => {
                let expr = self.expression()?;
                let token = self.consume(followed_by, followed_by_str)?;
                Ok((Some(expr), token))
            }
        }
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        let mut expr = self.expression_no_comma()?;
        while let Some(TokenType::Comma) = self.peek_token_type() {
            self.advance();
            let rhs = self.expression_no_comma()?;
            expr = Expr::Comma(Comma(Box::new(expr), Box::new(rhs)));
        }
        Ok(expr)
    }

    fn expression_no_comma(&mut self) -> ParseResult<Expr> {
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
            let qmark = self.advance().unwrap();
            let then_expr = Box::new(self.expression()?);
            let colon = self.consume(TokenType::Colon, ":")?;
            let else_expr = Box::new(self.conditional_expr()?);

            Ok(Expr::Conditional(Conditional {
                cond: Box::new(cond),
                then_expr,
                else_expr,
                qmark,
                colon,
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
                        token::Literal::Integer(value) => {
                            Literal::Integral(Integral::Integer(*value))
                        }
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
    fn declaration(&mut self) -> ParseResult<Vec<VarDecl>> {
        self.advance();
        let mut decls = Vec::new();

        loop {
            let name = self.consume_identifier()?;

            let init = if let Some(TokenType::Equal) = self.peek_token_type() {
                self.advance();
                let rhs = self.expression_no_comma()?;
                Some(rhs)
            } else {
                None
            };

            decls.push(VarDecl { name, init });
            if !self.match_(TokenType::Comma) {
                break;
            }
        }
        self.consume(TokenType::Semicolon, ";")?;

        Ok(decls)
    }

    fn statement(&mut self, context: &'static str) -> ParseResult<Stmt> {
        match self.peek_token_type() {
            Some(TokenType::LBrace) => self.block(context).map(|block| {
                Stmt::Compound(CompoundStmt {
                    introduce_scope: true,
                    block,
                })
            }),
            Some(TokenType::KBreak) => self.break_or_continue(TokenType::KBreak),
            Some(TokenType::KCase) => Err(self.error_at_current(ParseErrorType::CaseOutsideSwitch)),
            Some(TokenType::KContinue) => self.break_or_continue(TokenType::KContinue),
            Some(TokenType::KDefault) => {
                Err(self.error_at_current(ParseErrorType::DefaultOutsideSwitch))
            }
            Some(TokenType::KDo) => self.do_while_stmt(),
            Some(TokenType::KFor) => self.for_stmt(),
            Some(TokenType::KGoto) => self.goto_stmt(),
            Some(TokenType::KIf) => self.if_stmt(),
            Some(TokenType::KReturn) => self.return_statement(),
            Some(TokenType::KSwitch) => self.switch_statement(),
            Some(TokenType::KWhile) => self.while_statement(),
            Some(TokenType::Semicolon) => {
                self.advance();
                Ok(Stmt::Null)
            }
            Some(TokenType::KInt) => {
                let mut decl = self.declaration()?;
                let tok = decl.pop().unwrap();
                Err(self.error_at(
                    tok.name.1,
                    ParseErrorType::InvalidDeclaration {
                        decl_type: "variable",
                        context,
                    },
                ))
            }
            Some(TokenType::Identifier(_)) => {
                if let Some(Token {
                    tok_type: TokenType::Colon,
                    ..
                }) = self.peek_next()
                {
                    self.label(context)
                } else {
                    self.expression_statement()
                }
            }
            _ => self.expression_statement(),
        }
    }

    fn break_or_continue(&mut self, tok_type: TokenType) -> ParseResult<Stmt> {
        let tok = self.advance().unwrap();
        let stmt = match tok_type {
            TokenType::KBreak => Stmt::Break(BreakStmt {
                loop_or_switch: false,
                id: WithToken(-1, tok),
            }),
            TokenType::KContinue => Stmt::Continue(WithToken(-1, tok)),
            _ => unreachable!(),
        };
        self.consume(TokenType::Semicolon, ";")?;
        Ok(stmt)
    }

    fn block(&mut self, context: &'static str) -> ParseResult<Block> {
        self.consume(TokenType::LBrace, "{")?;

        let block = self.block_body(&[TokenType::RBrace], context)?;

        self.consume(TokenType::RBrace, "}")?;
        Ok(block)
    }

    fn block_body(&mut self, end: &[TokenType], context: &'static str) -> ParseResult<Block> {
        let mut body = Vec::new();
        loop {
            match self.peek_token_type() {
                Some(tt) if end.contains(tt) => break,
                _ => body.push(self.block_item(context)?),
            };
        }

        Ok(Block(body))
    }

    fn do_while_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KDo, "do")?;
        let body = Box::new(self.statement("do while loop")?);

        self.consume(TokenType::KWhile, "while")?;
        let token = self.consume(TokenType::LParen, "(")?;
        let cond = WithToken(self.expression()?, token);
        self.consume(TokenType::RParen, ")")?;
        self.consume(TokenType::Semicolon, ";")?;

        Ok(Stmt::While(WhileStmt {
            loop_id: -1,
            cond,
            body,
            do_while: true,
        }))
    }

    fn for_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KFor, "for")?;
        let lparen = self.consume(TokenType::LParen, "(")?;

        let init = if self.check_declaration() {
            Some(ForStmtInitializer::VarDecl(self.declaration()?))
        } else {
            self.optional_expression(TokenType::Semicolon, ";")?
                .0
                .map(ForStmtInitializer::Expr)
        };

        let (cond, cond_semi) = self.optional_expression(TokenType::Semicolon, ";")?;

        let (step, step_semi) = self.optional_expression(TokenType::RParen, ")")?;

        let body = self.statement("for statement")?;

        Ok(Stmt::For(ForStmt {
            loop_id: -1,
            initializer: WithToken(init, lparen),
            condition: WithToken(cond, cond_semi),
            step: WithToken(step, step_semi),
            body: Box::new(body),
        }))
    }

    fn goto_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KGoto, "goto")?;
        let label = self.consume_identifier()?;
        self.consume(TokenType::Semicolon, ";")?;
        Ok(Stmt::Goto(label))
    }

    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KIf, "if")?;

        let token = self.consume(TokenType::LParen, "(")?;
        let cond = WithToken(self.expression()?, token);
        self.consume(TokenType::RParen, ")")?;

        let then = Box::new(self.statement("if statement")?);
        let else_clause = if let Some(TokenType::KElse) = self.peek_token_type() {
            let token = self.advance().unwrap();
            Some(Box::new(WithToken(self.statement("if statement")?, token)))
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
        let token = self.consume(TokenType::KReturn, "return")?;
        let ret_value = WithToken(self.expression()?, token);
        self.consume(TokenType::Semicolon, ";")?;
        Ok(Stmt::Return { ret_value })
    }

    fn switch_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KSwitch, "switch")?;

        let cond_token = self.consume(TokenType::LParen, "(")?;
        let cond = WithToken(self.expression()?, cond_token);
        self.consume(TokenType::RParen, ")")?;

        let mut cases: OrderMap<Integral, (CompoundStmt, Token, Token)> = OrderMap::new();
        let mut default_case = None;
        self.consume(TokenType::LBrace, "{")?;

        while let Some(TokenType::KCase | TokenType::KDefault) = self.peek_token_type() {
            let kw = self.advance().unwrap();

            if let TokenType::KCase = kw.tok_type {
                let value = self.expression()?;
                let (value, tok) = match value {
                    Expr::Literal(WithToken(Literal::Integral(lit), tok)) => (lit, tok),
                    _ => return Err(self.error_at_current(ParseErrorType::CaseValueNotConstant)),
                };

                if cases.contains_key(&value) {
                    return Err(self.error_at_current(ParseErrorType::DuplicateCaseValue {
                        prev: cases[&value].1.clone(),
                    }));
                }

                self.consume(TokenType::Colon, ":")?;

                let stmt = CompoundStmt {
                    introduce_scope: false,
                    block: self.block_body(
                        &[TokenType::KCase, TokenType::KDefault, TokenType::RBrace],
                        "case body",
                    )?,
                };
                cases.insert(value, (stmt, tok, kw));
            } else {
                if let Some(WithToken(_, tok)) = default_case {
                    return Err(
                        self.error_at_current(ParseErrorType::DuplicateDefaultLabel { prev: tok })
                    );
                }
                self.consume(TokenType::Colon, ":")?;
                let stmt = CompoundStmt {
                    introduce_scope: false,
                    block: self.block_body(
                        &[TokenType::KCase, TokenType::KDefault, TokenType::RBrace],
                        "case body",
                    )?,
                };
                default_case = Some(WithToken(stmt, kw));
            }
        }

        self.consume(TokenType::RBrace, "}")?;
        let cases = cases
            .into_iter()
            .map(|(val, (stmt, lit, kw))| {
                WithToken(
                    Case {
                        value: WithToken(Literal::Integral(val), lit),
                        stmt,
                    },
                    kw,
                )
            })
            .collect();
        let default = default_case.map(|wt| wt.unwrap()).unwrap_or(CompoundStmt {
            block: Block(Vec::new()),
            introduce_scope: false,
        });

        Ok(Stmt::Switch(SwitchStmt {
            switch_id: -1,
            cond,
            cases,
            default,
        }))
    }

    fn while_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::KWhile, "while")?;

        let token = self.consume(TokenType::LParen, "(")?;
        let cond = WithToken(self.expression()?, token);
        self.consume(TokenType::RParen, ")")?;

        let body = Box::new(self.statement("while statement")?);

        Ok(Stmt::While(WhileStmt {
            loop_id: -1,
            cond,
            body,
            do_while: false,
        }))
    }

    fn label(&mut self, context: &'static str) -> ParseResult<Stmt> {
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
        let next_stmt = Box::new(self.statement(context)?);
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
