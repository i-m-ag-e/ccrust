#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Char(char),
    Float(f64),
    Integer(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Comma,
    Dot,
    LBrace,
    LParen,
    RBrace,
    RParen,
    Semicolon,

    And,
    Bang,
    BangEq,
    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    Decrement,
    EqEqual,
    Equal,
    Greater,
    GreaterEq,
    Increment,
    LShift,
    Lesser,
    LesserEq,
    Minus,
    Or,
    Perc,
    Plus,
    RShift,
    Slash,
    Star,
    Tilde,

    KElse,
    KFor,
    KIf,
    KReturn,
    KWhile,

    Literal(Literal),
    Identifier(String),

    Error,
    EOF,
}

#[derive(Debug, Clone, Copy)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub line: usize,
    pub tok_type: TokenType,
}
