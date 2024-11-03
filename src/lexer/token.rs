#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Char(char),
    Float,
    Integer,
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Equal,
    EqEqual,
    Greater,
    GreaterEq,
    Lesser,
    LesserEq,
    Increment,
    Minus,
    Or,
    Plus,
    Perc,
    Star,
    Slash,
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
