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
    Colon,
    Dot,
    LBrace,
    LParen,
    QMark,
    RBrace,
    RParen,
    Semicolon,

    And,
    Bang,
    BangEq,
    BitwiseAND,
    BitwiseANDEq,
    BitwiseOR,
    BitwiseOREq,
    BitwiseXOR,
    BitwiseXOREq,
    Decrement,
    EqEqual,
    Equal,
    Greater,
    GreaterEq,
    Increment,
    LShift,
    LShiftEq,
    Lesser,
    LesserEq,
    Minus,
    MinusEq,
    Or,
    Perc,
    PercEq,
    Plus,
    PlusEq,
    RShift,
    RShiftEq,
    Slash,
    SlashEq,
    Star,
    StarEq,
    Tilde,

    KBreak,
    KCase,
    KContinue,
    KDefault,
    KDo,
    KElse,
    KFor,
    KGoto,
    KIf,
    KInt,
    KReturn,
    KSwitch,
    KWhile,

    Literal(Literal),
    Identifier(String),

    Error,
    EOF,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
    pub line: usize,
    pub tok_type: TokenType,
}
