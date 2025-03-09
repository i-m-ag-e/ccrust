use super::*;

fn match_expected(input: &str, expected: Vec<TokenType>) {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens.len(), expected.len());

    tokens
        .iter()
        .zip(expected.iter())
        .for_each(|(t, e)| assert_eq!(t.tok_type, *e));
}

#[test]
fn test_simple_syntax() {
    let input = "+ - = < > !\n// hello\nint name;";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let expected = vec![
        Token {
            tok_type: TokenType::Plus,
            span: Span(0, 1),
            line: 1,
        },
        Token {
            tok_type: TokenType::Minus,
            span: Span(2, 3),
            line: 1,
        },
        Token {
            tok_type: TokenType::Equal,
            span: Span(4, 5),
            line: 1,
        },
        Token {
            tok_type: TokenType::Lesser,
            span: Span(6, 7),
            line: 1,
        },
        Token {
            tok_type: TokenType::Greater,
            span: Span(8, 9),
            line: 1,
        },
        Token {
            tok_type: TokenType::Bang,
            span: Span(10, 11),
            line: 1,
        },
        Token {
            tok_type: TokenType::KInt,
            span: Span(21, 24),
            line: 3,
        },
        Token {
            tok_type: TokenType::Identifier("name".to_string()),
            span: Span(25, 29),
            line: 3,
        },
        Token {
            tok_type: TokenType::Semicolon,
            span: Span(29, 30),
            line: 3,
        },
    ];

    assert_eq!(tokens.len(), expected.len());

    tokens
        .iter()
        .zip(expected.iter())
        .for_each(|(t, e)| assert_eq!(t, e));
}

#[test]
fn test_multi_char_toks() {
    let input = "++ -- == != <= >= << >> && || += -= *= /= %= &= |= ^= <<= >>=";
    let expected = vec![
        TokenType::Increment,
        TokenType::Decrement,
        TokenType::EqEqual,
        TokenType::BangEq,
        TokenType::LesserEq,
        TokenType::GreaterEq,
        TokenType::LShift,
        TokenType::RShift,
        TokenType::And,
        TokenType::Or,
        TokenType::PlusEq,
        TokenType::MinusEq,
        TokenType::StarEq,
        TokenType::SlashEq,
        TokenType::PercEq,
        TokenType::BitwiseANDEq,
        TokenType::BitwiseOREq,
        TokenType::BitwiseXOREq,
        TokenType::LShiftEq,
        TokenType::RShiftEq,
    ];

    match_expected(input, expected);
}

#[test]
fn test_strings() {
    let input = r#"
            "hello" "world"
            "hello\nworld" "hello\tworld"
            "hello\rworld" "hello\12world" "#;
    let expected = vec![
        TokenType::Literal(Literal::String("hello".to_string())),
        TokenType::Literal(Literal::String("world".to_string())),
        TokenType::Literal(Literal::String("hello\nworld".to_string())),
        TokenType::Literal(Literal::String("hello\tworld".to_string())),
        TokenType::Literal(Literal::String("hello\rworld".to_string())),
        TokenType::Literal(Literal::String("hello\nworld".to_string())),
    ];

    match_expected(input, expected);
}

#[test]
fn test_number_literals() {
    let input = "123 123.456";
    let expected = vec![
        TokenType::Literal(Literal::Integer(123)),
        TokenType::Literal(Literal::Float(123.456)),
    ];

    match_expected(input, expected);
}

#[test]
fn test_char_literals() {
    let input = "'a' 'b' '\\n' '\\t' '\\r' '\\0' '\\12'";
    let expected = vec![
        TokenType::Literal(Literal::Char('a')),
        TokenType::Literal(Literal::Char('b')),
        TokenType::Literal(Literal::Char('\n')),
        TokenType::Literal(Literal::Char('\t')),
        TokenType::Literal(Literal::Char('\r')),
        TokenType::Literal(Literal::Char('\0')),
        TokenType::Literal(Literal::Char('\n')),
    ];

    match_expected(input, expected);
}

#[test]
fn test_keywords() {
    let input = "else for goto if int return while";
    let expected = vec![
        TokenType::KElse,
        TokenType::KFor,
        TokenType::KGoto,
        TokenType::KIf,
        TokenType::KInt,
        TokenType::KReturn,
        TokenType::KWhile,
    ];

    match_expected(input, expected);
}

#[test]
fn test_punctuators() {
    let input = ", . { ( } ) ; ! & | ^ = > < + - * / ? :";
    let expected = vec![
        TokenType::Comma,
        TokenType::Dot,
        TokenType::LBrace,
        TokenType::LParen,
        TokenType::RBrace,
        TokenType::RParen,
        TokenType::Semicolon,
        TokenType::Bang,
        TokenType::BitwiseAND,
        TokenType::BitwiseOR,
        TokenType::BitwiseXOR,
        TokenType::Equal,
        TokenType::Greater,
        TokenType::Lesser,
        TokenType::Plus,
        TokenType::Minus,
        TokenType::Star,
        TokenType::Slash,
        TokenType::QMark,
        TokenType::Colon,
    ];

    match_expected(input, expected);
}
