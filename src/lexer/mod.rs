pub mod token;

use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::Chars;
use thiserror::Error;
use token::{Literal, Span, Token, TokenType};

macro_rules! hash_map {
    ( $( $key: expr => $value: expr ),* $(,)? ) => {{
        let mut m = HashMap::new();
        $(
            m.insert($key, $value);
        )*
        m
    }}
}

macro_rules! multi_char_tok {
    ( $self: ident => $c: literal ? $yes: expr ; $no: expr ) => {
        if $self.advance_if_eq($c) {
            Ok($self.make_token($yes))
        } else {
            Ok($self.make_token($no))
        }
    };
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = hash_map! {
        "else"   => TokenType::KElse,
        "for"    => TokenType::KFor,
        "if"     => TokenType::KIf,
        "return" => TokenType::KReturn,
        "while"  => TokenType::KWhile,
    };
}

#[derive(Error, Debug)]
pub enum LexerErrorType {
    #[error("unexpected EOF")]
    UnexpectedEOF,

    #[error("invalid digit in {lit_type} literal: {digit:?}")]
    InvalidDigit { lit_type: &'static str, digit: char },

    #[error("invalid octal literal {context}: {error}")]
    InvalidOctalLiteral {
        context: &'static str,
        error: String,
    },

    #[error("invalid escape {0:?}")]
    InvalidEscape(char),

    #[error("expected end of char `'`, found {0:?}")]
    MultiCharLiteral(char),

    #[error("expected start of token, found {0:?}")]
    UnexpectedChar(char),
}

pub type LexerResult = Result<Token, LexerError>;

#[derive(Debug)]
pub struct LexerError {
    pub token: Token,
    pub error: LexerErrorType,
}

pub struct Lexer<'a> {
    input_str: &'a str,
    input: Chars<'a>,
    line: usize,
    start: usize,
    current: usize,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input_str: &'a str) -> Self {
        Self {
            input_str,
            input: input_str.chars(),
            line: 1,
            start: 0,
            current: 0,
            eof: false,
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.clone().next()
    }

    fn peek_next(&self) -> Option<char> {
        let mut iter = self.input.clone();
        match iter.next() {
            Some(_) => iter.next(),
            None => None,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.input.next()
    }

    fn advance_if_digit(&mut self, lit_type: &'static str, c: char) -> Result<(), LexerError> {
        if let '0'..='9' = c {
            self.advance();
            Ok(())
        } else {
            Err(self.make_error(LexerErrorType::InvalidDigit { lit_type, digit: c }))
        }
    }

    fn make_token(&self, tok_type: TokenType) -> Token {
        let span = Span(self.start, self.current);
        Token {
            tok_type,
            span,
            line: self.line,
        }
    }

    fn make_error(&self, error: LexerErrorType) -> LexerError {
        LexerError {
            token: self.make_token(TokenType::Error),
            error,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some('\r' | '\t' | ' ') => self.advance(),
                Some('\n') => {
                    self.line += 1;
                    self.advance()
                }
                _ => break,
            };
        }
    }

    fn advance_if_eq(&mut self, c: char) -> bool {
        match self.peek() {
            Some(ch) if c == ch => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn get_lexeme(&self) -> &'a str {
        let start = self.start;
        let end = self.current;
        &self.input_str[start..end]
    }

    fn number(&mut self) -> LexerResult {
        while let Some(c) = self.peek() {
            if c.is_ascii_whitespace() || c == '.' {
                break;
            }
            self.advance_if_digit("decimal", c)?;
            // if let '0'..='9' = c {
            //     self.advance();
            // } else {
            //     return Err(self.make_error(LexerErrorType::InvalidDigit {
            //         lit_type: "decimal",
            //         digit: c,
            //     }));
            // }
        }

        if let Some('.') = self.peek() {
            self.advance();
            while let Some(c) = self.peek() {
                if c.is_ascii_whitespace() {
                    break;
                }
                self.advance_if_digit("decimal", c)?;
            }
            Ok(self.make_token(TokenType::Literal(Literal::Float)))
        } else {
            Ok(self.make_token(TokenType::Literal(Literal::Integer)))
        }
    }

    fn read_octal(&mut self, one_byte: bool) -> Result<u32, LexerError> {
        let mut counter = 1;
        loop {
            if one_byte && counter == 3 {
                break;
            }

            match self.peek() {
                Some('0'..='7') => {
                    self.advance();
                    counter += 1;
                }
                _ => break,
            };
        }

        let lexeme = self.get_lexeme();
        u32::from_str_radix(&lexeme[2..], 8).map_err(|e| {
            self.make_error(LexerErrorType::InvalidOctalLiteral {
                context: "in string",
                error: format!("{:?}", e),
            })
        })
    }

    fn read_escape(&mut self) -> Result<char, LexerError> {
        if let Some(c) = self.peek() {
            self.advance();
            match c {
                'n' => Ok('\n'),
                't' => Ok('\t'),
                'r' => Ok('\r'),
                '\\' => Ok('\\'),
                '"' => Ok('"'),
                '\'' => Ok('\''),
                '0'..='7' => self.read_octal(true).map(|n| n as u8 as char),
                _ => Err(self.make_error(LexerErrorType::InvalidEscape(c))),
            }
        } else {
            Err(self.make_error(LexerErrorType::UnexpectedEOF))
        }
    }

    fn parse_char(&mut self) -> Result<char, LexerError> {
        match self.peek() {
            Some(c) => {
                self.advance();
                if c == '\\' {
                    self.read_escape()
                } else {
                    Ok(c)
                }
            }
            None => Err(self.make_error(LexerErrorType::UnexpectedEOF)),
        }
    }

    fn string(&mut self) -> LexerResult {
        let mut string = String::new();
        loop {
            match self.peek() {
                None => return Err(self.make_error(LexerErrorType::UnexpectedEOF)),
                Some('"') => {
                    self.advance();
                    break;
                }
                Some(_) => string.push(self.parse_char()?),
            };
        }
        Ok(self.make_token(TokenType::Literal(Literal::String(string))))
    }

    fn identifier(&mut self) -> LexerResult {
        loop {
            match self.peek() {
                Some(_c) if _c == '_' || _c.is_ascii_alphanumeric() => self.advance(),
                _ => break,
            };
        }
        let lexeme = self.get_lexeme();

        if let Some(ttype) = KEYWORDS.get(lexeme) {
            Ok(self.make_token(ttype.clone()))
        } else {
            Ok(self.make_token(TokenType::Identifier(lexeme.to_string())))
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        while !self.eof {
            tokens.push(self.next_token()?);
        }
        tokens.pop().unwrap();
        Ok(tokens)
    }

    pub fn next_token(&mut self) -> LexerResult {
        self.skip_whitespace();

        self.start = self.current;
        let c = if let Some(c) = self.advance() {
            c
        } else {
            self.eof = true;
            return Ok(self.make_token(TokenType::EOF));
        };

        match c {
            ',' => Ok(self.make_token(TokenType::Comma)),
            '.' => Ok(self.make_token(TokenType::Dot)),
            '{' => Ok(self.make_token(TokenType::LBrace)),
            '(' => Ok(self.make_token(TokenType::LParen)),
            '}' => Ok(self.make_token(TokenType::RBrace)),
            ')' => Ok(self.make_token(TokenType::RParen)),
            ';' => Ok(self.make_token(TokenType::Semicolon)),

            '!' => multi_char_tok!(self => '=' ? TokenType::BangEq; TokenType::Bang),
            '&' => multi_char_tok!(self => '&' ? TokenType::And; TokenType::BitwiseAND),
            '|' => multi_char_tok!(self => '|' ? TokenType::Or; TokenType::BitwiseOR),
            '^' => Ok(self.make_token(TokenType::BitwiseXOR)),
            '-' => multi_char_tok!(self => '-' ? TokenType::Decrement; TokenType::Minus),
            '=' => multi_char_tok!(self => '=' ? TokenType::EqEqual; TokenType::Equal),
            '>' => multi_char_tok!(self => '=' ? TokenType::GreaterEq; TokenType::Greater),
            '<' => multi_char_tok!(self => '=' ? TokenType::LesserEq; TokenType::Lesser),
            '+' => multi_char_tok!(self => '+' ? TokenType::Increment; TokenType::Plus),
            '%' => Ok(self.make_token(TokenType::Perc)),
            '*' => Ok(self.make_token(TokenType::Star)),
            '/' => Ok(self.make_token(TokenType::Slash)),
            '~' => Ok(self.make_token(TokenType::Tilde)),

            '\'' => {
                let result = self.parse_char();
                match self.advance() {
                    Some('\'') => {
                        result.map(|c| self.make_token(TokenType::Literal(Literal::Char(c))))
                    }
                    Some(c) => Err(self.make_error(LexerErrorType::MultiCharLiteral(c))),
                    None => Err(self.make_error(LexerErrorType::UnexpectedEOF)),
                }
            }
            '"' => self.string(),

            c if c.is_ascii_digit() => self.number(),
            c if c == '_' || c.is_ascii_alphabetic() => self.identifier(),
            _ => Err(self.make_error(LexerErrorType::UnexpectedChar(c))),
        }
    }
}
