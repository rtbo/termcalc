use std::{iter::Filter, num::ParseFloatError};

use crate::input::{Cursor, LineCol, Pos, Span};

#[derive(Debug, Clone)]
pub enum Error {
    InvalidChar(LineCol, char),
    InvalidNum(LineCol, String, ParseFloatError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidChar((line, col), c) => {
                write!(f, "Invalid character '{}' at {}:{}", c, line, col)
            }
            Error::InvalidNum((line, col), s, e) => {
                write!(f, "Invalid number '{}' ({}) at {}:{}", s, e, line, col)
            }
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Num(f64),
    Symbol(String),
    OpenPar,
    ClosePar,
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Comma,
    NewLine,
    Space,
    Comment(String),
}

pub fn tokenize<I>(chars: I) -> Tokenizer<I::IntoIter>
where
    I: IntoIterator<Item = char>,
{
    Tokenizer::new(Cursor::new(chars.into_iter()))
}

#[derive(Debug, Clone)]
pub struct Tokenizer<I> {
    cursor: Cursor<I>,
}

impl<I> Tokenizer<I> {
    pub fn new(cursor: Cursor<I>) -> Tokenizer<I> {
        Tokenizer { cursor }
    }
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    pub fn in_band(self) -> Filter<Tokenizer<I>, fn(&Result<Token>) -> bool> {
        self.filter(|tok| match tok {
            Ok(Token {
                kind: TokenKind::Comment(_) | TokenKind::Space,
                ..
            }) => false,
            _ => true,
        })
    }

    fn parse_num(&mut self, pos: Pos, first: char) -> Result<f64> {
        let mut s = String::from(first);
        loop {
            let c = self.cursor.next();
            match c {
                Some(c) if c.is_ascii_digit() || c == '.' => s.push(c),
                Some(c) => {
                    self.cursor.put_back(c);
                    break;
                }
                None => break,
            }
        }
        match s.parse::<f64>() {
            Ok(n) => Ok(n),
            Err(err) => Err(Error::InvalidNum(self.cursor.line_col(pos), s, err)),
        }
    }

    fn next_token_kind(&mut self, pos: Pos) -> Result<Option<TokenKind>> {
        let c = match self.cursor.next() {
            None => return Ok(None),
            Some(c) => c,
        };

        let kind = match c {
            '(' => TokenKind::OpenPar,
            ')' => TokenKind::ClosePar,
            '=' => TokenKind::Equal,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            ',' => TokenKind::Comma,
            '\n' => TokenKind::NewLine,
            '#' => {
                let mut s = String::new();
                loop {
                    let c = self.cursor.next();
                    match c {
                        Some(c) if c != '\n' => s.push(c),
                        Some(c) => {
                            self.cursor.put_back(c);
                            break;
                        }
                        None => break,
                    }
                }
                TokenKind::Comment(s)
            }
            '0'..='9' | '.' => {
                let num = self.parse_num(pos, c)?;
                TokenKind::Num(num)
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut sym = String::new();
                sym.push(c);
                loop {
                    let c = self.cursor.next();
                    match c {
                        Some(c @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_')) => sym.push(c),
                        Some(c) => {
                            self.cursor.put_back(c);
                            break;
                        }
                        None => break,
                    }
                }
                TokenKind::Symbol(sym)
            }
            c if c.is_ascii_whitespace() => {
                loop {
                    let c = self.cursor.next();
                    match c {
                        Some(c) if c.is_ascii_whitespace() => (),
                        Some(c) => {
                            self.cursor.put_back(c);
                            break;
                        }
                        None => break,
                    }
                }
                TokenKind::Space
            }
            _ => return Err(Error::InvalidChar(self.cursor.line_col(pos), c)),
        };
        Ok(Some(kind))
    }
}

impl<I> Iterator for Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Result<Token>> {
        let pos = self.cursor.pos();
        let kind = match self.next_token_kind(pos) {
            Ok(Some(kind)) => kind,
            Ok(None) => return None,
            Err(err) => return Some(Err(err)),
        };
        let end = self.cursor.pos();
        Some(Ok(Token {
            kind,
            span: (pos, end),
        }))
    }
}

#[test]
fn test_tokenize() {
    let tokens: Vec<_> = tokenize("1 + 2 # a comment".chars())
        .map(Result::unwrap)
        .collect();
    assert_eq!(
        tokens,
        vec![
            Token{
                span: (0, 1),
                kind: TokenKind::Num(1.0),
            },
            Token{
                span: (1, 2),
                kind: TokenKind::Space,
            },
            Token{
               span: (2, 3),
                kind: TokenKind::Plus,
            },
            Token{
                span: (3, 4),
                kind: TokenKind::Space,
            },
            Token{
                span: (4, 5),
                kind: TokenKind::Num(2.0),
            },
            Token{
                span: (5, 6),
                kind: TokenKind::Space,
            },
            Token{
                span: (6, 17),
                kind: TokenKind::Comment(" a comment".to_string()),
            },
        ]
    );
}

#[test]
fn test_tokenize_in_band() {
    let tokens: Vec<_> = tokenize("1 + 2 # a comment".chars()).in_band()
        .map(Result::unwrap)
        .collect();
    assert_eq!(
        tokens,
        vec![
            Token{
                span: (0, 1),
                kind: TokenKind::Num(1.0),
            },
            Token{
               span: (2, 3),
                kind: TokenKind::Plus,
            },
            Token{
                span: (4, 5),
                kind: TokenKind::Num(2.0),
            },
        ]
    );
}
