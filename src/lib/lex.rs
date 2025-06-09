use std::{fmt::Display, iter::Filter, num::ParseFloatError};

use crate::input::{Cursor, HasSpan, Pos, Span};

#[derive(Debug, Clone)]
pub enum Error {
    InvalidChar(Span, char),
    InvalidNum(Span, String, ParseFloatError),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidChar(_, c) => {
                write!(f, "Invalid character: {c}")
            }
            Error::InvalidNum(_, s, err) => {
                write!(f, "Invalid number: {s}: {err}")
            }
        }
    }
}

impl HasSpan for Error {
    fn span(&self) -> Span {
        match self {
            Error::InvalidChar(span, _) => *span,
            Error::InvalidNum(span, _, _) => *span,
        }
    }
}

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
    Hat,
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
    I: Iterator<Item = char> + Clone,
{
    pub fn in_band(self) -> Filter<Tokenizer<I>, fn(&Result<Token>) -> bool> {
        self.filter(|tok| {
            !matches!(
                tok,
                Ok(Token {
                    kind: TokenKind::Space | TokenKind::Comment(..),
                    ..
                })
            )
        })
    }

    fn parse_num(&mut self, pos: Pos, first: char) -> Result<f64> {
        // externalize function for testing
        do_parse_num(&mut self.cursor, pos, first)
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
            '^' => TokenKind::Hat,
            ',' => TokenKind::Comma,
            '\n' => TokenKind::NewLine,
            '#' => {
                let mut s = String::new();
                loop {
                    let c = self.cursor.first();
                    match c {
                        Some(c) if c != '\n' => {
                            self.cursor.next();
                            s.push(c)
                        }
                        _ => break,
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
                    let c = self.cursor.first();
                    match c {
                        Some(c @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_')) => {
                            self.cursor.next();
                            sym.push(c)
                        }
                        _ => break,
                    }
                }
                TokenKind::Symbol(sym)
            }
            c if c.is_ascii_whitespace() => {
                loop {
                    let c = self.cursor.first();
                    match c {
                        Some(c) if c.is_ascii_whitespace() => {
                            self.cursor.next();
                        }
                        _ => break,
                    }
                }
                TokenKind::Space
            }
            _ => return Err(Error::InvalidChar((pos, pos + 1), c)),
        };
        Ok(Some(kind))
    }
}

impl<I> Iterator for Tokenizer<I>
where
    I: Iterator<Item = char> + Clone,
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

fn do_parse_num<I>(cursor: &mut Cursor<I>, pos: u32, first: char) -> Result<f64>
where
    I: Iterator<Item = char> + Clone,
{
        let mut s = String::from(first);
        loop {
            let c = cursor.first();
            match c {
                Some(c) if c.is_ascii_digit() || c == '.' => {
                    cursor.next();
                    s.push(c)
                }
                _ => break,
            }
        }
        match s.parse::<f64>() {
            Ok(n) => Ok(n),
            Err(err) => Err(Error::InvalidNum((pos, pos + s.len() as u32), s, err)),
        }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_number(s: &str) -> Result<f64> {
        let mut cursor = Cursor::new(s.chars());
        let first = cursor.next().unwrap(); 
        do_parse_num(&mut cursor, 0, first)
    }

    #[test]
    fn test_parse_num() {
        assert_eq!(parse_number("123").unwrap(), 123.0);
        assert_eq!(parse_number("123.456").unwrap(), 123.456);
        assert_eq!(parse_number(".456").unwrap(), 0.456);
        assert_eq!(parse_number("1.23e4").unwrap(), 12300.0);
        assert_eq!(parse_number("1.23e-4").unwrap(), 0.000123);
        assert_eq!(parse_number("1.23e+4").unwrap(), 12300.0);
        assert_eq!(parse_number(".456e+4").unwrap(), 4560.0);
        assert_eq!(parse_number("5e4").unwrap(), 50000.0);
        assert_eq!(parse_number("12e3").unwrap(), 12000.0);
        assert_eq!(parse_number("123.").unwrap(), 123.0);
        assert!(parse_number("123.456.789").is_err());
        assert!(parse_number("abc").is_err());
    }

    #[test]
    fn test_tokenize() {
        let tokens: Vec<_> = tokenize("1 + 2 # a comment".chars())
            .map(Result::unwrap)
            .collect();
        assert_eq!(
            tokens,
            vec![
                Token {
                    span: (0, 1),
                    kind: TokenKind::Num(1.0),
                },
                Token {
                    span: (1, 2),
                    kind: TokenKind::Space,
                },
                Token {
                    span: (2, 3),
                    kind: TokenKind::Plus,
                },
                Token {
                    span: (3, 4),
                    kind: TokenKind::Space,
                },
                Token {
                    span: (4, 5),
                    kind: TokenKind::Num(2.0),
                },
                Token {
                    span: (5, 6),
                    kind: TokenKind::Space,
                },
                Token {
                    span: (6, 17),
                    kind: TokenKind::Comment(" a comment".to_string()),
                },
            ]
        );
    }

    #[test]
    fn test_tokenize_in_band() {
        let tokens: Vec<_> = tokenize("1 + 2 # a comment".chars())
            .in_band()
            .map(Result::unwrap)
            .collect();
        assert_eq!(
            tokens,
            vec![
                Token {
                    span: (0, 1),
                    kind: TokenKind::Num(1.0),
                },
                Token {
                    span: (2, 3),
                    kind: TokenKind::Plus,
                },
                Token {
                    span: (4, 5),
                    kind: TokenKind::Num(2.0),
                },
            ]
        );
    }

    #[test]
    fn test_tokenize_sin_pi() {
        let tokens: Vec<_> = tokenize("sin(pi)".chars()).map(Result::unwrap).collect();
        assert_eq!(
            tokens,
            vec![
                Token {
                    span: (0, 3),
                    kind: TokenKind::Symbol("sin".to_string()),
                },
                Token {
                    span: (3, 4),
                    kind: TokenKind::OpenPar,
                },
                Token {
                    span: (4, 6),
                    kind: TokenKind::Symbol("pi".to_string()),
                },
                Token {
                    span: (6, 7),
                    kind: TokenKind::ClosePar,
                },
            ]
        );
    }
}
