//! Parser module
//!
//! See `Grammar.ebnf`
//! ```
use std::fmt::Display;

use crate::ast;
use crate::input::{HasSpan, Span};
use crate::lex::{self, Token, TokenKind};
use crate::util;

#[derive(Debug)]
pub enum Error {
    Lex(lex::Error),
    UnexpectedEndOfInput(Span),
    UnexpectedToken(Token, Option<String>),
}

impl From<lex::Error> for Error {
    fn from(e: lex::Error) -> Self {
        Error::Lex(e)
    }
}

impl HasSpan for Error {
    fn span(&self) -> Span {
        match self {
            Error::Lex(err) => err.span(),
            Error::UnexpectedEndOfInput(span) => *span,
            Error::UnexpectedToken(tok, _) => tok.span,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Lex(err) => err.fmt(f),
            Error::UnexpectedEndOfInput(_) => {
                write!(f, "Unexpected end of input")
            }
            Error::UnexpectedToken(tok, expected) => {
                write!(f, "Unexpected token: {:?}", tok.kind)?;
                if let Some(expected) = expected {
                    write!(f, " (expected {})", expected)?;
                }
                Ok(())
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

/// Parse the given string to produce an AST item
pub fn parse_line<I>(chars: I) -> Result<ast::Item>
where
    I: IntoIterator<Item = char>,
{
    let tokens = lex::tokenize(chars).in_band();
    Parser::new(tokens).parse_item()
}

#[derive(Debug, Clone)]
pub struct Parser<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    tokens: util::PutBack<T>,
    last_span: Span,
}

impl<T> Parser<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    pub fn new(tokens: T) -> Parser<T> {
        Parser {
            tokens: util::put_back(tokens),
            last_span: (0, 0),
        }
    }
}

impl<T> Parser<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    fn expect_kind(&mut self, kind: TokenKind) -> Result<Span> {
        let tok = self.next_token()?;
        if let Some(tok) = tok {
            if tok.kind == kind {
                Ok(tok.span)
            } else {
                Err(Error::UnexpectedToken(tok, Some(format!("{:?}", kind))))
            }
        } else {
            let span = (self.last_span.1, self.last_span.1 + 2);
            Err(Error::UnexpectedEndOfInput(span))
        }
    }

    fn next_is_kind(&mut self, kind: TokenKind) -> Result<bool> {
        let tok = self.next_token()?;
        if let Some(tok) = tok {
            let res = tok.kind == kind;
            self.tokens.put_back(Ok(tok));
            Ok(res)
        } else {
            Ok(false)
        }
    }
}

impl<T> Parser<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    fn next_token(&mut self) -> Result<Option<Token>> {
        let tok = self.tokens.next().transpose()?;
        if let Some(tok) = &tok {
            self.last_span = tok.span;
        }
        Ok(tok)
    }

    fn parse_item(&mut self) -> Result<ast::Item> {
        let tok = self.next_token()?;
        let item = match tok {
            Some(Token {
                kind: TokenKind::Symbol(sym),
                span,
            }) => {
                if self.next_is_kind(TokenKind::Equal)? {
                    self.next_token()?; // eat '='
                    let expr = self.parse_expr()?;
                    let span = (span.0, expr.span.1);
                    Ok(ast::Item {
                        kind: ast::ItemKind::Assign(sym, expr),
                        span,
                    })
                } else {
                    self.tokens.put_back(Ok(Token {
                        kind: TokenKind::Symbol(sym),
                        span,
                    }));
                    let expr = self.parse_expr()?;
                    let span = expr.span;
                    Ok(ast::Item {
                        kind: ast::ItemKind::Expr(expr),
                        span,
                    })
                }
            }
            Some(tok) => {
                self.tokens.put_back(Ok(tok));
                let expr = self.parse_expr()?;
                let span = expr.span;
                Ok(ast::Item {
                    kind: ast::ItemKind::Expr(expr),
                    span,
                })
            }
            None => Err(Error::UnexpectedEndOfInput(self.last_span)),
        };
        let tok = self.next_token()?;
        if let Some(tok) = tok {
            return Err(Error::UnexpectedToken(
                tok,
                Some("NewLine or Operator".to_string()),
            ));
        }
        item
    }

    fn parse_expr(&mut self) -> Result<ast::Expr> {
        self.parse_add_expr()
    }

    fn parse_add_expr(&mut self) -> Result<ast::Expr> {
        let lhs = self.parse_mul_expr()?;
        let tok = self.next_token()?;
        match tok {
            Some(Token { kind, .. }) if is_add_op(&kind) => {
                let rhs = self.parse_add_expr()?;
                let span = (lhs.span.0, rhs.span.1);
                Ok(ast::Expr {
                    kind: ast::ExprKind::BinOp(bin_op(&kind), Box::new(lhs), Box::new(rhs)),
                    span,
                })
            }
            Some(tok) => {
                self.tokens.put_back(Ok(tok));
                Ok(lhs)
            }
            _ => Ok(lhs),
        }
    }

    fn parse_mul_expr(&mut self) -> Result<ast::Expr> {
        let lhs = self.parse_unary_expr()?;
        let tok = self.next_token()?;
        match tok {
            Some(Token { kind, .. }) if is_mul_op(&kind) => {
                let rhs = self.parse_mul_expr()?;
                let span = (lhs.span.0, rhs.span.1);
                Ok(ast::Expr {
                    kind: ast::ExprKind::BinOp(bin_op(&kind), Box::new(lhs), Box::new(rhs)),
                    span,
                })
            }
            Some(tok) => {
                self.tokens.put_back(Ok(tok));
                Ok(lhs)
            }
            _ => Ok(lhs),
        }
    }

    fn parse_unary_expr(&mut self) -> Result<ast::Expr> {
        let tok = self.next_token()?;
        match tok {
            Some(Token { kind, span }) if is_un_op(&kind) => {
                let expr = self.parse_pow_expr()?;
                let span = (span.0, expr.span.1);
                Ok(ast::Expr {
                    kind: ast::ExprKind::UnOp(un_op(&kind), Box::new(expr)),
                    span,
                })
            }
            _ => {
                if let Some(tok) = tok {
                    self.tokens.put_back(Ok(tok));
                }
                Ok(self.parse_pow_expr()?)
            }
        }
    }

    fn parse_pow_expr(&mut self) -> Result<ast::Expr> {
        let lhs = self.parse_primary()?;
        let tok = self.next_token()?;
        match tok {
            Some(Token {
                kind: TokenKind::Hat,
                ..
            }) => {
                let rhs = self.parse_pow_expr()?;
                let span = (lhs.span.0, rhs.span.1);
                Ok(ast::Expr {
                    kind: ast::ExprKind::BinOp(ast::BinOp::Pow, Box::new(lhs), Box::new(rhs)),
                    span,
                })
            }
            _ => {
                if let Some(tok) = tok {
                    self.tokens.put_back(Ok(tok));
                }
                Ok(lhs)
            }
        }
    }

    fn parse_primary(&mut self) -> Result<ast::Expr> {
        let tok = self.next_token()?;
        match tok {
            Some(Token {
                kind: TokenKind::Num(val),
                span,
            }) => Ok(ast::Expr {
                kind: ast::ExprKind::Num(val),
                span,
            }),
            Some(Token {
                kind: TokenKind::OpenPar,
                span,
            }) => {
                let expr = self.parse_expr()?;
                let endspan = self.expect_kind(TokenKind::ClosePar)?;
                let span = (span.0, endspan.1);
                Ok(ast::Expr {
                    kind: expr.kind,
                    span,
                })
            }
            Some(Token {
                kind: TokenKind::Symbol(sym),
                span: symspan,
            }) => {
                let next = self.next_token()?;
                match next {
                    Some(Token {
                        kind: TokenKind::OpenPar,
                        ..
                    }) => {
                        let args = self.parse_arg_list()?;
                        let closespan = self.expect_kind(TokenKind::ClosePar)?;
                        let span = (symspan.0, closespan.1);
                        Ok(ast::Expr {
                            kind: ast::ExprKind::Call(sym, args),
                            span,
                        })
                    }
                    next => {
                        if let Some(tok) = next {
                            self.tokens.put_back(Ok(tok));
                        }
                        Ok(ast::Expr {
                            kind: ast::ExprKind::Var(sym),
                            span: symspan,
                        })
                    }
                }
            }
            Some(tok) => Err(Error::UnexpectedToken(tok, None)),
            None => Err(Error::UnexpectedEndOfInput(self.last_span)),
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<ast::Expr>> {
        let mut args = Vec::new();
        loop {
            let tok = self.next_token()?;
            match tok {
                Some(Token {
                    kind: TokenKind::ClosePar,
                    span,
                }) => {
                    self.tokens.put_back(Ok(Token {
                        kind: TokenKind::ClosePar,
                        span,
                    }));
                    break;
                }
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => continue,
                Some(tok) => {
                    self.tokens.put_back(Ok(tok));
                    args.push(self.parse_expr()?);
                }
                None => return Err(Error::UnexpectedEndOfInput(self.eoi_span())),
            }
        }
        Ok(args)
    }

    fn eoi_span(&self) -> Span {
        (self.last_span.1, self.last_span.1 + 1)
    }
}

fn is_add_op(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Plus | TokenKind::Minus)
}

fn is_mul_op(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent
    )
}

fn is_un_op(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Plus | TokenKind::Minus)
}

fn bin_op(kind: &TokenKind) -> ast::BinOp {
    match kind {
        TokenKind::Plus => ast::BinOp::Add,
        TokenKind::Minus => ast::BinOp::Sub,
        TokenKind::Star => ast::BinOp::Mul,
        TokenKind::Slash => ast::BinOp::Div,
        TokenKind::Percent => ast::BinOp::Mod,
        _ => unreachable!(),
    }
}

fn un_op(kind: &TokenKind) -> ast::UnOp {
    match kind {
        TokenKind::Plus => ast::UnOp::Plus,
        TokenKind::Minus => ast::UnOp::Minus,
        _ => unreachable!(),
    }
}

#[test]
fn test_parse_add() {
    let item: ast::Item = parse_line("1 + 2".chars()).unwrap();

    assert_eq!(
        item,
        ast::Item {
            span: (0, 5),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 5),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Add,
                    Box::new(ast::Expr {
                        span: (0, 1),
                        kind: ast::ExprKind::Num(1.0),
                    }),
                    Box::new(ast::Expr {
                        span: (4, 5),
                        kind: ast::ExprKind::Num(2.0),
                    })
                )
            })
        }
    )
}

#[test]
fn test_parse_mul() {
    let item: ast::Item = parse_line("2 * 3".chars()).unwrap();

    assert_eq!(
        item,
        ast::Item {
            span: (0, 5),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 5),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Mul,
                    Box::new(ast::Expr {
                        span: (0, 1),
                        kind: ast::ExprKind::Num(2.0),
                    }),
                    Box::new(ast::Expr {
                        span: (4, 5),
                        kind: ast::ExprKind::Num(3.0),
                    })
                )
            })
        }
    )
}

#[test]
fn test_parse_unary() {
    let item: ast::Item = parse_line("-3".chars()).unwrap();

    assert_eq!(
        item,
        ast::Item {
            span: (0, 2),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 2),
                kind: ast::ExprKind::UnOp(
                    ast::UnOp::Minus,
                    Box::new(ast::Expr {
                        span: (1, 2),
                        kind: ast::ExprKind::Num(3.0),
                    }),
                )
            })
        }
    )
}

#[test]
fn test_parse_2nd_order() {
    let item: ast::Item = parse_line("y = 3*x^2 + 4".chars()).unwrap();

    assert_eq!(
        item,
        ast::Item {
            span: (0, 13),
            kind: ast::ItemKind::Assign(
                "y".to_string(),
                ast::Expr {
                    span: (4, 13),
                    kind: ast::ExprKind::BinOp(
                        ast::BinOp::Add,
                        Box::new(ast::Expr {
                            span: (4, 9),
                            kind: ast::ExprKind::BinOp(
                                ast::BinOp::Mul,
                                Box::new(ast::Expr {
                                    span: (4, 5),
                                    kind: ast::ExprKind::Num(3.0),
                                }),
                                Box::new(ast::Expr {
                                    span: (6, 9),
                                    kind: ast::ExprKind::BinOp(
                                        ast::BinOp::Pow,
                                        Box::new(ast::Expr {
                                            span: (6, 7),
                                            kind: ast::ExprKind::Var("x".to_string()),
                                        }),
                                        Box::new(ast::Expr {
                                            span: (8, 9),
                                            kind: ast::ExprKind::Num(2.0),
                                        }),
                                    ),
                                }),
                            )
                        }),
                        Box::new(ast::Expr {
                            span: (12, 13),
                            kind: ast::ExprKind::Num(4.0),
                        })
                    ),
                },
            )
        }
    )
}

#[test]
fn test_parse_add_mul() {
    let items: ast::Item = parse_line("1 + 2 * 3".chars()).unwrap();

    assert_eq!(
        items,
        ast::Item {
            span: (0, 9),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 9),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Add,
                    Box::new(ast::Expr {
                        span: (0, 1),
                        kind: ast::ExprKind::Num(1.0),
                    }),
                    Box::new(ast::Expr {
                        span: (4, 9),
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Mul,
                            Box::new(ast::Expr {
                                span: (4, 5),
                                kind: ast::ExprKind::Num(2.0),
                            }),
                            Box::new(ast::Expr {
                                span: (8, 9),
                                kind: ast::ExprKind::Num(3.0),
                            })
                        ),
                    })
                )
            })
        }
    )
}

#[test]
fn test_parse_mul_add() {
    let items: ast::Item = parse_line("1 * 2 + 3".chars()).unwrap();

    assert_eq!(
        items,
        ast::Item {
            span: (0, 9),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 9),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Add,
                    Box::new(ast::Expr {
                        span: (0, 5),
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Mul,
                            Box::new(ast::Expr {
                                span: (0, 1),
                                kind: ast::ExprKind::Num(1.0),
                            }),
                            Box::new(ast::Expr {
                                span: (4, 5),
                                kind: ast::ExprKind::Num(2.0),
                            })
                        ),
                    }),
                    Box::new(ast::Expr {
                        span: (8, 9),
                        kind: ast::ExprKind::Num(3.0),
                    })
                )
            })
        }
    )
}

#[test]
fn test_parse_mul_add_parentheses() {
    let items: ast::Item = parse_line("1 * (2 + 3)".chars()).unwrap();

    assert_eq!(
        items,
        ast::Item {
            span: (0, 11),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 11),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Mul,
                    Box::new(ast::Expr {
                        span: (0, 1),
                        kind: ast::ExprKind::Num(1.0),
                    }),
                    Box::new(ast::Expr {
                        span: (4, 11),
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Add,
                            Box::new(ast::Expr {
                                span: (5, 6),
                                kind: ast::ExprKind::Num(2.0),
                            }),
                            Box::new(ast::Expr {
                                span: (9, 10),
                                kind: ast::ExprKind::Num(3.0),
                            })
                        )
                    })
                )
            })
        }
    )
}

#[test]
fn test_sin_pi() {
    let item: ast::Item = parse_line("sin(pi)".chars()).unwrap();

    assert_eq!(
        item,
        ast::Item {
            span: (0, 7),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 7),
                kind: ast::ExprKind::Call(
                    "sin".to_string(),
                    vec![ast::Expr {
                        span: (4, 6),
                        kind: ast::ExprKind::Var("pi".to_string(),)
                    }]
                )
            })
        }
    )
}

#[test]
fn fail_test_14_eq_12() {
    assert!(parse_line("14 = 12".chars()).is_err());
}

#[test]
fn test_add_add_add() {
    let item: ast::Item = parse_line("1 + 2 + 3".chars()).unwrap();

    assert_eq!(
        item,
        ast::Item {
            span: (0, 9),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 9),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Add,
                    Box::new(ast::Expr {
                        span: (0, 1),
                        kind: ast::ExprKind::Num(1.0),
                    }),
                    Box::new(ast::Expr {
                        span: (4, 9),
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Add,
                            Box::new(ast::Expr {
                                span: (4, 5),
                                kind: ast::ExprKind::Num(2.0),
                            }),
                            Box::new(ast::Expr {
                                span: (8, 9),
                                kind: ast::ExprKind::Num(3.0),
                            })
                        ),
                    })
                )
            })
        }
    );
}

#[test]
fn test_mul_mul_mul() {
    let item: ast::Item = parse_line("1 * 2 * 3".chars()).unwrap();

    assert_eq!(
        item,
        ast::Item {
            span: (0, 9),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 9),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Mul,
                    Box::new(ast::Expr {
                        span: (0, 1),
                        kind: ast::ExprKind::Num(1.0),
                    }),
                    Box::new(ast::Expr {
                        span: (4, 9),
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Mul,
                            Box::new(ast::Expr {
                                span: (4, 5),
                                kind: ast::ExprKind::Num(2.0),
                            }),
                            Box::new(ast::Expr {
                                span: (8, 9),
                                kind: ast::ExprKind::Num(3.0),
                            })
                        ),
                    })
                )
            })
        }
    );
}
