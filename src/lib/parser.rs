//! Parser module
//!
//! See `Grammar.ebnf`
//! ```
use crate::ast;
use crate::input::Span;
use crate::lex::{self, Token, TokenKind};
use crate::util;

#[derive(Debug)]
pub enum Error {
    Lex(lex::Error),
    UnexpectedEndOfInput,
    UnexpectedToken(Token, Option<TokenKind>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Lex(e) => e.fmt(f),
            Error::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            Error::UnexpectedToken(t, k) => match k {
                Some(k) => write!(f, "Unexpected token: {:?} (expected {:?})", t, k),
                None => write!(f, "Unexpected token: {:?}", t),
            },
        }
    }
}

impl std::error::Error for Error {}

impl From<lex::Error> for Error {
    fn from(e: lex::Error) -> Self {
        Error::Lex(e)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

/// Parse the given string to produce AST items
pub fn parse<I>(chars: I) -> impl Iterator<Item = Result<ast::Item>>
where
    I: IntoIterator<Item = char>,
{
    let tokens = lex::tokenize(chars).in_band();
    Parser::new(tokens).items()
}

#[derive(Debug, Clone)]
pub struct Parser<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    tokens: util::PutBack<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    pub fn new(tokens: T) -> Parser<T> {
        Parser {
            tokens: util::put_back(tokens),
        }
    }

    pub fn items(self) -> Items<T> {
        Items { inner: self }
    }
}

impl<T> Parser<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    fn expect_kind(&mut self, kind: TokenKind) -> Result<Span> {
        let tok = self.tokens.next().transpose()?;
        if let Some(tok) = tok {
            if tok.kind == kind {
                Ok(tok.span)
            } else {
                Err(Error::UnexpectedToken(tok, Some(kind)))
            }
        } else {
            Err(Error::UnexpectedEndOfInput)
        }
    }

    fn next_is_kind(&mut self, kind: TokenKind) -> Result<bool> {
        let tok = self.tokens.next().transpose()?;
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
    fn parse_item(&mut self) -> Result<ast::Item> {
        let mut tok = self.tokens.next().transpose()?;
        while matches!(
            tok,
            Some(Token {
                kind: TokenKind::Space,
                ..
            })
        ) {
            tok = self.tokens.next().transpose()?;
        }
        match tok {
            Some(Token {
                kind: TokenKind::Symbol(sym),
                span,
            }) => {
                if self.next_is_kind(TokenKind::Equal)? {
                    self.tokens.next().transpose()?; // eat '='
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
            _ => todo!(),
        }
    }

    fn parse_expr(&mut self) -> Result<ast::Expr> {
        self.parse_add_expr()
    }

    fn parse_add_expr(&mut self) -> Result<ast::Expr> {
        let lhs = self.parse_mul_expr()?;
        let tok = self.tokens.next().transpose()?;
        match tok {
            Some(Token { kind, .. }) if is_add_op(&kind) => {
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

    fn parse_mul_expr(&mut self) -> Result<ast::Expr> {
        let lhs = self.parse_unary_expr()?;
        let tok = self.tokens.next().transpose()?;
        match tok {
            Some(Token { kind, .. }) if is_mul_op(&kind) => {
                let rhs = self.parse_unary_expr()?;
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
        let tok = self.tokens.next().transpose()?;
        match tok {
            Some(Token { kind, span }) if is_un_op(&kind) => {
                let expr = self.parse_primary()?;
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
                Ok(self.parse_primary()?)
            }
        }
    }

    fn parse_primary(&mut self) -> Result<ast::Expr> {
        let tok = self.tokens.next().transpose()?;
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
                span,
            }) => {
                let next = self.tokens.next().transpose()?;
                match next {
                    Some(Token {
                        kind: TokenKind::OpenPar,
                        span: openspan,
                    }) => {
                        let args = self.parse_arg_list()?;
                        let closespan = self.expect_kind(TokenKind::ClosePar)?;
                        let span = (openspan.0, closespan.1);
                        Ok(ast::Expr {
                            kind: ast::ExprKind::Call(sym, args),
                            span,
                        })
                    }
                    _ => Ok(ast::Expr {
                        kind: ast::ExprKind::Var(sym),
                        span,
                    }),
                }
            }
            Some(tok) => Err(Error::UnexpectedToken(tok, None)),
            None => Err(Error::UnexpectedEndOfInput),
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<ast::Expr>> {
        let mut args = Vec::new();
        loop {
            let tok = self.tokens.next().transpose()?;
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
                None => return Err(Error::UnexpectedEndOfInput),
            }
        }
        Ok(args)
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

#[derive(Debug, Clone)]
pub struct Items<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    inner: Parser<T>,
}

impl<T> Iterator for Items<T>
where
    T: Iterator<Item = lex::Result<Token>>,
{
    type Item = Result<ast::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.inner.tokens.next()?;
        match tok {
            Ok(tok) => {
                self.inner.tokens.put_back(Ok(tok));
                Some(self.inner.parse_item())
            }
            Err(err) => Some(Err(err.into())),
        }
    }
}

#[test]
fn test_parse_add() {
    let items: Vec<ast::Item> = parse("1 + 2".chars()).map(Result::unwrap).collect();

    assert_eq!(
        items,
        vec![ast::Item {
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
        }]
    )
}

#[test]
fn test_parse_add_mul() {
    let items: Vec<ast::Item> = parse("1 + 2 * 3".chars()).map(Result::unwrap).collect();

    assert_eq!(
        items,
        vec![ast::Item {
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
        }]
    )
}

#[test]
fn test_parse_mul_add() {
    let items: Vec<ast::Item> = parse("1 * 2 + 3".chars()).map(Result::unwrap).collect();

    assert_eq!(
        items,
        vec![ast::Item {
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
        }]
    )
}

#[test]
fn test_parse_mul_add_parentheses() {
    let items: Vec<ast::Item> = parse("1 * (2 + 3)".chars()).map(Result::unwrap).collect();

    assert_eq!(
        items,
        vec![ast::Item {
            span: (0, 11),
            kind: ast::ItemKind::Expr(ast::Expr {
                span: (0, 11),
                kind: ast::ExprKind::BinOp(
                    ast::BinOp::Mul,
                    Box::new(ast::Expr{
                        span: (0, 1),
                        kind: ast::ExprKind::Num(1.0),
                    } ),
                    Box::new(ast::Expr{
                        span: (4, 11),
                        kind: ast::ExprKind::BinOp(
                            ast::BinOp::Add,
                            Box::new(ast::Expr{
                                span: (5, 6),
                                kind: ast::ExprKind::Num(2.0),
                            }),
                            Box::new(ast::Expr{
                                span: (9, 10),
                                kind: ast::ExprKind::Num(3.0),
                            })
                        )
                    })
                )
            })
        }]
    )
}
