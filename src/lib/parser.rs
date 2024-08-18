//! Parser module
//! 
//! See `Grammar.ebnf`
//! ```
use crate::ast;
use crate::input::Pos;
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

struct Parser<T>
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
        while matches!(tok, Some(Token{kind: TokenKind::Space, ..})) {
            tok = self.tokens.next().transpose()?;
        }
        match tok {
            Some(Token{kind: TokenKind::Symbol(sym), span}) => {
                if self.next_is_kind(TokenKind::Equal)? {
                    self.tokens.next().transpose()?; // eat '='
                    let expr = self.parse_expr()?;
                    let span = (span.0, expr.span.1);
                    Ok(ast::Item{
                        kind: ast::ItemKind::Assign ( sym, expr ),
                        span,
                    })
                } else {
                    self.tokens.put_back(Ok(Token{kind: TokenKind::Symbol(sym), span}));
                    let expr = self.parse_expr()?;
                    let span = expr.span;
                    Ok(ast::Item{
                        kind: ast::ItemKind::Expr(expr),
                        span,
                    })
                }
            }
            Some(tok) => {
                self.tokens.put_back(Ok(tok));
                let expr = self.parse_expr()?;
                let span = expr.span;
                Ok(ast::Item{
                    kind: ast::ItemKind::Expr(expr),
                    span,
                })
            }
            _ => todo!()
        }
    }


    // fn parse_var_or_call(&mut self, pos: Pos, sym: String) -> Result<ast::Expr> {
    //     let tok = self.tokens.next().transpose()?;
    //     if let Some(Token::LPar(p)) = tok {
    //         let mut args = Vec::new();
    //         loop {
    //             let tok = self.tokens.next().transpose()?;
    //             match tok {
    //                 Some(Token::RPar(_)) => break,
    //                 Some(Token::Comma(_)) => (),
    //                 Some(tok) => args.push(self.parse_expr()?),
    //                 None => return Err(Error::UnexpectedEndOfInput),
    //             }
    //         }
    //         Ok(ast::Expr::Call { pos, sym, args })
    //     } else {
    //         if let Some(tok) = tok {
    //             self.tokens.put_back(Ok(tok));
    //         }
    //         Ok(ast::Expr::Var { pos, sym })
    //     }
    // }

    // fn parse_par_expr(&mut self, pos: Pos) -> Result<ast::Expr> {
    //     let expr = self.parse_expr()?;
    //     let end = self.expect_token(TokenKind::RPar)?;
    //     Ok(ast::Expr::ParExpr {
    //         pos,
    //         end,
    //         expr: Box::new(expr),
    //     })
    // }

    // fn parse_prim(&mut self) -> Result<ast::Expr> {
    //     let tok = self.tokens.next().transpose()?;
    //     match tok {
    //         Some(Token::Num(pos, val)) => Ok(ast::Expr::Num { pos, val }),
    //         Some(Token::Sym(pos, sym)) => self.parse_var_or_call(pos, sym),
    //         Some(tok) => Err(Error::UnexpectedToken(tok, None)),
    //         None => Err(Error::UnexpectedEndOfInput),
    //     }
    // }

    fn parse_expr(&mut self) -> Result<ast::Expr> {
        todo!()
    }
}
