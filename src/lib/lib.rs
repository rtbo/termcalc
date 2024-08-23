use std::{collections::HashMap, fmt::Display};

mod ast;
pub mod input;
pub mod lex;
pub mod parse;
mod util;

use input::{HasSpan, Span};

#[derive(Debug)]
pub enum Error {
    Parse(parse::Error),
    UnknownVar(Span, String),
    UnknownFunc(Span, String),
    FuncArgCount {
        span: Span,
        name: String,
        expected: u32,
        actual: u32,
    },
    ZeroDiv(Span),
}

impl HasSpan for Error {
    fn span(&self) -> Span {
        match self {
            Error::Parse(err) => err.span(),
            Error::UnknownVar(span, _) => *span,
            Error::UnknownFunc(span, _) => *span,
            Error::FuncArgCount { span, .. } => *span,
            Error::ZeroDiv(span) => *span,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(err) => {
                err.fmt(f)
            }
            Error::UnknownVar(_, name) => {
                write!(f, "Unknown variable: {name}")
            }
            Error::UnknownFunc(_, name) => {
                write!(f, "Unknown function: {name}")
            }
            Error::FuncArgCount {
                name,
                expected,
                actual,
                ..
            } => {
                write!(
                    f,
                    "Function {name} expected {expected} argument{}, but got {actual}",
                    if *expected > 1 { "s" } else { "" }
                )
            }
            Error::ZeroDiv(_) => {
                write!(f, "Division by zero")
            }
        }
    }
}

impl From<parse::Error> for Error {
    fn from(e: parse::Error) -> Self {
        Error::Parse(e)
    }
}

#[derive(Debug)]
pub struct Eval {
    pub sym: String,
    pub val: f64,
}

#[derive(Debug)]
pub struct TermCalc {
    vars: HashMap<String, f64>,
}

impl TermCalc {
    pub fn new() -> Self {
        let mut vars = HashMap::new();
        vars.insert("pi".to_string(), std::f64::consts::PI);
        TermCalc { vars }
    }

    pub fn eval_line<S: AsRef<str>>(&mut self, line: S) -> Result<Eval, Error> {
        let line = line.as_ref();
        let item = parse::parse_line(line.chars())?;
        self.eval_item(item)
    }

    fn eval_item(&mut self, item: ast::Item) -> Result<Eval, Error> {
        let (sym, expr) = match item.kind {
            ast::ItemKind::Assign(sym, expr) => (sym, expr),
            ast::ItemKind::Expr(expr) => ("ans".to_string(), expr),
        };

                let val = self.eval_expr(expr)?;
                self.vars.insert(sym.clone(), val);
        Ok(Eval { sym, val })
    }

    fn eval_expr(&self, expr: ast::Expr) -> Result<f64, Error> {
        let ast::Expr { span, kind } = expr;
        match kind {
            ast::ExprKind::Num(n) => Ok(n),
            ast::ExprKind::Var(s) => match self.vars.get(&s) {
                Some(n) => Ok(*n),
                None => Err(Error::UnknownVar(span, s)),
            },
            ast::ExprKind::BinOp(op, lhs, rhs) => {
                let lhs = self.eval_expr(*lhs)?;
                let rhs = self.eval_expr(*rhs)?;
                match op {
                    ast::BinOp::Add => Ok(lhs + rhs),
                    ast::BinOp::Sub => Ok(lhs - rhs),
                    ast::BinOp::Mul => Ok(lhs * rhs),
                    ast::BinOp::Div if rhs == 0.0 => Err(Error::ZeroDiv(span)),
                    ast::BinOp::Div => Ok(lhs / rhs),
                    ast::BinOp::Mod if rhs == 0.0 => Err(Error::ZeroDiv(span)),
                    ast::BinOp::Mod => Ok(lhs % rhs),
                }
            }
            ast::ExprKind::UnOp(op, expr) => {
                let val = self.eval_expr(*expr)?;
                match op {
                    ast::UnOp::Plus => Ok(val),
                    ast::UnOp::Minus => Ok(-val),
                }
            }
            ast::ExprKind::Call(func, args) => self.eval_call(span, func, args),
        }
    }

    fn eval_call(&self, span: Span, func: String, args: Vec<ast::Expr>) -> Result<f64, Error> {
        match func.as_str() {
            // trivial
            "floor" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.floor())
            }
            "ceil" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.ceil())
            }
            "round" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.round())
            }
            "trunc" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.trunc())
            }
            "fract" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.fract())
            }
            "abs" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.abs())
            }
            "sign" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.signum())
            }
            "min" => {
                let (arg1, arg2) = expect_2_args(span, func, args)?;
                let arg1 = self.eval_expr(arg1)?;
                let arg2 = self.eval_expr(arg2)?;
                Ok(arg1.min(arg2))
            }
            "max" => {
                let (arg1, arg2) = expect_2_args(span, func, args)?;
                let arg1 = self.eval_expr(arg1)?;
                let arg2 = self.eval_expr(arg2)?;
                Ok(arg1.max(arg2))
            }
            // power and log
            "pow" => {
                let (arg1, arg2) = expect_2_args(span, func, args)?;
                let arg1 = self.eval_expr(arg1)?;
                let arg2 = self.eval_expr(arg2)?;
                Ok(arg1.powf(arg2))
            }
            "sqrt" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.sqrt())
            }
            "cbrt" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.cbrt())
            }
            "exp" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.exp())
            }
            "ln" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.ln())
            }
            "log2" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.log2())
            }
            "log10" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.log10())
            }
            "log" => {
                let (arg1, arg2) = expect_2_args(span, func, args)?;
                let arg1 = self.eval_expr(arg1)?;
                let arg2 = self.eval_expr(arg2)?;
                Ok(arg1.log(arg2))
            }
            // trigonometric
            "sin" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.sin())
            }
            "sinh" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.sinh())
            }
            "asin" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.asin())
            }
            "asinh" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.asinh())
            }
            "cos" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.cos())
            }
            "cosh" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.cosh())
            }
            "acos" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.acos())
            }
            "acosh" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.acosh())
            }
            "tan" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.tan())
            }
            "tanh" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.tanh())
            }
            "atan" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.atan())
            }
            "atanh" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.atanh())
            }
            "degrees" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.to_degrees())
            }
            "radians" => {
                let arg = self.eval_expr(expect_1_arg(span, func, args)?)?;
                Ok(arg.to_radians())
            }
            _ => Err(Error::UnknownFunc(span, func)),
        }
    }
}

fn expect_1_arg(span: Span, func: String, args: Vec<ast::Expr>) -> Result<ast::Expr, Error> {
    let mut args = args;
    if args.len() == 1 {
        Ok(args.pop().unwrap())
    } else {
        Err(Error::FuncArgCount {
            span,
            name: func,
            expected: 1,
            actual: args.len() as _,
        })
    }
}

fn expect_2_args(
    span: Span,
    func: String,
    args: Vec<ast::Expr>,
) -> Result<(ast::Expr, ast::Expr), Error> {
    let mut args = args;
    if args.len() == 2 {
        let arg2 = args.pop().unwrap();
        let arg1 = args.pop().unwrap();
        Ok((arg1, arg2))
    } else {
        Err(Error::FuncArgCount {
            span,
            name: func,
            expected: 2,
            actual: args.len() as _,
        })
    }
}
