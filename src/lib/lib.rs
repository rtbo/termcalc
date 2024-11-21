use std::{collections::HashMap, fmt::Display};

mod ast;
pub mod func;
pub mod input;
pub mod lex;
pub mod parse;

use input::{HasSpan, Span};

#[derive(Debug)]
pub enum Error {
    Parse(parse::Error),
    UnknownVar(Span, String),
    UnknownFunc(Span, String),
    FuncArgCount {
        span: Span,
        name: String,
        expected: func::ArgCount,
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
            Error::Parse(err) => err.fmt(f),
            Error::UnknownVar(_, name) => {
                write!(f, "Variable `{name}` is unknown")
            }
            Error::UnknownFunc(_, name) => {
                write!(f, "Function `{name}` is unknown")
            }
            Error::FuncArgCount {
                name,
                expected,
                actual,
                ..
            } => {
                write!(
                    f,
                    "Function `{name}` expects {expected}, but received {actual}"
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

#[derive(Debug, Clone, PartialEq)]
pub struct Eval {
    pub sym: String,
    pub val: f64,
}

#[derive(Debug, Clone)]
pub struct TermCalc {
    vars: HashMap<String, f64>,
    funcs: HashMap<String, func::Func>,
}

impl Default for TermCalc {
    fn default() -> Self {
        Self::new()
    }
}

impl TermCalc {
    pub fn new() -> Self {
        let mut vars = HashMap::new();
        vars.insert("pi".to_string(), std::f64::consts::PI);
        vars.insert("e".to_string(), std::f64::consts::E);
        let funcs = func::all_funcs()
            .into_iter()
            .map(|f| (f.name.clone(), f))
            .collect();
        TermCalc { vars, funcs }
    }

    pub fn get_var(&self, name: &str) -> Option<f64> {
        self.vars.get(name).copied()
    }

    pub fn eval_line<S: AsRef<str>>(&mut self, line: S) -> Result<Eval, Error> {
        let line = line.as_ref();
        let item = parse::parse_line(line.chars())?;
        self.eval_item(item)
    }

    fn eval_item(&mut self, item: ast::Item) -> Result<Eval, Error> {
        let (sym, expr) = match item.kind {
            ast::ItemKind::Assign(sym, expr) => (sym, expr),
            // if item is a single var, we return its name as evaluation symbol
            ast::ItemKind::Expr(ast::Expr {
                span,
                kind: ast::ExprKind::Var(sym),
            }) => (
                sym.clone(),
                ast::Expr {
                    span,
                    kind: ast::ExprKind::Var(sym),
                },
            ),
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
                    ast::BinOp::Pow => Ok(lhs.powf(rhs)),
                }
            }
            ast::ExprKind::UnOp(op, expr) => {
                let val = self.eval_expr(*expr)?;
                match op {
                    ast::UnOp::Plus => Ok(val),
                    ast::UnOp::Minus => Ok(-val),
                }
            }
            ast::ExprKind::Call {
                name_span,
                name,
                args,
            } => self.eval_call(span, name_span, name, args),
        }
    }

    fn eval_call(
        &self,
        span: Span,
        name_span: Span,
        name: String,
        args: Vec<ast::Expr>,
    ) -> Result<f64, Error> {
        let func = match self.funcs.get(&name) {
            Some(f) => f,
            None => return Err(Error::UnknownFunc(name_span, name)),
        };
        let args = self.eval_args(span, func, args)?;
        let f = func.eval;
        Ok(f(args))
    }

    fn eval_args(
        &self,
        span: Span,
        func: &func::Func,
        args: Vec<ast::Expr>,
    ) -> Result<func::Args, Error> {
        if !func.arg_count.check(args.len()) {
            return Err(Error::FuncArgCount {
                span,
                name: func.name.clone(),
                expected: func.arg_count,
                actual: args.len() as _,
            });
        }
        let mut args = args.into_iter();
        Ok(match func.arg_count {
            func::ArgCount::One => {
                let arg = self.eval_expr(args.next().unwrap())?;
                func::Args::One(arg)
            }
            func::ArgCount::Two => {
                let arg1 = self.eval_expr(args.next().unwrap())?;
                let arg2 = self.eval_expr(args.next().unwrap())?;
                func::Args::Two(arg1, arg2)
            }
            func::ArgCount::Atleast(..) => {
                let args = args
                    .map(|e| self.eval_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                func::Args::Dyn(args)
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Eval, TermCalc};
    use approx::{assert_relative_eq, AbsDiffEq, RelativeEq, UlpsEq};

    impl AbsDiffEq for Eval {
        type Epsilon = f64;
        fn default_epsilon() -> Self::Epsilon {
            f64::default_epsilon()
        }
        fn abs_diff_eq(&self, other: &Self, epsilon: Self::Epsilon) -> bool {
            self.sym == other.sym && self.val.abs_diff_eq(&other.val, epsilon)
        }
    }

    impl RelativeEq for Eval {
        fn default_max_relative() -> Self::Epsilon {
            f64::default_max_relative()
        }
        fn relative_eq(
            &self,
            other: &Self,
            epsilon: Self::Epsilon,
            max_relative: Self::Epsilon,
        ) -> bool {
            self.sym == other.sym && self.val.relative_eq(&other.val, epsilon, max_relative)
        }
    }

    impl UlpsEq for Eval {
        fn default_max_ulps() -> u32 {
            f64::default_max_ulps()
        }
        fn ulps_eq(&self, other: &Self, epsilon: Self::Epsilon, max_ulps: u32) -> bool {
            self.sym == other.sym && self.val.ulps_eq(&other.val, epsilon, max_ulps)
        }
    }

    #[test]
    fn test_eval_line() {
        let mut tc = TermCalc::new();
        // integers have perfect precision, no need for relative_eq!
        assert_eq!(
            tc.eval_line("1").unwrap(),
            Eval {
                sym: "ans".to_string(),
                val: 1.0,
            }
        );
        assert_relative_eq!(
            tc.eval_line("sin(pi/2)").unwrap(),
            Eval {
                sym: "ans".to_string(),
                val: 1.0,
            },
            epsilon = f64::EPSILON,
        );
        assert_relative_eq!(
            tc.eval_line("x = cos(pi)").unwrap(),
            Eval {
                sym: "x".to_string(),
                val: -1.0,
            },
            epsilon = f64::EPSILON,
        );
        assert_relative_eq!(
            tc.eval_line("y = x + ans").unwrap(),
            Eval {
                sym: "y".to_string(),
                val: 0.0,
            },
            epsilon = f64::EPSILON,
        );
        assert_relative_eq!(
            tc.eval_line("10 - 1 - 2 - 3").unwrap(),
            Eval {
                sym: "ans".to_string(),
                val: 4.0,
            },
            epsilon = f64::EPSILON,
        );
    }
}
