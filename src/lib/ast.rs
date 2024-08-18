use crate::input::{Pos, Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}


#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Assign(String, Expr),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Num {
        pos: Pos,
        val: f64,
    },
    Var {
        pos: Pos,
        sym: String,
    },
    UnOp {
        pos: Pos,
        op: UnOp,
        rhs: Box<Expr>,
    },
    BinOp {
        pos: Pos,
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        pos: Pos,
        sym: String,
        args: Vec<Expr>,
    },
}
