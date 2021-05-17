use crate::Span;

use std::fmt::{self, Debug};

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub struct Expr(pub ExprKind, pub Span);

impl Expr {
    pub fn kind(&self) -> &ExprKind {
        &self.0
    }

    pub fn span(&self) -> Span {
        self.1
    }
}

#[derive(Debug, Clone)]
pub struct Name(pub String, pub Span);

impl Name {
    pub fn span(&self) -> Span {
        self.1
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {

    Unit,

    Name(Name),

    Fun(Vec<Name>, Box<Expr>),

    Def(Name, Box<Expr>),

    Set(Box<Expr>, Box<Expr>),

    GetMember(Box<Expr>, Name),

    GetIndex(Box<Expr>, Box<Expr>),

    Mut(Box<Expr>),

    Apply(Box<Expr>, Box<Expr>),

    Unary(Operator, Box<Expr>),

    Binary(Operator, Box<Expr>, Box<Expr>),

    Operator(Operator),

    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Match(Box<Expr>, Vec<(Expr, Expr)>),

    For(Name, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),

    List(Vec<Expr>),

    Dict(Vec<(Expr, Expr)>),

    Block(Vec<Expr>),

    Group(Box<Expr>),

    Variant(Name, Vec<Expr>),

    Number(String),

    String(String),

    Template(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Not,
    Concat,
    BitAnd,
    BitOr,
    BitNot,
    BitXor,
    BitShl,
    BitShr,
    LPipe,
    RPipe,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}