use crate::Span;

use std::fmt::{self, Debug};

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {

    Unit,

    Name(Name),

    Fun(Fun),

    Def(Def),

    Set(Set),

    GetMember(GetMember),

    GetIndex(GetIndex),

    Apply(Apply),

    Unary(Unary),

    Binary(Binary),

    Block(Block),

    If(If),

    Match(Match),

    For(For),

    Tuple(Tuple),

    List(List),

    Dict(Dict),

    Variant(Variant),

    Number(String),

    String(String),

    Template(Template),
}

#[derive(Debug, Clone)]
pub struct Name {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub params: Vec<Name>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: Name,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct GetMember {
    pub expr: Box<Expr>,
    pub name: Name,
}

#[derive(Debug, Clone)]
pub struct GetIndex {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub fun: Box<Expr>,
    pub arg: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Operator,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: Operator,
    pub lexpr: Box<Expr>,
    pub rexpr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub test: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub value: Box<Expr>,
    pub cases: Vec<(Expr, Expr)>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub target: Box<Expr>,
    pub source: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub items: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct List {
    pub items: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Dict {
    pub items: Vec<(Expr, Expr)>,
}

#[derive(Debug, Clone)]
pub struct Template {
    pub elements: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Name,
    pub values: Vec<Expr>,
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