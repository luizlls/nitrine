use std::fmt::Debug;

use crate::{Span, token::TokenKind};

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub nodes: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {

    Unit(Span),

    Name(Name),

    Operator(Operator),

    Fn(Box<Fn>),

    Let(Box<Let>),

    Mut(Box<Mut>),

    Apply(Box<Apply>),

    Unary(Box<Unary>),

    Binary(Box<Binary>),

    If(Box<If>),

    Tuple(Tuple),

    List(List),

    Record(Record),

    Member(Box<Member>),

    Symbol(Symbol),

    Integer(Literal<i64>),

    Float(Literal<f64>),

    String(Literal<String>),

    Template(Template),
}

#[derive(Debug, Clone)]
pub struct Name {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Operator {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub parameters: Vec<Name>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub patt: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Mut {
    pub patt: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub function: Expr,
    pub arguments: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub rhs: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub lhs: Expr,
    pub rhs: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub test: Expr,
    pub then: Expr,
    pub otherwise: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub items: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct List {
    pub items: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub properties: Vec<(Name, Option<Expr>)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub expr: Expr,
    pub name: Name,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Literal<T: Debug + Clone> {
    pub value: T,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Template {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: Name,
    pub values: Vec<Expr>,
    pub span: Span,
}


impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Name(expr) => expr.span,
            Expr::Fn(expr) => expr.span,
            Expr::Operator(expr) => expr.span,
            Expr::Let(expr) => expr.span,
            Expr::Mut(expr) => expr.span,
            Expr::Apply(expr) => expr.span,
            Expr::Unary(expr) => expr.span,
            Expr::Binary(expr) => expr.span,
            Expr::If(expr) => expr.span,
            Expr::Tuple(expr) => expr.span,
            Expr::List(expr) => expr.span,
            Expr::Record(expr) => expr.span,
            Expr::Member(expr) => expr.span,
            Expr::Symbol(expr) => expr.span,
            Expr::Integer(expr) => expr.span,
            Expr::Float(expr) => expr.span,
            Expr::String(expr) => expr.span,
            Expr::Template(expr) => expr.span,
            Expr::Unit(span) => *span,
        }
    }
}