use crate::Span;

use std::fmt::{self, Debug};

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub enum Expr {

    Unit(Span),

    Name(Name),

    Fun(Fun),

    Let(Let),

    Mut(Mut),

    Apply(Apply),

    Unary(Unary),

    Binary(Binary),

    Block(Block),

    If(If),

    Tuple(Tuple),

    List(List),

    Record(Record),

    Member(Member),

    Symbol(Symbol),

    Integer(Literal<i64>),

    Number(Literal<f64>),

    String(Literal<String>),

    Template(Template),
}

#[derive(Debug, Clone)]
pub struct Name {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: Name,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub args: Vec<Name>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub patt: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Mut {
    pub patt: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub fun: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Operator,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: Operator,
    pub lexpr: Box<Expr>,
    pub rexpr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub test: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Box<Expr>,
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
    pub properties: Vec<(Name, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub expr: Box<Expr>,
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
            Expr::Fun(expr) => expr.span,
            Expr::Let(expr) => expr.span,
            Expr::Mut(expr) => expr.span,
            Expr::Apply(expr) => expr.span,
            Expr::Unary(expr) => expr.span,
            Expr::Binary(expr) => expr.span,
            Expr::Block(expr) => expr.span,
            Expr::If(expr) => expr.span,
            Expr::Tuple(expr) => expr.span,
            Expr::List(expr) => expr.span,
            Expr::Record(expr) => expr.span,
            Expr::Member(expr) => expr.span,
            Expr::Symbol(expr) => expr.span,
            Expr::Integer(expr) => expr.span,
            Expr::Number(expr) => expr.span,
            Expr::String(expr) => expr.span,
            Expr::Template(expr) => expr.span,
            Expr::Unit(span) => *span,
        }
    }
}


#[derive(Debug, Clone)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum OperatorKind {
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
    Is,
    IsNot,
    Pipe,
    Concat,
    BitAnd,
    BitOr,
    BitNot,
    BitXor,
    BitShl,
    BitShr,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}