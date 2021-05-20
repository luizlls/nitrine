use crate::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {

    pub const fn expr(kind: ExprKind, span: Span) -> Expr {
        Expr { kind, span }
    }

    pub const fn name(value: String, span: Span) -> Name {
        Name { value, span }
    }
}


#[derive(Debug, Clone)]
pub enum ExprKind {

    Unit,

    Name(Name),

    Fun(Fun),

    Def(Def),

    Set(Set),

    Mut(Mut),

    Member(Member),

    Index(Index),

    Apply(Apply),

    Unary(Unary),

    Binary(Binary),

    Partial(Partial),

    If(If),

    Match(Match),

    For(For),

    Tuple(Tuple),

    List(List),

    Dict(Dict),

    Block(Block),

    Group(Group),

    Number(String),

    String(String),

    Template(Template),

    Variant(Variant),

    True,

    False,

    Any, // `_` wildcard
}


#[derive(Debug, Clone)]
pub struct Name {
    pub value: String,
    pub span: Span,
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
pub struct Mut {
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub source: Box<Expr>,
    pub member: Name,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub source: Box<Expr>,
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
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Partial {
    pub op: Operator,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub test: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub target: Name,
    pub source: Box<Expr>,
    pub value:  Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub value: Box<Expr>,
    pub cases: Vec<(Expr, Expr)>,
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
    pub entries: Vec<(Expr, Expr)>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Group {
    pub inner: Box<Expr>,
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

impl ExprKind {
    pub const fn name(value: String, span: Span) -> ExprKind {
        ExprKind::Name(Name { value, span })
    }

    pub const fn function(params: Vec<Name>, value: Box<Expr>) -> ExprKind {
        ExprKind::Fun(Fun { params, value })
    }

    pub const fn def(name: Name, value: Box<Expr>) -> ExprKind {
        ExprKind::Def(Def { name, value })
    }

    pub const fn set(target: Box<Expr>, value: Box<Expr>) -> ExprKind {
        ExprKind::Set(Set { target, value })
    }

    pub const fn mutable(value: Box<Expr>) -> ExprKind {
        ExprKind::Mut(Mut { value })
    }

    pub const fn member(source: Box<Expr>, member: Name) -> ExprKind {
        ExprKind::Member(Member { source, member })
    }

    pub const fn index(source: Box<Expr>, index: Box<Expr>) -> ExprKind {
        ExprKind::Index(Index { source, index })
    }

    pub const fn apply(fun: Box<Expr>, arg: Box<Expr>) -> ExprKind {
        ExprKind::Apply(Apply { fun, arg })
    }

    pub const fn unary(op: Operator, rhs: Box<Expr>) -> ExprKind {
        ExprKind::Unary(Unary { op, rhs })
    }

    pub const fn binary(op: Operator, lhs: Box<Expr>, rhs: Box<Expr>) -> ExprKind {
        ExprKind::Binary(Binary { op, lhs, rhs })
    }

    pub const fn partial(op: Operator, expr: Option<Box<Expr>>) -> ExprKind {
        ExprKind::Partial(Partial { op, expr })
    }

    pub const fn tuple(items: Vec<Expr>) -> ExprKind {
        ExprKind::Tuple(Tuple { items })
    }

    pub const fn list(items: Vec<Expr>) -> ExprKind {
        ExprKind::List(List { items })
    }

    pub const fn dict(items: Vec<(Expr, Expr)>) -> ExprKind {
        ExprKind::Dict(Dict { entries: items })
    }

    pub const fn block(items: Vec<Expr>) -> ExprKind {
        ExprKind::Block(Block { items })
    }

    pub const fn group(inner: Box<Expr>) -> ExprKind {
        ExprKind::Group(Group { inner })
    }

    pub const fn template(elements: Vec<Expr>) -> ExprKind {
        ExprKind::Template(Template { elements })
    }

    pub const fn variant(name: Name, values: Vec<Expr>) -> ExprKind {
        ExprKind::Variant(Variant { name, values })
    }

    pub const fn if_(test: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr>) -> ExprKind {
        ExprKind::If(If { test, then, otherwise })
    }

    pub const fn for_(target: Name, source: Box<Expr>, value:  Box<Expr>) -> ExprKind {
        ExprKind::For(For { target, source, value })
    }

    pub const fn match_(value: Box<Expr>, cases: Vec<(Expr, Expr)>) -> ExprKind {
        ExprKind::Match(Match { value, cases })
    }

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


impl Operator {
    pub const fn unary(&self) -> bool {
        matches!(self, Operator::Not | Operator::BitNot)
    }
}