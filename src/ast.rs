use crate::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub enum Expr {

    Name(Name),

    Fun(Fn),

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

    Tuple(Tuple),

    List(List),

    Dict(Dict),

    Block(Block),

    Group(Group),

    Number(Literal),

    String(Literal),

    Template(Template),

    Variant(Variant),

    True(Span),

    False(Span),

    Any(Span), // `_` wildcard

    Unit(Span),
}


#[derive(Debug, Clone)]
pub struct Name {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub params: Vec<Name>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: Name,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Mut {
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub source: Box<Expr>,
    pub member: Name,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub source: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub fun: Box<Expr>,
    pub arg: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Operator,
    pub rhs: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Partial {
    pub op: Operator,
    pub expr: Option<Box<Expr>>,
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
pub struct Match {
    pub value: Box<Expr>,
    pub cases: Vec<(Expr, Expr)>,
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
pub struct Dict {
    pub entries: Vec<(Expr, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Group {
    pub inner: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}


#[derive(Debug, Clone)]
pub struct Template {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Name,
    pub values: Vec<Expr>,
    pub span: Span,
}

impl Expr {
    pub const fn name(value: String, span: Span) -> Expr {
        Expr::Name(Name { value, span })
    }

    pub const fn function(params: Vec<Name>, value: Box<Expr>, span: Span) -> Expr {
        Expr::Fun(Fn { params, value, span })
    }

    pub const fn def(name: Name, value: Box<Expr>, span: Span) -> Expr {
        Expr::Def(Def { name, value, span })
    }

    pub const fn set(target: Box<Expr>, value: Box<Expr>, span: Span) -> Expr {
        Expr::Set(Set { target, value, span })
    }

    pub const fn mutable(value: Box<Expr>, span: Span) -> Expr {
        Expr::Mut(Mut { value, span })
    }

    pub const fn member(source: Box<Expr>, member: Name, span: Span) -> Expr {
        Expr::Member(Member { source, member, span })
    }

    pub const fn index(source: Box<Expr>, index: Box<Expr>, span: Span) -> Expr {
        Expr::Index(Index { source, index, span })
    }

    pub const fn apply(fun: Box<Expr>, arg: Box<Expr>, span: Span) -> Expr {
        Expr::Apply(Apply { fun, arg, span })
    }

    pub const fn unary(op: Operator, rhs: Box<Expr>, span: Span) -> Expr {
        Expr::Unary(Unary { op, rhs, span })
    }

    pub const fn binary(op: Operator, lhs: Box<Expr>, rhs: Box<Expr>, span: Span) -> Expr {
        Expr::Binary(Binary { op, lhs, rhs, span })
    }

    pub const fn partial(op: Operator, expr: Option<Box<Expr>>, span: Span) -> Expr {
        Expr::Partial(Partial { op, expr, span })
    }

    pub const fn tuple(items: Vec<Expr>, span: Span) -> Expr {
        Expr::Tuple(Tuple { items, span })
    }

    pub const fn list(items: Vec<Expr>, span: Span) -> Expr {
        Expr::List(List { items, span })
    }

    pub const fn dict(items: Vec<(Expr, Expr)>, span: Span) -> Expr {
        Expr::Dict(Dict { entries: items, span })
    }

    pub const fn block(items: Vec<Expr>, span: Span) -> Expr {
        Expr::Block(Block { items, span })
    }

    pub const fn group(inner: Box<Expr>, span: Span) -> Expr {
        Expr::Group(Group { inner, span })
    }

    pub const fn template(elements: Vec<Expr>, span: Span) -> Expr {
        Expr::Template(Template { elements, span })
    }

    pub const fn number(value: String, span: Span) -> Expr {
        Expr::Number(Literal { value, span })
    }

    pub const fn string(value: String, span: Span) -> Expr {
        Expr::String(Literal { value, span })
    }

    pub const fn variant(name: Name, values: Vec<Expr>, span: Span) -> Expr {
        Expr::Variant(Variant { name, values, span })
    }

    pub const fn conditional(test: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr>, span: Span) -> Expr {
        Expr::If(If { test, then, otherwise, span })
    }

    pub const fn pattern_match(value: Box<Expr>, cases: Vec<(Expr, Expr)>, span: Span) -> Expr {
        Expr::Match(Match { value, cases, span })
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Name(expr) => expr.span,
            Expr::Fun(expr) => expr.span,
            Expr::Def(expr) => expr.span,
            Expr::Set(expr) => expr.span,
            Expr::Mut(expr) => expr.span,
            Expr::Member(expr) => expr.span,
            Expr::Index(expr) => expr.span,
            Expr::Apply(expr) => expr.span,
            Expr::Unary(expr) => expr.span,
            Expr::Binary(expr) => expr.span,
            Expr::Partial(expr) => expr.span,
            Expr::If(expr) => expr.span,
            Expr::Match(expr) => expr.span,
            Expr::Tuple(expr) => expr.span,
            Expr::List(expr) => expr.span,
            Expr::Dict(expr) => expr.span,
            Expr::Block(expr) => expr.span,
            Expr::Group(expr) => expr.span,
            Expr::Number(expr) => expr.span,
            Expr::String(expr) => expr.span,
            Expr::Template(expr) => expr.span,
            Expr::Variant(expr) => expr.span,
            Expr::True(span) => *span,
            Expr::False(span) => *span,
            Expr::Any(span) => *span,
            Expr::Unit(span) => *span,
        }
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