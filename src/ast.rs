use crate::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub enum Expr {

    Name(Name),

    Fn(Fn),

    Def(Def),

    Bind(Bind),

    Set(Set),

    Mut(Mut),

    Get(Get),

    Unary(Unary),

    Binary(Binary),

    Call(Call),

    Partial(Partial),

    If(If),

    Match(Match),

    Tuple(Tuple),

    List(List),

    Record(Record),

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

    Raise(Raise),
}


#[derive(Debug, Clone)]
pub struct Name {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub params: Vec<Name>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: Name,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Bind {
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
pub struct Get {
    pub source: Box<Expr>,
    pub name: Name,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub source: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub fun: Box<Expr>,
    pub args: Vec<Expr>,
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
    pub lhs: Option<Box<Expr>>,
    pub rhs: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub predicate: Box<Expr>,
    pub positive: Box<Expr>,
    pub negative: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub predicate: Box<Expr>,
    pub cases: Vec<(Expr, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub items: Vec<Expr>,
    pub rest: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct List {
    pub items: Vec<Expr>,
    pub rest: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub items: Vec<(Name, Expr)>,
    pub rest: Option<Box<Expr>>,
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

#[derive(Debug, Clone)]
pub struct Raise {
    pub value: Box<Expr>,
    pub span: Span,
}


impl Name {
    pub const fn new(value: String, span: Span) -> Name {
        Name { value, span }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Name(expr) => expr.span,
            Expr::Fn(expr) => expr.span,
            Expr::Def(expr) => expr.span,
            Expr::Bind(expr) => expr.span,
            Expr::Set(expr) => expr.span,
            Expr::Mut(expr) => expr.span,
            Expr::Get(expr) => expr.span,
            Expr::Call(expr) => expr.span,
            Expr::Unary(expr) => expr.span,
            Expr::Binary(expr) => expr.span,
            Expr::Partial(expr) => expr.span,
            Expr::If(expr) => expr.span,
            Expr::Match(expr) => expr.span,
            Expr::Tuple(expr) => expr.span,
            Expr::List(expr) => expr.span,
            Expr::Record(expr) => expr.span,
            Expr::Block(expr) => expr.span,
            Expr::Group(expr) => expr.span,
            Expr::Number(expr) => expr.span,
            Expr::String(expr) => expr.span,
            Expr::Template(expr) => expr.span,
            Expr::Variant(expr) => expr.span,
            Expr::Raise(expr) => expr.span,
            Expr::True(span) => *span,
            Expr::False(span) => *span,
            Expr::Any(span) => *span,
            Expr::Unit(span) => *span,
        }
    }

    pub fn display_name(&self) -> &str {
        match self {
            Expr::Name(_) => "name",
            Expr::Fn(_) => "fun",
            Expr::Def(_) => "def",
            Expr::Bind(_) => "bind",
            Expr::Set(_) => "set",
            Expr::Mut(_) => "mut",
            Expr::Get(_) => "get",
            Expr::Call(_) => "apply",
            Expr::Unary(_) => "unary",
            Expr::Binary(_) => "binary",
            Expr::Partial(_) => "partial",
            Expr::If(_) => "if",
            Expr::Match(_) => "match",
            Expr::Tuple(_) => "tuple",
            Expr::List(_) => "list",
            Expr::Record(_) => "record",
            Expr::Block(_) => "block",
            Expr::Group(_) => "group",
            Expr::Number(_) => "number",
            Expr::String(_) => "string",
            Expr::Template(_) => "template",
            Expr::Variant(_) => "variant",
            Expr::True(_) => "true",
            Expr::False(_) => "false",
            Expr::Any(_) => "any",
            Expr::Unit(_) => "unit",
            Expr::Raise(_) => "raise",
        }
    }

    pub fn is_pattern(&self) -> bool {
        matches!(self,
            Expr::Name(_)
          | Expr::Tuple(_)
          | Expr::List(_)
          | Expr::Record(_)
          | Expr::Number(_)
          | Expr::String(_)
          | Expr::Variant(_)
          | Expr::Group(_)
          | Expr::True(_)
          | Expr::False(_)
          | Expr::Any(_)
          | Expr::Unit(_))
    }

    pub fn extract_grouped(self) -> Expr {
        if let Expr::Group(Group { box inner, .. }) = self {
            inner.extract_grouped()
        } else {
            self
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Is,
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

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Operator::And => write!(f, "and"),
            Operator::Or => write!(f, "or"),
            Operator::Is => write!(f, "is"),
            Operator::Not => write!(f, "not"),
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Rem => write!(f, "%"),
            Operator::Eq => write!(f, "=="),
            Operator::Ne => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Le => write!(f, "<="),
            Operator::Gt => write!(f, ">"),
            Operator::Ge => write!(f, ">="),
            Operator::Concat => write!(f, "++"),
            Operator::BitAnd => write!(f, "&&&"),
            Operator::BitOr  => write!(f, "|||"),
            Operator::BitNot => write!(f, "~~~"),
            Operator::BitXor => write!(f, "^^^"),
            Operator::BitShr => write!(f, ">>>"),
            Operator::BitShl => write!(f, "<<<"),
            Operator::LPipe => write!(f, "|>"),
            Operator::RPipe => write!(f, "<|"),
        }
    }
}


impl Operator {
    pub const fn unary(&self) -> bool {
        matches!(self, Operator::Not | Operator::BitNot)
    }
}