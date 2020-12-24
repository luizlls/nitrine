use crate::Span;

#[derive(Debug, Clone)]
pub struct Name {
    pub name: std::string::String,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Name>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Name { name: Name },

    Lambda { parameters: Vec<Name>, body: Box<Expr> },

    Block { items: Vec<Expr> },

    Group { inner: Box<Expr> },

    Apply { function: Box<Expr>, args: Vec<Expr> },

    Binary { operator: Operator, lhs: Box<Expr>, rhs: Box<Expr> },

    Unary { operator: Operator, rhs: Box<Expr> },

    If { test: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr> },

    Tuple { items: Vec<Expr> },

    List { items: Vec<Expr> },

    Object { base: Option<Name>, props: Vec<(Name, Option<Expr>)> },

    String { value: String },

    Number { value: String },

    Symbol { name: Name, value: Option<Box<Expr>> },
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
    Concat,
    Pipe,
    BitAnd,
    BitOr,
    BitNot,
    BitXor,
    BitShl,
    BitShr,
    Neg,
    Pos,
    Is,
    Not,
    Member,
    Bind,
}
