use crate::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub name: String,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Item {
    pub name: Name,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function { parameters: Vec<Name>, value: Expr },

    Constant { value: Expr }
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

    String { value: String },

    Number { value: String },

    Template { parts: Vec<Expr> },

    Symbol { name: Name, value: Option<Box<Expr>> },

    Tuple { items: Vec<Expr> },

    List { items: Vec<Expr> },

    Object { base: Option<Name>, props: Vec<(Name, Option<Expr>)> },
}


#[derive(Debug, Clone)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum OperatorKind {
    Bind,
    Neg,
    Pos,
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
    Member,
}
