use crate::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub name: String,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span
}

#[derive(Debug, Clone)]
pub enum NodeKind {

    Function { parameters: Vec<Name>, value: Box<Node> },

    Let { name: Name, value: Box<Node> },

    Apply { function: Box<Node>, arguments: Vec<Node> },

    Name { name: Name },

    Unary { operator: Operator, rhs: Box<Node> },

    Binary { operator: Operator, lhs: Box<Node>, rhs: Box<Node> },

    Block { items: Vec<Node> },

    If { test: Box<Node>, then: Box<Node>, otherwise: Box<Node> },

    Symbol { name: Name },

    Tuple { items: Vec<Node> },

    List { items: Vec<Node> },

    Object { properties: Vec<(Name, Option<Node>)> },

    Integer { value: i64 },

    Float { value: f64 },

    String { value: String },

    Bool { value: bool },

    Template { parts: Vec<Node> },

    Unit,
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
    Chain,
    Member,
}
