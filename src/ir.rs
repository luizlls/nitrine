use crate::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Label {
    Local { name: String },

    Global { name: String },

    Block { name: String },
}

#[derive(Debug, Clone)]
pub struct Node {
    pub name: String,
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum NodeKind {

    Fn { parameters: Vec<String>, blocks: Vec<Block> },

    Apply { operation: Operation, arguments: Vec<Label> },

    Branch { target: (Label, Vec<Label>) },

    BranchIf { test: Label, target_then: (Label, Vec<Label>), target_else: (Label, Vec<Label>), },

    Local { name: Label },

    Global { name: Label },

    String { value: String },

    Number { value: String },

    Symbol { name: String, value: Label },

    Tuple { items: Vec<Node> },

    List { items: Vec<Node> },

    Object { base: Option<Label>, props: Vec<(String, Label)> },
}

#[derive(Debug, Clone)]
pub struct Block {
    label: Label,
    nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Operation {
    Call,
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
