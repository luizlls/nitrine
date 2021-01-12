use crate::Span;

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Fn { parameters: Vec<String>, body: Box<Node> },

    Let { name: String, value: Box<Node>, },

    Var { name: String },

    Apply { function: String, args: Vec<Node> },

    Cond { test: Box<Node>, then: Box<Node>, otherwise: Box<Node> },

    List { items: Vec<Node> },

    Object { base: Option<String>, props: Vec<(String, Option<Node>)> },

    String { value: String },

    Float { value: f64 },

    Integer { value: i64 },

    Symbol { name: String, value: Option<Box<Node>> },

    True,

    False,

    Unit,
}