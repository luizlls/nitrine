use crate::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Node {

    Name(Name),

    Fun(Fun),

    Let(Let),

    Set(Set),

    Get(Get),

    Apply(Apply),

    Block(Block),

    Cond(Cond),

    List(List),

    Record(Record),

    Variant(Variant),

    Number(Literal),

    String(Literal),

    Unit(Span)
}

#[derive(Debug, Clone)]
pub struct Name {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub param: Name,
    pub value: Box<Node>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: Name,
    pub value: Box<Node>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub name: Name,
    pub value: Box<Node>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Get {
    pub node: Box<Node>,
    pub name: Name,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub fun: Box<Node>,
    pub args: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub test: Box<Node>,
    pub then: Box<Node>,
    pub other: Box<Node>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct List {
    pub items: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub properties: Vec<(Name, Node)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Name,
    pub values: Vec<Node>,
    pub span: Span,
}


impl Node {
    pub fn span(&self) -> Span {
        match self {
            Node::Name(node) => node.span,
            Node::Fun(node) => node.span,
            Node::Let(node) => node.span,
            Node::Set(node) => node.span,
            Node::Get(node) => node.span,
            Node::Apply(node) => node.span,
            Node::Block(node) => node.span,
            Node::Cond(node) => node.span,
            Node::List(node) => node.span,
            Node::Record(node) => node.span,
            Node::Variant(node) => node.span,
            Node::Number(node) => node.span,
            Node::String(node) => node.span,
            Node::Unit(span) => *span,
        }
    }
}
