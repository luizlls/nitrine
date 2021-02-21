#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Node {

    Var(String),

    Fun(Fun),

    Def(Def),

    Set(Set),

    Get(Get),

    Apply(Apply),

    Block(Block),

    Cond(Cond),

    List(List),

    Record(Record),

    Variant(Variant),

    Number(String),

    String(String),

    Template(Template),

    Unit
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub param: String,
    pub value: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub value: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub name: String,
    pub value: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct Get {
    pub node: Box<Node>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub fun: Box<Node>,
    pub arg: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub test: Box<Node>,
    pub then: Box<Node>,
    pub other: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct List {
    pub items: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub properties: Vec<(String, Node)>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub values: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Template {
    pub elements: Vec<Node>,
}
