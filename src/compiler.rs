use crate::hir::*;
use crate::error::{Result, NitrineError};

const INDENT_SPACES: u8 = 4;

struct Compiler {
    buffer: String,
    indent: u8,
    newline: bool
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            buffer: String::new(),
            indent: 0,
            newline: true
        }
    }

    fn compile_node(&mut self, node: Node) -> Result<String> {
        match node {
            Node::Name(node) => self.compile_name(node),
            Node::Fun(node) => self.compile_fun(node),
            Node::Let(node) => self.compile_let(node),
            Node::Set(node) => self.compile_set(node),
            Node::Get(node) => self.compile_get(node),
            Node::Apply(node) => self.compile_apply(node),
            Node::Block(node) => self.compile_block(node),
            Node::Cond(node) => self.compile_cond(node),
            Node::List(node) => self.compile_list(node),
            Node::Record(node) => self.compile_record(node),
            Node::Variant(node) => self.compile_variant(node),
            Node::String(node) => self.compile_string(node),
            Node::Number(node) => self.compile_number(node),
            Node::Unit(_) => Ok("".into())
        }
    }

    fn compile_fun(&mut self, function: Fun) -> Result<String> {
        Ok(format!("function({}) {{ {}; }}", function.param.value, self.compile_body(*function.value)?))
    }

    fn compile_body(&mut self, body: Node) -> Result<String> {
        match body {
          Node::Let(decl) => self.compile_let(decl),
          Node::Set(decl) => self.compile_set(decl),
          _ => {
            Ok(format!("return {}", self.compile_node(body)?))
          }
        }
    }

    fn compile_name(&mut self, name: Name) -> Result<String> {
        Ok(name.value)
    }

    fn compile_let(&mut self, l: Let) -> Result<String> {
        Ok(format!("var {} = {};", l.name.value, self.compile_node(*l.value)?))
    }

    fn compile_set(&mut self, set: Set) -> Result<String> {
        Ok(format!("{} = {};", set.name.value, self.compile_node(*set.value)?))
    }

    fn compile_apply(&mut self, app: Apply) -> Result<String> {
        let args: Result<Vec<_>> = app.args
            .into_iter()
            .map(|arg| Ok(format!("({})", self.compile_node(arg)?)))
            .collect();

        Ok(format!("{}{}", self.compile_node(*app.fun)?, args?.join("")))
    }

    fn compile_block(&mut self, block: Block) -> Result<String> {
        let items: Result<Vec<_>> = block.items
            .into_iter()
            .map(|item| Ok(format!("{}", self.compile_node(item)?)))
            .collect();

        let mut items = items?;

        let last = items.pop();
        items.push(format!("return {};", last.unwrap()));

        Ok(format!("(function() {{ {} }})()", items.join("")))
    }

    fn compile_cond(&mut self, cond: Cond) -> Result<String> {
        let Cond { test, then, other, .. } = cond;

        let test = self.compile_node(*test)?;
        let then = self.compile_node(*then)?;
        let other = self.compile_node(*other)?;

        Ok(format!("({} ? {} : {})", test, then, other))
    }

    fn compile_variant(&mut self, variant: Variant) -> Result<String> {
        match &variant.name.value[..] {
            "True"  => Ok("true".into()),
            "False" => Ok("false".into()),
            _ => {
                Err(NitrineError::error(
                    variant.span,
                    "Variants are not supported for now".into()))
            }
        }
    }

    fn compile_list(&mut self, list: List) -> Result<String> {
        let items: Result<Vec<_>> = list.items
            .into_iter()
            .map(|item| self.compile_node(item))
            .collect();

        Ok(format!("[{}]", items?.join(", ")))
    }

    fn compile_record(&mut self, record: Record) -> Result<String> {
        let props: Result<Vec<_>> = record.properties
        .into_iter()
        .map(|(key, val)| Ok(format!("{}: {}", key.value, self.compile_node(val)?)))
        .collect();

        Ok(format!("{{ {} }}", props?.join(", ")))
    }

    fn compile_get(&mut self, get: Get) -> Result<String> {
        Ok(format!("{}.{}", self.compile_node(*get.node)?, get.name.value))
    }

    fn compile_string(&mut self, literal: Literal) -> Result<String> {
        Ok(format!("'{}'", literal.value))
    }

    fn compile_number(&mut self, literal: Literal) -> Result<String> {
        Ok(literal.value)
    }

    // will be used for prettyprinting in the future
    #[allow(dead_code)]
    fn emit(&mut self, str: &str) {
        if self.newline {
          self.buffer.push_str(&" ".repeat(self.indent as usize));
          self.newline = false;
        }
        self.buffer.push_str(str);
    }

    // will be used for prettyprinting in the future
    #[allow(dead_code)]
    fn line(&mut self) {
        self.newline = true;
        self.buffer.push('\n');
    }

    // will be used for prettyprinting in the future
    #[allow(dead_code)]
    fn indent(&mut self) {
        self.indent += INDENT_SPACES;
    }

    // will be used for prettyprinting in the future
    #[allow(dead_code)]
    fn dedent(&mut self) {
        self.indent -= INDENT_SPACES;
    }
}

pub fn compile(module: Module) -> Result<String> {
    let mut compiler = Compiler::new();

    let nodes: Result<Vec<_>> = module.nodes
        .into_iter()
        .map(|node| compiler.compile_node(node))
        .collect();

    Ok(nodes?.join("\n"))
}