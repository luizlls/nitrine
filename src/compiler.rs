use crate::syntax::*;
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

    fn compile_expr(&mut self, expr: Expr) -> Result<String> {
        match expr {
            Expr::Name(expr) => {
                self.compile_name(expr)
            }
            Expr::Fun(expr) => {
                self.compile_fun(expr)
            }
            Expr::Let(expr) => {
                self.compile_let(expr)
            }
            Expr::Mut(expr) => {
                self.compile_mut(expr)
            }
            Expr::Apply(expr) => {
                self.compile_apply(expr)
            }
            Expr::Unary(expr) => {
                self.compile_unary(expr)
            }
            Expr::Binary(expr) => {
                self.compile_binary(expr)
            }
            Expr::Block(expr) => {
                self.compile_block(expr)
            }
            Expr::If(expr) => {
                self.compile_if(expr)
            }
            Expr::Tuple(expr) => {
                self.compile_tuple(expr)
            }
            Expr::List(expr) => {
                self.compile_list(expr)
            }
            Expr::Record(expr) => {
                self.compile_record(expr)
            }
            Expr::Member(expr) => {
                self.compile_member(expr)
            }
            Expr::Variant(expr) => {
                self.compile_variant(expr)
            }
            Expr::Number(expr) => {
                self.compile_number(expr)
            }
            Expr::Integer(expr) => {
                self.compile_integer(expr)
            }
            Expr::String(expr) => {
                self.compile_string(expr)
            }
            Expr::Template(expr) => {
                self.compile_template(expr)
            }
            Expr::Unit(_) => Ok("".into())
        }
      }

    fn compile_fun(&mut self, mut function: Fun) -> Result<String> {
        let arg = function.args.pop().map(|arg| arg.value).unwrap_or("".into());

        Ok(format!("function({}) {{ {}; }}", arg, self.compile_body(*function.value)?))
    }

    fn compile_body(&mut self, body: Expr) -> Result<String> {
        match body {
          Expr::Let(decl) => self.compile_let(decl),
          Expr::Mut(decl) => self.compile_mut(decl),
          _ => {
            Ok(format!("return {}", self.compile_expr(body)?))
          }
        }
    }

    fn compile_name(&mut self, name: Name) -> Result<String> {
        Ok(name.value)
    }

    fn compile_let(&mut self, decl: Let) -> Result<String> {
        match *decl.patt {
            Expr::Name(name) => {
                Ok(format!("var {} = {};", name.value, self.compile_expr(*decl.value)?))
            }
            _ => {
                Err(NitrineError::error(
                    decl.span,
                    "Destructuring is not supported for now".into()))
            }
        }
    }

    fn compile_mut(&mut self, decl: Mut) -> Result<String> {
        match *decl.patt {
            Expr::Name(name) => {
                Ok(format!("var {} = {};", name.value, self.compile_expr(*decl.value)?))
            }
            _ => {
                Err(NitrineError::error(
                    decl.span,
                    "Destructuring is not supported for now".into()))
            }
        }
    }

    fn compile_apply(&mut self, app: Apply) -> Result<String> {
        let args: Result<Vec<_>> = app.args
            .into_iter()
            .map(|arg| Ok(format!("({})", self.compile_expr(arg)?)))
            .collect();

        Ok(format!("{}{}", self.compile_expr(*app.fun)?, args?.join("")))
    }

    fn compile_binary(&mut self, binary: Binary) -> Result<String> {
        let operator = format!("{:?}", binary.op.kind).to_lowercase();
        let lexpr = self.compile_expr(*binary.lexpr)?;
        let rexpr = self.compile_expr(*binary.rexpr)?;

        Ok(format!("{}({})({})", operator, lexpr, rexpr))
    }

    fn compile_unary(&mut self, unary: Unary) -> Result<String> {
        Ok(format!("{}({})", format!("{:?}", unary.op.kind).to_lowercase(), self.compile_expr(*unary.expr)?))
    }

    fn compile_block(&mut self, block: Block) -> Result<String> {
        let items: Result<Vec<_>> = block.items
            .into_iter()
            .map(|item| Ok(format!("{}", self.compile_expr(item)?)))
            .collect();

        let mut items = items?;

        let last = items.pop();
        items.push(format!("return {};", last.unwrap()));

        Ok(format!("(function() {{ {} }})()", items.join("\n")))
    }

    fn compile_if(&mut self, cond: If) -> Result<String> {
        let If { test, then, otherwise, .. } = cond;

        let test = self.compile_expr(*test)?;
        let then = self.compile_expr(*then)?;
        let elze = self.compile_expr(*otherwise)?;

        Ok(format!("({} ? {} : {})", test, then, elze))
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
            .map(|item| self.compile_expr(item))
            .collect();

        Ok(format!("[{}]", items?.join(", ")))
    }

    fn compile_tuple(&mut self, tuple: Tuple) -> Result<String> {
        let items: Result<Vec<_>> = tuple.items
            .into_iter()
            .map(|item| self.compile_expr(item))
            .collect();

        Ok(format!("[{}]", items?.join(", ")))
    }

    fn compile_record(&mut self, record: Record) -> Result<String> {
        let props: Result<Vec<_>> = record.properties
        .into_iter()
        .map(|(key, val)| Ok(format!("{}: {}", key.value, self.compile_expr(val)?)))
        .collect();

        Ok(format!("{{ {} }}", props?.join(", ")))
    }

    fn compile_member(&mut self, member: Member) -> Result<String> {
        Ok(format!("{}.{}", self.compile_expr(*member.expr)?, member.name.value))
    }

    fn compile_template(&mut self, tmpl: Template) -> Result<String> {
        let parts: Result<Vec<_>> = tmpl.elements
            .into_iter()
            .map(|element| {
                if let Expr::String(literal) = element {
                    Ok(format!("'{}'", literal.value))
                } else {
                    Ok(format!("({}).toString()", self.compile_expr(element)?))
                }
            })
            .collect();

        Ok(format!("{}", parts?.join(" + ")))
    }

    fn compile_string(&mut self, literal: Literal<String>) -> Result<String> {
        Ok(format!("'{}'", literal.value))
    }

    fn compile_number(&mut self, literal: Literal<f64>) -> Result<String> {
        Ok(literal.value.to_string())
    }

    fn compile_integer(&mut self, literal: Literal<i64>) -> Result<String> {
        Ok(literal.value.to_string())
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

    let nodes: Result<Vec<_>> = module.definitions
        .into_iter()
        .map(|def| Ok(format!("var {} = {};", def.name.value, compiler.compile_expr(*def.value)?)))
        .collect();

    Ok(nodes?.join("\n"))
}