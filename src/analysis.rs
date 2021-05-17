use std::{collections::HashMap, vec};

use crate::{Span, ast::{self, *}};
use crate::hir::{self, Node};
use crate::error::{Result, NitrineError};


struct Info {
    mutable: bool,
    span: Span,
}

struct Context<'ctx> {
    parent: Option<&'ctx Self>,
    items: HashMap<String, Info>
}

impl<'ctx> Context<'ctx> {
    fn new() -> Self {
        Context {
            parent: None,
            items: HashMap::new()
        }
    }

    fn nested(parent: &'ctx Self) -> Self {
        Context {
            parent: Some(parent),
            items: HashMap::new()
        }
    }

    fn insert(&mut self, name: String, mutable: bool, span: Span) {
        self.items.insert(name, Info { mutable, span });
    }

    fn find(&self, name: &String) -> Option<&Info> {
        let value = self.items.get(name);
        if value.is_some() {
            value
        } else {
            self.parent.and_then(|p| p.find(name))
        }
    }
}

struct Analyzer {
}


impl Analyzer {
    fn new() -> Analyzer {
        Analyzer { }
    }

    fn check_expr(&mut self, ctx: &mut Context, expr: Expr) -> Result<Node> {
        match expr.kind {
            Expr::Name(expr) => self.check_name(ctx, expr),
            Expr::Fun(expr) => self.check_fun(ctx, expr),
            Expr::Def(expr) => self.check_def(ctx, expr),
            Expr::Set(expr) => self.check_set(ctx, expr),
            Expr::Get(expr) => self.check_get(ctx, expr),
            Expr::Apply(expr) => self.check_apply(ctx, expr),
            Expr::Unary(expr) => self.check_unary(ctx, expr),
            Expr::Binary(expr) => self.check_binary(ctx, expr),
            Expr::Block(expr) => self.check_block(ctx, expr),
            Expr::If(expr) => self.check_if(ctx, expr),
            Expr::Tuple(expr) => self.check_tuple(ctx, expr),
            Expr::List(expr) => self.check_list(ctx, expr),
            Expr::Record(expr) => self.check_record(ctx, expr),
            Expr::Variant(expr) => self.check_variant(ctx, expr),
            Expr::String(expr) => self.check_string(expr),
            Expr::Number(expr) => self.check_number(expr),
            Expr::Template(expr) => self.check_template(ctx, expr),
            Expr::Unit => Ok(Node::Unit)
        }
    }

    fn check_name(&mut self, ctx: &mut Context, name: ast::Name) -> Result<Node> {
        if ctx.find(&name.value).is_none() {
            // disable ctx check for the moment

            // return Err(NitrineError::error(
            //     name.span,
            //     format!("cannot find value `{}` in this scope", name.value)));
        }

        Ok(Node::Var(name.value))
    }

    fn check_fun(&mut self, ctx: &mut Context, function: ast::Fun) -> Result<Node> {
        let mut ctx = Context::nested(ctx);

        let args = function.args
            .into_iter()
            .map(|arg| {
                match arg {
                    Expr::Name(name) => {
                        ctx.insert(name.value.clone(), false, name.span);
                        Ok(name.value)
                    }
                    Expr::Unit(_span) => {
                        Ok(String::new())
                    }
                    _ => {
                        Err(NitrineError::error(
                            arg.span(),
                            "Function argument destructuring is not supported for now".into()))
                    }
                }
            })
            .collect::<Result<Vec<_>>>()?;

        let value = self.check_expr(&mut ctx, *function.value)?;

        let function = args
            .into_iter()
            .rev()
            .fold(value, |value, param| {
                Node::Fun(hir::Fun { param, value: box value })
            });

        Ok(function)
    }

    fn check_def(&mut self, ctx: &mut Context, def: ast::Def) -> Result<Node> {
        let name = if let Expr::Name(name) = *def.patt {
            ctx.insert(name.value.clone(), def.mutable, name.span);
            name.value
        } else {
            return Err(NitrineError::error(
                def.span,
                "destructuring is not supported for now".into()))
        };

        let value = box self.check_expr(ctx, *def.value)?;

        Ok(Node::Def(hir::Def { name, value }))
    }

    fn check_set(&mut self, ctx: &mut Context, set: ast::Set) -> Result<Node> {
        let (name, span) = if let Expr::Name(name) = *set.patt {
            (name.value, name.span)
        } else {
            return Err(NitrineError::error(
                set.span,
                "destructuring is not supported for now".into()))
        };

        let value = box self.check_expr(ctx, *set.value)?;

        match ctx.find(&name) {
            Some(Info { mutable: true, .. }) => {
                Ok(Node::Set(hir::Set { name, value }))
            }
            Some(Info { mutable: false, .. }) => {
                Err(NitrineError::error(
                    span,
                    format!("variable `{}` not defined as mutable", name)))
            }
            None => {
                Err(NitrineError::error(
                    span,
                    format!("cannot find value `{}` in this scope", name)))
            }
        }
    }

    fn check_unary(&mut self, ctx: &mut Context, unary: ast::Unary) -> Result<Node> {
        let args = vec![
            self.check_expr(ctx, *unary.expr)?,
        ];

        let fun = Node::Var(format!("{}", unary.op).to_lowercase());

        self.generate_apply(fun, args)
    }

    fn check_binary(&mut self, ctx: &mut Context, binary: ast::Binary) -> Result<Node> {
        let args = vec![
            self.check_expr(ctx, *binary.lexpr)?,
            self.check_expr(ctx, *binary.rexpr)?
        ];

        let fun = Node::Var(format!("{}", binary.op).to_lowercase());

        self.generate_apply(fun, args)
    }

    fn check_apply(&mut self, ctx: &mut Context, app: ast::Apply) -> Result<Node> {
        let fun = self.check_expr(ctx, *app.fun)?;

        let args = app.args
            .into_iter()
            .map(|arg| {
                Ok(self.check_expr(ctx, arg)?)
            })
            .collect::<Result<Vec<_>>>()?;

        self.generate_apply(fun, args)
    }

    fn generate_apply(&self, fun: Node, args: Vec<Node>) -> Result<Node> {
        let app = args
            .into_iter()
            .fold(fun, |app, arg| {
                Node::Apply(hir::Apply { fun: box app, arg: box arg })
            });

        Ok(app)
    }

    fn check_block(&mut self, ctx: &mut Context, block: ast::Block) -> Result<Node> {
        let mut ctx = Context::nested(ctx);

        let items = block.items
            .into_iter()
            .map(|item| {
                Ok(self.check_expr(&mut ctx, item)?)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Node::Block(hir::Block { items }))

    }

    fn check_if(&mut self, ctx: &mut Context, cond: ast::If) -> Result<Node> {
        let ast::If { test, then, other, .. } = cond;

        let test  = box self.check_expr(ctx, *test)?;
        let then  = box self.check_expr(ctx, *then)?;
        let other = box self.check_expr(ctx, *other)?;

        Ok(Node::Cond(hir::Cond { test, then, other }))
    }

    fn check_tuple(&mut self, ctx: &mut Context, tuple: ast::Tuple) -> Result<Node> {
        let items = tuple.items
            .into_iter()
            .map(|item| {
                Ok(self.check_expr(ctx, item)?)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Node::List(hir::List { items }))
    }

    fn check_list(&mut self, ctx: &mut Context, list: ast::List) -> Result<Node> {
        let items = list.items
            .into_iter()
            .map(|item| {
                Ok(self.check_expr(ctx, item)?)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Node::List(hir::List { items }))
    }

    fn check_record(&mut self, ctx: &mut Context, record: ast::Record) -> Result<Node> {
        let properties = record.properties
            .into_iter()
            .map(|(key, val)| {
                Ok((key.value, self.check_expr(ctx, val)?))
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Node::Record(hir::Record { properties }))
    }

    fn check_get(&mut self, ctx: &mut Context, get: ast::Get) -> Result<Node> {
        let node = box self.check_expr(ctx, *get.expr)?;

        Ok(Node::Get(hir::Get { node, name: get.name.value }))
    }

    fn check_variant(&mut self, ctx: &mut Context, variant: ast::Variant) -> Result<Node> {
        let values = variant.values
            .into_iter()
            .map(|value| {
                Ok(self.check_expr(ctx, value)?)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Node::Variant(hir::Variant { name: variant.name.value, values }))
    }

    fn check_string(&mut self, Expr { kind: ExprKind::String(literal), span }: Expr) -> Result<Node> {
        Ok(Node::String(literal.value))
    }

    fn check_number(&mut self, literal: ast::Literal) -> Result<Node> {
        Ok(Node::Number(literal.value))
    }

    fn check_template(&mut self, ctx: &mut Context, template: ast::Template) -> Result<Node> {
        let elements = template.elements
            .into_iter()
            .map(|elements| {
                Ok(self.check_expr(ctx, elements)?)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Node::Template(hir::Template { elements }))
    }
}

pub fn analyze(module: ast::Module) -> Result<hir::Module> {
    let mut analyzer = Analyzer::new();

    let mut global = Context::new();

    let nodes = module.definitions
        .into_iter()
        .map(|def| {
            global.insert(def.name.value.clone(), false, def.span);

            let name  = def.name.value;
            let value = box analyzer.check_expr(&mut global, *def.value)?;

            Ok(Node::Def(hir::Def { name, value }))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(hir::Module { name: module.name, nodes })
}