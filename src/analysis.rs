use std::{collections::HashMap, vec};

use crate::{Span, ast::{self, Expr}};
use crate::hir::{self, Node};
use crate::error::{Result, NitrineError};


struct Info {
    mutable: bool
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

    fn insert(&mut self, name: String, mutable: bool) {
        self.items.insert(name, Info { mutable });
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
        match expr {
            Expr::Name(expr) => self.check_name(ctx, expr),
            Expr::Fun(expr) => self.check_fun(ctx, expr),
            Expr::Let(expr) => self.check_let(ctx, expr),
            Expr::Mut(expr) => self.check_mut(ctx, expr),
            Expr::Apply(expr) => self.check_apply(ctx, expr),
            Expr::Unary(expr) => self.check_unary(ctx, expr),
            Expr::Binary(expr) => self.check_binary(ctx, expr),
            Expr::Block(expr) => self.check_block(ctx, expr),
            Expr::If(expr) => self.check_if(ctx, expr),
            Expr::Tuple(expr) => self.check_tuple(ctx, expr),
            Expr::List(expr) => self.check_list(ctx, expr),
            Expr::Record(expr) => self.check_record(ctx, expr),
            Expr::Member(expr) => self.check_member(ctx, expr),
            Expr::Variant(expr) => self.check_variant(ctx, expr),
            Expr::String(expr) => self.check_string(expr),
            Expr::Number(expr) => self.check_number(expr),
            Expr::Template(expr) => self.check_template(ctx, expr),
            Expr::Unit(span) => Ok(Node::Unit(span))
        }
    }

    fn check_name(&mut self, ctx: &mut Context, name: ast::Name) -> Result<Node> {
        if ctx.find(&name.value).is_none() {
            // disable ctx check for the moment

            // return Err(NitrineError::error(
            //     name.span,
            //     format!("cannot find value `{}` in this scope", name.value)));
        }

        Ok(Node::Name(hir::Name { value: name.value, span: name.span }))
    }

    fn check_fun(&mut self, ctx: &mut Context, mut function: ast::Fun) -> Result<Node> {
        let mut ctx = Context::nested(ctx);

        for arg in function.args.iter() {
            ctx.insert(arg.value.clone(), false);
        }

        let value = self.check_expr(&mut ctx, *function.value)?;

        if function.args.is_empty() {
            function.args.push(ast::Name { value: "".into(), span: Span::new(0, 0, 0) })
        }

        let function = function.args
            .into_iter()
            .rev()
            .fold(value, |value, param| {
                let span = param.span;
                Node::Fun(hir::Fun { param: convert_name(param), value: box value, span })
            });

        Ok(function)
    }

    fn check_let(&mut self, ctx: &mut Context, decl: ast::Let) -> Result<Node> {
        let name = if let Expr::Name(name) = *decl.patt {
            hir::Name { value: name.value, span: name.span }
        } else {
            return Err(NitrineError::error(
                decl.span,
                "destructuring is not supported for now".into()))
        };

        let value = box self.check_expr(ctx, *decl.value)?;

        ctx.insert(name.value.clone(), false);

        Ok(Node::Let(hir::Let { name, value, span: decl.span }))
    }

    fn check_mut(&mut self, ctx: &mut Context, decl: ast::Mut) -> Result<Node> {
        let name = if let Expr::Name(name) = *decl.patt {
            hir::Name { value: name.value, span: name.span }
        } else {
            return Err(NitrineError::error(
                decl.span,
                "destructuring is not supported for now".into()))
        };

        let value = box self.check_expr(ctx, *decl.value)?;

        if ctx.find(&name.value).map(|info| info.mutable).unwrap_or(false) {
            Ok(Node::Set(hir::Set { name, value, span: decl.span }))
        } else {
            ctx.insert(name.value.clone(), true);
            Ok(Node::Let(hir::Let { name, value, span: decl.span }))
        }
    }

    fn check_apply(&mut self, ctx: &mut Context, app: ast::Apply) -> Result<Node> {
        let args: Result<Vec<_>> = app.args
            .into_iter()
            .map(|args| self.check_expr(ctx, args))
            .collect();

        let args = args?;

        let fun = box self.check_expr(ctx, *app.fun)?;

        Ok(Node::Apply(hir::Apply { fun, args, span: app.span }))
    }

    fn check_unary(&mut self, ctx: &mut Context, unary: ast::Unary) -> Result<Node> {
        let args = vec![
            self.check_expr(ctx, *unary.expr)?
        ];

        let fun = box Node::Name(hir::Name {
            value: format!("{}", unary.op).to_lowercase(),
            span: unary.op.span
        });

        Ok(Node::Apply(hir::Apply { fun, args, span: unary.span }))
    }

    fn check_binary(&mut self, ctx: &mut Context, binary: ast::Binary) -> Result<Node> {
        let args = vec![
            self.check_expr(ctx, *binary.lexpr)?,
            self.check_expr(ctx, *binary.rexpr)?
        ];

        let fun = box Node::Name(hir::Name {
            value: format!("{}", binary.op).to_lowercase(),
            span: binary.op.span
        });

        Ok(Node::Apply(hir::Apply { fun, args, span: binary.span }))
    }

    fn check_block(&mut self, ctx: &mut Context, block: ast::Block) -> Result<Node> {
        let mut ctx = Context::nested(ctx);

        let items: Result<Vec<_>> = block.items
            .into_iter()
            .map(|item| self.check_expr(&mut ctx, item))
            .collect();

        let items = items?;

        Ok(Node::Block(hir::Block { items, span: block.span }))

    }

    fn check_if(&mut self, ctx: &mut Context, cond: ast::If) -> Result<Node> {
        let ast::If { test, then, other, span } = cond;

        let test  = box self.check_expr(ctx, *test)?;
        let then  = box self.check_expr(ctx, *then)?;
        let other = box self.check_expr(ctx, *other)?;

        Ok(Node::Cond(hir::Cond { test, then, other, span }))
    }

    fn check_tuple(&mut self, ctx: &mut Context, tuple: ast::Tuple) -> Result<Node> {
        let items: Result<Vec<_>> = tuple.items
            .into_iter()
            .map(|item| self.check_expr(ctx, item))
            .collect();

        let items = items?;

        Ok(Node::List(hir::List { items, span: tuple.span }))
    }

    fn check_list(&mut self, ctx: &mut Context, list: ast::List) -> Result<Node> {
        let items: Result<Vec<_>> = list.items
            .into_iter()
            .map(|item| self.check_expr(ctx, item))
            .collect();

        let items = items?;

        Ok(Node::List(hir::List { items, span: list.span }))
    }

    fn check_record(&mut self, ctx: &mut Context, record: ast::Record) -> Result<Node> {
        let props: Result<Vec<_>> = record.properties
            .into_iter()
            .map(|(key, val)| Ok((convert_name(key), self.check_expr(ctx, val)?)))
            .collect();

        let properties = props?;

        Ok(Node::Record(hir::Record { properties, span: record.span }))
    }

    fn check_member(&mut self, ctx: &mut Context, member: ast::Member) -> Result<Node> {
        let node = box self.check_expr(ctx, *member.expr)?;

        Ok(Node::Get(hir::Get { node, name: convert_name(member.name), span: member.span }))
    }

    fn check_variant(&mut self, ctx: &mut Context, variant: ast::Variant) -> Result<Node> {
        let values: Result<Vec<_>> = variant.values
            .into_iter()
            .map(|value| Ok(self.check_expr(ctx, value)?))
            .collect();

        let values = values?;

        Ok(Node::Variant(hir::Variant { values, name: convert_name(variant.name), span: variant.span }))
    }

    fn check_string(&mut self, literal: ast::Literal) -> Result<Node> {
        Ok(Node::String(hir::Literal { value: literal.value, span: literal.span }))
    }

    fn check_number(&mut self, literal: ast::Literal) -> Result<Node> {
        Ok(Node::Number(hir::Literal { value: literal.value, span: literal.span }))
    }

    fn check_template(&mut self, ctx: &mut Context, template: ast::Template) -> Result<Node> {
        let arguments: Result<Vec<_>> = template.elements
            .into_iter()
            .map(|elements| Ok(self.check_expr(ctx, elements)?))
            .collect();

        let args = arguments?;

        let fun = box Node::Name(hir::Name { value: "concat".into(), span: template.span });

        Ok(Node::Apply(hir::Apply { fun, args, span: template.span }))
    }
}

fn convert_name(name: ast::Name) -> hir::Name {
    hir::Name { value: name.value, span: name.span }
}

pub fn analyze(module: ast::Module) -> Result<hir::Module> {
    let mut analyzer = Analyzer::new();

    let mut global = Context::new();

    let nodes: Result<Vec<_>> = module.definitions
        .into_iter()
        .map(|def| {
            global.insert(def.name.value.clone(), false);

            let name  = convert_name(def.name);
            let value = box analyzer.check_expr(&mut global, *def.value)?;

            Ok(Node::Let(hir::Let { name, value, span: def.span }))
        })
        .collect();

    let nodes = nodes?;

    Ok(hir::Module { name: module.name, nodes })
}