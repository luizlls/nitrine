use core::panic;
use std::{collections::HashMap, vec};

use crate::Span;
use crate::ast::*;
use crate::error::Result;


struct Context<'ctx> {
    parent: Option<&'ctx Self>,
    items: HashMap<String, ()>
}

impl<'ctx> Context<'ctx> {
    fn new() -> Context<'ctx> {
        Context {
            parent: None,
            items: HashMap::new()
        }
    }

    fn nested(parent: &'ctx Context) -> Context<'ctx> {
        Context {
            parent: Some(parent),
            items: HashMap::new()
        }
    }

    fn insert(&mut self, name: String) {
        self.items.insert(name, ());
    }

    fn find(&self, name: &String) -> bool {
        if let Some(_) = self.items.get(name) {
            true
        } else {
            if let Some(ref parent) = self.parent {
                parent.find(name)
            } else {
                false
            }
        }
    }
}

struct Analyzer {
}


impl Analyzer {
    fn new() -> Analyzer {
        Analyzer { }
    }

    fn check_expr(&self, ctx: &mut Context, Expr { kind, span }: Expr) -> Result<Expr> {
        match kind {
            ExprKind::Name(expr) => self.check_name(ctx, expr, span),
            ExprKind::Fun(expr) => self.check_fun(ctx, expr, span),
            ExprKind::Def(expr) => self.check_def(ctx, expr, span),
            ExprKind::Set(expr) => self.check_set(ctx, expr, span),
            ExprKind::Mut(expr) => self.check_mutable(ctx, expr, span),
            ExprKind::Member(expr) => self.check_member(ctx, expr, span),
            ExprKind::Index(expr) => self.check_index(ctx, expr, span),
            ExprKind::Apply(expr) => self.check_apply(ctx, expr, span),
            ExprKind::Unary(expr) => self.check_unary(ctx, expr, span),
            ExprKind::Binary(expr) => self.check_binary(ctx, expr, span),
            ExprKind::Partial(expr) => self.check_partial(ctx, expr, span),
            ExprKind::Tuple(expr) => self.check_tuple(ctx, expr, span),
            ExprKind::List(expr) => self.check_list(ctx, expr, span),
            ExprKind::Dict(expr) => self.check_dict(ctx, expr, span),
            ExprKind::Block(expr) => self.check_block(ctx, expr, span),
            ExprKind::Group(expr) => self.check_group(ctx, expr, span),
            ExprKind::Variant(expr) => self.check_variant(ctx, expr, span),
            ExprKind::String(expr) => self.check_string(expr, span),
            ExprKind::Number(expr) => self.check_number(expr, span),
            ExprKind::Template(expr) => self.check_template(ctx, expr, span),
            ExprKind::If(expr) => self.check_if(ctx, expr, span),
            ExprKind::Match(expr) => self.check_match(ctx, expr, span),
            ExprKind::For(expr) => self.check_for(ctx, expr, span),
            _ => self.expr(kind, span)
        }
    }

    fn check_name(&self, ctx: &mut Context, name: Name, span: Span) -> Result<Expr> {
        if ctx.find(&name.value) {
            // disable ctx check for the moment

            // return Err(NitrineError::error(
            //     name.span,
            //     format!("cannot find value `{}` in this scope", name.value)));
        }

        self.expr(ExprKind::Name(name), span)
    }

    fn check_fun(&self, ctx: &mut Context, function: Fun, span: Span) -> Result<Expr> {
        let mut ctx = Context::nested(ctx);

        let params = function.params
            .into_iter()
            .map(|param| {
                ctx.insert(param.value.clone());
                param
            })
            .collect::<Vec<_>>();

        let value = self.check_expr(&mut ctx, *function.value)?;

        self.expr(ExprKind::function(params, box value), span)
    }

    fn check_def(&self, ctx: &mut Context, def: Def, span: Span) -> Result<Expr> {
        let value = self.check_expr(ctx, *def.value)?;
        ctx.insert(def.name.value.clone());

        self.expr(ExprKind::def(def.name, box value), span)
    }

    fn check_set(&self, ctx: &mut Context, set: Set, span: Span) -> Result<Expr> {
        let target = self.check_expr(ctx, *set.target)?;
        let value  = self.check_expr(ctx, *set.value)?;

        self.expr(ExprKind::set(box target, box value), span)
    }

    fn check_member(&self, ctx: &mut Context, get: Member, span: Span) -> Result<Expr> {
        let source = self.check_expr(ctx, *get.source)?;

        self.expr(ExprKind::member(box source, get.member), span)
    }

    fn check_index(&self, ctx: &mut Context, get: Index, span: Span) -> Result<Expr> {
        let source = self.check_expr(ctx, *get.source)?;
        let index  = self.check_expr(ctx, *get.index)?;

        self.expr(ExprKind::index(box source, box index), span)
    }

    fn check_mutable(&self, ctx: &mut Context, mutable: Mut, span: Span) -> Result<Expr> {
        let value = self.check_expr(ctx, *mutable.value)?;

        // TODO check if is defined as binding (eg var = mut 10), then tag the binding as mutable if applicable
        self.expr(ExprKind::mutable(box value), span)
    }

    fn check_unary(&self, ctx: &mut Context, unary: Unary, span: Span) -> Result<Expr> {
        let arg = self.check_expr(ctx, *unary.rhs)?;

        if unary.op.unary() {
            self.expr(ExprKind::unary(unary.op, box arg), span)
        } else {
            self.expr(ExprKind::partial(unary.op, Some(box arg)), span)
        }
    }

    fn check_binary(&self, ctx: &mut Context, binary: Binary, span: Span) -> Result<Expr> {
        let arg1 = self.check_expr(ctx, *binary.lhs)?;
        let arg2 = self.check_expr(ctx, *binary.rhs)?;

        self.expr(ExprKind::binary(binary.op, box arg1, box arg2), span)
    }

    fn check_partial(&self, ctx: &mut Context, partial: Partial, span: Span) -> Result<Expr> {
        let expr = partial.expr.map(|it| self.check_expr(ctx, *it)).transpose()?;

        self.expr(ExprKind::partial(partial.op, expr.map(Box::new)), span)
    }

    fn check_apply(&self, ctx: &mut Context, app: Apply, span: Span) -> Result<Expr> {
        let fun = self.check_expr(ctx, *app.fun)?;
        let arg = self.check_expr(ctx, *app.arg)?;

        self.expr(ExprKind::apply(box fun, box arg), span)
    }

    fn check_block(&self, ctx: &mut Context, block: Block, span: Span) -> Result<Expr> {
        let mut ctx = Context::nested(ctx);

        let items = block.items
            .into_iter()
            .map(|item| {
                Ok(self.check_expr(&mut ctx, item)?)
            })
            .collect::<Result<Vec<_>>>()?;

        self.expr(ExprKind::block(items), span)
    }

    fn check_group(&self, ctx: &mut Context, group: Group, span: Span) -> Result<Expr> {
        self.check_expr(ctx, *group.inner)
    }

    fn check_if(&self, ctx: &mut Context, if_expr: If, span: Span) -> Result<Expr> {
        let test = self.check_expr(ctx, *if_expr.test)?;
        let then = self.check_expr(ctx, *if_expr.then)?;
        let otherwise = self.check_expr(ctx, *if_expr.otherwise)?;

        self.expr(ExprKind::if_(box test, box then, box otherwise), span)
    }

    fn check_match(&self, ctx: &mut Context, match_expr: Match, span: Span) -> Result<Expr> {
        let mut conditions = match_expr.cases
            .into_iter()
            .map(|(patt, value)| {

                let value = self.check_expr(ctx, value)?;
                let span  = patt.span + value.span;

                let (conditions, bindings) = self.check_pattern(ctx, patt, &value, span)?;

                let mut conditions = conditions.into_iter();
                let first_condition = conditions.next().unwrap(); // `match` always have at least one condition

                let cond = conditions
                    .fold(first_condition, |node, cond| {
                        let span = cond.span + node.span;
                        Expr::expr(
                            ExprKind::binary(Operator::Add, box node, box cond),
                            span)
                    });

                let mut bindings = bindings
                    .into_iter()
                    .map(|(name, value)| {
                        let span = name.span;
                        self.expr(ExprKind::def(name, box value), span)
                    })
                    .collect::<Result<Vec<_>>>()?;

                let value = if bindings.is_empty() {
                    value
                } else {
                    bindings.push(value);
                    self.expr(ExprKind::block(bindings), Span::zero())?
                };

                Ok((cond, value))
            })
            .collect::<Result<Vec<_>>>()?;

        let catchall = matches!(
            conditions.last(),
            Some((Expr { kind: ExprKind::True, .. }, _)));

        todo!()
    }

    fn check_pattern(&self, ctx: &mut Context, patt: Expr, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        match patt.kind {
            ExprKind::Name(name) => {
                self.check_name_pattern(ctx, name, result, span)
            }
            ExprKind::Number(literal)
          | ExprKind::String(literal) => {
                self.check_literal_pattern(ctx, literal, result, span)
            }
            ExprKind::Any => {
                self.check_any_pattern(ctx, result, span)
            }
            ExprKind::List(list) => {
                self.check_list_pattern(ctx, list, result, span)
            }
            ExprKind::Tuple(tuple) => {
                self.check_tuple_pattern(ctx, tuple, result, span)
            }
            ExprKind::Dict(dict) => {
                self.check_dict_pattern(ctx, dict, result, span)
            }
            ExprKind::Variant(dict) => {
                self.check_variant_pattern(ctx, dict, result, span)
            }
            _ => { todo!() }
        }
    }

    fn check_name_pattern(&self, ctx: &mut Context, name: Name, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        todo!()
    }

    fn check_literal_pattern(&self, ctx: &mut Context, value: String, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        todo!()
    }

    fn check_any_pattern(&self, ctx: &mut Context, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        todo!()
    }

    fn check_list_pattern(&self, ctx: &mut Context, list: List, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        todo!()
    }

    fn check_tuple_pattern(&self, ctx: &mut Context, tuple: Tuple, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        todo!()
    }

    fn check_dict_pattern(&self, ctx: &mut Context, dict: Dict, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        todo!()
    }

    fn check_variant_pattern(&self, ctx: &mut Context, variant: Variant, result: &Expr, span: Span)
        -> Result<(Vec<Expr>, Vec<(Name, Expr)>)>
    {
        todo!()
    }


    fn check_for(&self, ctx: &mut Context, for_expr: For, span: Span) -> Result<Expr> {
        let For { target, source, value } = for_expr;

        let source = self.check_expr(ctx, *source)?;
        let value  = self.check_expr(ctx, *value)?;

        let expr = ExprKind::apply(
            box Expr::expr(
                ExprKind::apply(
                    box Expr::expr(ExprKind::name("map".into(), span), span),
                    box Expr::expr(ExprKind::function(
                        vec![
                            target,
                        ],
                        box value), span)),
                span),
            box source);

        self.expr(expr, span)
    }

    fn check_tuple(&self, ctx: &mut Context, tuple: Tuple, span: Span) -> Result<Expr> {
        let items = tuple.items
            .into_iter()
            .map(|item| {
                Ok(self.check_expr(ctx, item)?)
            })
            .collect::<Result<Vec<_>>>()?;

        self.expr(ExprKind::tuple(items), span)
    }

    fn check_list(&self, ctx: &mut Context, list: List, span: Span) -> Result<Expr> {
        let items = list.items
            .into_iter()
            .map(|item| {
                Ok(self.check_expr(ctx, item)?)
            })
            .collect::<Result<Vec<_>>>()?;

        self.expr(ExprKind::list(items), span)
    }

    fn check_dict(&self, ctx: &mut Context, record: Dict, span: Span) -> Result<Expr> {
        let entries = record.entries
            .into_iter()
            .map(|(key, val)| {
                let key = self.check_expr(ctx, key)?;
                let val = self.check_expr(ctx, val)?;
                Ok((key, val))
            })
            .collect::<Result<Vec<_>>>()?;

        self.expr(ExprKind::dict(entries), span)
    }

    fn check_variant(&self, ctx: &mut Context, variant: Variant, span: Span) -> Result<Expr> {
        let values = variant.values
            .into_iter()
            .map(|value| {
                Ok(self.check_expr(ctx, value)?)
            })
            .collect::<Result<Vec<_>>>()?;

        self.expr(ExprKind::variant(variant.name, values), span)
    }

    fn check_string(&self, value: String, span: Span) -> Result<Expr> {
        self.expr(ExprKind::String(value), span)
    }

    fn check_number(&self, value: String, span: Span) -> Result<Expr> {
        self.expr(ExprKind::Number(value), span)
    }

    fn check_template(&self, ctx: &mut Context, template: Template, span: Span) -> Result<Expr> {
        let mut elements = template.elements
            .into_iter()
            .map(|element| {
                let element = self.check_expr(ctx, element)?;

                let element = if let ExprKind::String(_) = element.kind {
                    element
                } else {
                    Expr::expr(
                        ExprKind::apply(
                            box Expr::expr(ExprKind::name("string".into(), element.span), span),
                            box element),
                        span)
                };

                Ok(element)
            })
            .collect::<Result<Vec<_>>>()?;

        let last = elements.pop().unwrap();

        let concat = elements
            .into_iter()
            .fold(last, |chain, elem| {
                let span = chain.span + elem.span;

                Expr::expr(
                    ExprKind::binary(Operator::Concat, box chain, box elem),
                    span)
            });

        Ok(concat)
    }

    fn expr(&self, kind: ExprKind, span: Span) -> Result<Expr> {
        Ok(Expr { kind, span })
    }
}

pub fn analyze(module: Module) -> Result<Module> {
    let analyzer = Analyzer::new();

    let mut global = Context::new();

    let nodes = module.nodes
        .into_iter()
        .map(|node| {
            analyzer.check_expr(&mut global, node)
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(Module { nodes })
}