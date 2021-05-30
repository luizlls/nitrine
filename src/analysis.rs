use std::collections::HashMap;

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

    fn find(&self, name: &str) -> bool {
        if self.items.get(name).is_some() {
            true
        } else if let Some(parent) = self.parent {
            parent.find(name)
        } else {
            false
        }
    }
}

struct Analyzer {
}


struct PatternBinding {
    name: Name,
    value: Expr,
}

impl Analyzer {
    fn new() -> Analyzer {
        Analyzer { }
    }

    fn check_expr(&self, ctx: &mut Context, expr: Expr) -> Result<Expr> {
        match expr {
            Expr::Name(expr) => self.check_name(ctx, expr),
            Expr::Fn(expr) => self.check_fun(ctx, expr),
            Expr::Def(expr) => self.check_def(ctx, expr),
            Expr::Set(expr) => self.check_set(ctx, expr),
            Expr::Mut(expr) => self.check_mutable(ctx, expr),
            Expr::Member(expr) => self.check_member(ctx, expr),
            Expr::Index(expr) => self.check_index(ctx, expr),
            Expr::Apply(expr) => self.check_apply(ctx, expr),
            Expr::Unary(expr) => self.check_unary(ctx, expr),
            Expr::Binary(expr) => self.check_binary(ctx, expr),
            Expr::Partial(expr) => self.check_partial(ctx, expr),
            Expr::Tuple(expr) => self.check_tuple(ctx, expr),
            Expr::List(expr) => self.check_list(ctx, expr),
            Expr::Dict(expr) => self.check_dict(ctx, expr),
            Expr::Block(expr) => self.check_block(ctx, expr),
            Expr::Group(expr) => self.check_group(ctx, expr),
            Expr::Variant(expr) => self.check_variant(ctx, expr),
            Expr::Template(expr) => self.check_template(ctx, expr),
            Expr::If(expr) => self.check_if(ctx, expr),
            Expr::Match(expr) => self.check_match(ctx, expr),
            _ => Ok(expr)
        }
    }

    fn check_name(&self, ctx: &mut Context, name: Name) -> Result<Expr> {
        if ctx.find(&name.value) {
            // disable ctx check for the moment

            // return Err(NitrineError::error(
            //     name.span,
            //     format!("cannot find value `{}` in this scope", name.value)));
        }

        Ok(Expr::Name(name))
    }

    fn check_fun(&self, ctx: &mut Context, function: Fn) -> Result<Expr> {
        let mut ctx = Context::nested(ctx);

        let params = function.params
            .into_iter()
            .map(|param| {
                ctx.insert(param.value.clone());
                param
            })
            .collect::<Vec<_>>();

        let value = self.check_expr(&mut ctx, *function.value)?;

        Ok(Expr::function(params, box value, function.span))
    }

    fn check_def(&self, ctx: &mut Context, def: Def) -> Result<Expr> {
        let value = self.check_expr(ctx, *def.value)?;
        ctx.insert(def.name.value.clone());

        Ok(Expr::def(def.name, box value, def.span))
    }

    fn check_set(&self, ctx: &mut Context, set: Set) -> Result<Expr> {
        let target = self.check_expr(ctx, *set.target)?;
        let value  = self.check_expr(ctx, *set.value)?;

        Ok(Expr::set(box target, box value, set.span))
    }

    fn check_member(&self, ctx: &mut Context, get: Member) -> Result<Expr> {
        let source = self.check_expr(ctx, *get.source)?;

        Ok(Expr::member(box source, get.member, get.span))
    }

    fn check_index(&self, ctx: &mut Context, get: Index) -> Result<Expr> {
        let source = self.check_expr(ctx, *get.source)?;
        let index  = self.check_expr(ctx, *get.index)?;

        Ok(Expr::index(box source, box index, get.span))
    }

    fn check_mutable(&self, ctx: &mut Context, mutable: Mut) -> Result<Expr> {
        let value = self.check_expr(ctx, *mutable.value)?;

        // TODO check if is defined as binding (eg var = mut 10), then tag the binding as mutable if applicable
        Ok(Expr::mutable(box value, mutable.span))
    }

    fn check_unary(&self, ctx: &mut Context, unary: Unary) -> Result<Expr> {
        let arg = self.check_expr(ctx, *unary.rhs)?;

        Ok(Expr::unary(unary.op, box arg, unary.span))
    }

    fn check_binary(&self, ctx: &mut Context, binary: Binary) -> Result<Expr> {
        let arg1 = self.check_expr(ctx, *binary.lhs)?;
        let arg2 = self.check_expr(ctx, *binary.rhs)?;

        Ok(Expr::binary(binary.op, box arg1, box arg2, binary.span))
    }

    fn check_partial(&self, ctx: &mut Context, partial: Partial) -> Result<Expr> {
        let lhs = partial.lhs.map(|expr| self.check_expr(ctx, *expr)).transpose()?.map(Box::new);
        let rhs = partial.rhs.map(|expr| self.check_expr(ctx, *expr)).transpose()?.map(Box::new);

        Ok(Expr::partial(partial.op, lhs, rhs, partial.span))
    }

    fn check_apply(&self, ctx: &mut Context, app: Apply) -> Result<Expr> {
        let fun = self.check_expr(ctx, *app.fun)?;
        let arg = self.check_expr(ctx, *app.arg)?;

        Ok(Expr::apply(box fun, box arg, app.span))
    }

    fn check_block(&self, ctx: &mut Context, block: Block) -> Result<Expr> {
        let mut ctx = Context::nested(ctx);

        let items = block.items
            .into_iter()
            .map(|item| {
                self.check_expr(&mut ctx, item)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::block(items, block.span))
    }

    fn check_group(&self, ctx: &mut Context, group: Group) -> Result<Expr> {
        self.check_expr(ctx, *group.inner)
    }

    fn check_if(&self, ctx: &mut Context, conditional: If) -> Result<Expr> {
        let test = self.check_expr(ctx, *conditional.test)?;
        let then = self.check_expr(ctx, *conditional.then)?;
        let other = self.check_expr(ctx, *conditional.otherwise)?;

        Ok(Expr::conditional(box test, box then, box other, conditional.span))
    }

    fn check_match(&self, ctx: &mut Context, matching: Match) -> Result<Expr> {
        let mut conditions = matching.cases
            .into_iter()
            .map(|(patt, value)| {

                let value = self.check_expr(ctx, value)?;

                let (conditions, bindings) = self.check_pattern(ctx, patt, &value)?;

                let mut conditions = conditions.into_iter();

                let test = conditions.next()
                    .map(|first| {
                        conditions.fold(first, |node, cond| {
                            let span = node.span()
                                     + cond.span();
                            Expr::binary(Operator::Add, box node, box cond, span)
                        })
                    })
                    .unwrap();

                let value = if bindings.is_empty() {
                    value
                } else {
                    let mut bindings = bindings
                        .into_iter()
                        .map(|PatternBinding { name, value }| {
                            let span = name.span;
                            Expr::def(name, box value, span)
                        })
                        .collect::<Vec<_>>();

                    bindings.push(value);
                    Expr::block(bindings, Span::zero())
                };

                Ok((test, value))
            })
            .collect::<Result<Vec<_>>>()?;

        todo!()
    }

    fn check_pattern(&self, ctx: &mut Context, patt: Expr, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        match patt {
            Expr::Name(name) => {
                self.check_name_pattern(ctx, name, value)
            }
            Expr::Number(literal)
          | Expr::String(literal) => {
                self.check_literal_pattern(ctx, literal, value)
            }
            Expr::Any(_) => {
                self.check_any_pattern(ctx, value)
            }
            Expr::List(list) => {
                self.check_list_pattern(ctx, list, value)
            }
            Expr::Tuple(tuple) => {
                self.check_tuple_pattern(ctx, tuple, value)
            }
            Expr::Dict(dict) => {
                self.check_dict_pattern(ctx, dict, value)
            }
            Expr::Variant(dict) => {
                self.check_variant_pattern(ctx, dict, value)
            }
            _ => { todo!() }
        }
    }

    fn check_name_pattern(&self, ctx: &mut Context, name: Name, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        todo!()
    }

    fn check_literal_pattern(&self, ctx: &mut Context, literal: Literal, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        todo!()
    }

    fn check_any_pattern(&self, ctx: &mut Context, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        todo!()
    }

    fn check_list_pattern(&self, ctx: &mut Context, list: List, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        todo!()
    }

    fn check_tuple_pattern(&self, ctx: &mut Context, tuple: Tuple, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        todo!()
    }

    fn check_dict_pattern(&self, ctx: &mut Context, dict: Dict, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        todo!()
    }

    fn check_variant_pattern(&self, ctx: &mut Context, variant: Variant, value: &Expr)
        -> Result<(Vec<Expr>, Vec<PatternBinding>)>
    {
        todo!()
    }

    fn check_tuple(&self, ctx: &mut Context, tuple: Tuple) -> Result<Expr> {
        let items = tuple.items
            .into_iter()
            .map(|item| {
                self.check_expr(ctx, item)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::tuple(items, tuple.span))
    }

    fn check_list(&self, ctx: &mut Context, list: List) -> Result<Expr> {
        let items = list.items
            .into_iter()
            .map(|item| {
                self.check_expr(ctx, item)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::list(items, list.span))
    }

    fn check_dict(&self, ctx: &mut Context, dict: Dict) -> Result<Expr> {
        let entries = dict.entries
            .into_iter()
            .map(|(key, val)| {
                let key = self.check_expr(ctx, key)?;
                let val = self.check_expr(ctx, val)?;
                Ok((key, val))
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::dict(entries, dict.span))
    }

    fn check_variant(&self, ctx: &mut Context, variant: Variant) -> Result<Expr> {
        let values = variant.values
            .into_iter()
            .map(|value| {
                self.check_expr(ctx, value)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::variant(variant.name, values, variant.span))
    }

    fn check_template(&self, ctx: &mut Context, template: Template) -> Result<Expr> {
        let mut elements = template.elements
            .into_iter()
            .map(|element| {
                let element = self.check_expr(ctx, element)?;

                let element = if let Expr::String(_) = element {
                    element
                } else {
                    let span = element.span();
                    Expr::apply(box Expr::name("string".into(), span), box element, span)
                };

                Ok(element)
            });

        let initial = elements.next().unwrap()?;

        let concat = elements
            .try_fold(initial, |chain, elemement| {
                let elem = elemement?;
                let span = elem.span();
                Ok(Expr::binary(Operator::Concat, box chain, box elem, span))
            })?;

        Ok(concat)
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