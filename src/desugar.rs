use std::vec;

use crate::ast::*;
use crate::Span;
use crate::error::{Result, NitrineError};

struct Desugar {
    // 'unique' variable counter
    // TODO use some kind of 'context' to define
    counter: usize
}

struct ValueBinding {
    name: Name,
    value: Expr,
}

impl Desugar {
    fn new() -> Desugar {
        Desugar {
            counter: 0
        }
    }

    fn desugar_expr(&mut self, expr: Expr) -> Result<Expr> {
        match expr {
            Expr::Fn(expr) => self.desugar_fun(expr),
            Expr::Def(expr) => self.desugar_def(expr),
            Expr::Set(expr) => self.desugar_set(expr),
            Expr::Mut(expr) => self.desugar_mutable(expr),
            Expr::Member(expr) => self.desugar_member(expr),
            Expr::Index(expr) => self.desugar_index(expr),
            Expr::Apply(expr) => self.desugar_apply(expr),
            Expr::Unary(expr) => self.desugar_unary(expr),
            Expr::Binary(expr) => self.desugar_binary(expr),
            Expr::Partial(expr) => self.desugar_partial(expr),
            Expr::Tuple(expr) => self.desugar_tuple(expr),
            Expr::List(expr) => self.desugar_list(expr),
            Expr::Dict(expr) => self.desugar_dict(expr),
            Expr::Block(expr) => self.desugar_block(expr),
            Expr::Group(expr) => self.desugar_group(expr),
            Expr::Label(expr) => self.desugar_label(expr),
            Expr::Template(expr) => self.desugar_template(expr),
            Expr::If(expr) => self.desugar_if(expr),
            Expr::Match(expr) => self.desugar_match(expr),
            _ => Ok(expr)
        }
    }

    fn desugar_fun(&mut self, function: Fn) -> Result<Expr> {
        let value = self.desugar_expr(*function.value)?;
        Ok(Expr::function(function.params, box value, function.span))
    }

    fn desugar_def(&mut self, def: Def) -> Result<Expr> {
        let value = self.desugar_expr(*def.value)?;
        Ok(Expr::def(def.name, box value, def.span))
    }

    fn desugar_set(&mut self, set: Set) -> Result<Expr> {
        let target = self.desugar_expr(*set.target)?;
        let value  = self.desugar_expr(*set.value)?;
        Ok(Expr::set(box target, box value, set.span))
    }

    fn desugar_member(&mut self, get: Member) -> Result<Expr> {
        let source = self.desugar_expr(*get.source)?;
        Ok(Expr::member(box source, get.member, get.span))
    }

    fn desugar_index(&mut self, get: Index) -> Result<Expr> {
        let source = self.desugar_expr(*get.source)?;
        let index  = self.desugar_expr(*get.index)?;
        Ok(Expr::index(box source, box index, get.span))
    }

    fn desugar_mutable(&mut self, mutable: Mut) -> Result<Expr> {
        let value = self.desugar_expr(*mutable.value)?;
        Ok(Expr::mutable(box value, mutable.span))
    }

    fn desugar_unary(&mut self, unary: Unary) -> Result<Expr> {
        let arg = self.desugar_expr(*unary.rhs)?;
        Ok(Expr::unary(unary.op, box arg, unary.span))
    }

    fn desugar_binary(&mut self, binary: Binary) -> Result<Expr> {
        match binary.op {
            Operator::Is => {
                return self.desugar_is_operator(binary);
            }
            Operator::LPipe
          | Operator::RPipe => {
                return self.desugar_pipe_operator(binary);
            }
            _ => {}
        }

        let lhs = self.desugar_expr(*binary.lhs)?;
        let rhs = self.desugar_expr(*binary.rhs)?;

        Ok(Expr::binary(binary.op, box lhs, box rhs, binary.span))
    }

    fn desugar_pipe_operator(&mut self, binary: Binary) -> Result<Expr> {
        let lhs = self.desugar_expr(*binary.lhs)?;
        let rhs = self.desugar_expr(*binary.rhs)?;

        if binary.op == Operator::LPipe {
            Ok(Expr::apply(box rhs, box lhs, binary.span))
        } else {
            Ok(Expr::apply(box lhs, box rhs, binary.span))
        }
    }

    fn desugar_is_operator(&mut self, binary: Binary) -> Result<Expr> {
        let lhs = *binary.lhs;
        let rhs = *binary.rhs;

        if !rhs.is_pattern() {
            return Err(NitrineError::error(
                rhs.span(),
                format!("`{}` is not a valid pattern", rhs.display_name())));
        }

        let matcher = self.desugar_expr(lhs)?;

        let (pattern, negative) = if let Expr::Unary(Unary { op: Operator::Not, box rhs, .. }) = rhs {
            (rhs, true)
        } else {
            (rhs, false)
        };
        
        let condition = self.desugar_pattern(pattern.extract_grouped(), matcher)?;

        let condition = if negative {
            let span = condition.span();
            Expr::unary(Operator::Not, box condition, span)
        } else {
            condition
        };

        Ok(condition)
    }

    fn desugar_partial(&mut self, partial: Partial) -> Result<Expr> {
        match partial.op {
            Operator::LPipe
          | Operator::RPipe
          | Operator::Is => {
                return Err(NitrineError::error(
                    partial.span,
                    format!("`{}` operator cannot be partially applied", partial.op)))
            }
            _ => {}
        }

        let lhs = partial.lhs.map(|expr| self.desugar_expr(*expr)).transpose()?.map(Box::new);
        let rhs = partial.rhs.map(|expr| self.desugar_expr(*expr)).transpose()?.map(Box::new);

        Ok(Expr::partial(partial.op, lhs, rhs, partial.span))
    }

    fn desugar_apply(&mut self, app: Apply) -> Result<Expr> {
        let fun = self.desugar_expr(*app.fun)?;
        let arg = self.desugar_expr(*app.arg)?;

        Ok(Expr::apply(box fun, box arg, app.span))
    }

    fn desugar_block(&mut self, block: Block) -> Result<Expr> {
        let items = block.items
            .into_iter()
            .map(|item| {
                self.desugar_expr(item)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::block(items, block.span))
    }

    fn desugar_group(&mut self, group: Group) -> Result<Expr> {
        self.desugar_expr(*group.inner)
    }

    fn desugar_if(&mut self, conditional: If) -> Result<Expr> {
        let test = self.desugar_expr(*conditional.test)?;
        let then = self.desugar_expr(*conditional.then)?;
        let other = self.desugar_expr(*conditional.otherwise)?;

        Ok(Expr::conditional(box test, box then, box other, conditional.span))
    }

    fn desugar_match(&mut self, expr: Match) -> Result<Expr> {
        let span = expr.pred.span();

        let matcher = self.desugar_expr(*expr.pred)?;
        
        let (predicate, definition) = if let Expr::Name(_) = matcher {
            (matcher, None)
        } else {
            let name = Name::new(self.make_unique_name(), span);
            let defn = Expr::def(name.clone(), box matcher, span);

            (Expr::Name(name), Some(defn))
        };

        let infallible = expr.cases
            .iter()
            .any(|(patt, _)| matches!(patt, Expr::Any(_) | Expr::Name(_)));

        let otherwise = if infallible {
            Expr::Unit(Span::Undefined) // unreachable case
        } else {
            let span = expr.span;
            Expr::raise(box Expr::string("Could not match any of the patterns".into(), span), span)
        };

        let conditional = expr.cases
            .into_iter()
            .rev()
            .fold(otherwise, |otherwise, (patt, value)| {
                let span = patt.span();

                let test = Expr::binary(Operator::Is, box predicate.clone(), box patt, span);
                let then = value;

                Expr::conditional(box test, box then, box otherwise, span)
            });

        let conditional = self.desugar_expr(conditional)?;

        if let Some(definition) = definition {
            Ok(Expr::block(vec![definition, conditional], span))
        } else {
            Ok(conditional)
        }
    }

    fn desugar_pattern(&mut self, patt: Expr, matcher: Expr) -> Result<Expr> {
        let span = matcher.span();

        let (predicate, definition) = if let Expr::Name(_) = matcher {
            (matcher, None)
        } else {
            let name = Name::new(self.make_unique_name(), span);
            let defn = Expr::def(name.clone(), box matcher, span);

            (Expr::Name(name), Some(defn))
        };

        let (conditions, bindings) = match patt {
            Expr::Name(_) => {
                self.desugar_any_pattern(patt, predicate)?
            }
            Expr::Number(_)
          | Expr::String(_) => {
                self.desugar_literal_pattern(patt, predicate)?
            }
            Expr::Any(_) => {
                self.desugar_any_pattern(patt, predicate)?
            }
            Expr::List(list) => {
                self.desugar_list_pattern(list, predicate)?
            }
            Expr::Tuple(tuple) => {
                self.desugar_tuple_pattern(tuple, predicate)?
            }
            Expr::Dict(dict) => {
                self.desugar_dict_pattern(dict, predicate)?
            }
            Expr::Record(record) => {
                self.desugar_record_pattern(record, predicate)?
            }
            Expr::Label(dict) => {
                self.desugar_label_pattern(dict, predicate)?
            }
            _ => {
                return Err(NitrineError::error(
                    patt.span(),
                    format!("`{}` is not a valid pattern", patt.display_name())));
            }
        };

        let mut items = if let Some(definition) = definition {
            vec![definition]
        } else {
            vec![]
        };

        items.extend(
            bindings
                .into_iter()
                .map(|ValueBinding { name, value }| {
                    let span = name.span;
                    Expr::def(name, box value, span)
                })
        );

        let mut conditions = conditions.into_iter();

        let condition = conditions.next()
            .map(|first| {
                conditions.fold(first, |node, cond| {
                    Expr::binary(Operator::Add, box node, box cond, Span::Undefined)
                })
            })
            .unwrap();

        items.push(condition);

        if items.len() == 1 {
            Ok(items.pop().unwrap())
        } else {
            Ok(Expr::block(items, span))
        }
    }

    fn desugar_name_pattern(&mut self, pattern: Name, predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        let conditions = vec![
            Expr::True(pattern.span)
        ];

        let bindings = vec![
            ValueBinding {
                name: pattern, value: predicate
            }
        ];

        Ok((conditions, bindings))
    }

    fn desugar_any_pattern(&mut self, pattern: Expr, _predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        let conditions = vec![
            Expr::True(pattern.span())
        ];

        Ok((conditions, vec![]))
    }

    fn desugar_literal_pattern(&mut self, pattern: Expr, predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        let span = pattern.span();

        let conditions = vec![
            Expr::binary(Operator::Eq, box pattern, box predicate, span)
        ];

        Ok((conditions, vec![]))
    }

    fn desugar_list_pattern(&mut self, pattern: List, predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        todo!()
    }

    fn desugar_tuple_pattern(&mut self, pattern: Tuple, predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        todo!()
    }

    fn desugar_dict_pattern(&mut self, pattern: Dict, predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        todo!()
    }

    fn desugar_record_pattern(&mut self, record: Record, predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        todo!()
    }

    fn desugar_label_pattern(&mut self, pattern: Label, predicate: Expr)
        -> Result<(Vec<Expr>, Vec<ValueBinding>)>
    {
        todo!()
    }

    fn desugar_tuple(&mut self, tuple: Tuple) -> Result<Expr> {
        let items = tuple.items
            .into_iter()
            .map(|item| {
                self.desugar_expr(item)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::tuple(items, tuple.span))
    }

    fn desugar_list(&mut self, list: List) -> Result<Expr> {
        let items = list.items
            .into_iter()
            .map(|item| {
                self.desugar_expr(item)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::list(items, list.span))
    }

    fn desugar_dict(&mut self, dict: Dict) -> Result<Expr> {
        let entries = dict.entries
            .into_iter()
            .map(|(key, val)| {
                let key = self.desugar_expr(key)?;
                let val = self.desugar_expr(val)?;
                Ok((key, val))
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::dict(entries, dict.span))
    }

    fn desugar_label(&mut self, label: Label) -> Result<Expr> {
        let values = label.values
            .into_iter()
            .map(|value| {
                self.desugar_expr(value)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Expr::label(label.name, values, label.span))
    }

    fn desugar_template(&mut self, template: Template) -> Result<Expr> {
        let mut elements = template.elements
            .into_iter()
            .map(|element| {
                let element = self.desugar_expr(element)?;

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

    fn make_unique_name(&mut self) -> String {
        self.counter += 1;
        format!("${}", self.counter)
    }
}

pub fn desugar(module: Module) -> Result<Module> {
    let mut desugarer = Desugar::new();

    let nodes = module.nodes
        .into_iter()
        .map(|node| {
            desugarer.desugar_expr(node)
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(Module { nodes })
}