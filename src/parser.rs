use crate::{Source, Span};
use crate::token::*;
use crate::lexer::Lexer;
use crate::ast::*;
use crate::error::{Result, NitrineError};

struct Parser<'s> {
    lexer: Lexer<'s>,
    prev:  Token,
    token: Token,
    peek:  Token,
    source: &'s Source,
}

impl<'s> Parser<'s> {
    fn new(source: &'s Source) -> Parser {
        let mut parser = Parser {
            lexer: Lexer::new(&source.content),
            prev:  Token::default(),
            token: Token::default(),
            peek:  Token::default(),
            source,
        };

        parser.bump(); // token
        parser.bump(); // peek

        parser
    }

    fn parse_module(mut self) -> Result<Module> {
        let mut nodes = vec![];

        while !self.done() {
            nodes.push(self.parse_expr()?);
        }

        Ok(Module { nodes })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Keyword(KeywordKind::Mut) => {
                self.parse_mut()
            }
            TokenKind::Keyword(KeywordKind::Do) => {
                self.parse_do()
            }
            TokenKind::Keyword(KeywordKind::If) => {
                self.parse_if()
            }
            TokenKind::Keyword(KeywordKind::Match) => {
                self.parse_match()
            }
            TokenKind::Operator(_) => {
                self.parse_unary()
            }
            _ if self.start_term() => {
                let mut value = self.parse_term()?;

                if self.start_term() && self.match_lines() {
                    value = self.parse_apply(value)?
                } else if let Expr::Name(name) = value {
                    value = match self.token.kind {
                        TokenKind::Symbol(SymbolKind::Equals) => {
                            self.parse_def(name)?
                        }
                        TokenKind::Symbol(SymbolKind::BArrow) => {
                            self.parse_bind(name)?
                        }
                        TokenKind::Symbol(SymbolKind::FArrow) => {
                            self.parse_fn(vec![name])?
                        }
                        TokenKind::Symbol(SymbolKind::Warlus) => {
                            self.parse_set(Expr::Name(name))?
                        }
                        _ => Expr::Name(name)
                    }
                }

                self.parse_binary(Some(value), 0)
            }
            _ => {
                Err(self.handle_unexpected())
            }
        }
    }

    fn start_term(&self) -> bool {
        matches!(self.token.kind,
            TokenKind::Literal(LiteralKind::Lower)
          | TokenKind::Literal(LiteralKind::Upper)
          | TokenKind::Literal(LiteralKind::String)
          | TokenKind::Literal(LiteralKind::Number)
          | TokenKind::Template(TemplateKind::StringStart)
          | TokenKind::Symbol(SymbolKind::OpeningParen)
          | TokenKind::Symbol(SymbolKind::OpeningBracket)
          | TokenKind::Symbol(SymbolKind::OpeningBrace)
          | TokenKind::Symbol(SymbolKind::Any))
    }

    fn parse_term(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Literal(LiteralKind::Lower) => {
                let name = self.parse_name()?;
                Ok(Expr::Name(name))
            }
            TokenKind::Literal(LiteralKind::Upper) => {
                self.parse_variant()
            }
            TokenKind::Literal(LiteralKind::Number) => {
                self.parse_number()
            }
            TokenKind::Literal(LiteralKind::String) => {
                self.parse_string(TokenKind::Literal(LiteralKind::String))
            }
            TokenKind::Template(TemplateKind::StringStart) => {
                self.parse_template()
            }
            TokenKind::Symbol(SymbolKind::OpeningParen) => {
                self.parse_paren()
            }
            TokenKind::Symbol(SymbolKind::OpeningBracket) => {
                self.parse_list()
            }
            TokenKind::Symbol(SymbolKind::OpeningBrace) => {
                self.parse_record()
            }
            TokenKind::Symbol(SymbolKind::Any) => {
                let span = self.bump();
                Ok(Expr::Any(span))
            }
            _ => {
                Err(self.handle_unexpected())
            }
        }
    }

    fn parse_name(&mut self) -> Result<Name> {
        let span = self.span();

        self.eat(TokenKind::Literal(LiteralKind::Lower))?;
        let value = self.source.content[span.range()].into();

        Ok(Name { value, span })
    }

    fn parse_def(&mut self, name: Name) -> Result<Expr> {
        let start = name.span;

        self.eat(TokenKind::Symbol(SymbolKind::Equals))?;
        let value = box self.parse_expr()?;

        Ok(Expr::Def(
            Def {
                name,
                value,
                span: self.spanned(start),
            }))
    }

    fn parse_bind(&mut self, name: Name) -> Result<Expr> {
        let start = name.span;

        self.eat(TokenKind::Symbol(SymbolKind::BArrow))?;
        let value = box self.parse_expr()?;

        Ok(Expr::Bind(
            Bind {
                name,
                value,
                span: self.spanned(start),
            }))
    }

    fn parse_set(&mut self, expr: Expr) -> Result<Expr> {
        let start = expr.span();

        let target = box expr;
        self.eat(TokenKind::Symbol(SymbolKind::Warlus))?;
        let value = box self.parse_expr()?;

        Ok(Expr::Set(
            Set {
                target,
                value,
                span: self.spanned(start),
            }))
    }

    fn parse_get(&mut self, mut expr: Expr) -> Result<Expr> {
        let span = expr.span();

        while self.maybe_eat(TokenKind::Symbol(SymbolKind::Dot)) {
            let name = self.parse_name()?;
            
            expr = Expr::Get(
                Get {
                    source: box expr,
                    name,
                    span: self.spanned(span),
                });
        }

        Ok(expr)
    }

    fn parse_mut(&mut self) -> Result<Expr> {
        let start = self.span();

        self.eat(TokenKind::Keyword(KeywordKind::Mut))?;
        let value = box self.parse_expr()?;

        Ok(Expr::Mut(
            Mut {
                value,
                span: self.spanned(start),
            }))
    }

    fn parse_do(&mut self) -> Result<Expr> {
        self.eat(TokenKind::Keyword(KeywordKind::Do))
            .and_then(|_| {
                self.parse_block()
            })
    }

    fn parse_block(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut items = vec![];
        
        self.eat(TokenKind::Symbol(SymbolKind::OpeningBrace))?;

        while !self.token_is(TokenKind::Symbol(SymbolKind::ClosingBrace)) {
            items.push(self.parse_expr()?);
        }

        self.eat(TokenKind::Symbol(SymbolKind::ClosingBrace))?;

        Ok(Expr::Block(
            Block {
                items,
                span: self.spanned(start),
            }))
    }

    fn parse_fn(&mut self, params: Vec<Name>) -> Result<Expr> {
        
        let start = if let Some(name) = params.get(0) {
            name.span
        } else {
            self.span()
        };

        self.eat(TokenKind::Symbol(SymbolKind::FArrow))?;

        let body = box self.parse_expr()?;

        Ok(Expr::Fn(
            Fn {
                params,
                body,
                span: self.spanned(start),
            }))
    }

    fn parse_variant(&mut self) -> Result<Expr> {

        let start = self.span();
        self.eat(TokenKind::Literal(LiteralKind::Upper))?;

        let span  = self.prev.span;
        let value = self.source.content[span.range()].to_string();

        match &value[..] {
            "True"  => return Ok(Expr::True(span)),
            "False" => return Ok(Expr::False(span)),
            _ => {}
        }

        let name  = Name { value, span };

        if self.token_is(TokenKind::Symbol(SymbolKind::Dot)) && self.match_lines() {
            self.parse_get(Expr::Name(name))
        } else {
            let values = self.parse_args()?;

            Ok(Expr::Variant(
                Variant {
                    name,
                    values,
                    span: self.spanned(start),
                }))
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>> {
        self.parse_while(Self::parse_term, |this| this.start_term() && this.match_lines())
    }

    fn parse_apply(&mut self, callee: Expr) -> Result<Expr> {
        let start = callee.span();

        match callee {
            Expr::Name(_)
          | Expr::Call(_)
          | Expr::Group(_)
          | Expr::Get(_) => {},
            _ => {
                return Ok(callee)
            }
        };

        if !self.match_lines() {
            return Ok(callee)
        }

        let args = self.parse_args()?;

        Ok(Expr::Call(
            Call {
                fun: box callee,
                args,
                span: self.spanned(start),
            }))
    }

    fn parse_template(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut elements = vec![];

        loop {
            match self.token.kind {
                TokenKind::Template(TemplateKind::StringEnd) => {
                    elements.push(self.parse_string(self.token.kind)?);
                    break;
                }
                TokenKind::Template(TemplateKind::StringStart)
              | TokenKind::Template(TemplateKind::StringFragment) => {
                    elements.push(self.parse_string(self.token.kind)?);
                }
                TokenKind::Symbol(SymbolKind::OpeningBrace) => {
                    self.eat(TokenKind::Symbol(SymbolKind::OpeningBrace))?;
                    elements.push(self.parse_expr()?);
                    self.eat(TokenKind::Symbol(SymbolKind::ClosingBrace))?;
                }
                _ => { break; }
            }
        }

        let elements = elements
            .into_iter()
            .filter(|part| {
                match part {
                    Expr::String(ref s) => !s.value.is_empty(), _ => true
                }
            })
            .collect();

        Ok(Expr::Template(
            Template {
                elements,
                span: self.spanned(start),
            }))
    }

    fn parse_string(&mut self, kind: TokenKind) -> Result<Expr> {
        let span = self.span();

        self.eat(kind)?;
        let raw = self.source.content[span.range()].to_string();

        let value = match kind {
            TokenKind::Literal(LiteralKind::String) => {
                raw[1 .. raw.len() - 1].to_string()
            }
            TokenKind::Template(TemplateKind::StringStart) => {
                raw[1 ..].to_string()
            }
            TokenKind::Template(TemplateKind::StringEnd) => {
                raw[.. raw.len() - 1].to_string()
            }
            _ => raw
        };

        Ok(Expr::String(
            Literal {
                value,
                span: self.spanned(span),
            }))
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let span = self.span();

        self.eat(TokenKind::Literal(LiteralKind::Number))?;
        let value = self.source.content[span.range()].into();

        Ok(Expr::Number(
            Literal {
                value,
                span: self.spanned(span),
            }))
    }

    fn parse_list(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut items = vec![];
        let mut tail  = None;

        self.eat(TokenKind::Symbol(SymbolKind::OpeningBracket))?;

        while !self.done() {
            if self.token_is(TokenKind::Symbol(SymbolKind::ClosingBracket)) {
                break;
            }

            items.push(self.parse_expr()?);

            match self.token.kind {
                TokenKind::Symbol(SymbolKind::Amp) => {
                    self.bump();
                    tail = Some(box self.parse_expr()?);
                    break;
                }
                TokenKind::Symbol(SymbolKind::Comma) => {
                    self.bump();
                }
                _ => { break; }
            }
        }

        self.eat(TokenKind::Symbol(SymbolKind::ClosingBracket))?;

        Ok(Expr::List(
            List {
                items,
                tail,
                span: self.spanned(start),
            }))
    }

    fn parse_paren(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut items = vec![];
        let mut tail  = None;

        self.eat(TokenKind::Symbol(SymbolKind::OpeningParen))?;

        while !self.done() {
            if self.token_is(TokenKind::Symbol(SymbolKind::ClosingParen)) {
                break;
            }

            items.push(self.parse_expr()?);

            match self.token.kind {
                TokenKind::Symbol(SymbolKind::Amp) => {
                    self.bump();
                    tail = Some(box self.parse_expr()?);
                    break;
                }
                TokenKind::Symbol(SymbolKind::Comma) => {
                    self.bump();
                }
                _ => { break; }
            }
        }

        self.eat(TokenKind::Symbol(SymbolKind::ClosingParen))?;

        let span = self.spanned(start);

        if self.token_is(TokenKind::Symbol(SymbolKind::FArrow)) {
            return self.to_params(items)
                       .and_then(|params| self.parse_fn(params));
        }

        let e = if items.is_empty() && tail.is_none() {
            Expr::Unit(span)
        } else if items.len() == 1 && tail.is_none() {
            let inner = box items.into_iter().next().unwrap();
            Expr::Group(
                Group {
                    inner,
                    span: self.spanned(start),
                })
        } else {
            Expr::Tuple(
                Tuple {
                    items,
                    tail,
                    span: self.spanned(start),
                })
        };

        Ok(e)
    }

    fn to_params(&self, items: Vec<Expr>) -> Result<Vec<Name>> {
        return items
            .into_iter()
            .map(|item| {
                if let Expr::Name(name) = item {
                    Ok(name)
                } else {
                    Err(NitrineError::error(
                        item.span(),
                        format!("`{}` is not a valid function parameter", item.display_name())))
                }
            })
            .collect::<Result<Vec<_>>>()
    }

    fn parse_record(&mut self) -> Result<Expr> {
        let start = self.span();

        let properties = self.parse_block_of(
            TokenKind::Symbol(SymbolKind::OpeningBrace),
            TokenKind::Symbol(SymbolKind::ClosingBrace),
            Self::parse_property)?;
        
        Ok(Expr::Record(
            Record {
                properties,
                span: self.spanned(start),
            }))
    }

    fn parse_property(&mut self) -> Result<(Name, Expr)> {
        let key = self.parse_name()?;

        if !self.token_is(TokenKind::Symbol(SymbolKind::Comma))
        && !self.token_is(TokenKind::Symbol(SymbolKind::ClosingBrace))
        && !self.match_lines()
        {
            return Err(NitrineError::error(
                self.span(),
                "The `name` and `value` of the property must start at the same line".into()));
        }

        let val = match self.token.kind {
            TokenKind::Symbol(SymbolKind::Equals) => {
                self.bump();
                self.parse_expr()?
            }
            TokenKind::Symbol(SymbolKind::OpeningBrace) => {
                self.parse_record()?
            }
            _ => Expr::Name(key.clone())
        };

        Ok((key, val))
    }

    fn parse_unary(&mut self) -> Result<Expr> {

        let (kind, span) = if let Token { kind: TokenKind::Operator(kind), span } = self.token {
            (kind, span)
        } else {
            return Err(NitrineError::error(
                self.token.span,
                format!("{} is not a operator", self.token.kind)))
        };

        let operator = self.operator(kind);

        if self.prev.kind == TokenKind::Symbol(SymbolKind::OpeningParen)
        && self.peek.kind == TokenKind::Symbol(SymbolKind::ClosingParen) {
            self.bump();

            return Ok(Expr::Partial(
                Partial {
                    op: operator,
                    lhs: None,
                    rhs: None,
                    span: self.spanned(span),
                }))
        }

        self.bump();

        if !self.match_lines() {
            return Err(NitrineError::error(
                self.token.span,
                "Unary (infix) or partial operators must be in the same line as the operand".into()));
        }

        let expr = box self.parse_term()?;

        Ok(if kind.is_unary() {
            Expr::Unary(
                Unary {
                    op: operator,
                    rhs: expr,
                    span: self.spanned(span),
                })
        } else {
            Expr::Partial(
                Partial {
                    op: operator,
                    rhs: Some(expr),
                    lhs: None,
                    span: self.spanned(span),
                })
        })
    }

    fn parse_binary(&mut self, expr: Option<Expr>, minimum: u8) -> Result<Expr> {

        let mut expr = if let Some(expr) = expr {
            expr
        } else {
            self.parse_term().and_then(|term| self.parse_apply(term))?
        };

        while let TokenKind::Operator(operator) = self.token.kind {
            if let Some((precedence, associativity)) = operator.precedence() {

                if precedence < minimum {
                    break;
                }

                let operator = self.operator(operator);
                self.bump();

                // partial application of operators
                if self.token_is(TokenKind::Symbol(SymbolKind::ClosingParen)) {

                    let span = self.spanned(expr.span());

                    return Ok(Expr::Partial(
                        Partial {
                            op: operator,
                            lhs: Some(box expr),
                            rhs: None,
                            span: self.spanned(span),
                        }))
                }

                let weight = match associativity {
                    Associativity::Right => 0,
                    Associativity::Left  => 1,
                    Associativity::None  => 1,
                };

                let rhs = box self.parse_binary(None, precedence + weight)?;
                let lhs = box expr;

                let span = lhs.span() + rhs.span();

                expr = Expr::Binary(
                    Binary {
                        op: operator,
                        lhs,
                        rhs,
                        span: self.spanned(span),
                    });
            }
        }

        Ok(expr)
    }

    fn operator(&self, kind: OperatorKind) -> Operator {
        match kind {
            OperatorKind::Add => Operator::Add,
          | OperatorKind::Sub => Operator::Sub,
          | OperatorKind::Mul => Operator::Mul,
          | OperatorKind::Div => Operator::Div,
          | OperatorKind::Rem => Operator::Rem,
          | OperatorKind::And => Operator::And,
          | OperatorKind::Or  => Operator::Or,
          | OperatorKind::Is  => Operator::Is,
          | OperatorKind::Not => Operator::Not,
          | OperatorKind::Eq  => Operator::Eq,
          | OperatorKind::Ne  => Operator::Ne,
          | OperatorKind::Lt  => Operator::Lt,
          | OperatorKind::Le  => Operator::Le,
          | OperatorKind::Gt  => Operator::Gt,
          | OperatorKind::Ge  => Operator::Ge,
          | OperatorKind::Concat => Operator::Concat,
          | OperatorKind::BitAnd => Operator::BitAnd,
          | OperatorKind::BitOr  => Operator::BitOr,
          | OperatorKind::BitNot => Operator::BitNot,
          | OperatorKind::BitXor => Operator::BitXor,
          | OperatorKind::BitShr => Operator::BitShr,
          | OperatorKind::BitShl => Operator::BitShl,
          | OperatorKind::LPipe  => Operator::LPipe,
          | OperatorKind::RPipe  => Operator::RPipe,
        }
    }

    fn parse_if(&mut self) -> Result<Expr> {
        let start = self.span();

        self.eat(TokenKind::Keyword(KeywordKind::If))?;
        let predicate = box self.parse_expr()?;

        self.eat(TokenKind::Keyword(KeywordKind::Then))?;
        let positive = box self.parse_expr()?;

        self.eat(TokenKind::Keyword(KeywordKind::Else))?;
        let negative = box self.parse_expr()?;

        Ok(Expr::If(
            If {
                predicate,
                positive,
                negative,
                span: self.spanned(start),
            }))
    }

    fn parse_match(&mut self) -> Result<Expr> {
        let start = self.span();

        self.eat(TokenKind::Keyword(KeywordKind::Match))?;

        let predicate = box self.parse_expr()?;

        let mut cases = vec![];
        while self.maybe_eat(TokenKind::Keyword(KeywordKind::Case)) {
            
            let patt = self.parse_term()?;
            self.eat(TokenKind::Symbol(SymbolKind::FArrow))?;
            let value = self.parse_expr()?;

            if !patt.is_pattern() {
                return Err(NitrineError::error(
                    patt.span(),
                    format!("`{}` is not a valid pattern", patt.display_name())));
            }

            cases.push((patt, value));

            if self.token_is(TokenKind::Symbol(SymbolKind::ClosingBrace)) {
                break;
            }
        }

        if self.maybe_eat(TokenKind::Keyword(KeywordKind::Else)) {
            let span = self.span();

            let value = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningBrace)) {
                self.parse_block()?
            } else {
                self.parse_expr()?
            };

            cases.push((Expr::Any(span), value));
        }

        Ok(Expr::Match(
            Match {
                predicate,
                cases,
                span: self.spanned(start),
            }))
    }

    fn parse_while<T, F, P>(&mut self, mut collect: F, predicate: P)
        -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>, P: std::ops::Fn(&Self) -> bool
    {
        let mut result = vec![];

        while predicate(&self) {
            result.push(collect(self)?);
        }

        Ok(result)
    }

    fn parse_block_of<T, F>(&mut self, open: TokenKind, close: TokenKind, mut f: F)
        -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.eat(open)?;

        loop {
            if self.token_is(close) {
                break;
            }

            result.push(f(self)?);

            if !self.maybe_eat(TokenKind::Symbol(SymbolKind::Comma)) {
                break;
            }
        }

        self.eat(close)?;

        Ok(result)
    }

    fn bump(&mut self) -> Span {
        self.prev  = self.token;
        self.token = self.peek;
        self.peek  = if let Some(token) = self.lexer.next() {
            token
        } else {
            Token::default()
        };

        self.prev.span
    }

    fn done(&self) -> bool {
        self.token.kind == TokenKind::EOF
    }

    fn eat(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.token;

        if token.kind == kind {
            self.bump();
            Ok(token)
        } else {
            Err(NitrineError::error(
                token.span,
                format!("Expected `{}`, but found `{}`", kind, token.kind)))
        }
    }

    fn maybe_eat(&mut self, kind: TokenKind) -> bool {
        if self.token.kind == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn token_is(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn spanned(&self, start: Span) -> Span {
        start + self.prev.span
    }

    fn match_lines(&self) -> bool {
        self.prev.span.line == self.token.span.line
    }

    fn handle_unexpected(&mut self) -> NitrineError {
        let Token { kind, span } = self.token;

        let msg = match kind {
            TokenKind::Error(_) => {
                format!("{}", kind)
            }
            _ => {
                format!("Unexpected `{}`", kind)
            }
        };

        NitrineError::error(span, msg)
    }
}

pub fn parse(source: &Source) -> Result<Module> {
    Parser::new(source).parse_module()
}