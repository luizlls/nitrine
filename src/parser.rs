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
            let node = match self.token.kind {
                TokenKind::Literal(LiteralKind::Lower) => {
                    self.parse_name()
                        .and_then(|name| self.parse_def(name))?
                }
                _ => {
                    return Err(self.handle_unexpected())
                }
            };

            nodes.push(node);
        }

        Ok(Module { nodes })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Keyword(KeywordKind::Fn) => {
                self.parse_lambda()
            }
            TokenKind::Keyword(KeywordKind::If) => {
                self.parse_if()
            }
            TokenKind::Keyword(KeywordKind::Match) => {
                self.parse_match()
            }
            TokenKind::Keyword(KeywordKind::Mut) => {
                self.parse_mut()
            }
            TokenKind::Operator(_) => {
                self.parse_unary()
            }
            _ if self.start_term() => {
                let mut value = self.parse_term()?;

                if let Expr::Name(name) = value {
                    value = match self.token.kind {
                        TokenKind::Symbol(SymbolKind::Equals) => {
                            self.parse_def(name)?
                        }
                        TokenKind::Symbol(SymbolKind::Warlus) => {
                            self.parse_set(Expr::Name(name))?
                        }
                        _ if self.start_term() && self.match_lines() => {
                            self.parse_apply(Expr::Name(name))?
                        }
                        _ => Expr::Name(name)
                    };
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
                self.parse_label()
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
                self.parse_bracket()
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
        let span = self.token.span;

        self.eat(TokenKind::Literal(LiteralKind::Lower))?;
        let value = self.source.content[span.range()].into();

        Ok(Name { value, span })
    }

    fn parse_def(&mut self, name: Name) -> Result<Expr> {
        let start = name.span;

        let value = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningParen)) {
            self.parse_function()?
        } else {
            self.eat(TokenKind::Symbol(SymbolKind::Equals))?;
            self.parse_expr()?
        };

        Ok(Expr::def(name, box value, self.spanned(start)))
    }

    fn parse_set(&mut self, expr: Expr) -> Result<Expr> {
        let start = expr.span();

        self.eat(TokenKind::Symbol(SymbolKind::Warlus))?;
        let value = box self.parse_expr()?;

        Ok(Expr::set(box expr, value, self.spanned(start)))
    }

    fn parse_get(&mut self, mut expr: Expr) -> Result<Expr> {
        let span = expr.span();

        while self.maybe_eat(TokenKind::Symbol(SymbolKind::Dot)) {
            expr = match self.token.kind {
                TokenKind::Symbol(SymbolKind::OpeningBracket) => {
                    self.eat(TokenKind::Symbol(SymbolKind::OpeningBracket))?;
                    let index = box self.parse_expr()?;
                    self.eat(TokenKind::Symbol(SymbolKind::ClosingBracket))?;

                    Expr::index(box expr, index, self.spanned(span))
                }
                _ => {
                    let name = self.parse_name()?;
                    Expr::member(box expr, name, self.spanned(span))
                }
            };
        }

        Ok(expr)
    }

    fn parse_mut(&mut self) -> Result<Expr> {
        let start = self.span();

        self.eat(TokenKind::Keyword(KeywordKind::Mut))?;
        let value = self.parse_expr()?;

        Ok(Expr::mutable(box value, self.spanned(start)))
    }

    fn parse_block(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut items = vec![];

        self.eat(TokenKind::Symbol(SymbolKind::OpeningBrace))?;

        while !self.done() {
            if self.token_is(TokenKind::Symbol(SymbolKind::ClosingBrace)) {
                break;
            }
            items.push(self.parse_expr()?);
        }

        self.eat(TokenKind::Symbol(SymbolKind::ClosingBrace))?;

        Ok(Expr::block(items, self.spanned(start)))
    }

    fn parse_function(&mut self) -> Result<Expr> {
        let start = self.span();

        let params = self.parse_block_of(
            TokenKind::Symbol(SymbolKind::OpeningParen),
            TokenKind::Symbol(SymbolKind::ClosingParen),
            Self::parse_name)?;

        self.eat(TokenKind::Symbol(SymbolKind::Equals))?;

        let value = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningBrace)) {
            self.parse_block()?
        } else {
            self.parse_expr()?
        };

        Ok(Expr::function(params, box value, self.spanned(start)))
    }

    fn parse_lambda(&mut self) -> Result<Expr> {
        let start = self.span();

        self.eat(TokenKind::Keyword(KeywordKind::Fn))?;

        let params = self.parse_while(
            Self::parse_name,
            |this| this.token_is(TokenKind::Literal(LiteralKind::Lower)))?;

        let value = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningBrace)) {
            self.parse_block()?
        } else {
            self.eat(TokenKind::Symbol(SymbolKind::Arrow))?;
            self.parse_expr()?
        };

        Ok(Expr::function(params, box value, self.spanned(start)))
    }

    fn parse_label(&mut self) -> Result<Expr> {

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
            Ok(Expr::label(name, values, self.spanned(start)))
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>> {
        self.parse_while(
            Self::parse_term,
            |this| this.start_term() && this.match_lines())
    }

    fn parse_apply(&mut self, fun: Expr) -> Result<Expr> {
        match fun {
            Expr::Name(_)
          | Expr::Apply(_)
          | Expr::Group(_)
          | Expr::Member(_)
          | Expr::Index(_) => {},
            _ => {
                return Ok(fun)
            }
        };

        if !self.match_lines() {
            return Ok(fun)
        }

        let apply = self.parse_args()?
        .into_iter()
        .fold(fun, |callee, arg| {
            let span = callee.span();
            Expr::apply(box callee, box arg, span)
        });

        Ok(apply)
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

        Ok(Expr::template(elements, self.spanned(start)))
    }

    fn parse_string(&mut self, kind: TokenKind) -> Result<Expr> {
        let Token { span, .. } = self.eat(kind)?;

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

        Ok(Expr::string(value, self.spanned(span)))
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let span = self.span();

        self.eat(TokenKind::Literal(LiteralKind::Number))?;
        let value = self.source.content[span.range()].into();

        Ok(Expr::number(value, self.spanned(span)))
    }

    fn parse_bracket(&mut self) -> Result<Expr> {

        #[derive(PartialEq)]
        enum CollectionKind {
            None,
            List,
            Dict,
        }

        let start = self.span();

        let mut kind = CollectionKind::None;

        let mut list_items = vec![];
        let mut dict_items = vec![];

        self.eat(TokenKind::Symbol(SymbolKind::OpeningBracket))?;

        if self.token_is(TokenKind::Symbol(SymbolKind::Colon))
        && self.peek_is(TokenKind::Symbol(SymbolKind::ClosingBracket)) {
            self.bump();
            kind = CollectionKind::Dict;
        }

        loop {
            if self.token_is(TokenKind::Symbol(SymbolKind::ClosingBracket)) {
                break;
            }

            let key = self.parse_expr()?;

            if kind == CollectionKind::None {
                kind = if self.token_is(TokenKind::Symbol(SymbolKind::Colon)) {
                    CollectionKind::Dict
                } else {
                    CollectionKind::List
                };
            }

            if kind == CollectionKind::Dict {
                self.eat(TokenKind::Symbol(SymbolKind::Colon))?;
                let val = self.parse_expr()?;
                dict_items.push((key, val))
            } else {
                list_items.push(key)
            };

            if !self.maybe_eat(TokenKind::Symbol(SymbolKind::Comma)) {
                break;
            }
        }

        self.eat(TokenKind::Symbol(SymbolKind::ClosingBracket))?;

        let span = self.spanned(start);

        if kind == CollectionKind::Dict {
            Ok(Expr::dict(dict_items, span))
        } else {
            Ok(Expr::list(list_items, span))
        }
    }

    fn parse_paren(&mut self) -> Result<Expr> {

        let start = self.span();

        let items = self.parse_block_of(
            TokenKind::Symbol(SymbolKind::OpeningParen),
            TokenKind::Symbol(SymbolKind::ClosingParen),
            Self::parse_expr)?;

        let span = self.spanned(start);

        Ok(if items.is_empty() {
            Expr::Unit(span)
        } else if items.len() == 1 {
            Expr::group(box items.into_iter().next().unwrap(), span)
        } else {
            Expr::tuple(items, span)
        })
    }


    fn parse_record(&mut self) -> Result<Expr> {
        let start = self.span();

        let properties = self.parse_block_of(
            TokenKind::Symbol(SymbolKind::OpeningBrace),
            TokenKind::Symbol(SymbolKind::ClosingBrace),
            Self::parse_property)?;

        Ok(Expr::record(properties, self.spanned(start)))
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
            TokenKind::Symbol(SymbolKind::OpeningParen) => {
                self.parse_function()?
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
            return Ok(Expr::partial(operator, None, None, self.spanned(span)));
        }

        self.bump();

        if !self.match_lines() {
            return Err(NitrineError::error(
                self.token.span,
                "Unary (infix) or partial operators must be in the same line as the operand".into()));
        }

        let expr = self.parse_term()?;

        let span = self.spanned(span);

        if kind.is_unary() {
            Ok(Expr::unary(operator, box expr, span))
        } else {
            Ok(Expr::partial(operator, None, Some(box expr), span))
        }
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
                    let span = expr.span() + self.prev.span;
                    return Ok(Expr::partial(operator, Some(box expr), None, span));
                }

                let weight = match associativity {
                    Associativity::Right => 0,
                    Associativity::Left  => 1,
                    Associativity::None  => 1,
                };

                let rhs = self.parse_binary(None, precedence + weight)?;
                let lhs = expr;

                let span = lhs.span() + rhs.span();

                expr = Expr::binary(operator, box lhs, box rhs, span);
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
        let test = self.parse_expr()?;

        let then = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningBrace)) {
            self.parse_block()?
        } else {
            self.eat(TokenKind::Keyword(KeywordKind::Then))?;
            self.parse_expr()?
        };

        self.eat(TokenKind::Keyword(KeywordKind::Else))?;

        let other = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningBrace)) {
            self.parse_block()?
        } else {
            self.parse_expr()?
        };

        Ok(Expr::conditional(box test, box then, box other, self.spanned(start)))
    }

    fn parse_match(&mut self) -> Result<Expr> {
        let start = self.span();

        self.eat(TokenKind::Keyword(KeywordKind::Match))?;
        let value = self.parse_expr()?;

        let braces = self.maybe_eat(TokenKind::Symbol(SymbolKind::OpeningBrace));

        let mut cases = vec![];

        while self.maybe_eat(TokenKind::Keyword(KeywordKind::Case)) {
            let patt = self.parse_term()?;

            if !patt.is_pattern() {
                return Err(NitrineError::error(
                    patt.span(),
                    format!("`{}` is not a valid pattern", patt.display_name())));
            }

            self.eat(TokenKind::Symbol(SymbolKind::Arrow))?;

            let result = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningBrace)) {
                self.parse_block()?
            } else {
                self.parse_expr()?
            };

            cases.push((patt, result));

            if self.token_is(TokenKind::Symbol(SymbolKind::ClosingBrace)) {
                break;
            }
        }

        if self.maybe_eat(TokenKind::Keyword(KeywordKind::Else)) {
            let span = self.span();

            let result = if self.token_is(TokenKind::Symbol(SymbolKind::OpeningBrace)) {
                self.parse_block()?
            } else {
                self.parse_expr()?
            };

            cases.push((Expr::Any(span), result));
        }

        if braces {
            self.eat(TokenKind::Symbol(SymbolKind::ClosingBrace))?;
        }

        Ok(Expr::pattern_match(box value, cases, self.spanned(start)))
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

    fn peek_is(&self, kind: TokenKind) -> bool {
        self.peek.kind == kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn spanned(&self, start: Span) -> Span {
        start + self.prev.span
    }

    fn match_lines(&self) -> bool {
        match (self.prev.span, self.token.span) {
            (Span::Full { line: prev_line, .. },
             Span::Full { line: curr_line, .. }) => {
                prev_line == curr_line
            }
            _ => false
        }
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