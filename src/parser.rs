use crate::{Source, Span};
use crate::ast::{self, *};
use crate::token::{self, *};
use crate::lexer::Lexer;
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

    fn definition(&mut self) -> Result<Definition> {
        let start = self.span();

        let name = self.name()?;

        let value = box if self.matches(TokenKind::Symbol(SymbolKind::OpeningParen)) {
            self.function()?
        } else {
            self.eat(TokenKind::Symbol(SymbolKind::Equals))?;
            self.expr()?
        };

        Ok(Definition { name, value, span: self.complete(start) })
    }

    fn name(&mut self) -> Result<Name> {
        let span = self.token.span;
        self.eat(TokenKind::Literal(LiteralKind::Lower))?;
        let name = self.source.content[span.range()].into();

        Ok(Name { value: name, span })
    }

    fn expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Keyword(KeywordKind::If) => {
                self.cond()
            }
            TokenKind::Keyword(KeywordKind::Fn) => {
                self.lambda()
            }
            _ => {
                self.binary(0)
            }
        }
    }

    fn term(&mut self, apply: bool) -> Result<Expr> {
        let mut term = match self.token.kind {
            TokenKind::Literal(LiteralKind::Lower) => {
                Expr::Name(self.name()?)
            }
            TokenKind::Literal(LiteralKind::Upper) => {
                self.symbol()?
            }
            TokenKind::Literal(LiteralKind::Number) => {
                self.number()?
            }
            TokenKind::Literal(LiteralKind::String) => {
                self.string(TokenKind::Literal(LiteralKind::String))?
            }
            TokenKind::Template(TemplateKind::StringStart) => {
                self.template()?
            }
            TokenKind::Symbol(SymbolKind::OpeningParen) => {
                self.parens()?
            }
            TokenKind::Symbol(SymbolKind::OpeningBrace) => {
                self.record()?
            }
            TokenKind::Symbol(SymbolKind::OpeningBracket) => {
                self.list()?
            }
            TokenKind::Operator(_) => {
                self.unary()?
            }
            _ => {
                return Err(self.handle_unexpected())
            }
        };

        while self.match_lines() {
            term = match self.token.kind {
                TokenKind::Symbol(SymbolKind::Equals) => {
                    self.def(term)?
                }
                TokenKind::Symbol(SymbolKind::Warlus) => {
                    self.set(term)?
                }
                TokenKind::Symbol(SymbolKind::Dot) => {
                    self.dot(term)?
                }
                _ => { break; }
            }
        }

        if apply {
            self.apply(term)
        } else {
            Ok(term)
        }
    }

    fn def(&mut self, expr: Expr) -> Result<Expr> {
        let span = expr.span();

        self.eat(TokenKind::Symbol(SymbolKind::Equals))?;
        let mutable = self.maybe_eat(TokenKind::Keyword(KeywordKind::Mut));
        let value   = box self.expr()?;

        Ok(Expr::Def(Def { patt: box expr, mutable, value, span }))
    }


    fn set(&mut self, expr: Expr) -> Result<Expr> {
        let span = expr.span();

        self.eat(TokenKind::Symbol(SymbolKind::Warlus))?;
        let value = box self.expr()?;

        Ok(Expr::Set(Set { patt: box expr, value, span }))
    }

    fn dot(&mut self, mut expr: Expr) -> Result<Expr> {
        let span = expr.span();

        while self.maybe_eat(TokenKind::Symbol(SymbolKind::Dot)) {
            let name = self.name()?;
                expr = Expr::Get(Get { expr: box expr, name, span: self.complete(span) });
        }

        Ok(expr)
    }

    fn block(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut items = vec![];

        loop {
            items.push(self.expr()?);

            if !self.maybe_eat(TokenKind::Symbol(SymbolKind::Semi)) {
                break;
            }
        }

        Ok(Expr::Block(Block { items, span: self.complete(start) }))
    }

    fn function(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut args = self.block_of(
            TokenKind::Symbol(SymbolKind::OpeningParen),
            TokenKind::Symbol(SymbolKind::ClosingParen),
            Self::expr)?;

        if args.is_empty() {
            args.push(Expr::Unit(self.complete(start)));
        }

        self.eat(TokenKind::Symbol(SymbolKind::Equals))?;

        let value = box self.expr()?;

        Ok(Expr::Fun(Fun { args, value, span: self.complete(start) }))
    }

    fn symbol(&mut self) -> Result<Expr> {
        let Token { span, .. } = self.eat(TokenKind::Literal(LiteralKind::Upper))?;

        let name = self.source.content[span.range()].into();
        let name = Name { value: name, span };

        if self.match_lines() && self.matches(TokenKind::Symbol(SymbolKind::Dot)) {
            self.dot(Expr::Name(name))
        } else {
            let values = self.args()?;
            Ok(Expr::Variant(Variant { name, values, span: self.complete(span) }))
        }
    }

    fn args(&mut self) -> Result<Vec<Expr>> {
        let mut args = vec![];

        while self.match_lines() {
            match self.token.kind {
                TokenKind::Literal(LiteralKind::Lower)
              | TokenKind::Literal(LiteralKind::Upper)
              | TokenKind::Literal(LiteralKind::Number)
              | TokenKind::Literal(LiteralKind::String)
              | TokenKind::Template(TemplateKind::StringStart)
              | TokenKind::Symbol(SymbolKind::OpeningParen)
              | TokenKind::Symbol(SymbolKind::OpeningBrace)
              | TokenKind::Symbol(SymbolKind::OpeningBracket) => {
                    args.push(self.term(false)?)
                }
                _ => { break; }
            }
        }

        Ok(args)
    }

    fn apply(&mut self, expr: Expr) -> Result<Expr> {
        let span = expr.span();

        let args = self.args()?;

        if args.is_empty() {
            Ok(expr)
        } else {
            Ok(Expr::Apply(Apply { fun: box expr, args, span: self.complete(span) }))
        }
    }

    fn template(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut elements = vec![];

        loop {
            match self.token.kind {
                TokenKind::Template(TemplateKind::StringEnd) => {
                    elements.push(self.string(self.token.kind)?);
                    break;
                }
                TokenKind::Template(TemplateKind::StringStart)
              | TokenKind::Template(TemplateKind::StringFragment) => {
                    elements.push(self.string(self.token.kind)?);
                }
                TokenKind::Symbol(SymbolKind::OpeningBrace) => {
                    self.eat(TokenKind::Symbol(SymbolKind::OpeningBrace))?;
                    elements.push(self.expr()?);
                    self.eat(TokenKind::Symbol(SymbolKind::ClosingBrace))?;
                }
                _ => { break; }
            }
        }

        let elements = elements
            .into_iter()
            .filter(|part| {
                match part {
                    Expr::String(ref s) if s.value.is_empty() => {
                        false
                    }
                    _ => true
                }
            })
            .collect();

        Ok(Expr::Template(Template { elements, span: self.complete(start) }))
    }

    fn string(&mut self, kind: TokenKind) -> Result<Expr> {
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

        Ok(Expr::String(Literal { value, span }))
    }

    fn number(&mut self) -> Result<Expr> {
        let span = self.token.span;
        self.eat(TokenKind::Literal(LiteralKind::Number))?;
        let value = self.source.content[span.range()].to_string();

        Ok(Expr::Number(Literal { value, span }))
    }

    fn list(&mut self) -> Result<Expr> {
        let start = self.span();

        let items = self.block_of(
            TokenKind::Symbol(SymbolKind::OpeningBracket),
            TokenKind::Symbol(SymbolKind::ClosingBracket),
            Self::expr)?;

        Ok(Expr::List(List { items, span: self.complete(start) }))
    }

    fn parens(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut items = self.block_of(
            TokenKind::Symbol(SymbolKind::OpeningParen),
            TokenKind::Symbol(SymbolKind::ClosingParen),
            Self::expr)?;

        if items.is_empty() {
            Ok(Expr::Unit(self.complete(start)))
        } else if items.len() == 1 {
            Ok(items.pop().unwrap())
        } else {
            Ok(Expr::Tuple(Tuple { items, span: self.complete(start) }))
        }
    }

    fn record(&mut self) -> Result<Expr> {
        let start = self.span();

        let properties = self.block_of(
            TokenKind::Symbol(SymbolKind::OpeningBrace),
            TokenKind::Symbol(SymbolKind::ClosingBrace),
            Self::property)?;

        Ok(Expr::Record(Record { properties, span: self.complete(start) }))
    }

    fn property(&mut self) -> Result<(Name, Expr)> {
        let key = self.name()?;

        if !self.matches(TokenKind::Symbol(SymbolKind::Comma))
        && !self.matches(TokenKind::Symbol(SymbolKind::ClosingBrace))
        && !self.match_lines()
        {
            return Err(NitrineError::error(
                self.span(),
                "The property name and value must start in the same line".into()));
        }

        let val = match self.token.kind {
            TokenKind::Symbol(SymbolKind::Equals) => {
                self.bump();
                self.expr()?
            }
            TokenKind::Symbol(SymbolKind::OpeningBrace) => {
                self.expr()?
            }
            TokenKind::Symbol(SymbolKind::OpeningParen) => {
                self.function()?
            }
            _ => Expr::Name(key.clone())
        };

        Ok((key, val))
    }

    fn unary(&mut self) -> Result<Expr> {
        let kind = if let Token { kind: TokenKind::Operator(kind), span } = self.token {
            kind
        } else {
            return Err(NitrineError::error(
                self.token.span,
                format!("{} is not a operator", self.token.kind)))
        };

        let operator = self.operator(kind, self.token.span)?;

        if self.prev.kind == TokenKind::Symbol(SymbolKind::OpeningParen)
        && self.peek.kind == TokenKind::Symbol(SymbolKind::ClosingParen) {
            self.bump();
            return Ok(Expr::Name(Name { value: format!("{}", operator).to_lowercase(), span: self.token.span }));
        }

        self.bump();

        if !self.match_lines() {
            return Err(NitrineError::error(
                self.token.span,
                "Unary (infix) or partial operators must be in the same line as the operand".into()))
        }

        let expr = box self.term(false)?;
        Ok(Expr::Unary(Unary { op: operator, expr, span: self.complete(self.token.span) }))
    }

    fn binary(&mut self, minimum_precedence: u8) -> Result<Expr> {
        let mut expr = self.term(true)?;

        'outer:
        while let TokenKind::Operator(operator) = self.token.kind {
            while let Some((precedence, associativity)) = operator.precedence() {

                if precedence < minimum_precedence {
                    break 'outer;
                }

                let span = self.token.span;
                self.bump();

                let operator = self.operator(operator, span)?;

                let fix = match associativity {
                    Associativity::Right => 0,
                    Associativity::Left  => 1,
                    Associativity::None  => 1,
                };

                let span = self.complete(expr.span());

                let lexpr = box expr;
                let rexpr = box self.binary(precedence + fix)?;

                expr = Expr::Binary(Binary { op: operator, lexpr, rexpr, span });
            }
        }

        Ok(expr)
    }

    fn operator(&self, kind: token::OperatorKind, span: Span) -> Result<Operator> {
        let kind = match kind {
            token::OperatorKind::Add => ast::OperatorKind::Add,
          | token::OperatorKind::Sub => ast::OperatorKind::Sub,
          | token::OperatorKind::Mul => ast::OperatorKind::Mul,
          | token::OperatorKind::Div => ast::OperatorKind::Div,
          | token::OperatorKind::Rem => ast::OperatorKind::Rem,
          | token::OperatorKind::Concat => ast::OperatorKind::Concat,
          | token::OperatorKind::BitAnd => ast::OperatorKind::BitAnd,
          | token::OperatorKind::BitOr  => ast::OperatorKind::BitOr,
          | token::OperatorKind::BitNot => ast::OperatorKind::BitNot,
          | token::OperatorKind::BitXor => ast::OperatorKind::BitXor,
          | token::OperatorKind::BitShr => ast::OperatorKind::BitShr,
          | token::OperatorKind::BitShl => ast::OperatorKind::BitShl,
          | token::OperatorKind::And => ast::OperatorKind::And,
          | token::OperatorKind::Or  => ast::OperatorKind::Or,
          | token::OperatorKind::Not => ast::OperatorKind::Not,
          | token::OperatorKind::Eq  => ast::OperatorKind::Eq,
          | token::OperatorKind::Ne  => ast::OperatorKind::Ne,
          | token::OperatorKind::Lt  => ast::OperatorKind::Lt,
          | token::OperatorKind::Le  => ast::OperatorKind::Le,
          | token::OperatorKind::Gt  => ast::OperatorKind::Gt,
          | token::OperatorKind::Ge  => ast::OperatorKind::Ge,
          | token::OperatorKind::LPipe => ast::OperatorKind::LPipe,
          | token::OperatorKind::RPipe => ast::OperatorKind::RPipe,
            _ => {
                return Err(NitrineError::error(
                    span,
                    format!("Operator `{}` is not a valid binary (infix) operator", kind)));
            }
        };

        Ok(Operator { kind, span })
    }

    fn cond(&mut self) -> Result<Expr> {
        let span = self.span();

        self.eat(TokenKind::Keyword(KeywordKind::If))?;
        let test = box self.expr()?;

        self.eat(TokenKind::Keyword(KeywordKind::Then))?;
        let then = box self.expr()?;

        self.eat(TokenKind::Keyword(KeywordKind::Else))?;
        let other = box self.expr()?;

        Ok(Expr::If(If { test, then, other, span: self.complete(span) }))
    }

    fn lambda(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut args = self.block_of(
            TokenKind::Keyword(KeywordKind::Fn),
            TokenKind::Symbol(SymbolKind::Arrow),
            Self::expr)?;

        if args.is_empty() {
            args.push(Expr::Unit(self.complete(start)));
        }

        let value = box self.expr()?;

        Ok(Expr::Fun(Fun { args, value, span: self.complete(start) }))
    }

    fn block_of<T, F>(&mut self, open: TokenKind, close: TokenKind, mut f: F)
        -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.eat(open)?;

        loop {
            if self.matches(close) {
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

    fn complete(&self, start: Span) -> Span {
        start.to(self.prev.span)
    }

    fn bump(&mut self) {
        self.prev  = self.token;
        self.token = self.peek;
        self.peek  = if let Some(token) = self.lexer.next() {
            token
        } else {
            Token::default()
        }
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

    fn matches(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn match_lines(&self) -> bool {
        self.prev.span.line == self.token.span.line
    }

    fn handle_unexpected(&mut self) -> NitrineError {
        let Token { kind, span } = self.token;

        let msg = match kind {
            TokenKind::Error(ErrorKind::InvalidCharacter)
          | TokenKind::Error(ErrorKind::InvalidEscape)
          | TokenKind::Error(ErrorKind::InvalidOperator)
          | TokenKind::Error(ErrorKind::UnterminatedString) => {
                format!("{}", kind)
            }
            _ => format!("Unexpected `{}`", kind)
        };

        NitrineError::error(span, msg)
    }
}

pub fn parse<'s>(source: &'s Source) -> Result<Module> {
    let mut parser = Parser::new(source);

    let mut definitions = vec![];

    while !parser.done() {
        definitions.push(parser.definition()?);
    }

    let name = source.path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    Ok(Module { name, definitions })
}