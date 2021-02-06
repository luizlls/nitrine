use crate::{Source, Span};
use crate::ast::*;
use crate::token::{Token, TokenKind, TokenKindError, Associativity};
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

        let value = box if self.matches(TokenKind::OpeningParen) {
            self.function()?
        } else {
            self.eat(TokenKind::Equals)?;
            self.expr()?
        };

        Ok(Definition { name, value, span: self.complete(start) })
    }

    fn name(&mut self) -> Result<Name> {
        let span = self.token.span;
        self.eat(TokenKind::Lower)?;
        let name = self.source.content[span.range()].into();
        
        Ok(Name { name, span })
    }

    fn expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::If => {
                self.cond()
            }
            TokenKind::Do => {
                self.block()
            }
            TokenKind::Fn => {
                self.lambda()
            }
            _ => {
                self.binary(0)
            }
        }
    }

    fn term(&mut self, apply: bool) -> Result<Expr> {
        let mut term = match self.token.kind {
            TokenKind::Lower => {
                Expr::Name(self.name()?)
            }
            TokenKind::Upper => {
                self.symbol()?
            }
            TokenKind::Number => {
                self.number()?
            }
            TokenKind::String => {
                self.string(TokenKind::String)?
            }
            TokenKind::StringStart => {
                self.template()?
            }
            TokenKind::OpeningParen => {
                self.parens()?
            }
            TokenKind::OpeningBrace => {
                self.record()?
            }
            TokenKind::OpeningBracket => {
                self.list()?
            }
            tk if tk.is_operator() => {
                self.unary()?
            }
            _ => {
                return Err(self.handle_unexpected())
            }
        };

        while self.match_lines() {
            term = match self.token.kind {
                TokenKind::Equals
              | TokenKind::Warlus => {
                    self.def(term)?
                }
                TokenKind::Dot => {
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

    fn def(&mut self, patt: Expr) -> Result<Expr> {
        let span = patt.span();

        let mutable = self.matches(TokenKind::Warlus);
        self.bump();

        let value = box self.expr()?;

        if mutable {
            Ok(Expr::Mut(Mut { patt: box patt, value, span }))
        } else {
            Ok(Expr::Let(Let { patt: box patt, value, span }))
        }
    }

    fn dot(&mut self, mut expr: Expr) -> Result<Expr> {
        let span = expr.span();

        while self.maybe_eat(TokenKind::Dot) {
            let name = self.name()?;
                expr = Expr::Member(Member { expr: box expr, name, span: self.complete(span) });
        }

        Ok(expr)
    }

    fn block(&mut self) -> Result<Expr> {
        let start = self.span();

        self.eat(TokenKind::Do)?;

        let mut items = vec![];

        loop {
            items.push(self.expr()?);

            if !self.maybe_eat(TokenKind::Semi) {
                break;
            }
        }

        Ok(Expr::Block(Block { items, span: self.complete(start) }))
    }

    fn function(&mut self) -> Result<Expr> {
        let start = self.span();

        let args = self.block_of(
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
            Self::name)?;

        self.eat(TokenKind::Equals)?;

        let value = box self.expr()?;

        Ok(Expr::Fun(Fun { args, value, span: self.complete(start) }))
    }

    fn symbol(&mut self) -> Result<Expr> {
        let Token { span, .. } = self.eat(TokenKind::Upper)?;

        let name = self.source.content[span.range()].into();
        let name = Name { name, span };

        if self.match_lines() && self.matches(TokenKind::Dot) {
            self.dot(Expr::Name(name))
        } else {
            let values = self.args()?;
            Ok(Expr::Symbol(Symbol { name, values, span: self.complete(span) }))
        }
    }

    fn args(&mut self) -> Result<Vec<Expr>> {
        let mut args = vec![];

        while self.match_lines() {
            match self.token.kind {
                TokenKind::Lower
              | TokenKind::Upper
              | TokenKind::Number
              | TokenKind::StringStart
              | TokenKind::String
              | TokenKind::OpeningParen
              | TokenKind::OpeningBrace
              | TokenKind::OpeningBracket => {
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
                TokenKind::StringFinish => {
                    elements.push(self.string(self.token.kind)?);
                    break;
                }
                TokenKind::StringStart
              | TokenKind::StringFragment => {
                    elements.push(self.string(self.token.kind)?);
                }
                TokenKind::OpeningBrace => {
                    self.eat(TokenKind::OpeningBrace)?;
                    elements.push(self.expr()?);
                    self.eat(TokenKind::ClosingBrace)?;
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
            TokenKind::String => {
                raw[1 .. raw.len() - 1].to_string()
            }
            TokenKind::StringStart => {
                raw[1 ..].to_string()
            }
            TokenKind::StringFinish => {
                raw[.. raw.len() - 1].to_string()
            }
            _ => raw
        };

        Ok(Expr::String(Literal { value, span }))
    }

    fn number(&mut self) -> Result<Expr> {
        let Token { span, .. } = self.eat(TokenKind::Number)?;

        let value = self.source.content[span.range()].to_string();

        if value.contains(".") {
            Ok(Expr::Number(Literal { value: value.parse().unwrap(), span }))
        } else {
            Ok(Expr::Integer(Literal { value: value.parse().unwrap(), span }))
        }
    }

    fn list(&mut self) -> Result<Expr> {
        let start = self.span();

        let items = self.block_of(
            TokenKind::OpeningBracket,
            TokenKind::ClosingBracket,
            Self::expr)?;

        Ok(Expr::List(List { items, span: self.complete(start) }))
    }

    fn parens(&mut self) -> Result<Expr> {
        let start = self.span();

        let mut items = self.block_of(
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
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
            TokenKind::OpeningBrace,
            TokenKind::ClosingBrace,
            Self::property)?;

        Ok(Expr::Record(Record { properties, span: self.complete(start) }))
    }

    fn property(&mut self) -> Result<(Name, Expr)> {
        let key = self.name()?;

        if !self.matches(TokenKind::Comma)
        && !self.matches(TokenKind::ClosingBrace)
        && !self.match_lines()
        {
            return Err(NitrineError::error(
                self.span(),
                "The property name and value must start in the same line".into()));
        }

        let val = match self.token.kind {
            TokenKind::Equals => {
                self.bump();
                self.expr()?
            }
            TokenKind::OpeningBrace => {
                self.expr()?
            }
            TokenKind::OpeningParen => {
                self.function()?
            }
            _ => Expr::Name(key.clone())
        };

        Ok((key, val))
    }

    fn unary(&mut self) -> Result<Expr> {
        let Token { kind, span } = self.token;

        let operator = self.operator(kind, span)?;

        if self.prev.kind == TokenKind::OpeningParen
        && self.peek.kind == TokenKind::ClosingParen {
            self.bump();
            return Ok(Expr::Name(Name { name: format!("{}", operator), span }));
        }

        self.bump();

        if !self.match_lines() {
            return Err(NitrineError::error(
                span,
                "Unary (infix) or partial operators must be in the same line as the operand".into()))
        }

        let expr = box self.term(false)?;
        Ok(Expr::Unary(Unary { op: operator, expr, span: self.complete(span) }))
    }

    fn binary(&mut self, minimum_precedence: u8) -> Result<Expr> {
        let mut expr = self.term(true)?;

        while let Some((precedence, associativity)) = self.token.kind.precedence() {

            if precedence < minimum_precedence {
                break;
            }

            let Token { kind, span } = self.token;
            self.bump();

            let operator = self.operator(kind, span)?;

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

        Ok(expr)
    }

    fn operator(&self, kind: TokenKind, span: Span) -> Result<Operator> {
        let kind = match kind {
            TokenKind::Add => OperatorKind::Add,
          | TokenKind::Sub => OperatorKind::Sub,
          | TokenKind::Mul => OperatorKind::Mul,
          | TokenKind::Div => OperatorKind::Div,
          | TokenKind::Rem => OperatorKind::Rem,
          | TokenKind::Concat => OperatorKind::Concat,
          | TokenKind::BitAnd => OperatorKind::BitAnd,
          | TokenKind::BitOr  => OperatorKind::BitOr,
          | TokenKind::BitXor => OperatorKind::BitXor,
          | TokenKind::BitShr => OperatorKind::BitShr,
          | TokenKind::BitShl => OperatorKind::BitShl,
          | TokenKind::BitNot => OperatorKind::BitNot,
          | TokenKind::And => OperatorKind::And,
          | TokenKind::Or  => OperatorKind::Or,
          | TokenKind::Not => OperatorKind::Not,
          | TokenKind::Is  => OperatorKind::Is,
          | TokenKind::Eq  => OperatorKind::Eq,
          | TokenKind::Ne  => OperatorKind::Ne,
          | TokenKind::Lt  => OperatorKind::Lt,
          | TokenKind::Le  => OperatorKind::Le,
          | TokenKind::Gt  => OperatorKind::Gt,
          | TokenKind::Ge  => OperatorKind::Ge,
          | TokenKind::Pipe => OperatorKind::Pipe,
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

        self.eat(TokenKind::If)?;
        let test = box self.expr()?;

        self.eat(TokenKind::Then)?;
        let then = box self.expr()?;

        self.eat(TokenKind::Else)?;
        let otherwise = box self.expr()?;

        Ok(Expr::If(If { test, then, otherwise, span: self.complete(span) }))
    }

    fn lambda(&mut self) -> Result<Expr> {
        let span = self.span();

        let args = self.block_of(
            TokenKind::Fn,
            TokenKind::Arrow,
            Self::name)?;

        let value = box self.expr()?;

        Ok(Expr::Fun(Fun { args, value, span: self.complete(span) }))
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

            if !self.maybe_eat(TokenKind::Comma) {
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
            TokenKind::Error(TokenKindError::InvalidCharacter)
          | TokenKind::Error(TokenKindError::InvalidEscape)
          | TokenKind::Error(TokenKindError::InvalidOperator)
          | TokenKind::Error(TokenKindError::UnterminatedString) => {
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