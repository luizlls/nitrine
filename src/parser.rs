use crate::{Source, Span};
use crate::ast::*;
use crate::token::{Token, TokenKind, TokenKindError, Associativity};
use crate::lexer::Lexer;
use crate::error::{Result, NitrineError};

#[derive(Clone, Copy, PartialEq)]
enum Nesting {
    Paren,
    Brace,
    Bracket,
    Fun,
    Def,
}

struct Parser<'s> {
    lexer: Lexer<'s>,
    prev:  Token,
    token: Token,
    peek:  Token,
    source: &'s Source,
    depth: Vec<Nesting>,
}

impl<'s> Parser<'s> {
    fn new(source: &'s Source) -> Parser {
        let mut parser = Parser {
            lexer: Lexer::new(&source.content),
            prev:  Token::default(),
            token: Token::default(),
            peek:  Token::default(),
            source,
            depth: vec![]
        };

        parser.bump(); // token
        parser.bump(); // peek

        parser
    }

    fn name(&mut self) -> Result<Name> {
        let Token { span, .. } = self.eat(TokenKind::Lower)?;
        let name = self.source.content[span.range()].into();
        Ok(Name { name, span })
    }

    fn expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::If => {
                self.cond()
            }
            TokenKind::Fn => {
                self.lambda()
            }
            TokenKind::Lower if self.depth.is_empty() => {
                self.top_level()
            }
            _ => {
                self.binary(0)
            }
        }
    }

    fn top_level(&mut self) -> Result<Expr> {
        let start = self.span();

        let patt = Expr::Name(self.name()?);

        if self.matches(TokenKind::OpeningParen) {
            let value = self.function()?;
            Ok(Expr::Let(box Let { patt, value, span: self.complete(start) }))
        } else {
            self.def(patt)
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
                TokenKind::Semi if !self.nesting(Nesting::Def) => {
                    self.chain(term)?
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

        self.enter(Nesting::Def);

        let mutable = self.matches(TokenKind::Warlus);
        self.bump();

        let value = self.expr()?;

        self.exit(Nesting::Def)?;

        if mutable {
            Ok(Expr::Mut(box Mut { patt, value, span }))
        } else {
            Ok(Expr::Let(box Let { patt, value, span }))
        }
    }

    fn dot(&mut self, mut expr: Expr) -> Result<Expr> {
        let span = expr.span();

        while self.maybe_eat(TokenKind::Dot) {
            let name = self.name()?;
                expr = Expr::Member(box Member { expr, name, span: self.complete(span) });
        }

        Ok(expr)
    }

    fn chain(&mut self, expr: Expr) -> Result<Expr> {
        let span = expr.span();

        let operator = Operator {
            kind: TokenKind::Semi,
            span
        };

        self.bump();

        let rhs = self.expr()?;
        let lhs = expr;

        Ok(Expr::Binary(box Binary { operator, lhs, rhs, span: self.complete(span) }))
    }

    fn function(&mut self) -> Result<Expr> {
        let start = self.span();

        self.enter(Nesting::Fun);

        let parameters = self.block_of(
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
            Self::name)?;

        self.eat(TokenKind::Equals)?;

        let value = self.expr()?;

        self.exit(Nesting::Fun)?;

        Ok(Expr::Fn(box Fn { parameters, value, span: self.complete(start) }))
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

    fn apply(&mut self, function: Expr) -> Result<Expr> {
        let span = function.span();

        let arguments = self.args()?;

        if arguments.is_empty() {
            Ok(function)
        } else {
            Ok(Expr::Apply(box Apply { function, arguments, span: self.complete(span) }))
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
            Ok(Expr::Float(Literal { value: value.parse().unwrap(), span }))
        } else {
            Ok(Expr::Integer(Literal { value: value.parse().unwrap(), span }))
        }
    }

    fn list(&mut self) -> Result<Expr> {
        let start = self.span();

        self.enter(Nesting::Bracket);

        let items = self.block_of(
            TokenKind::OpeningBracket,
            TokenKind::ClosingBracket,
            Self::expr)?;

        self.exit(Nesting::Bracket)?;

        Ok(Expr::List(List { items, span: self.complete(start) }))
    }

    fn parens(&mut self) -> Result<Expr> {
        let start = self.span();

        self.enter(Nesting::Paren);

        let mut items = self.block_of(
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
            Self::expr)?;

        self.exit(Nesting::Paren)?;

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

        self.enter(Nesting::Brace);

        let properties = self.block_of(
            TokenKind::OpeningBrace,
            TokenKind::ClosingBrace,
            Self::property)?;

        self.exit(Nesting::Brace)?;

        Ok(Expr::Record(Record { properties, span: self.complete(start) }))
    }

    fn property(&mut self) -> Result<(Name, Option<Expr>)> {
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
                Some(self.expr()?)
            }
            TokenKind::OpeningBrace => {
                Some(self.expr()?)
            }
            TokenKind::OpeningParen => {
                Some(self.function()?)
            }
            _ => None
        };

        Ok((key, val))
    }

    fn unary(&mut self) -> Result<Expr> {
        let Token { kind, span } = self.token;

        let operator = self.operator(kind, span)?;

        if self.prev.kind == TokenKind::OpeningParen
        && self.peek.kind == TokenKind::ClosingParen {
            self.bump();
            return Ok(Expr::Operator(operator));
        }

        self.bump();

        if !self.match_lines() {
            return Err(NitrineError::error(
                self.span(),
                "Unary (infix) or partial operators must be in the same line as the operand".into()))
        }

        let rhs = self.term(false)?;
        Ok(Expr::Unary(box Unary { operator, rhs, span: self.complete(span) }))
    }

    fn binary(&mut self, minimum_precedence: u8) -> Result<Expr> {
        let mut lhs = self.term(true)?;

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

            let function = Expr::Operator(operator);

            let span = lhs.span();

            let arguments = vec![
                lhs,
                self.binary(precedence + fix)?
            ];

            lhs = Expr::Apply(box Apply { function, arguments, span: self.complete(span) });
        }

        Ok(lhs)
    }

    fn operator(&self, kind: TokenKind, span: Span) -> Result<Operator> {
        match kind {
            TokenKind::Add
          | TokenKind::Sub
          | TokenKind::Mul
          | TokenKind::Div
          | TokenKind::Rem
          | TokenKind::Concat
          | TokenKind::BitAnd
          | TokenKind::BitOr
          | TokenKind::BitXor
          | TokenKind::BitShr
          | TokenKind::BitShl
          | TokenKind::BitNot
          | TokenKind::And
          | TokenKind::Or
          | TokenKind::Not
          | TokenKind::Is
          | TokenKind::Eq
          | TokenKind::Ne
          | TokenKind::Lt
          | TokenKind::Le
          | TokenKind::Gt
          | TokenKind::Ge
          | TokenKind::Pipe => Ok(Operator { kind, span }),
            _ => {
                return Err(NitrineError::error(
                    span,
                    format!("Operator `{}` is not a valid binary (infix) operator", kind)));
            }
        }
    }

    fn cond(&mut self) -> Result<Expr> {
        let span = self.span();

        self.eat(TokenKind::If)?;
        let test = self.expr()?;

        self.eat(TokenKind::Then)?;
        let then = self.expr()?;

        self.eat(TokenKind::Else)?;
        let otherwise = self.expr()?;

        Ok(Expr::If(box If { test, then, otherwise, span: self.complete(span) }))
    }

    fn lambda(&mut self) -> Result<Expr> {
        let span = self.span();

        self.enter(Nesting::Fun);

        let parameters = self.block_of(
            TokenKind::Fn,
            TokenKind::Arrow,
            Self::name)?;

        let value = self.expr()?;

        self.exit(Nesting::Fun)?;

        Ok(Expr::Fn(box Fn { parameters, value, span: self.complete(span) }))
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

    fn enter(&mut self, nesting: Nesting) {
        self.depth.push(nesting)
    }

    fn exit(&mut self, nesting: Nesting) -> Result<()> {
        if !self.nesting(nesting) {
            return Err(NitrineError::basic("Compiler Error: Invalid nesting order".into()))
        }

        self.depth.pop();
        Ok(())
    }

    fn nesting(&self, nesting: Nesting) -> bool {
        self.depth.last().map(|it| it == &nesting).unwrap_or(false)
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

    let mut nodes = vec![];

    while !parser.done() {
        nodes.push(parser.expr()?);
    }

    let name = source.path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    Ok(Module { name, nodes })
}