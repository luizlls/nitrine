use crate::{Source, Span};
use crate::ast::*;
use crate::token::{Token, TokenKind, TokenKindError, Associativity};
use crate::lexer::Lexer;
use crate::error::{Result, NitrineError};

pub struct Parser<'s> {
    lexer: Lexer<'s>,
    prev: Token,
    token: Token,
    peek: Token,
    source: &'s Source,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s Source) -> Parser {
        let mut parser = Parser {
            lexer: Lexer::new(&source.content),
            prev: Token::default(),
            token: Token::default(),
            peek: Token::default(),
            source,
        };

        parser.bump(); // token
        parser.bump(); // peek

        parser
    }

    fn parse(&mut self) -> Result<Function> {
        self.function()
    }

    fn function(&mut self) -> Result<Function> {
        let start = self.span();

        let name = self.name()?;

        let parameters = if self.matches(TokenKind::OpeningParen) {
            self.block_of(
                TokenKind::OpeningParen,
                TokenKind::ClosingParen,
                Self::name)?
        } else {
            vec![]
        };

        self.eat(TokenKind::Equals)?;

        let body = self.expr()?;

        let span = start.to(body.span);

        Ok(Function { name, parameters, body, span })
    }

    fn name(&mut self) -> Result<Name> {
        let Token { span, .. } = self.eat(TokenKind::Lower)?;
        let name = self.source.content[span.range()].into();
        Ok(Name { name, span })
    }

    fn expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Do => {
                self.block()
            }
            TokenKind::If => {
                self.cond()
            }
            TokenKind::Fn => {
                self.lambda()
            }
            _ => {
                self.binary(0)
            }
        }
    }

    fn term(&mut self) -> Result<Expr> {
        let term = match self.token.kind {
            TokenKind::Lower => {
                if self.peek_is(TokenKind::OpeningBrace) {
                    self.object()?
                } else {
                    let span = self.span();
                    let name = self.name()?;
                    self.make(ExprKind::Name { name }, span)?
                }
            }
            TokenKind::Upper => {
                self.symbol()?
            }
            TokenKind::Number => {
                self.number()?
            }
            TokenKind::String(_) => {
                self.template()?
            }
            TokenKind::OpeningParen => {
                self.parens()?
            }
            TokenKind::OpeningBrace => {
                self.object()?
            }
            TokenKind::OpeningBracket => {
                self.list()?
            }
            tk if tk.is_prefix() => {
                self.unary()?
            }
            _ => {
                return Err(self.handle_unexpected())
            }
        };

        self.apply(term)
    }

    fn apply(&mut self, fun: Expr) -> Result<Expr> {
        let span = fun.span;

        let valid = match &fun.kind {
            ExprKind::Name  { .. }
          | ExprKind::Group { .. }
          | ExprKind::Block { .. } => true,
            _ => false
        };

        if !valid {
            return Ok(fun);
        }

        let mut expr = fun;

        loop {
            if self.matches(TokenKind::OpeningParen) && self.same_line(self.prev.span.line) {
                let args = self.block_of(
                    TokenKind::OpeningParen,
                    TokenKind::ClosingParen,
                    Self::expr)?;
                expr = self.make(ExprKind::Apply { function: box expr, args }, span)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn symbol(&mut self) -> Result<Expr> {
        let span = self.span();

        let name = self.name()?;

        let value = match self.token.kind {
            TokenKind::Lower
          | TokenKind::Upper
          | TokenKind::OpeningParen
          | TokenKind::OpeningBrace
          | TokenKind::OpeningBracket
          | TokenKind::Number
          | TokenKind::String(true) if self.same_line(span.line) => {
                Some(box self.term()?)
            }
            _ => None
        };

        self.make(ExprKind::Symbol { name, value }, span)
    }

    fn number(&mut self) -> Result<Expr> {
        let Token { span, .. } = self.eat(TokenKind::Number)?;

        let number = ExprKind::Number {
            value: self.source.content[span.range()].into()
        };

        self.make(number, span)
    }

    fn template(&mut self) -> Result<Expr> {
        let span = self.span();

        let mut parts = vec![];

        loop {
            match self.token.kind {
                TokenKind::String(false) => {
                    parts.push(self.string(false)?);
                }
                TokenKind::String(true) => {
                    parts.push(self.string(true)?);
                    break;
                }
                TokenKind::OpeningBrace => {
                    self.eat(TokenKind::OpeningBrace)?;
                    parts.push(self.expr()?);
                    self.eat(TokenKind::ClosingBrace)?;
                }
                _ => { break; }
            }
        }

        let parts = parts
            .into_iter()
            .filter(|part| {
                match part.kind {
                    ExprKind::String { ref value } if value.is_empty() => {
                        false
                    }
                    _ => true
                }
            })
            .collect();

        self.make(ExprKind::Template { parts }, span)
    }

    fn string(&mut self, done: bool) -> Result<Expr> {
        let Token { span, .. } = self.eat(TokenKind::String(done))?;

        let string = ExprKind::String {
            value: self.source.content[span.range()].into()
        };

        self.make(string, span)
    }

    fn list(&mut self) -> Result<Expr> {
        let span = self.span();

        let items = self.block_of(
            TokenKind::OpeningBracket,
            TokenKind::ClosingBracket,
            Self::expr)?;

        self.make(ExprKind::List { items }, span)
    }

    fn parens(&mut self) -> Result<Expr> {
        let span = self.span();

        let items = self.block_of(
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
            Self::expr)?;

        if items.len() == 1 {
            let inner = box items.into_iter().next().unwrap();
            self.make(ExprKind::Group { inner }, span)
        } else {
            self.make(ExprKind::Tuple { items }, span)
        }
    }

    fn object(&mut self) -> Result<Expr> {
        let span = self.span();

        let base = if self.matches(TokenKind::Lower) {
            Some(self.name()?)
        } else {
            None
        };

        if base.is_some() && !self.same_line(span.line) {
            return Err(NitrineError::error(
                self.span(),
                "The base object and the opening brace `{` must be in the same line".into()));
        }

        self.eat(TokenKind::OpeningBrace)?;

        let mut props = vec![];

        loop {
            if self.matches(TokenKind::ClosingBrace) {
                break;
            }

            props.push(self.object_key_val()?);

            if !self.maybe_eat(TokenKind::Comma) {
                break;
            }
        }

        self.eat(TokenKind::ClosingBrace)?;

        self.make(ExprKind::Object { base, props }, span)
    }

    fn object_key_val(&mut self) -> Result<(Name, Option<Expr>)> {
        let key = self.name()?;

        let val = if self.maybe_eat(TokenKind::Equals) {
            Some(self.expr()?)
        } else {
            None
        };

        Ok((key, val))
    }

    fn unary(&mut self) -> Result<Expr> {
        let span = self.span();

        let kind = match self.token.kind {
            TokenKind::Not => OperatorKind::Not,
            TokenKind::Sub => OperatorKind::Neg,
            TokenKind::Add => OperatorKind::Pos,
            TokenKind::BitNot => OperatorKind::BitNot,
            _ => {
                return Err(NitrineError::error(
                    self.span(),
                    format!("Operator `{}` is not a valid unary (prefix) operator", self.token.kind)));
            }
        };

        let operator = Operator { kind, span };

        self.bump();

        let rhs = box self.term()?;

        self.make(ExprKind::Unary { operator, rhs }, span)
    }

    fn binary(&mut self, min_precedence: u8) -> Result<Expr> {
        let mut lhs = self.term()?;

        while let Some((precedence, associativity)) = self.token.kind.precedence() {
            if precedence < min_precedence {
                break;
            }

            let Token { kind, span } = self.token;

            let operator = self.operator(kind, span)?;

            self.bump();

            let fix = match associativity {
                Associativity::Right => 0,
                Associativity::Left  => 1,
                Associativity::None  => 1,
            };

            let rhs = self.binary(precedence + fix)?;

            let span = lhs.span;

            lhs = self.make(ExprKind::Binary { operator, lhs: box lhs, rhs: box rhs }, span)?;
        }

        Ok(lhs)
    }

    fn operator(&self, kind: TokenKind, span: Span) -> Result<Operator> {
        let kind = match kind {
            TokenKind::Add => OperatorKind::Add,
            TokenKind::Sub => OperatorKind::Sub,
            TokenKind::Mul => OperatorKind::Mul,
            TokenKind::Div => OperatorKind::Div,
            TokenKind::Rem => OperatorKind::Rem,
            TokenKind::And => OperatorKind::And,
            TokenKind::Or  => OperatorKind::Or,
            TokenKind::Is  => OperatorKind::Is,
            TokenKind::Not => OperatorKind::Not,
            TokenKind::Eq  => OperatorKind::Eq,
            TokenKind::Ne  => OperatorKind::Ne,
            TokenKind::Lt  => OperatorKind::Lt,
            TokenKind::Le  => OperatorKind::Le,
            TokenKind::Gt  => OperatorKind::Gt,
            TokenKind::Ge  => OperatorKind::Ge,
            TokenKind::Concat => OperatorKind::Concat,
            TokenKind::Pipe   => OperatorKind::Pipe,
            TokenKind::BitAnd => OperatorKind::BitAnd,
            TokenKind::BitOr  => OperatorKind::BitOr,
            TokenKind::BitNot => OperatorKind::BitNot,
            TokenKind::BitXor => OperatorKind::BitXor,
            TokenKind::BitShr => OperatorKind::BitShr,
            TokenKind::BitShl => OperatorKind::BitShl,
            TokenKind::Dot    => OperatorKind::Member,
            TokenKind::Equals => OperatorKind::Bind,
            _ => {
                return Err(NitrineError::error(
                    span,
                    format!("Operator `{}` is not a valid binary (infix) operator", self.token.kind)));
            }
        };

        Ok(Operator { kind, span })
    }

    fn block(&mut self) -> Result<Expr> {
        let span = self.span();

        self.eat(TokenKind::Do)?;
        self.eat(TokenKind::OpeningBrace)?;

        let mut items = vec![];

        loop {
            if self.matches(TokenKind::ClosingBrace) {
                break;
            }
            items.push(self.expr()?);
        }

        self.eat(TokenKind::ClosingBrace)?;

        self.make(ExprKind::Block { items }, span)
    }

    fn cond(&mut self) -> Result<Expr> {
        let span = self.span();

        self.eat(TokenKind::If)?;
        let test = box self.expr()?;

        let then = if self.matches(TokenKind::Do) {
            box self.block()?
        } else {
            self.eat(TokenKind::Then)?;
            box self.expr()?
        };

        self.eat(TokenKind::Else)?;
        let otherwise = box self.expr()?;

        self.make(ExprKind::If { test, then, otherwise }, span)
    }

    fn lambda(&mut self) -> Result<Expr> {
        let span = self.span();

        let parameters = self.block_of(
            TokenKind::Fn,
            TokenKind::Arrow,
            Self::name)?;

        let body = box self.expr()?;

        self.make(ExprKind::Lambda { parameters, body }, span)
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

    fn make(&self, kind: ExprKind, span: Span) -> Result<Expr> {
        Ok(Expr { kind, span: span.to(self.prev.span) })
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

    fn peek_is(&self, kind: TokenKind) -> bool {
        self.peek.kind == kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn same_line(&self, line: u32) -> bool {
        self.span().line == line
    }

    fn handle_unexpected(&mut self) -> NitrineError {
        let Token { kind, span } = self.token;

        let msg = match kind {
            TokenKind::Error(TokenKindError::InvalidCharacter) => {
                "Invalid character".into()
            }
            TokenKind::Error(TokenKindError::InvalidEscape) => {
                "Invalid escape character".into()
            }
            TokenKind::Error(TokenKindError::InvalidOperator) => {
                "Invalid operator".into()
            }
            TokenKind::Error(TokenKindError::UnterminatedString) => {
                "Unterminated string".into()
            }
            _ => format!("Unexpected `{}`", kind)
        };

        NitrineError::error(span, msg)
    }
}

impl Iterator for Parser<'_> {
    type Item = Result<Function>;

    fn next(&mut self) -> Option<Result<Function>> {
        if !self.done() {
            Some(self.parse())
        } else {
            None
        }
    }
}