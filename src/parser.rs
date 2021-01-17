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

    pub fn parse(&mut self) -> Result<Program> {
        let mut nodes = vec![];

        while !self.done() {
            nodes.push(self.top_level()?);
        }

        let name = self.source.path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        Ok(Program { name, nodes })
    }

    fn top_level(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::Ident => {
                self.definition()
            }
            _ => {
                Err(NitrineError::error(
                    self.span(), "expected a constant or function definition".into()))
            }
        }
    }

    fn definition(&mut self) -> Result<Node> {
        let span = self.span();

        let name = self.name()?;

        let value = box if self.matches(TokenKind::OpeningParen) {
            self.function()?
        } else {
            if !self.match_line() {
                return Err(NitrineError::error(
                    self.span(),
                    "A constant definition expects the `=` to be in the same line".into()));
            }
            self.eat(TokenKind::Equals)?;
            self.expr()?
        };

        self.make(NodeKind::Let { name, value }, span)
    }

    fn function(&mut self) -> Result<Node> {

        if !self.match_line() {
            return Err(NitrineError::error(
                self.span(),
                "A function definition expects then opening parenthesis to be in the same line".into()));
        }

        let span = self.span();

        let parameters = self.block_of(
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
            Self::name)?;

        self.eat(TokenKind::Equals)?;

        let value = box self.expr()?;

        self.make(NodeKind::Function { parameters, value }, span)
    }

    fn name(&mut self) -> Result<Name> {
        let Token { span, .. } = self.eat(TokenKind::Ident)?;
        let name = self.source.content[span.range()].into();
        Ok(Name { name, span })
    }

    fn expr(&mut self) -> Result<Node> {
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

    fn term(&mut self, apply: bool) -> Result<Node> {
        match self.token.kind {
            TokenKind::Ident => {
                let term = self.ident()?;
                if apply { self.apply(term) } else { Ok(term) }
            }
            TokenKind::Symbol => {
                self.symbol()
            }
            TokenKind::Number => {
                self.number()
            }
            TokenKind::String(_) => {
                self.template()
            }
            TokenKind::True
          | TokenKind::False => {
                self.boolean()
            }
            TokenKind::OpeningParen => {
                let term = self.parens()?;
                if apply { self.apply(term) } else { Ok(term) }
            }
            TokenKind::OpeningBrace => {
                self.object()
            }
            TokenKind::OpeningBracket => {
                self.list()
            }
            tk if tk.is_prefix() => {
                self.unary()
            }
            _ => {
                return Err(self.handle_unexpected())
            }
        }
    }

    fn apply(&mut self, callee: Node) -> Result<Node> {
        let mut arguments = vec![];

        loop {
            if !self.match_line() {
                break;
            }

            match self.token.kind {
                TokenKind::Ident
              | TokenKind::Symbol
              | TokenKind::Number
              | TokenKind::String(_)
              | TokenKind::True
              | TokenKind::False
              | TokenKind::OpeningParen
              | TokenKind::OpeningBrace
              | TokenKind::OpeningBracket => {
                    arguments.push(self.term(false)?)
                }
                _ => { break; }
            }
        }

        if arguments.is_empty() {
            Ok(callee)
        } else {
            let span = callee.span;
            self.make(NodeKind::Apply { function: box callee, arguments }, span)
        }
    }

    fn ident(&mut self) -> Result<Node> {
        let span = self.span();
        let name = self.name()?;
        self.make(NodeKind::Name { name }, span)
    }

    fn symbol(&mut self) -> Result<Node> {
        let Token { span, .. } = self.eat(TokenKind::Symbol)?;

        let name = self.source.content[span.range()].to_string();
        let name = Name { name, span };

        self.make(NodeKind::Symbol { name }, span)
    }

    fn template(&mut self) -> Result<Node> {
        let span = self.span();

        let mut parts = vec![];

        loop {
            match self.token.kind {
                TokenKind::String(done) => {
                    parts.push(self.string(done)?);
                    if done {
                        break;
                    }
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
                    NodeKind::String { ref value } if value.is_empty() => {
                        false
                    }
                    _ => true
                }
            })
            .collect();

        self.make(NodeKind::Template { parts }, span)
    }

    fn string(&mut self, done: bool) -> Result<Node> {
        let Token { span, .. } = self.eat(TokenKind::String(done))?;

        let string = NodeKind::String {
            value: self.source.content[span.range()].into()
        };

        self.make(string, span)
    }

    fn number(&mut self) -> Result<Node> {
        let Token { span, .. } = self.eat(TokenKind::Number)?;

        let value = self.source.content[span.range()].to_string();

        let number = if value.contains(".") {
            NodeKind::Float { value: value.parse().unwrap() }
        } else {
            NodeKind::Integer { value: value.parse().unwrap() }
        };

        self.make(number, span)
    }

    fn boolean(&mut self) -> Result<Node> {
        let span = self.span();

        let boolean = NodeKind::Bool {
            value: self.matches(TokenKind::True)
        };

        self.bump();

        self.make(boolean, span)
    }

    fn list(&mut self) -> Result<Node> {
        let span = self.span();

        let items = self.block_of(
            TokenKind::OpeningBracket,
            TokenKind::ClosingBracket,
            Self::expr)?;

        self.make(NodeKind::List { items }, span)
    }

    fn parens(&mut self) -> Result<Node> {
        let span = self.span();

        let items = self.block_of(
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
            Self::expr)?;

        if items.len() == 1 {
            Ok(items.into_iter().next().unwrap())
        } else {
            self.make(NodeKind::Tuple { items }, span)
        }
    }

    fn object(&mut self) -> Result<Node> {
        let span = self.span();

        self.eat(TokenKind::OpeningBrace)?;

        let mut properties = vec![];

        loop {
            if self.matches(TokenKind::ClosingBrace) {
                break;
            }

            properties.push(self.property()?);

            if !self.maybe_eat(TokenKind::Comma) {
                break;
            }
        }

        self.eat(TokenKind::ClosingBrace)?;

        self.make(NodeKind::Object { properties }, span)
    }

    fn property(&mut self) -> Result<(Name, Option<Node>)> {
        let key = self.name()?;

        if !self.matches(TokenKind::Comma)
        && !self.matches(TokenKind::ClosingBrace)
        && !self.match_line()
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

    fn unary(&mut self) -> Result<Node> {
        let span = self.span();

        let kind = match self.token.kind {
            TokenKind::Not    => OperatorKind::Not,
            TokenKind::Sub    => OperatorKind::Neg,
            TokenKind::Add    => OperatorKind::Pos,
            TokenKind::BitNot => OperatorKind::BitNot,
            _ => {
                return Err(NitrineError::error(
                    self.span(),
                    format!("Operator `{}` is not a valid unary (prefix) operator", self.token.kind)));
            }
        };

        let operator = Operator { kind, span };

        self.bump();

        let rhs = box self.term(false)?;

        self.make(NodeKind::Unary { operator, rhs }, span)
    }

    fn binary(&mut self, required_precedence: u8) -> Result<Node> {
        let mut lhs = self.term(true)?;

        while let Some((precedence, associativity)) = self.token.kind.precedence() {
            if precedence < required_precedence {
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

            lhs = self.make(NodeKind::Binary { operator, lhs: box lhs, rhs: box rhs }, span)?;
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
            TokenKind::Semi   => OperatorKind::Chain,
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

    fn block(&mut self) -> Result<Node> {
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

        self.make(NodeKind::Block { items }, span)
    }

    fn cond(&mut self) -> Result<Node> {
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

        self.make(NodeKind::If { test, then, otherwise }, span)
    }

    fn lambda(&mut self) -> Result<Node> {
        let span = self.span();

        let parameters = self.block_of(
            TokenKind::Fn,
            TokenKind::Arrow,
            Self::name)?;

        let value = box self.expr()?;

        self.make(NodeKind::Function { parameters, value }, span)
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

    fn make(&self, kind: NodeKind, span: Span) -> Result<Node> {
        Ok(Node { kind, span: span.to(self.prev.span) })
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

    fn match_line(&self) -> bool {
        self.token.span.line == self.prev.span.line
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
