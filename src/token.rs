use std::fmt;

use crate::Span;

#[derive(Debug, Clone, Copy, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Symbol(SymbolKind),

    Keyword(KeywordKind),

    Operator(OperatorKind),

    Literal(LiteralKind),

    Template(TemplateKind),

    Error(ErrorKind),

    EOF,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolKind {
    OpeningParen,
    ClosingParen,
    OpeningBrace,
    ClosingBrace,
    OpeningBracket,
    ClosingBracket,
    Dot,
    Comma,
    Colon,
    Semi,   // ;
    Arrow,  // ->
    Equals, // =
    Warlus, // :=
    Any, // _
}


impl fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SymbolKind::OpeningParen => write!(f, "("),
            SymbolKind::ClosingParen => write!(f, ")"),
            SymbolKind::OpeningBrace => write!(f, "{{"),
            SymbolKind::ClosingBrace => write!(f, "}}"),
            SymbolKind::OpeningBracket => write!(f, "["),
            SymbolKind::ClosingBracket => write!(f, "]"),
            SymbolKind::Dot => write!(f, "."),
            SymbolKind::Comma => write!(f, ","),
            SymbolKind::Colon => write!(f, ":"),
            SymbolKind::Semi => write!(f, ";"),
            SymbolKind::Arrow => write!(f, "->"),
            SymbolKind::Equals => write!(f, "="),
            SymbolKind::Warlus => write!(f, ":="),
            SymbolKind::Any => write!(f, "_"),
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeywordKind {
    Fn,
    Mut,
    If,
    Then,
    Else,
    For,
    Match,
}


impl fmt::Display for KeywordKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            KeywordKind::Fn => write!(f, "fn"),
            KeywordKind::If => write!(f, "if"),
            KeywordKind::Then => write!(f, "then"),
            KeywordKind::Else => write!(f, "else"),
            KeywordKind::For => write!(f, "for"),
            KeywordKind::Mut => write!(f, "mut"),
            KeywordKind::Match => write!(f, "match"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OperatorKind {
    Add,     // +
    Sub,     // -
    Mul,     // *
    Div,     // /
    Rem,     // %
    And,     // and
    Or,      // or
    Not,     // not
    Concat,  // ++
    BitAnd,  // &&&
    BitOr,   // |||
    BitNot,  // ~~~
    BitXor,  // ^^^
    BitShr,  // >>>
    BitShl,  // <<<
    Eq,      // ==
    Ne,      // !=
    Lt,      // <
    Le,      // <=
    Gt,      // >
    Ge,      // >=
    LPipe,   // |>
    RPipe,   // <|
}

pub type Precedence = u8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    None,
}

impl OperatorKind {

    pub fn precedence(&self) -> Option<(Precedence, Associativity)> {
        match self {
            OperatorKind::Div    => Some((10, Associativity::Left)),
            OperatorKind::Mul    => Some((10, Associativity::Left)),
            OperatorKind::Rem    => Some((10, Associativity::Left)),
            OperatorKind::Add    => Some(( 9, Associativity::Left)),
            OperatorKind::Sub    => Some(( 9, Associativity::Left)),
            OperatorKind::Concat => Some(( 9, Associativity::Left)),
            OperatorKind::BitShr => Some(( 8, Associativity::Left)),
            OperatorKind::BitShl => Some(( 8, Associativity::Left)),
            OperatorKind::BitAnd => Some(( 5, Associativity::Left)),
            OperatorKind::BitXor => Some(( 6, Associativity::Left)),
            OperatorKind::BitOr  => Some(( 5, Associativity::Left)),
            OperatorKind::Lt     => Some(( 4, Associativity::Left)),
            OperatorKind::Le     => Some(( 4, Associativity::Left)),
            OperatorKind::Gt     => Some(( 4, Associativity::Left)),
            OperatorKind::Ge     => Some(( 4, Associativity::Left)),
            OperatorKind::Ne     => Some(( 4, Associativity::Left)),
            OperatorKind::Eq     => Some(( 4, Associativity::Left)),
            OperatorKind::And    => Some(( 3, Associativity::Left)),
            OperatorKind::Or     => Some(( 2, Associativity::Left)),
            OperatorKind::LPipe  => Some(( 1, Associativity::Left)),
            OperatorKind::RPipe  => Some(( 1, Associativity::Left)),
            _ => None
        }
    }
}

impl fmt::Display for OperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OperatorKind::And => write!(f, "and"),
            OperatorKind::Or => write!(f, "or"),
            OperatorKind::Not => write!(f, "not"),
            OperatorKind::Add => write!(f, "+"),
            OperatorKind::Sub => write!(f, "-"),
            OperatorKind::Mul => write!(f, "*"),
            OperatorKind::Div => write!(f, "/"),
            OperatorKind::Rem => write!(f, "%"),
            OperatorKind::Eq => write!(f, "=="),
            OperatorKind::Ne => write!(f, "!="),
            OperatorKind::Lt => write!(f, "<"),
            OperatorKind::Le => write!(f, "<="),
            OperatorKind::Gt => write!(f, ">"),
            OperatorKind::Ge => write!(f, ">="),
            OperatorKind::Concat => write!(f, "++"),
            OperatorKind::BitAnd => write!(f, "&&&"),
            OperatorKind::BitOr  => write!(f, "|||"),
            OperatorKind::BitNot => write!(f, "~~~"),
            OperatorKind::BitXor => write!(f, "^^^"),
            OperatorKind::BitShr => write!(f, ">>>"),
            OperatorKind::BitShl => write!(f, "<<<"),
            OperatorKind::LPipe => write!(f, "|>"),
            OperatorKind::RPipe => write!(f, "<|"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LiteralKind {
    Lower,
    Upper,
    Number,
    String,
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LiteralKind::Lower => write!(f, "lowercase identifier"),
            LiteralKind::Upper => write!(f, "uppercase identifier"),
            LiteralKind::Number => write!(f, "number"),
            LiteralKind::String => write!(f, "string"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TemplateKind {
    StringStart,
    StringEnd,
    StringFragment,
}

impl fmt::Display for TemplateKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TemplateKind::StringStart => write!(f, "start of string template"),
            TemplateKind::StringEnd => write!(f, "end of string template"),
            TemplateKind::StringFragment => write!(f, "fragment of string template"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorKind {
    InvalidCharacter,
    InvalidOperator,
    InvalidEscape,
    UnterminatedString,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::InvalidCharacter => write!(f, "invalid character"),
            ErrorKind::InvalidEscape => write!(f, "invalid escape"),
            ErrorKind::InvalidOperator => write!(f, "invalid operator"),
            ErrorKind::UnterminatedString => write!(f, "unterminated string"),
        }
    }
}

impl Default for TokenKind {
    fn default() -> TokenKind { TokenKind::EOF }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenKind::Symbol(symbol) => write!(f, "{}", symbol),
            TokenKind::Keyword(keyword) => write!(f, "{}", keyword),
            TokenKind::Operator(operator) => write!(f, "{}", operator),
            TokenKind::Literal(literal) => write!(f, "{}", literal),
            TokenKind::Template(template) => write!(f, "{}", template),
            TokenKind::Error(error) => write!(f, "{}", error),
            TokenKind::EOF => write!(f, "end of file"),
        }
    }
}

pub fn get_keyword(key: &str) -> Option<TokenKind> {
    match key {
        "fn"    => Some(TokenKind::Keyword(KeywordKind::Fn)),
        "if"    => Some(TokenKind::Keyword(KeywordKind::If)),
        "then"  => Some(TokenKind::Keyword(KeywordKind::Then)),
        "else"  => Some(TokenKind::Keyword(KeywordKind::Else)),
        "for"   => Some(TokenKind::Keyword(KeywordKind::For)),
        "mut"   => Some(TokenKind::Keyword(KeywordKind::Mut)),
        "match" => Some(TokenKind::Keyword(KeywordKind::Match)),
        "and"   => Some(TokenKind::Operator(OperatorKind::And)),
        "or"    => Some(TokenKind::Operator(OperatorKind::Or)),
        "not"   => Some(TokenKind::Operator(OperatorKind::Not)),
        _ => None,
    }
}

pub fn get_operator(key: &str) -> Option<TokenKind> {
    match key {
        ":"   => Some(TokenKind::Symbol(SymbolKind::Colon)),
        "."   => Some(TokenKind::Symbol(SymbolKind::Dot)),
        "="   => Some(TokenKind::Symbol(SymbolKind::Equals)),
        ":="  => Some(TokenKind::Symbol(SymbolKind::Warlus)),
        "->"  => Some(TokenKind::Symbol(SymbolKind::Arrow)),
        "+"   => Some(TokenKind::Operator(OperatorKind::Add)),
        "-"   => Some(TokenKind::Operator(OperatorKind::Sub)),
        "*"   => Some(TokenKind::Operator(OperatorKind::Mul)),
        "/"   => Some(TokenKind::Operator(OperatorKind::Div)),
        "%"   => Some(TokenKind::Operator(OperatorKind::Rem)),
        "=="  => Some(TokenKind::Operator(OperatorKind::Eq)),
        "!="  => Some(TokenKind::Operator(OperatorKind::Ne)),
        "<"   => Some(TokenKind::Operator(OperatorKind::Lt)),
        "<="  => Some(TokenKind::Operator(OperatorKind::Le)),
        ">"   => Some(TokenKind::Operator(OperatorKind::Gt)),
        ">="  => Some(TokenKind::Operator(OperatorKind::Ge)),
        "&&&" => Some(TokenKind::Operator(OperatorKind::BitAnd)),
        "|||" => Some(TokenKind::Operator(OperatorKind::BitOr)),
        "~~~" => Some(TokenKind::Operator(OperatorKind::BitNot)),
        "^^^" => Some(TokenKind::Operator(OperatorKind::BitXor)),
        ">>>" => Some(TokenKind::Operator(OperatorKind::BitShl)),
        "<<<" => Some(TokenKind::Operator(OperatorKind::BitShr)),
        "++"  => Some(TokenKind::Operator(OperatorKind::Concat)),
        "|>"  => Some(TokenKind::Operator(OperatorKind::LPipe)),
        "<|"  => Some(TokenKind::Operator(OperatorKind::RPipe)),
        _ => None
    }
}
