use std::fmt;

use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // symbols
    OpeningParen,
    ClosingParen,
    OpeningBrace,
    ClosingBrace,
    OpeningBracket,
    ClosingBracket,
    Semi,
    Comma,
    Dot,
    Colon,
    Arrow,

    // keywords
    Do,
    If,
    Fn,
    Then,
    Else,
    True,
    False,

    // operators
    Add,     // +
    Sub,     // -
    Mul,     // *
    Div,     // /
    Rem,     // %
    And,     // and
    Or,      // or
    BitAnd,  // &&&
    BitOr,   // |||
    BitXor,  // ^^^
    BitShr,  // >>>
    BitShl,  // <<<
    Concat,  // ++
    Not,     // not
    BitNot,  // ~~~
    Is,      // is
    Eq,      // ==
    Ne,      // !=
    Lt,      // <
    Le,      // <=
    Gt,      // >
    Ge,      // >=
    Pipe,    // |>

    Equals,
    Warlus,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    RemEq,
    BitAndEq,
    BitOrEq,
    BitXorEq,
    BitShrEq,
    BitShlEq,
    ConcatEq,

    // values
    Ident,
    Symbol,
    Number,
    String(bool), // done?

    // string template

    // others
    EOF,

    Error(TokenKindError),

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKindError {
    InvalidCharacter,

    InvalidOperator,

    InvalidEscape,

    Unterminated,
}

impl Default for TokenKind {
    fn default() -> TokenKind { TokenKind::EOF }
}

pub fn get_keyword(key: &str) -> Option<TokenKind> {
    match key {
        "do" => Some(TokenKind::Do),
        "if" => Some(TokenKind::If),
        "then" => Some(TokenKind::Then),
        "else" => Some(TokenKind::Else),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "fn" => Some(TokenKind::Fn),
        "is" => Some(TokenKind::Is),
        "and" => Some(TokenKind::And),
        "or" => Some(TokenKind::Or),
        "not" => Some(TokenKind::Not),
        _ => None,
    }
}

pub fn get_operator(key: &str) -> Option<TokenKind> {
    match key {
        ":" => Some(TokenKind::Colon),
        "." => Some(TokenKind::Dot),
        "=" => Some(TokenKind::Equals),
        ":=" => Some(TokenKind::Warlus),
        "->" => Some(TokenKind::Arrow),
        "+" => Some(TokenKind::Add),
        "-" => Some(TokenKind::Sub),
        "*" => Some(TokenKind::Mul),
        "/" => Some(TokenKind::Div),
        "%" => Some(TokenKind::Rem),
        "&&&" => Some(TokenKind::BitAnd),
        "|||" => Some(TokenKind::BitOr),
        "^^^" => Some(TokenKind::BitXor),
        ">>>" => Some(TokenKind::BitShl),
        "<<<" => Some(TokenKind::BitShr),
        "~~~" => Some(TokenKind::BitNot),
        "++" => Some(TokenKind::Concat),
        "|>" => Some(TokenKind::Pipe),
        "==" => Some(TokenKind::Eq),
        "!=" => Some(TokenKind::Ne),
        "<" => Some(TokenKind::Lt),
        "<=" => Some(TokenKind::Le),
        ">" => Some(TokenKind::Gt),
        ">=" => Some(TokenKind::Ge),
        "+=" => Some(TokenKind::AddEq),
        "-=" => Some(TokenKind::SubEq),
        "*=" => Some(TokenKind::MulEq),
        "/=" => Some(TokenKind::DivEq),
        "%=" => Some(TokenKind::RemEq),
        "&&&=" => Some(TokenKind::BitAndEq),
        "|||=" => Some(TokenKind::BitOrEq),
        "^^^=" => Some(TokenKind::BitXorEq),
        ">>>=" => Some(TokenKind::BitShlEq),
        "<<<=" => Some(TokenKind::BitShrEq),
        "++=" => Some(TokenKind::ConcatEq),
        _ => None
    }
}

pub type Precedence = u8;

#[derive(Debug, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
    None,
}

impl TokenKind {

    pub fn is_operator(&self) -> bool {
        match self {
            TokenKind::Add
          | TokenKind::Sub
          | TokenKind::Mul
          | TokenKind::Div
          | TokenKind::Rem
          | TokenKind::And
          | TokenKind::Or
          | TokenKind::BitAnd
          | TokenKind::BitOr
          | TokenKind::BitXor
          | TokenKind::BitShr
          | TokenKind::BitShl
          | TokenKind::Concat
          | TokenKind::Not
          | TokenKind::BitNot
          | TokenKind::Eq
          | TokenKind::Ne
          | TokenKind::Lt
          | TokenKind::Le
          | TokenKind::Gt
          | TokenKind::Ge
          | TokenKind::Is
          | TokenKind::Pipe
          | TokenKind::Semi
          | TokenKind::Dot
          | TokenKind::AddEq
          | TokenKind::SubEq
          | TokenKind::MulEq
          | TokenKind::DivEq
          | TokenKind::RemEq
          | TokenKind::BitAndEq
          | TokenKind::BitOrEq
          | TokenKind::BitXorEq
          | TokenKind::BitShrEq
          | TokenKind::BitShlEq
          | TokenKind::ConcatEq
          | TokenKind::Warlus
          | TokenKind::Equals => true,
          _ => false
        }
    }

    pub fn is_prefix(&self) -> bool {
        match self {
            TokenKind::BitNot
          | TokenKind::Not
          | TokenKind::Add
          | TokenKind::Sub => true,
          _ => false
        }
    }

    pub fn precedence(&self) -> Option<(Precedence, Associativity)> {
        match self {
          | TokenKind::Div
          | TokenKind::Mul
          | TokenKind::Rem => Some((12, Associativity::Left)),
          | TokenKind::Add
          | TokenKind::Sub
          | TokenKind::Concat => Some((11, Associativity::Left)),
            TokenKind::BitShr
          | TokenKind::BitShl => Some((10, Associativity::Left)),
            TokenKind::BitAnd => Some((9, Associativity::Left)),
            TokenKind::BitXor => Some((8, Associativity::Left)),
            TokenKind::BitOr  => Some((7, Associativity::Left)),
          | TokenKind::Lt
          | TokenKind::Le
          | TokenKind::Gt
          | TokenKind::Ge
          | TokenKind::Ne
          | TokenKind::Eq => Some((6, Associativity::Left)),
            TokenKind::And => Some((5, Associativity::Left)),
            TokenKind::Or => Some((4, Associativity::Left)),
            TokenKind::Pipe => Some((3, Associativity::Left)),
            TokenKind::Dot => Some((2, Associativity::Left)),
          | TokenKind::AddEq
          | TokenKind::SubEq
          | TokenKind::MulEq
          | TokenKind::DivEq
          | TokenKind::RemEq
          | TokenKind::BitAndEq
          | TokenKind::BitOrEq
          | TokenKind::BitXorEq
          | TokenKind::BitShrEq
          | TokenKind::BitShlEq
          | TokenKind::ConcatEq
          | TokenKind::Equals
          | TokenKind::Warlus => Some((1, Associativity::Right)),
            TokenKind::Semi => Some((0, Associativity::Right)),
            _ => None
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenKind::OpeningParen => write!(f, "("),
            TokenKind::ClosingParen => write!(f, ")"),
            TokenKind::OpeningBrace => write!(f, "{{"),
            TokenKind::ClosingBrace => write!(f, "}}"),
            TokenKind::OpeningBracket => write!(f, "["),
            TokenKind::ClosingBracket => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::Warlus => write!(f, ":="),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Do => write!(f, "do"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::Add => write!(f, "+"),
            TokenKind::Sub => write!(f, "-"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Rem => write!(f, "%"),
            TokenKind::BitAnd => write!(f, "&&&"),
            TokenKind::BitOr => write!(f, "|||"),
            TokenKind::BitNot => write!(f, "~~~"),
            TokenKind::BitXor => write!(f, "^^^"),
            TokenKind::BitShr => write!(f, ">>>"),
            TokenKind::BitShl => write!(f, "<<<"),
            TokenKind::Concat => write!(f, "++"),
            TokenKind::AddEq => write!(f, "+="),
            TokenKind::SubEq => write!(f, "-="),
            TokenKind::MulEq => write!(f, "*="),
            TokenKind::DivEq => write!(f, "/="),
            TokenKind::RemEq => write!(f, "%="),
            TokenKind::BitAndEq => write!(f, "&&&="),
            TokenKind::BitOrEq => write!(f, "|||="),
            TokenKind::BitXorEq => write!(f, "^^^="),
            TokenKind::BitShrEq => write!(f, ">>>="),
            TokenKind::BitShlEq => write!(f, "<<<="),
            TokenKind::ConcatEq => write!(f, "++="),
            TokenKind::Is => write!(f, "is"),
            TokenKind::Eq => write!(f, "=="),
            TokenKind::Ne => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Le => write!(f, "<="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::Ge => write!(f, ">="),
            TokenKind::Pipe => write!(f, "|>"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::String(done) => if done { write!(f, "string") } else { write!(f, "string fragment") },
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::Symbol => write!(f, "symbol"),
            TokenKind::EOF => write!(f, "end of file"),
            TokenKind::Error(TokenKindError::InvalidCharacter) => write!(f, "invalid character"),
            TokenKind::Error(TokenKindError::InvalidEscape) => write!(f, "invalid escape"),
            TokenKind::Error(TokenKindError::InvalidOperator) => write!(f, "invalid operator"),
            TokenKind::Error(TokenKindError::Unterminated) => write!(f, "unterminated string"),
        }
    }
}