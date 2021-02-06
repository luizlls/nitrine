use std::fmt;

use crate::Span;

#[derive(Debug, Clone, Copy, Default)]
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
    Equals,
    Warlus,

    // keywords
    Do,
    If,
    Fn,
    Then,
    Else,

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

    // values
    Lower,
    Upper,
    Number,
    String,
    StringStart,
    StringEnd,
    StringFragment,
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

    UnterminatedString,
}

impl Default for TokenKind {
    fn default() -> TokenKind { TokenKind::EOF }
}

pub fn get_keyword(key: &str) -> Option<TokenKind> {
    match key {
        "do"    => Some(TokenKind::Do),
        "if"    => Some(TokenKind::If),
        "then"  => Some(TokenKind::Then),
        "else"  => Some(TokenKind::Else),
        "fn"    => Some(TokenKind::Fn),
        "is"    => Some(TokenKind::Is),
        "and"   => Some(TokenKind::And),
        "or"    => Some(TokenKind::Or),
        "not"   => Some(TokenKind::Not),
        _ => None,
    }
}

pub fn get_operator(key: &str) -> Option<TokenKind> {
    match key {
        ":"   => Some(TokenKind::Colon),
        "."   => Some(TokenKind::Dot),
        "="   => Some(TokenKind::Equals),
        ":="  => Some(TokenKind::Warlus),
        "->"  => Some(TokenKind::Arrow),
        "+"   => Some(TokenKind::Add),
        "-"   => Some(TokenKind::Sub),
        "*"   => Some(TokenKind::Mul),
        "/"   => Some(TokenKind::Div),
        "%"   => Some(TokenKind::Rem),
        "&&&" => Some(TokenKind::BitAnd),
        "|||" => Some(TokenKind::BitOr),
        "~~~" => Some(TokenKind::BitNot),
        "^^^" => Some(TokenKind::BitXor),
        ">>>" => Some(TokenKind::BitShl),
        "<<<" => Some(TokenKind::BitShr),
        "++"  => Some(TokenKind::Concat),
        "|>"  => Some(TokenKind::Pipe),
        "=="  => Some(TokenKind::Eq),
        "!="  => Some(TokenKind::Ne),
        "<"   => Some(TokenKind::Lt),
        "<="  => Some(TokenKind::Le),
        ">"   => Some(TokenKind::Gt),
        ">="  => Some(TokenKind::Ge),
        _ => None
    }
}

pub type Precedence = u8;

#[derive(Debug, Clone, Copy, PartialEq)]
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
          | TokenKind::Not
          | TokenKind::BitAnd
          | TokenKind::BitOr
          | TokenKind::BitNot
          | TokenKind::BitXor
          | TokenKind::BitShr
          | TokenKind::BitShl
          | TokenKind::Concat
          | TokenKind::Eq
          | TokenKind::Ne
          | TokenKind::Lt
          | TokenKind::Le
          | TokenKind::Gt
          | TokenKind::Ge
          | TokenKind::Is
          | TokenKind::Pipe
          | TokenKind::Semi => true,
          _ => false
        }
    }

    pub fn precedence(&self) -> Option<(Precedence, Associativity)> {
        match self {
            TokenKind::Div    => Some((10, Associativity::Left)),
            TokenKind::Mul    => Some((10, Associativity::Left)),
            TokenKind::Rem    => Some((10, Associativity::Left)),
            TokenKind::Add    => Some(( 9, Associativity::Left)),
            TokenKind::Sub    => Some(( 9, Associativity::Left)),
            TokenKind::Concat => Some(( 9, Associativity::Left)),
            TokenKind::BitShr => Some(( 8, Associativity::Left)),
            TokenKind::BitShl => Some(( 8, Associativity::Left)),
            TokenKind::BitAnd => Some(( 5, Associativity::Left)),
            TokenKind::BitXor => Some(( 6, Associativity::Left)),
            TokenKind::BitOr  => Some(( 5, Associativity::Left)),
            TokenKind::Lt     => Some(( 4, Associativity::Left)),
            TokenKind::Le     => Some(( 4, Associativity::Left)),
            TokenKind::Gt     => Some(( 4, Associativity::Left)),
            TokenKind::Ge     => Some(( 4, Associativity::Left)),
            TokenKind::Ne     => Some(( 4, Associativity::Left)),
            TokenKind::Eq     => Some(( 4, Associativity::Left)),
            TokenKind::And    => Some(( 3, Associativity::Left)),
            TokenKind::Or     => Some(( 2, Associativity::Left)),
            TokenKind::Pipe   => Some(( 1, Associativity::Left)),
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
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::Add => write!(f, "+"),
            TokenKind::Sub => write!(f, "-"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Rem => write!(f, "%"),
            TokenKind::Concat => write!(f, "++"),
            TokenKind::BitAnd => write!(f, "&&&"),
            TokenKind::BitOr  => write!(f, "|||"),
            TokenKind::BitNot => write!(f, "~~~"),
            TokenKind::BitXor => write!(f, "^^^"),
            TokenKind::BitShr => write!(f, ">>>"),
            TokenKind::BitShl => write!(f, "<<<"),
            TokenKind::Pipe => write!(f, "|>"),
            TokenKind::Is => write!(f, "is"),
            TokenKind::Eq => write!(f, "=="),
            TokenKind::Ne => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Le => write!(f, "<="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::Ge => write!(f, ">="),
            TokenKind::String => write!(f, "string"),
            TokenKind::StringStart => write!(f, "start of string template"),
            TokenKind::StringEnd => write!(f, "end of string template"),
            TokenKind::StringFragment => write!(f, "fragment of string template"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::Lower => write!(f, "lowercase identifier"),
            TokenKind::Upper => write!(f, "uppercase identifier"),
            TokenKind::Error(TokenKindError::InvalidCharacter) => write!(f, "invalid character"),
            TokenKind::Error(TokenKindError::InvalidEscape) => write!(f, "invalid escape"),
            TokenKind::Error(TokenKindError::InvalidOperator) => write!(f, "invalid operator"),
            TokenKind::Error(TokenKindError::UnterminatedString) => write!(f, "unterminated string"),
            TokenKind::EOF => write!(f, "end of file"),
        }
    }
}