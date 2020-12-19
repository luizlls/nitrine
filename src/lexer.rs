use std::str::Chars;

use crate::token::{self, Token, TokenKind, TokenKindError};
use crate::Span;

pub const SYMBOLS: &str = ".%^&|:=~+-*<>!?/";


#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: Chars<'src>,
    curr: Option<char>,
    peek: Option<char>,
    byte_start: u32,
    byte_offset: u32,
    line: u32,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Lexer {
        let mut lexer = Lexer {
            src,
            chars: src.chars(),
            curr: None,
            peek: None,
            byte_start: 0,
            byte_offset: 0,
            line: 1,
        };
        lexer.bump(0);
        lexer.bump(0);

        lexer
    }

    fn advance(&mut self) {
        self.byte_start = self.byte_offset;
    }

    fn bump(&mut self, n: u32) {
        self.curr = self.peek;
        self.peek = self.chars.next();
        self.byte_offset += n;
    }

    fn bump_length(&mut self) {
        self.bump(self.curr.map(char::len_utf8).unwrap_or(0) as u32);
    }

    fn line(&mut self) {
        self.line += 1;
        self.bump(1);
    }

    fn span(&self) -> Span {
        Span::new(self.line, self.byte_start, self.byte_offset)
    }

    fn value(&self) -> &str {
        &self.src[(self.byte_start as usize) .. (self.byte_offset as usize)]
    }

    fn next_tokenkind(&mut self) -> Option<TokenKind> {
        loop {
            self.advance();

            let kind = match self.curr {
                Some('(') => self.single(TokenKind::OpenParen),
                Some(')') => self.single(TokenKind::CloseParen),
                Some('{') => self.single(TokenKind::OpenBrace),
                Some('}') => self.single(TokenKind::CloseBrace),
                Some('[') => self.single(TokenKind::OpenBracket),
                Some(']') => self.single(TokenKind::CloseBracket),
                Some(',') => self.single(TokenKind::Comma),
                Some(';') => self.single(TokenKind::Semi),
                Some(' ')
              | Some('\t')
              | Some('\r') => {
                    self.bump(1);
                    continue;
                }
                Some('\n') => {
                    self.line();
                    continue;
                }
                Some('_')
              | Some('a'..='z') => {
                    Some(self.lower())
                }
                Some('A'..='Z') => {
                    Some(self.upper())
                }
                Some('0'..='9') => {
                    Some(self.number())
                }
                Some('"') => {
                    Some(self.string())
                }
                Some('/') if self.peek == Some('/') => {
                    self.comment();
                    continue;
                }
                Some(_) if self.is_symbol(self.curr) => {
                    Some(self.operator())
                }
                None => {
                    None
                }
                Some(_) => {
                    Some(TokenKind::Error(TokenKindError::InvalidCharacter))
                }
            };

            return kind
        }
    }

    fn single(&mut self, token: TokenKind) -> Option<TokenKind> {
        self.bump(1);
        Some(token)
    }

    fn is_alpha(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('a'..='z') | Some('_') | Some('A'..='Z') | Some('0'..='9'))
    }

    fn is_number(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('0'..='9'))
    }

    fn is_symbol(&self, chr: Option<char>) -> bool {
        match chr {
            Some(chr) => SYMBOLS.contains(chr), _ => false,
        }
    }

    fn lower(&mut self) -> TokenKind {
        while self.is_alpha(self.curr) { self.bump_length(); }

        if let Some(keyword) = token::get_keyword(self.value()) {
            keyword
        } else {
            TokenKind::Lower
        }
    }

    fn upper(&mut self) -> TokenKind {
        while self.is_alpha(self.curr) { self.bump_length(); }
        TokenKind::Upper
    }

    fn operator(&mut self) -> TokenKind {
        while self.is_symbol(self.curr) { self.bump_length(); }

        if let Some(operator) = token::get_operator(self.value()) {
            operator
        } else {
            TokenKind::Error(TokenKindError::InvalidOperator)
        }
    }

    fn number(&mut self) -> TokenKind {
        let mut float = false;

        while self.is_number(self.curr) {
            self.bump(1);

            if self.curr == Some('.') && !float {
                float = true;
                self.bump(1);
            }
        }

        if float {
            TokenKind::Float
        } else {
            TokenKind::Integer
        }
    }

    fn string(&mut self) -> TokenKind {
        self.bump(1);

        loop {
            match self.curr {
                Some('\\') => {
                    self.bump(1);
                }
                Some('"') => {
                    break;
                }
                Some('\n') |
                None => {
                    return TokenKind::Error(TokenKindError::UnterminatedString);
                }
                _ => {}
            }

            self.bump_length();
        }

        self.bump(1);

        TokenKind::String
    }

    fn comment(&mut self) {
        loop {
            match self.curr {
                Some('\n') |
                None => {
                    return;
                }
                _ => {}
            }
            self.bump_length();
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.next_tokenkind().map(|kind| Token { kind, span: self.span() })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_identifier() {
        let mut lexer = Lexer::new("variable = 1");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lower);
        assert_eq!(lexer.value(), "variable");
    }

    #[test]
    fn lex_keyword() {
        let mut lexer = Lexer::new("if x > y then X else Y");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::If);
        assert_eq!(lexer.value(), "if");
    }

    #[test]
    fn lex_operator() {
       let mut lexer = Lexer::new("a + b");

       assert_eq!(lexer.next().unwrap().kind, TokenKind::Lower);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Add);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Lower);
    }

    #[test]
    fn lexer_integer() {
        let mut lexer = Lexer::new("42");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Integer);
        assert_eq!(lexer.value(), "42");
    }

    #[test]
    fn lex_float() {
        let mut lexer = Lexer::new("3.14519");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Float);
        assert_eq!(lexer.value(), "3.14519");
    }

    #[test]
    fn lex_simple_string() {
        let mut lexer = Lexer::new("\"Hello, World\"");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String);
        assert_eq!(lexer.value(), "\"Hello, World\"");
    }

    #[test]
    fn lex_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String);
        assert_eq!(lexer.value(), "\"src = \\\"y = 42\\\"\"");
    }
}
