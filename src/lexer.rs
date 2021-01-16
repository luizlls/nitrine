use std::str::Chars;

use crate::token::{get_keyword, get_operator, Token, TokenKind, TokenKindError};
use crate::Span;

const SYMBOLS: &str = "=.+-<>*/%^&|~:!?";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerMode {
    Regular,

    String,

    Interpolation
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: Chars<'src>,
    curr: Option<char>,
    peek: Option<char>,
    start: u32,
    offset: u32,
    line: u32,
    mode: LexerMode,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Lexer {
        let mut lexer = Lexer {
            src,
            chars: src.chars(),
            curr: None,
            peek: None,
            start: 0,
            offset: 0,
            line: 1,
            mode: LexerMode::Regular
        };
        lexer.bump();
        lexer.bump();
        lexer.offset = 0;

        lexer
    }

    fn align(&mut self) {
        self.start = self.offset;
    }

    fn bump(&mut self) {
        let length = self.curr.map(char::len_utf8).unwrap_or(0) as u32;
        self.offset += length;
        self.curr = self.peek;
        self.peek = self.chars.next();
    }

    fn line(&mut self) {
        self.line += 1;
        self.bump();
    }

    fn span(&self) -> Span {
        Span::new(self.line, self.start, self.offset)
    }

    fn value(&self) -> &str {
        &self.src[(self.start as usize) .. (self.offset as usize)]
    }

    fn next_kind(&mut self) -> Option<TokenKind> {
        match self.mode {
            LexerMode::String => self.next_string(),
            LexerMode::Regular |
            LexerMode::Interpolation => self.next_regular(),
        }
    }

    fn next_regular(&mut self) -> Option<TokenKind> {
        loop {
            self.align();

            match self.curr {
                Some('(') => {
                    return self.single(TokenKind::OpeningParen);
                }
                Some(')') => {
                    return self.single(TokenKind::ClosingParen);
                }
                Some('{') => {
                    return self.single(TokenKind::OpeningBrace);
                }
                Some('}') if self.mode == LexerMode::Interpolation => {
                    self.mode = LexerMode::String;
                    return self.single(TokenKind::ClosingBrace);
                }
                Some('}') => {
                    return self.single(TokenKind::ClosingBrace);
                }
                Some('[') => {
                    return self.single(TokenKind::OpeningBracket);
                }
                Some(']') => {
                    return self.single(TokenKind::ClosingBracket);
                }
                Some(',') => {
                    return self.single(TokenKind::Comma);
                }
                Some(';') => {
                    return self.single(TokenKind::Semi);
                }
                Some(' ')
              | Some('\t')
              | Some('\r') => {
                    self.bump();
                    continue;
                }
                Some('\n') => {
                    self.line();
                    continue;
                }
                Some('_')
              | Some('a'..='z')
              | Some('A'..='Z') => {
                    return Some(self.ident());
                }
                Some('0'..='9') => {
                    return Some(self.number());
                }
                Some('"') => {
                    return self.next_string();
                }
                Some('/') if self.peek == Some('/') => {
                    self.comment();
                    continue;
                }
                Some(_) if self.is_symbol(self.curr) => {
                    return Some(self.operator());
                }
                None => {
                    return None;
                }
                Some(_) => {
                    return Some(TokenKind::Error(TokenKindError::InvalidCharacter));
                }
            }
        }
    }

    fn next_string(&mut self) -> Option<TokenKind> {
        self.align();

        loop {
            match self.curr {
                Some('"') if self.mode == LexerMode::Regular => {
                    self.mode = LexerMode::String;
                    self.bump();
                }
                Some('"') => {
                    self.mode = LexerMode::Regular;
                    self.bump();
                    return Some(TokenKind::String(true)); // finished
                }
                Some('{') if self.peek == Some('{') => {
                    self.bump();
                }
                Some('{') => {
                    self.mode = LexerMode::Interpolation;
                    // self.bump();
                    return Some(TokenKind::String(false)); // fragment
                }
                Some('\n') |
                None => {
                    return Some(TokenKind::Error(TokenKindError::UnterminatedString));
                }
                Some('\\') => {
                    self.bump();

                    match self.curr {
                        Some('n') | Some('r')
                      | Some('t') | Some('v')
                      | Some('a') | Some('b')
                      | Some('"') | Some('0')
                      | Some('\\') => {
                            self.bump();
                        }
                        Some('u') => {
                            todo!("Validate unicode escape")
                        }
                        Some('x') => {
                            todo!("Validate binary escape")
                        }
                        _ => {
                            return Some(TokenKind::Error(TokenKindError::InvalidEscape));
                        }
                    }
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn single(&mut self, token: TokenKind) -> Option<TokenKind> {
        self.bump();
        Some(token)
    }

    fn is_alpha(&self, chr: Option<char>) -> bool {
        matches!(chr,
            Some('a'..='z') |
            Some('A'..='Z') |
            Some('0'..='9') |
            Some('_'))
    }

    fn is_number(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('0'..='9'))
    }

    fn is_symbol(&self, chr: Option<char>) -> bool {
        if let Some(chr) = chr {
            SYMBOLS.contains(chr)
        } else {
            false
        }
    }

    fn ident(&mut self) -> TokenKind {
        while self.is_alpha(self.curr) { self.bump(); }

        let mut symbol = false;

        match self.curr {
            Some('\'') => {
                symbol = true;
                self.bump();
            }
            Some('?')
          | Some('!') => {
                self.bump();
            }
            _ => {}
        }

        if let Some(keyword) = get_keyword(self.value()) {
            keyword
        } else if symbol {
            TokenKind::Symbol
        } else {
            TokenKind::Ident
        }
    }

    fn operator(&mut self) -> TokenKind {
        while self.is_symbol(self.curr) { self.bump(); }

        if let Some(operator) = get_operator(self.value()) {
            operator
        } else {
            TokenKind::Error(TokenKindError::InvalidOperator)
        }
    }

    fn number(&mut self) -> TokenKind {
        let mut float = false;

        while self.is_number(self.curr) {
            self.bump();

            if self.curr == Some('.') && !float {
                float = true;
                self.bump();
            }
        }

        TokenKind::Number
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
            self.bump();
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.next_kind().map(|kind| Token { kind, span: self.span() })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_identifier() {
        let mut lexer = Lexer::new("variable = 1");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Ident);
        assert_eq!(lexer.value(), "variable");
    }

    #[test]
    fn lex_symbol() {
        let mut lexer = Lexer::new("Symbolic'");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Symbol);
        assert_eq!(lexer.value(), "Symbolic'");
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

       assert_eq!(lexer.next().unwrap().kind, TokenKind::Ident);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Add);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Ident);
    }

    #[test]
    fn lexer_integer() {
        let mut lexer = Lexer::new("42");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Number);
        assert_eq!(lexer.value(), "42");
    }

    #[test]
    fn lex_float() {
        let mut lexer = Lexer::new("3.14519");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Number);
        assert_eq!(lexer.value(), "3.14519");
    }

    #[test]
    fn lex_simple_string() {
        let mut lexer = Lexer::new("\"Hello, World\"");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String(true));
        assert_eq!(lexer.value(), "\"Hello, World\"");
    }

    #[test]
    fn lex_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String(true));
        assert_eq!(lexer.value(), "\"src = \\\"y = 42\\\"\"");
    }
}
