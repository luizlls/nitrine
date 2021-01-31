use std::str::Chars;

use crate::token::{get_keyword, get_operator, Token, TokenKind, TokenKindError};
use crate::Span;

const SYMBOLS: &str = "=.+-<>*/%^&|~:!?";

#[derive(Debug, Clone, PartialEq, Eq)]
enum Mode {
    Regular,

    String,

    Template
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
    mode: Mode,
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
            mode: Mode::Regular
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
        if self.mode == Mode::String {
            return self.template(false);
        }

        loop {
            self.align();

            match self.curr {
                Some('a'..='z')
              | Some('_') => {
                    return Some(self.lower());
                }
                Some('A'..='Z') => {
                    return Some(self.upper());
                }
                Some('0'..='9') => {
                    return Some(self.number());
                }
                Some('(') => {
                    return self.single(TokenKind::OpeningParen);
                }
                Some(')') => {
                    return self.single(TokenKind::ClosingParen);
                }
                Some('{') => {
                    return self.single(TokenKind::OpeningBrace);
                }
                Some('}') if self.mode == Mode::Template => {
                    self.mode = Mode::String;
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
                Some('"') => {
                    return self.template(true);
                }
                Some('/') if self.peek == Some('/') => {
                    self.comment();
                    continue;
                }
                Some(' ')
              | Some('\t')
              | Some('\r') => {
                    self.space();
                    continue;
                }
                Some('\n') => {
                    self.line();
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

    fn template(&mut self, start: bool) -> Option<TokenKind> {
        self.align();

        loop {
            match self.curr {
                Some('"') if self.mode == Mode::Regular => {
                    self.bump();
                    self.mode = Mode::String;
                }
                Some('"') => {
                    self.mode = Mode::Regular;
                    self.bump();
                    return Some(if start { TokenKind::String } else { TokenKind::StringFinish });
                }
                Some('{') if self.peek == Some('{') => {
                    self.bump();
                    self.bump();
                }
                Some('{') => {
                    self.mode = Mode::Template;
                    // self.bump();
                    return Some(if start { TokenKind::StringStart } else { TokenKind::StringFragment });
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

    fn lower(&mut self) -> TokenKind {
        while self.is_alpha(self.curr) { self.bump(); }

        match self.curr {
            Some('?')
          | Some('!') => {
                self.bump();
            }
            Some('\'') => {
                while self.curr == Some('\'') {
                    self.bump();
                }
            }
            _ => {}
        }

        if let Some(keyword) = get_keyword(self.value()) {
            keyword
        } else {
            TokenKind::Lower
        }
    }

    fn upper(&mut self) -> TokenKind {
        while self.is_alpha(self.curr) { self.bump(); }
        TokenKind::Upper
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

    fn space(&mut self) {
        while matches!(self.curr, Some('\r') | Some('\t') | Some(' ')) {
            self.bump();
        }
    }

    fn comment(&mut self) {
        loop {
            match self.curr {
                Some('\n')
              | None => {
                    return;
                }
                _ => {
                    self.bump();
                }
            }
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

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lower);
        assert_eq!(lexer.value(), "variable");
    }

    #[test]
    fn lex_symbol() {
        let mut lexer = Lexer::new("Symbol");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Upper);
        assert_eq!(lexer.value(), "Symbol");
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

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String);
        assert_eq!(lexer.value(), "\"Hello, World\"");
    }

    #[test]
    fn lex_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String);
        assert_eq!(lexer.value(), "\"src = \\\"y = 42\\\"\"");
    }

    #[test]
    fn lex_string_template() {
        let mut lexer = Lexer::new(r#""Hello {name}!""#);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::StringStart);
        assert_eq!(lexer.value(), r#""Hello "#);
        assert_eq!(lexer.mode, Mode::Template);
    }
}
