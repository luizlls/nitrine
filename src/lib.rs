#![feature(box_syntax)]
#![feature(box_patterns)]
#![macro_use]

//pub mod compiler;
pub mod analysis;
pub mod ast;
pub mod token;
pub mod lexer;
pub mod parser;
pub mod error;

use std::{ops::Range, path::PathBuf};

#[derive(Debug, Clone)]
pub struct Source {
    pub path: PathBuf,
    pub content: String,
}

impl Source {
    pub fn new(content: &str, path: PathBuf) -> Source {
        Source {
            path,
            content: content.into(),
        }
    }

    pub fn source(content: &str) -> Source {
        Source::new(content, PathBuf::from("none"))
    }
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct Span {
    line: u32,
    start: u32,
    end: u32,
}

impl Default for Span {
    fn default() -> Self {
        Span::new(0, 0, 0)
    }
}

impl Span {
    pub const fn new(line: u32, start: u32, end: u32) -> Span {
        Span {
            line, start, end
        }
    }

    pub const fn basic(line: u32) -> Span {
        Span {
            line, start: 0, end: 0
        }
    }

    pub const fn range(self) -> Range<usize> {
        (self.start as usize) .. (self.end as usize)
    }
}

impl std::ops::Add for Span {
    type Output = Span;

    fn add(self, other: Self) -> Self::Output {
        Span::new(self.line, self.start, other.end)
    }
}