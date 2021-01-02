#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(try_trait)]
#![macro_use]

pub mod ast;
pub mod ir;
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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Span {
    line: u32,
    start: u32,
    end: u32,
}

impl Span {
    pub const fn new(line: u32, start: u32, end: u32) -> Span {
        Span {
            line, start, end
        }
    }

    pub const fn to(self, other: Self) -> Span {
        Span::new(self.line, self.start, other.end)
    }

    pub const fn range(self) -> Range<usize> {
        (self.start as usize) .. (self.end as usize)
    }

    pub fn trim_end(self) -> Span {
        Span::new(self.line, self.start, self.end - 1)
    }
}
