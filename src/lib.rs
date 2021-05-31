#![feature(box_syntax)]
#![feature(box_patterns)]
#![macro_use]

//pub mod compiler;
pub mod desugar;
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
pub enum Span {
    Undefined,

    Line {
        line: u32
    },

    Full {
        line: u32,
        start: u32,
        end: u32,
    },
}

impl Default for Span {
    fn default() -> Self {
        Span::Undefined
    }
}

impl Span {
    pub const fn new(line: u32, start: u32, end: u32) -> Span {
        Span::Full {
            line, start, end
        }
    }

    pub const fn range(self) -> Range<usize> {
        match self {
            Span::Full { start, end, .. } => {
                (start as usize) .. (end as usize)
            }
            _ => 0 .. 0
        }
    }

    pub const fn line(self) -> u32 {
        match self {
            Span::Full { line, .. } => line,
            Span::Line { line, .. } => line,
            _ => 0
        }
    }
}

impl std::ops::Add for Span {
    type Output = Span;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Span::Full { end: offset, .. },
             Span::Full { line, start, .. }) => {
                Span::new(line, start, offset)
            }
            _ => self
        }
    }
}