use std::error::Error;
use std::fmt;
use std::fmt::Display;

use crate::Span;

pub type Result<T> = ::std::result::Result<T, NitrineError>;

#[derive(Debug, Clone)]
pub struct NitrineError {
    text: String,
    span: Option<Span>
}

impl NitrineError {

    pub fn error(span: Span, text: String) -> NitrineError {
        NitrineError { text, span: Some(span) }
    }

    pub fn basic(text: String) -> NitrineError {
        NitrineError { text, span: None }
    }

    pub fn with_span(self, span: Span) -> NitrineError {
        NitrineError { span: Some(span), ..self }
    }
}

impl Error for NitrineError {}

impl Display for NitrineError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error")?;

        if let Some(span) = self.span {
            write!(f, " [{}]", span.line)?;
        }

        write!(f, ": {}", self.text)
    }
}
