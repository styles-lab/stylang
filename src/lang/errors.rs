use parserc::{Kind, span::Span};

/// Error raised by `stylang` parser combinators.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParseError {
    #[error(transparent)]
    ParserC(#[from] Kind),

    #[error("expect {0}, {1:?}")]
    Expect(Token, Span),
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum Token {
    #[error("`whitespace`")]
    S,
}
