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
    #[error("`[0-9]+`")]
    Digits,
    #[error("`[0-9a-fA-F]+`")]
    HexDigits,
    #[error("`suffix({0})`")]
    Suffix(&'static str),
}
