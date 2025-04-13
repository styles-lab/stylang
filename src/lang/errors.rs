use parserc::{Kind, span::Span};

/// Error raised by `stylang` parser combinators.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParseError {
    #[error(transparent)]
    ParserC(#[from] Kind),

    #[error("expect {0}, {1:?}")]
    Expect(TokenError, Span),

    #[error("unexpect {0}, {1:?}")]
    Unexpect(TokenError, Span),
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum TokenError {
    #[error("`unknown`")]
    Unknown,
    #[error("`whitespace`")]
    S,
    #[error("`[0-9]+`")]
    Digits,
    #[error("`[0-9a-fA-F]+`")]
    HexDigits,
    #[error("prefix `{0}`")]
    Prefix(&'static str),
    #[error("suffix `{0}`")]
    Suffix(&'static str),
    #[error("punctuation `{0}`")]
    Punct(&'static str),
    #[error("rgb component `{0}`")]
    Rgb(&'static str),
    #[error("`rgb(...)/#xxx/#xxxxxx`")]
    Color,
    #[error("`ident`")]
    Ident,
    #[error("keyword `{0}`")]
    Keyword(&'static str),
}
