use parserc::{Kind, span::Span};

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParseError {
    #[error(transparent)]
    ParserC(#[from] Kind),

    #[error("expect {0}, {1:?}")]
    Expect(ParseKind, Span),
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParseKind {
    #[error("`]`")]
    SliceEnd,

    #[error("`slice component type`")]
    SliceComponent,

    #[error("`(`")]
    ParamsStart,

    #[error("`)`")]
    ParamsEnd,

    #[error("`->`")]
    ReturnTypeArrow,

    #[error("`function return type.`")]
    ReturnType,

    #[error("`white space`")]
    S,
}
