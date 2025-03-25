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
    #[error("`delimiter({0})`")]
    Delimiter(char),
    #[error("`->`")]
    ReturnTypeArrow,
    #[error("`function input argument type.`")]
    FnArgType,
    #[error("`function return type.`")]
    ReturnType,
    #[error("`white space`")]
    S,
    #[error("`ident`")]
    Ident,
    #[error("`:`")]
    Colon,
}
