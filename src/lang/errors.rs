use parserc::{Kind, span::Span};

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParseError {
    #[error(transparent)]
    ParserC(#[from] Kind),

    #[error("expect {0}, {1:?}")]
    Expect(Token, Span),
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum Token {
    #[error("`slice component type`")]
    SliceComponent,

    #[error("`delimiter({0})`")]
    Delimiter(char),

    #[error("`->`")]
    ReturnTypeArrow,

    #[error("`function argument type.`")]
    FnArgType,

    #[error("`function return type.`")]
    ReturnType,

    #[error("`white space`")]
    S,

    #[error("`ident`")]
    Ident,

    #[error("`:`")]
    Colon,

    #[error("`one or more ascii numerical characters:0-9`")]
    Digits,

    #[error("`E` or `e`")]
    ExpToken,

    #[error("`exponent digits`")]
    ExpDigits,
}
