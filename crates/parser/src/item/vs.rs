use parserc::{lang::LangInput, syntax::Syntax};

use crate::{errors::LangError, token::*};

/// Optional scope for visibility level `pub`.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Scope<I>
where
    I: LangInput,
{
    Crate(Option<S<I>>, TokenCrate<I>, Option<S<I>>),
    Super(Option<S<I>>, TokenSuper<I>, Option<S<I>>),
}

/// The visibility level of an item: `pub`.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Visibility<I>
where
    I: LangInput,
{
    Scope {
        /// Token `pub`.
        token: KeywordPub<I>,
        /// optional scope part.
        scope: Paren<I, Scope<I>>,
    },
    Outer(
        /// Token `pub`.
        KeywordPub<I>,
        /// required tail `ws`.
        #[fatal]
        S<I>,
    ),
}

#[cfg(test)]
mod tests {
    use parserc::{
        errors::ControlFlow,
        lang::{Span, TokenStream},
        syntax::Delimiter,
    };

    use super::*;
    use crate::{errors::SyntaxKind, item::Visibility};

    #[test]
    fn without_scope() {
        assert_eq!(
            Visibility::parse(TokenStream::from("pub ")),
            Ok((
                Visibility::Outer(
                    KeywordPub(TokenStream {
                        offset: 0,
                        value: "pub"
                    }),
                    S(TokenStream {
                        offset: 3,
                        value: " "
                    })
                ),
                TokenStream {
                    offset: 4,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn with_scope() {
        assert_eq!(
            Visibility::parse(TokenStream::from("pub(crate) ")),
            Ok((
                Visibility::Scope {
                    token: KeywordPub(TokenStream {
                        offset: 0,
                        value: "pub"
                    }),
                    scope: Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 3,
                                value: "("
                            }),
                            None
                        ),
                        body: Scope::Crate(
                            None,
                            TokenCrate(TokenStream {
                                offset: 4,
                                value: "crate"
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightParen(TokenStream {
                                offset: 9,
                                value: ")"
                            }),
                            Some(S(TokenStream {
                                offset: 10,
                                value: " "
                            }))
                        )
                    }
                },
                TokenStream {
                    offset: 11,
                    value: ""
                }
            ))
        );
        assert_eq!(
            Visibility::parse(TokenStream::from("pub(super) ")),
            Ok((
                Visibility::Scope {
                    token: KeywordPub(TokenStream {
                        offset: 0,
                        value: "pub",
                    }),
                    scope: Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 3,
                                value: "(",
                            }),
                            None,
                        ),
                        body: Scope::Super(
                            None,
                            TokenSuper(TokenStream {
                                offset: 4,
                                value: "super",
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightParen(TokenStream {
                                offset: 9,
                                value: ")",
                            }),
                            Some(S(TokenStream {
                                offset: 10,
                                value: " ",
                            })),
                        ),
                    },
                },
                TokenStream {
                    offset: 11,
                    value: "",
                },
            ))
        );
    }

    #[test]
    fn without_tail_s() {
        assert_eq!(
            Visibility::parse(TokenStream::from("pub")),
            Err(ControlFlow::Fatal(LangError::expect(
                SyntaxKind::S,
                Span::Some { start: 3, end: 3 }
            )))
        );
    }
}
