use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    input::LangInput,
    token::{Paren, S, TokenCrate, TokenPub, TokenSuper},
};

/// Optional scope for visibility level `pub`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError, input = I)]
pub enum Scope<I>
where
    I: LangInput,
{
    Crate(TokenCrate<I>),
    Super(TokenSuper<I>),
}

/// The visibility level of an item: `pub`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError, input = I)]
pub enum Visibility<I>
where
    I: LangInput,
{
    Scope {
        /// Token `pub`.
        token: TokenPub<I>,
        /// optional scope part.
        scope: Paren<I, Scope<I>>,
    },
    Outer {
        /// Token `pub`.
        token: TokenPub<I>,
        /// required tail `ws`.
        #[fatal]
        tail_s: S<I>,
    },
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Delimiter, Parse, span::Span};

    use crate::lang::{
        errors::{LangError, SyntaxKind},
        input::TokenStream,
        item::{Scope, Visibility},
        token::*,
    };

    #[test]
    fn without_scope() {
        assert_eq!(
            Visibility::parse(TokenStream::from("pub ")),
            Ok((
                Visibility::Outer {
                    token: TokenPub(TokenStream {
                        offset: 0,
                        value: "pub"
                    }),
                    tail_s: S(TokenStream {
                        offset: 3,
                        value: " "
                    })
                },
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
                    token: TokenPub(TokenStream {
                        offset: 0,
                        value: "pub"
                    }),
                    scope: Delimiter {
                        delimiter_start: SepLeftParen(
                            None,
                            TokenStream {
                                offset: 3,
                                value: "("
                            },
                            None
                        ),
                        body: Scope::Crate(TokenCrate(TokenStream {
                            offset: 4,
                            value: "crate"
                        })),
                        delimiter_end: SepRightParen(
                            None,
                            TokenStream {
                                offset: 9,
                                value: ")"
                            },
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
                    token: TokenPub(TokenStream {
                        offset: 0,
                        value: "pub",
                    }),
                    scope: Delimiter {
                        delimiter_start: SepLeftParen(
                            None,
                            TokenStream {
                                offset: 3,
                                value: "(",
                            },
                            None,
                        ),
                        body: Scope::Super(TokenSuper(TokenStream {
                            offset: 4,
                            value: "super",
                        })),
                        delimiter_end: SepRightParen(
                            None,
                            TokenStream {
                                offset: 9,
                                value: ")",
                            },
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
                Span { offset: 3, len: 0 }
            )))
        );
    }
}
