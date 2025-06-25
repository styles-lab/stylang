use parserc::{lang::LangInput, syntax::Syntax};

use crate::{errors::LangError, token::*};

/// The parser for type path, be like `std::collection::HashMap`.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct TypePath<I>
where
    I: LangInput,
{
    /// first segment: `ident`.
    pub first: Ident<I>,
    /// rest segments: `[::ident]*`
    pub rest: Vec<(Option<S<I>>, TokenColonColon<I>, Option<S<I>>, Ident<I>)>,
}

#[cfg(test)]
mod tests {

    use parserc::lang::TokenStream;

    use super::*;

    #[test]
    fn multi() {
        assert_eq!(
            TypePath::parse(TokenStream::from("super:: collection :: HashMap")),
            Ok((
                TypePath {
                    first: Ident(TokenStream {
                        offset: 0,
                        value: "super"
                    }),
                    rest: vec![
                        (
                            None,
                            TokenColonColon(TokenStream {
                                offset: 5,
                                value: "::"
                            }),
                            Some(S(TokenStream {
                                offset: 7,
                                value: " "
                            })),
                            Ident(TokenStream {
                                offset: 8,
                                value: "collection"
                            })
                        ),
                        (
                            Some(S(TokenStream {
                                offset: 18,
                                value: " "
                            })),
                            TokenColonColon(TokenStream {
                                offset: 19,
                                value: "::"
                            }),
                            Some(S(TokenStream {
                                offset: 21,
                                value: " "
                            })),
                            Ident(TokenStream {
                                offset: 22,
                                value: "HashMap"
                            })
                        )
                    ]
                },
                TokenStream {
                    offset: 29,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn only_first() {
        assert_eq!(
            TypePath::parse(TokenStream::from("crate")),
            Ok((
                TypePath {
                    first: Ident(TokenStream {
                        offset: 0,
                        value: "crate"
                    }),
                    rest: vec![]
                },
                TokenStream {
                    offset: 5,
                    value: ""
                }
            ))
        );
    }
}
