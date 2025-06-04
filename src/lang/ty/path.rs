use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    input::LangInput,
    token::{Ident, SepColonColon},
};

/// The parser for type path, be like `std::collection::HashMap`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(input = I, error = LangError)]
pub struct TypePath<I>
where
    I: LangInput,
{
    /// first segment: `ident`.
    pub first: Ident<I>,
    /// rest segments: `[::ident]*`
    pub rest: Vec<(SepColonColon<I>, Ident<I>)>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{input::TokenStream, token::S};

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
                            SepColonColon(
                                None,
                                TokenStream {
                                    offset: 5,
                                    value: "::"
                                },
                                Some(S(TokenStream {
                                    offset: 7,
                                    value: " "
                                }))
                            ),
                            Ident(TokenStream {
                                offset: 8,
                                value: "collection"
                            })
                        ),
                        (
                            SepColonColon(
                                Some(S(TokenStream {
                                    offset: 18,
                                    value: " "
                                })),
                                TokenStream {
                                    offset: 19,
                                    value: "::"
                                },
                                Some(S(TokenStream {
                                    offset: 21,
                                    value: " "
                                }))
                            ),
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
            TypePath::parse(TokenStream::from("super")),
            Ok((
                TypePath {
                    first: Ident(TokenStream {
                        offset: 0,
                        value: "super"
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
