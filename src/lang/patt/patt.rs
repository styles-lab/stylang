use parserc::{Parse, Parser, ParserExt, Punctuated, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    input::LangInput,
    lit::Lit,
    patt::{PattRange, PattRest, PattType},
    token::{Brace, Bracket, Paren, SepComma, SepOr, TokenUnderscore},
    ty::TypePath,
};

/// A literal in place of an expression: 1, "foo".
pub type PattLit<I> = Lit<I>;

/// A type ascription pattern: text::Fill.
pub type PattPath<I> = TypePath<I>;

/// A tuple pattern: (a, b),(a | b),...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattTuple<I>(pub Paren<I, Punctuated<Patt<I>, SepComma<I>>>)
where
    I: LangInput;

/// A dynamically sized slice pattern: [a, b,.., y, z].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattSlice<I>(Bracket<I, Punctuated<Patt<I>, SepComma<I>>>)
where
    I: LangInput;

/// A pattern that matches any value: `_`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattWild<I>(pub TokenUnderscore<I>)
where
    I: LangInput;

/// A tuple struct or tuple variant pattern: $path(x, y, .., z).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattTupleStruct<I>
where
    I: LangInput,
{
    /// class/data type path.
    pub path: TypePath<I>,
    /// patt fields.
    pub fields: Paren<I, Punctuated<Patt<I>, SepComma<I>>>,
}

/// A tuple struct or tuple variant pattern: $path { x, y, .., z}.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattStruct<I>
where
    I: LangInput,
{
    /// class/data type path.
    pub path: TypePath<I>,
    /// patt fields.
    pub fields: Brace<I, Punctuated<Patt<I>, SepComma<I>>>,
}

/// A pattern that matches any one of a set of cases.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattOr<I>
where
    I: LangInput,
{
    /// the first pattern.
    pub first: Box<Patt<I>>,
    /// rest segments.
    pub rest: Vec<(SepOr<I>, Patt<I>)>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub(super) enum PattUnary<I>
where
    I: LangInput,
{
    /// 1,"hello",...
    Lit(PattLit<I>),
    /// ..
    Rest(PattRest<I>),
    /// 1..=2,1..10
    Range(PattRange<I>),
    /// _
    Wild(PattWild<I>),
    /// [a,b,..,c],...
    Slice(PattSlice<I>),
    /// (a|b),..
    Tuple(PattTuple<I>),
    /// $path(a,b)
    TupleStruct(PattTupleStruct<I>),
    /// $path { a,b }
    Struct(PattStruct<I>),
    /// a,std::collection::HashMap,...
    Path(PattPath<I>),
}

impl<I> From<PattUnary<I>> for Patt<I>
where
    I: LangInput,
{
    fn from(value: PattUnary<I>) -> Self {
        match value {
            PattUnary::Lit(lit) => Self::Lit(lit),
            PattUnary::Rest(patt_rest) => Self::Rest(patt_rest),
            PattUnary::Range(patt_range) => Self::Range(patt_range),
            PattUnary::Wild(patt_wild) => Self::Wild(patt_wild),
            PattUnary::Slice(patt_slice) => Self::Slice(patt_slice),
            PattUnary::Tuple(patt_tuple) => Self::Tuple(patt_tuple),
            PattUnary::TupleStruct(patt_tuple_struct) => Self::TupleStruct(patt_tuple_struct),
            PattUnary::Struct(patt_struct) => Self::Struct(patt_struct),
            PattUnary::Path(type_path) => Self::Path(type_path),
        }
    }
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Patt<I>
where
    I: LangInput,
{
    /// 1,"hello",...
    Lit(PattLit<I>),
    /// ..
    Rest(PattRest<I>),
    /// 1..=2,1..10
    Range(PattRange<I>),
    /// _
    Wild(PattWild<I>),
    /// a: u32,
    Type(PattType<I>),
    /// [a,b,..,c],...
    Slice(PattSlice<I>),
    /// (a|b),..
    Tuple(PattTuple<I>),
    /// $path(a,b)
    TupleStruct(PattTupleStruct<I>),
    /// $path { a,b }
    Struct(PattStruct<I>),
    /// a,std::collection::HashMap,...
    Path(PattPath<I>),
    /// a | b
    Or(PattOr<I>),
}

impl<I> Parse<I> for Patt<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::errors::Result<Self, I, Self::Error> {
        let (patt, input) = PattType::into_parser()
            .map(|v| Self::Type(v))
            .ok()
            .parse(input)?;

        if let Some(patt) = patt {
            return Ok((patt, input));
        }

        let (first, mut input) = PattUnary::into_parser()
            .map(|v| Self::from(v))
            .parse(input)?;

        let mut rest = vec![];

        loop {
            let or;
            (or, input) = SepOr::into_parser().ok().parse(input)?;

            let Some(or) = or else {
                break;
            };

            let rhs;

            (rhs, input) = PattUnary::into_parser()
                .map(|v| Self::from(v))
                .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
                .parse(input)?;

            rest.push((or, rhs));
        }

        if rest.is_empty() {
            Ok((first, input))
        } else {
            Ok((
                Patt::Or(PattOr {
                    first: Box::new(first),
                    rest,
                }),
                input,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Delimiter, Parse};

    use crate::lang::{input::TokenStream, token::*, ty::Type};

    use super::*;

    #[test]
    fn slice() {
        assert_eq!(
            Patt::parse(TokenStream::from("[a,..,b,_]")),
            Ok((
                Patt::Slice(PattSlice(Delimiter {
                    delimiter_start: SepLeftBracket(
                        None,
                        TokenStream {
                            offset: 0,
                            value: "["
                        },
                        None
                    ),
                    body: Punctuated {
                        pairs: vec![
                            (
                                Patt::Path(TypePath {
                                    first: Ident(TokenStream {
                                        offset: 1,
                                        value: "a"
                                    }),
                                    rest: vec![]
                                }),
                                SepComma(
                                    None,
                                    TokenStream {
                                        offset: 2,
                                        value: ","
                                    },
                                    None
                                )
                            ),
                            (
                                Patt::Rest(PattRest(SepDotDot(
                                    None,
                                    TokenStream {
                                        offset: 3,
                                        value: ".."
                                    },
                                    None
                                ))),
                                SepComma(
                                    None,
                                    TokenStream {
                                        offset: 5,
                                        value: ","
                                    },
                                    None
                                )
                            ),
                            (
                                Patt::Path(TypePath {
                                    first: Ident(TokenStream {
                                        offset: 6,
                                        value: "b"
                                    }),
                                    rest: vec![]
                                }),
                                SepComma(
                                    None,
                                    TokenStream {
                                        offset: 7,
                                        value: ","
                                    },
                                    None
                                )
                            )
                        ],
                        tail: Some(Box::new(Patt::Wild(PattWild(TokenUnderscore(
                            TokenStream {
                                offset: 8,
                                value: "_"
                            }
                        )))))
                    },
                    delimiter_end: SepRightBracket(
                        None,
                        TokenStream {
                            offset: 9,
                            value: "]"
                        },
                        None
                    )
                })),
                TokenStream {
                    offset: 10,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn or() {
        assert_eq!(
            Patt::parse(TokenStream::from("[a,..,b,_] | (a,b)")),
            Ok((
                Patt::Or(PattOr {
                    first: Box::new(Patt::Slice(PattSlice(Delimiter {
                        delimiter_start: SepLeftBracket(
                            None,
                            TokenStream {
                                offset: 0,
                                value: "["
                            },
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![
                                (
                                    Patt::Path(TypePath {
                                        first: Ident(TokenStream {
                                            offset: 1,
                                            value: "a"
                                        }),
                                        rest: vec![]
                                    }),
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 2,
                                            value: ","
                                        },
                                        None
                                    )
                                ),
                                (
                                    Patt::Rest(PattRest(SepDotDot(
                                        None,
                                        TokenStream {
                                            offset: 3,
                                            value: ".."
                                        },
                                        None
                                    ))),
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 5,
                                            value: ","
                                        },
                                        None
                                    )
                                ),
                                (
                                    Patt::Path(TypePath {
                                        first: Ident(TokenStream {
                                            offset: 6,
                                            value: "b"
                                        }),
                                        rest: vec![]
                                    }),
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 7,
                                            value: ","
                                        },
                                        None
                                    )
                                )
                            ],
                            tail: Some(Box::new(Patt::Wild(PattWild(TokenUnderscore(
                                TokenStream {
                                    offset: 8,
                                    value: "_"
                                }
                            )))))
                        },
                        delimiter_end: SepRightBracket(
                            None,
                            TokenStream {
                                offset: 9,
                                value: "]"
                            },
                            Some(S(TokenStream {
                                offset: 10,
                                value: " "
                            }))
                        )
                    }))),
                    rest: vec![(
                        SepOr(
                            None,
                            TokenStream {
                                offset: 11,
                                value: "|"
                            },
                            Some(S(TokenStream {
                                offset: 12,
                                value: " "
                            }))
                        ),
                        Patt::Tuple(PattTuple(Delimiter {
                            delimiter_start: SepLeftParen(
                                None,
                                TokenStream {
                                    offset: 13,
                                    value: "("
                                },
                                None
                            ),
                            body: Punctuated {
                                pairs: vec![(
                                    Patt::Path(TypePath {
                                        first: Ident(TokenStream {
                                            offset: 14,
                                            value: "a"
                                        }),
                                        rest: vec![]
                                    }),
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 15,
                                            value: ","
                                        },
                                        None
                                    )
                                )],
                                tail: Some(Box::new(Patt::Path(TypePath {
                                    first: Ident(TokenStream {
                                        offset: 16,
                                        value: "b"
                                    }),
                                    rest: vec![]
                                })))
                            },
                            delimiter_end: SepRightParen(
                                None,
                                TokenStream {
                                    offset: 17,
                                    value: ")"
                                },
                                None
                            )
                        }))
                    )]
                }),
                TokenStream {
                    offset: 18,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn patt_type() {
        assert_eq!(
            Patt::parse(TokenStream::from("value: string")),
            Ok((
                Patt::Type(PattType {
                    ident: Ident(TokenStream {
                        offset: 0,
                        value: "value"
                    }),
                    sep: SepColon(
                        None,
                        TokenStream {
                            offset: 5,
                            value: ":"
                        },
                        Some(S(TokenStream {
                            offset: 6,
                            value: " "
                        }))
                    ),
                    ty: Type::String(TokenString(TokenStream {
                        offset: 7,
                        value: "string"
                    }))
                }),
                TokenStream {
                    offset: 13,
                    value: ""
                }
            ))
        );
    }
}
