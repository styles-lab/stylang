use parserc::{
    lang::LangInput,
    syntax::{Punctuated, Syntax},
};

use crate::lang::{errors::LangError, token::*, ty::Type};

/// The parser for num types: `i32`,`f32`,`i128`,..
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[input(I)]
#[error(LangError)]
pub struct TypeFn<I>
where
    I: LangInput,
{
    /// `fn` token.
    pub fn_token: KeywordFn<I>,
    /// input parameter list.
    pub params: Paren<I, Punctuated<Type<I>, SepComma<I>>>,
    /// optional return type part: `-> i32`
    pub return_ty: Option<(SepArrowRight<I>, Box<Type<I>>)>,
}

#[cfg(test)]
mod tests {

    use parserc::{lang::TokenStream, syntax::Delimiter};

    use crate::lang::ty::TypeNum;

    use super::*;

    #[test]
    fn with_return_ty() {
        assert_eq!(
            TypeFn::parse(TokenStream::from("fn() -> i32")),
            Ok((
                TypeFn {
                    fn_token: KeywordFn(TokenStream {
                        offset: 0,
                        value: "fn"
                    }),
                    params: Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 2,
                                value: "("
                            },),
                            None
                        ),
                        end: (
                            None,
                            TokenRightParen(TokenStream {
                                offset: 3,
                                value: ")"
                            },),
                            Some(S(TokenStream {
                                offset: 4,
                                value: " "
                            }))
                        ),
                        body: Punctuated {
                            pairs: vec![],
                            tail: None
                        }
                    },
                    return_ty: Some((
                        (
                            None,
                            TokenArrowRight(TokenStream {
                                offset: 5,
                                value: "->"
                            }),
                            Some(S(TokenStream {
                                offset: 7,
                                value: " "
                            }))
                        ),
                        Box::new(Type::Num(TypeNum::I32(TokenI32(TokenStream {
                            offset: 8,
                            value: "i32"
                        }))))
                    ))
                },
                TokenStream {
                    offset: 11,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn without_return_ty() {
        assert_eq!(
            TypeFn::parse(TokenStream::from("fn(string,f32 ) -> i32")),
            Ok((
                TypeFn {
                    fn_token: KeywordFn(TokenStream {
                        offset: 0,
                        value: "fn"
                    }),
                    params: Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 2,
                                value: "("
                            }),
                            None
                        ),
                        end: (
                            Some(S(TokenStream {
                                offset: 13,
                                value: " "
                            })),
                            TokenRightParen(TokenStream {
                                offset: 14,
                                value: ")"
                            }),
                            Some(S(TokenStream {
                                offset: 15,
                                value: " "
                            }))
                        ),
                        body: Punctuated {
                            pairs: vec![(
                                Type::String(TokenString(TokenStream {
                                    offset: 3,
                                    value: "string"
                                })),
                                (
                                    None,
                                    TokenComma(TokenStream {
                                        offset: 9,
                                        value: ","
                                    }),
                                    None
                                )
                            )],
                            tail: Some(Box::new(Type::Num(TypeNum::F32(TokenF32(TokenStream {
                                offset: 10,
                                value: "f32"
                            })))))
                        },
                    },
                    return_ty: Some((
                        (
                            None,
                            TokenArrowRight(TokenStream {
                                offset: 16,
                                value: "->"
                            }),
                            Some(S(TokenStream {
                                offset: 18,
                                value: " "
                            }))
                        ),
                        Box::new(Type::Num(TypeNum::I32(TokenI32(TokenStream {
                            offset: 19,
                            value: "i32"
                        }))))
                    ))
                },
                TokenStream {
                    offset: 22,
                    value: ""
                }
            ))
        );
    }
}
