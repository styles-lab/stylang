use parserc::{ControlFlow, Parse, Parser, ParserExt, Punctuated, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    expr::{
        BinOp, ExprBinary, ExprIf, ExprLoop, ExprPath, ExprUnary, ExprWhile, ExprXml, XmlStart,
    },
    input::LangInput,
    lit::Lit,
    meta::MetaList,
    patt::Patt,
    stmt::Block,
    token::{
        Brace, Bracket, Digits, Ident, KeywordLet, Paren, RangeLimits, SepColon, SepComma, SepEq,
        SepSemiColon,
    },
    ty::TypePath,
};

/// A group of stmts with optional meta-data list.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprBlock<I>
where
    I: LangInput,
{
    /// The leading meta-data list.
    pub meta_list: MetaList<I>,
    /// Stmts group by `{...}`
    pub block: Block<I>,
}

/// A range expr: `a..b`,`a..=`,`..10`,..
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRange<I>
where
    I: LangInput,
{
    // start expr.
    pub start: Option<Box<Expr<I>>>,
    /// rnage token: `..=` or `..`
    pub limits: RangeLimits<I>,
    /// end expr.
    pub end: Option<Box<Expr<I>>>,
}

impl<I> Parse<I> for ExprRange<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (start, input) = Operand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .ok()
            .parse(input)?;

        let (limits, input) = RangeLimits::parse(input)?;

        let (end, input) = Operand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .ok()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .parse(input)?;

        if start.is_none() && end.is_none() {
            return Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::RightOperand,
                input.span(),
            )));
        }

        Ok((Self { start, limits, end }, input))
    }
}

/// A tuple expression: (a, b, c, d).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprTuple<I>(
    pub MetaList<I>,
    pub Paren<I, Punctuated<Expr<I>, SepComma<I>>>,
)
where
    I: LangInput;

/// A name filed init expr: a: 10,
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprField<I>
where
    I: LangInput,
{
    /// parameter name.
    pub ident: Ident<I>,
    /// seperate token: `:`
    pub sep: SepColon<I>,
    /// type declaration clause.
    #[fatal]
    pub value: Box<Expr<I>>,
}
/// fields init expr.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Fields<I>
where
    I: LangInput,
{
    Name(Brace<I, Punctuated<(Ident<I>, SepColon<I>, Expr<I>), SepComma<I>>>),
    Uname(Paren<I, Punctuated<Expr<I>, SepComma<I>>>),
}

/// A tuple expression: (a, b, c, d).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprStruct<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// type path.
    pub ty: TypePath<I>,
    /// field init expr.
    pub fields: Fields<I>,
}

/// Repeat parser: `[0u8;N]`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprRepeat<I>(pub Bracket<I, (Box<Expr<I>>, SepSemiColon<I>, Digits<I>)>)
where
    I: LangInput;

/// Repeat parser: `[0u8;N]`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLet<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `let`
    pub keyword: KeywordLet<I>,
    /// let pattern.
    #[fatal]
    pub patt: (MetaList<I>, Patt<I>),
    /// token `=`
    #[fatal]
    pub eq_token: SepEq<I>,
    /// init expr.
    #[fatal]
    pub expr: Box<Expr<I>>,
}

/// operand for binop/ unop.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub(super) enum Operand<I>
where
    I: LangInput,
{
    Path(ExprPath<I>),
    Unary(ExprUnary<I>),
}

impl<I> From<Operand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: Operand<I>) -> Self {
        match value {
            Operand::Path(expr_path) => expr_path.into(),
            Operand::Unary(expr_unary) => Self::Unary(expr_unary),
        }
    }
}

// Expr parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: LangInput,
{
    Lit(MetaList<I>, Lit<I>),
    TypePath(MetaList<I>, TypePath<I>),
    Struct(ExprStruct<I>),
    Tuple(ExprTuple<I>),
    Range(ExprRange<I>),
    Binary(ExprBinary<I>),
    Unary(ExprUnary<I>),
    Block(ExprBlock<I>),
    Path(ExprPath<I>),
    Repeat(ExprRepeat<I>),
    If(ExprIf<I>),
    Let(ExprLet<I>),
    Loop(ExprLoop<I>),
    While(ExprWhile<I>),
    Xml(ExprXml<I>),
}

impl<I> Parse<I> for Expr<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (expr, input) = ExprRange::into_parser()
            .map(|v| Self::Range(v))
            .or(ExprLet::into_parser().map(|v| Self::Let(v)))
            .or(ExprLoop::into_parser().map(|v| Self::Loop(v)))
            .or(ExprWhile::into_parser().map(|v| Self::While(v)))
            .or(ExprXml::into_parser().map(|v| Self::Xml(v)))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        let (start, mut input) = Operand::parse(input)?;

        let mut rest = vec![];

        loop {
            let (op, op_input) = BinOp::into_parser().ok().parse(input.clone())?;

            let Some(op) = op else {
                break;
            };

            if let BinOp::Lt(_) = op {
                let (xml_start, _) = XmlStart::into_parser().ok().parse(input.clone())?;

                if xml_start.is_some() {
                    break;
                }
            }

            input = op_input;

            let oprand;
            (oprand, input) = Operand::into_parser()
                .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
                .fatal()
                .parse(input)?;

            rest.push((op, oprand.into()));
        }

        if rest.is_empty() {
            Ok((start.into(), input))
        } else {
            Ok((
                Expr::Binary(ExprBinary {
                    start: Box::new(start.into()),
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

    use crate::lang::{
        expr::Expr,
        input::TokenStream,
        lit::{Lit, LitNum, LitStr},
        meta::{Attr, Meta},
        patt::PattType,
        token::*,
        ty::Type,
    };

    use super::*;

    #[test]
    fn expr_struct() {
        assert_eq!(
            Expr::parse(TokenStream::from(r#"Exp { a: 1, b: "hello"}"#)),
            Ok((
                Expr::Struct(ExprStruct {
                    meta_list: vec![],
                    ty: TypePath {
                        first: Ident(TokenStream {
                            offset: 0,
                            value: "Exp"
                        }),
                        rest: vec![]
                    },
                    fields: Fields::Name(Delimiter {
                        delimiter_start: SepLeftBrace(
                            Some(S(TokenStream {
                                offset: 3,
                                value: " "
                            })),
                            TokenStream {
                                offset: 4,
                                value: "{"
                            },
                            Some(S(TokenStream {
                                offset: 5,
                                value: " "
                            }))
                        ),
                        body: Punctuated {
                            pairs: vec![(
                                (
                                    Ident(TokenStream {
                                        offset: 6,
                                        value: "a"
                                    }),
                                    SepColon(
                                        None,
                                        TokenStream {
                                            offset: 7,
                                            value: ":"
                                        },
                                        Some(S(TokenStream {
                                            offset: 8,
                                            value: " "
                                        }))
                                    ),
                                    Expr::Lit(
                                        Default::default(),
                                        Lit::Num(LitNum {
                                            sign: None,
                                            trunc: Some(Digits(TokenStream {
                                                offset: 9,
                                                value: "1"
                                            })),
                                            dot: None,
                                            fract: None,
                                            exp: None,
                                            unit: None
                                        })
                                    ),
                                ),
                                SepComma(
                                    None,
                                    TokenStream {
                                        offset: 10,
                                        value: ","
                                    },
                                    Some(S(TokenStream {
                                        offset: 11,
                                        value: " "
                                    }))
                                )
                            )],
                            tail: Some(Box::new((
                                Ident(TokenStream {
                                    offset: 12,
                                    value: "b"
                                }),
                                SepColon(
                                    None,
                                    TokenStream {
                                        offset: 13,
                                        value: ":"
                                    },
                                    Some(S(TokenStream {
                                        offset: 14,
                                        value: " "
                                    }))
                                ),
                                Expr::Lit(
                                    Default::default(),
                                    Lit::String(LitStr(TokenStream {
                                        offset: 16,
                                        value: "hello"
                                    }))
                                ),
                            )))
                        },
                        delimiter_end: SepRightBrace(
                            None,
                            TokenStream {
                                offset: 22,
                                value: "}"
                            },
                            None
                        )
                    })
                }),
                TokenStream {
                    offset: 23,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn expr_tuple_struct() {
        assert_eq!(
            Expr::parse(TokenStream::from("Some(a)")),
            Ok((
                Expr::Struct(ExprStruct {
                    meta_list: vec![],
                    ty: TypePath {
                        first: Ident(TokenStream {
                            offset: 0,
                            value: "Some"
                        }),
                        rest: vec![]
                    },
                    fields: Fields::Uname(Delimiter {
                        delimiter_start: SepLeftParen(
                            None,
                            TokenStream {
                                offset: 4,
                                value: "("
                            },
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(Expr::TypePath(
                                Default::default(),
                                TypePath {
                                    first: Ident(TokenStream {
                                        offset: 5,
                                        value: "a"
                                    }),
                                    rest: vec![]
                                }
                            ),))
                        },
                        delimiter_end: SepRightParen(
                            None,
                            TokenStream {
                                offset: 6,
                                value: ")"
                            },
                            None
                        )
                    })
                }),
                TokenStream {
                    offset: 7,
                    value: ""
                }
            ))
        );
        assert_eq!(
            Expr::parse(TokenStream::from("None")),
            Ok((
                Expr::TypePath(
                    Default::default(),
                    TypePath {
                        first: Ident(TokenStream {
                            offset: 0,
                            value: "None"
                        }),
                        rest: vec![]
                    }
                ),
                TokenStream {
                    offset: 4,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn expr_tuple() {
        assert_eq!(
            Expr::parse(TokenStream::from("a * (b + c)")),
            Ok((
                Expr::Binary(ExprBinary {
                    start: Box::new(Expr::TypePath(
                        Default::default(),
                        TypePath {
                            first: Ident(TokenStream {
                                offset: 0,
                                value: "a"
                            }),
                            rest: vec![]
                        }
                    ),),
                    rest: vec![(
                        BinOp::Mul(SepStar(
                            Some(S(TokenStream {
                                offset: 1,
                                value: " "
                            })),
                            TokenStream {
                                offset: 2,
                                value: "*"
                            },
                            Some(S(TokenStream {
                                offset: 3,
                                value: " "
                            }))
                        )),
                        Expr::Tuple(ExprTuple(
                            vec![],
                            Delimiter {
                                delimiter_start: SepLeftParen(
                                    None,
                                    TokenStream {
                                        offset: 4,
                                        value: "("
                                    },
                                    None
                                ),
                                body: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(Expr::Binary(ExprBinary {
                                        start: Box::new(Expr::TypePath(
                                            Default::default(),
                                            TypePath {
                                                first: Ident(TokenStream {
                                                    offset: 5,
                                                    value: "b"
                                                }),
                                                rest: vec![]
                                            }
                                        ),),
                                        rest: vec![(
                                            BinOp::Add(SepPlus(
                                                Some(S(TokenStream {
                                                    offset: 6,
                                                    value: " "
                                                })),
                                                TokenStream {
                                                    offset: 7,
                                                    value: "+"
                                                },
                                                Some(S(TokenStream {
                                                    offset: 8,
                                                    value: " "
                                                }))
                                            )),
                                            Expr::TypePath(
                                                Default::default(),
                                                TypePath {
                                                    first: Ident(TokenStream {
                                                        offset: 9,
                                                        value: "c"
                                                    }),
                                                    rest: vec![]
                                                }
                                            ),
                                        )]
                                    })))
                                },
                                delimiter_end: SepRightParen(
                                    None,
                                    TokenStream {
                                        offset: 10,
                                        value: ")"
                                    },
                                    None
                                )
                            }
                        ))
                    )]
                }),
                TokenStream {
                    offset: 11,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn expr_repeat() {
        assert_eq!(
            Expr::parse(TokenStream::from("[Some(1);10]")),
            Ok((
                Expr::Repeat(ExprRepeat(Delimiter {
                    delimiter_start: SepLeftBracket(
                        None,
                        TokenStream {
                            offset: 0,
                            value: "["
                        },
                        None
                    ),
                    body: (
                        Box::new(Expr::Struct(ExprStruct {
                            meta_list: vec![],
                            ty: TypePath {
                                first: Ident(TokenStream {
                                    offset: 1,
                                    value: "Some"
                                }),
                                rest: vec![]
                            },
                            fields: Fields::Uname(Delimiter {
                                delimiter_start: SepLeftParen(
                                    None,
                                    TokenStream {
                                        offset: 5,
                                        value: "("
                                    },
                                    None
                                ),
                                body: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(Expr::Lit(
                                        Default::default(),
                                        Lit::Num(LitNum {
                                            sign: None,
                                            trunc: Some(Digits(TokenStream {
                                                offset: 6,
                                                value: "1"
                                            })),
                                            dot: None,
                                            fract: None,
                                            exp: None,
                                            unit: None
                                        })
                                    ),))
                                },
                                delimiter_end: SepRightParen(
                                    None,
                                    TokenStream {
                                        offset: 7,
                                        value: ")"
                                    },
                                    None
                                )
                            })
                        })),
                        SepSemiColon(
                            None,
                            TokenStream {
                                offset: 8,
                                value: ";"
                            },
                            None
                        ),
                        Digits(TokenStream {
                            offset: 9,
                            value: "10"
                        })
                    ),
                    delimiter_end: SepRightBracket(
                        None,
                        TokenStream {
                            offset: 11,
                            value: "]"
                        },
                        None
                    )
                })),
                TokenStream {
                    offset: 12,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn expr_let() {
        assert_eq!(
            Expr::parse(TokenStream::from(
                "let @state @option value: string = none;"
            )),
            Ok((
                Expr::Let(ExprLet {
                    meta_list: vec![],
                    keyword: KeywordLet(
                        TokenStream {
                            offset: 0,
                            value: "let"
                        },
                        S(TokenStream {
                            offset: 3,
                            value: " "
                        })
                    ),
                    patt: (
                        vec![
                            Meta::Attr(Attr {
                                token: (
                                    None,
                                    TokenAt(TokenStream {
                                        offset: 4,
                                        value: "@"
                                    })
                                ),
                                ident: Ident(TokenStream {
                                    offset: 5,
                                    value: "state"
                                }),
                                params: None
                            }),
                            Meta::Attr(Attr {
                                token: (
                                    Some(S(TokenStream {
                                        offset: 10,
                                        value: " "
                                    })),
                                    TokenAt(TokenStream {
                                        offset: 11,
                                        value: "@"
                                    })
                                ),
                                ident: Ident(TokenStream {
                                    offset: 12,
                                    value: "option"
                                }),
                                params: None
                            }),
                            Meta::S(S(TokenStream {
                                offset: 18,
                                value: " "
                            }))
                        ],
                        Patt::Type(PattType {
                            ident: Ident(TokenStream {
                                offset: 19,
                                value: "value"
                            }),
                            sep: SepColon(
                                None,
                                TokenStream {
                                    offset: 24,
                                    value: ":"
                                },
                                Some(S(TokenStream {
                                    offset: 25,
                                    value: " "
                                }))
                            ),
                            ty: Type::String(TokenString(TokenStream {
                                offset: 26,
                                value: "string"
                            }))
                        })
                    ),
                    eq_token: SepEq(
                        Some(S(TokenStream {
                            offset: 32,
                            value: " "
                        })),
                        TokenStream {
                            offset: 33,
                            value: "="
                        },
                        Some(S(TokenStream {
                            offset: 34,
                            value: " "
                        }))
                    ),
                    expr: Box::new(Expr::TypePath(
                        Default::default(),
                        TypePath {
                            first: Ident(TokenStream {
                                offset: 35,
                                value: "none"
                            }),
                            rest: vec![]
                        }
                    ),)
                }),
                TokenStream {
                    offset: 39,
                    value: ";"
                }
            ))
        );
    }
}
