use parserc::{
    errors::ControlFlow,
    inputs::lang::LangInput,
    parser::Parser,
    syntax::{Punctuated, Syntax},
};

use crate::lang::{
    errors::{LangError, SyntaxKind},
    expr::{
        ExprAssgin, ExprBits, ExprBool, ExprComp, ExprFactor, ExprIf, ExprLoop, ExprPath, ExprTerm,
        ExprUnary, ExprWhile, ExprXml,
    },
    lit::Lit,
    meta::MetaList,
    patt::Patt,
    stmt::Block,
    token::*,
    ty::TypePath,
};

/// A group of stmts with optional meta-data list.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprBlock<I>
where
    I: LangInput,
{
    /// The leading meta-data list.
    pub meta_list: MetaList<I>,
    /// Stmts group by `{...}`
    pub block: Block<I>,
}

#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
enum RangeOperand<I>
where
    I: LangInput,
{
    Bits(ExprBits<I>),
    Term(ExprTerm<I>),
    Factor(ExprFactor<I>),
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<RangeOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: RangeOperand<I>) -> Self {
        match value {
            RangeOperand::Unary(expr_unary) => Self::Unary(expr_unary),
            RangeOperand::Path(expr_path) => expr_path.into(),
            RangeOperand::Factor(expr_factor) => Self::Factor(expr_factor),
            RangeOperand::Term(expr_term) => Self::Term(expr_term),
            RangeOperand::Bits(expr_term) => Self::Bits(expr_term),
        }
    }
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

impl<I> Syntax<I, LangError> for ExprRange<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (start, input) = RangeOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .ok()
            .parse(input)?;

        let (limits, input) = RangeLimits::parse(input)?;

        let span = input.span();

        let (end, input) = RangeOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .ok()
            .map_err(|_: LangError| LangError::expect(SyntaxKind::RightOperand, span))
            .parse(input)?;

        if start.is_none() && end.is_none() {
            return Err(ControlFlow::Fatal(LangError::expect(
                SyntaxKind::RightOperand,
                input.span(),
            )));
        }

        Ok((Self { start, limits, end }, input))
    }
}

/// A tuple expression: (a, b, c, d).
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprTuple<I>(
    pub MetaList<I>,
    pub Paren<I, Punctuated<Expr<I>, SepComma<I>>>,
)
where
    I: LangInput;

/// A name filed init expr: a: 10,
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprField<I>
where
    I: LangInput,
{
    /// parameter name.
    pub ident: Ident<I>,
    /// seperate token: `:`
    pub sep: (Option<S<I>>, TokenColon<I>, Option<S<I>>),
    /// type declaration clause.
    #[fatal]
    pub value: Box<Expr<I>>,
}
/// fields init expr.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct Fields<I>(
    Brace<
        I,
        Punctuated<(Ident<I>, Option<S<I>>, TokenColon<I>, Option<S<I>>, Expr<I>), SepComma<I>>,
    >,
)
where
    I: LangInput;

/// A tuple expression: (a, b, c, d).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprStruct<I>
where
    I: LangInput,
{
    /// type path.
    pub ty: Box<Expr<I>>,
    /// field init expr.
    pub fields: Fields<I>,
}

impl<I> Syntax<I, LangError> for ExprStruct<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (ty, input) = TypePath::parse(input)?;
        let (fields, input) = Fields::parse(input)?;

        if ty.rest.is_empty() {
            Ok((
                Self {
                    ty: Box::new(Expr::Ident(meta_list, ty.first)),
                    fields,
                },
                input,
            ))
        } else {
            Ok((
                Self {
                    ty: Box::new(Expr::TypePath(meta_list, ty)),
                    fields,
                },
                input,
            ))
        }
    }
}

/// Repeat parser: `[0u8;N]`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprRepeat<I>(
    pub  Bracket<
        I,
        (
            Box<Expr<I>>,
            Option<S<I>>,
            TokenSemiColon<I>,
            Option<S<I>>,
            Digits<I>,
        ),
    >,
)
where
    I: LangInput;

/// Repeat parser: `[0u8;N]`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprLet<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `let`
    pub keyword: (KeywordLet<I>, S<I>),
    /// let pattern.
    #[fatal]
    pub patt: (MetaList<I>, Patt<I>),
    /// token `=`
    #[fatal]
    pub eq_token: (Option<S<I>>, TokenEq<I>, Option<S<I>>),
    /// init expr.
    #[fatal]
    pub expr: Box<Expr<I>>,
}

#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
enum _Expr<I>
where
    I: LangInput,
{
    Tuple(ExprTuple<I>),
    Unary(ExprUnary<I>),
    Block(ExprBlock<I>),
    Repeat(ExprRepeat<I>),
    If(ExprIf<I>),
    Let(ExprLet<I>),
    Loop(ExprLoop<I>),
    While(ExprWhile<I>),
    Xml(ExprXml<I>),
    Assgin(ExprAssgin<I>),
    Bool(ExprBool<I>),
    Comp(ExprComp<I>),
    Bits(ExprBits<I>),
    Term(ExprTerm<I>),
    Factor(ExprFactor<I>),
    Struct(ExprStruct<I>),
    Range(ExprRange<I>),
    Path(ExprPath<I>),
    Ident(MetaList<I>, Ident<I>),
    Lit(MetaList<I>, Lit<I>),
    TypePath(MetaList<I>, TypePath<I>),
}

// Expr parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: LangInput,
{
    Tuple(ExprTuple<I>),
    Unary(ExprUnary<I>),
    Block(ExprBlock<I>),
    Repeat(ExprRepeat<I>),
    If(ExprIf<I>),
    Let(ExprLet<I>),
    Loop(ExprLoop<I>),
    While(ExprWhile<I>),
    Xml(ExprXml<I>),
    Assgin(ExprAssgin<I>),
    Bool(ExprBool<I>),
    Comp(ExprComp<I>),
    Bits(ExprBits<I>),
    Term(ExprTerm<I>),
    Factor(ExprFactor<I>),
    Struct(ExprStruct<I>),
    Range(ExprRange<I>),
    Path(ExprPath<I>),
    Ident(MetaList<I>, Ident<I>),
    Lit(MetaList<I>, Lit<I>),
    TypePath(MetaList<I>, TypePath<I>),
}

impl<I> Syntax<I, LangError> for Expr<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (expr, input) = _Expr::parse(input)?;

        let expr = match expr {
            _Expr::Tuple(expr_tuple) => Self::Tuple(expr_tuple),
            _Expr::Unary(expr_unary) => Self::Unary(expr_unary),
            _Expr::Block(expr_block) => Self::Block(expr_block),
            _Expr::Repeat(expr_repeat) => Self::Repeat(expr_repeat),
            _Expr::If(expr_if) => Self::If(expr_if),
            _Expr::Let(expr_let) => Self::Let(expr_let),
            _Expr::Loop(expr_loop) => Self::Loop(expr_loop),
            _Expr::While(expr_while) => Self::While(expr_while),
            _Expr::Xml(expr_xml) => Self::Xml(expr_xml),
            _Expr::Assgin(expr_assgin) => Self::Assgin(expr_assgin),
            _Expr::Bool(expr_bool) => Self::Bool(expr_bool),
            _Expr::Comp(expr_comp) => Self::Comp(expr_comp),
            _Expr::Bits(expr_bits) => Self::Bits(expr_bits),
            _Expr::Term(expr_term) => Self::Term(expr_term),
            _Expr::Factor(expr_factor) => Self::Factor(expr_factor),
            _Expr::Struct(expr_struct) => Self::Struct(expr_struct),
            _Expr::Range(expr_range) => Self::Range(expr_range),
            _Expr::Path(expr_path) => expr_path.into(),
            _Expr::Ident(metas, ident) => Self::Ident(metas, ident),
            _Expr::Lit(metas, lit) => Self::Lit(metas, lit),
            _Expr::TypePath(metas, type_path) => Self::TypePath(metas, type_path),
        };

        Ok((expr, input))
    }
}

#[cfg(test)]
mod tests {

    use parserc::{inputs::lang::TokenStream, syntax::Delimiter};

    use crate::lang::{
        expr::{PathCall, PathSegment},
        lit::{LitNum, LitStr},
        meta::{Attr, Meta},
        patt::PattType,
        ty::Type,
    };

    use super::*;

    #[test]
    fn expr_struct() {
        assert_eq!(
            Expr::parse(TokenStream::from(r#"Exp { a: 1, b: "hello"}"#)),
            Ok((
                Expr::Struct(ExprStruct {
                    ty: Box::new(Expr::Ident(
                        vec![],
                        Ident(TokenStream {
                            offset: 0,
                            value: "Exp"
                        })
                    )),
                    fields: Fields(Delimiter {
                        start: (
                            Some(S(TokenStream {
                                offset: 3,
                                value: " "
                            })),
                            TokenLeftBrace(TokenStream {
                                offset: 4,
                                value: "{"
                            }),
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
                                    None,
                                    TokenColon(TokenStream {
                                        offset: 7,
                                        value: ":"
                                    }),
                                    Some(S(TokenStream {
                                        offset: 8,
                                        value: " "
                                    })),
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
                                (
                                    None,
                                    TokenComma(TokenStream {
                                        offset: 10,
                                        value: ","
                                    }),
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
                                None,
                                TokenColon(TokenStream {
                                    offset: 13,
                                    value: ":"
                                }),
                                Some(S(TokenStream {
                                    offset: 14,
                                    value: " "
                                })),
                                Expr::Lit(
                                    Default::default(),
                                    Lit::String(LitStr(TokenStream {
                                        offset: 16,
                                        value: "hello"
                                    }))
                                ),
                            )))
                        },
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 22,
                                value: "}"
                            }),
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
    fn expr_repeat() {
        assert_eq!(
            Expr::parse(TokenStream::from("[Some(1);10]")),
            Ok((
                Expr::Repeat(ExprRepeat(Delimiter {
                    start: (
                        None,
                        TokenLeftBracket(TokenStream {
                            offset: 0,
                            value: "["
                        }),
                        None
                    ),
                    body: (
                        Box::new(Expr::Path(ExprPath {
                            first: Box::new(Expr::Ident(
                                vec![],
                                Ident(TokenStream {
                                    offset: 1,
                                    value: "Some"
                                })
                            )),
                            rest: vec![PathSegment::Call(PathCall(Delimiter {
                                start: (
                                    None,
                                    TokenLeftParen(TokenStream {
                                        offset: 5,
                                        value: "("
                                    }),
                                    None
                                ),
                                body: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(Expr::Lit(
                                        vec![],
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
                                    )))
                                },
                                end: (
                                    None,
                                    TokenRightParen(TokenStream {
                                        offset: 7,
                                        value: ")"
                                    }),
                                    None
                                )
                            }))]
                        })),
                        None,
                        TokenSemiColon(TokenStream {
                            offset: 8,
                            value: ";"
                        }),
                        None,
                        Digits(TokenStream {
                            offset: 9,
                            value: "10"
                        })
                    ),
                    end: (
                        None,
                        TokenRightBracket(TokenStream {
                            offset: 11,
                            value: "]"
                        }),
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
                    keyword: (
                        KeywordLet(TokenStream {
                            offset: 0,
                            value: "let"
                        }),
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
                            sep: (
                                None,
                                TokenColon(TokenStream {
                                    offset: 24,
                                    value: ":"
                                }),
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
                    eq_token: (
                        Some(S(TokenStream {
                            offset: 32,
                            value: " "
                        })),
                        TokenEq(TokenStream {
                            offset: 33,
                            value: "="
                        }),
                        Some(S(TokenStream {
                            offset: 34,
                            value: " "
                        }))
                    ),
                    expr: Box::new(Expr::Lit(
                        Default::default(),
                        Lit::None(TokenNone(TokenStream {
                            offset: 35,
                            value: "none"
                        })),
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
