use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse};

use crate::lang::{Dot, LeftParenthesis, Member, Punctuated, RightParenthesis};

use super::{
    BinOp, Block, DotDot, DotDotEq, Eq, ExprBinary, ExprField, ExprIf, ExprIndex, ExprPath,
    ExprRange, ExprUnary, KeywordLet, LeftBracket, Lit, MetaList, ParseError, Patt, RangeLimits,
    RightBracket, S, StylangInput, TokenError, XmlEnd, XmlStart, call::ExprCall,
};

/// A local let binding: let x: u64 = 10.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprLet<I>
where
    I: StylangInput,
{
    /// optional metadata list.
    pub meta_list: MetaList<I>,
    /// keyword `let`.
    pub let_token: (KeywordLet<I>, S<I>),
    /// let binding patt.
    pub patt: Patt<I>,
    /// equal token: `=`
    pub eq_token: (Option<S<I>>, Eq<I>, Option<S<I>>),
    /// init expr part.
    pub expr: Box<Expr<I>>,
}

/// A variable expr
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprLit<I>
where
    I: StylangInput,
{
    pub meta_list: MetaList<I>,
    pub lit: Lit<I>,
}

/// A block expr
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprBlock<I>
where
    I: StylangInput,
{
    pub meta_list: MetaList<I>,
    pub block: Block<I>,
}

/// A parenthesized expression: (a + b).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprParen<I>
where
    I: StylangInput,
{
    pub meta_list: MetaList<I>,
    pub delimiter_start: LeftParenthesis<I>,
    pub expr: Box<Expr<I>>,
    pub delimiter_end: RightParenthesis<I>,
}

/// A Rust expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: StylangInput,
{
    Let(ExprLet<I>),
    Field(ExprField<I>),
    Call(ExprCall<I>),
    XmlStart(XmlStart<I>),
    XmlEnd(XmlEnd<I>),
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
    Block(ExprBlock<I>),
    If(ExprIf<I>),
    Binary(ExprBinary<I>),
    Unary(ExprUnary<I>),
    Paren(ExprParen<I>),
    Index(ExprIndex<I>),
    Range(ExprRange<I>),
}

impl<I> Parse<I> for Expr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (expr, input) = ExprLet::into_parser()
            .map(|v| Expr::Let(v))
            .or(XmlEnd::into_parser().map(|v| Expr::XmlEnd(v)))
            .or(XmlStart::into_parser().map(|v| Expr::XmlStart(v)))
            .or(ExprBlock::into_parser().map(|v| Expr::Block(v)))
            .or(ExprIf::into_parser().map(|v| Expr::If(v)))
            .or(ExprUnary::into_parser().map(|v| Expr::Unary(v)))
            .or(ExprParen::into_parser().map(|v| Expr::Paren(v)))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        let (mut expr, mut input) = ExprLit::into_parser()
            .map(|v| Expr::Lit(v))
            .or(ExprPath::into_parser().map(|v| Expr::Path(v)))
            .parse(input)?;

        loop {
            (_, input) = S::into_parser().ok().parse(input)?;

            let (None, _) = DotDotEq::into_parser()
                .map(|_| ())
                .or(DotDot::into_parser().map(|_| ()))
                .ok()
                .parse(input.clone())?
            else {
                break;
            };

            let dot_token;

            (dot_token, input) = Dot::into_parser().ok().parse(input)?;

            if let Some(dot_token) = dot_token {
                (_, input) = S::into_parser().ok().parse(input)?;
                let member;

                (member, input) = Member::parse(input)?;
                expr = Expr::Field(ExprField {
                    target: Box::new(expr),
                    dot_token,
                    member,
                });

                continue;
            }

            let delimiter_start;

            (delimiter_start, input) = LeftParenthesis::into_parser().ok().parse(input)?;

            if let Some(delimiter_start) = delimiter_start {
                let delimiter_end;

                let params;

                (params, input) = Punctuated::parse(input)?;

                (delimiter_end, input) = RightParenthesis::parse(input)?;

                expr = Expr::Call(ExprCall {
                    target: Box::new(expr),
                    delimiter_start,
                    params,
                    delimiter_end,
                });

                continue;
            }

            let delimiter_start;

            (delimiter_start, input) = LeftBracket::into_parser().ok().parse(input)?;

            if let Some(delimiter_start) = delimiter_start {
                let delimiter_end;

                let start;

                (start, input) = Expr::into_parser().ok().parse(input)?;

                let limits;

                (limits, input) = RangeLimits::into_parser().ok().parse(input)?;

                if let Some(limits) = limits {
                    let end;
                    (end, input) = Expr::into_parser().ok().parse(input)?;
                    (delimiter_end, input) = RightBracket::parse(input)?;

                    expr = Expr::Index(ExprIndex {
                        expr: Box::new(expr),
                        delimiter_start,
                        index: Box::new(Expr::Range(ExprRange {
                            start: start.map(|v| Box::new(v)),
                            limits,
                            end: end.map(|v| Box::new(v)),
                        })),
                        delimiter_end,
                    });
                } else {
                    let Some(start) = start else {
                        return Err(ControlFlow::Fatal(ParseError::Expect(
                            TokenError::Index,
                            input.span(),
                        )));
                    };

                    (delimiter_end, input) = RightBracket::parse(input)?;

                    expr = Expr::Index(ExprIndex {
                        expr: Box::new(expr),
                        delimiter_start,
                        index: Box::new(start),
                        delimiter_end,
                    });
                }

                continue;
            }

            break;
        }

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (op, input) = BinOp::into_parser().ok().parse(input)?;

        if let Some(op) = op {
            let (_, input) = S::into_parser().ok().parse(input)?;
            let (rhs, input) = Expr::parse(input)?;

            return Ok((
                Expr::Binary(ExprBinary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(rhs),
                }),
                input,
            ));
        }

        Ok((expr, input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::*;

    #[test]
    fn test_let() {
        assert_eq!(
            ExprLet::parse(TokenStream::from(
                "let @state @option value: string = none;",
            )),
            Ok((
                ExprLet {
                    meta_list: MetaList(vec![]),
                    let_token: (
                        KeywordLet(TokenStream::from((0, "let"))),
                        S(TokenStream::from((3, " ")))
                    ),
                    patt: Patt::Type(PattType {
                        meta_list: MetaList(vec![
                            Meta::Attr(Attr {
                                keyword: At(TokenStream::from((4, "@"))),
                                ident: (
                                    Ident(TokenStream::from((5, "state"))),
                                    Some(S(TokenStream::from((10, " "))))
                                ),
                                params: None
                            }),
                            Meta::Attr(Attr {
                                keyword: At(TokenStream::from((11, "@"))),
                                ident: (
                                    Ident(TokenStream::from((12, "option"))),
                                    Some(S(TokenStream::from((18, " "))))
                                ),
                                params: None
                            })
                        ]),
                        name: Ident(TokenStream::from((19, "value"))),
                        colon_token: (
                            None,
                            Colon(TokenStream::from((24, ":"))),
                            Some(S(TokenStream::from((25, " "))))
                        ),
                        ty: Box::new(Type::String(KeywordString(TokenStream::from((
                            26, "string"
                        )))))
                    }),
                    eq_token: (
                        Some(S(TokenStream::from((32, " ")))),
                        Eq(TokenStream::from((33, "="))),
                        Some(S(TokenStream::from((34, " "))))
                    ),
                    expr: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::None(KeywordNone(TokenStream::from((35, "none"))))
                    }))
                },
                TokenStream::from((39, ";"))
            ))
        );
    }

    #[test]
    fn test_paren() {
        assert_eq!(
            Expr::parse(TokenStream::from("((a))")),
            Ok((
                Expr::Paren(ExprParen {
                    meta_list: MetaList(vec![]),
                    delimiter_start: LeftParenthesis(TokenStream::from("(")),
                    expr: Box::new(Expr::Paren(ExprParen {
                        meta_list: MetaList(vec![]),
                        delimiter_start: LeftParenthesis(TokenStream::from((1, "("))),
                        expr: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from((2, "a"))),
                            tails: vec![]
                        })),
                        delimiter_end: RightParenthesis(TokenStream::from((3, ")")))
                    })),
                    delimiter_end: RightParenthesis(TokenStream::from((4, ")")))
                }),
                TokenStream::from((5, ""))
            ))
        );
    }
}
