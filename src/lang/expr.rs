use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{Dot, LeftParenthesis, Member, Punctuated, RightParenthesis};

use super::{
    Block, Eq, ExprField, ExprIf, ExprPath, KeywordLet, Lit, MetaList, ParseError, Patt, S,
    StylangInput, XmlEnd, XmlStart, call::ExprCall,
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

            break;
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
}
