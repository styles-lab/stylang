use parserc::{Parse, Parser, ParserExt};

use crate::lang::{
    errors::{LangError, TokenKind},
    expr::{ExprField, parse::PartialParse},
    inputs::LangInput,
    meta::MetaList,
    tokens::{Dot, DotStart, LeftBracket, LeftParenthesis},
};

use super::{ExprBlock, ExprCall, ExprIf, ExprIndex, ExprLet, ExprLit, ExprPath, ExprXml};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum LeadingToken<I>
where
    I: LangInput,
{
    LeftParenthesis(LeftParenthesis<I>),
    LeftBracket(LeftBracket<I>),
    Dot(Dot<I>),
}

impl<I> Parse<I> for LeadingToken<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let span = input.span();
        let (dot, input) = DotStart::into_parser().ok().parse(input)?;

        match dot {
            Some(DotStart::Dot(dot)) => Ok((Self::Dot(dot), input)),
            Some(_) => Err(parserc::ControlFlow::Recovable(LangError::expect(
                TokenKind::Token(". or [ or ("),
                span,
            ))),
            _ => LeftBracket::into_parser()
                .map(|v| Self::LeftBracket(v))
                .or(LeftParenthesis::into_parser().map(|v| Self::LeftParenthesis(v)))
                .parse(input),
        }
    }
}

/// A Rust expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: LangInput,
{
    Xml(ExprXml<I>),
    Block(ExprBlock<I>),
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
    Field(ExprField<I>),
    Call(ExprCall<I>),
    Index(ExprIndex<I>),
    Let(ExprLet<I>),
    If(ExprIf<I>),
}

impl<I> Parse<I> for Expr<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (mut expr, mut input) = ExprXml::into_parser()
            .map(|v| Self::Xml(v))
            .or(ExprIf::into_parser().map(|v| Self::If(v)))
            .or(ExprBlock::into_parser().map(|v| Self::Block(v)))
            .or(ExprLit::into_parser().map(|v| Self::Lit(v)))
            .or(ExprLet::into_parser().map(|v| Self::Let(v)))
            .or(ExprPath::into_parser().map(|v| Self::Path(v)))
            .parse(input)?;

        match &expr {
            Expr::Lit(_) | Expr::Path(_) => {}
            _ => return Ok((expr, input)),
        }

        loop {
            let (meta_list, input_leading) = MetaList::parse(input.clone())?;

            let (Some(leading_token), input_leading) =
                LeadingToken::into_parser().ok().parse(input_leading)?
            else {
                return Ok((expr, input));
            };

            match leading_token {
                LeadingToken::LeftParenthesis(leading_token) => {
                    (expr, input) =
                        ExprCall::partial_parse(meta_list, expr, leading_token, input_leading)?;
                }
                LeadingToken::LeftBracket(leading_token) => {
                    (expr, input) =
                        ExprIndex::partial_parse(meta_list, expr, leading_token, input_leading)?;
                }
                LeadingToken::Dot(leading_token) => {
                    (expr, input) =
                        ExprField::partial_parse(meta_list, expr, leading_token, input_leading)?;
                }
            }
        }
    }
}
