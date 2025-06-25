use parserc::{
    lang::LangInput,
    parser::Parser,
    span::ToSpan,
    syntax::{PartialSyntax, Syntax},
};

use crate::{
    errors::LangError,
    expr::{ExprIf, ExprLoop, ExprSlice, ExprTuple, ExprUnary, ExprWhile, ExprXml, UnOp},
    lit::Lit,
    meta::MetaList,
    stmt::Block,
    token::*,
    ty::TypePath,
};

/// A lookahead parser for `Expr` parsing.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
enum Lookahead<I>
where
    I: LangInput,
{
    /// A lookahead token `{`, corresponding syntax `ExprBlock`.
    LeftBrace(TokenLeftBrace<I>),
    /// A lookahead token `[`, corresponding syntax `ExprRepeat` or `ExprSlice`.
    LeftBracket(TokenLeftBracket<I>),
    /// A lookahead token `(`, corresponding syntax `ExprTuple`.
    LeftParen(TokenLeftParen<I>),
    /// A lookahead token `<`, corresponding syntax `ExprXml`.
    Lt(TokenLt<I>),
    /// A lookahead keyword `if`, corresponding syntax `ExprIf`.
    If(KeywordIf<I>),
    /// A lookahead keyword `while`, corresponding syntax `ExprWhile`.
    While(KeywordWhile<I>),
    /// A lookahead keyword `loop`, corresponding syntax `ExprLoop`.
    Loop(KeywordLoop<I>),
    /// A lookahead token `!` or `-`, corresponding syntax `ExprUnary`.
    UnOp(UnOp<I>),
    /// A lookahead syntax [`Lit`], corresponding syntax `ExprPath`.
    Lit(Lit<I>),
    /// A lookahead syntax [`TypePath`], corresponding syntax `ExprPath`.
    TypePath(TypePath<I>),
}

/// In `stylang`, everything is expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: LangInput,
{
    /// An ident expression: `a`,`name`,...
    Ident(MetaList<I>, Ident<I>),
    /// An type path expression: `std::collection::Map`,...
    TypePath(MetaList<I>, TypePath<I>),
    /// A literial expression: `1`,`"hello world"`,...
    Lit(MetaList<I>, Lit<I>),
    /// A block expression: `{...}`
    Block(MetaList<I>, Block<I>),
    /// A tuple expression: `(...)`
    Tuple(ExprTuple<I>),
    /// A repeat expression: `[a;10]`
    Slice(ExprSlice<I>),
    /// A unary expression: `-expr` or `!expr`
    Unary(ExprUnary<I>),
    /// A loop expression: `loop {...}`
    Loop(ExprLoop<I>),
    /// A while expression: `while $cond {...}`
    While(ExprWhile<I>),
    /// A if expression: `if $cond {...} else ...`
    If(ExprIf<I>),
    /// A xml expression: `<a>...</a>`
    Xml(ExprXml<I>),
}

impl<I> ToSpan<usize> for Expr<I>
where
    I: LangInput,
{
    fn to_span(&self) -> parserc::lang::Span {
        match self {
            Expr::Lit(metas, lit) => metas.to_span() ^ lit.to_span(),
            Expr::Ident(metas, ident) => metas.to_span() ^ ident.to_span(),
            Expr::Block(metas, block) => metas.to_span() ^ block.to_span(),
            Expr::Tuple(expr_tuple) => expr_tuple.to_span(),
            Expr::TypePath(metas, ty) => metas.to_span() ^ ty.to_span(),
            Expr::Slice(expr) => expr.to_span(),
            Expr::Unary(expr) => expr.to_span(),
            Expr::Loop(expr) => expr.to_span(),
            Expr::While(expr) => expr.to_span(),
            Expr::If(expr) => expr.to_span(),
            Expr::Xml(expr) => expr.to_span(),
        }
    }
}

impl<I> Syntax<I, LangError> for Expr<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let ((meta_list, lookahead), input) = <(MetaList<_>, Lookahead<_>)>::parse(input)?;

        let (expr, input) = match lookahead {
            Lookahead::LeftBrace(token_left_brace) => {
                Block::into_parser_with_prefix(token_left_brace)
                    .fatal()
                    .map(|expr| Expr::Block(meta_list, expr))
                    .parse(input)?
            }
            Lookahead::LeftBracket(token_left_bracket) => {
                ExprSlice::into_parser_with_prefix((meta_list, token_left_bracket))
                    .fatal()
                    .map(|expr| Expr::Slice(expr))
                    .parse(input)?
            }
            Lookahead::LeftParen(token_left_paren) => {
                ExprTuple::into_parser_with_prefix((meta_list, token_left_paren))
                    .fatal()
                    .map(|expr| Expr::Tuple(expr))
                    .parse(input)?
            }
            Lookahead::Loop(keyword) => ExprLoop::into_parser_with_prefix((meta_list, keyword))
                .fatal()
                .map(|expr| Expr::Loop(expr))
                .parse(input)?,
            Lookahead::While(keyword) => ExprWhile::into_parser_with_prefix((meta_list, keyword))
                .fatal()
                .map(|expr| Expr::While(expr))
                .parse(input)?,
            Lookahead::If(keyword) => ExprIf::into_parser_with_prefix((meta_list, keyword))
                .fatal()
                .map(|expr| Expr::If(expr))
                .parse(input)?,
            Lookahead::UnOp(un_op) => ExprUnary::into_parser_with_prefix((meta_list, un_op))
                .fatal()
                .map(|expr| Expr::Unary(expr))
                .parse(input)?,
            Lookahead::TypePath(ty) => {
                // try compress the `ty` syntax.
                if ty.rest.is_empty() {
                    (Expr::Ident(meta_list, ty.first), input)
                } else {
                    (Expr::TypePath(meta_list, ty), input)
                }
            }
            Lookahead::Lit(lit) => (Expr::Lit(meta_list, lit), input),
            Lookahead::Lt(token_lt) => ExprXml::into_parser_with_prefix((meta_list, token_lt))
                .fatal()
                .map(|expr| Expr::Xml(expr))
                .parse(input)?,
        };

        Ok((expr, input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{
        lang::TokenStream,
        syntax::{Delimiter, Syntax},
    };

    use crate::{
        lit::LitNum,
        stmt::{Stmt, Stmts},
    };

    use super::*;

    #[test]
    fn test_block() {
        assert_eq!(
            Expr::parse(TokenStream::from("{1}")),
            Ok((
                Expr::Block(
                    vec![],
                    Block(Delimiter {
                        start: (
                            None,
                            TokenLeftBrace(TokenStream {
                                offset: 0,
                                value: "{"
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 2,
                                value: "}"
                            }),
                            None
                        ),
                        body: Stmts(vec![Stmt::Expr(
                            Expr::Lit(
                                vec![],
                                Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 1,
                                        value: "1"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            ),
                            None,
                            None,
                            None
                        )])
                    })
                ),
                TokenStream {
                    offset: 3,
                    value: ""
                }
            ))
        );
    }
}
