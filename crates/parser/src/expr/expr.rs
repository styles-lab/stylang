use std::vec;

use parserc::{
    lang::LangInput,
    parser::Parser,
    span::ToSpan,
    syntax::{PartialSyntax, Syntax},
};

use crate::{
    errors::{LangError, SyntaxKind},
    expr::{
        AssignOp, BitsOp, BoolOp, CompOp, ExprAssign, ExprBits, ExprBool, ExprComp, ExprFactor,
        ExprIf, ExprLet, ExprLoop, ExprMatch, ExprPath, ExprSlice, ExprTerm, ExprTuple, ExprUnary,
        ExprWhile, ExprXml, FactorOp, PathSegment, TermOp, UnOp,
    },
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
    /// A lookahead keyword `let`, corresponding syntax `ExprLet`.
    Let(KeywordLet<I>),
    /// A lookahead keyword `while`, corresponding syntax `ExprWhile`.
    While(KeywordWhile<I>),
    /// A lookahead keyword `loop`, corresponding syntax `ExprLoop`.
    Loop(KeywordLoop<I>),
    /// A lookahead keyword `match` corresponding syntax `ExprMatch`.
    Match(KeywordMatch<I>),
    /// A lookahead token `!` or `-`, corresponding syntax `ExprUnary`.
    UnOp(UnOp<I>),
    /// A lookahead syntax [`Lit`], corresponding syntax `ExprPath`.
    Lit(Lit<I>),
    /// A lookahead syntax [`TypePath`], corresponding syntax `ExprPath`.
    TypePath(TypePath<I>),
}

/// A lookahead parser for `Expr` parsing.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
enum Infix<I>
where
    I: LangInput,
{
    /// See [`AssignOp`]
    Assign(AssignOp<I>),
    /// See [`BoolOp`]
    Bool(BoolOp<I>),
    /// See [`CompOp`]
    Comp(CompOp<I>),
    /// See [`BitsOp`]
    Bits(BitsOp<I>),
    /// See [`TermOp`]
    Term(TermOp<I>),
    /// See [`FactorOp`]
    Factor(FactorOp<I>),
    /// See [`PathSegment`]
    PathSegment(PathSegment<I>),
}

impl<I> Infix<I>
where
    I: LangInput,
{
    fn priority(&self) -> usize {
        match self {
            Infix::Assign(_) => 0,
            Infix::Bool(_) => 1,
            Infix::Comp(_) => 2,
            Infix::Bits(_) => 3,
            Infix::Term(_) => 4,
            Infix::Factor(_) => 5,
            Infix::PathSegment(_) => 6,
        }
    }

    fn merge(self, left: Expr<I>, right: Expr<I>) -> Expr<I> {
        match self {
            Infix::Assign(op) => Expr::Assign(ExprAssign {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }),
            Infix::Bool(op) => Expr::Bool(ExprBool {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }),
            Infix::Comp(op) => Expr::Comp(ExprComp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }),
            Infix::Bits(op) => Expr::Bits(ExprBits {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }),
            Infix::Term(op) => Expr::Term(ExprTerm {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }),
            Infix::Factor(op) => Expr::Factor(ExprFactor {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }),
            Infix::PathSegment(_) => unreachable!(),
        }
    }
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
    /// A path expression: `std::font::create()`
    Path(ExprPath<I>),
    // A let expression: `let xxx = ...`
    Let(ExprLet<I>),
    /// A assign expression.
    Assign(ExprAssign<I>),
    /// A bool expression: `a && b` or `a || b`
    Bool(ExprBool<I>),
    /// A compare expression: `a > b`,...
    Comp(ExprComp<I>),
    /// A bits expression: `a ^ b`,...
    Bits(ExprBits<I>),
    /// A term expression: `a + b`,...
    Term(ExprTerm<I>),
    /// A factor expression: `a * b`,...
    Factor(ExprFactor<I>),
    /// A match expression: `match ...`
    Match(ExprMatch<I>),
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
            Expr::Path(expr) => expr.to_span(),
            Expr::Let(expr) => expr.to_span(),
            Expr::Assign(expr) => expr.to_span(),
            Expr::Bool(expr) => expr.to_span(),
            Expr::Bits(expr) => expr.to_span(),
            Expr::Comp(expr) => expr.to_span(),
            Expr::Term(expr) => expr.to_span(),
            Expr::Factor(expr) => expr.to_span(),
            Expr::Match(expr) => expr.to_span(),
        }
    }
}

impl<I> Expr<I>
where
    I: LangInput,
{
    fn parse_simple_expr(input: I) -> parserc::errors::Result<Self, I, LangError> {
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
            Lookahead::Let(keyword_let) => {
                ExprLet::into_parser_with_prefix((meta_list, keyword_let))
                    .fatal()
                    .map(|expr| Expr::Let(expr))
                    .parse(input)?
            }
            Lookahead::Match(keyword_match) => {
                ExprMatch::into_parser_with_prefix((meta_list, keyword_match))
                    .fatal()
                    .map(|expr| Expr::Match(expr))
                    .parse(input)?
            }
        };

        Ok((expr, input))
    }
}

impl<I> Syntax<I, LangError> for Expr<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (mut expr, mut input) = Self::parse_simple_expr(input)?;

        if let Expr::Loop(_) | Expr::Unary(_) | Expr::Let(_) | Expr::Xml(_) = expr {
            return Ok((expr, input));
        }

        let mut cached: Vec<(Infix<I>, Expr<I>)> = vec![];

        loop {
            let infix;
            (infix, input) = Infix::into_parser().ok().parse(input)?;

            let Some(infix) = infix else {
                break;
            };

            if let Infix::PathSegment(segment) = infix {
                if let Some((infix, expr)) = cached.pop() {
                    cached.push((
                        infix,
                        Expr::Path(ExprPath {
                            expr: Box::new(expr),
                            segment,
                        }),
                    ));
                } else {
                    expr = Expr::Path(ExprPath {
                        expr: Box::new(expr),
                        segment,
                    });
                }

                continue;
            }

            let expand;
            (expand, input) = Self::parse_simple_expr
                .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
                .fatal()
                .parse(input.clone())?;

            match cached.last() {
                Some((last_infix, _)) if last_infix.priority() < infix.priority() => {
                    let (last_infix, last_expr) = cached.pop().unwrap();

                    cached.push((last_infix, infix.merge(last_expr, expand)));
                }
                _ => {
                    cached.push((infix, expand));
                }
            }
        }

        for (infix, right) in cached {
            expr = infix.merge(expr, right);
        }

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
