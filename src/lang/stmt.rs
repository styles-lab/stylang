use parserc::{Parse, Parser, ParserExt, derive_parse};

use super::{
    errors::{LangError, TokenKind},
    expr::Expr,
    inputs::LangInput,
    item::Item,
    tokens::{S, SemiColon},
};

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Stmt<I>
where
    I: LangInput,
{
    Item(Item<I>),
    Expr(Expr<I>, Option<S<I>>, Option<SemiColon<I>>),
}

impl<I> Stmt<I>
where
    I: LangInput,
{
    fn is_return_expr(&self) -> bool {
        match self {
            Stmt::Expr(Expr::XmlEnd(_), _, _) => false,
            Stmt::Expr(Expr::XmlStart(_), _, _) => false,
            Stmt::Expr(_, _, None) => true,
            _ => false,
        }
    }
}

/// Stmts stream.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Stmts<I>(pub Vec<Stmt<I>>)
where
    I: LangInput;

impl<I> Parse<I> for Stmts<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut stmts = vec![];

        loop {
            let span = input.span();
            let stmt;
            (stmt, input) = Stmt::into_parser().ok().parse(input)?;

            let Some(stmt) = stmt else {
                break;
            };

            if stmts.last().map(|v: &Stmt<I>| v.is_return_expr()) == Some(true) {
                return Err(parserc::ControlFlow::Fatal(LangError::expect(
                    TokenKind::Token("}"),
                    span,
                )));
            }

            stmts.push(stmt);
        }

        Ok((Self(stmts), input))
    }
}
