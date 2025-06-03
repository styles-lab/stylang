use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    expr::{Expr, ExprLit},
    inputs::LangInput,
    path::TypePath,
    punct::Punctuated,
    tokens::{Comma, Digits, Dot, Ident, LeftBracket, LeftParen, RightBracket, RightParen, S},
};

/// The start element of a path expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum PathStart<I>
where
    I: LangInput,
{
    Lit(ExprLit<I>),
    TypePath(TypePath<I>),
}

/// A call segment of one path expression: `(...)`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Call<I>
where
    I: LangInput,
{
    /// leading whitespace chars.
    pub leading_s: Option<S<I>>,
    /// delimiter start: `(`
    pub delimiter_start: LeftParen<I>,
    /// parameter list.
    pub params: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end: `)`
    pub delimiter_end: RightParen<I>,
}

/// A index segment of one path expression: `[...]`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Index<I>
where
    I: LangInput,
{
    /// leading whitespace chars.
    pub leading_s: Option<S<I>>,
    /// delimiter start: `]`
    pub delimiter_start: LeftBracket<I>,
    /// index expr.
    pub expr: Box<Expr<I>>,
    /// delimiter end: `]`
    pub delimiter_end: RightBracket<I>,
}

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Member<I>
where
    I: LangInput,
{
    Named(Ident<I>),
    Unnamed(Digits<I>),
}

/// Right part for accessing struct filed.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Field<I>
where
    I: LangInput,
{
    /// dot token `.`
    pub dot_token: (Option<S<I>>, Dot<I>, Option<S<I>>),
    /// member expr.
    pub member: Member<I>,
}

/// The rest segement type of one path expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum PathSegment<I>
where
    I: LangInput,
{
    /// `(...)`
    Call(Call<I>),
    /// `[...]`
    Index(Index<I>),
    /// `.1` or `.a`
    Field(Field<I>),
}

/// The start element of a path expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprPath<I>
where
    I: LangInput,
{
    /// the first segment of the path.
    pub start: PathStart<I>,
    /// The rest elements of the path.
    pub segments: Vec<PathSegment<I>>,
}
