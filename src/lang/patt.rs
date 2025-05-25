//! The parser for pattern syntax .

use parserc::{Parse, Parser, ParserExt, derive_parse};

use super::{
    errors::{LangError, TokenKind},
    expr::{ExprLit, RangeLimits},
    inputs::LangInput,
    lit::Lit,
    meta::MetaList,
    punct::Punctuated,
    tokens::*,
    ty::{Type, TypePath},
};

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattType<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// named variable.
    pub name: Ident<I>,
    /// token `:`
    #[key_field]
    pub colon_token: (Option<S<I>>, Colon<I>, Option<S<I>>),
    /// type declaration.
    pub ty: Box<Type<I>>,
}

/// A type ascription pattern: text::Fill.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattPath<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// type declaration.
    pub path: TypePath<I>,
}

/// A literal in place of an expression: 1, "foo".
pub type PattLit<I> = ExprLit<I>;

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattRange<I>
where
    I: LangInput,
{
    /// optional start operand.
    pub start: Option<Box<Lit<I>>>,
    /// limit types.
    pub limits: RangeLimits<I>,
    /// optional end operand.
    pub end: Option<Box<Lit<I>>>,
}

/// The dots in a tuple or slice pattern: [0, 1, ..].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattRest<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// token `..`
    pub dot_dot_token: DotDot<I>,
}

/// A pattern that matches any one of a set of cases.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattOr<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// cases `a | b ..`
    pub cases: Punctuated<Patt<I>, Or<I>>,
}

/// The dots in a tuple or slice pattern: [0, 1, ..].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattIdent<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// ident name.
    pub ident: Ident<I>,
    /// the subpattern of the ident: binding @..
    pub subpatt: Option<(AtAt<I>, Box<Patt<I>>)>,
}

impl<I> Parse<I> for PattIdent<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (ident, input) = Ident::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (Some(token), input) = AtAt::into_parser().ok().parse(input.clone())? else {
            return Ok((
                Self {
                    meta_list,
                    ident,
                    subpatt: None,
                },
                input,
            ));
        };

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (patt, input) = Patt::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .boxed()
            .parse(input)?;

        return Ok((
            Self {
                meta_list,
                ident,
                subpatt: Some((token, patt)),
            },
            input,
        ));
    }
}

/// A parenthesized pattern: (A | B).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattParen<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token: `(`
    pub delimiter_start: LeftParen<I>,
    /// the inner pattern.
    pub patt: Box<Patt<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParen<I>,
}

/// A tuple pattern: (a, b).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattTuple<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token: `(`
    pub delimiter_start: LeftParen<I>,
    /// tuple elems
    pub elems: Punctuated<Patt<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParen<I>,
}

/// A dynamically sized slice pattern: [a, b, ref i @ .., y, z].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattSlice<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token: `[`
    pub delimiter_start: LeftBrace<I>,
    /// slice elems
    pub elems: Punctuated<Patt<I>, Comma<I>>,
    /// delimiter end token: `]`
    pub delimiter_end: RightBrace<I>,
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Patt<I>
where
    I: LangInput,
{
    Type(PattType<I>),
    Ident(PattIdent<I>),
    Rest(PattRest<I>),
    Paren(PattParen<I>),
    Tuple(PattTuple<I>),
    Range(PattRange<I>),
    Or(PattOr<I>),
    Slice(PattSlice<I>),
    Path(PattPath<I>),
    Lit(PattLit<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        inputs::TokenStream,
        meta::{Attr, Meta, MetaList},
        patt::{PattIdent, PattRest, PattType},
        tokens::{At, AtAt, Colon, DotDot, Ident, KeywordString, S},
        ty::Type,
    };

    use super::Patt;

    #[test]
    fn test_patt_ident() {
        assert_eq!(
            Patt::parse(TokenStream::from("a @@ ..")),
            Ok((
                Patt::Ident(PattIdent {
                    meta_list: Default::default(),
                    ident: Ident(TokenStream::from("a")),
                    subpatt: Some((
                        AtAt(TokenStream::from((2, "@@"))),
                        Box::new(Patt::Rest(PattRest {
                            meta_list: Default::default(),
                            dot_dot_token: DotDot(TokenStream::from((5, "..")))
                        }))
                    ))
                }),
                TokenStream::from((7, ""))
            ))
        );
    }

    #[test]
    fn test_patt_type() {
        assert_eq!(
            Patt::parse(TokenStream::from("@state @option value: string")),
            Ok((
                Patt::Type(PattType {
                    meta_list: MetaList(vec![
                        Meta::Attr(Attr {
                            keyword: At(TokenStream::from("@")),
                            ident: (
                                Ident(TokenStream::from((1, "state"))),
                                Some(S(TokenStream::from((6, " "))))
                            ),
                            params: None
                        }),
                        Meta::Attr(Attr {
                            keyword: At(TokenStream::from((7, "@"))),
                            ident: (
                                Ident(TokenStream::from((8, "option"))),
                                Some(S(TokenStream::from((14, " "))))
                            ),
                            params: None
                        })
                    ]),
                    name: Ident(TokenStream::from((15, "value"))),
                    colon_token: (
                        None,
                        Colon(TokenStream::from((20, ":"))),
                        Some(S(TokenStream::from((21, " "))))
                    ),
                    ty: Box::new(Type::String(KeywordString(TokenStream::from((
                        22, "string"
                    )))))
                }),
                TokenStream::from((28, ""))
            ))
        );
    }
}
