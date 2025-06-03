//! The parser for pattern syntax .

use parserc::{ControlFlow, Parse, Parser, ParserExt, PartialParse, derive_parse};

use crate::lang::path::TypePath;

use super::{
    errors::{LangError, TokenKind},
    expr::ExprLit,
    inputs::LangInput,
    lit::Lit,
    meta::MetaList,
    punct::Punctuated,
    tokens::*,
    ty::Type,
};

/// A type ascription pattern: text::Fill.
pub type PattPath<I> = TypePath<I>;

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

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum _PattOr<I>
where
    I: LangInput,
{
    Type(PattType<I>),
    Ident(PattIdent<I>),
    Tuple(PattTuple<I>),
    Range(PattRange<I>),
    Slice(PattSlice<I>),
    Path(PattPath<I>),
    Lit(PattLit<I>),
}

impl<I> From<_PattOr<I>> for Patt<I>
where
    I: LangInput,
{
    fn from(value: _PattOr<I>) -> Self {
        match value {
            _PattOr::Type(patt_type) => Self::Type(patt_type),
            _PattOr::Ident(v) => Self::Ident(v),
            _PattOr::Tuple(v) => Self::Tuple(v),
            _PattOr::Range(v) => Self::Range(v),
            _PattOr::Slice(v) => Self::Slice(v),
            _PattOr::Path(v) => Self::Path(v),
            _PattOr::Lit(v) => Self::Lit(v),
        }
    }
}

/// A pattern that matches any one of a set of cases.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattOr<I>
where
    I: LangInput,
{
    /// the first pattern.
    pub first: Box<Patt<I>>,
    /// rest segments.
    pub rest: Vec<(Or<I>, Patt<I>)>,
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

/// A pattern that matches any value: `_`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattWild<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// token `_`
    pub under_score_token: Underscore<I>,
}

impl<I> Parse<I> for PattWild<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let span = input.span();
        let (under_score_token, input) = Underscore::parse(input)?;

        match input.iter().next() {
            Some(c) if c != b'_' && !c.is_ascii_alphanumeric() => {}
            None => {}
            _ => {
                return Err(ControlFlow::Recovable(LangError::expect(
                    TokenKind::Token("_"),
                    span,
                )));
            }
        }

        Ok((
            Self {
                meta_list,
                under_score_token,
            },
            input,
        ))
    }
}

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

impl<I> PartialParse<I> for PattType<I>
where
    I: LangInput,
{
    type Error = LangError;
    type Parsed = (MetaList<I>, Ident<I>);

    fn parse((meta_list, name): Self::Parsed, input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::InputParse;

        let (colon_token, input) = input.parse()?;
        let (ty, input) = Type::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Type, input.span()))
            .fatal()
            .boxed()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                name,
                colon_token,
                ty,
            },
            input,
        ))
    }
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

impl<I> PartialParse<I> for PattIdent<I>
where
    I: LangInput,
{
    type Error = LangError;

    type Parsed = (MetaList<I>, Ident<I>);

    fn parse((meta_list, ident): Self::Parsed, input: I) -> parserc::Result<Self, I, Self::Error> {
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

impl<I> Parse<I> for PattIdent<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (ident, input) = Ident::parse(input)?;
        <Self as PartialParse<_>>::parse((meta_list, ident), input)
    }
}

/// A tuple struct or tuple variant pattern: Variant(x, y, .., z).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattTupleStruct<I>
where
    I: LangInput,
{
    /// tuple struct type path.
    pub path: (TypePath<I>, Option<S<I>>),
    /// delimiter start token: `(`
    pub delimiter_start: LeftParen<I>,
    /// tuple elems list.
    pub elems: Punctuated<Patt<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParen<I>,
}

impl<I> PartialParse<I> for PattTupleStruct<I>
where
    I: LangInput,
{
    type Error = LangError;
    type Parsed = TypePath<I>;

    fn parse(parsed: Self::Parsed, input: I) -> parserc::Result<Self, I, Self::Error> {
        let (s, input) = S::into_parser().ok().parse(input)?;
        let (delimiter_start, input) = LeftParen::into_parser().parse(input)?;
        let (elems, input) = Punctuated::parse(input)?;
        let (delimiter_end, input) = RightParen::into_parser().parse(input)?;

        Ok((
            Self {
                path: (parsed, s),
                delimiter_start,
                elems,
                delimiter_end,
            },
            input,
        ))
    }
}

/// Simple Patt types.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum SimplePatt<I>
where
    I: LangInput,
{
    /// ...
    Rest(PattRest<I>),
    /// (...)
    Paren(PattParen<I>),
    /// (a,b,c)
    Tuple(PattTuple<I>),
    /// a..=b, a..b
    Range(PattRange<I>),
    /// [a,b,c]
    Slice(PattSlice<I>),
    /// 1,"hello"
    Lit(PattLit<I>),
    /// `_`
    Wild(PattWild<I>),
}

impl<I> From<SimplePatt<I>> for Patt<I>
where
    I: LangInput,
{
    fn from(value: SimplePatt<I>) -> Self {
        match value {
            SimplePatt::Rest(patt_rest) => Self::Rest(patt_rest),
            SimplePatt::Paren(patt_paren) => Self::Paren(patt_paren),
            SimplePatt::Tuple(patt_tuple) => Self::Tuple(patt_tuple),
            SimplePatt::Range(patt_range) => Self::Range(patt_range),
            SimplePatt::Slice(patt_slice) => Self::Slice(patt_slice),
            SimplePatt::Lit(expr_lit) => Self::Lit(expr_lit),
            SimplePatt::Wild(patt_wild) => Self::Wild(patt_wild),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum ComplexPatt<I>
where
    I: LangInput,
{
    /// a,b,...
    Ident(PattIdent<I>),
    /// a: i32, b: f32,...
    Type(PattType<I>),
    /// Some(a), None, ...
    TupleStruct(PattTupleStruct<I>),
    /// std::path::Path
    Path(PattPath<I>),
}

impl<I> Parse<I> for ComplexPatt<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (path, input) = PattPath::parse(input)?;

        let (expr, input) = PattTupleStruct::into_parser_with(path.clone())
            .map(|v| Self::TupleStruct(v))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        if path.segments.is_empty() {
            return PattType::into_parser_with((path.meta_list.clone(), path.first.clone()))
                .map(|v| Self::Type(v))
                .or(
                    PattIdent::into_parser_with((path.meta_list.clone(), path.first.clone()))
                        .map(|v| Self::Ident(v)),
                )
                .parse(input);
        }

        return Ok((Self::Path(path), input));
    }
}

impl<I> From<ComplexPatt<I>> for Patt<I>
where
    I: LangInput,
{
    fn from(value: ComplexPatt<I>) -> Self {
        match value {
            ComplexPatt::Ident(patt_ident) => Self::Ident(patt_ident),
            ComplexPatt::Type(patt_type) => Self::Type(patt_type),
            ComplexPatt::TupleStruct(patt_tuple_struct) => Self::TupleStruct(patt_tuple_struct),
            ComplexPatt::Path(path) => Self::Path(path),
        }
    }
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Patt<I>
where
    I: LangInput,
{
    /// ...
    Rest(PattRest<I>),
    /// (...)
    Paren(PattParen<I>),
    /// (a,b,c)
    Tuple(PattTuple<I>),
    /// a..=b, a..b
    Range(PattRange<I>),
    /// [a,b,c]
    Slice(PattSlice<I>),
    /// 1,"hello"
    Lit(PattLit<I>),
    /// `_`
    Wild(PattWild<I>),
    /// a,b,...
    Ident(PattIdent<I>),
    /// a: i32, b: f32,...
    Type(PattType<I>),
    /// Some(a), None, ...
    TupleStruct(PattTupleStruct<I>),
    /// std::path::Path
    Path(PattPath<I>),
    /// patt | patt..
    Or(PattOr<I>),
}

impl<I> Parse<I> for Patt<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (first, mut input) = SimplePatt::into_parser()
            .map(|v| Self::from(v))
            .or(ComplexPatt::into_parser().map(|v| Self::from(v)))
            .parse(input)?;

        let mut rest = vec![];

        loop {
            let or_token;
            (or_token, input) = <(Option<S<_>>, Or<_>, Option<S<_>>)>::into_parser()
                .ok()
                .parse(input)?;

            if let Some((_, or_token, _)) = or_token {
                (_, input) = S::into_parser().ok().parse(input)?;
                let patt;
                (patt, input) = _PattOr::into_parser()
                    .map(|v| Patt::from(v))
                    .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
                    .fatal()
                    .parse(input)?;
                rest.push((or_token, patt));
            } else {
                break;
            }
        }

        if rest.is_empty() {
            Ok((first, input))
        } else {
            Ok((
                Patt::Or(PattOr {
                    first: Box::new(first),
                    rest,
                }),
                input,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        inputs::TokenStream,
        meta::{Attr, Meta, MetaList},
        path::TypePath,
        patt::{PattIdent, PattRest, PattTuple, PattTupleStruct, PattType, PattWild},
        punct::Punctuated,
        tokens::{
            At, AtAt, Colon, Comma, DotDot, Ident, KeywordString, LeftParen, RightParen, S,
            Underscore,
        },
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

    #[test]
    fn test_patt_wild() {
        assert_eq!(
            Patt::parse(TokenStream::from("_")),
            Ok((
                Patt::Wild(PattWild {
                    meta_list: Default::default(),
                    under_score_token: Underscore(TokenStream::from("_"))
                }),
                TokenStream::from((1, ""))
            ))
        );
    }

    #[test]
    fn test_patt_tuple() {
        assert_eq!(
            Patt::parse(TokenStream::from((4, "(a,_)"))),
            Ok((
                Patt::Tuple(PattTuple {
                    meta_list: Default::default(),
                    delimiter_start: LeftParen(TokenStream::from((4, "("))),
                    elems: Punctuated {
                        items: vec![(
                            Patt::Ident(PattIdent {
                                meta_list: Default::default(),
                                ident: Ident(TokenStream::from((5, "a"))),
                                subpatt: None
                            }),
                            Comma(TokenStream::from((6, ",")))
                        )],
                        last: Some(Box::new(Patt::Wild(PattWild {
                            meta_list: Default::default(),
                            under_score_token: Underscore(TokenStream::from((7, "_")))
                        })))
                    },
                    delimiter_end: RightParen(TokenStream::from((8, ")")))
                }),
                TokenStream::from((9, ""))
            ))
        );
    }

    #[test]
    fn test_patt_tuple_struct() {
        assert_eq!(
            Patt::parse(TokenStream::from("Some(a)")),
            Ok((
                Patt::TupleStruct(PattTupleStruct {
                    path: (
                        TypePath {
                            meta_list: Default::default(),
                            first: Ident(TokenStream::from("Some")),
                            segments: vec![]
                        },
                        None
                    ),
                    delimiter_start: LeftParen(TokenStream::from((4, "("))),
                    elems: Punctuated {
                        items: vec![],
                        last: Some(Box::new(Patt::Ident(PattIdent {
                            meta_list: Default::default(),
                            ident: Ident(TokenStream::from((5, "a"))),
                            subpatt: None
                        })))
                    },
                    delimiter_end: RightParen(TokenStream::from((6, ")")))
                }),
                TokenStream::from((7, ""))
            ))
        );
    }
}
