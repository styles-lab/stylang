use parserc::{Parse, Parser, keyword};

use crate::lang::{delimited, parse_attr_comment_list, parse_punctuation_sep, skip_ws};

use super::{AttrOrComment, Delimiter, Ident, ParseError, Punctuated, StylangInput, Type};

/// Parsed attribute declaration.
#[derive(Debug, PartialEq, Clone)]
pub struct NamedField<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// required attribute ident.
    pub ident: Ident<I>,
    /// punct: `:`
    pub semi_colon: I,
    /// required attribute type declaration.
    pub ty: Type<I>,
}

impl<I> Parse<I> for NamedField<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;
        let (ident, input) = Ident::parse(input)?;
        let (semi_colon, input) = parse_punctuation_sep(b',').parse(input)?;
        let (ty, input) = Type::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                ident,
                semi_colon,
                ty,
            },
            input,
        ))
    }
}

/// Parsed class type declaration.
#[derive(Debug, PartialEq, Clone)]
pub struct Class<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// keyword: `class`.
    pub keyword: I,
    /// class body delimiter: `{...}`
    pub delimiter: Delimiter<I>,
    /// attr declartion list.
    pub named_fields: Punctuated<I, NamedField<I>, b','>,
}

impl<I> Parse<I> for Class<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (keyword, input) = keyword("class").parse(input)?;

        let (_, input) = skip_ws(input)?;

        let ((delimiter, named_fields), input) =
            delimited("{", Punctuated::into_parser(), "}").parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                keyword,
                delimiter,
                named_fields,
            },
            input,
        ))
    }
}
