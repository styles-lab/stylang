use parserc::{Parse, Parser, ParserExt, keyword, next};

use crate::lang::{delimited, parse_attr_comment_list, ws};

use super::{
    AttrOrComment, Delimiter, Fields, Ident, ParseError, Punctuated, StylangInput, TypeReturn,
};

/// Function body block.
#[derive(Debug, PartialEq, Clone)]
pub enum FnBlock<I> {
    SemiColon(I),
    Stats {
        /// delimiter for body: `{...}`
        delimiter: Delimiter<I>,
    },
}

impl<I> Parse<I> for FnBlock<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (semi_colon, input) = next(b';').parse(input)?;

        Ok((Self::SemiColon(semi_colon), input))
    }
}

/// Function declaration.
#[derive(Debug, PartialEq, Clone)]
pub struct Fn<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// func keyword `fn`
    pub keyword: I,
    /// function ident name.
    pub ident: Ident<I>,
    /// parameter list delimiter: `(...)`
    pub delimiter: Delimiter<I>,
    /// function input arguments.
    pub inputs: Punctuated<I, Fields<I>, b','>,
    /// optional returns argument type.
    pub return_ty: Option<TypeReturn<I>>,
    /// fn body block.
    pub block: FnBlock<I>,
}

impl<I> Parse<I> for Fn<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (keyword, input) = keyword("fn").parse(input)?;

        let (_, input) = ws(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (_, input) = ws(input)?;

        let ((delimiter, inputs), input) =
            delimited("(", Punctuated::into_parser(), ")").parse(input)?;

        let (return_ty, input) = TypeReturn::into_parser().ok().parse(input)?;

        let (block, input) = FnBlock::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                keyword,
                ident,
                delimiter,
                inputs,
                return_ty,
                block,
            },
            input,
        ))
    }
}
