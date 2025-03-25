use parserc::{AsBytes, Input, Kind, Parse, Parser, ParserExt, keyword, span::WithSpan};

use crate::lang::{ParseKind, parse_comments, skip_ws};

use super::{Comment, ParseError, Punctuated, Type, parse_return_type_arrow};

#[derive(Debug, PartialEq)]
pub enum Stat<I> {
    LitStr(I),
}

/// Parsed fn argument.
#[derive(Debug, PartialEq)]
pub struct FnArg<I> {
    /// argument name.
    pub name: I,
    /// colon token: `:`
    pub colon: I,
    /// argument type,
    pub ty: Type,
}

impl<I> Parse<I> for FnArg<I>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    type Error = ParseError;

    fn parse(_: I) -> parserc::Result<Self, I, Self::Error> {
        todo!()
    }
}

/// Parsed fn block.
#[derive(Debug, PartialEq)]
pub struct ItemFn<I> {
    /// optional comment list.
    pub comments: Vec<Comment<I>>,
    /// keyword `fn`
    pub keyword: I,
    /// reqired fn name.
    pub ident: I,
    /// params start tag: `(`
    pub params_start: I,
    /// params list.
    pub params: Punctuated<I, FnArg<I>, b','>,
    /// params end tag: `)`
    pub params_end: I,
    /// return type arrow: `->`
    pub return_type_arrow: I,
    /// returns type.
    pub return_type: Type,
    /// fn body block.
    pub body: Vec<Stat<I>>,
}

impl<I> Parse<I> for ItemFn<I>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (comments, input) = parse_comments(input)?;

        let (kw, input) = keyword("fn").parse(input)?;

        let (ident, input) = skip_ws(input)?;

        let (params_start, input) = keyword("(")
            .map_err(|input: I, _: Kind| ParseError::Expect(ParseKind::ParamsStart, input.span()))
            .fatal()
            .parse(input)?;

        let (params, input) = Punctuated::parse(input)?;

        let (params_end, input) = keyword(")")
            .map_err(|input: I, _: Kind| ParseError::Expect(ParseKind::ParamsEnd, input.span()))
            .fatal()
            .parse(input)?;

        let (return_type_arrow, input) = parse_return_type_arrow
            .map_err(|input: I, _| ParseError::Expect(ParseKind::ReturnTypeArrow, input.span()))
            .parse(input)?;

        let (return_type, input) = Type::parse(input)?;

        Ok((
            ItemFn {
                comments,
                keyword: kw,
                ident,
                params_start,
                params,
                params_end,
                return_type_arrow,
                return_type,
                body: vec![],
            },
            input,
        ))
    }
}
