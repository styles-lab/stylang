use parserc::{
    AsBytes, Input, Kind, Parse, Parser, ParserExt, keyword, next, satisfy, span::WithSpan,
    take_till, take_while,
};

use super::{ParseError, ParseKind, TokenStream};

pub(super) fn skip_ws<I>(input: I) -> parserc::Result<I, I, ParseError>
where
    I: Input<Item = u8>,
{
    let (s, input) = take_while(|c: u8| c.is_ascii_whitespace()).parse(input)?;

    Ok((s, input))
}

#[allow(unused)]
pub(super) fn ensure_ws<I>(input: I) -> parserc::Result<(), I, ParseError>
where
    I: Input<Item = u8> + WithSpan,
{
    let (s, input) = skip_ws(input)?;

    if s.is_empty() {
        let mut span = input.span();
        span.len = 0;
        return Err(parserc::ControlFlow::Fatal(ParseError::Expect(
            ParseKind::S,
            span,
        )));
    }

    Ok(((), input))
}

pub(super) fn parse_punctuated_sep<I>(p: u8) -> impl Parser<I, Error = ParseError, Output = I>
where
    I: Input<Item = u8>,
{
    move |input: I| {
        let (_, input) = skip_ws(input)?;

        let (comma, input) = next(p).parse(input)?;

        let (_, input) = skip_ws(input)?;

        Ok((comma, input))
    }
}

/// be like: `[S]->[S]`
pub(super) fn parse_return_type_arrow<I>(input: I) -> parserc::Result<I, I, ParseError>
where
    I: Input<Item = u8> + AsBytes,
{
    let (_, input) = skip_ws(input)?;

    let (sep, input) = keyword("->").parse(input)?;

    let (_, input) = skip_ws(input)?;

    Ok((sep, input))
}

/// Comment of the function, be like: `/// ...`
#[derive(Debug, PartialEq, Clone)]
pub struct Comment<I>(pub I);

impl<I> Parse<I> for Comment<I>
where
    I: Input<Item = u8> + AsBytes + WithSpan,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("///").parse(input)?;

        let (content, input) = take_till(|c| c == b'\n').parse(input)?;

        Ok((Comment(content), input))
    }
}

/// Parse multiline comments.
pub fn parse_comments<I>(mut input: I) -> parserc::Result<Vec<Comment<I>>, I, ParseError>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    let mut comments = vec![];

    (_, input) = skip_ws(input)?;

    loop {
        let comment;

        (comment, input) = Comment::into_parser().ok().parse(input)?;

        if let Some(comment) = comment {
            comments.push(comment);
            (_, input) = skip_ws(input)?;
        } else {
            return Ok((comments, input));
        }
    }
}

/// A punctuated sequence of syntax tree nodes of type T separated by punctuation of type P.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Punctuated<I, T, const P: u8> {
    pub items: Vec<(T, I)>,
    pub last: Option<Box<T>>,
}

impl<I, T, const P: u8> Parse<I> for Punctuated<I, T, P>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
    T: Parse<I, Error = ParseError>,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut items = vec![];

        let (_, mut input) = skip_ws(input)?;

        loop {
            let item;

            (item, input) = T::into_parser().ok().parse(input)?;

            if let Some(item) = item {
                let punctuated;
                (punctuated, input) = parse_punctuated_sep(P).ok().parse(input)?;

                if let Some(punctuated) = punctuated {
                    items.push((item, punctuated));
                } else {
                    return Ok((
                        Punctuated {
                            items,
                            last: Some(Box::new(item)),
                        },
                        input,
                    ));
                }
            } else {
                return Ok((Punctuated { items, last: None }, input));
            }
        }
    }
}

fn delimited<I, P>(
    start: u8,
    mut parser: P,
    end: u8,
) -> impl Parser<I, Output = (I, I, P::Output), Error = ParseError>
where
    I: TokenStream,
    P: Parser<I, Error = ParseError> + Clone,
{
    move |input: I| {
        let (start, input) = next(start).parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (ty, input) = parser.parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (end, input) = next(end)
            .map_err(|input: I, _: Kind| {
                ParseError::Expect(ParseKind::Delimiter(end as char), input.span())
            })
            .fatal()
            .parse(input)?;

        Ok(((start, end, ty), input))
    }
}

/// A token surround by `start` and `end` tokens.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Delimiter<I> {
    /// be like: `(...)`
    Paren(I, I),
    /// be like: `{...}`
    Brace(I, I),
    /// be like: `[...]`
    Bracked(I, I),
}

impl<I> Delimiter<I>
where
    I: TokenStream,
{
    /// parse `(...)`
    pub fn paren<T>(input: I) -> parserc::Result<(Delimiter<I>, T), I, ParseError>
    where
        T: Parse<I, Error = ParseError> + Clone,
    {
        let ((start, end, ty), input) = delimited(b'(', T::into_parser(), b')').parse(input)?;

        Ok(((Delimiter::Paren(start, end), ty), input))
    }

    /// parse `{...}`
    pub fn brace<T>(input: I) -> parserc::Result<(Delimiter<I>, T), I, ParseError>
    where
        T: Parse<I, Error = ParseError> + Clone,
    {
        let ((start, end, ty), input) = delimited(b'{', T::into_parser(), b'}').parse(input)?;

        Ok(((Delimiter::Brace(start, end), ty), input))
    }

    /// parse `[...]`
    pub fn bracked<T>(input: I) -> parserc::Result<(Delimiter<I>, T), I, ParseError>
    where
        T: Parse<I, Error = ParseError> + Clone,
    {
        let ((start, end, ty), input) = delimited(b'[', T::into_parser(), b']').parse(input)?;

        Ok(((Delimiter::Bracked(start, end), ty), input))
    }

    /// Return the start tag.
    pub fn start(&self) -> &I {
        match self {
            Delimiter::Paren(start, _) => start,
            Delimiter::Brace(start, _) => start,
            Delimiter::Bracked(start, _) => start,
        }
    }

    /// Return the end tag.
    pub fn end(&self) -> &I {
        match self {
            Delimiter::Paren(_, end) => end,
            Delimiter::Brace(_, end) => end,
            Delimiter::Bracked(_, end) => end,
        }
    }
}

/// Ident for fn name and others,...
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Ident<I>(pub I);

impl<I> Parse<I> for Ident<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut cloned = input.clone();

        let (_, input) = satisfy(|c: u8| c.is_ascii_alphabetic() || c == b'_')
            .map_err(|input: I, _: Kind| ParseError::Expect(ParseKind::Ident, input.span()))
            .parse(input)?;

        let (body, _) =
            take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_' || c == b'-').parse(input)?;

        Ok((Ident(cloned.split_to(body.len() + 1)), cloned))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{Bits, Comment, Source, Type, parse_comments};

    use super::Delimiter;

    #[test]
    fn parse_comment() {
        assert_eq!(
            Comment::parse(Source::from("/// hello world  \n")),
            Ok((
                Comment(Source::from((3, " hello world  ")),),
                Source::from((17, "\n"))
            ))
        );

        assert_eq!(
            parse_comments(Source::from("/// hello world  \n\t\n/// hello world  ")),
            Ok((
                vec![
                    Comment(Source::from((3, " hello world  ")),),
                    Comment(Source::from((23, " hello world  ")),)
                ],
                Source::from((37, ""))
            ))
        );
    }

    #[test]
    fn parse_delimiter() {
        assert_eq!(
            Delimiter::paren(Source::from("(i32)")),
            Ok((
                (
                    Delimiter::Paren(Source::from((0, "(")), Source::from((4, ")"))),
                    Type::Int(Bits::L32, Source::from((1, "i32")))
                ),
                Source::from((5, ""))
            ))
        );
    }
}
