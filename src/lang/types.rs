use parserc::{
    AsBytes, Input, Kind, Parse, Parser, ParserExt, keyword,
    span::{Span, WithSpan},
};

use crate::lang::{
    ParseKind,
    s::{parse_comma_sep, parse_return_type_sep},
};

use super::{ParseError, s::skip_ws};

/// Well known int/uint bits.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Bits {
    L8,
    L16,
    L32,
    L64,
    L128,
}

/// primary types for `stylang`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Type {
    /// be like: `void`
    Void(Span),
    /// be like: `i8,i16,i32,...`
    Int(
        /// signed int bits: 8,16,32,64,...
        Bits,
        Span,
    ),
    /// be like: `u8,u16,u32,...`
    Uint(
        /// unsigned int bits: 8,16,32,64,...
        Bits,
        Span,
    ),
    /// be like: `f32`,
    F32(Span),
    /// be like: `f64`
    F64(Span),
    /// be like: `string`
    String(Span),
    /// be like: `view`
    View(Span),
    /// be like: `data`
    Data(Span),
    /// be like: `class`
    Class(Span),
    /// be like: `[i8]`
    Slice(Box<Type>, Span),
    /// be like: `(i8,string) -> view`
    Fn {
        inputs: Vec<Type>,
        output: Box<Type>,
        span: Span,
    },
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Void(span) => *span,
            Type::Int(_, span) => *span,
            Type::Uint(_, span) => *span,
            Type::F32(span) => *span,
            Type::F64(span) => *span,
            Type::String(span) => *span,
            Type::View(span) => *span,
            Type::Data(span) => *span,
            Type::Class(span) => *span,
            Type::Slice(_, span) => *span,
            Type::Fn {
                inputs: _,
                output: _,
                span,
            } => *span,
        }
    }
}

fn parse_fn_type<I>(input: I) -> parserc::Result<Type, I, ParseError>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    let (start, input) = keyword("(").parse(input)?;

    let (_, mut input) = skip_ws(input)?;

    let mut inputs = vec![];

    loop {
        let param;
        (param, input) = Type::into_parser().ok().parse(input)?;

        if let Some(param) = param {
            inputs.push(param);

            let comma;

            (comma, input) = parse_comma_sep.ok().parse(input)?;

            if comma.is_none() {
                break;
            }

            continue;
        }

        break;
    }

    let (_, input) = skip_ws(input)?;

    let (_, input) = keyword(")")
        .map_err(|_: Kind| ParseError::Expect(ParseKind::ParamsEnd, input.span()))
        .parse(input.clone())?;

    let (return_type_sep, input) = parse_return_type_sep.ok().parse(input)?;

    if let Some(_) = return_type_sep {
        let (output, input) = Type::into_parser()
            .map_err(|_| ParseError::Expect(ParseKind::ReturnType, input.span()))
            .fatal()
            .parse(input.clone())?;

        let span = start.span().extend_to_inclusive(output.span());

        Ok((
            Type::Fn {
                inputs,
                output: Box::new(output),
                span,
            },
            input,
        ))
    } else {
        let span = start.span().extend_to(input.span());

        let mut void_span = input.span();

        void_span.len = 0;

        Ok((
            Type::Fn {
                inputs,
                output: Box::new(Type::Void(void_span)),
                span,
            },
            input,
        ))
    }
}

fn parse_slice_type<I>(input: I) -> parserc::Result<Type, I, ParseError>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    let (start, input) = keyword("[").parse(input)?;

    let (_, input) = skip_ws(input)?;

    let (c, input) = Type::into_parser()
        .map_err(|_| ParseError::Expect(ParseKind::SliceComponent, input.span()))
        .fatal()
        .parse(input.clone())?;

    let (_, input) = skip_ws(input)?;

    let (end, input) = keyword("]")
        .map_err(|_: Kind| ParseError::Expect(ParseKind::SliceEnd, input.span()))
        .parse(input.clone())?;

    let span = start.span().extend_to_inclusive(end.span());

    Ok((Type::Slice(Box::new(c), span), input))
}

fn parse_int_type<I>(input: I) -> parserc::Result<Type, I, ParseError>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    keyword("i8")
        .map(|v: I| Type::Int(Bits::L8, v.span()))
        .or(keyword("i16").map(|v: I| Type::Int(Bits::L16, v.span())))
        .or(keyword("i32").map(|v: I| Type::Int(Bits::L32, v.span())))
        .or(keyword("i64").map(|v: I| Type::Int(Bits::L64, v.span())))
        .or(keyword("i128").map(|v: I| Type::Int(Bits::L128, v.span())))
        .parse(input)
}

fn parse_uint_type<I>(input: I) -> parserc::Result<Type, I, ParseError>
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    keyword("u8")
        .map(|v: I| Type::Uint(Bits::L8, v.span()))
        .or(keyword("u16").map(|v: I| Type::Uint(Bits::L16, v.span())))
        .or(keyword("u32").map(|v: I| Type::Uint(Bits::L32, v.span())))
        .or(keyword("u64").map(|v: I| Type::Uint(Bits::L64, v.span())))
        .or(keyword("u128").map(|v: I| Type::Uint(Bits::L128, v.span())))
        .parse(input)
}

impl<I> Parse<I> for Type
where
    I: Input<Item = u8> + AsBytes + WithSpan + Clone,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        parse_int_type
            .or(parse_uint_type)
            .or(keyword("f32").map(|v: I| Type::F32(v.span())))
            .or(keyword("f64").map(|v: I| Type::F64(v.span())))
            .or(keyword("string").map(|v: I| Type::String(v.span())))
            .or(keyword("view").map(|v: I| Type::View(v.span())))
            .or(keyword("data").map(|v: I| Type::Data(v.span())))
            .or(keyword("class").map(|v: I| Type::Class(v.span())))
            .or(parse_slice_type)
            .or(parse_fn_type)
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Parse, span::Span};

    use crate::lang::Bits;

    use super::Type;

    #[test]
    fn parse_type_int() {
        assert_eq!(
            Type::parse((0, b"i8".as_slice())),
            Ok((
                Type::Int(Bits::L8, Span { offset: 0, len: 2 }),
                (2, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"i16".as_slice())),
            Ok((
                Type::Int(Bits::L16, Span { offset: 0, len: 3 }),
                (3, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"i32".as_slice())),
            Ok((
                Type::Int(Bits::L32, Span { offset: 0, len: 3 }),
                (3, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"i64".as_slice())),
            Ok((
                Type::Int(Bits::L64, Span { offset: 0, len: 3 }),
                (3, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"i128".as_slice())),
            Ok((
                Type::Int(Bits::L128, Span { offset: 0, len: 4 }),
                (4, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"u8".as_slice())),
            Ok((
                Type::Uint(Bits::L8, Span { offset: 0, len: 2 }),
                (2, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"u16".as_slice())),
            Ok((
                Type::Uint(Bits::L16, Span { offset: 0, len: 3 }),
                (3, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"u32".as_slice())),
            Ok((
                Type::Uint(Bits::L32, Span { offset: 0, len: 3 }),
                (3, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"u64".as_slice())),
            Ok((
                Type::Uint(Bits::L64, Span { offset: 0, len: 3 }),
                (3, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"u128".as_slice())),
            Ok((
                Type::Uint(Bits::L128, Span { offset: 0, len: 4 }),
                (4, b"".as_slice())
            ))
        );
    }

    #[test]
    fn parse_type_num() {
        assert_eq!(
            Type::parse((0, b"f32".as_slice())),
            Ok((Type::F32(Span { offset: 0, len: 3 }), (3, b"".as_slice())))
        );

        assert_eq!(
            Type::parse((0, b"f64".as_slice())),
            Ok((Type::F64(Span { offset: 0, len: 3 }), (3, b"".as_slice())))
        );
    }

    #[test]
    fn parse_type_string() {
        assert_eq!(
            Type::parse((0, b"string".as_slice())),
            Ok((
                Type::String(Span { offset: 0, len: 6 }),
                (6, b"".as_slice())
            ))
        );
    }

    #[test]
    fn parse_type_complex() {
        assert_eq!(
            Type::parse((0, b"data".as_slice())),
            Ok((Type::Data(Span { offset: 0, len: 4 }), (4, b"".as_slice())))
        );

        assert_eq!(
            Type::parse((0, b"class".as_slice())),
            Ok((Type::Class(Span { offset: 0, len: 5 }), (5, b"".as_slice())))
        );

        assert_eq!(
            Type::parse((0, b"view".as_slice())),
            Ok((Type::View(Span { offset: 0, len: 4 }), (4, b"".as_slice())))
        );
    }

    #[test]
    fn parse_type_slice() {
        assert_eq!(
            Type::parse((0, b"[view]".as_slice())),
            Ok((
                Type::Slice(
                    Box::new(Type::View(Span { offset: 1, len: 4 })),
                    Span { offset: 0, len: 6 }
                ),
                (6, b"".as_slice())
            ))
        );
    }

    #[test]
    fn parse_type_fn() {
        assert_eq!(
            Type::parse((0, b"(view,data) -> i32".as_slice())),
            Ok((
                Type::Fn {
                    inputs: vec![
                        Type::View(Span { offset: 1, len: 4 }),
                        Type::Data(Span { offset: 6, len: 4 }),
                    ],
                    output: Box::new(Type::Int(Bits::L32, Span { offset: 15, len: 3 }),),
                    span: Span { offset: 0, len: 18 }
                },
                (18, b"".as_slice())
            ))
        );

        assert_eq!(
            Type::parse((0, b"(view,data)".as_slice())),
            Ok((
                Type::Fn {
                    inputs: vec![
                        Type::View(Span { offset: 1, len: 4 }),
                        Type::Data(Span { offset: 6, len: 4 }),
                    ],
                    output: Box::new(Type::Void(Span { offset: 11, len: 0 }),),
                    span: Span { offset: 0, len: 11 }
                },
                (11, b"".as_slice())
            ))
        );
    }
}
