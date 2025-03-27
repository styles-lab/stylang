use parserc::{Parse, Parser, ParserExt, keyword};

use super::{Delimiter, ParseError, Punctuated, ReturnType, TokenStream};

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
pub enum Type<I> {
    /// be like: `void`
    Void(I),
    /// be like: `i8,i16,i32,...`
    Int(
        /// signed int bits: 8,16,32,64,...
        Bits,
        I,
    ),
    /// be like: `u8,u16,u32,...`
    Uint(
        /// unsigned int bits: 8,16,32,64,...
        Bits,
        I,
    ),
    /// bignum, be like: `100.0bigint`
    BigInt(I),
    /// bignum, be like: `100.0bignum`
    BigNum(I),
    /// be like: `f32`,
    F32(I),
    /// be like: `f64`
    F64(I),
    /// be like: `string`
    String(I),
    /// be like: `view`
    View(I),
    /// be like: `data`
    Data(I),
    /// be like: `class`
    Class(I),
    /// be like: `[i8]`
    Slice(Delimiter<I>, Box<Type<I>>),
    /// be like: `(i8,string) -> view`
    Fn {
        delimiter: Delimiter<I>,

        params: Punctuated<I, Type<I>, b','>,

        return_stmt: Option<ReturnType<I>>,
    },
}

fn parse_fn_type<I>(input: I) -> parserc::Result<Type<I>, I, ParseError>
where
    I: TokenStream,
{
    let ((delimiter, params), input) = Delimiter::paren(input)?;

    let (return_stmt, input) = ReturnType::into_parser().ok().parse(input)?;

    Ok((
        Type::Fn {
            delimiter,
            params,
            return_stmt,
        },
        input,
    ))
}

fn parse_slice_type<I>(input: I) -> parserc::Result<Type<I>, I, ParseError>
where
    I: TokenStream,
{
    let ((delimeter, ty), input) = Delimiter::bracked(input)?;

    Ok((Type::Slice(delimeter, Box::new(ty)), input))
}

impl<I> Parse<I> for Type<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        keyword("i8")
            .map(|v: I| Type::Int(Bits::L8, v))
            .or(keyword("i16").map(|v: I| Type::Int(Bits::L16, v)))
            .or(keyword("i32").map(|v: I| Type::Int(Bits::L32, v)))
            .or(keyword("i64").map(|v: I| Type::Int(Bits::L64, v)))
            .or(keyword("i128").map(|v: I| Type::Int(Bits::L128, v)))
            .or(keyword("u8").map(|v: I| Type::Uint(Bits::L8, v)))
            .or(keyword("u16").map(|v: I| Type::Uint(Bits::L16, v)))
            .or(keyword("u32").map(|v: I| Type::Uint(Bits::L32, v)))
            .or(keyword("u64").map(|v: I| Type::Uint(Bits::L64, v)))
            .or(keyword("u128").map(|v: I| Type::Uint(Bits::L128, v)))
            .or(keyword("bigint").map(|v: I| Type::BigInt(v)))
            .or(keyword("bignum").map(|v: I| Type::BigNum(v)))
            .or(keyword("f32").map(|v: I| Type::F32(v)))
            .or(keyword("f64").map(|v: I| Type::F64(v)))
            .or(keyword("string").map(|v: I| Type::String(v)))
            .or(keyword("view").map(|v: I| Type::View(v)))
            .or(keyword("data").map(|v: I| Type::Data(v)))
            .or(keyword("class").map(|v: I| Type::Class(v)))
            .or(parse_slice_type)
            .or(parse_fn_type)
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{Bits, Delimiter, Source};

    use super::Type;

    #[test]
    fn parse_type_slice() {
        assert_eq!(
            Type::parse(Source::from("[u8]")),
            Ok((
                Type::Slice(
                    Delimiter::Bracked(Source::from((0, "[")), Source::from((3, "]"))),
                    Box::new(Type::Uint(Bits::L8, Source::from((1, "u8"))))
                ),
                Source::from((4, ""))
            ))
        );

        Type::parse(Source::from("[[string]]")).expect("[[string]]");
    }

    #[test]
    fn parse_fn_type() {
        Type::parse(Source::from("() -> view")).unwrap();
    }
}
