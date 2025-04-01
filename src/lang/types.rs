use parserc::{Parse, Parser, ParserExt, keyword};

use crate::lang::{delimited, parse_punctuation_sep, skip_ws};

use super::{Delimiter, Digits, ParseError, Punctuated, StylangInput};

/// Fn type declaration tokens.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct TypeFn<I> {
    /// prefix keyword `fn`
    pub prefix: I,
    /// delimiter `(...)`
    pub delimiter: Delimiter<I>,
    /// input parameter list.
    pub inputs: Punctuated<I, Type<I>, b','>,
    /// optional returns type.
    pub output: Option<TypeReturn<I>>,
}

impl<I> Parse<I> for TypeFn<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (prefix, input) = keyword("fn").parse(input)?;

        let ((delimiter, inputs), input) =
            delimited("(", Punctuated::<I, Type<I>, b','>::into_parser(), ")").parse(input)?;

        let (output, input) = TypeReturn::into_parser().ok().parse(input)?;

        Ok((
            Self {
                prefix,
                delimiter,
                inputs,
                output,
            },
            input,
        ))
    }
}

/// Returns type used by [`TypeFn`]
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct TypeReturn<I> {
    /// required keyword `->`
    pub prefix: I,
    /// Real returned type.
    pub ty: Box<Type<I>>,
}

impl<I> Parse<I> for TypeReturn<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = skip_ws(input)?;

        let (prefix, input) = keyword("->").parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (ty, input) = Type::parse(input)?;

        Ok((
            TypeReturn {
                prefix,
                ty: Box::new(ty),
            },
            input,
        ))
    }
}

/// Returns type used by [`TypeFn`]
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct TypeSlice<I> {
    /// required keyword `->`
    pub delimiter: Delimiter<I>,
    /// Real returned type.
    pub ty: Box<Type<I>>,
}

impl<I> Parse<I> for TypeSlice<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let ((delimiter, ty), input) =
            delimited("[", Type::<I>::into_parser(), "]").parse(input)?;
        Ok((
            Self {
                delimiter,
                ty: Box::new(ty),
            },
            input,
        ))
    }
}

/// Returns type used by [`TypeFn`]
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct TypeArray<I> {
    /// required keyword `->`
    pub delimiter: Delimiter<I>,
    /// Real returned type.
    pub ty: Box<Type<I>>,
    /// punct `;`.
    pub semi_colon: I,
    /// array length.
    pub len: Digits<I>,
}

fn parse_type_array_content<I>(input: I) -> parserc::Result<(Type<I>, I, Digits<I>), I, ParseError>
where
    I: StylangInput,
{
    let (ty, input) = Type::<I>::parse(input)?;
    let (semi_colon, input) = parse_punctuation_sep(b';').parse(input)?;
    let (len, input) = Digits::parse(input)?;

    Ok(((ty, semi_colon, len), input))
}

impl<I> Parse<I> for TypeArray<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let ((delimiter, (ty, semi_colon, len)), input) =
            delimited("[", parse_type_array_content, "]").parse(input)?;
        Ok((
            Self {
                delimiter,
                ty: Box::new(ty),
                semi_colon,
                len,
            },
            input,
        ))
    }
}

/// Type declaration token.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Type<I> {
    /// primary type, be like: i32,..f64,string,bigint,...
    Primary(I),
    Slice(TypeSlice<I>),
    Array(TypeArray<I>),
    Fn(TypeFn<I>),
}

impl<I> Parse<I> for Type<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        keyword("void")
            .map(|input: I| Self::Primary(input))
            .or(keyword("color").map(|input: I| Self::Primary(input)))
            .or(keyword("data").map(|input: I| Self::Primary(input)))
            .or(keyword("class").map(|input: I| Self::Primary(input)))
            .or(keyword("view").map(|input: I| Self::Primary(input)))
            .or(keyword("string").map(|input: I| Self::Primary(input)))
            .or(keyword("i8").map(|input: I| Self::Primary(input)))
            .or(keyword("i16").map(|input: I| Self::Primary(input)))
            .or(keyword("i32").map(|input: I| Self::Primary(input)))
            .or(keyword("i64").map(|input: I| Self::Primary(input)))
            .or(keyword("i128").map(|input: I| Self::Primary(input)))
            .or(keyword("u8").map(|input: I| Self::Primary(input)))
            .or(keyword("u16").map(|input: I| Self::Primary(input)))
            .or(keyword("u32").map(|input: I| Self::Primary(input)))
            .or(keyword("u64").map(|input: I| Self::Primary(input)))
            .or(keyword("u128").map(|input: I| Self::Primary(input)))
            .or(keyword("f32").map(|input: I| Self::Primary(input)))
            .or(keyword("f64").map(|input: I| Self::Primary(input)))
            .or(keyword("bigint").map(|input: I| Self::Primary(input)))
            .or(keyword("bignum").map(|input: I| Self::Primary(input)))
            .or(TypeSlice::into_parser().map(|ty| Self::Slice(ty)))
            .or(TypeArray::into_parser().map(|ty| Self::Array(ty)))
            .or(TypeFn::<I>::into_parser().map(|ty| Self::Fn(ty)))
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Delimiter, Digits, Punctuated, TokenStream,
        types::{Type, TypeArray, TypeFn, TypeReturn, TypeSlice},
    };

    #[test]
    fn test_slice() {
        assert_eq!(
            Type::parse(TokenStream::from("[bignum]")),
            Ok((
                Type::Slice(TypeSlice {
                    delimiter: Delimiter {
                        prefix: TokenStream::from("["),
                        suffix: TokenStream::from((7, "]")),
                    },
                    ty: Box::new(Type::Primary(TokenStream::from((1, "bignum"))))
                }),
                TokenStream::from((8, ""))
            ))
        );
    }

    #[test]
    fn test_color() {
        assert_eq!(
            Type::parse(TokenStream::from("color,")),
            Ok((
                Type::Primary(TokenStream::from("color")),
                TokenStream::from((5, ","))
            ))
        );
    }

    #[test]
    fn test_array() {
        assert_eq!(
            Type::parse(TokenStream::from("[bignum ;10]")),
            Ok((
                Type::Array(TypeArray {
                    delimiter: Delimiter {
                        prefix: TokenStream::from("["),
                        suffix: TokenStream::from((11, "]")),
                    },
                    ty: Box::new(Type::Primary(TokenStream::from((1, "bignum")))),
                    semi_colon: TokenStream::from((8, ";")),
                    len: Digits(TokenStream::from((9, "10")))
                }),
                TokenStream::from((12, ""))
            ))
        );
    }

    #[test]
    fn test_fn_type() {
        assert_eq!(
            Type::parse(TokenStream::from("fn(i32 , string,bignum,)")),
            Ok((
                Type::Fn(TypeFn {
                    prefix: TokenStream::from("fn"),
                    delimiter: Delimiter {
                        prefix: TokenStream::from((2, "(")),
                        suffix: TokenStream::from((23, ")")),
                    },
                    inputs: Punctuated {
                        items: vec![
                            (
                                Type::Primary(TokenStream::from((3, "i32"))),
                                TokenStream::from((7, ","))
                            ),
                            (
                                Type::Primary(TokenStream::from((9, "string"))),
                                TokenStream::from((15, ","))
                            ),
                            (
                                Type::Primary(TokenStream::from((16, "bignum"))),
                                TokenStream::from((22, ","))
                            )
                        ],
                        last: None,
                    },
                    output: None,
                }),
                TokenStream::from((24, ""))
            ))
        );

        assert_eq!(
            Type::parse(TokenStream::from("fn(i32 , string,bignum,) -> view")),
            Ok((
                Type::Fn(TypeFn {
                    prefix: TokenStream::from("fn"),
                    delimiter: Delimiter {
                        prefix: TokenStream::from((2, "(")),
                        suffix: TokenStream::from((23, ")")),
                    },
                    inputs: Punctuated {
                        items: vec![
                            (
                                Type::Primary(TokenStream::from((3, "i32"))),
                                TokenStream::from((7, ","))
                            ),
                            (
                                Type::Primary(TokenStream::from((9, "string"))),
                                TokenStream::from((15, ","))
                            ),
                            (
                                Type::Primary(TokenStream::from((16, "bignum"))),
                                TokenStream::from((22, ","))
                            )
                        ],
                        last: None,
                    },
                    output: Some(TypeReturn {
                        prefix: TokenStream::from((25, "->")),
                        ty: Box::new(Type::Primary(TokenStream::from((28, "view"))))
                    }),
                }),
                TokenStream::from((32, ""))
            ))
        );
    }
}
