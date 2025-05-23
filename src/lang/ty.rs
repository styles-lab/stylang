//! syntax analyser for type declaration.

use parserc::{Parse, Parser, ParserExt, derive_parse};

use super::{errors::LangError, inputs::LangInput, meta::MetaList, punct::Punctuated, tokens::*};

/// Fn type declaration tokens.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct TypeFn<I>
where
    I: LangInput,
{
    /// prefix keyword `fn`
    pub keyword_fn: KeywordFn<I>,
    /// delimiter start token `(`
    pub delimiter_start: LeftParen<I>,
    /// input parameter list.
    pub inputs: Punctuated<Type<I>, Comma<I>>,
    /// delimiter end `)`
    pub delimiter_end: RightParen<I>,
    /// optional returns type.
    pub output: Option<TypeReturn<I>>,
}

/// Returns type used by [`TypeFn`]
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct TypeReturn<I>
where
    I: LangInput,
{
    /// required keyword `->`
    pub arrow_right: (Option<S<I>>, ArrowRight<I>, Option<S<I>>),
    /// Real returned type.
    pub ty: Box<Type<I>>,
}

/// Type array.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct TypeArray<I>
where
    I: LangInput,
{
    /// delimiter start token `[`
    pub delimiter_start: LeftBracket<I>,
    pub s1: Option<S<I>>,
    /// Real returned type.
    pub ty: Box<Type<I>>,
    pub s2: Option<S<I>>,
    /// punct `;`
    pub semi_colon: SemiColon<I>,
    pub s3: Option<S<I>>,
    /// array length value.
    pub len: Digits<I>,
    pub s4: Option<S<I>>,
    /// delimiter end token `]`
    pub delimiter_end: RightBracket<I>,
}

/// Type array.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct TypeSlice<I>
where
    I: LangInput,
{
    /// delimiter start token `[`
    pub delimiter_start: LeftBracket<I>,
    /// space after delimiter start.
    pub s1: Option<S<I>>,
    /// Real returned type.
    pub ty: Box<Type<I>>,
    /// space before delimiter end.
    pub s2: Option<S<I>>,
    /// delimiter end token `]`
    pub delimiter_end: RightBracket<I>,
}

/// Type path.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TypePath<I>
where
    I: LangInput,
{
    /// optional meta information list.
    pub meta_list: MetaList<I>,
    /// first segment .
    pub first: Ident<I>,
    /// The remaining segments of the path.
    pub rest: Vec<(PathSep<I>, Ident<I>)>,
}

impl<I> Parse<I> for TypePath<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;

        let (first, mut input) = Ident::parse(input)?;

        let mut rest = vec![];

        loop {
            (_, input) = S::into_parser().ok().parse(input)?;
            let sep;
            (sep, input) = PathSep::into_parser().ok().parse(input)?;

            if let Some(sep) = sep {
                (_, input) = S::into_parser().ok().parse(input)?;
                let ident;
                (ident, input) = Ident::into_parser().fatal().parse(input)?;
                rest.push((sep, ident));
                continue;
            }

            break;
        }

        Ok((
            Self {
                meta_list,
                first,
                rest,
            },
            input,
        ))
    }
}

/// Type declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Type<I>
where
    I: LangInput,
{
    Bool(Bool<I>),
    I8(I8<I>),
    I16(I16<I>),
    I32(I32<I>),
    I64(I64<I>),
    I128(I128<I>),
    U8(U8<I>),
    U16(U16<I>),
    U32(U32<I>),
    U64(U64<I>),
    U128(U128<I>),
    BigInt(BigInt<I>),
    BigNum(BigNum<I>),
    F32(F32<I>),
    F64(F64<I>),
    Length(KeywordLength<I>),
    String(KeywordString<I>),
    Angle(KeywordAngle<I>),
    Color(KeywordColor<I>),
    View(KeywordView<I>),
    Slice(TypeSlice<I>),
    Array(TypeArray<I>),
    Fn(TypeFn<I>),
    Path(TypePath<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::inputs::TokenStream;

    use super::*;

    #[test]
    fn test_slice() {
        assert_eq!(
            Type::parse(TokenStream::from("[bignum]")),
            Ok((
                Type::Slice(TypeSlice {
                    delimiter_start: LeftBracket(TokenStream::from("[")),
                    s1: None,
                    ty: Box::new(Type::BigNum(BigNum(TokenStream::from((1, "bignum"))))),
                    s2: None,
                    delimiter_end: RightBracket(TokenStream::from((7, "]")))
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
                Type::Color(KeywordColor(TokenStream::from("color"))),
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
                    delimiter_start: LeftBracket(TokenStream::from("[")),
                    s1: None,
                    ty: Box::new(Type::BigNum(BigNum(TokenStream::from((1, "bignum"))))),
                    s2: Some(S(TokenStream::from((7, " ")))),
                    semi_colon: SemiColon(TokenStream::from((8, ";"))),
                    s3: None,
                    len: Digits(TokenStream::from((9, "10"))),
                    s4: None,
                    delimiter_end: RightBracket(TokenStream::from((11, "]")))
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
                    keyword_fn: KeywordFn(TokenStream::from("fn")),
                    delimiter_start: LeftParen(TokenStream::from((2, "("))),
                    inputs: Punctuated {
                        items: vec![
                            (
                                Type::I32(I32(TokenStream::from((3, "i32")))),
                                Comma(TokenStream::from((7, ",")))
                            ),
                            (
                                Type::String(KeywordString(TokenStream::from((9, "string")))),
                                Comma(TokenStream::from((15, ",")))
                            ),
                            (
                                Type::BigNum(BigNum(TokenStream::from((16, "bignum")))),
                                Comma(TokenStream::from((22, ",")))
                            )
                        ],
                        last: None
                    },
                    delimiter_end: RightParen(TokenStream::from((23, ")"))),
                    output: None
                }),
                TokenStream::from((24, ""))
            ))
        );

        assert_eq!(
            Type::parse(TokenStream::from("fn(i32 , string,bignum,) -> view")),
            Ok((
                Type::Fn(TypeFn {
                    keyword_fn: KeywordFn(TokenStream::from("fn")),
                    delimiter_start: LeftParen(TokenStream::from((2, "("))),
                    inputs: Punctuated {
                        items: vec![
                            (
                                Type::I32(I32(TokenStream::from((3, "i32")))),
                                Comma(TokenStream::from((7, ",")))
                            ),
                            (
                                Type::String(KeywordString(TokenStream::from((9, "string")))),
                                Comma(TokenStream::from((15, ",")))
                            ),
                            (
                                Type::BigNum(BigNum(TokenStream::from((16, "bignum")))),
                                Comma(TokenStream::from((22, ",")))
                            )
                        ],
                        last: None
                    },
                    delimiter_end: RightParen(TokenStream::from((23, ")"))),
                    output: Some(TypeReturn {
                        arrow_right: (
                            Some(S(TokenStream::from((24, " ")))),
                            ArrowRight(TokenStream::from((25, "->"))),
                            Some(S(TokenStream::from((27, " "))))
                        ),
                        ty: Box::new(Type::View(KeywordView(TokenStream::from((28, "view"))))),
                    })
                }),
                TokenStream::from((32, ""))
            ))
        );
    }
}
