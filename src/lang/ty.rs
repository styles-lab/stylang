use parserc::derive_parse;

use super::*;

/// Fn type declaration tokens.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct TypeFn<I>
where
    I: StylangInput,
{
    /// prefix keyword `fn`
    pub keyword_fn: KeywordFn<I>,
    /// delimiter start token `(`
    pub delimiter_start: LeftParenthesis<I>,
    /// input parameter list.
    pub inputs: Punctuated<I, Type<I>, b','>,
    /// delimiter end `)`
    pub delimiter_end: RightParenthesis<I>,
    /// optional returns type.
    pub output: Option<TypeReturn<I>>,
}

/// Returns type used by [`TypeFn`]
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct TypeReturn<I>
where
    I: StylangInput,
{
    /// required keyword `->`
    pub arrow_right: ArrowRight<I>,
    /// Real returned type.
    pub ty: Box<Type<I>>,
}

/// Type array.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct TypeArray<I>
where
    I: StylangInput,
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
#[derive_parse(error = ParseError,input = I)]
pub struct TypeSlice<I>
where
    I: StylangInput,
{
    /// delimiter start token `[`
    pub delimiter_start: LeftBracket<I>,
    pub s1: Option<S<I>>,
    /// Real returned type.
    pub ty: Box<Type<I>>,
    pub s2: Option<S<I>>,
    /// delimiter end token `]`
    pub delimiter_end: RightBracket<I>,
}

/// Type declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Type<I>
where
    I: StylangInput,
{
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
    Length(KeywordLength<I>),
    String(KeywordString<I>),
    Angle(KeywordAngle<I>),
    Slice(TypeSlice<I>),
    Array(TypeArray<I>),
    Fn(TypeFn<I>),
}
