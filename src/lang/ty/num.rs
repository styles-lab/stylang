use parserc::{lang::LangInput, syntax::Syntax};

use crate::lang::{errors::LangError, token::*};

/// The parser for num types: `i32`,`f32`,`i128`,..
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[input(I)]
#[error(LangError)]
pub enum TypeNum<I>
where
    I: LangInput,
{
    I8(TokenI8<I>),
    I16(TokenI16<I>),
    I32(TokenI32<I>),
    I64(TokenI64<I>),
    I128(TokenI128<I>),
    U8(TokenU8<I>),
    U16(TokenU16<I>),
    U32(TokenU32<I>),
    U64(TokenU64<I>),
    U128(TokenU128<I>),
    F32(TokenF32<I>),
    F64(TokenF64<I>),
    BigInt(TokenBigInt<I>),
    BigNum(TokenBigNum<I>),
}

#[cfg(test)]
mod tests {
    use parserc::{input::Input, lang::TokenStream, syntax::Syntax};

    use crate::lang::ty::TypeNum;

    #[test]
    fn test_num_types() {
        let tys = [
            "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64",
            "bigint", "bignum",
        ];

        for ty in tys {
            let (_, input) = TypeNum::parse(TokenStream::from(ty)).unwrap();

            assert_eq!(input.len(), 0);
        }
    }
}
