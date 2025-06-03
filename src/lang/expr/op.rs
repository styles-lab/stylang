use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    expr::ExprPath,
    inputs::LangInput,
    lit::Lit,
    meta::MetaList,
    punct::Punctuated,
    tokens::{Comma, LeftParen, Minus, Not, RightParen},
};

/// A variable expr
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLit<I>
where
    I: LangInput,
{
    pub meta_list: MetaList<I>,
    pub lit: Lit<I>,
}

/// A paren expr: `(expr)`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprTuple<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token: `(`
    pub delimiter_start: LeftParen<I>,
    /// tuple items.
    pub items: Punctuated<ExprOperand<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParen<I>,
}

/// A unary operation: !x, *x.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum UnOp<I>
where
    I: LangInput,
{
    /// !x
    Not(Not<I>),
    /// -y
    Neg(Minus<I>),
}

/// A unary operation: !x, -x.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprUnary<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// operator
    pub op: UnOp<I>,
    /// right operand
    pub operand: Box<ExprOperand<I>>,
}

/// Expr that can be used as right oprand.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum ExprOperand<I>
where
    I: LangInput,
{
    /// `(...,...)`
    Tuple(ExprTuple<I>),
    /// `!x` or `-a`
    Unary(ExprUnary<I>),
    /// `2u32.b().c[1]`
    Path(ExprPath<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprOperand, ExprPath, ExprTuple, PathStart},
        inputs::TokenStream,
        path::TypePath,
        punct::Punctuated,
        tokens::{Ident, LeftParen, RightParen},
    };

    #[test]
    fn test_tuple() {
        assert_eq!(
            Expr::parse(TokenStream::from("(a,b,c )")),
            Ok((
                Expr::Operand(ExprOperand::Tuple(ExprTuple {
                    meta_list: Default::default(),
                    delimiter_start: LeftParen(TokenStream::from("(")),
                    items: Punctuated {
                        items: vec![],
                        last: Some(Box::new(ExprOperand::Path(ExprPath {
                            start: PathStart::TypePath(TypePath {
                                meta_list: Default::default(),
                                first: Ident(TokenStream::from((5, "c"))),
                                segments: vec![]
                            }),
                            segments: vec![]
                        })))
                    },
                    delimiter_end: RightParen(TokenStream::from((7, ")")))
                })),
                TokenStream::from((8, ""))
            ))
        );
    }
}
