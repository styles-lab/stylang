use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    patt::Patt,
    tokens::{
        KeywordBreak, KeywordElse, KeywordFor, KeywordIf, KeywordIn, KeywordLoop, KeywordReturn, S,
    },
};

use super::{Block, Expr, ExprBlock};

/// An if expression with an optional else block: if expr { ... } else { ... }.
///
/// The else branch expression may only be an If or Block expression, not any of the other types of expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprIf<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// token `if`
    pub if_token: KeywordIf<I>,
    /// if condition expr.
    pub cond: Box<Expr<I>>,
    /// required then code block.
    pub then_branch: ExprBlock<I>,
    /// optional else block.
    pub else_branch: Option<(Option<S<I>>, KeywordElse<I>, Option<S<I>>, Box<Expr<I>>)>,
}

/// A return, with an optional value to be returned.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprReturn<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `return`
    pub return_token: KeywordReturn<I>,
    /// Optional returns value expr.
    pub expr: Option<Box<Expr<I>>>,
}

/// A break, with an optional expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprBreak<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `break`
    pub break_token: KeywordBreak<I>,
    /// Optional break expr.
    pub expr: Option<Box<Expr<I>>>,
}

/// A for loop: for pat in expr { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprFor<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `for`
    pub for_token: KeywordFor<I>,
    /// for pattern expr.
    pub patt: Box<Patt<I>>,
    /// keyword `in`
    pub in_token: KeywordIn<I>,
    /// in expr.
    pub expr: Box<Expr<I>>,
    /// block of for expr.
    pub block: Block<I>,
}

/// Conditionless loop: loop { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLoop<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `loop`
    pub loop_token: KeywordLoop<I>,
    /// block of for expr.
    pub block: Block<I>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprPath, ExprReturn},
        inputs::TokenStream,
        meta::MetaList,
        tokens::{Ident, KeywordReturn},
    };

    #[test]
    fn test_return() {
        assert_eq!(
            Expr::parse(TokenStream::from("return;")),
            Ok((
                Expr::Return(ExprReturn {
                    meta_list: MetaList::default(),
                    return_token: KeywordReturn(TokenStream::from("return")),
                    expr: None,
                }),
                TokenStream::from((6, ";"))
            ))
        );
    }

    #[test]
    fn test_return_with_expr() {
        assert_eq!(
            Expr::parse(TokenStream::from("return a;")),
            Ok((
                Expr::Return(ExprReturn {
                    meta_list: MetaList::default(),
                    return_token: KeywordReturn(TokenStream::from("return")),
                    expr: Some(Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList::default(),
                        first: Ident(TokenStream::from((7, "a"))),
                        segments: vec![]
                    }))),
                }),
                TokenStream::from((8, ";"))
            ))
        );
    }
}
