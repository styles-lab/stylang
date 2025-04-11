use super::{AttrOrComment, Ident, Lit, Type};

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Patt<I> {
    Lit(Lit<I>),
    Ident(PattIdent<I>),
    Type(PattType<I>),
}

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattType<I> {
    /// atrribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// variable binding pattern.
    pub pat: Box<Patt<I>>,
    /// seperator `:`
    pub colon_token: I,
    /// type declaration.
    pub ty: Box<Type<I>>,
}

/// A pattern that binds a new variable:
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattIdent<I> {
    /// atrribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// variable ident.
    pub ident: Ident<I>,
}
