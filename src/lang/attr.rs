use super::Ident;

/// Attribute, be like: `@option`,`@state`,...
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Attr<I> {
    /// attribute prefix character `@`,
    pub prefix: I,
    /// attribute ident.
    pub ident: Ident<I>,
}
