use super::Type;

#[derive(Debug, PartialEq)]
pub enum Stat<I> {
    LitStr(I),
}

#[derive(Debug, PartialEq)]
pub struct ItemFn<I> {
    pub name: I,
    pub inputs: Vec<Type>,
    pub output: Type,
    pub body: Vec<Stat<I>>,
}
