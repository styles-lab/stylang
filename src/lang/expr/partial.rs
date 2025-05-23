use std::marker::PhantomData;

use parserc::Parser;

use crate::lang::{errors::LangError, inputs::LangInput};

use super::rr::RightRecursive;

pub(super) trait PartialParse<I>: Sized
where
    I: LangInput,
{
    fn partial_parse(left: RightRecursive<I>, input: I) -> parserc::Result<Self, I, LangError>;
}

pub(super) struct Partial<T, I>(RightRecursive<I>, PhantomData<T>)
where
    I: LangInput;

impl<T, I> From<RightRecursive<I>> for Partial<T, I>
where
    I: LangInput,
    T: PartialParse<I>,
{
    fn from(value: RightRecursive<I>) -> Self {
        Self(value, Default::default())
    }
}

impl<T, I> Parser<I> for Partial<T, I>
where
    I: LangInput,
    T: PartialParse<I>,
{
    type Output = T;

    type Error = LangError;

    fn parse(&mut self, input: I) -> parserc::Result<Self::Output, I, Self::Error> {
        T::partial_parse(self.0.clone(), input)
    }
}
