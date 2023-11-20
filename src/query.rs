use std::marker::PhantomData;

pub trait Qry {
    /// The item returned by this query.
    type Item<'a>;
    type Fetch<'a>;
    type State;
}

#[derive(Debug)]
pub struct Query<Q: Qry> {
    _marker: PhantomData<Q>, // TODO
}

#[derive(Debug)]
pub struct QueryState<Q: Qry> {
    _marker: PhantomData<Q>,
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Or<L, R> {
    Left(L),
    Right(R),
    Both(L, R),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Xor<L, R> {
    Left(L),
    Right(R),
}

