use std::marker::PhantomData;

pub trait Query {
    /// The item returned by this query.
    type Item<'a>;
    type Fetch<'a>;
    type State;
}

pub struct Select<Q: Query> {
    _marker: PhantomData<Q>, // TODO
}

pub struct SelectState<Q: Query> {
    _marker: PhantomData<Q>,
}
