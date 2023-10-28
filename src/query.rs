use std::marker::PhantomData;

pub trait Query {
    type State;
}

pub struct Select<Q: Query> {
    _marker: PhantomData<Q>, // TODO
}

pub struct SelectState<Q: Query> {
    _marker: PhantomData<Q>,
}
