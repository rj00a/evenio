use std::marker::PhantomData;

use crate::SystemParam;

pub trait Query {
    type State;
}

pub struct Select<Q: Query> {
    _marker: PhantomData<Q>, // TODO
}

/*
impl<Q: Query> SystemParam for Select<Q> {
    type State = SelectState<Q>;

    type Item<'a> = SelectState<Q>;

    unsafe fn new_state(args: &mut crate::SystemInitArgs) -> Self::State {
        todo!()
    }

    unsafe fn get_param<'a>(state: &'a mut Self::State) -> Option<Self::Item<'a>> {
        todo!()
    }
}*/

pub struct SelectState<Q: Query> {
    _marker: PhantomData<Q>,
}
