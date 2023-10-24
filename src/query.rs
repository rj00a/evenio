use std::marker::PhantomData;

use crate::SystemParam;

pub trait WorldQuery {
    type State;
}

pub struct Query<Q: WorldQuery> {
    _marker: PhantomData<Q>, // TODO
}

impl<Q: WorldQuery> SystemParam for Query<Q> {
    type State = QueryState<Q>;

    type Item<'a> = QueryState<Q>;

    unsafe fn init(world: &mut crate::World, access: &mut crate::SystemAccess) -> Self::State {
        todo!()
    }

    unsafe fn get_param<'a>(state: &'a mut Self::State) -> Self::Item<'a> {
        todo!()
    }
}

pub struct QueryState<Q: WorldQuery> {
    _marker: PhantomData<Q>,
}
