use std::error::Error;
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;

use evenio_macros::all_tuples;

use crate::access::FilteredAccessExpr;
use crate::archetype::{Archetype, ArchetypeRow};
use crate::component::ComponentId;
use crate::entity::EntityId;
use crate::prelude::{Component, World};
use crate::system::{SystemConfig, SystemParam};
use crate::world::UnsafeWorldCell;

pub struct Query<'a, 's, Q: WorldQuery> {
    world: UnsafeWorldCell<'a>,
    state: &'s QueryState<Q>,
}

impl<'a, 's, Q: WorldQuery> Query<'a, 's, Q> {
    // TODO
}

impl<Q> SystemParam for Query<'_, '_, Q>
where
    Q: WorldQuery + 'static,
{
    type State = QueryState<Q>;

    type Item<'s, 'a> = Query<'s, 'a, Q>;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        // TODO: iterate over archetypes and initialize dense and sparse arrays.

        todo!();

        Ok(QueryState {
            dense: vec![],
            sparse: vec![],
        })
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: crate::event::EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        todo!()
    }
}

pub struct QueryState<Q: WorldQuery> {
    /// Fetched data from archetypes that match this query. Used for iteration.
    dense: Vec<Q::Fetch>,
    /// Mapping from archetype ID to the fetched data for the archetype. `None`
    /// means the query doesn't match the archetype. Used for random access
    /// `Entity` lookups.
    sparse: Vec<Option<Q::Fetch>>,
}

pub unsafe trait WorldQuery {
    /// The item returned by this query. This is usually same type as `Self`,
    /// but with a transformed lifetime.
    type Item<'a>;
    /// Per-archetype state.
    type Fetch: Send + Sync + Clone + 'static;
    /// Cached data for fetch initialization. This is stored in [`QueryState`].
    type State: Send + Sync + 'static;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State);

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch>;

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_>;
}

pub trait ReadOnlyWorldQuery: WorldQuery {
    unsafe fn fetch(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'_>;
}

unsafe impl<C: Component> WorldQuery for &'_ C {
    type Item<'a> = &'a C;

    type Fetch = Column<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let id = world.init_component::<C>();

        let mut expr = FilteredAccessExpr::new();
        expr.and_read(id);

        (expr, id)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        Self::fetch(fetch, row)
    }
}

impl<C: Component> ReadOnlyWorldQuery for &'_ C {
    unsafe fn fetch(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        &*fetch.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

unsafe impl<C: Component> WorldQuery for &'_ mut C {
    type Item<'a> = &'a mut C;

    type Fetch = Column<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let id = world.init_component::<C>();

        let mut expr = FilteredAccessExpr::new();
        expr.and_read_write(id);

        (expr, id)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        &mut *fetch.0.as_ptr().add(row.0 as usize)
    }
}

macro_rules! impl_query_tuple {
    ($(($Q:ident, $q:ident)),*) => {
        unsafe impl<$($Q: WorldQuery),*> WorldQuery for ($($Q,)*) {
            type Item<'a> = ($($Q::Item<'a>,)*);

            type Fetch = ($($Q::Fetch,)*);

            type State = ($($Q::State,)*);

            fn init(world: &mut World, config: &mut SystemConfig) -> (FilteredAccessExpr<ComponentId>, Self::State) {
                let mut res = FilteredAccessExpr::new();

                $(
                    let (expr, $q) = $Q::init(world, config);
                    if !res.is_compatible(&expr) {
                        panic!(
                            "tuple element `{}` is incompatible with previous elements in the tuple",
                            std::any::type_name::<$Q>()
                        );
                    }
                    res.and(&expr);
                )*

                (res, ($($q,)*))
            }

            fn init_fetch(archetype: &Archetype, ($($q,)*): &mut Self::State) -> Option<Self::Fetch> {
                Some((
                    $(
                        $Q::init_fetch(archetype, $q)?,
                    )*
                ))
            }

            unsafe fn fetch_mut(($($q,)*): &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
                (
                    $(
                        $Q::fetch_mut($q, row),
                    )*
                )
            }
        }

        impl<$($Q: ReadOnlyWorldQuery),*> ReadOnlyWorldQuery for ($($Q,)*) {
            unsafe fn fetch(($($q,)*): &Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
                (
                    $(
                        $Q::fetch($q, row),
                    )*
                )
            }
        }
    }
}

all_tuples!(impl_query_tuple, 0, 15, Q, q);

unsafe impl<Q: WorldQuery> WorldQuery for Option<Q> {
    type Item<'a> = Option<Q::Item<'a>>;

    type Fetch = Option<Q::Fetch>;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let (mut expr, state) = Q::init(world, config);

        expr.or(&FilteredAccessExpr::new());

        (expr, state)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Some(Q::init_fetch(archetype, state))
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        fetch.as_mut().map(|f| Q::fetch_mut(f, row))
    }
}

impl<Q: ReadOnlyWorldQuery> ReadOnlyWorldQuery for Option<Q> {
    unsafe fn fetch(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        fetch.as_ref().map(|f| Q::fetch(f, row))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Or<L, R> {
    Left(L),
    Right(R),
    Both(L, R),
}

impl<L, R> Or<L, R> {
    pub fn map<F, G, M, S>(self, f: F, g: G) -> Or<M, S>
    where
        F: FnOnce(L) -> M,
        G: FnOnce(R) -> S,
    {
        match self {
            Or::Left(l) => Or::Left(f(l)),
            Or::Right(r) => Or::Right(g(r)),
            Or::Both(l, r) => Or::Both(f(l), g(r)),
        }
    }

    pub const fn as_ref(&self) -> Or<&L, &R> {
        match self {
            Or::Left(l) => Or::Left(l),
            Or::Right(r) => Or::Right(r),
            Or::Both(l, r) => Or::Both(l, r),
        }
    }

    pub fn as_mut(&mut self) -> Or<&mut L, &mut R> {
        match self {
            Or::Left(l) => Or::Left(l),
            Or::Right(r) => Or::Right(r),
            Or::Both(l, r) => Or::Both(l, r),
        }
    }
}

unsafe impl<L, R> WorldQuery for Or<L, R>
where
    L: WorldQuery,
    R: WorldQuery,
{
    type Item<'a> = Or<L::Item<'a>, R::Item<'a>>;

    type Fetch = Or<L::Fetch, R::Fetch>;

    type State = (L::State, R::State);

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let (mut left_access, left_state) = L::init(world, config);
        let (right_access, right_state) = R::init(world, config);

        // Need to check for compatibility because L and R can be active at the same
        // time in the `Or::Both` case.
        if !left_access.is_compatible(&right_access) {
            todo!("nice error message");
        }

        left_access.or(&right_access);

        (left_access, (left_state, right_state))
    }

    fn init_fetch(
        archetype: &Archetype,
        (left_state, right_state): &mut Self::State,
    ) -> Option<Self::Fetch> {
        match (
            L::init_fetch(archetype, left_state),
            R::init_fetch(archetype, right_state),
        ) {
            (None, None) => None,
            (None, Some(r)) => Some(Or::Right(r)),
            (Some(l), None) => Some(Or::Left(l)),
            (Some(l), Some(r)) => Some(Or::Both(l, r)),
        }
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        fetch
            .as_mut()
            .map(|l| L::fetch_mut(l, row), |r| R::fetch_mut(r, row))
    }
}

impl<L, R> ReadOnlyWorldQuery for Or<L, R>
where
    L: ReadOnlyWorldQuery,
    R: ReadOnlyWorldQuery,
{
    unsafe fn fetch(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        fetch
            .as_ref()
            .map(|l| L::fetch(l, row), |r| R::fetch(r, row))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Xor<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Xor<L, R> {
    pub fn map<F, G, M, S>(self, f: F, g: G) -> Xor<M, S>
    where
        F: FnOnce(L) -> M,
        G: FnOnce(R) -> S,
    {
        match self {
            Xor::Left(l) => Xor::Left(f(l)),
            Xor::Right(r) => Xor::Right(g(r)),
        }
    }

    pub const fn as_ref(&self) -> Xor<&L, &R> {
        match self {
            Xor::Left(l) => Xor::Left(l),
            Xor::Right(r) => Xor::Right(r),
        }
    }

    pub fn as_mut(&mut self) -> Xor<&mut L, &mut R> {
        match self {
            Xor::Left(l) => Xor::Left(l),
            Xor::Right(r) => Xor::Right(r),
        }
    }
}

unsafe impl<L, R> WorldQuery for Xor<L, R>
where
    L: WorldQuery,
    R: WorldQuery,
{
    type Item<'a> = Xor<L::Item<'a>, R::Item<'a>>;

    type Fetch = Xor<L::Fetch, R::Fetch>;

    type State = (L::State, R::State);

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let (mut left_access, left_state) = L::init(world, config);
        let (right_access, right_state) = R::init(world, config);

        // No need to check for compatilibity because only one of L and R is active at
        // the same time.
        left_access.xor(&right_access);

        (left_access, (left_state, right_state))
    }

    fn init_fetch(
        archetype: &Archetype,
        (left_state, right_state): &mut Self::State,
    ) -> Option<Self::Fetch> {
        match (
            L::init_fetch(archetype, left_state),
            R::init_fetch(archetype, right_state),
        ) {
            (None, None) => None,
            (None, Some(r)) => Some(Xor::Right(r)),
            (Some(l), None) => Some(Xor::Left(l)),
            (Some(_), Some(_)) => None,
        }
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        fetch
            .as_mut()
            .map(|l| L::fetch_mut(l, row), |r| R::fetch_mut(r, row))
    }
}

impl<L, R> ReadOnlyWorldQuery for Xor<L, R>
where
    L: ReadOnlyWorldQuery,
    R: ReadOnlyWorldQuery,
{
    unsafe fn fetch(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        fetch
            .as_ref()
            .map(|l| L::fetch(l, row), |r| R::fetch(r, row))
    }
}

pub struct Not<Q>(PhantomData<fn(Q)>);

impl<Q> Not<Q> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Q> Clone for Not<Q> {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<Q> Copy for Not<Q> {}

impl<Q> Default for Not<Q> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Q> fmt::Debug for Not<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Not").finish()
    }
}

unsafe impl<Q: WorldQuery> WorldQuery for Not<Q> {
    type Item<'a> = Self;

    type Fetch = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let (mut expr, state) = Q::init(world, config);

        expr.not();

        (expr, state)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        match Q::init_fetch(archetype, state) {
            Some(_) => None,
            None => Some(()),
        }
    }

    unsafe fn fetch_mut(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        Not::new()
    }
}

impl<Q: WorldQuery> ReadOnlyWorldQuery for Not<Q> {
    unsafe fn fetch(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        Not::new()
    }
}

pub struct With<Q>(PhantomData<fn(Q)>);

impl<Q> With<Q> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Q> Clone for With<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Q> Copy for With<Q> {}

impl<Q> Default for With<Q> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Q> fmt::Debug for With<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("With").finish()
    }
}

unsafe impl<Q: WorldQuery> WorldQuery for With<Q> {
    type Item<'a> = Self;

    type Fetch = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let (mut expr, state) = Q::init(world, config);

        expr.clear_access();

        (expr, state)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Q::init_fetch(archetype, state).map(|_| ())
    }

    unsafe fn fetch_mut(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        With::new()
    }
}

impl<Q: WorldQuery> ReadOnlyWorldQuery for With<Q> {
    unsafe fn fetch(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        With::new()
    }
}

pub struct Has<Q> {
    has: bool,
    _marker: PhantomData<fn(Q)>,
}

impl<Q> Has<Q> {
    pub const fn new(has: bool) -> Self {
        Self {
            has,
            _marker: PhantomData,
        }
    }

    pub const fn get(self) -> bool {
        self.has
    }
}

impl<Q> Clone for Has<Q> {
    fn clone(&self) -> Self {
        Self {
            has: self.has,
            _marker: PhantomData,
        }
    }
}

impl<Q> Copy for Has<Q> {}

impl<Q> Default for Has<Q> {
    fn default() -> Self {
        Self {
            has: false,
            _marker: PhantomData,
        }
    }
}

impl<Q> fmt::Debug for Has<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Has").field(&self.has).finish()
    }
}

impl<Q> Deref for Has<Q> {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.has
    }
}

unsafe impl<Q: WorldQuery> WorldQuery for Has<Q> {
    type Item<'a> = Self;

    type Fetch = bool;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        let (_, state) = Q::init(world, config);

        (FilteredAccessExpr::new(), state)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Some(Q::init_fetch(archetype, state).is_some())
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        Self::fetch(fetch, row)
    }
}

impl<Q: WorldQuery> ReadOnlyWorldQuery for Has<Q> {
    unsafe fn fetch(fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        Has::new(*fetch)
    }
}

unsafe impl WorldQuery for EntityId {
    type Item<'a> = Self;

    type Fetch = Column<EntityId>;

    type State = ();

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (FilteredAccessExpr<ComponentId>, Self::State) {
        Default::default()
    }

    fn init_fetch(archetype: &Archetype, (): &mut Self::State) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        *fetch.0.as_ptr().add(row.0 as usize)
    }
}

impl ReadOnlyWorldQuery for EntityId {
    unsafe fn fetch(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        *fetch.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

pub struct Column<T>(NonNull<T>);

impl<T> Clone for Column<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Copy for Column<T> {}

impl<T> fmt::Debug for Column<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Column").field(&self.0).finish()
    }
}

// SAFETY: `Column` is just a wrapper around a pointer, so these impls are
// safe on their own.
unsafe impl<T> Send for Column<T> {}
unsafe impl<T> Sync for Column<T> {}
