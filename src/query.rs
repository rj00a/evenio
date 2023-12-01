use std::error::Error;
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;

use evenio_macros::all_tuples;

use crate::access::{Access, AccessExpr};
use crate::archetype::{Archetype, ArchetypeId, ArchetypeRow, Archetypes};
use crate::component::ComponentId;
use crate::debug_checked::GetDebugChecked;
use crate::entity::EntityId;
use crate::prelude::{Component, World};
use crate::system::{SystemConfig, SystemParam};
use crate::world::UnsafeWorldCell;

pub unsafe trait Query {
    /// The item returned by this query. This is usually same type as `Self`,
    /// but with a transformed lifetime.
    type Item<'a>;
    /// Per-archetype state.
    type Fetch: Send + Sync + fmt::Debug + 'static;
    /// Cached data for fetch initialization. This is stored in [`QueryState`].
    type State: Send + Sync + fmt::Debug + 'static;

    fn init(world: &mut World, config: &mut SystemConfig)
        -> (AccessExpr<ComponentId>, Self::State);

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch>;

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a>;
}

pub trait ReadOnlyQuery: Query {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a>;
}

pub struct Fetcher<'a, Q: Query> {
    state: &'a mut FetcherState<Q>,
    world: UnsafeWorldCell<'a>,
}

pub struct FetcherState<Q: Query> {
    dense: Vec<Dense<Q::Fetch>>,
    /// Mapping from [`ArchetypeId`] to index in `dense`. `u32::MAX` indicates
    /// no index. Used for random access entity lookups.
    sparse: Vec<u32>,
    state: Q::State,
}

#[derive(Debug)]
struct Dense<F> {
    /// Collection of archetype column pointers for one archetype.
    fetch: F,
    id: ArchetypeId,
}

impl<Q: Query> Fetcher<'_, Q> {
    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::Item<'_>, FetchError> {
        let entities = self.world.entities();

        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity(entity));
        };

        let idx = *unsafe {
            self.state
                .sparse
                .get_debug_checked(loc.archetype.0 as usize)
        };

        if idx == u32::MAX {
            return Err(FetchError::QueryDoesNotMatch(entity));
        };

        let fetch = &mut unsafe { self.state.dense.get_debug_checked_mut(idx as usize) }.fetch;

        Ok(unsafe { Q::fetch_mut(fetch, loc.row) })
    }

    // TODO: get_many_mut

    pub fn iter_mut(&mut self) -> IterMut<Q> {
        IterMut {
            dense: self.state.dense.as_mut_slice(),
            row: ArchetypeRow(0),
            archetypes: self.world.archetypes(),
            _marker: PhantomData,
        }
    }
}

impl<Q: ReadOnlyQuery> Fetcher<'_, Q> {
    #[inline]
    pub fn get(&self, entity: EntityId) -> Result<Q::Item<'_>, FetchError> {
        let entities = self.world.entities();

        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity(entity));
        };

        let idx = *unsafe {
            self.state
                .sparse
                .get_debug_checked(loc.archetype.0 as usize)
        };

        if idx == u32::MAX {
            return Err(FetchError::QueryDoesNotMatch(entity));
        };

        let fetch = &unsafe { self.state.dense.get_debug_checked(idx as usize) }.fetch;

        Ok(unsafe { Q::fetch(fetch, loc.row) })
    }

    pub fn iter(&self) -> Iter<Q> {
        Iter {
            dense: &self.state.dense,
            row: ArchetypeRow(0),
            archetypes: self.world.archetypes(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FetchError {
    NoSuchEntity(EntityId),
    QueryDoesNotMatch(EntityId),
    AliasedMutability(EntityId),
}

impl<Q> SystemParam for Fetcher<'_, Q>
where
    Q: Query + 'static,
{
    type State = FetcherState<Q>;

    type Item<'a> = Fetcher<'a, Q>;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        let (expr, mut state) = Q::init(world, config);

        if !config.access.components.is_compatible(&expr) {
            panic!("aklwjdjklawdjkl");
        }

        config.access.components.and(&expr);

        // Get an upper bound on the size of the sparse array. This needs to be large
        // enough to avoid bounds checking.
        // TODO: iterating slotmap is not great. Also relies on implementation details
        // out of our control.
        let sparse_upper_bound = world
            .archetypes()
            .iter()
            .rev()
            .next()
            .map(|(id, _)| id.0 + 1)
            .unwrap_or(0);

        let mut sparse = vec![u32::MAX; sparse_upper_bound as usize];
        let mut dense = vec![];

        // TODO: don't iterate over every archetype.

        for (id, arch) in world.archetypes().iter() {
            if let Some(columns) = Q::init_fetch(arch, &mut state) {
                let idx = dense.len() as u32;
                dense.push(Dense { fetch: columns, id });
                *unsafe { sparse.get_debug_checked_mut(id.0 as usize) } = idx;
            }
        }

        Ok(FetcherState {
            dense,
            sparse,
            state,
        })
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: crate::event::EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Fetcher { state, world }
    }
}

impl<Q: Query> fmt::Debug for FetcherState<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("QueryState")
            .field("dense", &self.dense)
            .field("sparse", &self.sparse)
            .field("state", &self.state)
            .finish()
    }
}

unsafe impl<F> Send for Dense<F> {}
unsafe impl<F> Sync for Dense<F> {}

unsafe impl<C: Component> Query for &'_ C {
    type Item<'a> = &'a C;

    type Fetch = ColumnPtr<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
        let id = world.init_component::<C>();
        let expr = AccessExpr::with(id, Access::Read);

        (expr, id)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        Self::fetch(fetch, row)
    }
}

impl<C: Component> ReadOnlyQuery for &'_ C {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        &*fetch.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

unsafe impl<C: Component> Query for &'_ mut C {
    type Item<'a> = &'a mut C;

    type Fetch = ColumnPtr<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
        let id = world.init_component::<C>();
        let expr = AccessExpr::with(id, Access::ReadWrite);

        (expr, id)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        let cols = archetype.columns();

        cols.binary_search_by_key(state, |c| c.component_id())
            .ok()
            .map(|idx| ColumnPtr(cols[idx].data().cast()))
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        &mut *fetch.0.as_ptr().add(row.0 as usize)
    }
}

macro_rules! impl_query_tuple {
    ($(($Q:ident, $q:ident)),*) => {
        unsafe impl<$($Q: Query),*> Query for ($($Q,)*) {
            type Item<'a> = ($($Q::Item<'a>,)*);

            type Fetch = ($($Q::Fetch,)*);

            type State = ($($Q::State,)*);

            fn init(world: &mut World, config: &mut SystemConfig) -> (AccessExpr<ComponentId>, Self::State) {
                #![allow(unused_variables)]

                #[allow(unused_mut)]
                let mut res = AccessExpr::one();

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

            unsafe fn fetch_mut<'a>(($($q,)*): &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
                (
                    $(
                        $Q::fetch_mut($q, row),
                    )*
                )
            }
        }

        impl<$($Q: ReadOnlyQuery),*> ReadOnlyQuery for ($($Q,)*) {
            unsafe fn fetch<'a>(($($q,)*): &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
                (
                    $(
                        $Q::fetch($q, row),
                    )*
                )
            }
        }
    }
}

// Debug impls for tuples only go up to arity 12.
all_tuples!(impl_query_tuple, 0, 12, Q, q);

unsafe impl<Q: Query> Query for Option<Q> {
    type Item<'a> = Option<Q::Item<'a>>;

    type Fetch = Option<Q::Fetch>;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
        let (mut expr, state) = Q::init(world, config);

        expr.or(&AccessExpr::one());

        (expr, state)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Some(Q::init_fetch(archetype, state))
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch.as_mut().map(|f| Q::fetch_mut(f, row))
    }
}

impl<Q: ReadOnlyQuery> ReadOnlyQuery for Option<Q> {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
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

unsafe impl<L, R> Query for Or<L, R>
where
    L: Query,
    R: Query,
{
    type Item<'a> = Or<L::Item<'a>, R::Item<'a>>;

    type Fetch = Or<L::Fetch, R::Fetch>;

    type State = (L::State, R::State);

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
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

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch
            .as_mut()
            .map(|l| L::fetch_mut(l, row), |r| R::fetch_mut(r, row))
    }
}

impl<L, R> ReadOnlyQuery for Or<L, R>
where
    L: ReadOnlyQuery,
    R: ReadOnlyQuery,
{
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
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

unsafe impl<L, R> Query for Xor<L, R>
where
    L: Query,
    R: Query,
{
    type Item<'a> = Xor<L::Item<'a>, R::Item<'a>>;

    type Fetch = Xor<L::Fetch, R::Fetch>;

    type State = (L::State, R::State);

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
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

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch
            .as_mut()
            .map(|l| L::fetch_mut(l, row), |r| R::fetch_mut(r, row))
    }
}

impl<L, R> ReadOnlyQuery for Xor<L, R>
where
    L: ReadOnlyQuery,
    R: ReadOnlyQuery,
{
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
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

unsafe impl<Q: Query> Query for Not<Q> {
    type Item<'a> = Self;

    type Fetch = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
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

    unsafe fn fetch_mut<'a>(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        Not::new()
    }
}

impl<Q: Query> ReadOnlyQuery for Not<Q> {
    unsafe fn fetch<'a>(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
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

unsafe impl<Q: Query> Query for With<Q> {
    type Item<'a> = Self;

    type Fetch = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
        let (mut expr, state) = Q::init(world, config);

        expr.clear_access();

        (expr, state)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Q::init_fetch(archetype, state).map(|_| ())
    }

    unsafe fn fetch_mut<'a>(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        With::new()
    }
}

impl<Q: Query> ReadOnlyQuery for With<Q> {
    unsafe fn fetch<'a>(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
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

unsafe impl<Q: Query> Query for Has<Q> {
    type Item<'a> = Self;

    type Fetch = bool;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
        let (_, state) = Q::init(world, config);

        (AccessExpr::one(), state)
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Some(Q::init_fetch(archetype, state).is_some())
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        Self::fetch(fetch, row)
    }
}

impl<Q: Query> ReadOnlyQuery for Has<Q> {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        Has::new(*fetch)
    }
}

unsafe impl Query for EntityId {
    type Item<'a> = Self;

    type Fetch = ColumnPtr<EntityId>;

    type State = ();

    fn init(
        world: &mut World,
        config: &mut SystemConfig,
    ) -> (AccessExpr<ComponentId>, Self::State) {
        (AccessExpr::one(), ())
    }

    fn init_fetch(archetype: &Archetype, (): &mut Self::State) -> Option<Self::Fetch> {
        Some(ColumnPtr(archetype.entity_ids()))
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        *fetch.0.as_ptr().add(row.0 as usize)
    }
}

impl ReadOnlyQuery for EntityId {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        *fetch.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

pub struct ColumnPtr<T>(NonNull<T>);

impl<T> Clone for ColumnPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Copy for ColumnPtr<T> {}

impl<T> fmt::Debug for ColumnPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Column").field(&self.0).finish()
    }
}

// SAFETY: `ColumnPtr` is just a wrapper around a pointer, so these impls are
// safe on their own.
unsafe impl<T> Send for ColumnPtr<T> {}
unsafe impl<T> Sync for ColumnPtr<T> {}

pub struct Iter<'a, Q: ReadOnlyQuery> {
    dense: &'a [Dense<Q::Fetch>],
    /// Index into the current archetype.
    row: ArchetypeRow,
    archetypes: &'a Archetypes,
}

impl<'a, Q: ReadOnlyQuery> Iterator for Iter<'a, Q> {
    type Item = Q::Item<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (dense, rest) = self.dense.split_first()?;

            let len = unsafe { self.archetypes.get_debug_checked(dense.id) }.len();

            if self.row.0 == len {
                self.dense = rest;
                self.row.0 = 0;
                continue;
            }

            let item = unsafe { Q::fetch(&dense.fetch, self.row) };

            self.row.0 += 1;

            return Some(item);
        }
    }
}

pub struct IterMut<'a, Q: Query> {
    // Using a pointer to sneak past a puzzling lifetime problem.
    dense: *mut [Dense<Q::Fetch>],
    row: ArchetypeRow,
    archetypes: &'a Archetypes,
    _marker: PhantomData<&'a mut ()>,
}

impl<'a, Q: Query> Iterator for IterMut<'a, Q> {
    type Item = Q::Item<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (dense, rest) = unsafe { &mut (*self.dense) }.split_first_mut()?;

            let len = unsafe { self.archetypes.get_debug_checked(dense.id) }.len();

            if self.row.0 == len {
                self.dense = rest;
                self.row.0 = 0;
                continue;
            }

            let item = unsafe { Q::fetch_mut(&mut dense.fetch, self.row) };

            self.row.0 += 1;

            return Some(item);
        }
    }
}

impl<Q: Query> fmt::Debug for IterMut<'_, Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IterMut")
            .field("dense", &self.dense)
            .field("row", &self.row)
            .field("archetypes", &self.archetypes)
            .finish()
    }
}

// TODO: more iterator stuff.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::*;

    #[derive(Event)]
    struct E;

    fn check_query<Q: Query + 'static>() -> bool {
        let r = std::panic::catch_unwind(|| {
            let mut world = World::new();

            (world.add_system(|_: &E, _: Fetcher<Q>| {}), world)
        });

        if let Ok((_, mut world)) = r {
            dbg!(&world);

            // world.send(E); // TODO
            true
        } else {
            false
        }
    }

    /// Test for query access conflicts.
    macro_rules! t {
        ($name:ident, $succeed:expr, $q:ty) => {
            #[test]
            fn $name() {
                assert_eq!(check_query::<$q>(), $succeed);
            }
        };
    }

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    t!(t00, true, &mut A);
    t!(t01, true, (&mut A, &mut B));
    t!(t02, false, (&mut A, &mut A));
    t!(t03, false, (&mut A, (&mut A,)));
    t!(t04, false, (&mut A, &A));
    t!(t05, true, (&A, &A));
    t!(t06, true, ());
    t!(t07, false, (Option<&A>, &mut A));
    t!(t08, false, (&mut A, Option<&B>, &A, Not<&B>));
    t!(t09, true, (&mut A, Not<&A>, &A));
    t!(t10, false, Or<&mut A, &mut A>);
    t!(t11, true, Xor<&mut A, &mut A>);
    t!(t12, false, Or<(&A, &B), (&mut B, &C)>);
    t!(t13, true, (Xor<&mut A, &mut A>, &A));
    t!(t14, true, (Option<&A>, &A, &A));
    t!(t15, false, (Xor<(&A, &B), (&B, &C)>, &mut B));
    t!(t16, true, (Xor<(&A, &B), (&B, &C)>, &B));
    t!(t17, false, (Or<(&A, &B), (&B, &C)>, &mut B));
}
