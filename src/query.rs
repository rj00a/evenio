use std::error::Error;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::{fmt, mem};

use crate::access::Access;
use crate::archetype::{Archetype, ArchetypeRow};
use crate::prelude::{Component, World};
use crate::system::{SystemConfig, SystemParam};
use crate::world::UnsafeWorldCell;

pub unsafe trait WorldQuery {
    /// The item returned by this query. This is the same type as `Self`, but
    /// with the correct lifetime.
    type Item<'a>: WorldQuery;
    /// Per-archetype state.
    type Fetch: Send + Sync + Clone;

    fn init(world: &mut World, config: &mut SystemConfig);
    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch>;
    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_>;
}

pub trait ReadOnlyWorldQuery: WorldQuery {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a>;
}

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

unsafe impl<C: Component> WorldQuery for &'_ C {
    type Item<'a> = &'a C;

    type Fetch = RefFetch;

    fn init(world: &mut World, config: &mut SystemConfig) {
        let id = world.init_component::<C>();

        if !config.access.components.is_read_compatible(id) {
            todo!("nice error message");
        }

        config.access.components.and_read(id);
    }

    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        Self::fetch(fetch, row)
    }
}

impl<C: Component> ReadOnlyWorldQuery for &'_ C {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        &*fetch
            .column
            .cast::<C>()
            .as_ptr()
            .cast_const()
            .add(row.0 as usize)
    }
}

unsafe impl<C: Component> WorldQuery for &'_ mut C {
    type Item<'a> = &'a mut C;

    type Fetch = RefFetch;

    fn init(world: &mut World, config: &mut SystemConfig) {
        let id = world.init_component::<C>();

        assert!(
            config.access.components.is_read_write_compatible(id),
            "TODO: nice error message"
        );

        config.access.components.and_read_write(id);
    }

    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'_> {
        &mut *fetch.column.cast::<C>().as_ptr().add(row.0 as usize)
    }
}

/// The [`Qry::Fetch`] type for references (`&C` and `&mut C`).
#[derive(Clone, Copy, Debug)]
pub struct RefFetch {
    /// Direct pointer to the archetype's column containing the component data.
    column: NonNull<u8>,
}

// SAFETY: RefFetch is just a wrapper around a pointer.
unsafe impl Send for RefFetch {}
unsafe impl Sync for RefFetch {}

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

    fn init(world: &mut World, config: &mut SystemConfig) {
        let mut tmp = config.access.components.clone();
        L::init(world, config);
        mem::swap(&mut config.access.components, &mut tmp);
        R::init(world, config);

        // Need to check for compatibility because L and R can be active at the same
        // time in the Or::Both case.
        if !config.access.components.is_compatible(&tmp) {
            todo!("nice error message");
        }

        config.access.components.xor(&tmp);
    }

    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch> {
        todo!()
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

unsafe impl<L, R> WorldQuery for Xor<L, R>
where
    L: WorldQuery,
    R: WorldQuery,
{
    type Item<'a> = Xor<L::Item<'a>, R::Item<'a>>;

    type Fetch = Xor<L::Fetch, R::Fetch>;

    fn init(world: &mut World, config: &mut SystemConfig) {
        let mut tmp = config.access.components.clone();
        L::init(world, config);
        mem::swap(&mut config.access.components, &mut tmp);
        R::init(world, config);

        // No need to check for compatilibity because only one of L and R is active at
        // the same time.

        config.access.components.xor(&tmp);
    }

    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch> {
        todo!()
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

unsafe impl<Q: WorldQuery> WorldQuery for Not<Q> {
    type Item<'a> = Self;

    type Fetch = ();

    fn init(world: &mut World, config: &mut SystemConfig) {
        Q::init(world, config);
        config.access.components.not();
    }

    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        Not::new()
    }
}

impl<Q: WorldQuery> ReadOnlyWorldQuery for Not<Q> {
    unsafe fn fetch<'a>(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        Not::new()
    }
}

pub struct With<C>(PhantomData<fn(C)>);

impl<C> With<C> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<C> Clone for With<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<C> Copy for With<C> {}

impl<C> Default for With<C> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C> fmt::Debug for With<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("With").finish()
    }
}

unsafe impl<C: Component> WorldQuery for With<C> {
    type Item<'a> = Self;

    type Fetch = ();

    fn init(world: &mut World, config: &mut SystemConfig) {
        let id = world.init_component::<C>();

        config.access.components.and_with(id);
    }

    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        With::new()
    }
}

impl<C: Component> ReadOnlyWorldQuery for With<C> {
    unsafe fn fetch<'a>(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        With::new()
    }
}

pub struct Without<C>(PhantomData<fn(C)>);

impl<C> Without<C> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<C> Clone for Without<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<C> Copy for Without<C> {}

impl<C> Default for Without<C> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C> fmt::Debug for Without<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Without").finish()
    }
}

unsafe impl<C: Component> WorldQuery for Without<C> {
    type Item<'a> = Self;

    type Fetch = ();

    fn init(world: &mut World, config: &mut SystemConfig) {
        let id = world.init_component::<C>();

        config.access.components.and_without(id);
    }

    fn init_fetch(archetype: &Archetype) -> Option<Self::Fetch> {
        todo!()
    }

    unsafe fn fetch_mut(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'_> {
        Without::new()
    }
}

impl<C: Component> ReadOnlyWorldQuery for Without<C> {
    unsafe fn fetch<'a>(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        Without::new()
    }
}
